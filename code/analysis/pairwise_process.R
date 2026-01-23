# 
# ## Use Katz correction instead of pm_calc
# 
# 
# score_function <- function(mod, ref_firm = NULL) {
#   estfun_original <- estfun(mod, ref = ref_firm)
#   sample_size <- dim(estfun_original)[[1]]
#   scaling_factor <- sqrt(sample_size)
#   # estfun_corrected <- estfun_original * scaling_factor
#   estfun_corrected <- estfun_original
#   
#   score_means_corrected <- colMeans(estfun_corrected)
#   
#   max_score_mean <- max(abs(score_means_corrected))
#   if (max_score_mean > 0.01) {
#     # cat("⚠ Large score means detected - applying additional centering\n")
#     estfun_final <- scale(estfun_corrected, center = TRUE, scale = FALSE)
#   } else {
#     estfun_final <- estfun_corrected
#     # cat("✓ Scores are adequately centered\n")
#   }
#   
#   return(estfun_final)
# }

# ───────────────────────────────────────────────────────────────────────────────
# Pairwise process.
#   Item-worths assumed to sum to zero → use uncentered item moments.
#   noise1, noise2 now based on Katz-corrected signal variance.
# ───────────────────────────────────────────────────────────────────────────────
pairwise_process <- function(wide_mat1,
                             wide_mat2, 
                             id_map1,
                             id_map2,
                             unique_firms = NULL) {
  
  ## helper: fit PL once; extract firm-only coeffs, robust bits, etc.
  run_pl <- function(mat, k, weights = NULL, unique_firms = NULL) {
    X <- mat %>% dplyr::select(-resp_id)
    
    mod <- if (is.null(weights)) {
      PlackettLuce(X, npseudo = 0.5)
    } else {
      PlackettLuce(X, npseudo = 0.5, weights = weights)
    }
    
    # model-based covariance (this is B^{-1} in sandwich notation)
    B_inv_full <- vcov(mod, type = "observed", ref = NULL)
    
    # use summary() to get parameter table and locate 'tie' (drop it)
    smry <- summary(mod, vcov. = B_inv_full, ref = NULL)$coefficients
    rn   <- rownames(smry)
    firm_mask <- !grepl("^tie", rn)
    firm_ids_all  <- as.integer(sub("^firm", "", rn[firm_mask]))
    
    # coefficients + model-based SEs for firm params (ALL FIRMS)
    coeff_df_all <- as.data.frame(smry[firm_mask, , drop = FALSE]) %>%
      tibble::rownames_to_column("firm_id") %>%
      mutate(firm_id = as.integer(sub("^firm", "", firm_id))) %>%
      select(firm_id, Estimate, `Std. Error`) %>%
      rename(!!paste0("iter_", k) := Estimate,
             !!paste0("se_",   k) := `Std. Error`) %>%
      arrange(firm_id)
    
    # UN-SCALED per-observation scores; keep firm columns only (ALL FIRMS)
    S_full <- score_function(mod)                     # n x p
    S_firm_all <- S_full[, firm_mask, drop = FALSE]   # n x J_all
    
    # robust SEs (optional): Σ = B^{-1} (S^T S) B^{-1} (ALL FIRMS)
    nobs <- nrow(S_full)
    M_full <- crossprod(S_full)                       # no 1/n; consistent up to scale
    robust_cov_full <- B_inv_full %*% M_full %*% B_inv_full
    robust_se_all  <- sqrt(diag(robust_cov_full)[firm_mask])
    
    coeff_df_all <- coeff_df_all %>%
      left_join(
        tibble(firm_id = firm_ids_all,
               !!paste0("robust_", k) := robust_se_all),
        by = "firm_id"
      ) %>%
      arrange(firm_id)
    
    # ── NOW RESTRICT OUTPUTS IF unique_firms IS PROVIDED ───────────────────────
    if (!is.null(unique_firms)) {
      keep_mask <- firm_ids_all %in% unique_firms
      if (sum(keep_mask) < 2L) {
        stop("After restricting to unique_firms, fewer than 2 firms remain.")
      }
      firm_ids   <- firm_ids_all[keep_mask]
      coeff_df   <- coeff_df_all %>%
        filter(firm_id %in% firm_ids) %>%
        arrange(match(firm_id, firm_ids))
      S_firm     <- S_firm_all[, keep_mask, drop = FALSE]
      B_inv_firm <- B_inv_full[firm_mask, firm_mask, drop = FALSE][keep_mask, keep_mask, drop = FALSE]
    } else {
      firm_ids   <- firm_ids_all
      coeff_df   <- coeff_df_all
      S_firm     <- S_firm_all
      B_inv_firm <- B_inv_full[firm_mask, firm_mask, drop = FALSE]
    }
    
    list(
      coeff_df   = coeff_df,     # firm_id, iter_k, se_k, robust_k
      firm_ids   = firm_ids,     # J-length vector in column order (possibly restricted)
      S_firm     = S_firm,       # n x J, unscaled scores (possibly restricted)
      B_inv_firm = B_inv_firm,   # J x J (model-based vcov, possibly restricted)
      nobs       = nobs
    )
  }
  
  # ── Fit each outcome on its own sample (fit on all; restrict inside run_pl) ──
  fit1 <- run_pl(wide_mat1, k = 0, unique_firms = unique_firms)
  fit2 <- run_pl(wide_mat2, k = 0, unique_firms = unique_firms)
  
  # sample sizes and overlap accounting
  resp_id1 <- wide_mat1$resp_id; N1 <- length(resp_id1)
  resp_id2 <- wide_mat2$resp_id; N2 <- length(resp_id2)
  overlap_ids <- intersect(resp_id1, resp_id2)
  Ncommon <- length(overlap_ids)
  Ntot    <- N1 + N2 - Ncommon
  alpha   <- if (Ntot > 0) Ncommon / Ntot else 0
  
  # align firms (intersection)
  common_firms <- intersect(fit1$firm_ids, fit2$firm_ids)
  if (length(common_firms) == 0L) stop("No overlapping firms between outcomes.")
  i1 <- match(common_firms, fit1$firm_ids)
  i2 <- match(common_firms, fit2$firm_ids)
  
  # coefficients / SEs aligned to common firm order
  coeff1 <- fit1$coeff_df %>% filter(firm_id %in% common_firms) %>%
    arrange(match(firm_id, common_firms))
  coeff2 <- fit2$coeff_df %>% filter(firm_id %in% common_firms) %>%
    arrange(match(firm_id, common_firms))
  
  beta1 <- coeff1$iter_0
  beta2 <- coeff2$iter_0
  se1   <- coeff1$robust_0  # kept for sanity / debugging if needed
  se2   <- coeff2$robust_0
  
  # sanity: zero-sum
  if (abs(sum(beta1)) > 1e-8 || abs(sum(beta2)) > 1e-8) {
    warning("Item worths do not sum ~ 0; check reference/constraints.")
  }
  
  # uncentered item moments (zero-sum by construction)
  variance1  <- mean(beta1^2)
  variance2  <- mean(beta2^2)
  covariance <- mean(beta1 * beta2)
  
  # ── Build model-based vcov blocks for the common firms ──────────────────────
  B1_inv <- fit1$B_inv_firm[i1, i1, drop = FALSE]  # J x J
  B2_inv <- fit2$B_inv_firm[i2, i2, drop = FALSE]  # J x J
  
  # ── NEW: Katz-based signal + noise for each outcome ────────────────────────
  # Use variance-components with Sigma = model-based vcov (B_inv).
  vc1 <- var_component_with_var(theta_hat = beta1, Sigma = B1_inv)
  vc2 <- var_component_with_var(theta_hat = beta2, Sigma = B2_inv)
  
  signal1 <- katz_correct(vc1$sigma2_hat, vc1$Vhat)
  signal2 <- katz_correct(vc2$sigma2_hat, vc2$Vhat)
  
  # Noise = total variance - (Katz-corrected) signal; clamp at 0
  noise1 <- variance1 - signal1
  if (!is.finite(noise1) || noise1 < 0) noise1 <- 0
  
  noise2 <- variance2 - signal2
  if (!is.finite(noise2) || noise2 < 0) noise2 <- 0
  
  # ── Build full block B and M, then Θ = B^{-1} M B^{-1}, extract upper-right block
  B_blockInv <- Matrix::bdiag(B1_inv, B2_inv)          # (2J) x (2J)
  
  # S rows for overlap (aligned to same order on both sides)
  if (Ncommon > 0L) {
    ord_ids <- sort(overlap_ids)
    map1 <- match(ord_ids, resp_id1)
    map2 <- match(ord_ids, resp_id2)
  }
  
  # Within-outcome meats with UN-SCALED scores
  S1_full <- fit1$S_firm[, i1, drop = FALSE]          # N1 x J
  S2_full <- fit2$S_firm[, i2, drop = FALSE]          # N2 x J
  M11 <- crossprod(S1_full)                           # J x J
  M22 <- crossprod(S2_full)                           # J x J
  
  # Cross meat:
  if (Ncommon == 0L) {
    M12 <- matrix(0, nrow = ncol(B1_inv), ncol = ncol(B2_inv))
  } else {
    S1_ovl <- S1_full[map1, , drop = FALSE]           # Ncommon x J
    S2_ovl <- S2_full[map2, , drop = FALSE]           # Ncommon x J
    
    M12 <- crossprod(S1_ovl, S2_ovl)                  # J x J
  }
  
  # Assemble full M
  M_block <- rbind(
    cbind(M11,       M12),
    cbind(t(M12),    M22)
  )
  
  # Sandwich with B^{-1} blocks
  Theta_block <- B_blockInv %*% M_block %*% B_blockInv          # (2J) x (2J)
  
  J <- ncol(B1_inv)
  Theta12 <- Theta_block[1:J, (J + 1):(J + ncol(B2_inv)), drop = FALSE]  # upper-right J x J
  Theta11 <- as.matrix(Theta_block[1:J, 1:J, drop = FALSE])
  Theta22 <- as.matrix(Theta_block[(J+1):(2*J), (J+1):(2*J), drop = FALSE])
  se1_test <- sqrt(diag(Theta11))
  se2_test <- sqrt(diag(Theta22))
  
  # Scalar cross-noise correction from the cross block:
  noise12 <- sum(Matrix::diag(Theta12))/J
  
  list(
    firms         = common_firms,
    N1            = N1,
    N2            = N2,
    Ncommon       = Ncommon,
    Ntot          = Ntot,
    overlap_frac  = alpha,
    variance1     = variance1,
    variance2     = variance2,
    covariance    = covariance,
    signal1       = signal1,
    signal2       = signal2,
    noise1        = noise1,
    noise2        = noise2,
    noise12       = noise12,
    corr          = (covariance)/(sqrt(variance1)*sqrt(variance2)),
    corr_c        = (covariance - noise12) /
      (sqrt(variance1 - noise1) * sqrt(variance2 - noise2)),
    corr_den      = (covariance) /
      (sqrt(variance1 - noise1) * sqrt(variance2 - noise2))
  )
}
