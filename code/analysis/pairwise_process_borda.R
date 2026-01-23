#===============================================================================
# Pairwise Borda process (two outcomes, using precomputed B_indiv tables)
# Returns the SAME structure as your PL pairwise function.
#
# Inputs:
#   B_indiv1, B_indiv2: tibbles/data.frames with columns:
#       - resp_id (or id_var), firm_id, firm, B
#   unique_firms: optional integer vector of firm_ids to restrict to
#   id_var: name of respondent id column (default "resp_id")
#   union_scale: TRUE -> scale cross meat by Ncommon/Ntot (matches your PL code)
#   pm_calc_path: file path to pm_calc.R (must define pm_calc())
#
# Requires: dplyr, tidyr, tibble, Matrix; pm_calc.R provides pm_calc()
#===============================================================================
pairwise_borda_process <- function(B_indiv1, B_indiv2,
                                   unique_firms = NULL,
                                   id_var = "resp_id") {

  source("~/Documents/consolidated_code_server/code/pm_calc.R")
  
  # -- Basic column checks --------------------------------------------------------
  need_cols <- c(id_var, "firm_id", "firm", "B")
  if (!all(need_cols %in% names(B_indiv1))) {
    stop("B_indiv1 must contain columns: ", paste(need_cols, collapse = ", "))
  }
  if (!all(need_cols %in% names(B_indiv2))) {
    stop("B_indiv2 must contain columns: ", paste(need_cols, collapse = ", "))
  }
  
  # -- Optional restriction to unique_firms --------------------------------------
  if (!is.null(unique_firms)) {
    B_indiv1 <- dplyr::filter(B_indiv1, firm_id %in% unique_firms)
    B_indiv2 <- dplyr::filter(B_indiv2, firm_id %in% unique_firms)
  }
  
  # -- Common firms across the two outcomes --------------------------------------
  firms1 <- dplyr::distinct(B_indiv1, firm_id, firm)
  firms2 <- dplyr::distinct(B_indiv2, firm_id, firm)
  common_firms <- intersect(firms1$firm_id, firms2$firm_id)
  if (length(common_firms) < 2L) stop("Need at least 2 overlapping firms between outcomes.")
  
  B1c <- dplyr::filter(B_indiv1, firm_id %in% common_firms)
  B2c <- dplyr::filter(B_indiv2, firm_id %in% common_firms)
  
  # -- Per-firm means and counts (own samples) -----------------------------------
  mean1 <- B1c %>%
    dplyr::group_by(firm_id) %>%
    dplyr::summarise(n1_j = dplyr::n(), mu1 = mean(B), .groups = "drop")
  mean2 <- B2c %>%
    dplyr::group_by(firm_id) %>%
    dplyr::summarise(n2_j = dplyr::n(), mu2 = mean(B), .groups = "drop")
  
  # Align the same firm order
  ord <- sort(common_firms)
  mean1 <- mean1[match(ord, mean1$firm_id), , drop = FALSE]
  mean2 <- mean2[match(ord, mean2$firm_id), , drop = FALSE]
  
  # -- Build score matrices (0 for missing respondents-firm pairs) ----------------
  build_S <- function(B_indiv_c, mu_vec, firm_order, id_var) {
    B_cent <- B_indiv_c %>%
      dplyr::mutate(Bc = B - mu_vec[match(firm_id, firm_order)]) %>%
      dplyr::select(!!rlang::sym(id_var), firm_id, Bc)
    
    wide <- tidyr::pivot_wider(
      B_cent,
      names_from = firm_id, values_from = Bc,
      values_fill = list(Bc = 0)
    )
    
    # Ensure all firm columns present in the specified order
    miss_cols <- setdiff(as.character(firm_order), names(wide))
    for (mc in miss_cols) wide[[mc]] <- 0
    wide <- wide[, c(id_var, as.character(firm_order)), drop = FALSE]
    
    list(S = as.matrix(wide[, -1, drop = FALSE]),
         rid = wide[[id_var]])
  }
  
  S1_list <- build_S(B1c, mu_vec = mean1$mu1, firm_order = mean1$firm_id, id_var = id_var)
  S2_list <- build_S(B2c, mu_vec = mean2$mu2, firm_order = mean2$firm_id, id_var = id_var)
  
  S1_full <- S1_list$S
  S2_full <- S2_list$S
  rid1    <- S1_list$rid
  rid2    <- S2_list$rid
  
  # -- Respondent universes & overlap -------------------------------------------
  resp1 <- sort(unique(rid1)); N1 <- length(resp1)
  resp2 <- sort(unique(rid2)); N2 <- length(resp2)
  overlap_ids <- intersect(resp1, resp2)
  Ncommon <- length(overlap_ids)
  Ntot    <- N1 + N2 - Ncommon
  alpha   <- if (Ntot > 0) Ncommon / Ntot else 0
  
  J <- ncol(S1_full)
  
  # -- Meat blocks ---------------------------------------------------------------
  M11 <- crossprod(S1_full)
  M22 <- crossprod(S2_full)
  
  if (Ncommon > 0L) {
    ord_ids <- sort(overlap_ids)
    map1 <- match(ord_ids, rid1)
    map2 <- match(ord_ids, rid2)
    S1_ovl <- S1_full[map1, , drop = FALSE]
    S2_ovl <- S2_full[map2, , drop = FALSE]
    M12 <- crossprod(S1_ovl,S2_ovl)
  } else {
    M12 <- matrix(0, J, J)
  }
  
  M_block <- rbind(
    cbind(M11,        M12),
    cbind(t(M12),     M22)
  )
  
  # -- Bread inverses (means): G^{-1} = diag(N/n_j) ------------------------------
  G1_inv <- diag(1 / mean1$n1_j, nrow = J, ncol = J)
  G2_inv <- diag(1 / mean2$n2_j, nrow = J, ncol = J)
  B_blockInv <- Matrix::bdiag(G1_inv, G2_inv)
  
  # -- Sandwich ------------------------------------------------------------------
  Theta_block <- B_blockInv %*% M_block %*% B_blockInv
  Theta11 <- as.matrix(Theta_block[1:J, 1:J, drop = FALSE])
  Theta22 <- as.matrix(Theta_block[(J+1):(2*J), (J+1):(2*J), drop = FALSE])
  Theta12 <- as.matrix(Theta_block[1:J, (J+1):(2*J), drop = FALSE])
  
  # -- Across-firm moments (centered across firms to mimic zero-sum worths) -----
  mu1c <- mean1$mu1 - mean(mean1$mu1, na.rm = TRUE)
  mu2c <- mean2$mu2 - mean(mean2$mu2, na.rm = TRUE)
  
  variance1  <- mean(mu1c^2)
  variance2  <- mean(mu2c^2)
  covariance <- mean(mu1c * mu2c)
  
  # Per-firm robust SE vectors from sandwich diagonals
  se1_vec <- sqrt(pmax(diag(Theta11), 0))
  se2_vec <- sqrt(pmax(diag(Theta22), 0))
  
  # Use external pm_calc() (returns list with $post_mode, etc.)
  res1 <- pm_calc(mu1c, se1_vec, center = FALSE)
  res2 <- pm_calc(mu2c, se2_vec, center = FALSE)
  
  post1  <- res1$post_mode
  post2  <- res2$post_mode
  noise1 <- variance1 - post1; if (is.na(noise1) || noise1 < 0) noise1 <- 0
  noise2 <- variance2 - post2; if (is.na(noise2) || noise2 < 0) noise2 <- 0
  
  # Cross-noise: average diagonal of cross block (same convention as your PL)
  noise12 <- sum(diag(Theta12)) / J
  
  corr     <- covariance / (sqrt(variance1) * sqrt(variance2))
  corr_c   <- (covariance - noise12) /
    (sqrt(variance1 - noise1) * sqrt(variance2 - noise2))
  corr_den <-  covariance /
    (sqrt(variance1 - noise1) * sqrt(variance2 - noise2))
  
  # -- Return EXACT structure ----------------------------------------------------
  list(
    firms         = as.integer(mean1$firm_id),   # aligned common firms
    N1            = N1,
    N2            = N2,
    Ncommon       = Ncommon,
    Ntot          = Ntot,
    overlap_frac  = alpha,
    variance1     = variance1,
    variance2     = variance2,
    covariance    = covariance,
    noise1        = noise1,
    noise2        = noise2,
    noise12       = noise12,
    corr          = corr,
    corr_c        = corr_c,
    corr_den      = corr_den,
    all_firms     = is.null(unique_firms)
  )
}
