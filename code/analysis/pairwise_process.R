# ───────────────────────────────────────────────────────────────────────────────
# Pairwise process.
#   Item-worths assumed to sum to zero → use uncentered item moments.
#   noise1, noise2 now based on Katz-corrected signal variance.
# ───────────────────────────────────────────────────────────────────────────────
pairwise_process <- function(S1,
                             S2,
                             Binv1,
                             Binv2,
                             robust_cov1,
                             robust_cov2,
                             beta1,
                             beta2) {
  
  # Remove resp_id and coerce to numeric matrix (ensure order preserved)
  # NOTE: column order must match Binv row/col order. We assume upstream ensured that.
  S1_full <- if (is.data.frame(S1)) {
    as.matrix(dplyr::select(S1, -resp_id))
  } else {
    as.matrix(S1[, setdiff(colnames(S1), "resp_id"), drop = FALSE])
  }
  
  S2_full <- if (is.data.frame(S2)) {
    as.matrix(dplyr::select(S2, -resp_id))
  } else {
    as.matrix(S2[, setdiff(colnames(S2), "resp_id"), drop = FALSE])
  }
  
  # recenter outcome 1
  rec1 <- recenter_pl_objects(
    beta   = beta1,
    Binv   = Binv1,
    cov    = robust_cov1,
    S_full = S1_full
  )
  
  beta1        <- rec1$beta
  Binv1        <- rec1$Binv
  robust_cov1  <- rec1$cov
  S1_full      <- rec1$S
  
  # recenter outcome 2
  rec2 <- recenter_pl_objects(
    beta   = beta2,
    Binv   = Binv2,
    cov    = robust_cov2,
    S_full = S2_full
  )
  
  beta2        <- rec2$beta
  Binv2        <- rec2$Binv
  robust_cov2  <- rec2$cov
  S2_full      <- rec2$S
  
  # sample sizes and overlap accounting
  resp_id1 <- if (is.data.frame(S1)) S1$resp_id else S1[, "resp_id"]
  resp_id2 <- if (is.data.frame(S2)) S2$resp_id else S2[, "resp_id"]
  
  N1 <- length(resp_id1)
  N2 <- length(resp_id2)
  
  overlap_ids <- intersect(resp_id1, resp_id2)
  Ncommon <- length(overlap_ids)
  Ntot    <- N1 + N2 - Ncommon
  alpha   <- if (Ntot > 0) Ncommon / Ntot else 0
  
  # Robust variances and SEs
  var1_vec <- diag(robust_cov1)
  var2_vec <- diag(robust_cov2)
  
  se1_vec <- sqrt(var1_vec)
  se2_vec <- sqrt(var2_vec)
  
  # Raw moments
  variance1  <- mean(beta1^2)
  variance2  <- mean(beta2^2)
  covariance <- mean(beta1 * beta2)
  
  # Signal (Katz)
  vc1 <- var_component_with_var(theta_hat = beta1, Sigma = robust_cov1)
  vc2 <- var_component_with_var(theta_hat = beta2, Sigma = robust_cov2)
  
  signal1 <- katz_correct(vc1$sigma2_hat, vc1$Vhat)
  signal2 <- katz_correct(vc2$sigma2_hat, vc2$Vhat)
  
  # Noise
  noise1 <- variance1 - signal1
  noise2 <- variance2 - signal2
  
  # Noise check
  noise1_check <- mean(var1_vec, na.rm = TRUE)
  noise2_check <- mean(var2_vec, na.rm = TRUE)
  
  # -----------------------
  # Prepare score matrices
  # -----------------------
  if (Ncommon > 0L) {
    ord_ids <- sort(overlap_ids)
    map1 <- match(ord_ids, resp_id1)
    map2 <- match(ord_ids, resp_id2)
    
    S1_ovl <- as.matrix(S1_full[map1, , drop = FALSE])
    S2_ovl <- as.matrix(S2_full[map2, , drop = FALSE])
  } else {
    S1_ovl <- matrix(0, nrow = 0, ncol = ncol(S1_full))
    S2_ovl <- matrix(0, nrow = 0, ncol = ncol(S2_full))
  }
  
  # Form blocks
  M11 <- crossprod(S1_full)
  M22 <- crossprod(S2_full)
  
  M12 <- if (Ncommon == 0L) {
    matrix(0, nrow = ncol(S1_full), ncol = ncol(S2_full))
  } else {
    crossprod(S1_ovl, S2_ovl)
  }
  
  # B block
  B_blockInv <- Matrix::bdiag(Binv1, Binv2)
  
  # Sandwich
  M_block <- rbind(
    cbind(M11,    M12),
    cbind(t(M12), M22)
  )
  
  Theta_block <- B_blockInv %*% M_block %*% B_blockInv
  
  # Extract blocks
  J <- ncol(Binv1)
  
  Theta11 <- as.matrix(Theta_block[1:J, 1:J, drop = FALSE])
  Theta22 <- as.matrix(Theta_block[(J + 1):(2 * J), (J + 1):(2 * J), drop = FALSE])
  Theta12 <- Theta_block[1:J, (J + 1):(2 * J), drop = FALSE]
  
  se1_test <- mean(diag(Theta11))
  se2_test <- mean(diag(Theta22))
  
  # Cross-noise
  noise12 <- sum(Matrix::diag(Theta12)) / J
  
  # Correlations
  denom_corr <- sqrt(pmax(0, signal1)) *
    sqrt(pmax(0, signal1))
  
  corr     <- covariance / (sqrt(variance1) * sqrt(variance2))
  corr_c   <- if (denom_corr > 0) (covariance - noise12) / denom_corr else NA_real_
  corr_den <- if (denom_corr > 0) covariance / denom_corr else NA_real_
  
  list(
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
    noise1_check  = noise1_check,
    noise2_check  = noise2_check,
    sigmahat1     = vc1$sigma2_hat,
    sigmahat2     = vc2$sigma2_hat,
    vhat1         = vc1$Vhat,
    vhat2         = vc2$Vhat,
    noise12       = noise12,
    se1_test      = se1_test,
    se2_test      = se2_test,
    corr          = corr,
    corr_c        = corr_c,
    corr_den      = corr_den
  )
}
