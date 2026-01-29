run_plackett_luce <- function(data_wide, id_map, outcome, firms97) {
  
  # ---------------------------------
  # Fit PL model (MLE)
  # ---------------------------------
  resp_ids <- data_wide$resp_id
  data_wide <- data_wide %>% dplyr::select(-resp_id)
  mod_mle   <- PlackettLuce(data_wide, npseudo = 0.5)
  summary_mod_mle <- summary(
    mod_mle,
    vcov. = vcov(mod_mle, type = "observed"),
    ref   = NULL
  )
  
  ## --- NEW: extract log-likelihood and # of parameters (for LRTs etc.) ----
  ll_obj    <- logLik(mod_mle)
  logLik_mle <- as.numeric(ll_obj)
  n_par_mle  <- attr(ll_obj, "df")
  if (is.null(n_par_mle)) {
    # fallback: count parameters (includes firm + tie parameters)
    n_par_mle <- length(coef(mod_mle))
  }
  ## -----------------------------------------------------------------------
  
  # ---------------------------------
  # Coefficients & usual SEs
  # ---------------------------------
  outcome_coeff <- as.data.frame(summary_mod_mle$coefficients) %>%
    tibble::rownames_to_column(var = "firm_id") %>%
    dplyr::filter(!grepl("^tie", firm_id)) %>%
    dplyr::mutate(firm_id = as.integer(sub("firm", "", firm_id))) %>%
    dplyr::inner_join(
      id_map %>%
        dplyr::mutate(firm_id = as.integer(firm_id)) %>%
        dplyr::select(firm, firm_id),
      by = "firm_id"
    ) %>%
    dplyr::select(
      firm, firm_id,
      Estimate    = Estimate,
      Estimate_se = `Std. Error`
    )
  
  # ---------------------------------
  # Robust SEs (sandwich) – FIRMS ONLY
  # ---------------------------------
  K <- nrow(outcome_coeff)
  
  # ---------------------------------
  # Robust SEs (sandwich) – FIRMS ONLY
  # ---------------------------------
  S_all <- score_function(mod_mle)  # n x p_all (firms + ties)
  
  # firm params in the same order as outcome_coeff
  firm_mask_names <- paste0("firm", outcome_coeff$firm_id)
  
  take_idx <- match(firm_mask_names, colnames(S_all))
  if (anyNA(take_idx)) {
    miss <- firm_mask_names[is.na(take_idx)]
    stop("Some firm params not found in score_function columns: ", paste(miss, collapse = ", "))
  }
  
  S_firm <- as.matrix(S_all[, take_idx, drop = FALSE])  # n x J
  
  # observed info inverse (your "Binv") restricted to firms
  Binv_firm <- vcov(mod_mle, type = "observed", ref = NULL)
  Binv_firm <- Binv_firm[firm_mask_names, firm_mask_names, drop = FALSE]
  
  # sandwich covariance (firm x firm)
  M          <- crossprod(S_firm)          # match your old scaling
  robust_cov_firm <- Binv_firm %*% M %*% Binv_firm
  
  # build S as resp_id + firm<id> cols (for pairwise_process downstream)
  S_df <- cbind(resp_id = resp_ids, as.data.frame(S_firm))
  colnames(S_df)[-1] <- firm_mask_names
  
  robust_se <- sqrt(diag(robust_cov_firm))
  
  robust_df <- tibble::enframe(robust_se, name = "param", value = "robust") %>%
    dplyr::mutate(firm_id = as.integer(sub("^firm", "", param))) %>%
    dplyr::select(firm_id, robust) %>%
    dplyr::arrange(firm_id)
  
  outcome_coeff <- outcome_coeff %>%
    dplyr::left_join(robust_df, by = "firm_id")
  
  
  new_est <- paste0(outcome)
  new_se  <- paste0(outcome, "_rse")
  
  coeff <- outcome_coeff %>%
    dplyr::select(firm, firm_id, Estimate) %>%
    dplyr::rename(!!new_est := Estimate)
  
  se <- outcome_coeff %>%
    dplyr::select(firm, firm_id, Estimate_se) %>%
    dplyr::rename(!!new_se := Estimate_se)
  
  rse <- outcome_coeff %>%
    dplyr::select(firm, firm_id, robust) %>%
    dplyr::rename(!!new_se := robust)
  
  # ---------------------------------
  # Helper to build variance-component summary list
  # ---------------------------------
  compute_var_comp_list <- function(alpha_hat, Sigma_alpha) {
    vc <- var_component_with_var(
      theta_hat = alpha_hat,
      Sigma     = Sigma_alpha
    )
    
    tot_var <- stats::var(alpha_hat)
    Y       <- mean(alpha_hat^2)
    noise   <- mean(diag(Sigma_alpha))
    
    sigma2_dot <- katz_correct(vc$sigma2_hat, vc$Vhat)
    
    list(
      tot_var    = tot_var,
      Y          = Y,
      noise      = noise,
      sigma2_hat = vc$sigma2_hat,
      Vhat       = vc$Vhat,
      se_sigma2  = vc$se_sigma2,
      sigma2_dot = sigma2_dot
    )
  }
  
  # ---------------------------------
  # Variance component for ALL firms
  # ---------------------------------
  alpha_hat_all   <- outcome_coeff$Estimate
  Sigma_alpha_all <- robust_cov_firm
  
  var_comp_alpha_all <- compute_var_comp_list(alpha_hat_all, Sigma_alpha_all)
  
  # ---------------------------------
  # Variance component restricted to firms97
  # ---------------------------------
  idx_97 <- which(outcome_coeff$firm_id %in% firms97)
  
  if (length(idx_97) >= 2) {
    alpha_hat_97   <- alpha_hat_all[idx_97]
    Sigma_alpha_97 <- Sigma_alpha_all[idx_97, idx_97, drop = FALSE]
    
    # centering matrix C = I - 11'/J
    J97 <- length(alpha_hat_97)
    C97 <- diag(J97) - matrix(1 / J97, J97, J97)
    
    alpha_97  <- as.numeric(C97 %*% alpha_hat_97)
    Sigma_97  <- C97 %*% Sigma_alpha_97 %*% t(C97)
    
    var_comp_alpha_97 <- compute_var_comp_list(alpha_97, Sigma_97)
    
  } else {
    # Not enough firms to compute a meaningful variance component
    var_comp_alpha_97 <- list(
      tot_var    = NA_real_,
      Y          = NA_real_,
      noise      = NA_real_,
      sigma2_hat = NA_real_,
      Vhat       = NA_real_,
      se_sigma2  = NA_real_,
      sigma2_dot = NA_real_
    )
  }
  
  # ---------------------------------
  # EB estimates (normal prior) using sigma2_dot from ALL firms
  # ---------------------------------
  log_worths <- outcome_coeff$Estimate
  
  # Use Katz-corrected variance component as prior variance
  bias_corrected_variance <- as.numeric(var_comp_alpha_all$sigma2_dot)
  
  # Safety: enforce small positive variance and cap by empirical second moment
  if (!is.finite(bias_corrected_variance) || bias_corrected_variance <= 0) {
    bias_corrected_variance <- 1e-6
  }
  max_var <- mean(log_worths^2)
  if (bias_corrected_variance > max_var) {
    bias_corrected_variance <- max_var
  }
  
  prior <- list(
    mu    = rep(0, ncol(data_wide)),
    Sigma = diag(rep(bias_corrected_variance, ncol(data_wide)))
  )
  
  mod_eb <- PlackettLuce(
    data_wide,
    npseudo = 0.5,
    normal  = prior
  )
  
  summary_mod_eb <- summary(mod_eb, ref = NULL)
  outcome_coeff_eb <- as.data.frame(summary_mod_eb$coefficients) %>%
    tibble::rownames_to_column(var = "firm_id") %>%
    dplyr::filter(!grepl("^tie", firm_id)) %>%
    dplyr::mutate(firm_id = as.integer(sub("firm", "", firm_id))) %>%
    dplyr::inner_join(
      id_map %>%
        dplyr::mutate(firm_id = as.integer(firm_id)) %>%
        dplyr::select(firm, firm_id),
      by = "firm_id"
    ) %>%
    dplyr::select(
      firm, firm_id,
      Estimate    = Estimate,
      Estimate_se = `Std. Error`
    )
  
  new_est <- paste0(outcome)
  new_se  <- paste0(outcome, "_se")
  
  coeff_eb <- outcome_coeff_eb %>%
    dplyr::select(firm, firm_id, Estimate) %>%
    dplyr::rename(!!new_est := Estimate)
  
  se_eb <- outcome_coeff_eb %>%
    dplyr::select(firm, firm_id, Estimate_se) %>%
    dplyr::rename(!!new_se := Estimate_se)
  
  # ---------------------------------
  # Average ratings & counts
  # ---------------------------------
  data_wide[data_wide == 0] <- NA
  avg_rating       <- colMeans(data_wide, na.rm = TRUE)
  num_observations <- colSums(!is.na(data_wide))
  std_dev          <- apply(data_wide, 2, sd, na.rm = TRUE)
  
  avg_ratings <- data.frame(
    firm_id          = seq_along(avg_rating),
    avg_rating       = as.vector(avg_rating),
    std_dev          = std_dev,
    num_observations = num_observations
  )
  
  avg_ratings <- dplyr::left_join(avg_ratings, id_map, by = "firm_id")
  
  num_obs <- avg_ratings %>%
    dplyr::select(firm, firm_id, num_observations) %>%
    dplyr::rename(!!new_est := num_observations)
  
  avg_ratings <- avg_ratings %>%
    dplyr::select(firm, firm_id, avg_rating) %>%
    dplyr::rename(!!new_est := avg_rating)
  
  # ---------------------------------
  # Return everything
  # ---------------------------------
  list(
    coeff              = coeff,
    se                 = se,
    rse                = rse,
    coeff_eb           = coeff_eb,
    avg_ratings        = avg_ratings,
    obs                = num_obs,
    var_comp_alpha_all = var_comp_alpha_all,
    var_comp_alpha_97  = var_comp_alpha_97,
    ## NEW:
    logLik_mle         = logLik_mle,
    n_par_mle          = n_par_mle,
    #NEW
    robust_cov        = robust_cov_firm,
    S                  = S_df,
    Binv              = Binv_firm
  )
}
