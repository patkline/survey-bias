# ------------------------------------------------------------------------------
# Purpose: Variance and Noise Calculations
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
compute_variance_noise_signal <- function(res) {
  # res: single model result object, e.g. results$all$OL[[outcome]]
  # expects:
  #   res$firm_table$estimate  (centered firm effects)
  #   res$mats$rcov            (robust covariance, centered, JxJ)
  
  stopifnot(!is.null(res$firm_table), "estimate" %in% names(res$firm_table))
  stopifnot(!is.null(res$mats$rcov))
  
  beta <- as.numeric(res$firm_table$estimate)
  # Center beta so mean(beta^2) = Var(beta) for any model. OL/Borda/OLS/OLSC
  # are already mean-zero (sum-to-zero / recenter_objects); PL pins the
  # baseline firm to 0, so its betas need explicit centering before any
  # second-moment formula gives the right answer.
  beta <- beta - mean(beta, na.rm = TRUE)

  Sigma <- as.matrix(res$mats$rcov)
  if (is.null(dim(Sigma)) || nrow(Sigma) != ncol(Sigma)) stop("rcov must be square JxJ")

  # "variance" of centered firm effects (your convention: mean(beta^2))
  variance <- mean(beta^2, na.rm = TRUE)
  
  # "noise" (your convention: mean diag(rcov))
  noise <- mean(diag(Sigma), na.rm = TRUE)
  
  # Katz-corrected signal via your helper
  vc <- var_component_with_var(theta_hat = beta, Sigma = Sigma)
  signal <- katz_correct(vc$sigma2_hat, vc$Vhat)

  # njobs-weighted variance/noise/Katz components, computed only when every firm has a positive job weight (i.e. the subset97 sample); NA on the full sample
  njobs_weighted_components <- if (!is.null(res$firm_table$njobs) && !anyNA(res$firm_table$njobs) && all(res$firm_table$njobs > 0)) {
    compute_njobs_weighted_signal_components(
      firm_regressor_vector = as.numeric(res$firm_table$estimate),
      firm_number_of_jobs_vector = as.numeric(res$firm_table$njobs),
      firm_robust_covariance_matrix = Sigma
    )
  } else {
    list(
      variance_njobs_weighted = NA_real_,
      noise_njobs_weighted = NA_real_,
      sigma2_hat_njobs_weighted = NA_real_,
      Vhat_njobs_weighted = NA_real_,
      signal_njobs_weighted_katz = NA_real_,
      noise_njobs_weighted_katz = NA_real_
    )
  }

  list(
    J = length(beta),
    variance = variance,
    noise = noise,
    sigma2_hat = vc$sigma2_hat,
    Vhat = vc$Vhat,
    signal = signal,
    variance_njobs_weighted = njobs_weighted_components$variance_njobs_weighted,
    noise_njobs_weighted = njobs_weighted_components$noise_njobs_weighted,
    sigma2_hat_njobs_weighted = njobs_weighted_components$sigma2_hat_njobs_weighted,
    Vhat_njobs_weighted = njobs_weighted_components$Vhat_njobs_weighted,
    signal_njobs_weighted_katz = njobs_weighted_components$signal_njobs_weighted_katz,
    noise_njobs_weighted_katz = njobs_weighted_components$noise_njobs_weighted_katz
  )
}


write_variance_sheet <- function(results, output_dir, sheet_name = "variance") {
  # results: list(all=..., subset97=...)
  stopifnot(is.list(results), !is.null(results$all))
  
  models <- intersect(c("OL", "PL", "Borda", "OLS", "OLSC"), names(results$all))

  # infer outcomes present (union over models AND sets — _dm_w/_im_w are only
  # built for subset97 since njobs is NA for firms outside the subset)
  sets_present <- intersect(c("all", "subset97"), names(results))
  survey_vars <- unique(unlist(lapply(sets_present, function(s)
    unlist(lapply(models, function(m) names(results[[s]][[m]])))
  )))
  survey_vars <- survey_vars[!is.na(survey_vars) & nzchar(survey_vars)]
  
  rows <- list()
  k <- 1L
  
  for (subset in c("all", "subset97")) {
    if (subset == "subset97" && is.null(results$subset97)) next
    
    for (model in models) {
      bucket <- if (subset == "all") results$all[[model]] else results$subset97[[model]]
      if (is.null(bucket)) next
      
      for (outcome in survey_vars) {
        res <- bucket[[outcome]]
        if (is.null(res)) next
        
        out <- compute_variance_noise_signal(res)
        
        rows[[k]] <- data.frame(
          subset    = subset,
          model     = model,
          outcome   = outcome,
          J         = out$J,
          variance  = out$variance,
          noise     = out$noise,
          sigma2_hat= out$sigma2_hat,
          Vhat      = out$Vhat,
          signal    = out$signal,
          variance_njobs_weighted = out$variance_njobs_weighted,
          noise_njobs_weighted = out$noise_njobs_weighted,
          sigma2_hat_njobs_weighted = out$sigma2_hat_njobs_weighted,
          Vhat_njobs_weighted = out$Vhat_njobs_weighted,
          signal_njobs_weighted_katz = out$signal_njobs_weighted_katz,
          noise_njobs_weighted_katz = out$noise_njobs_weighted_katz,
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }
  
  var_df <- dplyr::bind_rows(rows) %>%
    dplyr::arrange(subset, model, outcome)   # <-- FIX: no OL/PL/Borda columns anymore

  write_parquet_sheet(output_dir, sheet_name, var_df)

  invisible(var_df)
}
