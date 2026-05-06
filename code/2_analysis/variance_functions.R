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
  
  list(
    J = length(beta),
    variance = variance,
    noise = noise,
    sigma2_hat = vc$sigma2_hat,
    Vhat = vc$Vhat,
    signal = signal
  )
}


write_variance_sheet <- function(results, output_dir, sheet_name = "variance") {
  # results: list(all=..., subset97=...)
  stopifnot(is.list(results), !is.null(results$all))
  
  models <- intersect(c("OL", "PL", "Borda", "OLS", "OLSC"), names(results$all))
  
  # infer outcomes present (union over models)
  survey_vars <- unique(unlist(lapply(models, function(m) names(results$all[[m]]))))
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