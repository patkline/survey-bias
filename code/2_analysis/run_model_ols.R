# ----------------------------------------------------------------------------------------
# Purpose: Construct firm-level estimates from respondent-level survey responses ---
#   - build the respondent x firm scores for the chosen aggregation method
#   - aggregate them to firm-level estimates with the mean estimator
#   - shrink the estimates with empirical Bayes
#   - return the entity table and covariance matrices the pipeline stores
#
# Created: Jordan Cammarota 2026-03-06
# Edited: Nico Rotundo 2026-07-01
# ----------------------------------------------------------------------------------------
run_model <- function(
  # Aggregation method, either "OLS" or "Borda"
  aggregation_method,
  # Long data frame of respondent x firm ratings, used when the method is "OLS"
  respondent_firm_ratings_long,
  # Wide data frame of respondent rankings, used when the method is "Borda"
  respondent_firm_rankings_wide,
  # Firm identifier crosswalk carrying firm names and job counts
  firm_id_map,
  # Name of the rating variable to aggregate
  outcome_variable_name
) {
  # Require the method to be one we implement
  stopifnot(aggregation_method %in% c("OLS", "Borda"))

  # -------------------------------------------------------------------------------------
  # Build the respondent x firm scores fed to the mean estimator
  # -------------------------------------------------------------------------------------
  if (aggregation_method == "OLS") {
    # Require the rating variable in the long data
    stopifnot(outcome_variable_name %in% names(respondent_firm_ratings_long))
    # Flip the 1-5 rating so a higher value means a more preferred firm
    respondent_firm_ratings_long$rating <- 6 - respondent_firm_ratings_long[[outcome_variable_name]]
    # Use the flipped long ratings as the estimator input
    respondent_firm_scores <- respondent_firm_ratings_long
    # Name the score column the estimator reads
    score_variable_name <- "rating"
  } else {
    # Compute each respondent's individual Borda counts across firms
    respondent_firm_scores <- compute_borda_individual_wide(
      data_wide        = respondent_firm_rankings_wide,
      id_map           = firm_id_map,
      id_var           = "resp_id",
      higher_is_better = FALSE,
      normalize        = TRUE,
      # Anchor the Borda scale to reference firms 38, 76, and 90
      ref_firm_ids     = c(38, 76, 90)
    )
    # Name the score column the estimator reads
    score_variable_name <- "B"
  }

  # Aggregate the respondent x firm scores to recentered and non-recentered firm-level estimates
  estimator_output <- mean_estimator_bread_and_score(
    respondent_firm_scores,
    respondent_id_variable_name = "resp_id",
    firm_id_variable_name       = "firm_id",
    rating_variable_name        = score_variable_name
  )

  firm_scores <- out$firm_scores  # firm_id, firm_mean_rating, se, rse
  Psi <- out$score  # actually influence functions now
  cov_mat <- out$cov
  rcov_mat <- out$rcov

  # Add firm names
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_scores <- firm_scores %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm, njobs), by = "firm_id")
  } else {
    firm_scores$firm <- NA_character_
  }

  # Align everything to firm_scores order
  firm_ids  <- as.integer(firm_scores$firm_id)
  firm_cols <- paste0("firm", firm_ids)
  entity_cols <- paste0("entity", firm_ids)

  miss <- setdiff(firm_cols, colnames(Psi))
  if (length(miss)) stop("run_model_ols(): score matrix missing firm cols: ", paste(miss, collapse = ", "))

  Psi_mat   <- as.matrix(Psi[, firm_cols, drop = FALSE])
  cov_mat   <- as.matrix(cov_mat[firm_cols, firm_cols, drop = FALSE])
  rcov_mat  <- as.matrix(rcov_mat[firm_cols, firm_cols, drop = FALSE])

  # Rename mats to entity<id> naming
  colnames(Psi_mat) <- entity_cols
  dimnames(cov_mat)   <- list(entity_cols, entity_cols)
  dimnames(rcov_mat)  <- list(entity_cols, entity_cols)

  # EB step (uses robust SE)
  eb_hat <- rep(NA_real_, nrow(firm_scores))
  if (isTRUE(do_eb) && exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_scores$firm_mean_rating) & is.finite(firm_scores$rse) & firm_scores$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_scores$firm_mean_rating[ok], s = pmax(firm_scores$rse[ok], 1e-8))
      eb_hat[ok] <- eb_fit$theta_eb
    }
  }

  # Build entity-style table
  entity_table <- firm_scores %>%
    dplyr::transmute(
      entity_type = "Firm",
      entity_id   = as.integer(firm_id),
      entity      = firm,
      njobs       = njobs,
      estimate    = as.numeric(firm_mean_rating),
      se          = as.numeric(se),
      rse         = as.numeric(rse),
      eb          = as.numeric(eb_hat)
    ) %>%
    dplyr::arrange(entity_id)

  # Score df: resp_id + entity<id>
  S_df <- as.data.frame(Psi_mat)
  S_df <- cbind(resp_id = rownames(Psi_mat), S_df)
  rownames(S_df) <- NULL

  list(
    fit = NULL,
    firm_table = entity_table,  # keep slot name for compat; contents are entity schema
    mats = list(
      S     = S_df,
      cov   = cov_mat,
      rcov  = rcov_mat
    )
  )
}
