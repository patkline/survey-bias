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
  # Firm-level table carrying each firm's name and job count, keyed by firm_id
  firm_names_and_job_counts,
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
      data_wide    = respondent_firm_rankings_wide,
      id_map       = firm_names_and_job_counts,
      # Anchor the Borda scale to reference firms 38, 76, and 90
      ref_firm_ids = c(38, 76, 90)
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

  # -------------------------------------------------------------------------------------
  # Assemble the firm-level estimates from the recentered and non-recentered outputs
  # -------------------------------------------------------------------------------------
  # Bundle the recentered estimator objects under the aggregation method's model name
  centered_output_set <- list(
    # Model name to store the recentered estimates under
    model_name                = aggregation_method,
    # Recentered firm-level estimates and standard errors
    firm_estimates            = estimator_output$firm_level_rating_estimates_centered,
    # Recentered influence function matrix
    influence_function_matrix = estimator_output$influence_function_matrix_centered,
    # Recentered naive covariance matrix
    naive_covariance_matrix   = estimator_output$naive_covariance_matrix_centered,
    # Recentered robust covariance matrix
    robust_covariance_matrix  = estimator_output$robust_covariance_matrix_centered
  )

  # Bundle the non-recentered estimator objects under the "_not_recentered" model name
  raw_output_set <- list(
    # Model name to store the non-recentered estimates under
    model_name                = paste0(aggregation_method, "_not_recentered"),
    # Non-recentered firm-level estimates and standard errors
    firm_estimates            = estimator_output$firm_level_rating_estimates_raw,
    # Non-recentered influence function matrix
    influence_function_matrix = estimator_output$influence_function_matrix_raw,
    # Non-recentered naive covariance matrix
    naive_covariance_matrix   = estimator_output$naive_covariance_matrix_raw,
    # Non-recentered robust covariance matrix
    robust_covariance_matrix  = estimator_output$robust_covariance_matrix_raw
  )

  # Initialize an empty list to store each model's firm-level estimates
  model_estimates <- list()
  
  # Assemble the entity table and covariance matrices for each output set
  for (output_set in list(centered_output_set, raw_output_set)) {

    # Attach each firm's name and job count to its estimates
    firm_estimates <- output_set$firm_estimates %>%
      dplyr::left_join(
        dplyr::distinct(firm_names_and_job_counts, firm_id, firm, njobs),
        by = "firm_id"
      )

    # Order the firm ids ascending to fix the matrix row and column ordering
    firm_ids      <- as.integer(firm_estimates$firm_id)
    # Build the firm<id> labels naming the estimator's matrix columns
    firm_labels   <- paste0("firm", firm_ids)
    # Build the entity<id> labels the pipeline stores the matrices under
    entity_labels <- paste0("entity", firm_ids)

    # Subset the influence function matrix to the firm ordering
    influence_function_matrix <- as.matrix(output_set$influence_function_matrix[, firm_labels, drop = FALSE])
    # Subset the naive covariance matrix to the firm ordering
    naive_covariance_matrix   <- as.matrix(output_set$naive_covariance_matrix[firm_labels, firm_labels, drop = FALSE])
    # Subset the robust covariance matrix to the firm ordering
    robust_covariance_matrix  <- as.matrix(output_set$robust_covariance_matrix[firm_labels, firm_labels, drop = FALSE])

    # Relabel the influence function matrix columns from firm<id> to entity<id>
    colnames(influence_function_matrix) <- entity_labels
    # Relabel the naive covariance matrix rows and columns from firm<id> to entity<id>
    dimnames(naive_covariance_matrix)   <- list(entity_labels, entity_labels)
    # Relabel the robust covariance matrix rows and columns from firm<id> to entity<id>
    dimnames(robust_covariance_matrix)  <- list(entity_labels, entity_labels)

    # Initialize the empirical Bayes estimate as missing for every firm
    empirical_bayes_estimate <- rep(NA_real_, nrow(firm_estimates))
    # Flag the firms with a finite estimate and a positive robust standard error
    firms_with_positive_robust_se <- is.finite(firm_estimates$firm_mean_rating) & is.finite(firm_estimates$robust_se) & firm_estimates$robust_se > 0
    # Shrink the qualifying firms' estimates with two-step empirical Bayes when at least two qualify
    if (sum(firms_with_positive_robust_se) >= 2) {
      # Fit the two-step empirical Bayes model on the qualifying firms
      empirical_bayes_fit <- eb_two_step(
        theta_hat = firm_estimates$firm_mean_rating[firms_with_positive_robust_se],
        s         = pmax(firm_estimates$robust_se[firms_with_positive_robust_se], 1e-8)
      )
      # Place the posterior means back at the qualifying firms
      empirical_bayes_estimate[firms_with_positive_robust_se] <- empirical_bayes_fit$theta_eb
    }

    # Build the firm table in the entity-column layout the pipeline stores
    entity_table <- firm_estimates %>%
      dplyr::transmute(
        # Label every row as a firm entity
        entity_type = "Firm",
        # Firm identifier
        entity_id   = as.integer(firm_id),
        # Firm name
        entity      = firm,
        # Firm job count
        njobs       = njobs,
        # Firm mean rating estimate
        estimate    = as.numeric(firm_mean_rating),
        # Naive standard error of the estimate
        se          = as.numeric(naive_se),
        # Robust standard error of the estimate
        rse         = as.numeric(robust_se),
        # Empirical Bayes posterior mean of the estimate
        eb          = as.numeric(empirical_bayes_estimate)
      ) %>%
      # Order the table by firm identifier
      dplyr::arrange(entity_id)

    # Convert the influence function matrix to a data frame
    influence_function_data_frame <- as.data.frame(influence_function_matrix)
    # Prepend the respondent identifier as a column
    influence_function_data_frame <- cbind(resp_id = rownames(influence_function_matrix), influence_function_data_frame)
    # Drop the row names now that the respondent id is a column
    rownames(influence_function_data_frame) <- NULL

    # Store this model's estimates under its model name
    model_estimates[[output_set$model_name]] <- list(
      # No fitted model object is returned
      fit        = NULL,
      # Entity table of firm-level estimates and standard errors
      firm_table = entity_table,
      # Influence function and covariance matrices
      mats       = list(
        # Influence function data frame keyed by respondent
        S    = influence_function_data_frame,
        # Naive covariance matrix
        cov  = naive_covariance_matrix,
        # Robust covariance matrix
        rcov = robust_covariance_matrix
      )
    )
  }

  # Return each model's firm-level estimates keyed by model name
  model_estimates
}
