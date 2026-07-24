# ----------------------------------------------------------------------------------------
# Purpose: Aggregate individual respondents' firm ratings into firm-level
# estimates --- for each firm and rating variable compute,
#   - mean rating across respondents
#   - the naive and robust standard error of the mean rating
#   - the across-firm robust covariance matrix of those mean-rating estimates
#
# Created: Jordan Cammarota 2026-03-06
# Edited: Nico Rotundo 2026-06-30
# ----------------------------------------------------------------------------------------
compute_firm_mean_ratings <- function(
  # Long data frame, one row per respondent x firm scored
  respondent_firm_scores,
  # Name of the respondent identifier variable in respondent_firm_scores
  respondent_id_variable_name = "resp_id",
  # Name of the firm identifier variable in respondent_firm_scores
  firm_id_variable_name = "firm_id",
  # Name of the score variable in respondent_firm_scores
  rating_variable_name = "B",
  # Optional nonnegative aggregation weight. NULL gives every respondent-firm
  # score weight one; Borda passes the eligible-opponent count e_ij.
  weight_variable_name = NULL
) {
  # -------------------------------------------------------------------------------------
  # Validate inputs and fix the respondent and firm ordering
  # -------------------------------------------------------------------------------------
  # Require the identifier and score variables to be present
  stopifnot(all(c(respondent_id_variable_name, firm_id_variable_name, rating_variable_name) %in% names(respondent_firm_scores)))

  # Require (respondent, firm) to uniquely identify a row, so each rating maps to one matrix cell
  stopifnot(!anyDuplicated(respondent_firm_scores[c(respondent_id_variable_name, firm_id_variable_name)]))

  # Fail rather than silently dropping a missing score from the numerator while
  # retaining it in the respondent count or weight denominator
  rating_values <- respondent_firm_scores[[rating_variable_name]]
  stopifnot(!anyNA(rating_values))
  rating_values <- suppressWarnings(as.numeric(rating_values))
  stopifnot(
    length(rating_values) == nrow(respondent_firm_scores),
    all(is.finite(rating_values))
  )
  respondent_firm_scores[[rating_variable_name]] <- rating_values

  # Use unit weights for the ordinary mean and explicit eligible-opponent
  # weights for the corrected Borda ratio-of-sums estimator
  if (is.null(weight_variable_name)) {
    aggregation_weights <- rep(1, nrow(respondent_firm_scores))
  } else {
    stopifnot(
      length(weight_variable_name) == 1L,
      weight_variable_name %in% names(respondent_firm_scores)
    )
    aggregation_weights <- suppressWarnings(as.numeric(respondent_firm_scores[[weight_variable_name]]))
  }
  stopifnot(
    length(aggregation_weights) == nrow(respondent_firm_scores),
    !anyNA(aggregation_weights),
    all(aggregation_weights >= 0),
    all(is.finite(aggregation_weights))
  )
  respondent_firm_scores$.firm_mean_weight <- aggregation_weights

  # Store distinct respondent ids and sort in ascending order
  respondent_ids <- sort(unique(respondent_firm_scores[[respondent_id_variable_name]]))
  
  # Store distinct firm ids and sort in ascending order
  firm_ids <- sort(unique(respondent_firm_scores[[firm_id_variable_name]]))
  
  # -------------------------------------------------------------------------------------
  # Compute each firm's mean rating and the naive standard error of that 
  # mean
  # -------------------------------------------------------------------------------------
  # Rename the firm identifier to firm_id
  collapsed_firm_ratings <- respondent_firm_scores |> dplyr::rename(firm_id = !!firm_id_variable_name)

  # Collapse to one row per firm: respondent count, total aggregation weight,
  # and the weighted mean rating
  collapsed_firm_ratings <- collapsed_firm_ratings |>
    
    # Group by firm_id to compute firm-level summaries
    dplyr::group_by(firm_id) |>
    
    # Summarize to compute firm-level statistics
    dplyr::summarise(
      # Number of respondents who rated the firm
      firm_number_of_respondents = dplyr::n(),
      # Sum of respondent-firm aggregation weights for this firm
      firm_sum_of_weights = sum(.data$.firm_mean_weight),
      # Firm's weighted mean rating across respondents. With Borda weights this
      # is algebraically the pooled-wins / pooled-eligible-comparisons estimator.
      firm_mean_rating = sum(.data$.firm_mean_weight * .data[[rating_variable_name]]) / firm_sum_of_weights,
      # Ungroup to a one-row-per-firm data frame
      .groups = "drop"
    )

  # Every retained firm must have a well-defined weighted mean
  stopifnot(
    all(is.finite(collapsed_firm_ratings$firm_sum_of_weights)),
    all(collapsed_firm_ratings$firm_sum_of_weights > 0),
    all(is.finite(collapsed_firm_ratings$firm_mean_rating))
  )

  # Sort firms ascending by id
  collapsed_firm_ratings <- collapsed_firm_ratings |> dplyr::arrange(firm_id)

  # Confirm collapsed_firm_ratings is one row per firm in ascending id order by checking it matches the firm_ids vector
  stopifnot(length(collapsed_firm_ratings$firm_id) == length(firm_ids), all(collapsed_firm_ratings$firm_id == firm_ids))

  
  # -------------------------------------------------------------------------------------
  # Set up the row and column indices and dimensions for the 
  # respondent x firm matrices
  # -------------------------------------------------------------------------------------
  # Row position of each observation's respondent within respondent_ids
  respondent_row_index <- match(respondent_firm_scores[[respondent_id_variable_name]], respondent_ids)

  # Column position of each observation's firm within firm_ids
  firm_column_index <- match(respondent_firm_scores[[firm_id_variable_name]], firm_ids)

  # Matrix row dimension: number of distinct respondents
  total_number_of_respondents_across_all_firms <- length(respondent_ids)

  # Matrix column dimension: number of distinct firms
  number_of_firms <- length(firm_ids)

  # firm<id> labels for the matrix columns
  firm_column_names <- paste0("firm", firm_ids)
  
  # -------------------------------------------------------------------------------------
  # Build the robust naive covariance matrix 
  # -------------------------------------------------------------------------------------
  # Diagonal matrix of one over each firm's total aggregation weight i.e., the
  # inverse Hessian of the weighted firm mean rating
  inverse_hessian_matrix_of_firm_mean_rating <- diag(1 / collapsed_firm_ratings$firm_sum_of_weights)
  
  # Label rows and columns of inverse hessian matrix by firm
  dimnames(inverse_hessian_matrix_of_firm_mean_rating) <- list(firm_column_names, firm_column_names)

  # Define empty respondent x firm matrix to hold each respondent's weighted
  # score residual from their firm's mean rating
  score_matrix_of_firm_mean_rating <- matrix(
    0, 
    nrow = total_number_of_respondents_across_all_firms, 
    ncol = number_of_firms, 
    dimnames = list(respondent_ids, firm_column_names)
  )

  # Compute the residual of each observed rating from its firm's weighted mean
  residual_from_firm_mean_rating  <- respondent_firm_scores[[rating_variable_name]] - collapsed_firm_ratings$firm_mean_rating[firm_column_index]

  # The weighted-mean estimating equation is
  # sum_i w_ij * (rating_ij - firm_mean_j) = 0
  weighted_residual_from_firm_mean_rating <- respondent_firm_scores$.firm_mean_weight * residual_from_firm_mean_rating
  
  # Place each weighted residual at its respondent x firm cell, leaving
  # unobserved pairs zero
  score_matrix_of_firm_mean_rating[cbind(respondent_row_index, firm_column_index)] <- weighted_residual_from_firm_mean_rating

  # Compute the cross-product of the score matrix across respondents
  score_matrix_of_firm_mean_rating_cross_product <- crossprod(score_matrix_of_firm_mean_rating)
  
  # Compute the robust covariance matrix of the firm mean ratings
  robust_covariance_matrix_raw  <- inverse_hessian_matrix_of_firm_mean_rating %*% score_matrix_of_firm_mean_rating_cross_product %*% inverse_hessian_matrix_of_firm_mean_rating
  
  # Label rows and columns of robust covariance matrix by firm
  dimnames(robust_covariance_matrix_raw) <- list(firm_column_names, firm_column_names)

  # -------------------------------------------------------------------------------------
  # Build the influence function matrix and the naive covariance matrix
  # -------------------------------------------------------------------------------------
  # Compute the influence function of each respondent on each firm's mean rating --- cross-product equals the robust covariance
  influence_function_matrix_raw <- score_matrix_of_firm_mean_rating %*% inverse_hessian_matrix_of_firm_mean_rating
  
  # Label influence function rows by respondent
  rownames(influence_function_matrix_raw) <- respondent_ids
  
  # Label influence function columns by firm
  colnames(influence_function_matrix_raw) <- firm_column_names

  # Compute the naive diagonal variance from respondent-level influence
  # contributions with the same finite-sample correction as the previous
  # unweighted sample-variance / n formula. For unit weights this reduces
  # exactly to sum(residual^2) / (n * (n - 1)).
  robust_variance_raw <- diag(robust_covariance_matrix_raw)
  naive_variance_raw <- dplyr::if_else(
    collapsed_firm_ratings$firm_number_of_respondents > 1,
    robust_variance_raw *
      collapsed_firm_ratings$firm_number_of_respondents /
      (collapsed_firm_ratings$firm_number_of_respondents - 1),
    NA_real_
  )
  naive_covariance_matrix_raw <- diag(naive_variance_raw)

  # Label the naive covariance matrix rows and columns by firm
  dimnames(naive_covariance_matrix_raw) <- list(firm_column_names, firm_column_names)
  
  # -------------------------------------------------------------------------------------
  # Recenter the firm-level objects to sum to zero across firms
  # -------------------------------------------------------------------------------------
  # Center the firm mean ratings, their covariances, and the influence functions via recenter_objects()
  recentered_objects <- recenter_objects(
    beta   = collapsed_firm_ratings$firm_mean_rating,
    S_full = influence_function_matrix_raw,
    cov = naive_covariance_matrix_raw,
    rcov = robust_covariance_matrix_raw
  )
  
  # Compute the recentered firm mean ratings
  firm_mean_rating_centered <- as.numeric(recentered_objects$beta)
  
  # Label the recentered firm mean ratings by firm
  names(firm_mean_rating_centered) <- firm_column_names
  
  # Compute the recentered naive covariance matrix
  naive_covariance_matrix_centered <- as.matrix(recentered_objects$cov)
  
  # Label the recentered naive covariance matrix rows and columns by firm
  dimnames(naive_covariance_matrix_centered) <- list(firm_column_names, firm_column_names)

  # Compute the recentered robust covariance matrix
  robust_covariance_matrix_centered <- as.matrix(recentered_objects$rcov)
  
  # Label the recentered robust covariance matrix rows and columns by firm
  dimnames(robust_covariance_matrix_centered) <- list(firm_column_names, firm_column_names)
  
  # Compute the recentered influence function matrix
  influence_function_matrix_centered <- as.matrix(recentered_objects$S)
  
  # Label the recentered influence function matrix rows by respondent
  rownames(influence_function_matrix_centered) <- respondent_ids
  
  # Label the recentered influence function matrix columns by firm
  colnames(influence_function_matrix_centered) <- firm_column_names

  # Confirm the recentered influence functions reproduce the recentered robust covariance
  stopifnot(isTRUE(all.equal(crossprod(influence_function_matrix_centered), robust_covariance_matrix_centered, tolerance = 1e-10)))
  
  # Compute the naive standard errors from the recentered naive covariance diagonal
  naive_se_centered <- sqrt(diag(naive_covariance_matrix_centered))
  
  # Compute the robust standard errors from the recentered robust covariance diagonal
  robust_se_centered <- sqrt(diag(robust_covariance_matrix_centered))
  
  # -------------------------------------------------------------------------------------
  # Assemble the returned recentered and non-recentered firm-level estimates
  # -------------------------------------------------------------------------------------
  # Compute the naive standard errors from the raw naive covariance diagonal
  naive_se_raw <- sqrt(diag(naive_covariance_matrix_raw))

  # Compute the robust standard errors from the raw robust covariance diagonal
  robust_se_raw <- sqrt(diag(robust_covariance_matrix_raw))

  # One row per firm with the recentered estimate and its naive and robust standard errors
  firm_level_rating_estimates_centered <- data.frame(
    # Firm identifier
    firm_id          = firm_ids,
    # Number of respondent-firm score observations for this firm
    firm_number_of_respondents = collapsed_firm_ratings$firm_number_of_respondents,
    # Sum of aggregation weights contributing to this firm's estimate
    firm_sum_of_weights = collapsed_firm_ratings$firm_sum_of_weights,
    # Number of distinct respondents contributing to this outcome
    total_number_of_respondents = total_number_of_respondents_across_all_firms,
    # Recentered firm mean rating
    firm_mean_rating = firm_mean_rating_centered,
    # Naive standard error of the recentered firm mean rating
    naive_se         = naive_se_centered,
    # Robust standard error of the recentered firm mean rating
    robust_se        = robust_se_centered,
    # Keep character columns as strings rather than factors
    stringsAsFactors = FALSE
  )

  # One row per firm with the non-recentered estimate and its naive and robust standard errors
  firm_level_rating_estimates_raw <- data.frame(
    # Firm identifier
    firm_id          = firm_ids,
    # Number of respondent-firm score observations for this firm
    firm_number_of_respondents = collapsed_firm_ratings$firm_number_of_respondents,
    # Sum of aggregation weights contributing to this firm's estimate
    firm_sum_of_weights = collapsed_firm_ratings$firm_sum_of_weights,
    # Number of distinct respondents contributing to this outcome
    total_number_of_respondents = total_number_of_respondents_across_all_firms,
    # Non-recentered firm mean rating
    firm_mean_rating = collapsed_firm_ratings$firm_mean_rating,
    # Naive standard error of the non-recentered firm mean rating
    naive_se         = naive_se_raw,
    # Robust standard error of the non-recentered firm mean rating
    robust_se        = robust_se_raw,
    # Keep character columns as strings rather than factors
    stringsAsFactors = FALSE
  )

  # Return the recentered and non-recentered firm estimates, influence functions, and covariances
  list(
    # Recentered firm-level estimates and standard errors, one row per firm
    firm_level_rating_estimates_centered = firm_level_rating_estimates_centered,
    # Recentered influence function matrix
    influence_function_matrix_centered = influence_function_matrix_centered,
    # Recentered naive covariance matrix
    naive_covariance_matrix_centered = naive_covariance_matrix_centered,
    # Recentered robust covariance matrix
    robust_covariance_matrix_centered = robust_covariance_matrix_centered,
    # Non-recentered firm-level estimates and standard errors, one row per firm
    firm_level_rating_estimates_raw = firm_level_rating_estimates_raw,
    # Non-recentered influence function matrix
    influence_function_matrix_raw = influence_function_matrix_raw,
    # Non-recentered naive covariance matrix
    naive_covariance_matrix_raw = naive_covariance_matrix_raw,
    # Non-recentered robust covariance matrix
    robust_covariance_matrix_raw = robust_covariance_matrix_raw
  )
}
