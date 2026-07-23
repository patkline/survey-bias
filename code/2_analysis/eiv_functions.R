# ----------------------------------------------------------------------------------------
# Purpose: Define the shared errors-in-variables (EIV) regression functions
# used in the codebase
#   - build_noise_matrix(): assemble the measurement-error covariance matrix
#     (Sigma_error) fed to eivreg(), from the variance and covariance sheets
#   - run_eiv_one(): run one EIV specification (one LHS, one+ RHS) for one model
#   - run_eiv_suite(): loop run_eiv_one() over specifications x models
#   - write_eiv_sheet(): run a suite and write the results to a parquet sheet
#
# Created: Jordan Cammarota 2026-03-06
# Edited: Nico Rotundo 2026-06-10
# ----------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# ----------------------------------------------------------------------------------------
# compute_njobs_weighted_signal_components() --- njobs-weighted variance,
# noise, and scalar Katz pieces for one error-prone regressor i.e. one
# diagonal entry of the EIV Sigma_error
# ----------------------------------------------------------------------------------------
compute_njobs_weighted_signal_components <- function(
  # Firm-level estimate of the error-prone regressor (belief, discretion, wage gap, etc.), one entry per firm
  firm_regressor_vector,

  # Firm-level number-of-jobs weight vector, same firm order as the regressor vector
  firm_number_of_jobs_vector,

  # Firm-level robust covariance matrix, rows/columns in the same firm order
  firm_robust_covariance_matrix
) {

  # Check the regressor and weight vectors share the covariance matrix dimension, none missing
  stopifnot(
    length(firm_regressor_vector) == nrow(firm_robust_covariance_matrix),
    length(firm_number_of_jobs_vector) == nrow(firm_robust_covariance_matrix),
    nrow(firm_robust_covariance_matrix) == ncol(firm_robust_covariance_matrix),
    !anyNA(firm_regressor_vector), !anyNA(firm_number_of_jobs_vector), !anyNA(firm_robust_covariance_matrix)
  )

  # Normalize the job weights to sum to one
  firm_weight_vector <- firm_number_of_jobs_vector / sum(firm_number_of_jobs_vector)

  # Center the regressor estimate at its weighted mean across firms
  firm_regressor_centered_vector <- firm_regressor_vector - sum(firm_weight_vector * firm_regressor_vector)

  # Weighted variance of the centered regressor estimate across firms
  njobs_weighted_variance_across_firms <- sum(firm_weight_vector * firm_regressor_centered_vector^2)

  # Weighted mean squared standard error across firms i.e. the raw weighted noise
  njobs_weighted_noise_across_firms <- sum(firm_weight_vector * diag(firm_robust_covariance_matrix))

  # Weighted unbiased signal variance estimate across firms i.e. weighted variance minus weighted noise
  njobs_weighted_unbiased_signal_variance_across_firms <- njobs_weighted_variance_across_firms - njobs_weighted_noise_across_firms

  # Job-weighted centered regressor estimate i.e. the weight times the centered estimate for each firm
  firm_weighted_regressor_centered_vector <- firm_weight_vector * firm_regressor_centered_vector

  # Sampling variance of the weighted unbiased signal variance estimate
  njobs_weighted_signal_variance_sampling_variance <-
    4 * sum(firm_weighted_regressor_centered_vector * (firm_robust_covariance_matrix %*% firm_weighted_regressor_centered_vector)) -
    2 * sum((firm_weight_vector * firm_robust_covariance_matrix) * t(firm_weight_vector * firm_robust_covariance_matrix))

  # Katz-corrected signal variance across firms
  njobs_weighted_katz_signal_variance_across_firms <- katz_correct(
    njobs_weighted_unbiased_signal_variance_across_firms,
    njobs_weighted_signal_variance_sampling_variance
  )

  # Katz measurement-error variance across firms i.e. weighted variance minus the Katz-corrected signal variance
  njobs_weighted_katz_noise_across_firms <- njobs_weighted_variance_across_firms - njobs_weighted_katz_signal_variance_across_firms

  # Return NA when the Katz noise is non-finite or non-positive (signal swamped by noise on a small subsample); eivreg rejects an NA Sigma_error so a cell actually used still fails loudly
  if (!is.finite(njobs_weighted_katz_noise_across_firms) || njobs_weighted_katz_noise_across_firms <= 0) {
    njobs_weighted_katz_noise_across_firms <- NA_real_
  }

  # Return every component used by the scalar Katz diagonal.
  list(
    variance_njobs_weighted = njobs_weighted_variance_across_firms,
    noise_njobs_weighted = njobs_weighted_noise_across_firms,
    sigma2_hat_njobs_weighted = njobs_weighted_unbiased_signal_variance_across_firms,
    Vhat_njobs_weighted = njobs_weighted_signal_variance_sampling_variance,
    signal_njobs_weighted_katz = njobs_weighted_katz_signal_variance_across_firms,
    noise_njobs_weighted_katz = njobs_weighted_katz_noise_across_firms
  )
}

# ----------------------------------------------------------------------------------------
# compute_njobs_weighted_katz_noise() --- backwards-compatible scalar helper
# ----------------------------------------------------------------------------------------
compute_njobs_weighted_katz_noise <- function(
  firm_regressor_vector,
  firm_number_of_jobs_vector,
  firm_robust_covariance_matrix
) {
  compute_njobs_weighted_signal_components(
    firm_regressor_vector = firm_regressor_vector,
    firm_number_of_jobs_vector = firm_number_of_jobs_vector,
    firm_robust_covariance_matrix = firm_robust_covariance_matrix
  )$noise_njobs_weighted_katz
}

# ----------------------------------------------------------------------------------------
# Matrix helpers for the multivariate Katz correction.
# ----------------------------------------------------------------------------------------
symmetrize_matrix <- function(x) {
  x <- as.matrix(x)
  (x + t(x)) / 2
}

is_positive_definite_matrix <- function(x, tol = 1e-10) {
  x <- symmetrize_matrix(x)
  all(is.finite(x)) && min(eigen(x, symmetric = TRUE, only.values = TRUE)$values) > tol
}

is_positive_semidefinite_matrix <- function(x, tol = 1e-10) {
  x <- symmetrize_matrix(x)
  all(is.finite(x)) && min(eigen(x, symmetric = TRUE, only.values = TRUE)$values) >= -tol
}

make_positive_semidefinite_matrix <- function(x, floor_value = 1e-10) {
  x <- symmetrize_matrix(x)
  eig <- eigen(x, symmetric = TRUE)
  out <- eig$vectors %*% diag(pmax(eig$values, floor_value), nrow = length(eig$values)) %*% t(eig$vectors)
  dimnames(out) <- dimnames(x)
  symmetrize_matrix(out)
}

matrix_square_root <- function(x, inverse = FALSE, floor_value = 1e-10) {
  x <- symmetrize_matrix(x)
  eig <- eigen(x, symmetric = TRUE)
  vals <- pmax(eig$values, floor_value)
  vals <- if (isTRUE(inverse)) 1 / sqrt(vals) else sqrt(vals)
  out <- eig$vectors %*% diag(vals, nrow = length(vals)) %*% t(eig$vectors)
  dimnames(out) <- dimnames(x)
  symmetrize_matrix(out)
}

vech_matrix <- function(x) {
  x <- as.matrix(x)
  x[lower.tri(x, diag = TRUE)]
}

unvech_matrix <- function(x, names = NULL) {
  x <- as.numeric(x)
  k <- (sqrt(8 * length(x) + 1) - 1) / 2
  if (abs(k - round(k)) > 1e-8) {
    stop("unvech_matrix(): vector length is not triangular.", call. = FALSE)
  }
  k <- as.integer(round(k))
  out <- matrix(0, nrow = k, ncol = k, dimnames = list(names, names))
  out[lower.tri(out, diag = TRUE)] <- x
  out <- out + t(out) - diag(diag(out), nrow = k)
  out
}

weighted_covariance_matrix <- function(data, vars, weights) {
  x <- as.matrix(data[, vars, drop = FALSE])
  storage.mode(x) <- "double"
  weights <- as.numeric(weights)
  weights <- weights / sum(weights)
  x_centered <- sweep(x, 2, colSums(x * weights), "-")
  out <- crossprod(x_centered, x_centered * weights)
  dimnames(out) <- list(vars, vars)
  symmetrize_matrix(out)
}

estimate_signal_covariance_vcov <- function(observed_x, raw_noise_matrix, weights) {
  # First-order delta-method covariance for vech(Sigma_hat), using the
  # average RHS measurement-error covariance as the per-entity error covariance.
  observed_x <- as.matrix(observed_x)
  raw_noise_matrix <- symmetrize_matrix(raw_noise_matrix)
  weights <- as.numeric(weights)
  weights <- weights / sum(weights)

  k <- ncol(observed_x)
  var_names <- colnames(observed_x)
  if (is.null(var_names)) var_names <- paste0("x", seq_len(k))

  x_centered <- sweep(observed_x, 2, colSums(observed_x * weights), "-")
  centering_matrix <- diag(weights, nrow = length(weights)) - tcrossprod(weights)

  vech_positions <- which(lower.tri(matrix(0, k, k), diag = TRUE), arr.ind = TRUE)
  n_elements <- nrow(vech_positions)

  gradients <- vector("list", n_elements)
  element_names <- character(n_elements)

  for (element_index in seq_len(n_elements)) {
    row_index <- vech_positions[element_index, "row"]
    col_index <- vech_positions[element_index, "col"]

    gradient_matrix <- matrix(0, nrow = nrow(x_centered), ncol = k)
    if (row_index == col_index) {
      gradient_matrix[, row_index] <- 2 * as.numeric(centering_matrix %*% x_centered[, row_index])
    } else {
      gradient_matrix[, row_index] <- as.numeric(centering_matrix %*% x_centered[, col_index])
      gradient_matrix[, col_index] <- as.numeric(centering_matrix %*% x_centered[, row_index])
    }

    gradients[[element_index]] <- gradient_matrix
    element_names[element_index] <- paste(var_names[row_index], var_names[col_index], sep = "__")
  }

  signal_vcov <- matrix(0, nrow = n_elements, ncol = n_elements,
                        dimnames = list(element_names, element_names))

  for (i in seq_len(n_elements)) {
    for (j in i:n_elements) {
      covariance_value <- sum(rowSums((gradients[[i]] %*% raw_noise_matrix) * gradients[[j]]))
      signal_vcov[i, j] <- covariance_value
      signal_vcov[j, i] <- covariance_value
    }
  }

  make_positive_semidefinite_matrix(signal_vcov, floor_value = 0)
}

draw_multivariate_normal <- function(n, mean, vcov) {
  mean <- as.numeric(mean)
  vcov <- make_positive_semidefinite_matrix(vcov, floor_value = 0)
  eig <- eigen(vcov, symmetric = TRUE)
  transform <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = length(eig$values))
  draws <- matrix(stats::rnorm(n * length(mean)), nrow = n)
  sweep(draws %*% t(transform), 2, mean, "+")
}

multivariate_katz_signal_mean <- function(
  signal_hat,
  signal_vcov,
  draws = as.integer(Sys.getenv("EIV_MULTIVARIATE_KATZ_DRAWS", unset = "50000")),
  target_accepts = as.integer(Sys.getenv("EIV_MULTIVARIATE_KATZ_TARGET_ACCEPTS", unset = "5000")),
  batch_size = as.integer(Sys.getenv("EIV_MULTIVARIATE_KATZ_BATCH_SIZE", unset = "10000")),
  seed = as.integer(Sys.getenv("EIV_MULTIVARIATE_KATZ_SEED", unset = "1961")),
  pd_tol = 1e-10
) {
  signal_hat <- symmetrize_matrix(signal_hat)
  signal_vcov <- make_positive_semidefinite_matrix(signal_vcov, floor_value = 0)

  if (ncol(signal_hat) < 2L ||
      (is_positive_definite_matrix(signal_hat, tol = pd_tol) && max(abs(signal_vcov)) < 1e-16)) {
    return(signal_hat)
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  mean_vector <- vech_matrix(signal_hat)
  accepted_sum <- rep(0, length(mean_vector))
  accepted_count <- 0L
  drawn_count <- 0L
  max_draws <- max(draws, target_accepts)

  while (drawn_count < max_draws && accepted_count < target_accepts) {
    current_batch_size <- min(batch_size, max_draws - drawn_count)
    current_draws <- draw_multivariate_normal(current_batch_size, mean_vector, signal_vcov)
    drawn_count <- drawn_count + current_batch_size

    for (draw_index in seq_len(nrow(current_draws))) {
      candidate <- unvech_matrix(current_draws[draw_index, ], names = colnames(signal_hat))
      if (is_positive_definite_matrix(candidate, tol = pd_tol)) {
        accepted_sum <- accepted_sum + current_draws[draw_index, ]
        accepted_count <- accepted_count + 1L
      }
    }
  }

  if (accepted_count == 0L) {
    warning("multivariate_katz_signal_mean(): no positive-definite draws accepted; using nearest positive-definite signal matrix.", call. = FALSE)
    return(make_positive_semidefinite_matrix(signal_hat, floor_value = pd_tol))
  }

  out <- unvech_matrix(accepted_sum / accepted_count, names = colnames(signal_hat))
  attr(out, "multivariate_katz_acceptance_rate") <- accepted_count / drawn_count
  attr(out, "multivariate_katz_draws") <- drawn_count
  attr(out, "multivariate_katz_accepts") <- accepted_count
  out
}

constrain_signal_inside_observed_covariance <- function(signal_matrix, observed_covariance_matrix,
                                                        eigen_floor = 1e-8) {
  signal_matrix <- symmetrize_matrix(signal_matrix)
  observed_covariance_matrix <- make_positive_semidefinite_matrix(
    observed_covariance_matrix,
    floor_value = eigen_floor
  )

  observed_sqrt <- matrix_square_root(observed_covariance_matrix, inverse = FALSE, floor_value = eigen_floor)
  observed_inv_sqrt <- matrix_square_root(observed_covariance_matrix, inverse = TRUE, floor_value = eigen_floor)

  relative_signal <- symmetrize_matrix(observed_inv_sqrt %*% signal_matrix %*% observed_inv_sqrt)
  relative_eig <- eigen(relative_signal, symmetric = TRUE)
  clipped_values <- pmin(pmax(relative_eig$values, eigen_floor), 1 - eigen_floor)
  constrained_relative_signal <- relative_eig$vectors %*%
    diag(clipped_values, nrow = length(clipped_values)) %*%
    t(relative_eig$vectors)

  out <- observed_sqrt %*% constrained_relative_signal %*% observed_sqrt
  dimnames(out) <- dimnames(signal_matrix)
  symmetrize_matrix(out)
}

compute_multivariate_katz_noise_matrix <- function(observed_covariance_matrix,
                                                   raw_noise_matrix,
                                                   weights,
                                                   observed_x,
                                                   pd_tol = 1e-10) {
  observed_covariance_matrix <- symmetrize_matrix(observed_covariance_matrix)
  raw_noise_matrix <- symmetrize_matrix(raw_noise_matrix)

  if (anyNA(observed_covariance_matrix) || anyNA(raw_noise_matrix) ||
      !all(is.finite(observed_covariance_matrix)) || !all(is.finite(raw_noise_matrix))) {
    return(NULL)
  }

  if (!is_positive_semidefinite_matrix(raw_noise_matrix, tol = 1e-8)) {
    raw_noise_matrix <- make_positive_semidefinite_matrix(raw_noise_matrix, floor_value = pd_tol)
  }

  raw_signal_matrix <- symmetrize_matrix(observed_covariance_matrix - raw_noise_matrix)
  signal_vcov <- estimate_signal_covariance_vcov(
    observed_x = observed_x,
    raw_noise_matrix = raw_noise_matrix,
    weights = weights
  )

  katz_signal_matrix <- multivariate_katz_signal_mean(
    signal_hat = raw_signal_matrix,
    signal_vcov = signal_vcov,
    pd_tol = pd_tol
  )

  constrained_signal_matrix <- constrain_signal_inside_observed_covariance(
    signal_matrix = katz_signal_matrix,
    observed_covariance_matrix = observed_covariance_matrix,
    eigen_floor = pd_tol
  )

  katz_noise_matrix <- symmetrize_matrix(observed_covariance_matrix - constrained_signal_matrix)
  if (!is_positive_semidefinite_matrix(katz_noise_matrix, tol = 1e-8)) {
    katz_noise_matrix <- make_positive_semidefinite_matrix(katz_noise_matrix, floor_value = pd_tol)
  }

  attr(katz_noise_matrix, "multivariate_katz_acceptance_rate") <- attr(katz_signal_matrix, "multivariate_katz_acceptance_rate")
  attr(katz_noise_matrix, "multivariate_katz_draws") <- attr(katz_signal_matrix, "multivariate_katz_draws")
  attr(katz_noise_matrix, "multivariate_katz_accepts") <- attr(katz_signal_matrix, "multivariate_katz_accepts")

  katz_noise_matrix
}

# ----------------------------------------------------------------------------------------
# build_noise_matrix() --- assemble the sampling-noise variance-covariance
# matrix for one model from the variance and covariance sheets
#   - one row/column per belief measure with,
#       i. noise variances on the diagonal 
#       ii. noise covariances off the diagonal
# ----------------------------------------------------------------------------------------
build_noise_matrix <- function(
  # Variance sheet dataframe; one row per subset x model x belief measure with its noise variance
  variance_df,

  # Covariance sheet dataframe; one row per subset x model x belief-measure pair with its noise covariance
  covariance_df,

  # Belief measures to include as rows/columns; NULL = every measure present in either sheet
  outcomes = NULL,

  # Subset to build the matrix for (e.g., "all", "subset97")
  subset_value = "all",

  # Model to build the matrix for (e.g., OLS, Borda, etc...)
  model_value  = "OL",

  # Column name in both the variance and covariance sheets that identifies the subset
  subset_col = "subset",

  # Column name in both the variance and covariance sheets that identifies the model
  model_col  = "model",

  # Column name in variance_df that identifies the belief measure
  outcome_col = "outcome",

  # Column names in covariance_df that identify the two belief measures of each pair; unrelated to the regression LHS/RHS
  lhs_col = "lhs",
  rhs_col = "rhs"
) {

  # Check the variance and covariance sheet inputs are dataframes
  stopifnot(is.data.frame(variance_df), is.data.frame(covariance_df))

  # Check the variance sheet has the subset, model, belief measure, and njobs-weighted Katz noise columns
  stopifnot(all(c(subset_col, model_col, outcome_col, "noise_njobs_weighted_katz") %in% names(variance_df)))

  # Check the covariance sheet has the subset, model, belief-measure pair, and noise columns
  stopifnot(all(c(subset_col, model_col, lhs_col, rhs_col, "noise") %in% names(covariance_df)))
  
  # If no belief measures are passed, use every measure present in either sheet, dropping NA and empty names
  if (is.null(outcomes)) {

    # Define a character vector of the distinct belief measure names in the variance sheet
    belief_measures_in_variance_df <- unique(variance_df[[outcome_col]])

    # Define a character vector of the distinct belief measure names in the stacked pair columns of the covariance sheet
    belief_measures_in_covariance_df <- unique(c(covariance_df[[lhs_col]], covariance_df[[rhs_col]]))

    # Define the belief measure universe, i.e., the sorted union of the two vectors
    outcomes <- sort(unique(c(belief_measures_in_variance_df, belief_measures_in_covariance_df)))

    # Drop NA and empty-string names
    outcomes <- outcomes[!is.na(outcomes) & nzchar(outcomes)]
  }

  # Define empty matrix (i.e., NA values) with one row and column per belief measure
  noise_variance_covariance_matrix <- matrix(NA_real_, nrow = length(outcomes), ncol = length(outcomes), dimnames = list(outcomes, outcomes))
  raw_noise_variance_covariance_matrix <- matrix(NA_real_, nrow = length(outcomes), ncol = length(outcomes), dimnames = list(outcomes, outcomes))

  # Restrict the variance sheet to the given subset x model
  noise_variance_per_belief_measure <- variance_df |> dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value)

  # Keep just the belief measure and scalar Katz/raw noise variance columns. Older
  # output sheets do not have the raw njobs-weighted column, so fall back to the
  # unweighted raw noise when needed.
  if (!("noise_njobs_weighted" %in% names(noise_variance_per_belief_measure))) {
    noise_variance_per_belief_measure$noise_njobs_weighted <- NA_real_
  }
  noise_variance_per_belief_measure <- noise_variance_per_belief_measure |>
    dplyr::select(dplyr::all_of(c(outcome_col, "noise", "noise_njobs_weighted", "noise_njobs_weighted_katz")))

  # Find each belief measure's row position in the filtered variance sheet; NA when the measure has no row
  belief_measure_row_positions <- match(outcomes, noise_variance_per_belief_measure[[outcome_col]])

  # Fill the diagonal with the njobs-weighted Katz noise variances, reordered to the matrix row order; absent measures stay NA
  diag(noise_variance_covariance_matrix) <- as.numeric(noise_variance_per_belief_measure$noise_njobs_weighted_katz[belief_measure_row_positions])
  raw_diagonal_noise <- dplyr::if_else(
    is.na(noise_variance_per_belief_measure$noise_njobs_weighted),
    noise_variance_per_belief_measure$noise,
    noise_variance_per_belief_measure$noise_njobs_weighted
  )
  diag(raw_noise_variance_covariance_matrix) <- as.numeric(raw_diagonal_noise[belief_measure_row_positions])

  # Restrict the covariance sheet to the given subset x model
  noise_covariance_per_belief_measure_pair <- covariance_df |> dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value)

  # Keep just the two belief-measure pair columns and the raw noise covariance
  # columns. Older output sheets do not have the raw njobs-weighted column.
  if (!("noise_njobs_weighted" %in% names(noise_covariance_per_belief_measure_pair))) {
    noise_covariance_per_belief_measure_pair$noise_njobs_weighted <- NA_real_
  }
  noise_covariance_per_belief_measure_pair <- noise_covariance_per_belief_measure_pair |>
    dplyr::select(dplyr::all_of(c(lhs_col, rhs_col, "noise", "noise_njobs_weighted")))

  # Fill the off-diagonals one belief-measure pair at a time
  if (nrow(noise_covariance_per_belief_measure_pair) > 0) {
    
    # Loop through each row in the pair dataframe
    for (pair_row in seq_len(nrow(noise_covariance_per_belief_measure_pair))) {
      
      # Extract the name of the first belief measure
      belief_measure_1 <- as.character(noise_covariance_per_belief_measure_pair[[lhs_col]][pair_row])
      
      # Extract the name of the second belief measure
      belief_measure_2 <- as.character(noise_covariance_per_belief_measure_pair[[rhs_col]][pair_row])
      
      # Extract the noise covariance value for the current pair 
      raw_noise_covariance_value <- as.numeric(noise_covariance_per_belief_measure_pair$noise_njobs_weighted[pair_row])
      if (is.na(raw_noise_covariance_value)) {
        raw_noise_covariance_value <- as.numeric(noise_covariance_per_belief_measure_pair$noise[pair_row])
      }
      noise_covariance_value <- raw_noise_covariance_value

      # If both measures are rows/columns of the matrix, fill the pair's two symmetric cells
      if (!is.na(belief_measure_1) && !is.na(belief_measure_2) && belief_measure_1 %in% outcomes && belief_measure_2 %in% outcomes) {
        # Fill the first cell
        noise_variance_covariance_matrix[belief_measure_1, belief_measure_2] <- noise_covariance_value
        
        # Fill the second cell
        noise_variance_covariance_matrix[belief_measure_2, belief_measure_1] <- noise_covariance_value

        # Fill the raw-noise cells used by the multivariate Katz posterior step.
        raw_noise_variance_covariance_matrix[belief_measure_1, belief_measure_2] <- raw_noise_covariance_value
        raw_noise_variance_covariance_matrix[belief_measure_2, belief_measure_1] <- raw_noise_covariance_value
      }
    }
  }

  attr(noise_variance_covariance_matrix, "raw_noise_matrix") <- raw_noise_variance_covariance_matrix

  # Return the completed noise variance-covariance matrix
  noise_variance_covariance_matrix
}

# ----------------------------------------------------------------------------------------
# run_eiv_one() --- run one EIV specification for one model
# ----------------------------------------------------------------------------------------
run_eiv_one <- function(
  # One-row-per-entity (i.e., firm or industry) dataframe of model coefficients, wide by belief measure 
  coef_df_wide,

  # Sampling-noise matrix with one row/col per belief measure; rhs_vars block subset of this matrix becomes Sigma_error
  noise_mat,

  # Model to run (e.g., OLS, Borda, etc...)
  model_value,

  # LHS variable name for given regression
  lhs_var,

  # RHS variable name(s) for given regression
  rhs_vars,

  # Column name in coef_df_wide that identifies the entity
  id_col = "entity_id",      

  # Column name in coef_df_wide that identifies the model (e.g. OLS, Borda, etc...)
  model_col = "model",

  # Column name in coef_df_wide that identifies the fixed effect group (e.g. "aer_naics2"); only used if use_fe = TRUE
  fe_col = "aer_naics2",

  # Column name in coef_df_wide that contains regression weights (default: NULL, no weighting is applied)
  weights_col = NULL,

  # Column name in coef_df_wide used to cluster standard errors (default: NULL, no clustering is applied)
  cluster_col = NULL,

  # Apply the EIV finite-sample degrees-of-freedom adjustment
  cluster_df_adj = FALSE,

  # Option to run with or without fes (default: TRUE)
  use_fe = TRUE
) {

  # Check coefficient input is a dataframe
  stopifnot(is.data.frame(coef_df_wide))

  # Check model identifier column exists in coef_df_wide
  stopifnot(model_col %in% names(coef_df_wide))

  # If running the FE spec, check that the FE column exists in coef_df_wide
  if (use_fe) stopifnot(fe_col %in% names(coef_df_wide))

  # Check that the entity identifier column exists in coef_df_wide
  stopifnot(id_col %in% names(coef_df_wide))

  # Validate the optional clustering inputs
  if (!is.null(cluster_col)) {
    stopifnot(is.character(cluster_col), length(cluster_col) == 1L, nzchar(cluster_col))
  }
  stopifnot(is.logical(cluster_df_adj), length(cluster_df_adj) == 1L, !is.na(cluster_df_adj))

  # Check at least one RHS belief measure was passed 
  stopifnot(length(rhs_vars) >= 1)

  # Restrict the coefficient dataframe to the given model
  coef_df_wide <- coef_df_wide |> dplyr::filter(.data[[model_col]] == model_value)
  
  # Assign regression weights from weights_col; equal weights when NULL
  if (!is.null(weights_col)) {
    coef_df_wide[["weight"]] <- coef_df_wide[[weights_col]]
  } else {
    coef_df_wide[["weight"]] <- 1
  }
  
  # Assign required regression variables to a vector 
  required_columns <- unique(c(
    id_col,
    if (use_fe) fe_col,
    if (!is.null(cluster_col)) cluster_col,
    lhs_var,
    rhs_vars,
    "weight"
  ))
  
  # Stop execution if any regression columns are missing
  if (!all(required_columns %in% names(coef_df_wide))) {
    stop(
      "🪦 Error in run_eiv_one(): missing columns in coef_df_wide for model ", model_value, ": ",
      paste(setdiff(required_columns, names(coef_df_wide)), collapse = ", "),
      call. = FALSE
    )
  }

  # Define an estimation sample dataframe with just the required columns for the regression
  estimation_sample <- coef_df_wide |> dplyr::select(dplyr::all_of(required_columns)) 
    
  # Restrict the estimation sample dataframe to observations with non-missing LHS values
  estimation_sample <- estimation_sample |> dplyr::filter(!is.na(.data[[lhs_var]]))

  # If using fixed effects, restrict the estimation sample to observations with non-missing FE values
  if (use_fe) {
    estimation_sample <- estimation_sample |> dplyr::filter(!is.na(.data[[fe_col]]))
  }

  # Restrict the estimation sample to observations with a non-missing cluster
  if (!is.null(cluster_col)) {
    estimation_sample <- estimation_sample |> dplyr::filter(!is.na(.data[[cluster_col]]))
  }
  
  # Restrict the estimation sample dataframe to observations with non-missing RHS values
  for (rhs_var in rhs_vars) {
    estimation_sample <- estimation_sample |> dplyr::filter(!is.na(.data[[rhs_var]]))
  }
  
  # Stop if the noise matrix lacks row/column names since the noise variance-covariance matrix block is extracted by name
  if (is.null(dimnames(noise_mat)) || is.null(rownames(noise_mat)) || is.null(colnames(noise_mat))) {
    stop("🪦 Error in run_eiv_one(): noise_mat must have rownames/colnames equal to variable names.", call. = FALSE)
  }

  # Stop if any RHS variables are missing from the noise variance-covariance matrix
  if (!all(rhs_vars %in% rownames(noise_mat)) || !all(rhs_vars %in% colnames(noise_mat))) {
    stop(
      "🪦 Error in run_eiv_one(): RHS vars missing from noise_mat dimnames: ",
      paste(setdiff(rhs_vars, intersect(rownames(noise_mat), colnames(noise_mat))), collapse = ", "),
      call. = FALSE
    )
  }

  # Extract the noise variance-covariance matrix for the RHS variables
  rhs_vars_noise_variance_covariance_matrix <- as.matrix(noise_mat[rhs_vars, rhs_vars, drop = FALSE])

  # Extract the raw, pre-Katz noise matrix when available. The multivariate
  # Katz correction starts from the raw signal covariance and then truncates
  # the posterior to the positive-definite region.
  raw_noise_mat <- attr(noise_mat, "raw_noise_matrix")
  if (is.null(raw_noise_mat)) raw_noise_mat <- noise_mat
  rhs_vars_raw_noise_variance_covariance_matrix <- as.matrix(raw_noise_mat[rhs_vars, rhs_vars, drop = FALSE])

  # Apply the multivariate Katz correction when two or more RHS variables are
  # noisy. Zero-error controls, such as EEO-1 shares, remain outside this block.
  rhs_raw_noise_for_noisy_check <- symmetrize_matrix(rhs_vars_raw_noise_variance_covariance_matrix)
  noisy_rhs_vars <- rhs_vars[vapply(rhs_vars, function(rhs_var) {
    any(abs(rhs_raw_noise_for_noisy_check[rhs_var, ]) > 1e-12, na.rm = TRUE) ||
      any(abs(rhs_raw_noise_for_noisy_check[, rhs_var]) > 1e-12, na.rm = TRUE)
  }, logical(1))]

  multivariate_katz_applied <- FALSE
  multivariate_katz_acceptance_rate <- NA_real_
  multivariate_katz_draws <- NA_integer_
  multivariate_katz_accepts <- NA_integer_

  if (length(noisy_rhs_vars) >= 2L) {
    observed_covariance_noisy_rhs <- weighted_covariance_matrix(
      data = estimation_sample,
      vars = noisy_rhs_vars,
      weights = estimation_sample[["weight"]]
    )
    observed_x_noisy_rhs <- as.matrix(estimation_sample[, noisy_rhs_vars, drop = FALSE])
    storage.mode(observed_x_noisy_rhs) <- "double"

    multivariate_katz_noise_matrix <- compute_multivariate_katz_noise_matrix(
      observed_covariance_matrix = observed_covariance_noisy_rhs,
      raw_noise_matrix = rhs_vars_raw_noise_variance_covariance_matrix[noisy_rhs_vars, noisy_rhs_vars, drop = FALSE],
      weights = estimation_sample[["weight"]],
      observed_x = observed_x_noisy_rhs
    )

    if (!is.null(multivariate_katz_noise_matrix) &&
        !anyNA(multivariate_katz_noise_matrix) &&
        is_positive_semidefinite_matrix(multivariate_katz_noise_matrix, tol = 1e-8)) {
      rhs_vars_noise_variance_covariance_matrix[noisy_rhs_vars, noisy_rhs_vars] <- multivariate_katz_noise_matrix
      multivariate_katz_applied <- TRUE
      multivariate_katz_acceptance_rate <- attr(multivariate_katz_noise_matrix, "multivariate_katz_acceptance_rate")
      multivariate_katz_draws <- attr(multivariate_katz_noise_matrix, "multivariate_katz_draws")
      multivariate_katz_accepts <- attr(multivariate_katz_noise_matrix, "multivariate_katz_accepts")
    } else {
      warning(
        "run_eiv_one(): multivariate Katz correction failed for model = ",
        model_value, ", LHS = ", lhs_var, ", RHS = ", paste(noisy_rhs_vars, collapse = " + "),
        "; falling back to scalar-Katz noise matrix.",
        call. = FALSE
      )
    }
  }

  # Extract the formula text for the RHS variables
  rhs_formula_text <- paste(rhs_vars, collapse = " + ")

  # Additional setup for when using fixed effects
  if (use_fe) {
    
    # List the FE groups present in the estimation sample
    fe_groups <- sort(unique(estimation_sample[[fe_col]]))

    # Create a one-row-per-entity dataset (for those entities in the estimation sample) with one indicator column per FE group, that takes the value 1 if the entity belongs to that group and 0 otherwise
    fe_dummy_matrix <- sapply(fe_groups, function(fe_group) as.integer(estimation_sample[[fe_col]] == fe_group))
    
    # Rename indicator columns in fe_dummy_matrix to fe_1, fe_2, ...
    colnames(fe_dummy_matrix) <- paste0("fe_", seq_len(ncol(fe_dummy_matrix)))
    
    # Append the fe indicator columns to the estimation sample dataframe 
    estimation_sample <- dplyr::bind_cols(estimation_sample, as.data.frame(fe_dummy_matrix))
    
    # Store the names of the dummy columns as a vector 
    fe_dummy_columns <- colnames(fe_dummy_matrix)

    # Define an expanded noise variance-covariance matrix for the RHS variables with rows/columns of zeros for the fe indicators 
    rhs_vars_noise_variance_covariance_matrix_fe <- matrix(
      0,
      nrow = length(rhs_vars) + length(fe_dummy_columns),
      ncol = length(rhs_vars) + length(fe_dummy_columns),
      dimnames = list(c(rhs_vars, fe_dummy_columns), c(rhs_vars, fe_dummy_columns))
    )
    
    # Fill in the noise variance-covariance matrix for the RHS variables
    rhs_vars_noise_variance_covariance_matrix_fe[rhs_vars, rhs_vars] <- rhs_vars_noise_variance_covariance_matrix
  }
  
  # Report the EIV run
  message("🎃 Running EIV: Model = ", model_value, ", LHS = ", lhs_var, ", RHS = ", rhs_formula_text)
  
  # Run the no-FE EIV regression
  eiv_regression_result_no_fe <- tryCatch(
    eivreg(
      
      # Regression spec is LHS var regressed on concatenated RHS vars string
      stats::as.formula(paste(lhs_var, "~", rhs_formula_text)),
      
      # Data is the estimation sample defined above 
      data = estimation_sample,

      # Noise variance-covariance matrix for the RHS variables used to account for measurement error
      Sigma_error = rhs_vars_noise_variance_covariance_matrix,
      
      # Weights for the regression
      weights = estimation_sample[["weight"]],

      # Optional clustered variance estimator and finite-sample adjustment
      cluster_varname = cluster_col,
      df_adj = cluster_df_adj
    ),

    # On any error return NULL so the spec yields no output rows, i.e., NA in tables
    error = function(e) NULL
  )
  
  # If call specifies FEs, run the FE EIV regression
  if (use_fe) {

    # Run the FE EIV regression
    eiv_regression_result_fe <- tryCatch(
      eivreg(

        # Regression spec is LHS var regressed on concatenated RHS vars + FE indicators string; omit the intercept since the FE indicators span it
        stats::as.formula(paste(lhs_var, "~ 0 +", paste(c(rhs_vars, fe_dummy_columns), collapse = " + "))),

        # Data is the estimation sample defined above
        data = estimation_sample,

        # Expanded noise variance-covariance matrix used to account for measurement error; zero rows/columns for the FE indicators
        Sigma_error = rhs_vars_noise_variance_covariance_matrix_fe,

        # Weights for the regression
        weights = estimation_sample[["weight"]],

        # Optional clustered variance estimator and finite-sample adjustment
        cluster_varname = cluster_col,
        df_adj = cluster_df_adj
      ),

      # On any error return NULL so the spec yields no output rows, i.e., NA in tables
      error = function(e) NULL
    )
  }

  # Define list of EIV regression results that we will extract coefficients, ses, etc... from, add the no fixed effect spec to this list, and tag its coefficient with an indicator for no fe present (i.e., 1L)
  eiv_regression_results <- list(list(fit = eiv_regression_result_no_fe, coef_tag = 1L))
  
  # If call specifies FEs, add the FE spec to the regression result list, and tag its coefficient with an indicator for FE present (i.e., 2L) 
  if (use_fe) {
    eiv_regression_results <- c(eiv_regression_results, list(list(fit = eiv_regression_result_fe, coef_tag = 2L)))
  }

  # Define list to hold one output row per RHS variable per regression
  output_rows <- list()

  # Loop over the regression results and extract the output rows
  for (eiv_regression_result in eiv_regression_results) {

    # Skip regressions that errored i.e., NULL fits with no results to extract
    if (is.null(eiv_regression_result$fit) || is.null(eiv_regression_result$fit$vcov)) next

    # Extract the corrected coefficient estimates and their variance-covariance matrix
    coefficient_estimates <- stats::coef(eiv_regression_result$fit)
    coefficient_vcov <- eiv_regression_result$fit$vcov

    # Build one output row per RHS variable; NA when the coefficient or se is unavailable
    for (rhs_var in rhs_vars) {
      
      # Append one output row for the current RHS variable
      output_rows[[length(output_rows) + 1L]] <- data.frame(
        
        # Model identifier e.g., OLS, Borda
        model      = model_value,
        
        # LHS variable of the regression
        lhs        = lhs_var,

        # Full RHS formula text 
        formula    = rhs_formula_text,
        
        # RHS variable the row's coefficient is for 
        rhs        = rhs_var,

        # Coefficient tag indicating whether fixed effects are used
        coef       = eiv_regression_result$coef_tag,
        
        # Corrected coefficient estimate for this RHS variable
        sample_est = coefficient_estimates[[rhs_var]],
        
        # Standard error i.e., the square root of this RHS variable's diagonal entry of the variance-covariance matrix
        sample_se  = sqrt(coefficient_vcov[rhs_var, rhs_var]),
        
        # Number of observations in the estimation sample of the regression
        n          = nrow(estimation_sample),

        # Multivariate Katz diagnostics; TRUE only when two or more noisy RHS
        # variables were jointly corrected before calling eivreg().
        multivariate_katz = multivariate_katz_applied,
        multivariate_katz_acceptance_rate = multivariate_katz_acceptance_rate,
        multivariate_katz_draws = multivariate_katz_draws,
        multivariate_katz_accepts = multivariate_katz_accepts,
        
        # Keep character columns as strings rather than converting to factors 
        stringsAsFactors = FALSE
      )
    }
  }

  # Append the output rows into one dataframe; zero rows when all eiv regressions failed
  dplyr::bind_rows(output_rows)
}

# ----------------------------------------------------------------------------------------
# run_eiv_suite() --- loop run_eiv_one() over specifications x models
# ----------------------------------------------------------------------------------------
run_eiv_suite <- function(
  # List of regression specifications; each element is a list with $lhs (LHS variable name) and $rhs (RHS variable name(s))
  regs,

  # One-row-per-entity (i.e., firm or industry) dataframe of model coefficients, wide by belief measure
  coef_df_wide,

  # List of sampling-noise matrices keyed by model name, e.g., noise_mats_97[["OLS"]]
  noise_mats_97,

  # Models to run (default: every model with a noise matrix)
  models = names(noise_mats_97),

  # Column name in coef_df_wide that identifies the entity; passed through to run_eiv_one()
  id_col = "entity_id",

  # Column name in coef_df_wide that identifies the model; passed through to run_eiv_one()
  model_col = "model",

  # Column name in coef_df_wide that identifies the fixed effect group; passed through to run_eiv_one()
  fe_col = "aer_naics2",

  # Column name in coef_df_wide that contains regression weights; passed through to run_eiv_one()
  weights_col = NULL,

  # Column name in coef_df_wide used to cluster standard errors; passed through to run_eiv_one()
  cluster_col = NULL,

  # Apply the EIV finite-sample degrees-of-freedom adjustment; passed through to run_eiv_one()
  cluster_df_adj = FALSE,

  # Option to run with or without fes; passed through to run_eiv_one()
  use_fe = TRUE
) {
  
  # Check that the regression specification input is a list and the coefficient input is a dataframe
  stopifnot(is.list(regs), is.data.frame(coef_df_wide))
  
  # Check the noise matrix input is a list
  stopifnot(is.list(noise_mats_97))

  # Keep only models that have a noise matrix
  models <- intersect(models, names(noise_mats_97))
  
  # Stop if no models remain
  if (length(models) == 0) stop("🪦 Error in run_eiv_suite(): no models to run (models not in noise_mats_97).", call. = FALSE)

  # Define list to hold one output dataframe per specification x model run
  output_dataframes <- list()

  # Loop over the regression specifications
  for (eiv_specification in regs) {

    # Assign the LHS and RHS variable names for the current specification
    lhs_var <- eiv_specification$lhs
    rhs_vars <- eiv_specification$rhs

    # Loop over the relevant models 
    for (model_value in models) {

      # Extract the current model's noise matrix
      noise_mat <- noise_mats_97[[model_value]]
      
      # If the noise matrix is null, skip this model
      if (is.null(noise_mat)) next

      # Run the EIV regressions for the current specification x model; returns one row per RHS variable per spec i.e., no FE and FE
      eiv_output_rows <- run_eiv_one(
        coef_df_wide = coef_df_wide,
        noise_mat    = noise_mat,
        model_value  = model_value,
        lhs_var      = lhs_var,
        rhs_vars     = rhs_vars,
        id_col       = id_col,
        model_col    = model_col,
        fe_col       = fe_col,
        weights_col  = weights_col,
        cluster_col  = cluster_col,
        cluster_df_adj = cluster_df_adj,
        use_fe       = use_fe
      )

      # If the output is not null and has rows, add it to the list of output dataframes
      if (!is.null(eiv_output_rows) && nrow(eiv_output_rows) > 0) {
        output_dataframes[[length(output_dataframes) + 1L]] <- eiv_output_rows
      }
    }
  }

  # Append all output dataframes into a single dataframe
  dplyr::bind_rows(output_dataframes)
}

# ----------------------------------------------------------------------------------------
# write_eiv_sheet() --- run an EIV suite and write the results to a parquet sheet
# ----------------------------------------------------------------------------------------
write_eiv_sheet <- function(
  # Directory the parquet sheet is written into
  output_dir,

  # Name of the parquet sheet, e.g., "EIV_firm", "EIV_between"
  sheet_name = "EIV",

  # List of regression specifications; passed through to run_eiv_suite()
  regs,

  # One-row-per-entity (i.e., firm or industry) dataframe of model coefficients; passed through to run_eiv_suite()
  coef_df_wide,

  # List of sampling-noise matrices keyed by model name; passed through to run_eiv_suite()
  noise_mats_97,

  # Models to run; passed through to run_eiv_suite()
  models = names(noise_mats_97),

  # Column name in coef_df_wide that identifies the entity; passed through to run_eiv_suite()
  id_col = "entity_id",

  # Column name in coef_df_wide that identifies the model; passed through to run_eiv_suite()
  model_col = "model",

  # Column name in coef_df_wide that identifies the fixed effect group; passed through to run_eiv_suite()
  fe_col = "aer_naics2",

  # Column name in coef_df_wide that contains regression weights; passed through to run_eiv_suite()
  weights_col = NULL,

  # Column name in coef_df_wide used to cluster standard errors; passed through to run_eiv_suite()
  cluster_col = NULL,

  # Apply the EIV finite-sample degrees-of-freedom adjustment; passed through to run_eiv_suite()
  cluster_df_adj = FALSE,

  # Option to run with or without fes; passed through to run_eiv_suite()
  use_fe = TRUE
) {

  # Run the EIV regressions for all specifications x models, storing the results in a dataframe
  eiv_output_dataframe <- run_eiv_suite(
    regs         = regs,
    coef_df_wide = coef_df_wide,
    noise_mats_97 = noise_mats_97,
    models       = models,
    id_col       = id_col,
    model_col    = model_col,
    fe_col       = fe_col,
    weights_col  = weights_col,
    cluster_col  = cluster_col,
    cluster_df_adj = cluster_df_adj,
    use_fe       = use_fe
  )

  # Write the output rows to the parquet sheet
  write_parquet_sheet(output_dir, sheet_name, eiv_output_dataframe)

  # Return the output rows invisibly, i.e., not printed when the call result is unassigned
  invisible(eiv_output_dataframe)
}
