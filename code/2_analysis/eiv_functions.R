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
# compute_njobs_weighted_katz_noise() --- njobs-weighted Katz measurement-error variance
# for one error-prone regressor i.e. one diagonal entry of the EIV Sigma_error
# ----------------------------------------------------------------------------------------
compute_njobs_weighted_katz_noise <- function(
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

  # Katz measurement-error variance across firms i.e. weighted variance minus the Katz-corrected signal variance
  njobs_weighted_katz_noise_across_firms <- njobs_weighted_variance_across_firms - katz_correct(njobs_weighted_unbiased_signal_variance_across_firms, njobs_weighted_signal_variance_sampling_variance)

  # Check the Katz measurement-error variance is positive (PSD) and no larger than the raw weighted noise
  stopifnot(njobs_weighted_katz_noise_across_firms > 0, njobs_weighted_katz_noise_across_firms <= njobs_weighted_noise_across_firms + 1e-12)

  # Return the njobs-weighted Katz measurement-error variance
  njobs_weighted_katz_noise_across_firms
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

  # Restrict the variance sheet to the given subset x model
  noise_variance_per_belief_measure <- variance_df |> dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value)

  # Keep just the belief measure and njobs-weighted Katz noise variance columns
  noise_variance_per_belief_measure <- noise_variance_per_belief_measure |> dplyr::select(dplyr::all_of(c(outcome_col, "noise_njobs_weighted_katz")))

  # Find each belief measure's row position in the filtered variance sheet; NA when the measure has no row
  belief_measure_row_positions <- match(outcomes, noise_variance_per_belief_measure[[outcome_col]])

  # Fill the diagonal with the njobs-weighted Katz noise variances, reordered to the matrix row order; absent measures stay NA
  diag(noise_variance_covariance_matrix) <- as.numeric(noise_variance_per_belief_measure$noise_njobs_weighted_katz[belief_measure_row_positions])

  # Restrict the covariance sheet to the given subset x model
  noise_covariance_per_belief_measure_pair <- covariance_df |> dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value)

  # Keep just the two belief-measure pair columns and the noise covariance column
  noise_covariance_per_belief_measure_pair <- noise_covariance_per_belief_measure_pair |> dplyr::select(dplyr::all_of(c(lhs_col, rhs_col, "noise")))

  # Fill the off-diagonals one belief-measure pair at a time
  if (nrow(noise_covariance_per_belief_measure_pair) > 0) {
    
    # Loop through each row in the pair dataframe
    for (pair_row in seq_len(nrow(noise_covariance_per_belief_measure_pair))) {
      
      # Extract the name of the first belief measure
      belief_measure_1 <- as.character(noise_covariance_per_belief_measure_pair[[lhs_col]][pair_row])
      
      # Extract the name of the second belief measure
      belief_measure_2 <- as.character(noise_covariance_per_belief_measure_pair[[rhs_col]][pair_row])
      
      # Extract the noise covariance value for the current pair 
      noise_covariance_value <- as.numeric(noise_covariance_per_belief_measure_pair$noise[pair_row])

      # If both measures are rows/columns of the matrix, fill the pair's two symmetric cells
      if (!is.na(belief_measure_1) && !is.na(belief_measure_2) && belief_measure_1 %in% outcomes && belief_measure_2 %in% outcomes) {
        # Fill the first cell
        noise_variance_covariance_matrix[belief_measure_1, belief_measure_2] <- noise_covariance_value
        
        # Fill the second cell
        noise_variance_covariance_matrix[belief_measure_2, belief_measure_1] <- noise_covariance_value
      }
    }
  }

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
  required_columns <- unique(c(id_col, if (use_fe) fe_col, lhs_var, rhs_vars, "weight"))
  
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
      weights = estimation_sample[["weight"]]
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
        weights = estimation_sample[["weight"]]
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
    use_fe       = use_fe
  )

  # Write the output rows to the parquet sheet
  write_parquet_sheet(output_dir, sheet_name, eiv_output_dataframe)

  # Return the output rows invisibly, i.e., not printed when the call result is unassigned
  invisible(eiv_output_dataframe)
}
