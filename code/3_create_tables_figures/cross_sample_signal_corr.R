# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Cross-sample signal correlation table (Table 4) --- for each respondent split, the debiased
# correlation between the two subgroups' firm-level belief estimates, alongside a Wald test of belief
# equality
#
# Created: Jordan Cammarota
# Cleaned: Nico Rotundo 2026-06-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Load the lightweight analysis helpers needed to recompute firm estimates in
# respondent-level bootstrap draws.
source(file.path(analysis, "leave_in_connected.R"))
source(file.path(analysis, "create_wide_rankings.R"))
source(file.path(analysis, "borda_score.R"))

# Number of respondent bootstrap draws used to calibrate the Wald and CMD
# reference distributions. Set CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS=0 to
# recover the original chi-squared p-values while testing code paths.
cross_sample_signal_corr_bootstrap_reps <- as.integer(Sys.getenv(
  "CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS",
  "499"
))
cross_sample_signal_corr_bootstrap_seed <- as.integer(Sys.getenv(
  "CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_SEED",
  "123"
))
cross_sample_signal_corr_bootstrap_max_attempt_multiplier <- as.integer(Sys.getenv(
  "CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_MAX_ATTEMPT_MULTIPLIER",
  "5"
))
cross_sample_signal_corr_bootstrap_progress_interval <- as.integer(Sys.getenv(
  "CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_PROGRESS_INTERVAL",
  "25"
))

stopifnot(
  length(cross_sample_signal_corr_bootstrap_reps) == 1,
  !is.na(cross_sample_signal_corr_bootstrap_reps),
  cross_sample_signal_corr_bootstrap_reps >= 0,
  length(cross_sample_signal_corr_bootstrap_seed) == 1,
  !is.na(cross_sample_signal_corr_bootstrap_seed),
  length(cross_sample_signal_corr_bootstrap_max_attempt_multiplier) == 1,
  !is.na(cross_sample_signal_corr_bootstrap_max_attempt_multiplier),
  cross_sample_signal_corr_bootstrap_max_attempt_multiplier >= 1,
  length(cross_sample_signal_corr_bootstrap_progress_interval) == 1,
  !is.na(cross_sample_signal_corr_bootstrap_progress_interval),
  cross_sample_signal_corr_bootstrap_progress_interval >= 0
)

set.seed(cross_sample_signal_corr_bootstrap_seed)

# -----------------------------------------------------------------------------------------------------------------------------
# Define the respondent splits compared in the table, each with its two file-suffix sample names and its
# display label, in table-row order
# -----------------------------------------------------------------------------------------------------------------------------
sample_pair_list <- list(
    list(sample_1 = "Black",                   sample_2 = "White",                   row_label = "Black vs White"),
    list(sample_1 = "Female",                  sample_2 = "Male",                    row_label = "Female vs Male"),
    list(sample_1 = "Looking",                 sample_2 = "Not_Looking",             row_label = "Looking for a Job vs Not"),
    list(sample_1 = "Feared_Discrimination_1", sample_2 = "Feared_Discrimination_0", row_label = "Feared Discrimination vs Not"),
    list(sample_1 = "Age_gte40",               sample_2 = "Age_lt40",                row_label = "Age $>=$ 40 vs $<$ 40"),
    list(sample_1 = "College",                 sample_2 = "No_College",              row_label = "At Least Some College vs HS Diploma or less"),
    list(sample_1 = "Convenience",             sample_2 = "Probability",             row_label = "Convenience vs Probability"),
    list(sample_1 = "Conf_Gender_Y",           sample_2 = "Conf_Gender_N",           row_label = "Confident vs Not (Gender)"),
    list(sample_1 = "Conf_Race_Y",             sample_2 = "Conf_Race_N",             row_label = "Confident vs Not (Race)")
)

# Define vector to store every sample name
sample_vector <- character(0)

# Loop over each sample pair, and append the two sample names to the vector
for (sample_pair in sample_pair_list) {
    sample_vector <- c(sample_vector, sample_pair$sample_1, sample_pair$sample_2)
}

# Map each output-subdirectory sample name to the processed-data variable used
# to define the respondent subgroup.
sample_definition_table <- data.frame(
  sample = c(
    "Black", "White",
    "Female", "Male",
    "Looking", "Not_Looking",
    "Feared_Discrimination_1", "Feared_Discrimination_0",
    "Age_gte40", "Age_lt40",
    "College", "No_College",
    "Convenience", "Probability",
    "Conf_Gender_Y", "Conf_Gender_N",
    "Conf_Race_Y", "Conf_Race_N"
  ),
  subset_var = c(
    "race", "race",
    "gender", "gender",
    "looking_job", "looking_job",
    "fear", "fear",
    "age", "age",
    "educ", "educ",
    "sample", "sample",
    "confidence_gend", "confidence_gend",
    "confidence_race", "confidence_race"
  ),
  subset_value = c(
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    0, 1,
    1, 0,
    1, 0
  ),
  stringsAsFactors = FALSE
)

stopifnot(
  setequal(sample_vector, sample_definition_table$sample),
  !anyDuplicated(sample_definition_table$sample)
)

# Invert a covariance matrix only when it is numerically full rank.
safe_solve <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (qr(matrix_value)$rank < nrow(matrix_value)) return(NULL)
  tryCatch(solve(matrix_value), error = function(e) NULL)
}

# Wald quadratic form for a supplied difference vector and covariance matrix.
compute_wald_statistic <- function(difference_vector, covariance_matrix) {
  covariance_inverse <- safe_solve(covariance_matrix)
  if (is.null(covariance_inverse)) return(NA_real_)
  as.numeric(t(difference_vector) %*% covariance_inverse %*% difference_vector)
}

# Minimum-distance fit of the perfect-correlation null, allowing an intercept
# and positive slope between the two firm-level belief vectors.
compute_minimum_distance_fit <- function(
    belief_sample_1,
    belief_sample_2,
    robust_covariance_sample_1,
    robust_covariance_sample_2,
    starting_intercept_and_slope = NULL
) {
  weight_matrix_sample_1 <- safe_solve(robust_covariance_sample_1)
  weight_matrix_sample_2 <- safe_solve(robust_covariance_sample_2)
  if (is.null(weight_matrix_sample_1) || is.null(weight_matrix_sample_2)) return(NULL)

  if (is.null(starting_intercept_and_slope)) {
    starting_intercept_and_slope <- unname(coef(lm(belief_sample_1 ~ belief_sample_2)))
  }
  if (length(starting_intercept_and_slope) != 2 ||
      any(!is.finite(starting_intercept_and_slope))) {
    starting_intercept_and_slope <- c(mean(belief_sample_1) - mean(belief_sample_2), 1)
  }
  if (!is.finite(starting_intercept_and_slope[2]) ||
      starting_intercept_and_slope[2] <= 0) {
    starting_intercept_and_slope[2] <- 1
  }

  compute_distance <- function(intercept_and_slope) {
    perfect_fit_intercept <- intercept_and_slope[1]
    perfect_fit_slope <- intercept_and_slope[2]
    if (!is.finite(perfect_fit_intercept) ||
        !is.finite(perfect_fit_slope) ||
        perfect_fit_slope <= 0) {
      return(.Machine$double.xmax)
    }

    perfect_fit_belief_sample_2 <- tryCatch(
      solve(
        perfect_fit_slope^2 * weight_matrix_sample_1 + weight_matrix_sample_2,
        perfect_fit_slope * weight_matrix_sample_1 %*%
          (belief_sample_1 - perfect_fit_intercept) +
          weight_matrix_sample_2 %*% belief_sample_2
      ),
      error = function(e) NULL
    )
    if (is.null(perfect_fit_belief_sample_2)) return(.Machine$double.xmax)

    perfect_fit_belief_sample_2 <- as.numeric(perfect_fit_belief_sample_2)
    perfect_fit_belief_sample_1 <- perfect_fit_intercept +
      perfect_fit_slope * perfect_fit_belief_sample_2

    diff_sample_1 <- belief_sample_1 - perfect_fit_belief_sample_1
    diff_sample_2 <- belief_sample_2 - perfect_fit_belief_sample_2

    as.numeric(
      t(diff_sample_1) %*% weight_matrix_sample_1 %*% diff_sample_1 +
        t(diff_sample_2) %*% weight_matrix_sample_2 %*% diff_sample_2
    )
  }

  fit <- optim(
    par = starting_intercept_and_slope,
    fn = compute_distance,
    method = "BFGS"
  )
  if (fit$convergence != 0 || !is.finite(fit$value) || fit$par[2] <= 0) {
    fit <- optim(
      par = starting_intercept_and_slope,
      fn = compute_distance,
      method = "Nelder-Mead"
    )
  }
  if (fit$convergence != 0 || !is.finite(fit$value) || fit$par[2] <= 0) return(NULL)

  fitted_belief_sample_2 <- solve(
    fit$par[2]^2 * weight_matrix_sample_1 + weight_matrix_sample_2,
    fit$par[2] * weight_matrix_sample_1 %*% (belief_sample_1 - fit$par[1]) +
      weight_matrix_sample_2 %*% belief_sample_2
  )
  fitted_belief_sample_2 <- as.numeric(fitted_belief_sample_2)
  fitted_belief_sample_1 <- fit$par[1] + fit$par[2] * fitted_belief_sample_2

  list(
    statistic = as.numeric(fit$value),
    intercept = as.numeric(fit$par[1]),
    slope = as.numeric(fit$par[2]),
    fitted_belief_sample_1 = fitted_belief_sample_1,
    fitted_belief_sample_2 = fitted_belief_sample_2
  )
}

# Build a respondent-by-firm score matrix for one subgroup, outcome, and
# aggregation method. Bootstrap draws are then just respondent weights on this
# fixed matrix.
prepare_bootstrap_score_input <- function(sample_data, outcome, aggregation_method, firm_id_vector) {
  method_without_suffix <- sub("_not_recentered$", "", aggregation_method)
  stopifnot(method_without_suffix %in% c("OLS", "Borda"))

  prep <- suppressWarnings(prepare_pltree_data(
    data = sample_data,
    rank_col = outcome,
    subgroup_var = NULL,
    subgroup_filter = NULL
  ))

  if (method_without_suffix == "OLS") {
    scores <- prep$data_rating_long |>
      dplyr::mutate(score = 6 - .data$rating) |>
      dplyr::select(resp_id, firm_id, score)
  } else {
    scores <- compute_borda_individual_wide(
      data_wide = prep$data_wide_pltree,
      id_map = prep$id_map,
      ref_firm_ids = c(38, 76, 90)
    ) |>
      dplyr::transmute(resp_id, firm_id, score = B)
  }

  scores <- scores |>
    dplyr::mutate(
      resp_id = as.character(.data$resp_id),
      firm_id = as.integer(.data$firm_id),
      score = as.numeric(.data$score)
    ) |>
    dplyr::filter(.data$firm_id %in% firm_id_vector, is.finite(.data$score))

  if (anyDuplicated(scores[c("resp_id", "firm_id")])) {
    stop("Bootstrap score input has duplicated respondent-firm scores for ",
         outcome, " / ", aggregation_method)
  }

  respondent_ids <- sort(unique(scores$resp_id))
  score_matrix <- matrix(
    NA_real_,
    nrow = length(respondent_ids),
    ncol = length(firm_id_vector),
    dimnames = list(respondent_ids, as.character(firm_id_vector))
  )

  score_matrix[cbind(
    match(scores$resp_id, respondent_ids),
    match(scores$firm_id, firm_id_vector)
  )] <- scores$score

  if (any(colSums(is.finite(score_matrix)) == 0)) {
    stop("Bootstrap score input is missing at least one firm for ",
         outcome, " / ", aggregation_method)
  }

  list(
    respondent_ids = respondent_ids,
    score_matrix = score_matrix
  )
}

# Recompute non-recentered firm means and robust VCVs from respondent bootstrap
# weights. This is algebraically equivalent to expanding the data by the
# respondent resample, but much faster.
compute_weighted_firm_mean_result <- function(bootstrap_score_input, respondent_weights) {
  score_matrix <- bootstrap_score_input$score_matrix
  observed_score <- is.finite(score_matrix)
  respondent_weights <- as.numeric(respondent_weights)
  stopifnot(length(respondent_weights) == nrow(score_matrix))

  firm_weighted_counts <- colSums(observed_score * respondent_weights)
  if (any(!is.finite(firm_weighted_counts)) || any(firm_weighted_counts <= 0)) {
    return(NULL)
  }

  score_matrix_zero <- score_matrix
  score_matrix_zero[!observed_score] <- 0

  firm_estimates <- colSums(score_matrix_zero * respondent_weights) / firm_weighted_counts

  residual_matrix <- sweep(score_matrix_zero, 2, firm_estimates, "-")
  residual_matrix[!observed_score] <- 0
  weighted_residual_matrix <- residual_matrix * sqrt(respondent_weights)

  robust_covariance_matrix <- crossprod(weighted_residual_matrix) /
    outer(firm_weighted_counts, firm_weighted_counts)

  dimnames(robust_covariance_matrix) <- list(
    colnames(score_matrix),
    colnames(score_matrix)
  )

  list(
    firm_estimates = as.numeric(firm_estimates),
    robust_covariance_matrix = robust_covariance_matrix
  )
}

# Cache bootstrap score inputs so each sample/outcome/method matrix is prepared
# only once per table run.
bootstrap_score_input_cache <- new.env(parent = emptyenv())

get_bootstrap_score_input <- function(sample_name, outcome, aggregation_method, firm_id_vector) {
  cache_key <- paste(sample_name, outcome, aggregation_method, sep = "||")
  if (exists(cache_key, envir = bootstrap_score_input_cache, inherits = FALSE)) {
    return(get(cache_key, envir = bootstrap_score_input_cache, inherits = FALSE))
  }

  sample_definition <- sample_definition_table |>
    dplyr::filter(.data$sample == sample_name)
  stopifnot(nrow(sample_definition) == 1)

  sample_data <- bootstrap_survey_data |>
    dplyr::filter(.data[[sample_definition$subset_var]] == sample_definition$subset_value)

  score_input <- prepare_bootstrap_score_input(
    sample_data = sample_data,
    outcome = outcome,
    aggregation_method = aggregation_method,
    firm_id_vector = firm_id_vector
  )

  assign(cache_key, score_input, envir = bootstrap_score_input_cache)
  score_input
}

# Bootstrap both null distributions for one table cell.
compute_bootstrap_null_p_values <- function(
    sample_1,
    sample_2,
    outcome,
    aggregation_method,
    observed_belief_sample_1,
    observed_belief_sample_2,
    observed_wald_statistic,
    observed_cmd_statistic,
    cmd_fitted_belief_sample_1,
    cmd_fitted_belief_sample_2,
    firm_id_vector,
    bootstrap_reps,
    max_attempt_multiplier,
    progress_label = "",
    progress_interval = 25L
) {
  if (bootstrap_reps == 0) {
    return(list(
      wald_p_value = NA_real_,
      cmd_p_value = NA_real_,
      wald_reps_used = 0L,
      cmd_reps_used = 0L,
      attempts = 0L
    ))
  }

  score_input_sample_1 <- get_bootstrap_score_input(
    sample_1, outcome, aggregation_method, firm_id_vector
  )
  score_input_sample_2 <- get_bootstrap_score_input(
    sample_2, outcome, aggregation_method, firm_id_vector
  )

  null_residual_sample_1 <- observed_belief_sample_1 - cmd_fitted_belief_sample_1
  null_residual_sample_2 <- observed_belief_sample_2 - cmd_fitted_belief_sample_2

  wald_statistics <- numeric(0)
  cmd_statistics <- numeric(0)
  attempts <- 0L
  max_attempts <- max(bootstrap_reps, bootstrap_reps * max_attempt_multiplier)
  last_progress_rep <- 0L

  while ((length(wald_statistics) < bootstrap_reps ||
          length(cmd_statistics) < bootstrap_reps) &&
         attempts < max_attempts) {
    attempts <- attempts + 1L

    weights_sample_1 <- tabulate(
      sample.int(
        nrow(score_input_sample_1$score_matrix),
        size = nrow(score_input_sample_1$score_matrix),
        replace = TRUE
      ),
      nbins = nrow(score_input_sample_1$score_matrix)
    )
    weights_sample_2 <- tabulate(
      sample.int(
        nrow(score_input_sample_2$score_matrix),
        size = nrow(score_input_sample_2$score_matrix),
        replace = TRUE
      ),
      nbins = nrow(score_input_sample_2$score_matrix)
    )

    bootstrap_result_sample_1 <- compute_weighted_firm_mean_result(
      score_input_sample_1, weights_sample_1
    )
    bootstrap_result_sample_2 <- compute_weighted_firm_mean_result(
      score_input_sample_2, weights_sample_2
    )
    if (is.null(bootstrap_result_sample_1) || is.null(bootstrap_result_sample_2)) next

    if (length(wald_statistics) < bootstrap_reps) {
      bootstrap_wald_difference <-
        (bootstrap_result_sample_1$firm_estimates - observed_belief_sample_1) -
        (bootstrap_result_sample_2$firm_estimates - observed_belief_sample_2)
      bootstrap_wald_statistic <- compute_wald_statistic(
        bootstrap_wald_difference,
        bootstrap_result_sample_1$robust_covariance_matrix +
          bootstrap_result_sample_2$robust_covariance_matrix
      )
      if (is.finite(bootstrap_wald_statistic)) {
        wald_statistics <- c(wald_statistics, bootstrap_wald_statistic)
      }
    }

    if (length(cmd_statistics) < bootstrap_reps) {
      bootstrap_cmd_belief_sample_1 <-
        bootstrap_result_sample_1$firm_estimates - null_residual_sample_1
      bootstrap_cmd_belief_sample_2 <-
        bootstrap_result_sample_2$firm_estimates - null_residual_sample_2
      bootstrap_cmd_fit <- compute_minimum_distance_fit(
        belief_sample_1 = bootstrap_cmd_belief_sample_1,
        belief_sample_2 = bootstrap_cmd_belief_sample_2,
        robust_covariance_sample_1 = bootstrap_result_sample_1$robust_covariance_matrix,
        robust_covariance_sample_2 = bootstrap_result_sample_2$robust_covariance_matrix
      )
      if (!is.null(bootstrap_cmd_fit) && is.finite(bootstrap_cmd_fit$statistic)) {
        cmd_statistics <- c(cmd_statistics, bootstrap_cmd_fit$statistic)
      }
    }

    usable_reps <- min(length(wald_statistics), length(cmd_statistics))
    should_print_progress <- progress_interval > 0 &&
      usable_reps > last_progress_rep &&
      (usable_reps %% progress_interval == 0 || usable_reps == bootstrap_reps)
    if (should_print_progress) {
      message(
        "🎃 Bootstrap progress",
        if (nzchar(progress_label)) paste0(" [", progress_label, "]") else "",
        ": usable draw ", usable_reps, "/", bootstrap_reps,
        " (attempt ", attempts, ")"
      )
      last_progress_rep <- usable_reps
    }
  }

  if (length(wald_statistics) < bootstrap_reps ||
      length(cmd_statistics) < bootstrap_reps) {
    warning(
      "Bootstrap completed with fewer usable draws than requested for ",
      sample_1, " vs ", sample_2, " / ", outcome, " / ", aggregation_method,
      ". Wald draws: ", length(wald_statistics),
      "; CMD draws: ", length(cmd_statistics),
      "; attempts: ", attempts,
      call. = FALSE
    )
  }

  list(
    wald_p_value = (1 + sum(wald_statistics >= observed_wald_statistic)) /
      (1 + length(wald_statistics)),
    cmd_p_value = (1 + sum(cmd_statistics >= observed_cmd_statistic)) /
      (1 + length(cmd_statistics)),
    wald_reps_used = length(wald_statistics),
    cmd_reps_used = length(cmd_statistics),
    attempts = attempts
  )
}

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of firm-level belief estimates by sample i.e., the vectors correlated across subgroups
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store every sample's firm-level beliefs
aggregated_sample_beliefs <- data.frame()

# Loop over each sample
for (sample_name in sample_vector) {

    # Load the given sample's firm-level coefficient sheet
    sample_beliefs <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", sample_name)), "Coefficients")

    # Uniquely identified by subset x aggregation model x belief measure x entity type x entity, none missing
    stopifnot(!anyDuplicated(sample_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(sample_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]))

    # Keep firm-level observations
    sample_beliefs <- sample_beliefs |> dplyr::filter(entity_type == "Firm")

    # Keep the full-sample estimates
    sample_beliefs <- sample_beliefs |> dplyr::filter(subset == "all")

    # Keep the non-recentered OLS and Borda observations i.e., the raw firm-level beliefs
    sample_beliefs <- sample_beliefs |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

    # Keep just the two pooled belief measures
    sample_beliefs <- sample_beliefs |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Should be 164 firms x 2 aggregation methods x 2 belief measures = 656 observations remaining
    stopifnot(nrow(sample_beliefs) == 164 * 2 * 2)

    # Belief estimates should be non-missing
    stopifnot(!anyNA(sample_beliefs$estimate))

    # Keep necessary variables
    sample_beliefs <- sample_beliefs |> dplyr::select(entity_id, model, outcome, estimate)

    # Rename variables to be more descriptive
    sample_beliefs <- sample_beliefs |> dplyr::rename(firm_id = entity_id, aggregation_method = model, belief_measure = outcome, belief_estimate = estimate)

    # Define a variable to indicate the sample
    sample_beliefs <- sample_beliefs |> dplyr::mutate(sample = sample_name)

    # Place the sample variable at the beginning of the dataset
    sample_beliefs <- sample_beliefs |> dplyr::select(sample, dplyr::everything())

    # Append the sample beliefs to the aggregated dataframe
    aggregated_sample_beliefs <- rbind(aggregated_sample_beliefs, sample_beliefs)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of the Katz-corrected signal variance by sample i.e., the signal correlation denominator
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store every sample's signal variance
aggregated_sample_signal_variance <- data.frame()

# Loop over each sample
for (sample_name in sample_vector) {

    # Load in given sample's signal variance sheet
    sample_signal_variance <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", sample_name)), "variance")

    # Uniquely identified by subset x aggregation model x outcome, none missing
    stopifnot(!anyDuplicated(sample_signal_variance[c("subset", "model", "outcome")]), !anyNA(sample_signal_variance[c("subset", "model", "outcome")]))

    # Keep OLS and Borda observations
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(model %in% c("OLS", "Borda"))

    # Keep just the two pooled belief measures
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Keep the full firm sample
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(subset == "all")

    # Should be 2 aggregation methods x 2 belief measures = 4 observations remaining
    stopifnot(nrow(sample_signal_variance) == 4)

    # Keep necessary variables
    sample_signal_variance <- sample_signal_variance |> dplyr::select(model, outcome, signal)

    # Rename variables to be more descriptive
    sample_signal_variance <- sample_signal_variance |> dplyr::rename(aggregation_method = model, belief_measure = outcome, katz_corrected_signal_variance_across_firms = signal)

    # Define a variable to indicate the sample
    sample_signal_variance <- sample_signal_variance |> dplyr::mutate(sample = sample_name)

    # Place the sample variable at the beginning of the dataset
    sample_signal_variance <- sample_signal_variance |> dplyr::select(sample, dplyr::everything())

    # Append the sample signal variance to the aggregated dataframe
    aggregated_sample_signal_variance <- rbind(aggregated_sample_signal_variance, sample_signal_variance)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Compute the firm-level constants shared across all splits
# -----------------------------------------------------------------------------------------------------------------------------
# Firm ids sorted ascending, the order every belief vector and covariance matrix is aligned to
firm_id_vector <- sort(unique(aggregated_sample_beliefs$firm_id))

# Should be the 164 firms
stopifnot(length(firm_id_vector) == 164)

if (cross_sample_signal_corr_bootstrap_reps > 0) {
  bootstrap_survey_path <- file.path(processed, "long_survey_final.csv")
  if (!file.exists(bootstrap_survey_path)) {
    stop("Cannot run cross-sample bootstrap because processed data is missing: ",
         bootstrap_survey_path)
  }
  bootstrap_survey_data <- read.csv(bootstrap_survey_path, stringsAsFactors = FALSE)
  message(
    "🎃 Cross-sample signal-correlation bootstrap reps: ",
    cross_sample_signal_corr_bootstrap_reps
  )
} else {
  bootstrap_survey_data <- NULL
  message("🎃 Cross-sample signal-correlation bootstrap disabled; using chi-squared p-values.")
}

# -----------------------------------------------------------------------------------------------------------------------------
# For each split, aggregation method, and belief measure, compute the debiased signal correlation and the
# Wald test of belief equality across the two subsamples
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all correlation results
aggregated_correlation_results <- data.frame()

# Define dataframe to store observed beliefs and minimum-distance estimates
minimum_distance_observed_belief_data <- data.frame()

# Loop over each split
for (sample_pair in sample_pair_list) {
    # Loop over aggregation method, using the non-recentered i.e., raw belief estimates
    for (aggregation_method_value in c("OLS_not_recentered", "Borda_not_recentered")) {
        # Loop over each belief measure
        for (belief_measure_value in c("pooled_favor_white", "pooled_favor_male")) {

            #### Collect each subsample's inputs
            # Store each subsample's belief vector, signal variance, and robust covariance matrix
            belief_vector_by_subsample <- list()
            signal_variance_by_subsample <- list()
            robust_covariance_matrix_by_subsample <- list()

            # Loop over the two subsamples of the split
            for (subsample in c(sample_pair$sample_1, sample_pair$sample_2)) {

                # Keep this subsample's firm-level beliefs
                subsample_beliefs <- aggregated_sample_beliefs |> dplyr::filter(sample == subsample)

                # Keep this aggregation method
                subsample_beliefs <- subsample_beliefs |> dplyr::filter(aggregation_method == aggregation_method_value)

                # Keep this belief measure
                subsample_beliefs <- subsample_beliefs |> dplyr::filter(belief_measure == belief_measure_value)

                # Sort by firm_id to align with firm_id_vector
                subsample_beliefs <- subsample_beliefs |> dplyr::arrange(firm_id)

                # Check one belief row per firm
                stopifnot(nrow(subsample_beliefs) == length(firm_id_vector))

                # Check the belief firm_id order equals firm_id_vector
                stopifnot(all(subsample_beliefs$firm_id == firm_id_vector))

                # Store this subsample's belief vector
                belief_vector_by_subsample[[subsample]] <- subsample_beliefs$belief_estimate

                # Keep this subsample's signal variance
                subsample_signal_variance <- aggregated_sample_signal_variance |> dplyr::filter(sample == subsample)

                # Keep this aggregation method, dropping the suffix since the signal variance is identical across the recentered and non-recentered estimates
                subsample_signal_variance <- subsample_signal_variance |> dplyr::filter(aggregation_method == sub("_not_recentered$", "", aggregation_method_value))

                # Keep this belief measure
                subsample_signal_variance <- subsample_signal_variance |> dplyr::filter(belief_measure == belief_measure_value)

                # Should be a single row
                stopifnot(nrow(subsample_signal_variance) == 1)

                # Store this subsample's Katz-corrected signal variance
                signal_variance_by_subsample[[subsample]] <- subsample_signal_variance$katz_corrected_signal_variance_across_firms

                # Load this subsample's robust covariance sheet
                subsample_robust_covariance <- arrow::open_dataset(parquet_sheet_path(file.path(intermediate, paste0("Subset_", subsample)), "rcov"))

                # Keep the full firm sample
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(subset == "all")

                # Keep this aggregation method
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(model == aggregation_method_value)

                # Keep this belief measure
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(outcome == belief_measure_value)

                # Keep necessary variables
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::select(entity_id_i, entity_id_j, rcov)

                # Collect the filtered robust covariance rows
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::collect()

                # Should be 164 firms x 164 firms = 26896 firm pairs
                stopifnot(nrow(subsample_robust_covariance) == 164 * 164)

                # Firm-pair identifiers should uniquely identify the filtered robust covariance rows
                stopifnot(!anyDuplicated(subsample_robust_covariance[c("entity_id_i", "entity_id_j")]))

                # Firm-pair identifiers should be non-missing
                stopifnot(!anyNA(subsample_robust_covariance[c("entity_id_i", "entity_id_j")]))

                # Rename variables to be more descriptive
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::rename(firm_id_i = entity_id_i, firm_id_j = entity_id_j, robust_covariance = rcov)

                # Define a matrix of 0s to hold this subsample's robust covariance, rows and columns ordered by firm_id_vector
                firm_robust_covariance_matrix <- matrix(0, nrow = length(firm_id_vector), ncol = length(firm_id_vector), dimnames = list(as.character(firm_id_vector), as.character(firm_id_vector)))

                # Populate the robust covariance matrix from the firm-pair rows
                firm_robust_covariance_matrix[cbind(as.character(subsample_robust_covariance$firm_id_i), as.character(subsample_robust_covariance$firm_id_j))] <- subsample_robust_covariance$robust_covariance

                # Store this subsample's robust covariance matrix
                robust_covariance_matrix_by_subsample[[subsample]] <- firm_robust_covariance_matrix

            }

            #### Unweighted signal correlation
            # Center each subsample's beliefs at their unweighted mean
            belief_sample_1_centered <- belief_vector_by_subsample[[sample_pair$sample_1]] - mean(belief_vector_by_subsample[[sample_pair$sample_1]])
            belief_sample_2_centered <- belief_vector_by_subsample[[sample_pair$sample_2]] - mean(belief_vector_by_subsample[[sample_pair$sample_2]])

            # Population covariance between the two subsamples' beliefs
            belief_covariance <- mean(belief_sample_1_centered * belief_sample_2_centered)

            # Unweighted signal correlation
            signal_correlation <- belief_covariance / sqrt(signal_variance_by_subsample[[sample_pair$sample_1]] * signal_variance_by_subsample[[sample_pair$sample_2]])

            #### Wald test of belief equality
            # Belief difference across the two subsamples
            belief_difference <- belief_vector_by_subsample[[sample_pair$sample_1]] - belief_vector_by_subsample[[sample_pair$sample_2]]

            # Summed robust covariance under independence of the two samples
            summed_robust_covariance_matrix <- robust_covariance_matrix_by_subsample[[sample_pair$sample_1]] + robust_covariance_matrix_by_subsample[[sample_pair$sample_2]]

            # Wald statistic and the original large-sample chi-squared p-value
            wald_statistic <- compute_wald_statistic(
              belief_difference,
              summed_robust_covariance_matrix
            )
            stopifnot(is.finite(wald_statistic))
            wald_degrees_of_freedom <- length(firm_id_vector)
            wald_p_value_chisq <- pchisq(wald_statistic, df = wald_degrees_of_freedom, lower.tail = FALSE)

            #### Classical minimum distance test of perfect correlation
            minimum_distance_fit <- compute_minimum_distance_fit(
              belief_sample_1 = belief_vector_by_subsample[[sample_pair$sample_1]],
              belief_sample_2 = belief_vector_by_subsample[[sample_pair$sample_2]],
              robust_covariance_sample_1 = robust_covariance_matrix_by_subsample[[sample_pair$sample_1]],
              robust_covariance_sample_2 = robust_covariance_matrix_by_subsample[[sample_pair$sample_2]]
            )
            stopifnot(!is.null(minimum_distance_fit))

            # Store the minimum observed-to-perfect-fit belief distance
            minimum_distance_statistic <- minimum_distance_fit$statistic

            # Store degrees of freedom for the original chi-squared perfect-correlation test
            minimum_distance_degrees_of_freedom <- length(firm_id_vector) - 2

            # Original large-sample chi-squared p-value for the perfect-correlation test
            minimum_distance_p_value_chisq <- pchisq(
              minimum_distance_statistic,
              df = minimum_distance_degrees_of_freedom,
              lower.tail = FALSE
            )

            #### Minimum distance scatterplot data
            # Store the minimum distance intercept and slope
            minimum_distance_intercept <- minimum_distance_fit$intercept
            minimum_distance_slope <- minimum_distance_fit$slope

            # Back out the sample beliefs implied by the final minimum distance intercept and slope
            minimum_distance_implied_belief_sample_1 <- minimum_distance_fit$fitted_belief_sample_1
            minimum_distance_implied_belief_sample_2 <- minimum_distance_fit$fitted_belief_sample_2

            #### Bootstrap null p-values
            if (cross_sample_signal_corr_bootstrap_reps > 0) {
              message(
                "🎃 Bootstrapping Table 4 cell: ",
                sample_pair$row_label, " / ",
                aggregation_method_value, " / ",
                belief_measure_value
              )
            }
            bootstrap_p_values <- compute_bootstrap_null_p_values(
              sample_1 = sample_pair$sample_1,
              sample_2 = sample_pair$sample_2,
              outcome = belief_measure_value,
              aggregation_method = aggregation_method_value,
              observed_belief_sample_1 = belief_vector_by_subsample[[sample_pair$sample_1]],
              observed_belief_sample_2 = belief_vector_by_subsample[[sample_pair$sample_2]],
              observed_wald_statistic = wald_statistic,
              observed_cmd_statistic = minimum_distance_statistic,
              cmd_fitted_belief_sample_1 = minimum_distance_implied_belief_sample_1,
              cmd_fitted_belief_sample_2 = minimum_distance_implied_belief_sample_2,
              firm_id_vector = firm_id_vector,
              bootstrap_reps = cross_sample_signal_corr_bootstrap_reps,
              max_attempt_multiplier = cross_sample_signal_corr_bootstrap_max_attempt_multiplier,
              progress_label = paste(
                sample_pair$row_label,
                aggregation_method_value,
                belief_measure_value,
                sep = " / "
              ),
              progress_interval = cross_sample_signal_corr_bootstrap_progress_interval
            )

            wald_p_value <- if (is.finite(bootstrap_p_values$wald_p_value)) {
              bootstrap_p_values$wald_p_value
            } else {
              wald_p_value_chisq
            }
            minimum_distance_p_value <- if (is.finite(bootstrap_p_values$cmd_p_value)) {
              bootstrap_p_values$cmd_p_value
            } else {
              minimum_distance_p_value_chisq
            }

            # Append this cell's cross sample correlation results
            aggregated_correlation_results <- rbind(aggregated_correlation_results, data.frame(
              row_label = sample_pair$row_label,
              aggregation_method = tolower(aggregation_method_value),
              belief_measure = belief_measure_value,
              signal_correlation = signal_correlation,
              wald_statistic = wald_statistic,
              wald_degrees_of_freedom = wald_degrees_of_freedom,
              wald_p_value_chisq = wald_p_value_chisq,
              wald_p_value_bootstrap = bootstrap_p_values$wald_p_value,
              wald_p_value = wald_p_value,
              minimum_distance_statistic = minimum_distance_statistic,
              minimum_distance_degrees_of_freedom = minimum_distance_degrees_of_freedom,
              minimum_distance_p_value_chisq = minimum_distance_p_value_chisq,
              minimum_distance_p_value_bootstrap = bootstrap_p_values$cmd_p_value,
              minimum_distance_p_value = minimum_distance_p_value,
              bootstrap_reps_requested = cross_sample_signal_corr_bootstrap_reps,
              wald_bootstrap_reps_used = bootstrap_p_values$wald_reps_used,
              minimum_distance_bootstrap_reps_used = bootstrap_p_values$cmd_reps_used,
              bootstrap_attempts = bootstrap_p_values$attempts
            ))

            # Append this cell's observed beliefs and minimum-distance estimates
            minimum_distance_observed_belief_data <- rbind(minimum_distance_observed_belief_data, data.frame(
              row_label = sample_pair$row_label,
              sample_1 = sample_pair$sample_1,
              sample_2 = sample_pair$sample_2,
              aggregation_method = tolower(aggregation_method_value),
              belief_measure = belief_measure_value,
              firm_id = firm_id_vector,
              belief_sample_1 = belief_vector_by_subsample[[sample_pair$sample_1]],
              belief_sample_2 = belief_vector_by_subsample[[sample_pair$sample_2]],
              minimum_distance_implied_belief_sample_1 = minimum_distance_implied_belief_sample_1,
              minimum_distance_implied_belief_sample_2 = minimum_distance_implied_belief_sample_2,
              minimum_distance_intercept = minimum_distance_intercept,
              minimum_distance_slope = minimum_distance_slope,
              signal_correlation = signal_correlation,
              wald_p_value = wald_p_value,
              minimum_distance_p_value = minimum_distance_p_value
            ))
        }
    }
}

# Signal correlations should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$signal_correlation))

# Wald p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$wald_p_value))

# Wald p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$wald_p_value, 0, 1)))

# Minimum distance p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$minimum_distance_p_value))

# Minimum distance p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$minimum_distance_p_value, 0, 1)))

# Minimum distance observed belief data should have one row per firm in each split x aggregation-method x belief-measure cell
stopifnot(nrow(minimum_distance_observed_belief_data) == length(sample_pair_list) * 2 * 2 * length(firm_id_vector))

# Minimum distance observed belief data should have non-missing observed beliefs and minimum-distance estimates
stopifnot(!anyNA(minimum_distance_observed_belief_data[c("belief_sample_1", "belief_sample_2", "minimum_distance_intercept", "minimum_distance_slope")]))

# Number of firms in each sample x aggregation-method x belief-measure cell
number_of_firms_by_sample_cell <- aggregated_sample_beliefs |> dplyr::count(sample, aggregation_method, belief_measure, name = "number_of_firms")

# Each sample cell should contain 164 firms
stopifnot(all(number_of_firms_by_sample_cell$number_of_firms == 164))

# Number of firms reported in the table
number_of_firms_in_table <- unique(number_of_firms_by_sample_cell$number_of_firms)

# There should be a single number of firms across all table cells
stopifnot(length(number_of_firms_in_table) == 1)

# Save diagnostics with both the original chi-squared p-values and the
# bootstrap-calibrated p-values used in the LaTeX table.
write.csv(
  aggregated_correlation_results,
  file.path(tables, "cross_sample_signal_corr_ols_borda_not_recentered.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the cross-sample correlation table, with an OLS/Likert panel and a Borda panel
# -----------------------------------------------------------------------------------------------------------------------------
# Format a p-value to three decimals, printing values below 0.001 as $<$0.001
format_p_value <- function(p_value) {
  if (is.na(p_value) || !is.finite(p_value)) return("")
  if (p_value < 0.001) "$<$0.001" else formatC(p_value, digits = 3, format = "f")
}

# Open the table
latex_lines <- c(
    "  \\centering",
    "  \\begin{tabular}{lcccccc}",
    "    \\toprule",
    "    & \\multicolumn{3}{c}{Discrimination Black (Pooled)} & \\multicolumn{3}{c}{Discrimination Female (Pooled)} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "    & Corr & CMD p-value & Wald p-value & Corr & CMD p-value & Wald p-value \\\\",
    "    & & $H_0: \\rho = 1$ & $H_0: \\theta_1 = \\theta_2$ & & $H_0: \\rho = 1$ & $H_0: \\theta_1 = \\theta_2$ \\\\",
    "    \\midrule"
)

# Loop over the two aggregation-method panels
for (panel in list(list(aggregation_method = "ols_not_recentered", panel_label = "Panel A: Likert"), list(aggregation_method = "borda_not_recentered", panel_label = "Panel B: Borda"))) {

    # Separate the Borda panel from the Likert panel
    if (panel$aggregation_method == "borda_not_recentered") {
        latex_lines <- c(latex_lines, "    \\addlinespace")
    }

    # Panel header
    latex_lines <- c(latex_lines, paste0("    \\multicolumn{7}{l}{\\textbf{", panel$panel_label, "}}\\\\"))

    # Loop over the splits in table-row order
    for (sample_pair in sample_pair_list) {

        # This split's Discrimination Black (white-favoritism belief) results for this panel
        race_results <- aggregated_correlation_results |> dplyr::filter(row_label == sample_pair$row_label)
        race_results <- race_results |> dplyr::filter(aggregation_method == panel$aggregation_method)
        race_results <- race_results |> dplyr::filter(belief_measure == "pooled_favor_white")
        stopifnot(nrow(race_results) == 1)

        # This split's Discrimination Female (male-favoritism belief) results for this panel
        gender_results <- aggregated_correlation_results |> dplyr::filter(row_label == sample_pair$row_label)
        gender_results <- gender_results |> dplyr::filter(aggregation_method == panel$aggregation_method)
        gender_results <- gender_results |> dplyr::filter(belief_measure == "pooled_favor_male")
        stopifnot(nrow(gender_results) == 1)

        # Table row: split label, race correlation and p-values, gender correlation and p-values
        latex_lines <- c(latex_lines, paste0("    ", sample_pair$row_label, " & ", formatC(race_results$signal_correlation, digits = 3, format = "f"), " & ", format_p_value(race_results$minimum_distance_p_value), " & ", format_p_value(race_results$wald_p_value), " & ", formatC(gender_results$signal_correlation, digits = 3, format = "f"), " & ", format_p_value(gender_results$minimum_distance_p_value), " & ", format_p_value(gender_results$wald_p_value), " \\\\"))
    }
}

# Add number of firms row
latex_lines <- c(latex_lines, "    \\midrule", paste0("    N & ", number_of_firms_in_table, " &  &  & ", number_of_firms_in_table, " &  &  \\\\"))

# Close the table
latex_lines <- c(latex_lines, "    \\bottomrule", "  \\end{tabular}")

# Write the table
writeLines(latex_lines, file.path(tables, "cross_sample_signal_corr_ols_borda_not_recentered.tex"), useBytes = TRUE)

# Announce the written table
message("🎃 Generated cross_sample_signal_corr_ols_borda_not_recentered.tex")

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the minimum distance fitted scatterplots
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each subgroup comparison
for (subgroup_comparison in list(c("Black", "White"), c("Female", "Male"))) {
    # Loop over aggregation method
    for (aggregation_method in c("ols_not_recentered")) {
        # Loop over the belief measure on the comparison's own demographic margin
        for (belief_measure in c(Black = "pooled_favor_white", Female = "pooled_favor_male")[[subgroup_comparison[1]]]) {

            # Restrict observed belief data to the given subgroup comparison
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_data |> dplyr::filter(sample_1 == subgroup_comparison[1], sample_2 == subgroup_comparison[2])

            # Keep this aggregation method
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_scatterplot_data |> dplyr::filter(aggregation_method == .env$aggregation_method)

            # Keep this belief measure
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_scatterplot_data |> dplyr::filter(belief_measure == .env$belief_measure)

            # Should be one row per firm
            stopifnot(nrow(minimum_distance_observed_belief_scatterplot_data) == length(firm_id_vector))

            # Should be one minimum-distance intercept for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept)) == 1)

            # Should be one minimum-distance slope for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope)) == 1)

            # Should be one signal correlation for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$signal_correlation)) == 1)

            # Should be one Wald p-value for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value)) == 1)

            # Should be one minimum-distance p-value for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value)) == 1)

            # Define scatterplot of observed beliefs i.e., the first animation stage
            minimum_distance_observed_belief_scatterplot <- ggplot(minimum_distance_observed_belief_scatterplot_data, aes(x = belief_sample_2, y = belief_sample_1)) +

                # Observed firm-level belief pairs
                geom_point(color = "darkorange", size = 2.2, alpha = 0.7) +

                # Axis labels name the belief measure and subsample
                labs(
                    x = paste0(c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)")[[belief_measure]], " for ", subgroup_comparison[2]),
                    y = paste0(c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)")[[belief_measure]], " for ", subgroup_comparison[1])
                ) +

                # Theme baseline
                theme_minimal(base_size = 11) +

                # Theme adjustments
                theme(
                    # No grid lines
                    panel.grid = element_blank(),

                    # White background
                    panel.background = element_rect(fill = "white", color = NA),
                    plot.background = element_rect(fill = "white", color = NA),

                    # Bottom and left axis spines, no ticks
                    axis.line = element_line(color = "black"),
                    axis.ticks = element_blank()
                )

            # Add the minimum-distance fit to the observed scatterplot i.e., the second animation stage
            minimum_distance_fitted_belief_scatterplot <- minimum_distance_observed_belief_scatterplot +

                # Minimum-distance fitted line
                geom_abline(
                    intercept = unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept),
                    slope = unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope),
                    color = "steelblue",
                    linewidth = 0.7
                ) +

                # Minimum-distance implied firm-level belief pairs
                geom_point(aes(x = minimum_distance_implied_belief_sample_2, y = minimum_distance_implied_belief_sample_1), color = "steelblue", size = 1.4, alpha = 0.9)

            # Add the statistic annotations to the fitted scatterplot i.e., the full figure
            minimum_distance_annotated_belief_scatterplot <- minimum_distance_fitted_belief_scatterplot +

                # Minimum-distance intercept and slope annotation
                annotation_custom(
                    grid::textGrob(
                        label = bquote("CMD (intercept, slope)" == group("(", list(.(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept), digits = 3, format = "f")), .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope), digits = 3, format = "f"))), ")")),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.975, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Minimum-distance p-value annotation
                annotation_custom(
                    grid::textGrob(
                        label = if (unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value) < 0.001) bquote("CMD p-value"~(H[0]:~rho == 1) < 0.001) else bquote("CMD p-value"~(H[0]:~rho == 1) == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.930, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Signal correlation annotation
                annotation_custom(
                    grid::textGrob(
                        label = bquote("Signal corr." == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$signal_correlation), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.885, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Wald p-value annotation
                annotation_custom(
                    grid::textGrob(
                        label = if (unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value) < 0.001) bquote("Wald p-value"~(H[0]:~theta[1] == theta[2]) < 0.001) else bquote("Wald p-value"~(H[0]:~theta[1] == theta[2]) == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.840, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                )

            # Display the scatterplot in the active graphics device
            print(minimum_distance_annotated_belief_scatterplot)

            # Export the scatterplot, one file per subgroup comparison x aggregation method x belief measure
            ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, ".png")), plot = minimum_distance_annotated_belief_scatterplot, width = 10, height = 6, dpi = 300, bg = "white")

            # Export the animation stages for the race comparison, pinning both stages to the full figure's axis ranges
            if (identical(subgroup_comparison, c("Black", "White"))) {

                # Extract the full figure's panel ranges, fixing the axes across the animation stages
                minimum_distance_scatterplot_panel_ranges <- ggplot_build(minimum_distance_annotated_belief_scatterplot)$layout$panel_params[[1]]

                # Export the observed-scatter stage
                ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, "_animation_1.png")), plot = minimum_distance_observed_belief_scatterplot + coord_cartesian(xlim = minimum_distance_scatterplot_panel_ranges$x.range, ylim = minimum_distance_scatterplot_panel_ranges$y.range, expand = FALSE), width = 10, height = 6, dpi = 300, bg = "white")

                # Export the fitted-scatter stage
                ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, "_animation_2.png")), plot = minimum_distance_fitted_belief_scatterplot + coord_cartesian(xlim = minimum_distance_scatterplot_panel_ranges$x.range, ylim = minimum_distance_scatterplot_panel_ranges$y.range, expand = FALSE), width = 10, height = 6, dpi = 300, bg = "white")
            }
        }
    }
}
