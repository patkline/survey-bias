# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Cross-sample signal correlation table --- for each respondent split, the debiased
# correlation between the two subgroups' firm-level belief estimates, a Wald test of belief equality, and a
# minimum distance test of perfect correlation
#
# Created: Jordan Cammarota
# Edited: Nico Rotundo 2026-06-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Load the lightweight analysis helpers needed to recompute firm estimates in
# respondent-level bootstrap draws.
source(file.path(analysis, "create_wide_rankings.R"))
source(file.path(analysis, "borda_score.R"))
source(file.path(analysis, "katz_correct.R"))
source(file.path(analysis, "eiv_functions.R"))
source(file.path(analysis, "covariance_functions.R"))

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

# The two samples in every reported comparison are disjoint partitions. This
# justifies a zero cross-sample measurement-error covariance block in the
# multivariate delta-method calculation below.
for (sample_pair in sample_pair_list) {
  sample_pair_definitions <- sample_definition_table |>
    dplyr::filter(sample %in% c(sample_pair$sample_1, sample_pair$sample_2))
  stopifnot(
    nrow(sample_pair_definitions) == 2,
    length(unique(sample_pair_definitions$subset_var)) == 1,
    length(unique(sample_pair_definitions$subset_value)) == 2
  )
}

# Read one or more robust firm-level covariance matrices from a subgroup's
# rcov sheet, returning each matrix in the common firm order.
read_firm_robust_covariance_matrices <- function(
    subsample,
    model_values,
    outcome_value,
    firm_id_vector
) {
  covariance_rows <- arrow::open_dataset(
    parquet_sheet_path(file.path(intermediate, paste0("Subset_", subsample)), "rcov")
  ) |>
    dplyr::filter(
      subset == "all",
      model %in% model_values,
      outcome == outcome_value
    ) |>
    dplyr::select(model, entity_id_i, entity_id_j, rcov) |>
    dplyr::collect()

  expected_rows <- length(model_values) * length(firm_id_vector)^2
  stopifnot(
    nrow(covariance_rows) == expected_rows,
    !anyDuplicated(covariance_rows[c("model", "entity_id_i", "entity_id_j")]),
    !anyNA(covariance_rows[c("model", "entity_id_i", "entity_id_j", "rcov")])
  )

  matrices <- lapply(model_values, function(model_value) {
    model_rows <- covariance_rows[covariance_rows$model == model_value, , drop = FALSE]
    covariance_matrix <- matrix(
      0,
      nrow = length(firm_id_vector),
      ncol = length(firm_id_vector),
      dimnames = list(as.character(firm_id_vector), as.character(firm_id_vector))
    )
    covariance_matrix[cbind(
      as.character(model_rows$entity_id_i),
      as.character(model_rows$entity_id_j)
    )] <- model_rows$rcov
    covariance_matrix
  })
  stats::setNames(matrices, model_values)
}

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
      dplyr::mutate(
        score = 6 - .data$rating,
        aggregation_weight = 1
      ) |>
      dplyr::select(resp_id, firm_id, score, aggregation_weight)
  } else {
    scores <- compute_borda_individual_wide(
      data_wide = prep$data_wide_pltree,
      id_map = prep$id_map,
      higher_is_better = FALSE,
      ref_firm_ids = c(38, 76, 90)
    ) |>
      dplyr::transmute(
        resp_id,
        firm_id,
        score = B,
        aggregation_weight = e
      )
  }

  scores <- scores |>
    dplyr::mutate(
      resp_id = as.character(.data$resp_id),
      firm_id = as.integer(.data$firm_id),
      score = as.numeric(.data$score),
      aggregation_weight = as.numeric(.data$aggregation_weight)
    ) |>
    dplyr::filter(
      .data$firm_id %in% firm_id_vector,
      is.finite(.data$score),
      is.finite(.data$aggregation_weight),
      .data$aggregation_weight >= 0
    )

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

  aggregation_weight_matrix <- matrix(
    NA_real_,
    nrow = length(respondent_ids),
    ncol = length(firm_id_vector),
    dimnames = list(respondent_ids, as.character(firm_id_vector))
  )
  aggregation_weight_matrix[cbind(
    match(scores$resp_id, respondent_ids),
    match(scores$firm_id, firm_id_vector)
  )] <- scores$aggregation_weight

  if (any(colSums(is.finite(score_matrix)) == 0)) {
    stop("Bootstrap score input is missing at least one firm for ",
         outcome, " / ", aggregation_method)
  }

  list(
    respondent_ids = respondent_ids,
    score_matrix = score_matrix,
    aggregation_weight_matrix = aggregation_weight_matrix
  )
}

# Recompute non-recentered firm means and robust VCVs from respondent bootstrap
# weights. This is algebraically equivalent to expanding the data by the
# respondent resample, but much faster.
compute_weighted_firm_mean_result <- function(bootstrap_score_input, respondent_weights) {
  score_matrix <- bootstrap_score_input$score_matrix
  aggregation_weight_matrix <- bootstrap_score_input$aggregation_weight_matrix
  observed_score <- is.finite(score_matrix)
  respondent_weights <- as.numeric(respondent_weights)
  stopifnot(length(respondent_weights) == nrow(score_matrix))
  stopifnot(
    identical(dim(aggregation_weight_matrix), dim(score_matrix)),
    all(is.finite(aggregation_weight_matrix[observed_score])),
    all(aggregation_weight_matrix[observed_score] >= 0)
  )

  aggregation_weight_matrix_zero <- aggregation_weight_matrix
  aggregation_weight_matrix_zero[!observed_score] <- 0
  bootstrap_aggregation_weights <-
    aggregation_weight_matrix_zero * respondent_weights

  firm_weighted_counts <- colSums(bootstrap_aggregation_weights)
  if (any(!is.finite(firm_weighted_counts)) || any(firm_weighted_counts <= 0)) {
    return(NULL)
  }

  score_matrix_zero <- score_matrix
  score_matrix_zero[!observed_score] <- 0

  firm_estimates <-
    colSums(score_matrix_zero * bootstrap_aggregation_weights) /
    firm_weighted_counts

  residual_matrix <- sweep(score_matrix_zero, 2, firm_estimates, "-")
  residual_matrix[!observed_score] <- 0
  # The bootstrap expands each respondent according to respondent_weights.
  # Each replicated respondent contributes e_ij * (B_ij - mean_j) to the
  # weighted-mean estimating equation.
  weighted_residual_matrix <-
    residual_matrix * aggregation_weight_matrix_zero *
    sqrt(respondent_weights)

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
# For each split, aggregation method, and belief measure, compute the debiased signal correlation, the Wald
# test of belief equality across the two subsamples, and the minimum distance test of perfect correlation
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all correlation results
aggregated_correlation_results <- data.frame()

# Loop over each split
for (sample_pair in sample_pair_list) {
    # Loop over aggregation method, using the non-recentered i.e., raw belief estimates
    for (aggregation_method_value in c("OLS_not_recentered", "Borda_not_recentered")) {
        # Loop over each belief measure
        for (belief_measure_value in c("pooled_favor_white", "pooled_favor_male")) {

            #### Collect each subsample's inputs
            # Store each subsample's belief vector and robust covariance matrices.
            # The raw (non-recentered) matrix stays
            # on the Wald/CMD path; the recentered matrix matches the signal-
            # covariance calculation used in Section 2.
            belief_vector_by_subsample <- list()
            robust_covariance_matrix_by_subsample <- list()
            signal_robust_covariance_matrix_by_subsample <- list()

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

                signal_aggregation_method_value <- sub(
                  "_not_recentered$", "", aggregation_method_value
                )
                covariance_matrices <- read_firm_robust_covariance_matrices(
                  subsample = subsample,
                  model_values = c(
                    aggregation_method_value,
                    signal_aggregation_method_value
                  ),
                  outcome_value = belief_measure_value,
                  firm_id_vector = firm_id_vector
                )

                # Preserve the existing non-recentered covariance inputs for
                # the Wald and CMD statistics.
                robust_covariance_matrix_by_subsample[[subsample]] <-
                  covariance_matrices[[aggregation_method_value]]

                # Use the recentered covariance inputs for the cross-firm
                # signal covariance and its delta-method sampling VCV.
                signal_robust_covariance_matrix_by_subsample[[subsample]] <-
                  covariance_matrices[[signal_aggregation_method_value]]

            }

            #### Unweighted signal correlation
            # Center each subsample's beliefs at their unweighted mean
            belief_sample_1_centered <- belief_vector_by_subsample[[sample_pair$sample_1]] - mean(belief_vector_by_subsample[[sample_pair$sample_1]])
            belief_sample_2_centered <- belief_vector_by_subsample[[sample_pair$sample_2]] - mean(belief_vector_by_subsample[[sample_pair$sample_2]])

            # Population covariance between the two subsamples' beliefs
            belief_covariance <- mean(belief_sample_1_centered * belief_sample_2_centered)

            # Pairwise multivariate Katz. Since each comparison uses disjoint
            # respondent samples, the cross-sample firm-by-firm covariance
            # blocks are zero. The diagonal blocks retain the full cross-firm
            # dependence and firm-specific precision from the rcov sheets.
            number_of_firms <- length(firm_id_vector)
            zero_cross_sample_covariance <- matrix(
              0,
              nrow = number_of_firms,
              ncol = number_of_firms
            )
            signal_covariance_sample_1 <-
              signal_robust_covariance_matrix_by_subsample[[sample_pair$sample_1]]
            signal_covariance_sample_2 <-
              signal_robust_covariance_matrix_by_subsample[[sample_pair$sample_2]]
            clustered_signal_inputs <- list(
              beta1 = belief_vector_by_subsample[[sample_pair$sample_1]],
              beta2 = belief_vector_by_subsample[[sample_pair$sample_2]],
              C = rbind(
                cbind(signal_covariance_sample_1, zero_cross_sample_covariance),
                cbind(zero_cross_sample_covariance, signal_covariance_sample_2)
              ),
              J = number_of_firms
            )
            signal_vcov <- compute_clustered_signal_vcov(
              weights = rep(1 / number_of_firms, number_of_firms),
              include_cross_outcome = FALSE,
              prepared_inputs = clustered_signal_inputs
            )
            observed_covariance_matrix <- matrix(
              c(
                mean(belief_sample_1_centered^2), belief_covariance,
                belief_covariance, mean(belief_sample_2_centered^2)
              ),
              nrow = 2,
              byrow = TRUE,
              dimnames = list(
                c(sample_pair$sample_1, sample_pair$sample_2),
                c(sample_pair$sample_1, sample_pair$sample_2)
              )
            )
            raw_noise_matrix <- matrix(
              c(
                mean(diag(signal_covariance_sample_1)), 0,
                0, mean(diag(signal_covariance_sample_2))
              ),
              nrow = 2,
              byrow = TRUE,
              dimnames = dimnames(observed_covariance_matrix)
            )
            multivariate_katz_result <- compute_multivariate_katz_signal_correlation(
              observed_covariance_matrix = observed_covariance_matrix,
              raw_noise_matrix = raw_noise_matrix,
              signal_vcov = signal_vcov
            )
            if (is.null(multivariate_katz_result)) {
              stop(
                "Multivariate Katz correction failed for ",
                sample_pair$row_label, " / ", aggregation_method_value,
                " / ", belief_measure_value,
                call. = FALSE
              )
            }
            signal_correlation <- multivariate_katz_result$signal_correlation
            stopifnot(dplyr::between(signal_correlation, -1, 1))

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

            # Back out the sample beliefs implied by the final minimum distance intercept and slope
            minimum_distance_implied_belief_sample_1 <- minimum_distance_fit$fitted_belief_sample_1
            minimum_distance_implied_belief_sample_2 <- minimum_distance_fit$fitted_belief_sample_2

            #### Bootstrap null p-values
            if (cross_sample_signal_corr_bootstrap_reps > 0) {
              message(
                "🎃 Bootstrapping Table 5 cell: ",
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
              wald_p_value = wald_p_value,
              minimum_distance_p_value = minimum_distance_p_value
            ))
        }
    }
}

# Signal correlations should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$signal_correlation))

# Pairwise multivariate-Katz signal correlations must respect the correlation
# bounds by construction.
stopifnot(all(dplyr::between(
  aggregated_correlation_results$signal_correlation,
  -1,
  1
)))

# Wald p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$wald_p_value))

# Wald p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$wald_p_value, 0, 1)))

# Minimum distance p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$minimum_distance_p_value))

# Minimum distance p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$minimum_distance_p_value, 0, 1)))

# Number of firms in each sample x aggregation-method x belief-measure cell
number_of_firms_by_sample_cell <- aggregated_sample_beliefs |> dplyr::count(sample, aggregation_method, belief_measure, name = "number_of_firms")

# Each sample cell should contain 164 firms
stopifnot(all(number_of_firms_by_sample_cell$number_of_firms == 164))

# Number of firms reported in the table
number_of_firms_in_table <- unique(number_of_firms_by_sample_cell$number_of_firms)

# There should be a single number of firms across all table cells
stopifnot(length(number_of_firms_in_table) == 1)

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
