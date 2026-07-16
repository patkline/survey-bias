# ------------------------------------------------------------------------------
# Purpose: Regress NAICS3-level beliefs on EEO-1 workforce shares
# ------------------------------------------------------------------------------

is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

as_numeric_suppressed <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

row_sum_strict <- function(data, variables) {
  values <- vapply(
    variables,
    function(variable) as_numeric_suppressed(data[[variable]]),
    FUN.VALUE = numeric(nrow(data))
  )
  rowSums(values, na.rm = FALSE)
}

fit_naics3_share_regression <- function(data, model_value, outcome_value,
                                        share_variable, demographic,
                                        job_scope, weighting) {
  estimation_sample <- data |>
    dplyr::filter(
      .data$model == model_value,
      .data$outcome == outcome_value,
      is.finite(.data$belief_mean),
      is.finite(.data[[share_variable]]),
      is.finite(.data$n_firms),
      .data$n_firms > 0
    ) |>
    dplyr::mutate(share = .data[[share_variable]])

  regression_weights <- if (weighting == "number_of_firms") {
    estimation_sample$n_firms
  } else if (weighting == "equal_naics3") {
    rep(1, nrow(estimation_sample))
  } else {
    stop("Unknown NAICS3 weighting specification: ", weighting)
  }

  fit <- stats::lm(
    belief_mean ~ share,
    data = estimation_sample,
    weights = regression_weights
  )
  robust_vcov <- sandwich::vcovHC(fit, type = "HC1")
  robust_se <- sqrt(diag(robust_vcov))
  coefficients <- stats::coef(fit)
  residual_df <- stats::df.residual(fit)
  slope_t <- coefficients[["share"]] / robust_se[["share"]]
  critical_value <- stats::qt(0.975, df = residual_df)

  tibble::tibble(
    belief_model = model_value,
    belief_outcome = outcome_value,
    demographic = demographic,
    eeo1_job_scope = job_scope,
    share_variable = share_variable,
    weighting = weighting,
    n_naics3 = nrow(estimation_sample),
    n_firms = sum(estimation_sample$n_firms),
    intercept = unname(coefficients[["(Intercept)"]]),
    intercept_se_hc1 = unname(robust_se[["(Intercept)"]]),
    share_slope = unname(coefficients[["share"]]),
    share_slope_se_hc1 = unname(robust_se[["share"]]),
    share_slope_p_value_hc1 = 2 * stats::pt(
      abs(slope_t),
      df = residual_df,
      lower.tail = FALSE
    ),
    share_slope_ci_low_hc1 = unname(
      coefficients[["share"]] - critical_value * robust_se[["share"]]
    ),
    share_slope_ci_high_hc1 = unname(
      coefficients[["share"]] + critical_value * robust_se[["share"]]
    ),
    effect_per_10pp_share = 0.1 * unname(coefficients[["share"]]),
    effect_per_10pp_se_hc1 = 0.1 * unname(robust_se[["share"]]),
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared
  )
}

# Read the firm-to-NAICS3 crosswalk.
firm_naics3 <- readr::read_csv(
  file.path(processed, "firm_naics3.csv"),
  col_types = readr::cols(
    firm_name = readr::col_character(),
    naics3 = readr::col_integer(),
    naics3_name = readr::col_character()
  ),
  show_col_types = FALSE
)

if (!identical(names(firm_naics3), c("firm_name", "naics3", "naics3_name")) ||
    nrow(firm_naics3) != 164L ||
    anyDuplicated(firm_naics3$firm_name) ||
    dplyr::n_distinct(firm_naics3$naics3) != 48L) {
  stop("The firm-to-NAICS3 crosswalk is not the expected 164-firm input.")
}

# Build national 2022 NAICS3 workforce shares from the 2023 EEO-1 PUF.
eeo1_raw <- readxl::read_excel(
  file.path(external, "EEO1_2023_PUF.xlsx"),
  sheet = "Data",
  col_types = "text"
)

national_rows <- Reduce(
  `&`,
  lapply(c("Region", "Division", "State", "CBSA", "County"), function(variable) {
    is_blank(eeo1_raw[[variable]])
  })
)

eeo1_naics3 <- eeo1_raw |>
  dplyr::filter(national_rows, !is_blank(.data$NAICS3)) |>
  dplyr::mutate(
    naics3 = as.integer(.data$NAICS3),
    eeo1_total_employment_all_jobs = as_numeric_suppressed(.data$TOTAL10),
    eeo1_black_employment_all_jobs = as_numeric_suppressed(.data$BLKT10),
    eeo1_female_employment_all_jobs = as_numeric_suppressed(.data$FT10),
    eeo1_total_employment_front_line = row_sum_strict(
      dplyr::pick(dplyr::everything()),
      c("TOTAL4", "TOTAL5", "TOTAL6", "TOTAL8", "TOTAL9")
    ),
    eeo1_black_employment_front_line = row_sum_strict(
      dplyr::pick(dplyr::everything()),
      c("BLKT4", "BLKT5", "BLKT6", "BLKT8", "BLKT9")
    ),
    eeo1_female_employment_front_line = row_sum_strict(
      dplyr::pick(dplyr::everything()),
      c("FT4", "FT5", "FT6", "FT8", "FT9")
    ),
    eeo1_black_all_jobs_share =
      .data$eeo1_black_employment_all_jobs / .data$eeo1_total_employment_all_jobs,
    eeo1_female_all_jobs_share =
      .data$eeo1_female_employment_all_jobs / .data$eeo1_total_employment_all_jobs,
    eeo1_black_front_line_share =
      .data$eeo1_black_employment_front_line / .data$eeo1_total_employment_front_line,
    eeo1_female_front_line_share =
      .data$eeo1_female_employment_front_line / .data$eeo1_total_employment_front_line
  ) |>
  dplyr::select(
    naics3,
    eeo1_black_all_jobs_share,
    eeo1_black_front_line_share,
    eeo1_female_all_jobs_share,
    eeo1_female_front_line_share
  )

if (anyDuplicated(eeo1_naics3$naics3)) {
  stop("National EEO-1 rows are not unique by NAICS3.")
}

# Collapse full-sample firm beliefs to equal-firm NAICS3 means.
firm_beliefs <- read_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  "Coefficients"
) |>
  dplyr::filter(
    .data$subset == "all",
    .data$entity_type == "Firm",
    .data$model %in% c("OLS", "Borda"),
    .data$outcome %in% c("pooled_favor_male", "pooled_favor_white")
  ) |>
  dplyr::transmute(
    model = .data$model,
    outcome = .data$outcome,
    firm_name = .data$entity,
    estimate = .data$estimate
  ) |>
  dplyr::left_join(firm_naics3, by = "firm_name")

if (any(is.na(firm_beliefs$naics3))) {
  stop("At least one belief firm is missing from the NAICS3 crosswalk.")
}

belief_counts <- firm_beliefs |>
  dplyr::group_by(.data$model, .data$outcome) |>
  dplyr::summarise(n_firms = dplyr::n_distinct(.data$firm_name), .groups = "drop")

if (nrow(belief_counts) != 4L || any(belief_counts$n_firms != 164L)) {
  stop("Expected 164 firm beliefs for each model-by-outcome cell.")
}

industry_beliefs <- firm_beliefs |>
  dplyr::group_by(.data$model, .data$outcome, .data$naics3, .data$naics3_name) |>
  dplyr::summarise(
    n_firms = dplyr::n_distinct(.data$firm_name),
    belief_mean = mean(.data$estimate),
    .groups = "drop"
  ) |>
  dplyr::left_join(eeo1_naics3, by = "naics3")

if (dplyr::n_distinct(industry_beliefs$naics3) != 48L ||
    any(is.na(industry_beliefs$eeo1_female_all_jobs_share)) ||
    any(is.na(industry_beliefs$eeo1_black_all_jobs_share))) {
  stop("The NAICS3 beliefs did not merge cleanly to the EEO-1 shares.")
}

regression_specs <- tibble::tribble(
  ~outcome, ~share_variable, ~demographic, ~job_scope,
  "pooled_favor_male", "eeo1_female_all_jobs_share", "gender", "all_jobs",
  "pooled_favor_male", "eeo1_female_front_line_share", "gender", "front_line",
  "pooled_favor_white", "eeo1_black_all_jobs_share", "race", "all_jobs",
  "pooled_favor_white", "eeo1_black_front_line_share", "race", "front_line"
)

regression_results <- list()
result_index <- 1L

for (model_value in c("OLS", "Borda")) {
  for (spec_index in seq_len(nrow(regression_specs))) {
    for (weighting in c("number_of_firms", "equal_naics3")) {
      spec <- regression_specs[spec_index, ]
      regression_results[[result_index]] <- fit_naics3_share_regression(
        data = industry_beliefs,
        model_value = model_value,
        outcome_value = spec$outcome,
        share_variable = spec$share_variable,
        demographic = spec$demographic,
        job_scope = spec$job_scope,
        weighting = weighting
      )
      result_index <- result_index + 1L
    }
  }
}

regression_results <- dplyr::bind_rows(regression_results)

# The primary firm-count-weighted industry regression must reproduce firm-level
# OLS because the share is constant within NAICS3.
firm_validation_data <- firm_beliefs |>
  dplyr::left_join(eeo1_naics3, by = "naics3")

for (model_value in c("OLS", "Borda")) {
  for (spec_index in seq_len(nrow(regression_specs))) {
    spec <- regression_specs[spec_index, ]
    firm_sample <- firm_validation_data |>
      dplyr::filter(
        .data$model == model_value,
        .data$outcome == spec$outcome,
        is.finite(.data[[spec$share_variable]])
      ) |>
      dplyr::mutate(share = .data[[spec$share_variable]])

    firm_slope <- unname(stats::coef(
      stats::lm(estimate ~ share, data = firm_sample)
    )[["share"]])

    aggregate_slope <- regression_results |>
      dplyr::filter(
        .data$belief_model == model_value,
        .data$belief_outcome == spec$outcome,
        .data$share_variable == spec$share_variable,
        .data$weighting == "number_of_firms"
      ) |>
      dplyr::pull(.data$share_slope)

    if (length(aggregate_slope) != 1L ||
        abs(firm_slope - aggregate_slope) > 1e-10) {
      stop("Firm-count-weighted NAICS3 OLS does not reproduce firm-level OLS.")
    }
  }
}

write_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  "NAICS3_belief_share_regressions",
  regression_results
)
