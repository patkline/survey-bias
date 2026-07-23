# ------------------------------------------------------------------------------
# Purpose: Regress firm beliefs on EEO-1 NAICS3 workforce shares
# ------------------------------------------------------------------------------

fit_naics3_share_regression <- function(data, model_value, outcome_value,
                                        share_variable, demographic,
                                        job_scope, specification) {
  if (specification == "firm_level") {
    estimation_sample <- data |>
      dplyr::filter(
        .data$model == model_value,
        .data$outcome == outcome_value,
        is.finite(.data$estimate),
        is.finite(.data[[share_variable]])
      ) |>
      dplyr::mutate(share = .data[[share_variable]])

    fit <- stats::lm(estimate ~ share, data = estimation_sample)
    regression_vcov <- sandwich::vcovCL(
      fit,
      cluster = estimation_sample$naics3,
      type = "HC1"
    )
    se_type <- "NAICS3-clustered"
    inference_df <- dplyr::n_distinct(estimation_sample$naics3) - 1L
  } else if (specification == "equal_naics3") {
    estimation_sample <- data |>
      dplyr::filter(
        .data$model == model_value,
        .data$outcome == outcome_value,
        is.finite(.data$belief_mean),
        is.finite(.data[[share_variable]])
      ) |>
      dplyr::mutate(share = .data[[share_variable]])

    fit <- stats::lm(belief_mean ~ share, data = estimation_sample)
    regression_vcov <- sandwich::vcovHC(fit, type = "HC1")
    se_type <- "HC1"
    inference_df <- stats::df.residual(fit)
  } else {
    stop("Unknown NAICS3 specification: ", specification)
  }

  robust_se <- sqrt(diag(regression_vcov))
  coefficients <- stats::coef(fit)
  slope_t <- coefficients[["share"]] / robust_se[["share"]]
  critical_value <- stats::qt(0.975, df = inference_df)

  tibble::tibble(
    belief_model = model_value,
    belief_outcome = outcome_value,
    demographic = demographic,
    eeo1_job_scope = job_scope,
    share_variable = share_variable,
    specification = specification,
    se_type = se_type,
    inference_df = inference_df,
    n_naics3 = dplyr::n_distinct(estimation_sample$naics3),
    n_firms = if (specification == "firm_level") {
      dplyr::n_distinct(estimation_sample$firm_name)
    } else {
      sum(estimation_sample$n_firms)
    },
    intercept = unname(coefficients[["(Intercept)"]]),
    intercept_se = unname(robust_se[["(Intercept)"]]),
    share_slope = unname(coefficients[["share"]]),
    share_slope_se = unname(robust_se[["share"]]),
    share_slope_p_value = 2 * stats::pt(
      abs(slope_t),
      df = inference_df,
      lower.tail = FALSE
    ),
    share_slope_ci_low = unname(
      coefficients[["share"]] - critical_value * robust_se[["share"]]
    ),
    share_slope_ci_high = unname(
      coefficients[["share"]] + critical_value * robust_se[["share"]]
    ),
    effect_per_10pp_share = 0.1 * unname(coefficients[["share"]]),
    effect_per_10pp_se = 0.1 * unname(robust_se[["share"]]),
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared
  )
}

firm_naics3 <- load_firm_naics3_crosswalk()
eeo1_naics3 <- load_eeo1_naics3_shares()

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
  add_eeo1_naics3_shares_to_firms(
    firm_name_col = "firm_name",
    crosswalk = firm_naics3,
    shares = eeo1_naics3
  )

belief_counts <- firm_beliefs |>
  dplyr::group_by(.data$model, .data$outcome) |>
  dplyr::summarise(
    n_firms = dplyr::n_distinct(.data$firm_name),
    n_naics3 = dplyr::n_distinct(.data$naics3),
    .groups = "drop"
  )

if (nrow(belief_counts) != 4L ||
    any(belief_counts$n_firms != 164L) ||
    any(belief_counts$n_naics3 != 48L)) {
  stop("Expected 164 firms and 48 NAICS3 industries for each belief measure.")
}

industry_beliefs <- firm_beliefs |>
  dplyr::group_by(
    .data$model,
    .data$outcome,
    .data$naics3,
    .data$naics3_name
  ) |>
  dplyr::summarise(
    n_firms = dplyr::n_distinct(.data$firm_name),
    belief_mean = mean(.data$estimate),
    dplyr::across(
      dplyr::starts_with("eeo1_"),
      dplyr::first
    ),
    .groups = "drop"
  )

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
    for (specification in c("firm_level", "equal_naics3")) {
      spec <- regression_specs[spec_index, ]
      regression_results[[result_index]] <- fit_naics3_share_regression(
        data = if (specification == "firm_level") {
          firm_beliefs
        } else {
          industry_beliefs
        },
        model_value = model_value,
        outcome_value = spec$outcome,
        share_variable = spec$share_variable,
        demographic = spec$demographic,
        job_scope = spec$job_scope,
        specification = specification
      )
      result_index <- result_index + 1L
    }
  }
}

regression_results <- dplyr::bind_rows(regression_results)

if (nrow(regression_results) != 16L ||
    any(regression_results$se_type[regression_results$specification == "firm_level"] !=
          "NAICS3-clustered") ||
    any(regression_results$n_naics3[regression_results$demographic == "race" &
          regression_results$eeo1_job_scope == "front_line"] != 45L) ||
    any(regression_results$n_firms[regression_results$demographic == "race" &
          regression_results$eeo1_job_scope == "front_line"] != 158L)) {
  stop("The NAICS3 belief-share regression output failed its validation checks.")
}

write_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  "NAICS3_belief_share_regressions",
  regression_results
)
