# ------------------------------------------------------------------------------
# Purpose: Merge Yimfor LinkedIn workforce shares and run belief regressions
# ------------------------------------------------------------------------------

# This is an executable section 2 script. These guards make it runnable either
# on its own from the repository root or from code/2_analysis/!metafile.R.
if (!exists("git_survey_bias_root", inherits = TRUE)) {
  source("code/globals.R")
}
if (!exists("read_parquet_sheet", mode = "function", inherits = TRUE)) {
  source(file.path(helper_functions, "sheet_functions.R"))
}
if (!exists("load_firm_naics3_crosswalk", mode = "function", inherits = TRUE)) {
  source(file.path(analysis, "eeo1_naics3_shares.R"))
}
if (!exists("var_component_with_var", mode = "function", inherits = TRUE)) {
  source(file.path(analysis, "katz_correct.R"))
}
if (!exists("eivreg", mode = "function", inherits = TRUE)) {
  source(file.path(analysis, "eivreg.R"))
}
if (!exists("run_eiv_suite", mode = "function", inherits = TRUE)) {
  source(file.path(analysis, "eiv_functions.R"))
}
if (!exists("eeo1_coef_to_wide", mode = "function", inherits = TRUE)) {
  source(file.path(analysis, "eeo1_eiv.R"))
}

yimfor_linkedin_workbook <- file.path(
  git_survey_bias_root,
  "data",
  "external",
  "race_shares_fortune1000_kline97.xlsx"
)

linkedin_firm_shares_sheet <- "LinkedIn_firm_shares"
linkedin_regressions_sheet <- "LinkedIn_belief_share_regressions"
linkedin_eiv_sheet <- "EIV_linkedin_shares"

yimfor_kline_to_survey <- c(
  "Avis-Budget" = "Avis Budget Group",
  "CBRE" = "CBRE Group",
  "Charter / Spectrum" = "Charter Communications",
  "Sears (incl. repair / auto)" = "Sears Holdings",
  "US Bank" = "U.S. Bancorp",
  "State Farm" = "State Farm Insurance Cos.",
  "Dick's" = "Dick's Sporting Goods",
  "J.B. Hunt" = "J.B. Hunt Transport Services",
  "Walgreens" = "Walgreens Boots Alliance",
  "Marriott" = "Marriott International",
  "Honeywell" = "Honeywell International",
  "Disney (incl. stores)" = "Disney",
  "DISH" = "DISH Network",
  "Lab Corp" = "Laboratory Corp. of America",
  "Hertz" = "Hertz Global Holdings",
  "Hilton" = "Hilton Worldwide Holdings",
  "Genuine Parts (Napa Auto)" = "Genuine Parts",
  "Goodyear" = "Goodyear Tire & Rubber",
  "Publix" = "Publix Super Markets",
  "Edward Jones" = "Jones Financial",
  "Mondelez" = "Mondelez International",
  "VFC (North Face / Vans)" = "VF",
  "Dr Pepper" = "Dr Pepper Snapple Group",
  "Universal Health" = "Universal Health Services",
  "UnitedHealth" = "UnitedHealth Group",
  "US Foods" = "US Foods Holding",
  "LKQ Auto" = "LKQ",
  "Ascena (Ann Taylor / Loft)" = "Ascena Retail Group"
)

yimfor_fortune_fallbacks <- tibble::tribble(
  ~firm, ~rcid, ~method, ~note,
  "BB&T Corp.", "21041371", "fortune1000_manual_successor_rcid",
  "Use 2023 Truist Financial Corp. as the successor to BB&T.",
  "Dell Technologies", "22142190", "fortune1000_manual_successor_rcid",
  "Use the workbook's Dell, Inc. RCID.",
  "Fruit of the Loom", "383823", "fortune1000_manual_parent_rcid",
  "Use Berkshire Hathaway, consistent with parent-brand pooling."
)

yimfor_expected_naics3_fallback_firms <- c(
  "Emerson Electric",
  "Icahn Enterprises",
  "United Continental Holdings"
)

yimfor_belief_specs <- tibble::tribble(
  ~belief_order, ~belief_label, ~belief_outcome,
  1L, "Race Beliefs", "pooled_favor_white",
  2L, "Gender Beliefs", "pooled_favor_male",
  3L, "Selectivity Beliefs", "FirmSelective"
)

yimfor_eiv_rhs_outcomes <- c(
  "pooled_favor_white",
  "pooled_favor_male"
)

yimfor_eiv_zero_error_controls <- c(
  "analysis_share_black",
  "analysis_share_female"
)

yimfor_eiv_specs <- list(
  list(
    lhs = "log_dif",
    rhs = c("pooled_favor_white", "analysis_share_black")
  ),
  list(
    lhs = "log_dif_gender",
    rhs = c("pooled_favor_male", "analysis_share_female")
  )
)

clean_yimfor_rcid <- function(x) {
  out <- trimws(as.character(x))
  out[out %in% c("", "NA", "NaN", "<NA>")] <- NA_character_
  sub("\\.0+$", "", out)
}

load_yimfor_survey_firms <- function(
    path = file.path(processed, "revelio_firm_measures.csv")
) {
  firms <- readr::read_csv(
    path,
    col_types = readr::cols(
      .default = readr::col_skip(),
      firm_id = readr::col_integer(),
      firm = readr::col_character(),
      requested_rcid = readr::col_character(),
      workforce_source_rcid = readr::col_character(),
      parent_company = readr::col_character(),
      aer_naics2 = readr::col_integer()
    ),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      requested_rcid = clean_yimfor_rcid(.data$requested_rcid),
      workforce_source_rcid = clean_yimfor_rcid(
        .data$workforce_source_rcid
      )
    )

  if (nrow(firms) != 164L ||
      dplyr::n_distinct(firms$firm) != 164L ||
      dplyr::n_distinct(firms$firm_id) != 164L ||
      dplyr::n_distinct(firms$aer_naics2) != 19L ||
      any(is.na(firms$aer_naics2))) {
    stop("Expected 164 unique survey firms in 19 AER industries.")
  }

  firms
}

load_yimfor_2023_workbook <- function(path = yimfor_linkedin_workbook) {
  if (!file.exists(path)) {
    stop("Yimfor LinkedIn workbook not found: ", path)
  }

  kline <- readxl::read_excel(
    path,
    sheet = "kline97",
    col_types = c("numeric", "text", rep("numeric", 8L))
  ) |>
    dplyr::filter(.data$year == 2023) |>
    dplyr::mutate(
      firm_id = as.integer(.data$firm_id),
      survey_firm = dplyr::if_else(
        .data$company %in% names(yimfor_kline_to_survey),
        unname(yimfor_kline_to_survey[.data$company]),
        .data$company
      )
    )

  fortune_all_years <- readxl::read_excel(
    path,
    sheet = "fortune1000",
    col_types = c("text", "text", rep("numeric", 8L))
  ) |>
    dplyr::mutate(rcid = clean_yimfor_rcid(.data$rcid))

  fortune <- fortune_all_years |>
    dplyr::filter(.data$year == 2023)

  if (nrow(kline) != 97L ||
      dplyr::n_distinct(kline$firm_id) != 97L ||
      dplyr::n_distinct(kline$survey_firm) != 97L) {
    stop("Expected 97 unique 2023 audited firms in the kline97 sheet.")
  }
  if (anyDuplicated(fortune$rcid)) {
    stop("Expected unique 2023 RCIDs in the fortune1000 sheet.")
  }

  list(
    kline = kline,
    fortune = fortune,
    fortune_all_rcids = unique(stats::na.omit(fortune_all_years$rcid))
  )
}

match_yimfor_2023_shares <- function(
    firms = load_yimfor_survey_firms(),
    workbook = load_yimfor_2023_workbook()
) {
  kline <- workbook$kline
  fortune <- workbook$fortune

  missing_kline_firms <- setdiff(kline$survey_firm, firms$firm)
  if (length(missing_kline_firms)) {
    stop(
      "Kline firms absent from the survey data: ",
      paste(sort(missing_kline_firms), collapse = ", ")
    )
  }

  matched_records <- lapply(seq_len(nrow(firms)), function(i) {
    firm_row <- firms[i, ]
    kline_index <- match(firm_row$firm, kline$survey_firm)
    source_row <- NULL
    match_method <- "unmatched"
    source_id <- NA_character_
    source_company <- NA_character_
    match_note <- ""

    if (!is.na(kline_index)) {
      source_row <- kline[kline_index, ]
      match_method <- "kline97_audited_firm"
      source_id <- paste0("kline97:", source_row$firm_id)
      source_company <- source_row$company
      match_note <-
        "Audited-employer aggregation from kline97; subsidiaries pooled."
    } else {
      requested_index <- match(firm_row$requested_rcid, fortune$rcid)
      workforce_index <- match(firm_row$workforce_source_rcid, fortune$rcid)
      fallback <- yimfor_fortune_fallbacks |>
        dplyr::filter(.data$firm == firm_row$firm)

      if (!is.na(requested_index)) {
        source_row <- fortune[requested_index, ]
        match_method <- "fortune1000_requested_rcid"
        source_id <- firm_row$requested_rcid
        source_company <- source_row$company
      } else if (!is.na(workforce_index)) {
        source_row <- fortune[workforce_index, ]
        match_method <- "fortune1000_existing_parent_rcid"
        source_id <- firm_row$workforce_source_rcid
        source_company <- source_row$company
        match_note <- paste0(
          "Existing project parent inheritance from ", firm_row$firm,
          " to ", firm_row$parent_company, "."
        )
      } else if (nrow(fallback) == 1L) {
        fallback_index <- match(fallback$rcid, fortune$rcid)
        if (is.na(fallback_index)) {
          stop(
            "Fallback RCID ", fallback$rcid,
            " is absent for ", firm_row$firm, "."
          )
        }
        source_row <- fortune[fallback_index, ]
        match_method <- fallback$method
        source_id <- fallback$rcid
        source_company <- source_row$company
        match_note <- fallback$note
      } else {
        candidate_rcids <- stats::na.omit(c(
          firm_row$requested_rcid,
          firm_row$workforce_source_rcid
        ))
        if (any(candidate_rcids %in% workbook$fortune_all_rcids)) {
          match_note <- paste(
            "Not in kline97; the project RCID appears in fortune1000",
            "in another year but has no published 2023 row."
          )
        } else {
          match_note <- paste(
            "Not in kline97; the project RCID is absent from",
            "fortune1000 in every year."
          )
        }
      }
    }

    source_value <- function(variable) {
      if (is.null(source_row)) NA_real_ else as.numeric(source_row[[variable]])
    }

    tibble::tibble(
      firm_id = firm_row$firm_id,
      firm = firm_row$firm,
      aer_naics2 = firm_row$aer_naics2,
      requested_rcid = firm_row$requested_rcid,
      workforce_source_rcid = firm_row$workforce_source_rcid,
      parent_company = firm_row$parent_company,
      new_match_method = match_method,
      new_source_id = source_id,
      new_source_company = source_company,
      new_match_note = match_note,
      new_n_race = source_value("n"),
      new_n_gender = source_value("n_gender"),
      new_share_white = source_value("share_white"),
      new_share_black = source_value("share_black"),
      new_share_asian = source_value("share_asian"),
      new_share_hispanic = source_value("share_hispanic"),
      new_share_female = source_value("share_female"),
      new_matched = !is.null(source_row)
    )
  }) |>
    dplyr::bind_rows()

  race_share_sum <- rowSums(
    matched_records[c(
      "new_share_white",
      "new_share_black",
      "new_share_asian",
      "new_share_hispanic"
    )]
  )
  matched_race_share_sum <- race_share_sum[matched_records$new_matched]
  unmatched_firms <- matched_records$firm[!matched_records$new_matched]

  if (sum(matched_records$new_matched) != 161L ||
      sum(matched_records$new_match_method == "kline97_audited_firm") != 97L ||
      !setequal(unmatched_firms, yimfor_expected_naics3_fallback_firms) ||
      any(abs(matched_race_share_sum - 1) > 1e-8)) {
    stop("The 2023 Yimfor LinkedIn firm match failed its validation checks.")
  }

  matched_records
}

add_yimfor_analysis_shares <- function(
    matched_shares = match_yimfor_2023_shares(),
    crosswalk = load_firm_naics3_crosswalk(),
    eeo1_shares = load_eeo1_naics3_shares()
) {
  firm_shares <- matched_shares |>
    dplyr::left_join(
      crosswalk,
      by = c("firm" = "firm_name")
    ) |>
    dplyr::left_join(eeo1_shares, by = "naics3") |>
    dplyr::mutate(
      share_is_naics3_fallback = !.data$new_matched,
      analysis_share_black = dplyr::coalesce(
        .data$new_share_black,
        .data$eeo1_black_all_jobs_share
      ),
      analysis_share_female = dplyr::coalesce(
        .data$new_share_female,
        .data$eeo1_female_all_jobs_share
      ),
      analysis_share_source = dplyr::if_else(
        .data$share_is_naics3_fallback,
        "2023 EEO-1 NAICS3 all jobs",
        "2023 Yimfor LinkedIn"
      )
    )

  fallback_firms <- firm_shares$firm[firm_shares$share_is_naics3_fallback]
  analysis_shares <- firm_shares[c(
    "analysis_share_black",
    "analysis_share_female"
  )]

  if (nrow(firm_shares) != 164L ||
      !setequal(fallback_firms, yimfor_expected_naics3_fallback_firms) ||
      any(is.na(firm_shares$naics3)) ||
      any(!is.finite(as.matrix(analysis_shares))) ||
      any(as.matrix(analysis_shares) < 0 | as.matrix(analysis_shares) > 1)) {
    stop("The Yimfor/EEO-1 analysis shares failed their validation checks.")
  }

  firm_shares
}

run_yimfor_share_control_eiv <- function(
    coefficients_long,
    variance_df,
    covariance_df,
    firm_shares,
    models_to_run = c("OLS", "Borda")
) {
  eiv_firm_shares <- firm_shares |>
    dplyr::transmute(
      firm_id = as.integer(.data$firm_id),
      share_firm = .data$firm,
      analysis_share_black = as.numeric(.data$analysis_share_black),
      analysis_share_female = as.numeric(.data$analysis_share_female),
      naics3 = as.integer(.data$naics3),
      share_is_naics3_fallback = .data$share_is_naics3_fallback
    )

  coef_firm_wide <- eeo1_coef_to_wide(coefficients_long, "Firm") |>
    dplyr::left_join(
      eiv_firm_shares,
      by = c("entity_id" = "firm_id")
    )

  if (nrow(coef_firm_wide) != 194L ||
      dplyr::n_distinct(coef_firm_wide$entity_id) != 97L ||
      any(coef_firm_wide$entity != coef_firm_wide$share_firm) ||
      any(coef_firm_wide$share_is_naics3_fallback) ||
      any(is.na(coef_firm_wide$naics3)) ||
      any(!is.finite(coef_firm_wide$analysis_share_black)) ||
      any(!is.finite(coef_firm_wide$analysis_share_female))) {
    stop("The 97-firm EIV sample did not merge cleanly to Yimfor shares.")
  }

  noise_mats <- stats::setNames(
    vector("list", length(models_to_run)),
    models_to_run
  )
  for (model_value in models_to_run) {
    noise_mats[[model_value]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = yimfor_eiv_rhs_outcomes,
      subset_value = "subset97",
      model_value = model_value
    ) |>
      add_zero_error_controls_eeo1(yimfor_eiv_zero_error_controls)
  }

  # Match the earlier Yimfor EIV table: employment weights, no fixed effects,
  # and NAICS3-clustered standard errors with the finite-sample adjustment.
  # Workforce shares are measured without sampling error; only beliefs are
  # corrected for measurement error.
  eiv_results <- run_eiv_suite(
    regs = yimfor_eiv_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    weights_col = "njobs",
    cluster_col = "naics3",
    cluster_df_adj = TRUE,
    use_fe = FALSE
  ) |>
    dplyr::mutate(
      linkedin_regression_level = "Firm",
      linkedin_spec_group = "share_control"
    )

  if (nrow(eiv_results) != 8L ||
      any(eiv_results$n != 97L) ||
      !setequal(eiv_results$model, models_to_run)) {
    stop("The Yimfor share-control EIV output failed its validation checks.")
  }

  eiv_results
}

build_yimfor_rcov_matrix <- function(rcov_long, entity_ids) {
  if (nrow(rcov_long) != length(entity_ids)^2) {
    stop("The firm-estimate covariance matrix is incomplete.")
  }

  Sigma <- matrix(
    NA_real_,
    nrow = length(entity_ids),
    ncol = length(entity_ids),
    dimnames = list(as.character(entity_ids), as.character(entity_ids))
  )
  Sigma[cbind(
    match(rcov_long$entity_id_i, entity_ids),
    match(rcov_long$entity_id_j, entity_ids)
  )] <- as.numeric(rcov_long$rcov)

  if (anyNA(Sigma) || max(abs(Sigma - t(Sigma))) > 1e-10) {
    stop("The firm-estimate covariance matrix is missing or asymmetric.")
  }

  Sigma
}

calculate_yimfor_projected_signal <- function(theta_hat, Sigma, design) {
  design_qr <- qr(design)
  if (design_qr$rank != ncol(design)) {
    stop("The signal-projection design matrix is not full rank.")
  }

  design_q <- qr.Q(design_qr)
  residual_maker <- diag(length(theta_hat)) - tcrossprod(design_q)
  residual_theta_hat <- as.numeric(residual_maker %*% theta_hat)
  residual_Sigma <- residual_maker %*% Sigma %*% residual_maker
  variance_component <- var_component_with_var(
    residual_theta_hat,
    residual_Sigma
  )

  list(
    signal_variance = katz_correct(
      variance_component$sigma2_hat,
      variance_component$Vhat
    ),
    sigma2_hat = variance_component$sigma2_hat,
    Vhat = variance_component$Vhat
  )
}

fit_yimfor_joint_share_regression <- function(data, Sigma, industry_fe) {
  estimation_sample <- data |>
    dplyr::transmute(
      entity_id = as.integer(.data$entity_id),
      firm = .data$firm,
      estimate = as.numeric(.data$estimate),
      analysis_share_black = as.numeric(.data$analysis_share_black),
      analysis_share_female = as.numeric(.data$analysis_share_female),
      aer_naics2 = as.integer(.data$aer_naics2),
      share_is_naics3_fallback = .data$share_is_naics3_fallback
    ) |>
    dplyr::filter(
      is.finite(.data$estimate),
      is.finite(.data$analysis_share_black),
      is.finite(.data$analysis_share_female),
      !is.na(.data$aer_naics2)
    ) |>
    dplyr::arrange(.data$entity_id)

  regression_formula <- if (isTRUE(industry_fe)) {
    estimate ~ analysis_share_black + analysis_share_female +
      factor(aer_naics2)
  } else {
    estimate ~ analysis_share_black + analysis_share_female
  }
  fit <- stats::lm(regression_formula, data = estimation_sample)
  regression_vcov <- sandwich::vcovHC(fit, type = "HC1")
  robust_se <- sqrt(diag(regression_vcov))
  coefficients <- stats::coef(fit)

  # The no-FE baseline is the unconditional Table 4 signal. With FE, the
  # baseline first removes AER-industry means so signal_explained isolates the
  # incremental contribution of the two workforce shares within industries.
  baseline_design <- if (isTRUE(industry_fe)) {
    stats::model.matrix(~ factor(aer_naics2), data = estimation_sample)
  } else {
    matrix(1, nrow = nrow(estimation_sample), ncol = 1L)
  }
  full_design <- stats::model.matrix(
    regression_formula,
    data = estimation_sample
  )
  # Project both the firm estimates and their covariance matrix before applying
  # the same Katz signal correction used in Table 4.
  baseline_signal <- calculate_yimfor_projected_signal(
    estimation_sample$estimate,
    Sigma,
    baseline_design
  )
  residual_signal <- calculate_yimfor_projected_signal(
    estimation_sample$estimate,
    Sigma,
    full_design
  )

  black_slope <- unname(coefficients[["analysis_share_black"]])
  black_slope_se <- unname(robust_se[["analysis_share_black"]])
  female_slope <- unname(coefficients[["analysis_share_female"]])
  female_slope_se <- unname(robust_se[["analysis_share_female"]])

  tibble::tibble(
    industry_fe = isTRUE(industry_fe),
    se_type = "HC1",
    n_firms = dplyr::n_distinct(estimation_sample$firm),
    n_aer_naics2 = dplyr::n_distinct(estimation_sample$aer_naics2),
    n_linkedin_shares = sum(!estimation_sample$share_is_naics3_fallback),
    n_naics3_share_fallbacks = sum(
      estimation_sample$share_is_naics3_fallback
    ),
    black_share_slope = black_slope,
    black_share_slope_se = black_slope_se,
    black_share_slope_p_value = 2 * stats::pt(
      abs(black_slope / black_slope_se),
      df = stats::df.residual(fit),
      lower.tail = FALSE
    ),
    black_effect_per_10pp_share = 0.1 * black_slope,
    black_effect_per_10pp_se = 0.1 * black_slope_se,
    female_share_slope = female_slope,
    female_share_slope_se = female_slope_se,
    female_share_slope_p_value = 2 * stats::pt(
      abs(female_slope / female_slope_se),
      df = stats::df.residual(fit),
      lower.tail = FALSE
    ),
    female_effect_per_10pp_share = 0.1 * female_slope,
    female_effect_per_10pp_se = 0.1 * female_slope_se,
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared,
    baseline_signal_variance = baseline_signal$signal_variance,
    residual_signal_variance = residual_signal$signal_variance,
    signal_explained = 1 -
      residual_signal$signal_variance / baseline_signal$signal_variance,
    signal_explained_definition = if (isTRUE(industry_fe)) {
      "incremental_within_aer_naics2"
    } else {
      "unconditional"
    }
  )
}

run_linkedin_share_analysis <- function(
    output_dir = file.path(intermediate, "Full_Sample"),
    write_sheets = TRUE
) {
  firm_shares <- add_yimfor_analysis_shares()
  coefficients_long <- read_parquet_sheet(output_dir, "Coefficients")
  coefficients <- coefficients_long |>
    dplyr::filter(
      .data$subset == "all",
      .data$entity_type == "Firm",
      .data$model %in% c("OLS", "Borda"),
      .data$outcome %in% yimfor_belief_specs$belief_outcome
    ) |>
    dplyr::transmute(
      entity_id = as.integer(.data$entity_id),
      model = .data$model,
      belief_outcome = .data$outcome,
      firm = .data$entity,
      estimate = .data$estimate
    )

  rcov_path <- parquet_sheet_path(output_dir, "rcov")
  if (!file.exists(rcov_path)) {
    stop("Firm-estimate covariance sheet not found: ", rcov_path)
  }
  rcov_relevant <- arrow::open_dataset(rcov_path, format = "parquet") |>
    dplyr::filter(
      .data$subset == "all",
      .data$model %in% c("OLS", "Borda"),
      .data$outcome %in% yimfor_belief_specs$belief_outcome
    ) |>
    dplyr::collect()

  variance_all <- read_parquet_sheet(output_dir, "variance")
  variance_results <- variance_all |>
    dplyr::filter(
      .data$subset == "all",
      .data$model %in% c("OLS", "Borda"),
      .data$outcome %in% yimfor_belief_specs$belief_outcome
    )

  coefficient_counts <- coefficients |>
    dplyr::group_by(.data$model, .data$belief_outcome) |>
    dplyr::summarise(
      n_firms = dplyr::n_distinct(.data$firm),
      .groups = "drop"
    )
  if (nrow(coefficient_counts) != 6L ||
      any(coefficient_counts$n_firms != 164L)) {
    stop("Expected 164 firms for all six belief-model combinations.")
  }

  regression_results <- list()
  result_index <- 1L
  for (model_value in c("OLS", "Borda")) {
    for (belief_index in seq_len(nrow(yimfor_belief_specs))) {
      belief_spec <- yimfor_belief_specs[belief_index, ]
      estimation_data <- coefficients |>
        dplyr::filter(
          .data$model == model_value,
          .data$belief_outcome == belief_spec$belief_outcome
        ) |>
        dplyr::left_join(firm_shares, by = "firm") |>
        dplyr::arrange(.data$entity_id)

      if (nrow(estimation_data) != 164L ||
          any(is.na(estimation_data$analysis_share_black)) ||
          any(is.na(estimation_data$analysis_share_female))) {
        stop("Belief estimates did not merge cleanly to the workforce shares.")
      }

      rcov_spec <- rcov_relevant |>
        dplyr::filter(
          .data$model == model_value,
          .data$outcome == belief_spec$belief_outcome
        )
      Sigma <- build_yimfor_rcov_matrix(
        rcov_spec,
        estimation_data$entity_id
      )

      for (industry_fe in c(FALSE, TRUE)) {
        result <- fit_yimfor_joint_share_regression(
          estimation_data,
          Sigma = Sigma,
          industry_fe = industry_fe
        )

        if (!isTRUE(industry_fe)) {
          table4_signal <- variance_results |>
            dplyr::filter(
              .data$model == model_value,
              .data$outcome == belief_spec$belief_outcome
            ) |>
            dplyr::pull("signal")
          if (length(table4_signal) != 1L ||
              abs(result$baseline_signal_variance - table4_signal) > 1e-10) {
            stop("Unconditional signal variance does not match Table 4.")
          }
        }

        regression_results[[result_index]] <- dplyr::bind_cols(
          tibble::tibble(
            model = model_value,
            model_order = match(model_value, c("OLS", "Borda"))
          ),
          belief_spec,
          result
        )
        result_index <- result_index + 1L
      }
    }
  }

  regression_results <- dplyr::bind_rows(regression_results)
  if (nrow(regression_results) != 12L ||
      any(regression_results$n_firms != 164L) ||
      any(regression_results$n_aer_naics2 != 19L) ||
      any(regression_results$n_linkedin_shares != 161L) ||
      any(regression_results$n_naics3_share_fallbacks != 3L) ||
      any(regression_results$se_type != "HC1") ||
      any(!is.finite(regression_results$signal_explained)) ||
      any(regression_results$signal_explained < 0) ||
      any(regression_results$signal_explained > 1)) {
    stop("The Yimfor belief-share regressions failed their validation checks.")
  }

  eiv_results <- run_yimfor_share_control_eiv(
    coefficients_long = coefficients_long,
    variance_df = variance_all,
    covariance_df = read_parquet_sheet(output_dir, "covariance"),
    firm_shares = firm_shares
  )

  if (isTRUE(write_sheets)) {
    write_parquet_sheet(
      output_dir,
      linkedin_firm_shares_sheet,
      firm_shares
    )
    write_parquet_sheet(
      output_dir,
      linkedin_regressions_sheet,
      regression_results
    )
    write_parquet_sheet(
      output_dir,
      linkedin_eiv_sheet,
      eiv_results
    )
  }

  invisible(list(
    firm_shares = firm_shares,
    regression_results = regression_results,
    eiv_results = eiv_results
  ))
}

run_linkedin_share_analysis()
