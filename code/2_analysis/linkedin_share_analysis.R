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

yimfor_linkedin_workbook <- file.path(
  git_survey_bias_root,
  "data",
  "external",
  "race_shares_fortune1000_kline97.xlsx"
)

linkedin_firm_shares_sheet <- "LinkedIn_firm_shares"
linkedin_regressions_sheet <- "LinkedIn_belief_share_regressions"

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

yimfor_share_specs <- tibble::tribble(
  ~panel_order, ~panel, ~share_label, ~share_variable,
  1L, "Panel A: Race", "Black Share", "analysis_share_black",
  2L, "Panel B: Gender", "Female Share", "analysis_share_female"
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

fit_yimfor_belief_share_regression <- function(
    data,
    share_variable,
    industry_fe
) {
  estimation_sample <- data |>
    dplyr::transmute(
      firm = .data$firm,
      estimate = as.numeric(.data$estimate),
      analysis_share = as.numeric(.data[[share_variable]]),
      aer_naics2 = as.integer(.data$aer_naics2),
      share_is_naics3_fallback = .data$share_is_naics3_fallback
    ) |>
    dplyr::filter(
      is.finite(.data$estimate),
      is.finite(.data$analysis_share),
      !is.na(.data$aer_naics2)
    )

  regression_formula <- if (isTRUE(industry_fe)) {
    estimate ~ analysis_share + factor(aer_naics2)
  } else {
    estimate ~ analysis_share
  }
  fit <- stats::lm(regression_formula, data = estimation_sample)
  regression_vcov <- sandwich::vcovHC(fit, type = "HC1")
  robust_se <- sqrt(diag(regression_vcov))
  share_slope <- unname(stats::coef(fit)[["analysis_share"]])
  share_slope_se <- unname(robust_se[["analysis_share"]])
  share_t <- share_slope / share_slope_se

  tibble::tibble(
    industry_fe = isTRUE(industry_fe),
    se_type = "HC1",
    n_firms = dplyr::n_distinct(estimation_sample$firm),
    n_aer_naics2 = dplyr::n_distinct(estimation_sample$aer_naics2),
    n_linkedin_shares = sum(!estimation_sample$share_is_naics3_fallback),
    n_naics3_share_fallbacks = sum(
      estimation_sample$share_is_naics3_fallback
    ),
    share_slope = share_slope,
    share_slope_se = share_slope_se,
    share_slope_p_value = 2 * stats::pt(
      abs(share_t),
      df = stats::df.residual(fit),
      lower.tail = FALSE
    ),
    effect_per_10pp_share = 0.1 * share_slope,
    effect_per_10pp_se = 0.1 * share_slope_se,
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared
  )
}

run_linkedin_share_analysis <- function(
    output_dir = file.path(intermediate, "Full_Sample"),
    write_sheets = TRUE
) {
  firm_shares <- add_yimfor_analysis_shares()
  coefficients <- read_parquet_sheet(output_dir, "Coefficients") |>
    dplyr::filter(
      .data$subset == "all",
      .data$entity_type == "Firm",
      .data$model %in% c("OLS", "Borda"),
      .data$outcome %in% yimfor_belief_specs$belief_outcome
    ) |>
    dplyr::transmute(
      model = .data$model,
      belief_outcome = .data$outcome,
      firm = .data$entity,
      estimate = .data$estimate
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
  for (panel_index in seq_len(nrow(yimfor_share_specs))) {
    panel_spec <- yimfor_share_specs[panel_index, ]
    for (model_value in c("OLS", "Borda")) {
      for (belief_index in seq_len(nrow(yimfor_belief_specs))) {
        belief_spec <- yimfor_belief_specs[belief_index, ]
        estimation_data <- coefficients |>
          dplyr::filter(
            .data$model == model_value,
            .data$belief_outcome == belief_spec$belief_outcome
          ) |>
          dplyr::left_join(firm_shares, by = "firm")

        if (nrow(estimation_data) != 164L ||
            any(is.na(estimation_data$analysis_share_black)) ||
            any(is.na(estimation_data$analysis_share_female))) {
          stop("Belief estimates did not merge cleanly to the workforce shares.")
        }

        for (industry_fe in c(FALSE, TRUE)) {
          result <- fit_yimfor_belief_share_regression(
            estimation_data,
            share_variable = panel_spec$share_variable,
            industry_fe = industry_fe
          )
          regression_results[[result_index]] <- dplyr::bind_cols(
            panel_spec,
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
  }

  regression_results <- dplyr::bind_rows(regression_results)
  if (nrow(regression_results) != 24L ||
      any(regression_results$n_firms != 164L) ||
      any(regression_results$n_aer_naics2 != 19L) ||
      any(regression_results$n_linkedin_shares != 161L) ||
      any(regression_results$n_naics3_share_fallbacks != 3L) ||
      any(regression_results$se_type != "HC1")) {
    stop("The Yimfor belief-share regressions failed their validation checks.")
  }

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
  }

  invisible(list(
    firm_shares = firm_shares,
    regression_results = regression_results
  ))
}

run_linkedin_share_analysis()
