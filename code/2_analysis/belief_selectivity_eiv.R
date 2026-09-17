# ------------------------------------------------------------------------------
# Purpose: Build the belief-selectivity EIV output used by Table 7
# ------------------------------------------------------------------------------

belief_selectivity_eiv_sheet <- "EIV_belief_selectivity"
belief_selectivity_survey_file <- file.path(processed, "long_survey_final.csv")

belief_selectivity_rhs_outcomes <- c(
  "pooled_favor_male",
  "pooled_favor_white",
  "FirmSelective"
)

belief_selectivity_specs <- list(
  list(
    lhs = "log_dif_gender",
    rhs = c("pooled_favor_male", "FirmSelective")
  ),
  list(
    lhs = "log_dif",
    rhs = c("pooled_favor_white", "FirmSelective")
  )
)

load_belief_selectivity_firm_industry_map <- function(
    path = belief_selectivity_survey_file
) {
  if (!file.exists(path)) {
    stop("Survey file not found: ", path, call. = FALSE)
  }

  firm_industry_map <- readr::read_csv(
    path,
    col_types = readr::cols(
      .default = readr::col_skip(),
      firm_id = readr::col_integer(),
      aer_naics2 = readr::col_integer()
    ),
    show_col_types = FALSE
  ) |>
    dplyr::filter(!is.na(.data$firm_id), !is.na(.data$aer_naics2)) |>
    dplyr::distinct(.data$firm_id, .data$aer_naics2)

  if (nrow(firm_industry_map) != 164L ||
      anyDuplicated(firm_industry_map$firm_id) ||
      dplyr::n_distinct(firm_industry_map$aer_naics2) != 19L) {
    stop(
      "Expected a unique 164-firm map spanning 19 AER industries.",
      call. = FALSE
    )
  }

  firm_industry_map
}

add_aer_industry_to_firm_coefficients <- function(
    coef_firm_wide,
    firm_industry_map = load_belief_selectivity_firm_industry_map()
) {
  out <- coef_firm_wide |>
    dplyr::left_join(
      dplyr::rename(firm_industry_map, entity_id = firm_id),
      by = "entity_id"
    )

  if (any(is.na(out$aer_naics2))) {
    stop(
      "At least one firm coefficient is missing its AER industry.",
      call. = FALSE
    )
  }

  out
}

run_belief_selectivity_eiv_for_subdir <- function(
    subdir = "Full_Sample",
    firm_industry_map = load_belief_selectivity_firm_industry_map(),
    write_sheet = TRUE,
    models_to_run = c("OLS", "Borda")
) {
  dir_path <- file.path(intermediate, subdir)
  if (!dir.exists(dir_path)) {
    stop("Missing intermediate directory: ", dir_path, call. = FALSE)
  }

  coef_long <- read_parquet_sheet(dir_path, "Coefficients")
  variance_df <- read_parquet_sheet(dir_path, "variance")
  covariance_df <- read_parquet_sheet(dir_path, "covariance")

  coef_firm_wide <- eiv_coefficients_to_wide(coef_long, "Firm") |>
    add_aer_industry_to_firm_coefficients(
      firm_industry_map = firm_industry_map
    )

  noise_mats <- stats::setNames(
    vector("list", length(models_to_run)),
    models_to_run
  )
  for (model in models_to_run) {
    noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = belief_selectivity_rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    )
  }

  eiv_df <- run_eiv_suite(
    regs = belief_selectivity_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = TRUE
  ) |>
    dplyr::mutate(
      regression_level = "Firm",
      spec_group = "belief_selectivity"
    )

  if (nrow(eiv_df) != 16L ||
      !setequal(eiv_df$model, models_to_run) ||
      !all(eiv_df$spec_group == "belief_selectivity")) {
    stop("The belief-selectivity EIV output failed validation.", call. = FALSE)
  }

  if (isTRUE(write_sheet)) {
    write_parquet_sheet(dir_path, belief_selectivity_eiv_sheet, eiv_df)
  }

  invisible(eiv_df)
}
