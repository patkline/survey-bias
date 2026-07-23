# ------------------------------------------------------------------------------
# Purpose: EIV contact-gap regressions with EEO-1 NAICS3 share controls
# ------------------------------------------------------------------------------

eeo1_naics3_eiv_sheet <- "EIV_eeo1_naics3_shares"

eeo1_naics3_rhs_outcomes <- c(
  "pooled_favor_male",
  "pooled_favor_white"
)

eeo1_naics3_zero_error_controls <- c(
  "eeo1_female_front_line_share",
  "eeo1_female_all_jobs_share",
  "eeo1_black_front_line_share",
  "eeo1_black_all_jobs_share"
)

eeo1_naics3_control_specs <- list(
  list(
    lhs = "log_dif_gender",
    rhs = c("pooled_favor_male", "eeo1_female_front_line_share")
  ),
  list(
    lhs = "log_dif_gender",
    rhs = c("pooled_favor_male", "eeo1_female_all_jobs_share")
  ),
  list(
    lhs = "log_dif",
    rhs = c("pooled_favor_white", "eeo1_black_front_line_share")
  ),
  list(
    lhs = "log_dif",
    rhs = c("pooled_favor_white", "eeo1_black_all_jobs_share")
  )
)

run_eeo1_naics3_eiv_for_subdir <- function(
    subdir = "Full_Sample",
    crosswalk = load_firm_naics3_crosswalk(),
    shares = load_eeo1_naics3_shares(),
    write_sheet = TRUE,
    models_to_run = c("OLS", "Borda")
) {
  dir_path <- file.path(intermediate, subdir)
  if (!dir.exists(dir_path)) {
    stop("Missing intermediate directory: ", dir_path)
  }

  coef_long <- read_parquet_sheet(dir_path, "Coefficients")
  variance_df <- read_parquet_sheet(dir_path, "variance")
  covariance_df <- read_parquet_sheet(dir_path, "covariance")

  coef_firm_wide <- eeo1_coef_to_wide(coef_long, "Firm") |>
    add_eeo1_naics3_shares_to_firms(
      firm_name_col = "entity",
      crosswalk = crosswalk,
      shares = shares
    )

  noise_mats <- stats::setNames(
    vector("list", length(models_to_run)),
    models_to_run
  )

  for (model in models_to_run) {
    noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = eeo1_naics3_rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    ) |>
      add_zero_error_controls_eeo1(eeo1_naics3_zero_error_controls)
  }

  eiv_df <- run_eiv_suite(
    regs = eeo1_naics3_control_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "naics3",
    weights_col = "njobs",
    cluster_col = "naics3",
    cluster_df_adj = TRUE,
    use_fe = FALSE
  ) |>
    dplyr::mutate(
      eeo1_regression_level = "Firm",
      eeo1_spec_group = "naics3_share_control"
    )

  if (nrow(eiv_df) != 16L) {
    stop("Expected 16 EEO-1 NAICS3 share-control EIV output rows.")
  }

  if (isTRUE(write_sheet)) {
    write_parquet_sheet(dir_path, eeo1_naics3_eiv_sheet, eiv_df)
  }

  invisible(eiv_df)
}
