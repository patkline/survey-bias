# ------------------------------------------------------------------------------
# Purpose: Build EIV outputs that use Revelio firm-level measures
#
# This file assumes the firm-name cleaning and matching has already happened in
# the data build and is available as data/processed/revelio_firm_measures.csv.
# ------------------------------------------------------------------------------

revelio_firm_measures_file <- file.path(processed, "revelio_firm_measures.csv")

default_revelio_eiv_filemap <- tibble::tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination",
             "40 Years or Older",
             "Less than 40 Years Old"),
  subdir = c("Full_Sample",
             "Subset_Black",
             "Subset_White",
             "Subset_Female",
             "Subset_Male",
             "Subset_Looking",
             "Subset_Not_Looking",
             "Subset_Feared_Discrimination_1",
             "Subset_Feared_Discrimination_0",
             "Subset_Age_gte40",
             "Subset_Age_lt40")
)

revelio_rhs_outcomes <- c(
  "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male",
  "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"
)

revelio_zero_error_controls <- c("female_share", "black_share")

add_zero_error_controls <- function(noise_mat, controls) {
  missing_controls <- setdiff(controls, rownames(noise_mat))
  if (!length(missing_controls)) return(noise_mat)

  expanded_names <- c(rownames(noise_mat), missing_controls)
  out <- matrix(
    0,
    nrow = length(expanded_names),
    ncol = length(expanded_names),
    dimnames = list(expanded_names, expanded_names)
  )

  out[rownames(noise_mat), colnames(noise_mat)] <- noise_mat
  out
}

safe_log_gap <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  ifelse(
    !is.na(numerator) & !is.na(denominator) & numerator > 0 & denominator > 0,
    log(numerator) - log(denominator),
    NA_real_
  )
}

revelio_eiv_specs <- list(
  list(lhs = "female_share", rhs = c("FirmCont_favor_male")),
  list(lhs = "female_share", rhs = c("conduct_favor_male")),
  list(lhs = "female_share", rhs = c("pooled_favor_male")),
  list(lhs = "black_share", rhs = c("FirmCont_favor_white")),
  list(lhs = "black_share", rhs = c("conduct_favor_white")),
  list(lhs = "black_share", rhs = c("pooled_favor_white")),
  list(lhs = "gender_wage_gap", rhs = c("FirmCont_favor_male")),
  list(lhs = "gender_wage_gap", rhs = c("conduct_favor_male")),
  list(lhs = "gender_wage_gap", rhs = c("pooled_favor_male")),
  list(lhs = "race_wage_gap", rhs = c("FirmCont_favor_white")),
  list(lhs = "race_wage_gap", rhs = c("conduct_favor_white")),
  list(lhs = "race_wage_gap", rhs = c("pooled_favor_white")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("FirmCont_favor_male")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("conduct_favor_male")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("pooled_favor_male")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("FirmCont_favor_white")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("conduct_favor_white")),
  list(lhs = "avg_di_rating_current_ft_seniority1_2023", rhs = c("pooled_favor_white")),
  list(lhs = "log_dif_gender", rhs = c("FirmCont_favor_male", "female_share")),
  list(lhs = "log_dif_gender", rhs = c("conduct_favor_male", "female_share")),
  list(lhs = "log_dif_gender", rhs = c("pooled_favor_male", "female_share")),
  list(lhs = "log_dif", rhs = c("FirmCont_favor_white", "black_share")),
  list(lhs = "log_dif", rhs = c("conduct_favor_white", "black_share")),
  list(lhs = "log_dif", rhs = c("pooled_favor_white", "black_share"))
)

load_revelio_firm_measures <- function(path = revelio_firm_measures_file) {
  if (!file.exists(path)) {
    stop(
      "Revelio firm measures file not found: ",
      path,
      ". Run code/1_data_build/build_revelio_firm_measures.py first.",
      call. = FALSE
    )
  }

  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::transmute(
      entity_id = as.integer(.data$firm_id),
      revelio_firm = .data$firm,
      aer_naics2 = suppressWarnings(as.integer(.data$aer_naics2)),
      matched_to_revelio = .data$matched_to_revelio,
      female_share = .data$female_share,
      male_share = .data$male_share,
      white_share = .data$white_share,
      black_share = .data$black_share,
      api_share = .data$api_share,
      hispanic_share = .data$hispanic_share,
      native_share = .data$native_share,
      multiple_share = .data$multiple_share,
      avg_salary_all = .data$avg_salary_all,
      avg_salary_female = .data$avg_salary_female,
      avg_salary_male = .data$avg_salary_male,
      avg_salary_white = .data$avg_salary_white,
      avg_salary_black = .data$avg_salary_black,
      avg_salary_api = .data$avg_salary_api,
      avg_salary_hispanic = .data$avg_salary_hispanic,
      avg_salary_native = .data$avg_salary_native,
      avg_salary_multiple = .data$avg_salary_multiple,
      race_wage_gap = safe_log_gap(.data$avg_salary_white, .data$avg_salary_black),
      gender_wage_gap = safe_log_gap(.data$avg_salary_male, .data$avg_salary_female),
      div_and_inclusion_sentiment = .data$div_and_inclusion_sentiment,
      avg_di_rating_current_ft_seniority1_2023 = .data$avg_di_rating_current_ft_seniority1_2023
    )
}

revelio_coef_to_firm_wide <- function(coef_long) {
  survey_coef_wide <- coef_long %>%
    dplyr::filter(.data$subset == "subset97",
                  .data$entity_type == "Firm",
                  .data$model %in% c("OLS", "Borda")) %>%
    dplyr::select(model, entity_id, entity, outcome, estimate, njobs) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = c(model, entity_id, entity, njobs),
      names_from = outcome,
      values_from = estimate
    )

  experimental_wide <- coef_long %>%
    dplyr::filter(.data$subset == "subset97",
                  .data$entity_type == "Firm",
                  .data$model == "EXPERIMENTAL") %>%
    dplyr::select(entity_id, outcome, estimate) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = entity_id,
      names_from = outcome,
      values_from = estimate
    )

  dplyr::left_join(survey_coef_wide, experimental_wide, by = "entity_id")
}

run_revelio_eiv_for_subdir <- function(
    subdir,
    revelio_measures = load_revelio_firm_measures(),
    write_sheet = TRUE,
    models_to_run = c("OLS", "Borda")
) {
  dir_path <- file.path(intermediate, subdir)
  if (!dir.exists(dir_path)) {
    warning("Skipping missing intermediate directory: ", dir_path)
    return(tibble::tibble())
  }

  coef_long <- read_parquet_sheet(dir_path, "Coefficients")
  variance_df <- read_parquet_sheet(dir_path, "variance")
  covariance_df <- read_parquet_sheet(dir_path, "covariance")

  coef_firm_wide <- revelio_coef_to_firm_wide(coef_long) %>%
    dplyr::left_join(revelio_measures, by = "entity_id")

  noise_mats <- setNames(vector("list", length(models_to_run)), models_to_run)

  for (model in models_to_run) {
    noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = revelio_rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    ) %>%
      add_zero_error_controls(revelio_zero_error_controls)
  }

  eiv_df <- run_eiv_suite(
    regs = revelio_eiv_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = TRUE
  )

  if (isTRUE(write_sheet)) {
    write_parquet_sheet(dir_path, "EIV_revelio_firm", eiv_df)
  }

  invisible(eiv_df)
}

run_revelio_eiv_for_subdirs <- function(
    subdirs = default_revelio_eiv_filemap$subdir,
    write_sheet = TRUE
) {
  revelio_measures <- load_revelio_firm_measures()

  setNames(
    lapply(
      subdirs,
      run_revelio_eiv_for_subdir,
      revelio_measures = revelio_measures,
      write_sheet = write_sheet
    ),
    subdirs
  )
}
