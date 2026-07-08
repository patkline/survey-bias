# ------------------------------------------------------------------------------
# Purpose: Build EIV outputs that use EEO-1 industry-level workforce shares
#
# The data build writes EEO-1 employment shares by AER industry group to
# data/dump/industry_emp_share_by_demographic_eeo1.csv. This file merges those
# industry-level shares onto the Section 2 coefficient outputs and writes EIV
# specs used by the EEO-1 share-control and share-outcome tables.
# ------------------------------------------------------------------------------

eeo1_industry_shares_file <- file.path(dump, "industry_emp_share_by_demographic_eeo1.csv")
eeo1_survey_file <- file.path(processed, "long_survey_final.csv")
eeo1_eiv_sheet <- "EIV_eeo1_industry_shares"

default_eeo1_eiv_filemap <- tibble::tibble(
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

eeo1_firm_rhs_outcomes <- c("pooled_favor_male", "pooled_favor_white", "FirmSelective")
eeo1_industry_rhs_outcomes <- c("pooled_favor_male_im_w", "pooled_favor_white_im_w")
eeo1_zero_error_controls <- c(
  "eeo1_female_entry_level_share",
  "eeo1_black_entry_level_share",
  "eeo1_female_all_jobs_share",
  "eeo1_black_all_jobs_share"
)

eeo1_firm_control_specs <- list(
  list(lhs = "log_dif_gender",
       rhs = c("pooled_favor_male", "eeo1_female_entry_level_share")),
  list(lhs = "log_dif_gender",
       rhs = c("pooled_favor_male", "eeo1_female_all_jobs_share")),
  list(lhs = "log_dif",
       rhs = c("pooled_favor_white", "eeo1_black_entry_level_share")),
  list(lhs = "log_dif",
       rhs = c("pooled_favor_white", "eeo1_black_all_jobs_share"))
)

eeo1_firm_selectivity_base_specs <- list(
  list(lhs = "log_dif_gender",
       rhs = c("pooled_favor_male", "FirmSelective")),
  list(lhs = "log_dif",
       rhs = c("pooled_favor_white", "FirmSelective"))
)

eeo1_firm_selectivity_share_specs <- list(
  list(lhs = "log_dif_gender",
       rhs = c("pooled_favor_male", "FirmSelective", "eeo1_female_entry_level_share")),
  list(lhs = "log_dif_gender",
       rhs = c("pooled_favor_male", "FirmSelective", "eeo1_female_all_jobs_share")),
  list(lhs = "log_dif",
       rhs = c("pooled_favor_white", "FirmSelective", "eeo1_black_entry_level_share")),
  list(lhs = "log_dif",
       rhs = c("pooled_favor_white", "FirmSelective", "eeo1_black_all_jobs_share"))
)

eeo1_industry_outcome_specs <- list(
  list(lhs = "eeo1_female_entry_level_share",
       rhs = c("pooled_favor_male_im_w")),
  list(lhs = "eeo1_black_entry_level_share",
       rhs = c("pooled_favor_white_im_w"))
)

eeo1_key <- function(x) {
  trimws(as.character(x))
}

add_zero_error_controls_eeo1 <- function(noise_mat, controls) {
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

load_eeo1_industry_shares <- function(path = eeo1_industry_shares_file) {
  if (!file.exists(path)) {
    stop(
      "EEO-1 industry shares file not found: ",
      path,
      ". Run code/1_data_build/build_industry_emp_share_by_demographic_eeo1.do first.",
      call. = FALSE
    )
  }

  shares <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::transmute(
      eeo1_industry_key = eeo1_key(.data$sic_two_digit_bin_title_aer),
      eeo1_industry_name = .data$sic_two_digit_bin_title_aer,
      eeo1_black_all_jobs_share = as.numeric(.data$share_emp_black_all_jobs),
      eeo1_black_mid_off_manager_share = as.numeric(.data$share_emp_black_mid_off_manager),
      eeo1_black_front_line_share = as.numeric(.data$share_emp_black_front_line),
      eeo1_black_entry_level_share = as.numeric(.data$share_emp_black_front_line),
      eeo1_female_all_jobs_share = as.numeric(.data$share_emp_female_all_jobs),
      eeo1_female_mid_off_manager_share = as.numeric(.data$share_emp_female_mid_off_manager),
      eeo1_female_front_line_share = as.numeric(.data$share_emp_female_front_line),
      eeo1_female_entry_level_share = as.numeric(.data$share_emp_female_front_line)
    )

  if (anyDuplicated(shares$eeo1_industry_key)) {
    stop("EEO-1 industry shares contain duplicated industry names.", call. = FALSE)
  }

  shares
}

load_eeo1_survey_industry_map <- function(path = eeo1_survey_file) {
  if (!file.exists(path)) {
    stop("Survey file not found: ", path, call. = FALSE)
  }

  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_guess()),
    show_col_types = FALSE
  ) %>%
    dplyr::transmute(
      firm_id = as.integer(.data$firm_id),
      aer_naics2 = suppressWarnings(as.integer(.data$aer_naics2)),
      aer_naics2_name = .data$aer_naics2_name,
      eeo1_industry_key = eeo1_key(.data$aer_naics2_name)
    ) %>%
    dplyr::filter(!is.na(.data$firm_id),
                  !is.na(.data$aer_naics2),
                  !is.na(.data$aer_naics2_name)) %>%
    dplyr::distinct()
}

check_unique_key <- function(data, key, label) {
  if (anyDuplicated(data[[key]])) {
    stop(label, " has duplicated ", key, " values.", call. = FALSE)
  }
  invisible(data)
}

eeo1_coef_to_wide <- function(coef_long, entity_type_filter) {
  survey_coef_wide <- coef_long %>%
    dplyr::filter(.data$subset == "subset97",
                  .data$entity_type == entity_type_filter,
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
                  .data$entity_type == entity_type_filter,
                  .data$model == "EXPERIMENTAL") %>%
    dplyr::select(entity_id, outcome, estimate) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = entity_id,
      names_from = outcome,
      values_from = estimate
    )

  if (!nrow(experimental_wide)) return(survey_coef_wide)

  dplyr::left_join(survey_coef_wide, experimental_wide, by = "entity_id")
}

add_eeo1_shares_to_firms <- function(coef_firm_wide,
                                     industry_map = load_eeo1_survey_industry_map(),
                                     eeo1_shares = load_eeo1_industry_shares()) {
  firm_industry_map <- industry_map %>%
    dplyr::select(firm_id, aer_naics2, aer_naics2_name, eeo1_industry_key) %>%
    dplyr::distinct() %>%
    check_unique_key("firm_id", "Survey firm industry map") %>%
    dplyr::rename(entity_id = firm_id)

  out <- coef_firm_wide %>%
    dplyr::left_join(firm_industry_map, by = "entity_id") %>%
    dplyr::left_join(eeo1_shares, by = "eeo1_industry_key")

  missing_eeo1 <- out %>%
    dplyr::filter(is.na(.data$eeo1_black_entry_level_share) |
                    is.na(.data$eeo1_female_entry_level_share) |
                    is.na(.data$eeo1_black_all_jobs_share) |
                    is.na(.data$eeo1_female_all_jobs_share))
  if (nrow(missing_eeo1)) {
    stop(
      "Some firm industries do not match the EEO-1 industry-share file: ",
      paste(unique(missing_eeo1$aer_naics2_name), collapse = ", "),
      call. = FALSE
    )
  }

  out
}

add_eeo1_shares_to_industries <- function(coef_industry_wide,
                                          industry_map = load_eeo1_survey_industry_map(),
                                          eeo1_shares = load_eeo1_industry_shares()) {
  industry_lookup <- industry_map %>%
    dplyr::select(aer_naics2, aer_naics2_name, eeo1_industry_key) %>%
    dplyr::distinct() %>%
    check_unique_key("aer_naics2", "Survey industry map")

  out <- coef_industry_wide %>%
    dplyr::left_join(industry_lookup, by = c("entity_id" = "aer_naics2")) %>%
    dplyr::left_join(eeo1_shares, by = "eeo1_industry_key")

  missing_eeo1 <- out %>%
    dplyr::filter(is.na(.data$eeo1_black_entry_level_share) |
                    is.na(.data$eeo1_female_entry_level_share) |
                    is.na(.data$eeo1_black_all_jobs_share) |
                    is.na(.data$eeo1_female_all_jobs_share))
  if (nrow(missing_eeo1)) {
    stop(
      "Some industry coefficients do not match the EEO-1 industry-share file: ",
      paste(unique(missing_eeo1$entity_id), collapse = ", "),
      call. = FALSE
    )
  }

  out
}

run_eeo1_eiv_for_subdir <- function(
    subdir,
    industry_map = load_eeo1_survey_industry_map(),
    eeo1_shares = load_eeo1_industry_shares(),
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

  coef_firm_wide <- eeo1_coef_to_wide(coef_long, "Firm") %>%
    add_eeo1_shares_to_firms(industry_map = industry_map,
                             eeo1_shares = eeo1_shares)

  coef_industry_wide <- eeo1_coef_to_wide(coef_long, "Industry") %>%
    add_eeo1_shares_to_industries(industry_map = industry_map,
                                  eeo1_shares = eeo1_shares)

  firm_noise_mats <- setNames(vector("list", length(models_to_run)), models_to_run)
  industry_noise_mats <- setNames(vector("list", length(models_to_run)), models_to_run)

  for (model in models_to_run) {
    firm_noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = eeo1_firm_rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    ) %>%
      add_zero_error_controls_eeo1(eeo1_zero_error_controls)

    industry_noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = eeo1_industry_rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    )
  }

  firm_eiv <- run_eiv_suite(
    regs = eeo1_firm_control_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = firm_noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = FALSE
  ) %>%
    dplyr::mutate(
      eeo1_regression_level = "Firm",
      eeo1_spec_group = "share_control"
    )

  firm_selectivity_base_eiv <- run_eiv_suite(
    regs = eeo1_firm_selectivity_base_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = firm_noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = TRUE
  ) %>%
    dplyr::mutate(
      eeo1_regression_level = "Firm",
      eeo1_spec_group = "belief_selectivity"
    )

  firm_selectivity_share_eiv <- run_eiv_suite(
    regs = eeo1_firm_selectivity_share_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = firm_noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = FALSE
  ) %>%
    dplyr::mutate(
      eeo1_regression_level = "Firm",
      eeo1_spec_group = "belief_selectivity_share"
    )

  industry_eiv <- run_eiv_suite(
    regs = eeo1_industry_outcome_specs,
    coef_df_wide = coef_industry_wide,
    noise_mats_97 = industry_noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    weights_col = "njobs",
    use_fe = FALSE
  ) %>%
    dplyr::mutate(
      eeo1_regression_level = "Industry",
      eeo1_spec_group = "share_outcome"
    )

  eiv_df <- dplyr::bind_rows(
    firm_eiv,
    firm_selectivity_base_eiv,
    firm_selectivity_share_eiv,
    industry_eiv
  )

  if (isTRUE(write_sheet)) {
    write_parquet_sheet(dir_path, eeo1_eiv_sheet, eiv_df)
  }

  invisible(eiv_df)
}

run_eeo1_eiv_for_subdirs <- function(
    subdirs = default_eeo1_eiv_filemap$subdir,
    write_sheet = TRUE
) {
  industry_map <- load_eeo1_survey_industry_map()
  eeo1_shares <- load_eeo1_industry_shares()

  setNames(
    lapply(
      subdirs,
      run_eeo1_eiv_for_subdir,
      industry_map = industry_map,
      eeo1_shares = eeo1_shares,
      write_sheet = write_sheet
    ),
    subdirs
  )
}
