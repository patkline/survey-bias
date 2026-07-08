# ------------------------------------------------------------------------------
# Purpose: Targeted builder for within/between-industry heatmap inputs
#
# This reruns the model-building objects needed in memory, but only writes the
# requested industry-decomposition correlation inputs:
#   - covariance_within_industry / correlation_within_industry
#   - covariance_between_industry / correlation_between_industry
#
# It intentionally skips the full section 2 metafile's subgroup loops and EIV
# blocks. Use this when the standard Full_Sample outputs already exist and only
# the industry heatmap correlation sheets need to be backfilled.
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

industry_heatmap_inputs_mode <- Sys.getenv("INDUSTRY_HEATMAP_INPUTS", unset = "all")
valid_industry_heatmap_inputs_modes <- c("all", "within", "between")
if (!industry_heatmap_inputs_mode %in% valid_industry_heatmap_inputs_modes) {
  stop(
    "Invalid INDUSTRY_HEATMAP_INPUTS='", industry_heatmap_inputs_mode, "'. ",
    "Valid values are: ", paste(valid_industry_heatmap_inputs_modes, collapse = ", "),
    call. = FALSE
  )
}

run_within_industry_inputs <- industry_heatmap_inputs_mode %in% c("all", "within")
run_between_industry_inputs <- industry_heatmap_inputs_mode %in% c("all", "between")

message("Building industry heatmap inputs only")
message("  mode: ", industry_heatmap_inputs_mode)
message("  intermediate path: ", intermediate)

file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

survey_vars <- c(
  "FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
  "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
  "conduct_favor_white", "conduct_black", "conduct_white",
  "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
  "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
  "conduct_favor_male", "conduct_male", "conduct_female",
  "conduct_favor_younger", "conduct_younger", "conduct_older",
  "discretion", "FirmSelective", "FirmDesire",
  "pooled_favor_white", "pooled_favor_male",
  "pooled_white", "pooled_black",
  "pooled_male", "pooled_female"
)

valence_triples <- list(
  list(valence1 = "conduct_black", valence2 = "conduct_white", new_outcome = "conduct_favor_white_ep"),
  list(valence1 = "FirmCont_black", valence2 = "FirmCont_white", new_outcome = "FirmCont_favor_white_ep"),
  list(valence1 = "FirmHire_black", valence2 = "FirmHire_white", new_outcome = "FirmHire_favor_white_ep"),
  list(valence1 = "pooled_black", valence2 = "pooled_white", new_outcome = "pooled_favor_white_ep"),
  list(valence1 = "conduct_female", valence2 = "conduct_male", new_outcome = "conduct_favor_male_ep"),
  list(valence1 = "FirmCont_female", valence2 = "FirmCont_male", new_outcome = "FirmCont_favor_male_ep"),
  list(valence1 = "FirmHire_female", valence2 = "FirmHire_male", new_outcome = "FirmHire_favor_male_ep"),
  list(valence1 = "pooled_female", valence2 = "pooled_male", new_outcome = "pooled_favor_male_ep"),
  list(valence1 = "conduct_older", valence2 = "conduct_younger", new_outcome = "conduct_favor_younger_ep")
)

experimental_vars <- c(
  "dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age",
  "log_dif_age", "log_dif_gender_sq", "log_dif_sq", "cb_central_full"
)

respondent_col <- "ResponseId"
output_dir <- file.path(intermediate, "Full_Sample")
firms97 <- data %>%
  dplyr::filter(!is.na(.data$dif)) %>%
  dplyr::select(firm_id) %>%
  dplyr::distinct() %>%
  dplyr::pull(firm_id)

industry_map <- data %>%
  dplyr::select(firm_id, aer_naics2, aer_naics2_name) %>%
  dplyr::distinct() %>%
  dplyr::mutate(firm_id = as.integer(.data$firm_id))

stopifnot(all(c("firm_id", "aer_naics2", "aer_naics2_name") %in% names(data)))
stopifnot(nrow(industry_map) == dplyr::n_distinct(industry_map$firm_id))
stopifnot(sum(is.na(industry_map$aer_naics2)) == 0)

set.seed(123)

prep <- prep_outcomes(data, survey_vars)

results <- run_models(
  survey_vars = survey_vars,
  respondent_col = respondent_col,
  data_wide_list = prep$wide,
  data_long_list = prep$long,
  id_map_list = prep$id_map,
  experimental_vars = experimental_vars,
  data_for_experimental = data,
  run_borda = TRUE,
  run_ols = TRUE,
  firms97 = firms97,
  seed = 123,
  build_subset97 = TRUE,
  combine_valences = TRUE,
  valence_triples = valence_triples
)

results <- add_industry_means_to_results(
  results = results,
  industry_map = industry_map,
  outcomes = survey_vars,
  model_names = c("Borda", "OLS"),
  which_sets = c("all", "subset97"),
  industry_col = "aer_naics2",
  suffix_dm = "_dm",
  suffix_im = "_im",
  weight_col = "njobs",
  suffix_dm_w = "_dm_w",
  suffix_im_w = "_im_w",
  require_positive_weights = TRUE
)

variance_df <- read_parquet_sheet(output_dir, "variance")
sheets_to_check <- character(0)

if (run_within_industry_inputs) {
  write_within_industry_correlation_sheets(
    results = results,
    output_dir = output_dir,
    survey_vars = survey_vars,
    variance_df = variance_df
  )
  sheets_to_check <- c(
    sheets_to_check,
    "covariance_within_industry",
    "correlation_within_industry"
  )
}

if (run_between_industry_inputs) {
  write_between_industry_correlation_sheets(
    results = results,
    output_dir = output_dir,
    survey_vars = survey_vars,
    variance_df = variance_df
  )
  sheets_to_check <- c(
    sheets_to_check,
    "covariance_between_industry",
    "correlation_between_industry"
  )
}

message("Industry heatmap input write check:")
for (sheet in sheets_to_check) {
  check_path <- parquet_sheet_path(output_dir, sheet)
  check_info <- file.info(check_path)
  message(
    "  ", basename(check_path),
    " | exists=", file.exists(check_path),
    " | size=", check_info$size,
    " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S")
  )
}

message("Industry heatmap inputs complete")
