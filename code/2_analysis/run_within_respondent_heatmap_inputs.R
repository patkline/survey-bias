# ------------------------------------------------------------------------------
# Purpose: Targeted builder for the within-respondent heatmap inputs
#
# Writes the correlation_within_respondent sheet: for each pair of the survey
# measures shown on the paper's signal-correlation heatmaps, the within-
# respondent correlation of Likert (OLS) or Borda scores --- the equal-weighted
# average across respondents of each respondent's covariance across the firms
# they rated on both questions, divided by the square root of the product of
# the corresponding average within-respondent variances --- plus the
# OLS x Borda diagonal. See within_respondent_correlations.R for the
# construction and the diagnostic columns.
#
# This skips the full section 2 metafile: it needs only the analysis data, not
# the stored model results, and can be run standalone to backfill the sheet.
#
# The minimum number of overlapping firms a respondent needs to contribute
# moments defaults to 2 and can be overridden with the environment variable
# WITHIN_RESPONDENT_MIN_FIRMS.
#
# Created: Evan K. Rose 2026-08-29
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

min_common_firms <- Sys.getenv("WITHIN_RESPONDENT_MIN_FIRMS", unset = "2")
min_common_firms <- suppressWarnings(as.integer(min_common_firms))
if (is.na(min_common_firms) || min_common_firms < 2L) {
  stop(
    "Invalid WITHIN_RESPONDENT_MIN_FIRMS; it must be an integer >= 2.",
    call. = FALSE
  )
}

message("Building within-respondent heatmap inputs only")
message("  minimum overlapping firms per respondent: ", min_common_firms)
message("  intermediate path: ", intermediate)

file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

# The ten survey measures shown on the signal-correlation heatmaps
heatmap_survey_vars <- c(
  "conduct_favor_white", "FirmHire_favor_white", "FirmCont_favor_white",
  "conduct_favor_male", "FirmHire_favor_male", "FirmCont_favor_male",
  "conduct_favor_younger",
  "FirmDesire", "FirmSelective", "discretion"
)

output_dir <- file.path(intermediate, "Full_Sample")

# The 97 firms with experimental callback data, as in the pipeline runners
firms97 <- data %>%
  dplyr::filter(!is.na(.data$dif)) %>%
  dplyr::select(firm_id) %>%
  dplyr::distinct() %>%
  dplyr::pull(firm_id)

correlation_within_respondent_df <- write_within_respondent_correlation_sheet(
  data = data,
  outcomes = heatmap_survey_vars,
  output_dir = output_dir,
  sheet_name = "correlation_within_respondent",
  firms97 = firms97,
  min_common_firms = min_common_firms
)

message("Within-respondent heatmap input write check:")
check_path <- parquet_sheet_path(output_dir, "correlation_within_respondent")
check_info <- file.info(check_path)
message(
  "  ", basename(check_path),
  " | exists=", file.exists(check_path),
  " | size=", check_info$size,
  " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S")
)

message("Within-respondent heatmap inputs complete")
