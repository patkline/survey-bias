# ------------------------------------------------------------------------------
# Purpose: Targeted builder for the within-respondent cross-question AMAD sheet
#
# Writes belief_amad_within_respondent: the belief-summary AMAD idea applied at
# the person-firm level across questions --- for each same-construct question
# pair, the mean absolute difference between one respondent's two answers about
# the same firm (Likert) or the same firm pair (Borda win indicators), averaged
# equally across firms. Directly comparable to the between-respondent AMADs in
# belief_amad_summary. See within_respondent_cross_question_amad.R for the
# construction.
#
# This skips the full section 2 metafile: it needs only the analysis data and
# can be run standalone to backfill the sheet.
#
# Created: Evan K. Rose 2026-08-29
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

message("Building within-respondent cross-question AMAD inputs only")
message("  intermediate path: ", intermediate)

file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

run_within_respondent_cross_question_amad_analysis(
  survey_data = data,
  output_dir = file.path(intermediate, "Full_Sample")
)

# The ten survey measures shown on the signal-correlation heatmaps; the Borda
# win indicator is scale-free, so every feasible pair gets an AMAD
heatmap_survey_vars <- c(
  "conduct_favor_white", "FirmHire_favor_white", "FirmCont_favor_white",
  "conduct_favor_male", "FirmHire_favor_male", "FirmCont_favor_male",
  "conduct_favor_younger",
  "FirmDesire", "FirmSelective", "discretion"
)

run_within_respondent_borda_amad_pairs_analysis(
  survey_data = data,
  outcomes = heatmap_survey_vars,
  output_dir = file.path(intermediate, "Full_Sample")
)

message("Within-respondent cross-question AMAD input write check:")
for (sheet in c("belief_amad_within_respondent", "belief_amad_within_respondent_borda_pairs")) {
  check_path <- parquet_sheet_path(file.path(intermediate, "Full_Sample"), sheet)
  check_info <- file.info(check_path)
  message(
    "  ", basename(check_path),
    " | exists=", file.exists(check_path),
    " | size=", check_info$size,
    " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S")
  )
}

message("Within-respondent cross-question AMAD inputs complete")
