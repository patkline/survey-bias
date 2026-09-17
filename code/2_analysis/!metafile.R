# ------------------------------------------------------------------------------
# Purpose: Metafile for analysis
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# Load Necessary Functions
source(file.path(analysis, "load_all.R"))

## Read in Data
file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Outcomes used in the main draft exhibits and the within/between-industry table.
standard_survey_vars <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male", "FirmHire_favor_male", "conduct_favor_male",
  "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)

# Additional raw framing outcomes used by Appendix Table A1 and Figure A3.
alternate_framing_vars <- c(
  "FirmCont_black", "FirmCont_white", "FirmHire_black", "FirmHire_white",
  "conduct_black", "conduct_white", "FirmCont_male", "FirmCont_female",
  "FirmHire_male", "FirmHire_female", "conduct_male", "conduct_female",
  "conduct_younger", "conduct_older"
)

# Preserve the established outcome order so the orientation of pairwise
# covariance rows remains stable across replication runs.
survey_vars <- c(
  "FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
  "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
  "conduct_favor_white", "conduct_black", "conduct_white",
  "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
  "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
  "conduct_favor_male", "conduct_male", "conduct_female",
  "conduct_favor_younger", "conduct_younger", "conduct_older",
  "discretion", "FirmSelective", "FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)
stopifnot(setequal(survey_vars, unique(c(standard_survey_vars, alternate_framing_vars))))
subgroup_survey_vars <- c(
  "pooled_favor_white", "pooled_favor_male", "conduct_favor_younger"
)
experimental_vars <- c("log_dif", "log_dif_gender")

firms97 <- data %>% dplyr::filter(!is.na(log_dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)

subset_var <- NULL
subset_value <- NULL
output_dir <- file.path(intermediate, "Full_Sample")

run_analysis_pipeline(
  data, survey_vars, experimental_vars,
  subset_var = subset_var, subset_value = subset_value,
  output_dir = output_dir, firms97 = firms97,
  run_borda = TRUE, run_ols = TRUE,
  industry_mean_outcomes = standard_survey_vars,
  seed = 123
)

# Respondent-pair AMAD statistics used by the belief summary tables.
# This is intentionally computed in section 2; section 3 only formats outputs.
run_belief_summary_amad_analysis(data, output_dir)

# Merge the 2023 Yimfor LinkedIn shares and run the firm-level belief
# regressions used by the corresponding section 3 table.
source(file.path(analysis, "linkedin_share_analysis.R"))

# Compare latest-filing EEO-1 shares with the 2023 Yimfor LinkedIn shares
source(file.path(analysis, "eeo1_yimfor_correlations.R"))

#---- 1) Define the subset runs (mirrors your bash VARS/VALS/OUTS) ----
runs <- tibble::tribble(
  ~subset_var,   ~subset_value, ~output_stub,
  "confidence_race", 1,         "Subset_Conf_Race_Y",
  "confidence_race", 0,         "Subset_Conf_Race_N",
  "confidence_gend", 1,         "Subset_Conf_Gender_Y",
  "confidence_gend", 0,         "Subset_Conf_Gender_N",
  "sample",          1,         "Subset_Probability",
  "sample",          0,         "Subset_Convenience",
  "gender",          1,         "Subset_Female",
  "gender",          0,         "Subset_Male",
  "race",            1,         "Subset_Black",
  "race",            0,         "Subset_White",
  "age",             1,         "Subset_Age_gte40",
  "age",             0,         "Subset_Age_lt40",
  "looking_job",     1,         "Subset_Looking",
  "looking_job",     0,         "Subset_Not_Looking",
  "fear",            1,         "Subset_Feared_Discrimination_1",
  "fear",            0,         "Subset_Feared_Discrimination_0",
  "educ",            1,         "Subset_College",
  "educ",            0,         "Subset_No_College"
)


# ---- 3) Run them all ----
for (i in seq_len(nrow(runs))) {
  subset_var   <- runs$subset_var[i]
  subset_value <- runs$subset_value[i]

  output_dir <- file.path(intermediate, runs$output_stub[i])

  cat("\n=== Running:", runs$output_stub[i],
      "| subset_var =", subset_var,
      "| subset_value =", subset_value,
      "===\n")

  run_analysis_pipeline(
    data, subgroup_survey_vars,
    experimental_vars = NULL,
    subset_var = subset_var, subset_value = subset_value,
    output_dir = output_dir, firms97 = firms97,
    run_borda = TRUE, run_ols = TRUE,
    industry_mean_outcomes = NULL,
    seed = 123
  )
}

message("Running full-sample belief-selectivity EIV output")
run_belief_selectivity_eiv_for_subdir("Full_Sample")
