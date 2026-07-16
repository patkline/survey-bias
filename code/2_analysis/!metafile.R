# ------------------------------------------------------------------------------
# Purpose: Metafile for analysis
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")
message("🎃 2_analysis working directory: ", getwd())
message("🎃 2_analysis intermediate path: ", intermediate)
message("🎃 2_analysis tables path: ", tables)

# Load Necessary Functions
source(file.path(analysis, "load_all.R"))
source(file.path(analysis, "analysis_pipeline.R"))

# 
# ## 0) parse args -------------------------------------------------------------
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) != 3) {
#   stop("Usage: Rscript run_wrapper.R <subset_var> <subset_value> <output_filename>")
# }
# 
# subset_var      <- args[[1]]                   # still a string
# subset_value    <- as.integer(args[[2]])       # coerce to integer
# if (is.na(subset_value)) {
#   stop("`<subset_value>` must be an integer")
# }
# output_filename <- args[[3]]                   # still a string


## Read in Data
file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Define outcomes and relevant columns
survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white", 
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female", 
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female", 
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older", 
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white","pooled_favor_male", 
                 "pooled_white", "pooled_black",
                 "pooled_male", "pooled_female")

valence_triples <- list(
  list(valence1 = "FirmCont_black",    valence2 = "FirmCont_white",    new_outcome = "FirmCont_favor_white_ep"),
  list(valence1 = "FirmHire_black",    valence2 = "FirmHire_white",    new_outcome = "FirmHire_favor_white_ep"),
  list(valence1 = "pooled_black",      valence2 = "pooled_white",    new_outcome = "pooled_favor_white_ep"),
  list(valence1 = "conduct_female",     valence2 = "conduct_male",     new_outcome = "conduct_favor_male_ep"),
  list(valence1 = "FirmCont_female",    valence2 = "FirmCont_male",    new_outcome = "FirmCont_favor_male_ep"),
  list(valence1 = "FirmHire_female",    valence2 = "FirmHire_male",    new_outcome = "FirmHire_favor_male_ep"),
  list(valence1 = "pooled_female",      valence2 = "pooled_male",    new_outcome = "pooled_favor_male_ep"),
  list(valence1 = "conduct_older",     valence2 = "conduct_younger",     new_outcome = "conduct_favor_younger_ep")
)

experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "log_dif_gender_sq", "log_dif_sq", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

firms97 <- data %>% dplyr::filter(!is.na(dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)

subset_var <- NULL
subset_value <- NULL
output_dir <- file.path(intermediate, "Full_Sample")
analysis_check_sheets <- c(
  "Coefficients", "rcov", "variance", "covariance", "correlation",
  "covariance_within_industry", "correlation_within_industry",
  "covariance_between_industry", "correlation_between_industry",
  "EIV_firm", "EIV_within", "EIV_between",
  "EIV_within_selectivity", "EIV_between_selectivity"
)

# Function Call Female
system.time({
  run_analysis_pipeline(
    data, respondent_col, survey_vars, experimental_vars,
    subset_var = subset_var, subset_value = subset_value,
    output_dir = output_dir, firms97 = firms97,
    run_ol = FALSE, run_pl = FALSE, run_borda = TRUE, run_ols = TRUE, run_ols_centered = FALSE,
    combine_valences = TRUE, valence_triples = valence_triples, industry_means = TRUE,
    seed = 123
  )
})

message("🎃 Full_Sample write check:")
for (sheet in analysis_check_sheets) {
  check_path <- parquet_sheet_path(file.path(intermediate, "Full_Sample"), sheet)
  check_info <- file.info(check_path)
  message("  ", basename(check_path), " | exists=", file.exists(check_path),
          " | size=", check_info$size,
          " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S"))
}


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
results <- vector("list", nrow(runs))

for (i in seq_len(nrow(runs))) {
  subset_var   <- runs$subset_var[i]
  subset_value <- runs$subset_value[i]

  output_dir <- file.path(intermediate, runs$output_stub[i])

  cat("\n=== Running:", runs$output_stub[i],
      "| subset_var =", subset_var,
      "| subset_value =", subset_value,
      "===\n")

  results[[i]] <- system.time({
    run_analysis_pipeline(
      data, respondent_col, survey_vars, experimental_vars,
      subset_var = subset_var, subset_value = subset_value,
      output_dir = output_dir, firms97 = firms97,
      run_ol = FALSE, run_pl = FALSE, run_borda = TRUE, run_ols = TRUE, run_ols_centered = FALSE,
      combine_valences = TRUE, valence_triples = valence_triples, industry_means = TRUE,
      seed = 123
    )
  })

  message("🎃 Subset write check: ", runs$output_stub[i])
  for (sheet in analysis_check_sheets) {
    check_path <- parquet_sheet_path(file.path(intermediate, runs$output_stub[i]), sheet)
    check_info <- file.info(check_path)
    message("  ", basename(check_path), " | exists=", file.exists(check_path),
            " | size=", check_info$size,
            " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S"))
  }
}

# message("Running Revelio EIV outputs")
# revelio_eiv_by_subdir <- run_revelio_eiv_for_subdirs(default_revelio_eiv_filemap$subdir)
#
# message("Revelio EIV write check:")
# for (subdir in default_revelio_eiv_filemap$subdir) {
#   check_path <- parquet_sheet_path(file.path(intermediate, subdir), "EIV_revelio_firm")
#   check_info <- file.info(check_path)
#   message("  ", subdir, "/", basename(check_path),
#           " | exists=", file.exists(check_path),
#           " | size=", check_info$size,
#           " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S"))
# }

message("Running EEO-1 industry-share EIV outputs")
eeo1_eiv_by_subdir <- run_eeo1_eiv_for_subdirs(default_eeo1_eiv_filemap$subdir)

message("EEO-1 industry-share EIV write check:")
for (subdir in default_eeo1_eiv_filemap$subdir) {
  check_path <- parquet_sheet_path(file.path(intermediate, subdir), eeo1_eiv_sheet)
  check_info <- file.info(check_path)
  message("  ", subdir, "/", basename(check_path),
          " | exists=", file.exists(check_path),
          " | size=", check_info$size,
          " | mtime=", format(check_info$mtime, "%Y-%m-%d %H:%M:%S"))
}

# ------------------------------------------------------------------------------
# NAICS3 beliefs and EEO-1 workforce shares
# ------------------------------------------------------------------------------

source(file.path(analysis, "regress_beliefs_on_eeo1_naics3_shares.R"))
