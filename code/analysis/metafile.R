# ------------------------------------------------------------------------------
# Purpose: Metafile for analysis
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

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
survey_vars <- c("FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
                 "FirmCont_favor_male", "FirmHire_favor_male", "conduct_favor_male",
                 "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white","pooled_favor_male")
experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

outcome_types <- list(
  "FirmCont_favor_white" = "Non-Experimental","FirmHire_favor_white" = "Non-Experimental",
  "conduct_favor_white" = "Non-Experimental","FirmCont_favor_male" = "Non-Experimental",
  "FirmHire_favor_male" = "Non-Experimental","conduct_favor_male" = "Non-Experimental",
  "conduct_black" = "Non-Experimental","conduct_white" = "Non-Experimental",
  "conduct_male" = "Non-Experimental","conduct_female" = "Non-Experimental",
  "FirmCont_black" = "Non-Experimental","FirmCont_white" = "Non-Experimental",
  "FirmCont_male" = "Non-Experimental","FirmCont_female" = "Non-Experimental",
  "FirmHire_white" = "Non-Experimental","FirmHire_black" = "Non-Experimental",
  "FirmHire_male" = "Non-Experimental","FirmHire_female" = "Non-Experimental",
  "conduct_favor_younger" = "Non-Experimental","discretion" = "Non-Experimental",
  "FirmSelective" = "Non-Experimental","FirmDesire" = "Non-Experimental",
  "pooled_favor_white" = "Non-Experimental","pooled_favor_male" = "Non-Experimental",
  "dif" = "Experimental","log_dif" = "Experimental","dif_gender" = "Experimental",
  "log_dif_gender" = "Experimental","dif_age" = "Experimental","log_dif_age" = "Experimental"
)

firms97 <- data %>% filter(!is.na(dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)
industry_map_path <- file.path(processed,"industry_map.xlsx")

subset_var <- NULL
subset_value <- NULL
output_path <- file.path(excel,"Plackett_Luce_Full_Sample.xlsx")

# Function Call Female
system.time({
  run_analysis_pipeline(data, respondent_col, firm_col, survey_vars, experimental_vars,
                        outcome_types, subset_var = subset_var, subset_value = subset_value,
                        firms97 = firms97,
                        output_path = output_path,
                        industry_map_path = industry_map_path,
                        generate_wide = TRUE,
                        process_outcomes = TRUE,
                        run_bootstrap = TRUE,
                        run_bs_eiv = TRUE,
                        eiv_summary = TRUE,
                        run_pairwise_process = TRUE,
                        borda_score = TRUE,
                        borda_bs_w = TRUE,
                        run_borda_eiv = TRUE,
                        borda_eiv_summary = TRUE,
                        run_pairwise_process_borda = TRUE,
                        sum_signal_noise = TRUE,
                        sim_pl_to_borda = FALSE,
                        exact_pl_to_borda = FALSE,
                        diagnostic = FALSE,
                        B = 1)
})


#---- 1) Define the subset runs (mirrors your bash VARS/VALS/OUTS) ----
runs <- tibble::tribble(
  ~subset_var,   ~subset_value, ~output_stub,
  "gender",      1,             "Plackett_Luce_Subset_Female",
  "gender",      0,             "Plackett_Luce_Subset_Male",
  "race",        1,             "Plackett_Luce_Subset_Black",
  "race",        0,             "Plackett_Luce_Subset_White",
  "age",         1,             "Plackett_Luce_Subset_Age_gte40",
  "age",         0,             "Plackett_Luce_Subset_Age_lt40",
  "looking_job", 1,             "Plackett_Luce_Subset_Looking",
  "looking_job", 0,             "Plackett_Luce_Subset_Not_Looking",
  "fear",        1,             "Plackett_Luce_Subset_Feared_Discrimination_1",
  "fear",        0,             "Plackett_Luce_Subset_Feared_Discrimination_0",
  "educ",        1,             "Plackett_Luce_Subset_College",
  "educ",        0,             "Plackett_Luce_Subset_No_College"
)


# ---- 3) Run them all ----
results <- vector("list", nrow(runs))

for (i in seq_len(nrow(runs))) {
  subset_var   <- runs$subset_var[i]
  subset_value <- runs$subset_value[i]

  output_path <- file.path(excel, paste0(runs$output_stub[i], ".xlsx"))

  cat("\n=== Running:", runs$output_stub[i],
      "| subset_var =", subset_var,
      "| subset_value =", subset_value,
      "===\n")

  results[[i]] <- system.time({
    run_analysis_pipeline(
      data, respondent_col, firm_col, survey_vars, experimental_vars,
      outcome_types,
      subset_var = subset_var,
      subset_value = subset_value,
      firms97 = firms97,
      output_path = output_path,
      industry_map_path = industry_map_path,
      generate_wide = TRUE,
      process_outcomes = TRUE,
      run_bootstrap = TRUE,
      run_bs_eiv = TRUE,
      eiv_summary = TRUE,
      run_pairwise_process = TRUE,
      borda_score = TRUE,
      borda_bs_w = TRUE,
      run_borda_eiv = TRUE,
      borda_eiv_summary = TRUE,
      run_pairwise_process_borda = TRUE,
      sum_signal_noise = TRUE,
      sim_pl_to_borda = FALSE,
      exact_pl_to_borda = FALSE,
      diagnostic = FALSE,
      B = 1
    )
  })
}


