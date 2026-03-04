# Test script for new OLS, OL EIV, OLS EIV blocks
# Runs full sample + all 18 subsamples
source("code/globals.R")
source(file.path(analysis, "load_all.R"))
source(file.path(analysis, "analysis_pipeline.R"))

file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older",
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white","pooled_favor_male")

experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

firms97 <- data %>% dplyr::filter(!is.na(dif)) %>% dplyr::select(firm_id) %>% dplyr::distinct() %>% dplyr::pull(firm_id)
industry_map_path <- file.path(processed,"industry_map.xlsx")

# ---- Full sample ----
output_path <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")

cat("\n=== Running: Full Sample ===\n")
system.time({
  run_analysis_pipeline(data, respondent_col, firm_col, survey_vars, experimental_vars,
                        subset_var = NULL, subset_value = NULL,
                        firms97 = firms97,
                        output_path = output_path,
                        industry_map_path = industry_map_path,
                        generate_wide = TRUE,
                        ordered_logit = FALSE,
                        process_outcomes = FALSE,
                        run_bootstrap = FALSE,
                        run_bs_eiv = FALSE,
                        eiv_summary = FALSE,
                        eiv_bivariate = FALSE,
                        run_pairwise_process = FALSE,
                        run_pairwise_process_ol = FALSE,
                        borda_score = FALSE,
                        borda_bs_w = FALSE,
                        run_borda_eiv = FALSE,
                        borda_eiv_summary = FALSE,
                        run_pairwise_process_borda = FALSE,
                        borda_eiv_bivariate = FALSE,
                        sum_signal_noise = FALSE,
                        run_ols = TRUE,
                        run_ol_eiv = TRUE,
                        run_ols_eiv = TRUE,
                        sim_pl_to_borda = FALSE,
                        exact_pl_to_borda = FALSE,
                        diagnostic = FALSE,
                        B = 1)
})

# ---- Subset runs ----
runs <- tibble::tribble(
  ~subset_var,   ~subset_value, ~output_stub,
  "confidence_race", 1,         "Plackett_Luce_Subset_Conf_Race_Y",
  "confidence_race", 0,         "Plackett_Luce_Subset_Conf_Race_N",
  "confidence_gend", 1,         "Plackett_Luce_Subset_Conf_Gender_Y",
  "confidence_gend", 0,         "Plackett_Luce_Subset_Conf_Gender_N",
  "sample",          1,         "Plackett_Luce_Subset_Probability",
  "sample",          0,         "Plackett_Luce_Subset_Convenience",
  "gender",          1,         "Plackett_Luce_Subset_Female",
  "gender",          0,         "Plackett_Luce_Subset_Male",
  "race",            1,         "Plackett_Luce_Subset_Black",
  "race",            0,         "Plackett_Luce_Subset_White",
  "age",             1,         "Plackett_Luce_Subset_Age_gte40",
  "age",             0,         "Plackett_Luce_Subset_Age_lt40",
  "looking_job",     1,         "Plackett_Luce_Subset_Looking",
  "looking_job",     0,         "Plackett_Luce_Subset_Not_Looking",
  "fear",            1,         "Plackett_Luce_Subset_Feared_Discrimination_1",
  "fear",            0,         "Plackett_Luce_Subset_Feared_Discrimination_0",
  "educ",            1,         "Plackett_Luce_Subset_College",
  "educ",            0,         "Plackett_Luce_Subset_No_College"
)

results <- vector("list", nrow(runs))

for (i in seq_len(nrow(runs))) {
  sv <- runs$subset_var[i]
  sval <- runs$subset_value[i]
  output_path <- file.path(excel, paste0(runs$output_stub[i], ".xlsx"))

  cat("\n=== Running:", runs$output_stub[i],
      "| subset_var =", sv,
      "| subset_value =", sval,
      "===\n")

  results[[i]] <- system.time({
    run_analysis_pipeline(data, respondent_col, firm_col, survey_vars, experimental_vars,
                          subset_var = sv, subset_value = sval,
                          firms97 = firms97,
                          output_path = output_path,
                          industry_map_path = industry_map_path,
                          generate_wide = TRUE,
                          ordered_logit = TRUE,
                          process_outcomes = FALSE,
                          run_bootstrap = FALSE,
                          run_bs_eiv = FALSE,
                          eiv_summary = FALSE,
                          eiv_bivariate = FALSE,
                          run_pairwise_process = FALSE,
                          run_pairwise_process_ol = FALSE,
                          borda_score = FALSE,
                          borda_bs_w = FALSE,
                          run_borda_eiv = FALSE,
                          borda_eiv_summary = FALSE,
                          run_pairwise_process_borda = FALSE,
                          borda_eiv_bivariate = FALSE,
                          sum_signal_noise = FALSE,
                          run_ols = TRUE,
                          run_ol_eiv = TRUE,
                          run_ols_eiv = TRUE,
                          sim_pl_to_borda = FALSE,
                          exact_pl_to_borda = FALSE,
                          diagnostic = FALSE,
                          B = 1)
  })
}

cat("\n=== All runs complete! ===\n")
