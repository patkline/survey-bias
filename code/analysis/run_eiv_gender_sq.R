# ------------------------------------------------------------------------------
# Purpose: Run EIV for log_dif_gender_sq ~ discretion and
#          log_dif_gender_sq ~ FirmSelective (OLS + Borda, full sample).
#          Saves results to a standalone xlsx to avoid slow loadWorkbook.
#
# Created: 2026-03-24
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

# --- Read analysis-ready data ---
file_path <- file.path(processed, "long_survey_final.csv")
message("Reading analysis data...")
data <- read.csv(file_path, stringsAsFactors = FALSE)

survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older",
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white", "pooled_favor_male",
                 "pooled_white", "pooled_black",
                 "pooled_male", "pooled_female")

valence_triples <- list(
  list(valence1 = "conduct_black",   valence2 = "conduct_white",   new_outcome = "conduct_favor_white_ep"),
  list(valence1 = "FirmCont_black",  valence2 = "FirmCont_white",  new_outcome = "FirmCont_favor_white_ep"),
  list(valence1 = "FirmHire_black",  valence2 = "FirmHire_white",  new_outcome = "FirmHire_favor_white_ep"),
  list(valence1 = "pooled_black",    valence2 = "pooled_white",    new_outcome = "pooled_favor_white_ep"),
  list(valence1 = "conduct_female",  valence2 = "conduct_male",    new_outcome = "conduct_favor_male_ep"),
  list(valence1 = "FirmCont_female", valence2 = "FirmCont_male",   new_outcome = "FirmCont_favor_male_ep"),
  list(valence1 = "FirmHire_female", valence2 = "FirmHire_male",   new_outcome = "FirmHire_favor_male_ep"),
  list(valence1 = "pooled_female",   valence2 = "pooled_male",     new_outcome = "pooled_favor_male_ep"),
  list(valence1 = "conduct_older",   valence2 = "conduct_younger", new_outcome = "conduct_favor_younger_ep")
)

new_outcomes <- vapply(valence_triples, `[[`, character(1), "new_outcome")
variables <- c(survey_vars, new_outcomes)

output_path <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")
stopifnot(file.exists(output_path))

# --- Industry map ---
industry_map <- data %>%
  dplyr::select(firm_id, aer_naics2) %>%
  dplyr::distinct() %>%
  dplyr::mutate(firm_id = as.integer(firm_id))

# --- Read existing sheets with read_xlsx (NOT loadWorkbook) ---
message("Reading existing sheets...")
variance_df   <- readxl::read_xlsx(output_path, sheet = "variance")
message("  variance done")
covariance_df <- readxl::read_xlsx(output_path, sheet = "covariance")
message("  covariance done")
coef_long_df  <- readxl::read_xlsx(output_path, sheet = "Coefficients")
message("  coefficients done")

# --- Build noise matrices (OLS and Borda only) ---
models_to_build <- c("Borda", "OLS")
noise_mats_97 <- setNames(vector("list", length(models_to_build)), models_to_build)

for (m in models_to_build) {
  noise_mats_97[[m]] <- tryCatch(
    build_noise_matrix(
      variance_df   = variance_df,
      covariance_df = covariance_df,
      outcomes      = variables,
      subset_value  = "subset97",
      model_value   = m
    ),
    error = function(e) { message("Skipping model ", m, ": ", e$message); NULL }
  )
}
message("Noise matrices built")

# --- Build firm-level wide dataframe ---
coef_long_97 <- coef_long_df %>% dplyr::filter(subset == "subset97")
coef_firm <- coef_long_97 %>% dplyr::filter(entity_type == "Firm")

to_wide_by_outcome <- function(df_long) {
  df_wide <- df_long %>%
    dplyr::filter(model != "EXPERIMENTAL") %>%
    dplyr::select(model, entity_id, entity, outcome, estimate, njobs) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = c(model, entity_id, entity, njobs),
      names_from = outcome,
      values_from = estimate
    )

  experimental <- df_long %>%
    dplyr::filter(model == "EXPERIMENTAL") %>%
    dplyr::select(entity_id, entity, outcome, estimate) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = c(entity_id, entity),
      names_from = outcome,
      values_from = estimate
    )

  dplyr::left_join(df_wide, experimental, by = "entity_id")
}

coef_firm_wide <- to_wide_by_outcome(coef_firm) %>%
  dplyr::left_join(industry_map %>% dplyr::rename(entity_id = firm_id), by = "entity_id")

# --- Verify log_dif_gender_sq exists ---
stopifnot("log_dif_gender_sq" %in% names(coef_firm_wide))
message("coef_firm_wide built, log_dif_gender_sq confirmed present")

# --- Define the two new specs ---
regs_new <- list(
  list(lhs = "log_dif_gender_sq", rhs = c("FirmSelective")),
  list(lhs = "log_dif_gender_sq", rhs = c("discretion"))
)

# --- Run EIV ---
message("Running EIV for log_dif_gender_sq ~ FirmSelective and log_dif_gender_sq ~ discretion")
eiv_new <- run_eiv_suite(
  regs         = regs_new,
  coef_df_wide = coef_firm_wide,
  noise_mats_97 = noise_mats_97,
  models       = names(Filter(Negate(is.null), noise_mats_97)),
  id_col       = "entity_id",
  model_col    = "model",
  fe_col       = "aer_naics2",
  weights_col  = "njobs",
  use_fe       = TRUE
)

# --- Save to standalone file ---
standalone_path <- file.path(excel, "EIV_gender_sq_results.xlsx")
wb_new <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_new, "EIV_firm_gender_sq")
openxlsx::writeData(wb_new, "EIV_firm_gender_sq", eiv_new)
openxlsx::saveWorkbook(wb_new, standalone_path, overwrite = TRUE)

message("Done. Saved ", nrow(eiv_new), " rows to: ", standalone_path)
print(eiv_new)
