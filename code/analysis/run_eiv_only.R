# ------------------------------------------------------------------------------
# Purpose: Run just Step 5 (EIV) of the pipeline using existing workbook data.
#          This avoids re-running Steps 1-4 which are already complete.
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

# --- Config ---
output_path     <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")
industry_map_path <- file.path(processed, "industry_map.xlsx")

survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older",
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white", "pooled_favor_male")

# --- Read existing data from workbook ---
wb <- openxlsx::loadWorkbook(output_path)

variance_df   <- openxlsx::read.xlsx(output_path, sheet = "variance")
covariance_df <- openxlsx::read.xlsx(output_path, sheet = "covariance")

industry_map <- openxlsx::read.xlsx(industry_map_path, sheet = 1) %>%
  dplyr::select(firm_id, aer_naics2) %>%
  dplyr::mutate(firm_id = as.integer(firm_id))

# --- Weights from data ---
file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

weights <- data %>%
  dplyr::select(firm_id, njobs) %>%
  dplyr::distinct() %>%
  dplyr::rename(weights = njobs)

# --- Build noise matrices for all models ---
models_to_build <- c("PL", "Borda", "OLS", "OLSC")

message("Building noise matrices for: ", paste(models_to_build, collapse = ", "))
noise_mats_97 <- setNames(vector("list", length(models_to_build)), models_to_build)

for (m in models_to_build) {
  noise_mats_97[[m]] <- build_noise_matrix(
    variance_df   = variance_df,
    covariance_df = covariance_df,
    outcomes      = survey_vars,
    subset_value  = "subset97",
    model_value   = m
  )
}

# --- EIV specifications ---
regs_uni <- list(
  list(lhs = "cb_central_full",   rhs = c("discretion")),
  list(lhs = "log_dif",           rhs = c("FirmCont_favor_white")),
  list(lhs = "log_dif",           rhs = c("conduct_favor_white")),
  list(lhs = "log_dif",           rhs = c("pooled_favor_white")),
  list(lhs = "log_dif",           rhs = c("FirmSelective")),
  list(lhs = "log_dif",           rhs = c("discretion")),
  list(lhs = "log_dif_gender",    rhs = c("FirmCont_favor_male")),
  list(lhs = "log_dif_gender",    rhs = c("conduct_favor_male")),
  list(lhs = "log_dif_gender",    rhs = c("pooled_favor_male")),
  list(lhs = "log_dif_gender",    rhs = c("FirmSelective")),
  list(lhs = "log_dif_gender",    rhs = c("discretion")),
  list(lhs = "log_dif_gender_sq", rhs = c("FirmCont_favor_male")),
  list(lhs = "log_dif_gender_sq", rhs = c("conduct_favor_male")),
  list(lhs = "log_dif_gender_sq", rhs = c("pooled_favor_male")),
  list(lhs = "log_dif_gender_sq", rhs = c("FirmSelective")),
  list(lhs = "log_dif_gender_sq", rhs = c("discretion")),
  list(lhs = "log_dif_age",       rhs = c("conduct_favor_younger")),
  list(lhs = "log_dif_age",       rhs = c("FirmSelective")),
  list(lhs = "log_dif_age",       rhs = c("discretion"))
)

regs_bi <- list(
  list(lhs = "log_dif",        rhs = c("FirmSelective", "discretion")),
  list(lhs = "log_dif_gender", rhs = c("FirmSelective", "discretion")),
  list(lhs = "log_dif_age",    rhs = c("FirmSelective", "discretion"))
)

regs_all <- c(regs_uni, regs_bi)

# --- Read Coefficients (97) ---
coef97_df <- openxlsx::read.xlsx(output_path, sheet = "Coefficients (97)")

models_to_run_eiv <- intersect(models_to_build, names(noise_mats_97))
message("Running EIV for models: ", paste(models_to_run_eiv, collapse = ", "))

# --- Run EIV ---
eiv_df <- write_eiv_sheet(
  wb            = wb,
  sheet_name    = "EIV",
  regs          = regs_all,
  coef97_df     = coef97_df,
  industry_map  = industry_map,
  noise_mats_97 = noise_mats_97,
  models        = models_to_run_eiv,
  weights_df    = weights,
  weights_col   = "weights"
)

# --- Save ---
openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
message("✅ EIV sheet written to: ", output_path)
