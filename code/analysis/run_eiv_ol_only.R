# ------------------------------------------------------------------------------
# Purpose: Run EIV for OL model only, appending to existing EIV sheets.
#          For Full Sample + all subsamples.
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

# --- Shared config ---
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

industry_map <- openxlsx::read.xlsx(industry_map_path, sheet = 1) %>%
  dplyr::select(firm_id, aer_naics2) %>%
  dplyr::mutate(firm_id = as.integer(firm_id))

# Weights from data
file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

weights <- data %>%
  dplyr::select(firm_id, njobs) %>%
  dplyr::distinct() %>%
  dplyr::rename(weights = njobs)

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

models_to_build <- c("OL")

# --- Files to process (Full Sample + all subsamples) ---
all_files <- c(
  "Plackett_Luce_Full_Sample.xlsx",
  "Plackett_Luce_Subset_Black.xlsx",
  "Plackett_Luce_Subset_White.xlsx",
  "Plackett_Luce_Subset_Female.xlsx",
  "Plackett_Luce_Subset_Male.xlsx",
  "Plackett_Luce_Subset_Looking.xlsx",
  "Plackett_Luce_Subset_Not_Looking.xlsx",
  "Plackett_Luce_Subset_Feared_Discrimination_1.xlsx",
  "Plackett_Luce_Subset_Feared_Discrimination_0.xlsx",
  "Plackett_Luce_Subset_Age_gte40.xlsx",
  "Plackett_Luce_Subset_Age_lt40.xlsx",
  "Plackett_Luce_Subset_College.xlsx",
  "Plackett_Luce_Subset_No_College.xlsx",
  "Plackett_Luce_Subset_Conf_Race_Y.xlsx",
  "Plackett_Luce_Subset_Conf_Race_N.xlsx",
  "Plackett_Luce_Subset_Conf_Gender_Y.xlsx",
  "Plackett_Luce_Subset_Conf_Gender_N.xlsx",
  "Plackett_Luce_Subset_Probability.xlsx",
  "Plackett_Luce_Subset_Convenience.xlsx"
)

# --- Helper: run OL EIV for one workbook, appending to existing EIV sheet ---
run_ol_eiv_for_file <- function(filename) {
  output_path <- file.path(excel, filename)

  if (!file.exists(output_path)) {
    message("⚠ File not found, skipping: ", filename)
    return(invisible(NULL))
  }

  message("\n========================================")
  message("Processing OL for: ", filename)
  message("========================================")

  wb <- openxlsx::loadWorkbook(output_path)

  variance_df   <- tryCatch(openxlsx::read.xlsx(output_path, sheet = "variance"),
                             error = function(e) { message("  No variance sheet, skipping."); return(NULL) })
  covariance_df <- tryCatch(openxlsx::read.xlsx(output_path, sheet = "covariance"),
                             error = function(e) { message("  No covariance sheet, skipping."); return(NULL) })
  coef97_df     <- tryCatch(openxlsx::read.xlsx(output_path, sheet = "Coefficients (97)"),
                             error = function(e) { message("  No Coefficients (97) sheet, skipping."); return(NULL) })

  if (is.null(variance_df) || is.null(covariance_df) || is.null(coef97_df)) {
    return(invisible(NULL))
  }

  # Build noise matrix for OL only
  noise_mats_97 <- list()
  noise_mats_97[["OL"]] <- build_noise_matrix(
    variance_df   = variance_df,
    covariance_df = covariance_df,
    outcomes      = survey_vars,
    subset_value  = "subset97",
    model_value   = "OL"
  )

  # Read existing EIV data to preserve it
  existing_eiv <- tryCatch(openxlsx::read.xlsx(output_path, sheet = "EIV"),
                           error = function(e) NULL)

  # Run EIV for OL only (get data frame back)
  ol_eiv_df <- run_eiv_suite(
    regs          = regs_all,
    coef97_df     = coef97_df,
    industry_map  = industry_map,
    noise_mats_97 = noise_mats_97,
    models        = "OL",
    weights_df    = weights,
    weights_col   = "weights"
  )

  # Merge: append OL rows to existing EIV sheet
  if (!is.null(existing_eiv)) {
    combined <- dplyr::bind_rows(existing_eiv, ol_eiv_df)
  } else {
    combined <- ol_eiv_df
  }

  remove_sheet_safely(wb, "EIV")
  openxlsx::addWorksheet(wb, "EIV")
  openxlsx::writeData(wb, "EIV", combined)

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ OL EIV appended to: ", filename)
}

# --- Run all ---
total_time <- system.time({
  for (f in all_files) {
    tryCatch(
      run_ol_eiv_for_file(f),
      error = function(e) message("❌ Error processing ", f, ": ", conditionMessage(e))
    )
  }
})

message("\n========================================")
message("OL EIV complete for all files!")
message("Total elapsed time: ", round(total_time["elapsed"] / 60, 1), " minutes")
message("========================================")
