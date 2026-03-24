# ------------------------------------------------------------------------------
# Purpose: Run Step 5 (EIV) for ALL subsample workbooks using existing data.
#          Mirrors run_eiv_only.R but loops over every subsample file.
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "load_all.R"))

# --- Shared config ---
survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white",
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female",
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female",
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older",
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white", "pooled_favor_male")

# Weights from data and industry map
file_path <- file.path(processed, "long_survey_final.csv")
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Map from firms to industries using aer_naics2 already merged into analysis data
industry_map <- data %>%
  dplyr::select(firm_id, aer_naics2) %>%
  dplyr::distinct() %>%
  dplyr::mutate(firm_id = as.integer(firm_id))

# Assert firm_id and aer_naics2 variables exist in analysis data
stopifnot(all(c("firm_id", "aer_naics2") %in% names(data)))

# Assert there is one unique aer_naics2 value per firm_id in industry map
stopifnot(nrow(industry_map) == dplyr::n_distinct(industry_map$firm_id))

# Assert aer_naics2 is non-missing for all firm_id values in industry map
stopifnot(sum(is.na(industry_map$aer_naics2)) == 0)

weights <- data %>%
  dplyr::select(firm_id, njobs) %>%
  dplyr::distinct() %>%
  dplyr::rename(weights = njobs)

# --- EIV specifications (same as pipeline Step 5) ---
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

models_to_build <- c("PL", "Borda", "OL", "OLS", "OLSC")

# --- Subsample files to process ---
subsample_files <- c(
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

# --- Helper: run EIV for one workbook ---
run_eiv_for_file <- function(filename) {
  output_path <- file.path(excel, filename)

  if (!file.exists(output_path)) {
    message("⚠ File not found, skipping: ", filename)
    return(invisible(NULL))
  }

  message("\n========================================")
  message("Processing: ", filename)
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

  # Build noise matrices
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

  models_to_run_eiv <- intersect(models_to_build, names(noise_mats_97))
  message("  Running EIV for models: ", paste(models_to_run_eiv, collapse = ", "))

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

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ EIV sheet written to: ", filename)
}

# --- Run all ---
total_time <- system.time({
  for (f in subsample_files) {
    tryCatch(
      run_eiv_for_file(f),
      error = function(e) message("❌ Error processing ", f, ": ", conditionMessage(e))
    )
  }
})

message("\n========================================")
message("All subsamples complete!")
message("Total elapsed time: ", round(total_time["elapsed"] / 60, 1), " minutes")
message("========================================")
