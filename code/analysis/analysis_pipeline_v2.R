
# ------------------------------------------------------------------------------
# PIPELINE
# ------------------------------------------------------------------------------
run_analysis_pipeline_v2 <- function(
    data, respondent_col, survey_vars, experimental_vars = NULL,
    output_path, industry_map_path,
    run_ol = TRUE, run_pl = TRUE, run_borda = TRUE,
    firms97 = NULL,
    seed = 123
) {
  
  
################################################################################
## Step 0: Preliminaries
################################################################################
  set.seed(seed)
  prefix_map <- list(OL = "ol", PL = "pl", Borda = "b")
  
  # load workbook
  wb <- if (file.exists(output_path)) openxlsx::loadWorkbook(output_path) else openxlsx::createWorkbook()
  
  # (still read industry_map here if other functions expect it in scope)
  industry_map <- openxlsx::read.xlsx(industry_map_path, sheet = 1) %>%
    dplyr::select(firm_id, aer_naics2) %>%
    dplyr::mutate(firm_id = as.integer(firm_id))
  
  # Prepare Outcomes for Analysis
  prep <- prep_outcomes(data, survey_vars)
  data_wide_list <- prep$wide
  data_long_list <- prep$long
  id_map_list    <- prep$id_map
  
  
################################################################################
## Step 1: Run Models
################################################################################ 
  results <- run_models(
    wb = wb,
    output_path = output_path,
    survey_vars = survey_vars,
    respondent_col = respondent_col,
    data_wide_list = data_wide_list,
    data_long_list = data_long_list,
    id_map_list    = id_map_list,
    experimental_vars = experimental_vars,
    data_for_experimental = data,   # only if your subset97 writer needs it
    run_ol = run_ol,
    run_pl = run_pl,
    run_borda = run_borda,
    firms97 = firms97,
    seed = seed,
    build_subset97 = TRUE
  )
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 1 Complete. Saved: ", output_path)
  
################################################################################
## Step 2: Variance, Noise, Signal Variance
################################################################################
  ## See functions in "variance_functions.R"
  message("Variance Denoising")
  variance_df <- write_variance_sheet(results, wb, sheet_name = "variance")
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 2 Complete. Saved: ", output_path)
  
################################################################################
## Step 3: Covariance, Noise, Signal Covariance
################################################################################  
  # Step 3: Covariance + noise12
  message("Covariance Denoising")
  covariance_df <- write_covariance_sheet(results, wb, sheet_name = "covariance")
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 3 Complete. Saved: ", output_path)

################################################################################
## Step 4: Correlation
################################################################################
  message("Correlation Calculations")
  corr_df <- build_correlation_from_varcov(variance_df, covariance_df)

  remove_sheet_safely(wb, "correlation")
  openxlsx::addWorksheet(wb, "correlation")
  openxlsx::writeData(wb, "correlation", corr_df)
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 4 Complete. Saved: ", output_path)
  
################################################################################
## Step 5: EIV
################################################################################
  message("Building Noise Matrices for EIV")
  
  # which models were run?
  models_to_build <- character(0)
  if (isTRUE(run_pl))    models_to_build <- c(models_to_build, "PL")
  if (isTRUE(run_borda)) models_to_build <- c(models_to_build, "Borda")
  if (isTRUE(run_ol))    models_to_build <- c(models_to_build, "OL")
  
  # build one noise matrix per model (subset97)
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
  
  # Optional convenience objects if you still want them:
  N97_PL    <- if ("PL"    %in% names(noise_mats_97)) noise_mats_97[["PL"]]    else NULL
  N97_Borda <- if ("Borda" %in% names(noise_mats_97)) noise_mats_97[["Borda"]] else NULL
  N97_OL    <- if ("OL"    %in% names(noise_mats_97)) noise_mats_97[["OL"]]    else NULL
  
  # List of Specifications
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
  
  message("Running EIV")

  # weights (optional)
  weights <- data %>%
    dplyr::select(firm_id, njobs) %>%
    dplyr::distinct() %>%
    dplyr::rename(weights = njobs)
  
  coef97_df <- openxlsx::read.xlsx(output_path, sheet = "Coefficients (97)")
  
  models_to_run_eiv <- intersect(c("PL", "Borda", "OL"), names(noise_mats_97))
  
  eiv_df <- write_eiv_sheet(
    wb            = wb,
    sheet_name    = "EIV",
    regs          = regs_all,
    coef97_df     = coef97_df,
    industry_map  = industry_map,
    noise_mats_97 = noise_mats_97,
    models        = models_to_run_eiv,
    weights_df    = weights,      # set to NULL for equal weights
    weights_col   = "weights"
  )
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 5 Complete. Saved: ", output_path)

}




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
survey_vars <- c("FirmCont_favor_white", "FirmCont_black", "FirmCont_white", 
                 "FirmHire_favor_white", "FirmHire_black", "FirmHire_white",
                 "conduct_favor_white", "conduct_black", "conduct_white",
                 "FirmCont_favor_male", "FirmCont_male", "FirmCont_female", 
                 "FirmHire_favor_male", "FirmHire_male", "FirmHire_female", 
                 "conduct_favor_male", "conduct_male", "conduct_female",
                 "conduct_favor_younger", "conduct_younger", "conduct_older", 
                 "discretion", "FirmSelective", "FirmDesire",
                 "pooled_favor_white","pooled_favor_male")

experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "log_dif_gender_sq", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

firms97 <- data %>% dplyr::filter(!is.na(dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)
industry_map_path <- file.path(processed,"industry_map.xlsx")

subset_var <- NULL
subset_value <- NULL
output_path <- file.path(excel,"Plackett_Luce_Full_Sample_test.xlsx")

run_analysis_pipeline_v2(
    data, respondent_col, survey_vars, experimental_vars,
    output_path, industry_map_path,
    run_ol = TRUE, run_pl = TRUE, run_borda = TRUE,
    firms97 = firms97,
    seed = 123
) 