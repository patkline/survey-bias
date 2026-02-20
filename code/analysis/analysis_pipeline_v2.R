run_analysis_pipeline_v2 <- function(
    data, respondent_col, survey_vars,
    output_path, industry_map_path,
    run_ol = TRUE, run_pl = TRUE, run_borda = TRUE,
    firms97 = NULL,
    seed = 123
) {
  set.seed(seed)
  
  # map, workbook
  industry_map <- openxlsx::read.xlsx(industry_map_path, sheet = 1) %>%
    dplyr::select(firm_id, aer_naics2) %>%
    dplyr::mutate(firm_id = as.integer(firm_id))
  
  wb <- if (file.exists(output_path)) openxlsx::loadWorkbook(output_path) else openxlsx::createWorkbook()
  
  # prep once
  prep <- prep_outcomes(data, survey_vars)
  data_wide_list <- prep$wide
  data_long_list <- prep$long
  id_map_list    <- prep$id_map
  
  # store in R
  results <- list(OL = list(), PL = list(), Borda = list())
  
  # stacked master tables (long)
  coef_long <- list()
  se_long   <- list()
  rse_long  <- list()
  eb_long   <- list()
  
  add_flags <- function(df, is_borda, is_pl, is_ol) {
    df %>%
      dplyr::mutate(
        Borda = is_borda,
        PL    = is_pl,
        OL    = is_ol
      )
  }
  
  for (outcome in survey_vars) {
    cat("Outcome:", outcome, "\n")
    
    id_map <- id_map_list[[outcome]]
    d_wide <- data_wide_list[[outcome]]
    d_long <- data_long_list[[outcome]]
    
    # ---------- OL ----------
    if (isTRUE(run_ol)) {
      ol_res <- run_model_ol(d_long, outcome, respondent_col, id_map)
      results$OL[[outcome]] <- ol_res
      
      ft <- ol_res$firm_table %>% dplyr::mutate(outcome = outcome)
      coef_long[[length(coef_long)+1]] <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = estimate), FALSE, FALSE, TRUE)
      se_long[[length(se_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = se),       FALSE, FALSE, TRUE)
      rse_long[[length(rse_long)+1]]   <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = rse),      FALSE, FALSE, TRUE)
      eb_long[[length(eb_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = eb),       FALSE, FALSE, TRUE)
      
      write_matrix_sheet(wb, paste0("S_ol_", outcome),    ol_res$mats$S)
      write_matrix_sheet(wb, paste0("Binv_ol_", outcome), ol_res$mats$bread)
      write_matrix_sheet(wb, paste0("cov_ol_", outcome),  ol_res$mats$cov)
      write_matrix_sheet(wb, paste0("rcov_ol_", outcome), ol_res$mats$rcov)
    }
    
    # ---------- PL ----------
    if (isTRUE(run_pl)) {
      pl_res <- run_model_pl(d_wide, id_map, outcome, firms97 = firms97)
      results$PL[[outcome]] <- pl_res
      
      ft <- pl_res$firm_table %>% dplyr::mutate(outcome = outcome)
      coef_long[[length(coef_long)+1]] <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = estimate), FALSE, TRUE, FALSE)
      se_long[[length(se_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = se),       FALSE, TRUE, FALSE)
      rse_long[[length(rse_long)+1]]   <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = rse),      FALSE, TRUE, FALSE)
      eb_long[[length(eb_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = eb),       FALSE, TRUE, FALSE)
      
      write_matrix_sheet(wb, paste0("S_pl_", outcome),    pl_res$mats$S)
      write_matrix_sheet(wb, paste0("Binv_pl_", outcome), pl_res$mats$bread)
      write_matrix_sheet(wb, paste0("cov_pl_", outcome),  pl_res$mats$cov)
      write_matrix_sheet(wb, paste0("rcov_pl_", outcome), pl_res$mats$rcov)
    }
    
    # ---------- Borda ----------
    if (isTRUE(run_borda)) {
      borda_res <- run_model_borda(
        data_wide        = d_wide,
        id_map           = id_map,
        outcome          = outcome,
        higher_is_better = FALSE,
        normalize        = TRUE,
        ref_firm_ids     = c(38, 76, 90),
        do_eb            = TRUE
      )
      results$Borda[[outcome]] <- borda_res
      
      ft <- borda_res$firm_table %>% dplyr::mutate(outcome = outcome)
      coef_long[[length(coef_long)+1]] <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = estimate), TRUE, FALSE, FALSE)
      se_long[[length(se_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = se),       TRUE, FALSE, FALSE)
      rse_long[[length(rse_long)+1]]   <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = rse),      TRUE, FALSE, FALSE)
      eb_long[[length(eb_long)+1]]     <- add_flags(ft %>% dplyr::transmute(firm_id, firm, outcome, value = eb),       TRUE, FALSE, FALSE)
      
      write_matrix_sheet(wb, paste0("S_b_", outcome),    borda_res$mats$S)
      write_matrix_sheet(wb, paste0("Binv_b_", outcome), borda_res$mats$bread)
      write_matrix_sheet(wb, paste0("cov_b_", outcome),  borda_res$mats$cov)
      write_matrix_sheet(wb, paste0("rcov_b_", outcome), borda_res$mats$rcov)
    }
  }
  
  # Build stacked master sheets
  coef_df <- dplyr::bind_rows(coef_long)
  se_df   <- dplyr::bind_rows(se_long)
  rse_df  <- dplyr::bind_rows(rse_long)
  eb_df   <- dplyr::bind_rows(eb_long)
  
  # Write master sheets (vertical stacked)
  remove_sheet_safely(wb, "Coefficients")
  openxlsx::addWorksheet(wb, "Coefficients")
  openxlsx::writeData(wb, "Coefficients", coef_df)
  
  remove_sheet_safely(wb, "Standard_Errors")
  openxlsx::addWorksheet(wb, "Standard_Errors")
  openxlsx::writeData(wb, "Standard_Errors", se_df)
  
  remove_sheet_safely(wb, "Robust SEs")
  openxlsx::addWorksheet(wb, "Robust SEs")
  openxlsx::writeData(wb, "Robust SEs", rse_df)
  
  remove_sheet_safely(wb, "EB Coefficients")
  openxlsx::addWorksheet(wb, "EB Coefficients")
  openxlsx::writeData(wb, "EB Coefficients", eb_df)
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("✅ Done. Saved:", output_path, "\n")
  
  invisible(results)
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
survey_vars <- c("FirmCont_favor_white")

experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "log_dif_gender_sq", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

firms97 <- data %>% dplyr::filter(!is.na(dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)
industry_map_path <- file.path(processed,"industry_map.xlsx")

subset_var <- NULL
subset_value <- NULL
output_path <- file.path(excel,"Plackett_Luce_Full_Sample_test.xlsx")

run_analysis_pipeline_v2(
    data, respondent_col, survey_vars,
    output_path, industry_map_path,
    run_ol = TRUE, run_pl = TRUE, run_borda = TRUE,
    firms97 = NULL,
    seed = 123
) 