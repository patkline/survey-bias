
# ------------------------------------------------------------------------------
# Purpose: Simplified Pipeline for Analysis
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
run_analysis_pipeline <- function(
    data, survey_vars, experimental_vars = NULL,
    subset_var = NULL, subset_value = NULL,
    output_dir, firms97 = NULL,
    run_borda = TRUE, run_ols = TRUE,
    industry_mean_outcomes = NULL,
    seed = 123
) {


################################################################################
## Step 0: Preliminaries
################################################################################
  set.seed(seed)

  # Restrict to correct subset
  # Subset data if a subset variable and value are provided
  if (!is.null(subset_var) & !is.null(subset_value)) {
    data <- data %>% dplyr::filter(!!sym(subset_var) == subset_value)
  }

  is_full_sample <- is.null(subset_var) && is.null(subset_value)

  # Ensure output directory exists (one parquet file per sheet lives here)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Industry information is needed only for full-sample industry means and EIV.
  industry_map <- NULL
  if (is_full_sample) {
    stopifnot(all(c("firm_id", "aer_naics2", "aer_naics2_name") %in% names(data)))

    industry_map <- data %>%
      dplyr::select(firm_id, aer_naics2, aer_naics2_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(firm_id = as.integer(firm_id))

    stopifnot(nrow(industry_map) == dplyr::n_distinct(industry_map$firm_id))
    stopifnot(!anyNA(industry_map$aer_naics2))
  }
  
  # Prepare Outcomes for Analysis
  prep <- prep_outcomes(data, survey_vars)
  data_wide_list          <- prep$wide
  data_long_list          <- prep$long
  id_map_list             <- prep$id_map
  
################################################################################
## Step 1a: Run Models
################################################################################ 
  results <- run_models(
    survey_vars = survey_vars,
    data_wide_list = data_wide_list,
    data_long_list = data_long_list,
    id_map_list    = id_map_list,
    run_borda = run_borda,
    run_ols = run_ols,
    firms97 = firms97,
    seed = seed,
    build_subset97 = TRUE
  )

  message("✅ Step 1 Complete. Output directory: ", output_dir)
 
################################################################################
## Step 1b: Industry means + demeaned outcomes
################################################################################
  if (!is.null(industry_mean_outcomes) && length(industry_mean_outcomes) > 0L) {
    if (!is_full_sample) {
      stop("Industry-mean outcomes are only generated for the full-sample run.")
    }
    if (!all(industry_mean_outcomes %in% survey_vars)) {
      stop("Every industry_mean_outcome must also appear in survey_vars.")
    }
    message("Adding industry means + demeaned outcomes")
    models_to_transform <- c(if (isTRUE(run_borda)) "Borda", if (isTRUE(run_ols)) "OLS")
    
    results <- add_industry_means_to_results(
      results       = results,
      industry_map  = industry_map,
      outcomes      = industry_mean_outcomes,
      model_names   = models_to_transform,
      which_sets    = c("all", "subset97"),
      industry_col  = "aer_naics2",
      suffix_dm     = "_dm",
      suffix_im     = "_im",
      entity_weight_col = "njobs"
    )
  }
  
################################################################################
## Step 1c: Save Results
################################################################################  
  message("Writing coefficient outputs after Step 2")
  coef_long_df <- write_coefficients_long_sheet(
    results = results,
    output_dir = output_dir,
    sheet_name = "Coefficients",
    include_sets = c("all","subset97"),
    data_for_experimental = data,
    experimental_vars = experimental_vars
  )

  message("Writing rcov outputs after Step 2")
  write_rcov_long_sheet(
    results = results,
    output_dir = output_dir,
    sheet_name = "rcov",
    include_sets = c("all", "subset97")
  )

################################################################################
## Step 2: Variance, Noise, Signal Variance
################################################################################
  ## See functions in "variance_functions.R"
  message("Variance Denoising")
  variance_df <- write_variance_sheet(results, output_dir, sheet_name = "variance")

  message("✅ Step 2 Complete. Output directory: ", output_dir)

  # Subgroup draft exhibits use only Coefficients, rcov, and variance.
  if (!is_full_sample) {
    return(invisible(NULL))
  }

################################################################################
## Step 3: Covariance, Noise, Signal Covariance
################################################################################
  message("Covariance Denoising")
  covariance_df <- write_covariance_sheet(
    results,
    output_dir,
    sheet_name = "covariance",
    survey_vars = survey_vars
  )

  message("✅ Step 3 Complete. Output directory: ", output_dir)

################################################################################
## Step 4: EIV
################################################################################
  message("Building Noise Matrices for EIV")
  models_to_build <- c(if (isTRUE(run_borda)) "Borda", if (isTRUE(run_ols)) "OLS")
  
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

  # EIV specifications used by Tables 7--8 and Appendix Tables A5--A6.
  eiv_specs <- list(
    list(lhs = "log_dif",           rhs = c("FirmCont_favor_white")),
    list(lhs = "log_dif",           rhs = c("conduct_favor_white")),
    list(lhs = "log_dif",           rhs = c("pooled_favor_white")),
    list(lhs = "log_dif",           rhs = c("FirmSelective")),
    list(lhs = "log_dif",           rhs = c("discretion")),
    list(lhs = "log_dif_gender",    rhs = c("FirmCont_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("conduct_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("pooled_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("FirmSelective")),
    list(lhs = "log_dif_gender",    rhs = c("discretion"))
  )
  
  message("Build EIV Dataframes")
  coef_long_97 <- coef_long_df |> dplyr::filter(.data$subset == "subset97")
  coef_firm <- coef_long_97 |> dplyr::filter(.data$entity_type == "Firm")
  
  to_wide_by_outcome <- function(df_long) {
    df_wide <- df_long |>
      dplyr::filter(model != "EXPERIMENTAL") |>
      dplyr::select(model, entity_id, entity, outcome, estimate, njobs) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = c(model, entity_id, entity, njobs),
        names_from = outcome,
        values_from = estimate
      )
    
    experimental <- df_long |>
      dplyr::filter(model == "EXPERIMENTAL") |>
      dplyr::select(entity_id, outcome, estimate) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = entity_id,
        names_from = outcome,
        values_from = estimate
      )
    
    dplyr::left_join(df_wide, experimental, by = "entity_id")
  }
  
  coef_firm_wide <- to_wide_by_outcome(coef_firm) %>%
    dplyr::left_join(
      industry_map %>% dplyr::rename(entity_id = firm_id),
      by = "entity_id"
    )

  message("Running EIV")

  write_eiv_sheet(
    output_dir,
    sheet_name = "EIV_firm",
    regs = eiv_specs,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats_97,
    models = models_to_build,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs"
  )

  message("✅ Step 4 Complete. Output directory: ", output_dir)
  invisible(NULL)
}
