# ------------------------------------------------------------------------------
# Purpose: Simplified Pipeline for Analysis
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# PIPELINE
# ------------------------------------------------------------------------------
run_analysis_pipeline <- function(
    data, respondent_col, survey_vars, experimental_vars = NULL,
    subset_var = NULL, subset_value = NULL, 
    output_path, firms97 = NULL,
    run_ol = FALSE, run_pl = FALSE, run_borda = FALSE, run_ols = FALSE, run_ols_centered = FALSE,
    combine_valences = FALSE, valence_triples = NULL, industry_means = FALSE,
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
  
  prefix_map <- list(OL = "ol", PL = "pl", Borda = "b", OLS = "ols", OLSC = "olsc")
  
  # load workbook
  wb <- if (file.exists(output_path)) openxlsx::loadWorkbook(output_path) else openxlsx::createWorkbook()
  
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
  
  # Prepare Outcomes for Analysis
  prep <- prep_outcomes(data, survey_vars)
  data_wide_list          <- prep$wide
  data_long_list          <- prep$long
  id_map_list             <- prep$id_map
  
################################################################################
## Step 1a: Run Models
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
    run_ols = run_ols,
    run_ols_centered = run_ols_centered,
    firms97 = firms97,
    seed = seed,
    build_subset97 = TRUE,
    combine_valences = combine_valences,
    valence_triples = valence_triples
  )
  
  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 1 Complete. Saved: ", output_path)
 
################################################################################
## Step 1b: Industry means + demeaned outcomes
################################################################################
  if (isTRUE(industry_means)) {
    message("Adding industry means + demeaned outcomes")
    
    models_to_transform <- character(0)
    if (isTRUE(run_ol))           models_to_transform <- c(models_to_transform, "OL")
    if (isTRUE(run_pl))           models_to_transform <- c(models_to_transform, "PL")
    if (isTRUE(run_borda))        models_to_transform <- c(models_to_transform, "Borda")
    if (isTRUE(run_ols))          models_to_transform <- c(models_to_transform, "OLS")
    if (isTRUE(run_ols_centered)) models_to_transform <- c(models_to_transform, "OLSC")
    
    results <- add_industry_means_to_results(
      results       = results,
      industry_map  = industry_map,
      outcomes      = survey_vars,
      model_names   = models_to_transform,
      which_sets    = c("all", "subset97"),
      industry_col  = "aer_naics2",
      suffix_dm     = "_dm",
      suffix_im     = "_im",
      
      # NEW: weighted versions using firm_table$njobs
      weight_col    = "njobs",
      suffix_dm_w   = "_dm_w",
      suffix_im_w   = "_im_w",
      require_positive_weights = TRUE
    )
  }
  
################################################################################
## Step 1c: Save Results
################################################################################  
  message("Writing coefficient outputs after Step 2")
  coef_long_df <- write_coefficients_long_sheet(
    results = results,
    wb = wb,
    sheet_name = "Coefficients",
    include_sets = c("all","subset97"),
    data_for_experimental = data,
    experimental_vars = experimental_vars,
    industry_map = industry_map
  )
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
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
  
  if (!is.null(valence_triples)) {
    new_outcomes <- vapply(valence_triples, `[[`, character(1), "new_outcome")
    variables <- c(survey_vars, new_outcomes)
  } else {
    variables <- survey_vars
  }
  covariance_df <- write_covariance_sheet(results, wb, sheet_name = "covariance", survey_vars = variables)

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
  if (!is.null(valence_triples)) {
    new_outcomes <- vapply(valence_triples, `[[`, character(1), "new_outcome")
    variables <- c(survey_vars, new_outcomes)
  } else {
    variables <- survey_vars
  }

  # which models were run?
  models_to_build <- character(0)
  if (isTRUE(run_pl))    models_to_build <- c(models_to_build, "PL")
  if (isTRUE(run_borda)) models_to_build <- c(models_to_build, "Borda")
  if (isTRUE(run_ol))    models_to_build <- c(models_to_build, "OL")
  if (isTRUE(run_ols))    models_to_build <- c(models_to_build, "OLS")
  if (isTRUE(run_ols_centered))    models_to_build <- c(models_to_build, "OLSC")
  
  # build one noise matrix per model (subset97)
  noise_mats_97 <- setNames(vector("list", length(models_to_build)), models_to_build)

  for (m in models_to_build) {
    noise_mats_97[[m]] <- build_noise_matrix(
      variance_df   = variance_df,
      covariance_df = covariance_df,
      outcomes      = variables,
      subset_value  = "subset97",
      model_value   = m
    )
  }

  # Optional convenience objects if you still want them:
  N97_PL    <- if ("PL"    %in% names(noise_mats_97)) noise_mats_97[["PL"]]    else NULL
  N97_Borda <- if ("Borda" %in% names(noise_mats_97)) noise_mats_97[["Borda"]] else NULL
  N97_OL    <- if ("OL"    %in% names(noise_mats_97)) noise_mats_97[["OL"]]    else NULL
  N97_OLS    <- if ("OLS"    %in% names(noise_mats_97)) noise_mats_97[["OLS"]]    else NULL
  N97_OLSC    <- if ("OLSC"    %in% names(noise_mats_97)) noise_mats_97[["OLSC"]]    else NULL

  # List of Specifications
  regs_uni <- list(
    list(lhs = "cb_central_full",   rhs = c("discretion")),
    list(lhs = "log_dif",           rhs = c("FirmCont_favor_white")),
    list(lhs = "log_dif",           rhs = c("conduct_favor_white")),
    list(lhs = "log_dif",           rhs = c("pooled_favor_white")),
    list(lhs = "log_dif",           rhs = c("FirmCont_favor_white_ep")),
    list(lhs = "log_dif",           rhs = c("conduct_favor_white_ep")),
    list(lhs = "log_dif",           rhs = c("pooled_favor_white_ep")),
    list(lhs = "log_dif",           rhs = c("FirmSelective")),
    list(lhs = "log_dif",           rhs = c("discretion")),
    list(lhs = "log_dif_gender",    rhs = c("FirmCont_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("conduct_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("pooled_favor_male")),
    list(lhs = "log_dif_gender",    rhs = c("FirmCont_favor_male_ep")),
    list(lhs = "log_dif_gender",    rhs = c("conduct_favor_male_ep")),
    list(lhs = "log_dif_gender",    rhs = c("pooled_favor_male_ep")),
    list(lhs = "log_dif_gender",    rhs = c("FirmSelective")),
    list(lhs = "log_dif_gender",    rhs = c("discretion")),
    list(lhs = "log_dif_gender_sq", rhs = c("FirmCont_favor_male")),
    list(lhs = "log_dif_gender_sq", rhs = c("conduct_favor_male")),
    list(lhs = "log_dif_gender_sq", rhs = c("pooled_favor_male")),
    list(lhs = "log_dif_gender_sq", rhs = c("FirmSelective")),
    list(lhs = "log_dif_gender_sq", rhs = c("discretion")),
    list(lhs = "log_dif_sq",        rhs = c("FirmCont_favor_white")),
    list(lhs = "log_dif_sq",        rhs = c("conduct_favor_white")),
    list(lhs = "log_dif_sq",        rhs = c("pooled_favor_white")),
    list(lhs = "log_dif_sq",        rhs = c("FirmSelective")),
    list(lhs = "log_dif_sq",        rhs = c("discretion")),
    list(lhs = "log_dif_age",       rhs = c("conduct_favor_younger")),
    list(lhs = "log_dif_age",       rhs = c("conduct_favor_younger_ep")),
    list(lhs = "log_dif_age",       rhs = c("FirmSelective")),
    list(lhs = "log_dif_age",       rhs = c("discretion"))
  )

  regs_bi <- list(
    list(lhs = "log_dif",        rhs = c("FirmSelective", "discretion")),
    list(lhs = "log_dif_gender", rhs = c("FirmSelective", "discretion")),
    list(lhs = "log_dif_age",    rhs = c("FirmSelective", "discretion"))
  )

  regs_all <- c(regs_uni, regs_bi)
  
  message("Build EIV Dataframes")
  # (optional) filter to a subset for EIV
  coef_long_97 <- coef_long_df |> dplyr::filter(.data$subset == "subset97")
  
  coef_firm <- coef_long_97 |> dplyr::filter(.data$entity_type == "Firm")
  coef_ind  <- coef_long_97 |> dplyr::filter(.data$entity_type == "Industry")
  
  to_wide_by_outcome <- function(df_long, data) {
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
      dplyr::select(entity_id, entity, outcome, estimate) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = c(entity_id, entity),
        names_from = outcome,
        values_from = estimate
      )
    
    df_final <- left_join(df_wide, experimental, by="entity_id")
    
    return(df_final)
  }
  
  coef_firm_wide <- to_wide_by_outcome(coef_firm) %>% left_join(industry_map %>% rename(entity_id = firm_id), by="entity_id")
  coef_ind_wide  <- to_wide_by_outcome(coef_ind)

  message("Running EIV")

  models_to_run_eiv <- intersect(c("PL", "Borda", "OL", "OLS", "OLSC"), names(noise_mats_97))

  eiv_df_firm <- write_eiv_sheet(
    wb,
    sheet_name = "EIV_firm",
    regs = regs_all,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats_97,
    models = names(noise_mats_97),
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs"
  )

  # Save once at end
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 5 Complete. Saved: ", output_path)

################################################################################
## Step 5b: Within/Between Industry EIV
################################################################################
  message("Building Within/Between Industry EIV")

  # RHS outcomes for within (demeaned) and between (industry means)
  dm_rhs_outcomes <- c(
    "FirmCont_favor_white_dm", "conduct_favor_white_dm", "pooled_favor_white_dm",
    "FirmCont_favor_male_dm",  "conduct_favor_male_dm",  "pooled_favor_male_dm",
    "conduct_favor_younger_dm"
  )
  im_rhs_outcomes <- c(
    "FirmCont_favor_white_im", "conduct_favor_white_im", "pooled_favor_white_im",
    "FirmCont_favor_male_im",  "conduct_favor_male_im",  "pooled_favor_male_im",
    "conduct_favor_younger_im"
  )

  # Build noise matrices for _dm and _im outcomes
  noise_dm_97 <- setNames(vector("list", length(models_to_build)), models_to_build)
  noise_im_97 <- setNames(vector("list", length(models_to_build)), models_to_build)

  for (m in models_to_build) {
    noise_dm_97[[m]] <- build_noise_matrix(
      variance_df, covariance_df, outcomes = dm_rhs_outcomes,
      subset_value = "subset97", model_value = m
    )
    noise_im_97[[m]] <- build_noise_matrix(
      variance_df, covariance_df, outcomes = im_rhs_outcomes,
      subset_value = "subset97", model_value = m
    )
  }

  # Compute demeaned LHS vars (experimental outcomes demeaned by industry)
  lhs_to_demean <- c("log_dif", "log_dif_gender", "log_dif_age")
  for (lv in lhs_to_demean) {
    if (lv %in% names(coef_firm_wide)) {
      coef_firm_wide[[paste0(lv, "_dm")]] <- ave(
        coef_firm_wide[[lv]],
        coef_firm_wide$model, coef_firm_wide$aer_naics2,
        FUN = function(x) x - mean(x, na.rm = TRUE)
      )
    }
  }

  # Within-industry EIV specs (no industry FE — already demeaned)
  regs_within <- list(
    list(lhs = "log_dif_dm",        rhs = c("FirmCont_favor_white_dm")),
    list(lhs = "log_dif_dm",        rhs = c("conduct_favor_white_dm")),
    list(lhs = "log_dif_dm",        rhs = c("pooled_favor_white_dm")),
    list(lhs = "log_dif_gender_dm", rhs = c("FirmCont_favor_male_dm")),
    list(lhs = "log_dif_gender_dm", rhs = c("conduct_favor_male_dm")),
    list(lhs = "log_dif_gender_dm", rhs = c("pooled_favor_male_dm")),
    list(lhs = "log_dif_age_dm",    rhs = c("conduct_favor_younger_dm"))
  )

  write_eiv_sheet(
    wb, sheet_name = "EIV_within",
    regs = regs_within,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_dm_97,
    models = models_to_run_eiv,
    id_col = "entity_id",
    model_col = "model",
    use_fe = FALSE
  )

  # Between-industry: build industry-level dataframe
  # Compute industry means of experimental LHS vars from firm data
  lhs_im <- coef_firm_wide |>
    dplyr::group_by(model, aer_naics2) |>
    dplyr::summarize(
      log_dif_im        = mean(log_dif, na.rm = TRUE),
      log_dif_gender_im = mean(log_dif_gender, na.rm = TRUE),
      log_dif_age_im    = mean(log_dif_age, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(entity_id = as.integer(aer_naics2)) |>
    dplyr::select(-aer_naics2)

  # Drop experimental LHS columns from coef_ind_wide before join to avoid .x/.y collision
  lhs_im_cols <- setdiff(names(lhs_im), c("model", "entity_id"))
  coef_ind_wide_clean <- coef_ind_wide[, !(names(coef_ind_wide) %in% lhs_im_cols), drop = FALSE]
  coef_ind_eiv <- dplyr::left_join(coef_ind_wide_clean, lhs_im, by = c("model", "entity_id"))

  # Between-industry EIV specs (no FE — each row is an industry)
  regs_between <- list(
    list(lhs = "log_dif_im",        rhs = c("FirmCont_favor_white_im")),
    list(lhs = "log_dif_im",        rhs = c("conduct_favor_white_im")),
    list(lhs = "log_dif_im",        rhs = c("pooled_favor_white_im")),
    list(lhs = "log_dif_gender_im", rhs = c("FirmCont_favor_male_im")),
    list(lhs = "log_dif_gender_im", rhs = c("conduct_favor_male_im")),
    list(lhs = "log_dif_gender_im", rhs = c("pooled_favor_male_im")),
    list(lhs = "log_dif_age_im",    rhs = c("conduct_favor_younger_im"))
  )

  write_eiv_sheet(
    wb, sheet_name = "EIV_between",
    regs = regs_between,
    coef_df_wide = coef_ind_eiv,
    noise_mats_97 = noise_im_97,
    models = models_to_run_eiv,
    id_col = "entity_id",
    model_col = "model",
    use_fe = FALSE
  )

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  message("✅ Step 5b Complete. Saved: ", output_path)

}
