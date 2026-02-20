clean_experimental <- function(data, outcomes) {
  
  # Initialize dataframes to store coefficients and standard errors
  coeff_df <- data.frame()
  se_df    <- data.frame()
  
  for (outcome in outcomes) {
    cat("Processing outcome:", outcome, "\n")
    
    # Skip if outcome column missing or all NA
    if (!(outcome %in% names(data)) || all(is.na(data[[outcome]]))) next
    
    se_col <- paste0(outcome, "_se")
    
    if (outcome != "cb_central_full") {
      
      # If SE column exists, use it; otherwise create NA SEs
      if (se_col %in% names(data)) {
        outcome_coeff <- data %>%
          dplyr::select(firm, firm_id, !!rlang::sym(outcome), !!rlang::sym(se_col)) %>%
          dplyr::rename(
            Estimate    = !!rlang::sym(outcome),
            Estimate_se = !!rlang::sym(se_col)
          ) %>%
          dplyr::distinct() %>%
          tidyr::drop_na(Estimate)     # keep rows with estimate; SE can be NA
      } else {
        outcome_coeff <- data %>%
          dplyr::select(firm, firm_id, !!rlang::sym(outcome)) %>%
          dplyr::rename(Estimate = !!rlang::sym(outcome)) %>%
          dplyr::mutate(Estimate_se = NA_real_) %>%
          dplyr::distinct() %>%
          tidyr::drop_na(Estimate)
      }
      
    } else {
      
      # cb_central_full: SE defined as 0 (as in your original)
      outcome_coeff <- data %>%
        dplyr::select(firm, firm_id, !!rlang::sym(outcome)) %>%
        dplyr::rename(Estimate = !!rlang::sym(outcome)) %>%
        dplyr::mutate(Estimate_se = 0) %>%
        dplyr::distinct() %>%
        tidyr::drop_na(Estimate)
    }
    
    # Append / merge into cumulative frames
    if (nrow(coeff_df) == 0) {
      coeff_df <- outcome_coeff %>% dplyr::select(firm, firm_id, Estimate)
      names(coeff_df)[3] <- outcome
      
      se_df <- outcome_coeff %>% dplyr::select(firm, firm_id, Estimate_se)
      names(se_df)[3] <- paste0(outcome, "_se")
    } else {
      coeff_df <- dplyr::full_join(
        coeff_df,
        outcome_coeff %>% dplyr::select(firm, firm_id, Estimate),
        by = c("firm", "firm_id")
      )
      names(coeff_df)[ncol(coeff_df)] <- outcome
      
      se_df <- dplyr::full_join(
        se_df,
        outcome_coeff %>% dplyr::select(firm, firm_id, Estimate_se),
        by = c("firm", "firm_id")
      )
      names(se_df)[ncol(se_df)] <- paste0(outcome, "_se")
    }
  }
  
  print("Experimental Outcomes Completed.")
  return(list(coefficients = coeff_df, standard_errors = se_df))
}
