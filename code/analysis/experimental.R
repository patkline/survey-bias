# -----------------------------------------------
# Define Function to Run Plackett-Luce and Extract Coefficients
# -----------------------------------------------
clean_experimental <- function(data, outcomes) {
  
  # Initialize dataframes to store coefficients and standard errors
  coeff_df <- data.frame()
  se_df <- data.frame()
  
  # Loop through all outcomes
  for (outcome in outcomes) {
    cat("Processing outcome:", outcome, "\n")
    if (!all(is.na(data[[outcome]]))) {
        if (outcome != "cb_central_full") {
          outcome_coeff <- data %>%
            select(firm, firm_id, !!sym(outcome), paste0(outcome, "_se")) %>%
            rename(Estimate := !!sym(outcome),
                   Estimate_se := !!sym(paste0(outcome, "_se"))) %>%
            distinct() %>%
            drop_na()
        } else {
          outcome_coeff <- data %>%
            select(firm, firm_id, !!sym(outcome)) %>%
            rename(Estimate := !!sym(outcome)) %>%
            mutate(Estimate_se = 0) %>%
            distinct() %>%
            drop_na()
        }
      } 
      
      # Append to cumulative dataframes
      if (nrow(coeff_df) == 0) {
          coeff_df <- outcome_coeff %>% select(firm, firm_id, Estimate)
          colnames(coeff_df)[3] <- outcome
          
          se_df <- outcome_coeff %>% select(firm, firm_id, Estimate_se)
          colnames(se_df)[3] <- paste0(outcome, "_se")
      } else {
          coeff_df <- full_join(coeff_df, outcome_coeff %>% select(firm, firm_id, Estimate), by = c("firm", "firm_id"))
          colnames(coeff_df)[ncol(coeff_df)] <- outcome
          
          se_df <- full_join(se_df, outcome_coeff %>% select(firm, firm_id, Estimate_se), by = c("firm", "firm_id"))
          colnames(se_df)[ncol(se_df)] <- paste0(outcome, "_se")
      }
  }
  print("Experimental Outcomes Completed.")
  
  return(list(coefficients = coeff_df, standard_errors = se_df))
}
