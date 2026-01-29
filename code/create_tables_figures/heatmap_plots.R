# Prev. called: 3_heatmap_plots
source("code/globals.R")

# ----------------------------------------------- 
# Define Heatmap Generator Function 
# ----------------------------------------------- 
# Ensure every (lhs, rhs) pair also has (rhs, lhs)
ensure_symmetry <- function(df) {
  stopifnot(all(c("lhs","rhs") %in% names(df)))
  
  # Make a flipped copy
  flipped <- df %>%
    mutate(lhs_new = rhs, rhs_new = lhs) %>%
    select(-lhs, -rhs) %>%
    rename(lhs = lhs_new, rhs = rhs_new)
  
  # Match column order to original
  flipped <- flipped[names(df)]
  
  # Bind original + flipped; drop duplicate diagonal/duplicates if any
  bind_rows(df, flipped) %>%
    distinct(lhs, rhs, .keep_all = TRUE)
}

create_lower_left_heatmap <- function(data, title, filename, label_mapping, custom_order, lower_triangle, custom_order2 = NULL) {
  stopifnot(nrow(data) > 0)
  
  # Map lhs/rhs -> pretty labels (keep everything inside `data$...`)
  data <- data %>%
    mutate(
      out1 = unname(label_mapping[as.character(lhs)]),
      out2 = unname(label_mapping[as.character(rhs)]),
      corr_c = as.numeric(corr_c)
    ) 
  
  # Warn if anything isn't mapped
  missing <- unique(c(
    as.character(data$lhs[is.na(data$out1)]),
    as.character(data$rhs[is.na(data$out2)])
  ))
  if (length(missing) > 0) {
    warning(sprintf("Unmapped keys in label_mapping: %s", paste(missing, collapse = ", ")))
  }
  
  if (isTRUE(lower_triangle)) {
    # Indices in the intended final orientation:
    # - X axis (columns) goes left -> right as custom_order
    # - Y axis (rows) goes TOP -> BOTTOM as custom_order
    c_idx <- match(data$out1, custom_order)   # columns
    r_idx <- match(data$out2, custom_order)   # rows (top->bottom)
    
    data_plot <- data %>%
      mutate(c_idx = c_idx, r_idx = r_idx) %>%
      filter(!is.na(c_idx), !is.na(r_idx), !is.na(corr_c)) %>%
      # keep LOWER-LEFT (row index >= col index) + diagonal
      filter(r_idx >= c_idx) %>%
      # now set factors for plotting:
      mutate(
        Outcome1 = factor(out1, levels = custom_order),         # x left->right
        Outcome2 = factor(out2, levels = rev(custom_order))     # y TOP->BOTTOM
      )
  } else {
    stopifnot(!is.null(custom_order2))
    data_plot <- data %>%
      mutate(
        Outcome1 = factor(out1, levels = custom_order),
        Outcome2 = factor(out2, levels = custom_order2)
      ) %>%
      filter(!is.na(Outcome1), !is.na(Outcome2), !is.na(corr_c))
  }
  
  plot <- ggplot(data_plot, aes(x = Outcome1, y = Outcome2, fill = corr_c)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, name = "Correlation") +
    geom_text(aes(label = sprintf("%.2f", corr_c)), size = 3) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(title = title, x = "Outcomes", y = "Outcomes")
  
  ggsave(filename = filename, plot = plot, width = 12, height = 10, dpi = 300)
}


# ----------------------------------------------- 
# Define Wrapper Function for Heatmap Generation 
# ----------------------------------------------- 
generate_heatmaps <- function(file_path, prefix, suffix, sheet, all) {  
  
  # Read Data  
  sheet_name <- sheet
  results_df <- read.xlsx(file_path, sheet = sheet_name)  
  
  # Outcome List  
  outcomes <- c(    
    "discretion", "FirmSelective","FirmDesire", "conduct_black", "conduct_favor_younger",   
    "conduct_favor_male", "FirmHire_favor_male", "FirmHire_favor_white",   
    "FirmCont_favor_male", "FirmCont_favor_white", "FirmCont_favor_male_theta"
  )   
  
  # For each outcome, create a row that is lhs = rhs = outcome and corr = 1
  # Build a minimal identity frame
  identity_rows <- data.frame(
    lhs = outcomes,
    rhs = outcomes,
    corr_c = 1,
    all_firms = all,
    stringsAsFactors = FALSE
  )
  
  # Add any missing columns from results_df, filled with NA
  for (col in setdiff(names(results_df), names(identity_rows))) {
    identity_rows[[col]] <- NA
  }
  
  # Reorder columns to match results_df
  identity_rows <- identity_rows[names(results_df)]
  
  # Bind
  results_df <- rbind(results_df, identity_rows)
  
  # After you've created identity_rows and rbind'ed:
  results_df <- results_df %>%
    mutate(lhs = as.character(lhs), rhs = as.character(rhs))
  
  results_df <- results_df %>%
    filter(all_firms == all)
  
  results_df <- ensure_symmetry(results_df)
  
  # Define Label Mapping and Custom Order  
  label_mapping <- c(    
    "dif" = "E: Race, Contact Gap",    
    "log_dif" = "E: Race, Contact Gap (log)",    
    "dif_gender" = "E: Gender, Contact Gap",    
    "log_dif_gender" = "E: Gender, Contact Gap (log)",    
    "dif_age" = "E: Age, Contact Gap",   
    "log_dif_age" = "E: Age, Contact Gap (log)",   
    "discretion" = "S: Manager Discretion",    
    "FirmSelective" = "S: Firm Selectivity",    
    "FirmDesire" = "S: Firm Desirability",    
    "conduct_black" = "S: Discrimination Black (Conduct)",    
    "conduct_favor_younger" = "S: Discrimination Older (Conduct)",    
    "conduct_favor_male" = "S: Discrimination Female (Conduct)",    
    "FirmHire_favor_male" = "S: Discrimination Female (Hire)",    
    "FirmHire_favor_white" = "S: Discrimination Black (Hire)",    
    "FirmCont_favor_male" = "S: Discrimination Female (Contact)",    
    "FirmCont_favor_white" = "S: Discrimination Black (Contact)"
  )    
  
  non_experimental_data <- results_df %>% 
    filter(lhs %in% c("conduct_black", "conduct_favor_younger", "FirmHire_favor_white", "FirmHire_favor_male",
                           "FirmCont_favor_white", "FirmCont_favor_male", "discretion", "FirmSelective", "FirmDesire")) %>%
    filter(rhs %in% c("conduct_black", "conduct_favor_younger", "FirmHire_favor_white", "FirmHire_favor_male",
                      "FirmCont_favor_white", "FirmCont_favor_male", "discretion", "FirmSelective", "FirmDesire"))
  
  # Custom order for the axes  
  custom_order_non <- c(    
    "S: Discrimination Black (Conduct)",    
    "S: Discrimination Black (Hire)",   
    "S: Discrimination Black (Contact)", 
    "S: Discrimination Female (Conduct)",    
    "S: Discrimination Female (Hire)",    
    "S: Discrimination Female (Contact)",
    "S: Discrimination Older (Conduct)",    
    "S: Firm Desirability",    
    "S: Firm Selectivity",    
    "S: Manager Discretion"
  )    
  
  
  custom_order_exp <- c(    
    "E: Race, Contact Gap",    
    "E: Race, Contact Gap (log)",    
    "E: Gender, Contact Gap",    
    "E: Gender, Contact Gap (log)",   
    "E: Age, Contact Gap",   
    "E: Age, Contact Gap (log)"  
  )    
  
  # Generate Heatmaps  
  lower_triangle <- TRUE
  # create_lower_left_heatmap(
  #   experimental_data,
  #   "",
  #   paste0(prefix, "heatmap_experimental_lower_left", suffix, ".png"),
  #   label_mapping,
  #   custom_order_exp,
  #   lower_triangle
  # )

  lower_triangle <- TRUE 
  create_lower_left_heatmap(    
    results_df,    
    "",    
    paste0(prefix, "heatmap_non_experimental_lower_left", suffix, ".png"),    
    label_mapping,    
    custom_order_non,   
    lower_triangle   
  )     
  
  lower_triangle <- FALSE
  # create_lower_left_heatmap(
  #   mixed_data,
  #   "",
  #   paste0(prefix, "heatmap_mixed_horizontal", suffix, ".png"),
  #   label_mapping,
  #   custom_order_non,
  #   lower_triangle,
  #   custom_order_exp
  # )
  # 
  # create_lower_left_heatmap(
  #   mixed_data,
  #   "",
  #   paste0(prefix, "heatmap_mixed_vertical", suffix, ".png"),
  #   label_mapping,
  #   custom_order_exp,
  #   lower_triangle,
  #   custom_order_non
  # )

  cat("✅ Heatmaps generated and saved with prefix:", prefix, "\n")  
}   

# Example Usage  
generate_heatmaps("~/Documents/consolidated_code_server/excel/Plackett_Luce_Full_Sample.xlsx","~/Documents/consolidated_code_server/figures/heatmaps/", "_full", "pairwise_summary", all = TRUE)
generate_heatmaps("~/Documents/consolidated_code_server/excel/Plackett_Luce_Full_Sample.xlsx", 
                  "~/Documents/consolidated_code_server/figures/heatmaps/", "_full_overlap", "pairwise_summary", all = FALSE)
generate_heatmaps("~/Documents/consolidated_code_server/excel/Plackett_Luce_Full_Sample.xlsx", 
                  "~/Documents/consolidated_code_server/figures/heatmaps/", "_full_borda", "pairwise_summary_borda", all = TRUE)
generate_heatmaps("~/Documents/consolidated_code_server/excel/Plackett_Luce_Full_Sample.xlsx", 
                  "~/Documents/consolidated_code_server/figures/heatmaps/", "_full_overlap_borda", "pairwise_summary_borda", all = FALSE)


# 
# generate_heatmaps("excel/Plackett_Luce_Subset_Black.xlsx", "figures/heatmaps/", "_subset_black", "pairwise_summary", all = TRUE)
# generate_heatmaps("excel/Plackett_Luce_Subset_White.xlsx", "figures/heatmaps/", "_subset_white", "pairwise_summary", all = TRUE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Male.xlsx", "figures/heatmaps/", "_subset_male", "pairwise_summary", all = TRUE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Female.xlsx", "figures/heatmaps/", "_subset_female", "pairwise_summary", all = TRUE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Age_gte40.xlsx", "figures/heatmaps/", "_subset_age_gte40", "pairwise_summary", all = TRUE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Age_lt40.xlsx", "figures/heatmaps/", "_subset_age_lt40", "pairwise_summary", all = TRUE)
# 
# 
# generate_heatmaps("excel/Plackett_Luce_Subset_Black.xlsx", "figures/heatmaps/", "_subset_black_overlap", "pairwise_summary", all = FALSE)
# generate_heatmaps("excel/Plackett_Luce_Subset_White.xlsx", "figures/heatmaps/", "_subset_white_overlap", "pairwise_summary", all = FALSE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Male.xlsx", "figures/heatmaps/", "_subset_male_overlap", "pairwise_summary", all = FALSE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Female.xlsx", "figures/heatmaps/", "_subset_female_overlap", "pairwise_summary", all = FALSE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Age_gte40.xlsx", "figures/heatmaps/", "_subset_age_gte40_overlap", "pairwise_summary", all = FALSE)
# generate_heatmaps("excel/Plackett_Luce_Subset_Age_lt40.xlsx", "figures/heatmaps/", "_subset_age_lt40_overlap", "pairwise_summary", all = FALSE)
