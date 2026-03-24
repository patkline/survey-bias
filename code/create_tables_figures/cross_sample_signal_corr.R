# ----------------------------------------------------------------------------------------------------
# Purpose: Construct cross-sample signal correlation tables for different outcomes 
# and models + Wald tests comparing subsamples
# 
# Created: Jordan Cammarota 
# Edited: Nico Rotundo 2026-03-24
# ----------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# ----------------------------------------------------------------------------------------------------
# Build cached theta vectors for every model-sample-outcome combination
# ----------------------------------------------------------------------------------------------------
# Initialize list to store cached `entity_id`-`theta` vectors
theta_vectors <- list()

# Define local that defines the set of samples we are looping over here, where the name corresponds to the Plackett-Luce file suffixes
sample_list <- c("Black", "White", "Female", "Male", "Looking", "Not_Looking", "Feared_Discrimination_1", "Feared_Discrimination_0", "Age_gte40", "Age_lt40", "College", "No_College", "Convenience", "Probability", "Conf_Gender_N", "Conf_Gender_Y", "Conf_Race_N", "Conf_Race_Y")
# sample_list <- c("Black", "White")

# Loop over sample excel sheets
for (sample in sample_list) {

  # Assign `Coefficient` sheet in excel to a data frame
  theta_vector_sample <- readxl::read_xlsx(
    # File path 
    file.path(excel, paste0("Plackett_Luce_Subset_", sample, ".xlsx")),
    # Sheet name
    sheet = "Coefficients"
  )

  # Assert unique on entity_type x subset x model x outcome x entity_id 
  stopifnot(anyDuplicated(theta_vector_sample[, c("entity_type", "subset", "model", "outcome", "entity_id")]) == 0L)

  # Keep observations where entity_type is firm and subset is all 
  theta_vector_sample <- theta_vector_sample %>%
    dplyr::filter(.data$entity_type == "Firm", .data$subset == "all")

  # Loop over models
  for (model in c("OLS", "Borda")) {
    # Loop over outcomes 
    for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
      # Print current sample, model, and outcome 
      print(paste("🎃 Processing theta for sample:", sample, "| model:", model, "| outcome:", outcome))

      # Keep necessary observations for current model x outcome
      theta_vector_sample_model_outcome <- theta_vector_sample %>%
        dplyr::filter(.data$model == .env$model, .data$outcome == .env$outcome)
        
      # Keep only entity_id and estimate columns, renaming estimate to theta
      theta_vector_sample_model_outcome <- theta_vector_sample_model_outcome %>%  
        dplyr::transmute(entity_id = .data$entity_id, theta = .data$estimate)

      # Assert non-missing entity_id and theta
      stopifnot(!anyNA(theta_vector_sample_model_outcome$entity_id), !anyNA(theta_vector_sample_model_outcome$theta))

      # Assert uniqueness on entity_id
      stopifnot(anyDuplicated(theta_vector_sample_model_outcome$entity_id) == 0L)

      # Assert 164 observations (one for each firm)
      stopifnot(nrow(theta_vector_sample_model_outcome) == 164L)

      # Define lookup key as `model'_`sample'_'`outcome`
      theta_key <- paste("theta", model, sample, outcome, sep = "_")

      # Store current model x outcome theta vector in cache with underscore-joined key
      theta_vectors[[theta_key]] <- theta_vector_sample_model_outcome
    }
  }
}

# Print cached theta list
print(names(theta_vectors))

# ----------------------------------------------------------------------------------------------------
# Build cached signal and total variance vectors for every model-sample-outcome 
# combination
# ----------------------------------------------------------------------------------------------------
# Initialize list to store cached `total_variance`-`signal` vectors
signal_total_variance_vectors <- list()

# Loop over sample excel sheets
for (sample in sample_list) {

  # Assign `variance` sheet in excel to a data frame
  signal_total_variance_sample <- readxl::read_xlsx(
    # File paths
    file.path(excel, paste0("Plackett_Luce_Subset_", sample, ".xlsx")),
    # Sheet name
    sheet = "variance"
  )

  # Assert unique on subset x model x outcome
  stopifnot(anyDuplicated(signal_total_variance_sample[, c("subset", "model", "outcome")]) == 0L)

  # Keep observations where subset is all
  signal_total_variance_sample <- signal_total_variance_sample %>%
    dplyr::filter(.data$subset == "all")

  # Loop over models
  for (model in c("OLS", "Borda")) {
    # Loop over outcomes 
    for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
      # Print current sample, model, and outcome 
      print(paste("🎃 Processing variance for sample:", sample, "| model:", model, "| outcome:", outcome))

      # Keep necessary observations for current model x outcome
      signal_total_variance_sample_model_outcome <- signal_total_variance_sample %>%
        dplyr::filter(.data$model == .env$model, .data$outcome == .env$outcome)
        
      # Keep only variance and signal columns, renaming variance to total_variance
      signal_total_variance_sample_model_outcome <- signal_total_variance_sample_model_outcome %>%  
        dplyr::transmute(total_variance = as.numeric(.data$variance), signal = as.numeric(.data$signal))

      # Assert non-missing total variance and signal
      stopifnot(!anyNA(signal_total_variance_sample_model_outcome$total_variance), !anyNA(signal_total_variance_sample_model_outcome$signal))

      # Assert one observation
      stopifnot(nrow(signal_total_variance_sample_model_outcome) == 1L)

      # Define lookup key as `model'_`sample'_'`outcome`
      signal_key <- paste("signal_total_variance", model, sample, outcome, sep = "_")

      # Store current model x outcome signal/variance vector in cache with underscore-joined key
      signal_total_variance_vectors[[signal_key]] <- signal_total_variance_sample_model_outcome
    }
  }
}

# Print cached signal and total variance list
print(names(signal_total_variance_vectors))

# ----------------------------------------------------------------------------------------------------
# Run cross-sample correlations, and save outputs in a model x sample pair x 
# outcome data frame
# ----------------------------------------------------------------------------------------------------
# Define local pair list using file-suffix names and display labels
sample_pair_list <- list(
  
  # Black vs White
  list(sample_1 = "Black", sample_2 = "White"),
  
  # Female vs Male
  list(sample_1 = "Female", sample_2 = "Male"),
  
  # Looking for a job vs Not looking for a job
  list(sample_1 = "Looking", sample_2 = "Not_Looking"),
  
  # Feared discrimination vs Did not fear discrimination
  list(sample_1 = "Feared_Discrimination_1", sample_2 = "Feared_Discrimination_0"),
  
  # 40 years or older vs Less than 40 years old
  list(sample_1 = "Age_gte40", sample_2 = "Age_lt40"),
  
  # At least some college vs HS diploma or less
  list(sample_1 = "College", sample_2 = "No_College"),
  
  # Convenience vs Probability sample
  list(sample_1 = "Convenience", sample_2 = "Probability"),
  
  # Not confident vs Confident Gender 
  list(sample_1 = "Conf_Gender_N", sample_2 = "Conf_Gender_Y"),
  
  # Not confident vs Confident Race 
  list(sample_1 = "Conf_Race_N", sample_2 = "Conf_Race_Y")
)

# Initialize list to store correlation rows
correlation_rows <- list()

# Initialize row counter for correlation row list
correlation_row_id <- 1L

# Loop over sample pairs
for (sample_pair in sample_pair_list) {
  # Loop over models
  for (model in c("OLS", "Borda")) {
    # Loop over outcomes
    for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
      
      # Print current sample pair, model, and outcome
      print(paste("🎃 Processing correlation for pair:", sample_pair$sample_1, "vs", sample_pair$sample_2, "| model:", model, "| outcome:", outcome))

      # Assert all required vectors are available
      stopifnot(
        !is.null(theta_vectors[[paste("theta", model, sample_pair$sample_1, outcome, sep = "_")]]),
        !is.null(theta_vectors[[paste("theta", model, sample_pair$sample_2, outcome, sep = "_")]]),
        !is.null(signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_1, outcome, sep = "_")]]),
        !is.null(signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_2, outcome, sep = "_")]])
      )

      # Merge theta vectors on common entity IDs (i.e., firms)
      theta_vector_merged <- dplyr::inner_join(
        
        # First sample theta vector, renaming theta column to theta_sample_1 for downstream covariance calculation
        dplyr::rename(theta_vectors[[paste("theta", model, sample_pair$sample_1, outcome, sep = "_")]], theta_sample_1 = theta),

        # Second sample theta vector, renaming theta column to theta_sample_2 for downstream covariance calculation
        dplyr::rename(theta_vectors[[paste("theta", model, sample_pair$sample_2, outcome, sep = "_")]], theta_sample_2 = theta),
        
        # Merge on entity_id
        by = "entity_id"
      ) 
      
      # Sort merged theta vector by entity_id 
      theta_vector_merged <- theta_vector_merged %>%
         dplyr::arrange(.data$entity_id)

      # Assert at least two common IDs for covariance calculation
      stopifnot(nrow(theta_vector_merged) >= 2L)

      ## Compute Cov(x, y) = E[(x-\bar{x})(y-\bar{y})]
      # Center first theta vector
      theta_values_1_centered <- theta_vector_merged$theta_sample_1 - mean(theta_vector_merged$theta_sample_1, na.rm = TRUE)

      # Center second theta vector
      theta_values_2_centered <- theta_vector_merged$theta_sample_2 - mean(theta_vector_merged$theta_sample_2, na.rm = TRUE)

      # Compute covariance between centered theta vectors
      theta_covariance <- mean(theta_values_1_centered * theta_values_2_centered, na.rm = TRUE)
      
      ## Compute signal correlation as Cov(x, y) / sqrt(SignalVar(x) * SignalVar(y)), where signal variance is pulled from the `variance` sheet in excel (and stored in cache above)
      # Pull first sample total variance
      total_variance_1 <- signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_1, outcome, sep = "_")]]$total_variance[1]

      # Pull second sample total variance
      total_variance_2 <- signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_2, outcome, sep = "_")]]$total_variance[1]

      # Pull first sample signal variance
      signal_variance_1 <- signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_1, outcome, sep = "_")]]$signal[1]

      # Pull second sample signal variance
      signal_variance_2 <- signal_total_variance_vectors[[paste("signal_total_variance", model, sample_pair$sample_2, outcome, sep = "_")]]$signal[1]

      # Compute denominator for signal correlation
      signal_correlation_denominator <- sqrt(signal_variance_1 * signal_variance_2)

      # Compute signal correlation
      signal_correlation <- theta_covariance / signal_correlation_denominator

      # Compute temporary signal correlation using built-in correlation as a check
      signal_correlation_temp <- stats::cor(theta_vector_merged$theta_sample_1, theta_vector_merged$theta_sample_2) * stats::sd(theta_vector_merged$theta_sample_1) * stats::sd(theta_vector_merged$theta_sample_2) * ((nrow(theta_vector_merged) - 1) / nrow(theta_vector_merged)) / sqrt(signal_variance_1 * signal_variance_2)

      # Assert temporary and manual signal correlations are equal
      stopifnot(isTRUE(all.equal(signal_correlation, signal_correlation_temp, tolerance = 1e-12)))

      # Store current correlation row
      correlation_rows[[correlation_row_id]] <- tibble::tibble(
        model      = tolower(model),
        outcome    = outcome,
        sample_1    = sample_pair$sample_1,
        sample_2    = sample_pair$sample_2,
        J_entities = nrow(theta_vector_merged),
        theta_covariance = theta_covariance,
        total_variance_sample_1   = total_variance_1,
        total_variance_sample_2   = total_variance_2,
        signal_variance_sample_1    = signal_variance_1,
        signal_variance_sample_2    = signal_variance_2,
        signal_correlation     = signal_correlation #,
        #Wald_stat  = NA_real_,
        #Wald_df    = NA_integer_,
        #Wald_pval  = NA_real_
      )

      # Increment row counter
      correlation_row_id <- correlation_row_id + 1L
    }
  }
}

# Bind correlation rows into one data frame
correlation_rows_data_frame <- dplyr::bind_rows(correlation_rows)

# Assert unique on model x outcome x sample pair
stopifnot(anyDuplicated(correlation_rows_data_frame[, c("model", "outcome", "sample_1", "sample_2")]) == 0L)

# ----------------------------------------------------------------------------------------------------
# XXWald tests 
# ----------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------
# Reshape correlation_rows_data_frame to wide format by model and outcome, 
# keep necessary variables for table construction, and set row labels and ordering  
# ----------------------------------------------------------------------------------------------------
# Reshape correlation_rows_data_frame to wide by outcome 
correlation_rows_data_frame <- correlation_rows_data_frame %>%
  tidyr::pivot_wider(
    id_cols = c("model", "sample_1", "sample_2"),
    names_from = "outcome",
    values_from = c("J_entities", "theta_covariance", "total_variance_sample_1", "total_variance_sample_2", "signal_variance_sample_1", "signal_variance_sample_2", "signal_correlation" #, "Wald_stat", "Wald_df", "Wald_pval"
    )
  )

# Assert unique on model x sample pair
stopifnot(anyDuplicated(correlation_rows_data_frame[, c("model", "sample_1", "sample_2")]) == 0L)

# Add row label variable to dataframe depending on sample pair
correlation_rows_data_frame <- correlation_rows_data_frame %>%
  dplyr::mutate(
    row_label = dplyr::case_when(
      .data$sample_1 == "Black" & .data$sample_2 == "White" ~ "Black vs White",
      .data$sample_1 == "Female" & .data$sample_2 == "Male" ~ "Female vs Male",
      .data$sample_1 == "Looking" & .data$sample_2 == "Not_Looking" ~ "Looking for a Job vs Not",
      .data$sample_1 == "Feared_Discrimination_1" & .data$sample_2 == "Feared_Discrimination_0" ~ "Feared Discrimination vs Not",
      .data$sample_1 == "Age_gte40" & .data$sample_2 == "Age_lt40" ~ "Age $>=$ 40 vs $<$ 40",
      .data$sample_1 == "College" & .data$sample_2 == "No_College" ~ "At Least Some College vs HS Diploma or less",
      .data$sample_1 == "Convenience" & .data$sample_2 == "Probability" ~ "Convenience vs Probability",
      .data$sample_1 == "Conf_Gender_N" & .data$sample_2 == "Conf_Gender_Y" ~ "Confident vs Not (Gender)",
      .data$sample_1 == "Conf_Race_N" & .data$sample_2 == "Conf_Race_Y" ~ "Confident vs Not (Race)",
      TRUE ~ NA_character_
    )
  )

# Define an order variable corresponding to the order of the rows in the final table
correlation_rows_data_frame <- correlation_rows_data_frame %>%
  dplyr::mutate(
    order = dplyr::case_when(
      .data$row_label == "Black vs White" ~ 1L,
      .data$row_label == "Female vs Male" ~ 2L,
      .data$row_label == "Looking for a Job vs Not" ~ 3L,
      .data$row_label == "Feared Discrimination vs Not" ~ 4L,
      .data$row_label == "Age $>=$ 40 vs $<$ 40" ~ 5L,
      .data$row_label == "At Least Some College vs HS Diploma or less" ~ 6L,
      .data$row_label == "Convenience vs Probability" ~ 7L,
      .data$row_label == "Confident vs Not (Gender)" ~ 8L,
      .data$row_label == "Confident vs Not (Race)" ~ 9L
    )
  )

# Sort the data frame by the order variable within model 
correlation_rows_data_frame <- correlation_rows_data_frame %>%
  dplyr::arrange(.data$model, .data$order)

# Keep necessary variables for final table 
correlation_rows_data_frame <- correlation_rows_data_frame %>%
  dplyr::select(model, order, row_label, signal_correlation_pooled_favor_white, signal_correlation_pooled_favor_male #, starts_with("Wald")
  )

#View(correlation_rows_data_frame)

# ----------------------------------------------------------------------------------------------------
# Construct model-specific panels from correlation_rows_data_frame
# ----------------------------------------------------------------------------------------------------

# Assign ols panel to data frame
panel_ols   <- dplyr::filter(correlation_rows_data_frame, model == "ols")

# Assign borda panel to data frame
panel_borda <- dplyr::filter(correlation_rows_data_frame, model == "borda")

# Assert row counts are equal across panels
stopifnot(nrow(panel_ols) == nrow(panel_borda))

# View Panel A and Panel B table inputs
#View(panel_ols)
#View(panel_borda)

# ----------------------------------------------------------------------------------------------------
# Input panels into latex table, and export 
# ----------------------------------------------------------------------------------------------------
# Initialize latex line vector with header rows
latex_lines_ols_borda <- c(
  "  \\centering",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\multicolumn{2}{c}{Discrimination Black} & \\multicolumn{2}{c}{Discrimination Female} \\\\",
  "    & Corr & Wald p-value & Corr & Wald p-value \\\\",
  "    \\midrule",
  "    \\multicolumn{5}{l}{\\textbf{Panel A: Likert}}\\\\"
)

# Append Panel A table rows
for (row_index in seq_len(nrow(panel_ols))) {
  latex_lines_ols_borda <- c(
    latex_lines_ols_borda,
    paste0(
      "    ",
      panel_ols$row_label[row_index], " & ",
      formatC(panel_ols$signal_correlation_pooled_favor_white[row_index], digits = 3, format = "f"), " & ",
      "", " & ",
      formatC(panel_ols$signal_correlation_pooled_favor_male[row_index], digits = 3, format = "f"), " & ",
      "", " \\\\"
    )
  )
}

# Append Panel B header
latex_lines_ols_borda <- c(
  latex_lines_ols_borda,
  "    \\addlinespace",
  "    \\multicolumn{5}{l}{\\textbf{Panel B: Borda}}\\\\"
)

# Append Panel B table rows
for (row_index in seq_len(nrow(panel_borda))) {
  latex_lines_ols_borda <- c(
    latex_lines_ols_borda,
    paste0(
      "    ",
      panel_borda$row_label[row_index], " & ",
      formatC(panel_borda$signal_correlation_pooled_favor_white[row_index], digits = 3, format = "f"), " & ",
      "", " & ",
      formatC(panel_borda$signal_correlation_pooled_favor_male[row_index], digits = 3, format = "f"), " & ",
      "", " \\\\"
    )
  )
}

# Append latex footer
latex_lines_ols_borda <- c(
  latex_lines_ols_borda,
  "    \\bottomrule",
  "  \\end{tabular}"
)

# Define output path for OLS-Borda latex table
out_tex_ols_borda <- file.path(tables, "cross_sample_signal_corr_ols_borda.tex")

# Write latex table to disk
writeLines(latex_lines_ols_borda, out_tex_ols_borda)

# Print output path
message("🎃 Generated cross_sample_signal_corr_ols_borda.tex")
