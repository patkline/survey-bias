# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Cross-sample signal correlation table (Table 4) --- for each respondent split, the debiased
# correlation between the two subgroups' firm-level belief estimates, alongside a Wald test of belief
# equality
#
# Created: Jordan Cammarota
# Cleaned: Nico Rotundo 2026-06-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Define the respondent splits compared in the table, each with its two file-suffix sample names and its
# display label, in table-row order
# -----------------------------------------------------------------------------------------------------------------------------
sample_pair_list <- list(
    list(sample_1 = "Black",                   sample_2 = "White",                   row_label = "Black vs White"),
    list(sample_1 = "Female",                  sample_2 = "Male",                    row_label = "Female vs Male"),
    list(sample_1 = "Looking",                 sample_2 = "Not_Looking",             row_label = "Looking for a Job vs Not"),
    list(sample_1 = "Feared_Discrimination_1", sample_2 = "Feared_Discrimination_0", row_label = "Feared Discrimination vs Not"),
    list(sample_1 = "Age_gte40",               sample_2 = "Age_lt40",                row_label = "Age $>=$ 40 vs $<$ 40"),
    list(sample_1 = "College",                 sample_2 = "No_College",              row_label = "At Least Some College vs HS Diploma or less"),
    list(sample_1 = "Convenience",             sample_2 = "Probability",             row_label = "Convenience vs Probability"),
    list(sample_1 = "Conf_Gender_Y",           sample_2 = "Conf_Gender_N",           row_label = "Confident vs Not (Gender)"),
    list(sample_1 = "Conf_Race_Y",             sample_2 = "Conf_Race_N",             row_label = "Confident vs Not (Race)")
)

# Define vector to store every sample name
sample_vector <- character(0)

# Loop over each sample pair, and append the two sample names to the vector
for (sample_pair in sample_pair_list) {
    sample_vector <- c(sample_vector, sample_pair$sample_1, sample_pair$sample_2)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of firm-level belief estimates by sample i.e., the vectors correlated across subgroups
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store every sample's firm-level beliefs
aggregated_sample_beliefs <- data.frame()

# Loop over each sample
for (sample_name in sample_vector) {

    # Load the given sample's firm-level coefficient sheet
    sample_beliefs <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", sample_name)), "Coefficients")

    # Uniquely identified by subset x aggregation model x belief measure x entity type x entity, none missing
    stopifnot(!anyDuplicated(sample_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(sample_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]))

    # Keep firm-level observations
    sample_beliefs <- sample_beliefs |> dplyr::filter(entity_type == "Firm")

    # Keep the full-sample estimates
    sample_beliefs <- sample_beliefs |> dplyr::filter(subset == "all")

    # Keep OLS and Borda observations
    sample_beliefs <- sample_beliefs |> dplyr::filter(model %in% c("OLS", "Borda"))

    # Keep just the two pooled belief measures
    sample_beliefs <- sample_beliefs |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Should be 164 firms x 2 aggregation methods x 2 belief measures = 656 observations remaining
    stopifnot(nrow(sample_beliefs) == 164 * 2 * 2)

    # Keep necessary variables
    sample_beliefs <- sample_beliefs |> dplyr::select(entity_id, model, outcome, estimate)

    # Rename variables to be more descriptive
    sample_beliefs <- sample_beliefs |> dplyr::rename(firm_id = entity_id, aggregation_method = model, belief_measure = outcome, belief_estimate = estimate)

    # Define a variable to indicate the sample
    sample_beliefs <- sample_beliefs |> dplyr::mutate(sample = sample_name)

    # Place the sample variable at the beginning of the dataset
    sample_beliefs <- sample_beliefs |> dplyr::select(sample, dplyr::everything())

    # Append the sample beliefs to the aggregated dataframe
    aggregated_sample_beliefs <- rbind(aggregated_sample_beliefs, sample_beliefs)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of the Katz-corrected signal variance by sample i.e., the signal correlation denominator
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store every sample's signal variance
aggregated_sample_signal_variance <- data.frame()

# Loop over each sample
for (sample_name in sample_vector) {

    # Load in given sample's signal variance sheet
    sample_signal_variance <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", sample_name)), "variance")

    # Uniquely identified by subset x aggregation model x outcome, none missing
    stopifnot(!anyDuplicated(sample_signal_variance[c("subset", "model", "outcome")]), !anyNA(sample_signal_variance[c("subset", "model", "outcome")]))

    # Keep OLS and Borda observations
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(model %in% c("OLS", "Borda"))

    # Keep just the two pooled belief measures
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Keep the full firm sample
    sample_signal_variance <- sample_signal_variance |> dplyr::filter(subset == "all")

    # Should be 2 aggregation methods x 2 belief measures = 4 observations remaining
    stopifnot(nrow(sample_signal_variance) == 4)

    # Keep necessary variables
    sample_signal_variance <- sample_signal_variance |> dplyr::select(model, outcome, signal)

    # Rename variables to be more descriptive
    sample_signal_variance <- sample_signal_variance |> dplyr::rename(aggregation_method = model, belief_measure = outcome, katz_corrected_signal_variance_across_firms = signal)

    # Define a variable to indicate the sample
    sample_signal_variance <- sample_signal_variance |> dplyr::mutate(sample = sample_name)

    # Place the sample variable at the beginning of the dataset
    sample_signal_variance <- sample_signal_variance |> dplyr::select(sample, dplyr::everything())

    # Append the sample signal variance to the aggregated dataframe
    aggregated_sample_signal_variance <- rbind(aggregated_sample_signal_variance, sample_signal_variance)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Compute the firm-level constants shared across all splits
# -----------------------------------------------------------------------------------------------------------------------------
# Firm ids sorted ascending, the order every belief vector and covariance matrix is aligned to
firm_id_vector <- sort(unique(aggregated_sample_beliefs$firm_id))

# Should be the 164 firms
stopifnot(length(firm_id_vector) == 164)

# -----------------------------------------------------------------------------------------------------------------------------
# For each split, aggregation method, and belief measure, compute the debiased signal correlation and the
# Wald test of belief equality across the two subsamples
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all correlation results
aggregated_correlation_results <- data.frame()

# Loop over each split
for (sample_pair in sample_pair_list) {
    # Loop over aggregation method
    for (aggregation_method_value in c("OLS", "Borda")) {
        # Loop over each belief measure
        for (belief_measure_value in c("pooled_favor_white", "pooled_favor_male")) {

            #### Collect each subsample's inputs
            # Store each subsample's belief vector, signal variance, and robust covariance matrix
            belief_vector_by_subsample <- list()
            signal_variance_by_subsample <- list()
            robust_covariance_matrix_by_subsample <- list()

            # Loop over the two subsamples of the split
            for (subsample in c(sample_pair$sample_1, sample_pair$sample_2)) {

                # Keep this subsample's firm-level beliefs
                subsample_beliefs <- aggregated_sample_beliefs |> dplyr::filter(sample == subsample)

                # Keep this aggregation method
                subsample_beliefs <- subsample_beliefs |> dplyr::filter(aggregation_method == aggregation_method_value)

                # Keep this belief measure
                subsample_beliefs <- subsample_beliefs |> dplyr::filter(belief_measure == belief_measure_value)

                # Sort by firm_id to align with firm_id_vector
                subsample_beliefs <- subsample_beliefs |> dplyr::arrange(firm_id)

                # Check the belief firm_id order equals firm_id_vector
                stopifnot(all(subsample_beliefs$firm_id == firm_id_vector))

                # Store this subsample's belief vector
                belief_vector_by_subsample[[subsample]] <- subsample_beliefs$belief_estimate

                # Keep this subsample's signal variance
                subsample_signal_variance <- aggregated_sample_signal_variance |> dplyr::filter(sample == subsample)

                # Keep this aggregation method
                subsample_signal_variance <- subsample_signal_variance |> dplyr::filter(aggregation_method == aggregation_method_value)

                # Keep this belief measure
                subsample_signal_variance <- subsample_signal_variance |> dplyr::filter(belief_measure == belief_measure_value)

                # Should be a single row
                stopifnot(nrow(subsample_signal_variance) == 1)

                # Store this subsample's Katz-corrected signal variance
                signal_variance_by_subsample[[subsample]] <- subsample_signal_variance$katz_corrected_signal_variance_across_firms

                # Load this subsample's robust covariance sheet
                subsample_robust_covariance <- arrow::open_dataset(parquet_sheet_path(file.path(intermediate, paste0("Subset_", subsample)), "rcov"))

                # Keep the full firm sample
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(subset == "all")

                # Keep this aggregation method
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(model == aggregation_method_value)

                # Keep this belief measure
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::filter(outcome == belief_measure_value)

                # Keep necessary variables
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::select(entity_id_i, entity_id_j, rcov)

                # Collect the filtered robust covariance rows
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::collect()

                # Should be 164 firms x 164 firms = 26896 firm pairs
                stopifnot(nrow(subsample_robust_covariance) == 164 * 164)

                # Firm-pair identifiers should uniquely identify the filtered robust covariance rows
                stopifnot(!anyDuplicated(subsample_robust_covariance[c("entity_id_i", "entity_id_j")]))

                # Firm-pair identifiers should be non-missing
                stopifnot(!anyNA(subsample_robust_covariance[c("entity_id_i", "entity_id_j")]))

                # Rename variables to be more descriptive
                subsample_robust_covariance <- subsample_robust_covariance |> dplyr::rename(firm_id_i = entity_id_i, firm_id_j = entity_id_j, robust_covariance = rcov)

                # Define a matrix of 0s to hold this subsample's robust covariance, rows and columns ordered by firm_id_vector
                firm_robust_covariance_matrix <- matrix(0, nrow = length(firm_id_vector), ncol = length(firm_id_vector), dimnames = list(as.character(firm_id_vector), as.character(firm_id_vector)))

                # Populate the robust covariance matrix from the firm-pair rows
                firm_robust_covariance_matrix[cbind(as.character(subsample_robust_covariance$firm_id_i), as.character(subsample_robust_covariance$firm_id_j))] <- subsample_robust_covariance$robust_covariance

                # Store this subsample's robust covariance matrix
                robust_covariance_matrix_by_subsample[[subsample]] <- firm_robust_covariance_matrix

            }

            #### Unweighted signal correlation
            # Center each subsample's beliefs at their unweighted mean
            belief_sample_1_centered <- belief_vector_by_subsample[[sample_pair$sample_1]] - mean(belief_vector_by_subsample[[sample_pair$sample_1]])
            belief_sample_2_centered <- belief_vector_by_subsample[[sample_pair$sample_2]] - mean(belief_vector_by_subsample[[sample_pair$sample_2]])

            # Population covariance between the two subsamples' beliefs
            belief_covariance <- mean(belief_sample_1_centered * belief_sample_2_centered)

            # Unweighted signal correlation
            signal_correlation <- belief_covariance / sqrt(signal_variance_by_subsample[[sample_pair$sample_1]] * signal_variance_by_subsample[[sample_pair$sample_2]])

            #### Wald test of belief equality
            # Belief difference across the two subsamples
            belief_difference <- belief_vector_by_subsample[[sample_pair$sample_1]] - belief_vector_by_subsample[[sample_pair$sample_2]]

            # Summed robust covariance under independence of the two samples
            summed_robust_covariance_matrix <- robust_covariance_matrix_by_subsample[[sample_pair$sample_1]] + robust_covariance_matrix_by_subsample[[sample_pair$sample_2]]

            # Wald statistic, chi-square with J - 1 degrees of freedom (sum-to-zero centering removes one direction)
            wald_statistic <- as.numeric(t(belief_difference) %*% MASS::ginv(summed_robust_covariance_matrix) %*% belief_difference)
            wald_degrees_of_freedom <- length(firm_id_vector) - 1
            wald_p_value <- pchisq(wald_statistic, df = wald_degrees_of_freedom, lower.tail = FALSE)

            #### Classical minimum distance test of perfect correlation
            # Compute minimum distance weight matrix for each subsample --- Moore-Penrose inverse of the robust covariance
            minimum_distance_weight_matrix_sample_1 <- MASS::ginv(robust_covariance_matrix_by_subsample[[sample_pair$sample_1]])
            minimum_distance_weight_matrix_sample_2 <- MASS::ginv(robust_covariance_matrix_by_subsample[[sample_pair$sample_2]])

            # Run and store OLS intercept and slope to initialize the minimum distance search
            minimum_distance_starting_intercept_and_slope <- coef(lm(belief_vector_by_subsample[[sample_pair$sample_1]] ~ belief_vector_by_subsample[[sample_pair$sample_2]]))

            # Remove names from the starting intercept and slope
            minimum_distance_starting_intercept_and_slope <- unname(minimum_distance_starting_intercept_and_slope)
            
            # Define a function that computes distance between observed beliefs and perfect-fit beliefs for a trial intercept and slope
            compute_observed_to_perfect_fit_belief_distance <- function(intercept_and_slope) {
              # Assign the intercept 
              perfect_fit_intercept <- intercept_and_slope[1]
              
              # Assign the slope
              perfect_fit_slope <- intercept_and_slope[2]

              # Estimate latent sample_2 beliefs conditional on the trial intercept and slope
              perfect_fit_belief_sample_2 <- MASS::ginv(perfect_fit_slope^2 * minimum_distance_weight_matrix_sample_1 +
              minimum_distance_weight_matrix_sample_2) %*% (
                perfect_fit_slope * minimum_distance_weight_matrix_sample_1 %*%
                (belief_vector_by_subsample[[sample_pair$sample_1]] - perfect_fit_intercept) +
                minimum_distance_weight_matrix_sample_2 %*% belief_vector_by_subsample[[sample_pair$sample_2]]
              )

              # Convert one-column matrix to a vector
              perfect_fit_belief_sample_2 <- as.numeric(perfect_fit_belief_sample_2)

              # Estimate latent sample_1 beliefs implied by the trial intercept and slope
              perfect_fit_belief_sample_1 <- perfect_fit_intercept + perfect_fit_slope * perfect_fit_belief_sample_2

              # Compute the difference between observed and estimated sample_1 beliefs
              diff_observed_perfect_fit_belief_sample_1 <- belief_vector_by_subsample[[sample_pair$sample_1]] - perfect_fit_belief_sample_1

              # Compute the difference between observed and estimated sample_2 beliefs
              diff_observed_perfect_fit_belief_sample_2 <- belief_vector_by_subsample[[sample_pair$sample_2]] - perfect_fit_belief_sample_2

              # Store the minimum distance criterion value for the trial intercept and slope
              as.numeric(
                t(diff_observed_perfect_fit_belief_sample_1) %*% minimum_distance_weight_matrix_sample_1 %*% diff_observed_perfect_fit_belief_sample_1 +
                t(diff_observed_perfect_fit_belief_sample_2) %*% minimum_distance_weight_matrix_sample_2 %*% diff_observed_perfect_fit_belief_sample_2
              )
            }

            # Estimate the perfect-fit line by minimizing observed-to-perfect-fit belief distance
            minimum_distance_perfect_fit_intercept_and_slope <- optim(
              par = minimum_distance_starting_intercept_and_slope,
              fn = compute_observed_to_perfect_fit_belief_distance,
              method = "BFGS"
            )

            # Store the minimum observed-to-perfect-fit belief distance
            minimum_distance_statistic <- minimum_distance_perfect_fit_intercept_and_slope$value

            # Store degrees of freedom for the perfect-correlation test
            minimum_distance_degrees_of_freedom <- length(firm_id_vector) - 2

            # Compute p-value for the perfect-correlation test
            minimum_distance_p_value <- pchisq(minimum_distance_statistic, df = minimum_distance_degrees_of_freedom, lower.tail = FALSE)

            # Append this cell's results
            aggregated_correlation_results <- rbind(aggregated_correlation_results, data.frame(
              row_label = sample_pair$row_label,
              aggregation_method = tolower(aggregation_method_value),
              belief_measure = belief_measure_value,
              signal_correlation = signal_correlation,
              wald_p_value = wald_p_value,
              minimum_distance_p_value = minimum_distance_p_value
            ))
        }
    }
}

# Minimum distance p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$minimum_distance_p_value))

# Minimum distance p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$minimum_distance_p_value, 0, 1)))

# Number of firms in each sample x aggregation-method x belief-measure cell
number_of_firms_by_sample_cell <- aggregated_sample_beliefs |> dplyr::count(sample, aggregation_method, belief_measure, name = "number_of_firms")

# Each sample cell should contain 164 firms
stopifnot(all(number_of_firms_by_sample_cell$number_of_firms == 164))

# Number of firms reported in the table
number_of_firms_in_table <- unique(number_of_firms_by_sample_cell$number_of_firms)

# There should be a single number of firms across all table cells
stopifnot(length(number_of_firms_in_table) == 1)

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the cross-sample correlation table, with an OLS/Likert panel and a Borda panel
# -----------------------------------------------------------------------------------------------------------------------------
# Open the table
latex_lines <- c(
    "  \\centering",
    "  \\begin{tabular}{lcccccc}",
    "    \\toprule",
    "    & \\multicolumn{3}{c}{Discrimination Black} & \\multicolumn{3}{c}{Discrimination Female} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "    & Corr & Wald p-value & CMD p-value & Corr & Wald p-value & CMD p-value \\\\",
    "    & & $H_0: \\theta_1 = \\theta_2$ & $H_0: \\rho = 1$ & & $H_0: \\theta_1 = \\theta_2$ & $H_0: \\rho = 1$ \\\\",
    "    \\midrule"
)

# Loop over the two aggregation-method panels
for (panel in list(list(aggregation_method = "ols", panel_label = "Panel A: Likert"), list(aggregation_method = "borda", panel_label = "Panel B: Borda"))) {

    # Separate the Borda panel from the Likert panel
    if (panel$aggregation_method == "borda") {
        latex_lines <- c(latex_lines, "    \\addlinespace")
    }

    # Panel header
    latex_lines <- c(latex_lines, paste0("    \\multicolumn{7}{l}{\\textbf{", panel$panel_label, "}}\\\\"))

    # Loop over the splits in table-row order
    for (sample_pair in sample_pair_list) {

        # This split's Discrimination Black (white-favoritism belief) results for this panel
        race_results <- aggregated_correlation_results |> dplyr::filter(row_label == sample_pair$row_label)
        race_results <- race_results |> dplyr::filter(aggregation_method == panel$aggregation_method)
        race_results <- race_results |> dplyr::filter(belief_measure == "pooled_favor_white")
        stopifnot(nrow(race_results) == 1)

        # This split's Discrimination Female (male-favoritism belief) results for this panel
        gender_results <- aggregated_correlation_results |> dplyr::filter(row_label == sample_pair$row_label)
        gender_results <- gender_results |> dplyr::filter(aggregation_method == panel$aggregation_method)
        gender_results <- gender_results |> dplyr::filter(belief_measure == "pooled_favor_male")
        stopifnot(nrow(gender_results) == 1)

        # Table row: split label, race correlation and p-values, gender correlation and p-values
        latex_lines <- c(latex_lines, paste0("    ", sample_pair$row_label, " & ", formatC(race_results$signal_correlation, digits = 3, format = "f"), " & ", formatC(race_results$wald_p_value, digits = 3, format = "f"), " & ", formatC(race_results$minimum_distance_p_value, digits = 3, format = "f"), " & ", formatC(gender_results$signal_correlation, digits = 3, format = "f"), " & ", formatC(gender_results$wald_p_value, digits = 3, format = "f"), " & ", formatC(gender_results$minimum_distance_p_value, digits = 3, format = "f"), " \\\\"))
    }
}

# Add number of firms row
latex_lines <- c(latex_lines, "    \\midrule", paste0("    N & ", number_of_firms_in_table, " &  &  & ", number_of_firms_in_table, " &  &  \\\\"))

# Close the table
latex_lines <- c(latex_lines, "    \\bottomrule", "  \\end{tabular}")

# Write the table
writeLines(latex_lines, file.path(tables, "cross_sample_signal_corr_ols_borda.tex"), useBytes = TRUE)

# Announce the written table
message("🎃 Generated cross_sample_signal_corr_ols_borda.tex")
