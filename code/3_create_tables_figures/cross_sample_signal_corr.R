# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Cross-sample signal correlation table --- for each respondent split, the debiased
# correlation between the two subgroups' firm-level belief estimates, a Wald test of belief equality, and a
# minimum distance test of perfect correlation --- plus the minimum distance scatterplots for the race and
# gender splits, with animation stages for the race split
#
# Created: Jordan Cammarota
# Edited: Nico Rotundo 2026-06-29
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

    # Keep the non-recentered OLS and Borda observations i.e., the raw firm-level beliefs
    sample_beliefs <- sample_beliefs |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

    # Keep just the two pooled belief measures
    sample_beliefs <- sample_beliefs |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Should be 164 firms x 2 aggregation methods x 2 belief measures = 656 observations remaining
    stopifnot(nrow(sample_beliefs) == 164 * 2 * 2)

    # Belief estimates should be non-missing
    stopifnot(!anyNA(sample_beliefs$estimate))

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
# For each split, aggregation method, and belief measure, compute the debiased signal correlation, the Wald
# test of belief equality across the two subsamples, and the minimum distance test of perfect correlation
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all correlation results
aggregated_correlation_results <- data.frame()

# Define dataframe to store observed beliefs and minimum-distance estimates
minimum_distance_observed_belief_data <- data.frame()

# Loop over each split
for (sample_pair in sample_pair_list) {
    # Loop over aggregation method, using the non-recentered i.e., raw belief estimates
    for (aggregation_method_value in c("OLS_not_recentered", "Borda_not_recentered")) {
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

                # Check one belief row per firm
                stopifnot(nrow(subsample_beliefs) == length(firm_id_vector))

                # Check the belief firm_id order equals firm_id_vector
                stopifnot(all(subsample_beliefs$firm_id == firm_id_vector))

                # Store this subsample's belief vector
                belief_vector_by_subsample[[subsample]] <- subsample_beliefs$belief_estimate

                # Keep this subsample's signal variance
                subsample_signal_variance <- aggregated_sample_signal_variance |> dplyr::filter(sample == subsample)

                # Keep this aggregation method, dropping the suffix since the signal variance is identical across the recentered and non-recentered estimates
                subsample_signal_variance <- subsample_signal_variance |> dplyr::filter(aggregation_method == sub("_not_recentered$", "", aggregation_method_value))

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

            # Require the summed covariance to be full rank so the Wald statistic has J degrees of freedom
            stopifnot(qr(summed_robust_covariance_matrix)$rank == length(firm_id_vector))

            # Wald statistic, chi-square with J degrees of freedom
            wald_statistic <- as.numeric(t(belief_difference) %*% solve(summed_robust_covariance_matrix) %*% belief_difference)
            wald_degrees_of_freedom <- length(firm_id_vector)
            wald_p_value <- pchisq(wald_statistic, df = wald_degrees_of_freedom, lower.tail = FALSE)

            #### Classical minimum distance test of perfect correlation
            # Require each subsample's robust covariance to be full rank so its inverse is a valid minimum distance weight matrix
            stopifnot(qr(robust_covariance_matrix_by_subsample[[sample_pair$sample_1]])$rank == length(firm_id_vector))
            stopifnot(qr(robust_covariance_matrix_by_subsample[[sample_pair$sample_2]])$rank == length(firm_id_vector))

            # Compute minimum distance weight matrix for each subsample --- inverse of the robust covariance
            minimum_distance_weight_matrix_sample_1 <- solve(robust_covariance_matrix_by_subsample[[sample_pair$sample_1]])
            minimum_distance_weight_matrix_sample_2 <- solve(robust_covariance_matrix_by_subsample[[sample_pair$sample_2]])

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
              perfect_fit_belief_sample_2 <- solve(perfect_fit_slope^2 * minimum_distance_weight_matrix_sample_1 +
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

            # Require the minimization to have converged
            stopifnot(minimum_distance_perfect_fit_intercept_and_slope$convergence == 0)

            # Require a positive fitted slope, so the perfect-fit line tests rho = +1 rather than rho = -1
            stopifnot(minimum_distance_perfect_fit_intercept_and_slope$par[2] > 0)

            # Store the minimum observed-to-perfect-fit belief distance
            minimum_distance_statistic <- minimum_distance_perfect_fit_intercept_and_slope$value

            # Store degrees of freedom for the perfect-correlation test
            minimum_distance_degrees_of_freedom <- length(firm_id_vector) - 2

            # Compute p-value for the perfect-correlation test
            minimum_distance_p_value <- pchisq(minimum_distance_statistic, df = minimum_distance_degrees_of_freedom, lower.tail = FALSE)

            # Append this cell's cross sample correlation results
            aggregated_correlation_results <- rbind(aggregated_correlation_results, data.frame(
              row_label = sample_pair$row_label,
              aggregation_method = tolower(aggregation_method_value),
              belief_measure = belief_measure_value,
              signal_correlation = signal_correlation,
              wald_p_value = wald_p_value,
              minimum_distance_p_value = minimum_distance_p_value
            ))

            #### Minimum distance scatterplot data
            # Store the minimum distance intercept
            minimum_distance_intercept <- minimum_distance_perfect_fit_intercept_and_slope$par[1]

            # Store the minimum distance slope
            minimum_distance_slope <- minimum_distance_perfect_fit_intercept_and_slope$par[2]

            # Back out sample_2 beliefs implied by the final minimum distance intercept and slope
            minimum_distance_implied_belief_sample_2 <- solve(minimum_distance_slope^2 * minimum_distance_weight_matrix_sample_1 +
            minimum_distance_weight_matrix_sample_2) %*% (
              minimum_distance_slope * minimum_distance_weight_matrix_sample_1 %*%
              (belief_vector_by_subsample[[sample_pair$sample_1]] - minimum_distance_intercept) +
              minimum_distance_weight_matrix_sample_2 %*% belief_vector_by_subsample[[sample_pair$sample_2]]
            )

            # Convert one-column matrix to a vector
            minimum_distance_implied_belief_sample_2 <- as.numeric(minimum_distance_implied_belief_sample_2)

            # Back out sample_1 beliefs implied by the final minimum distance intercept and slope
            minimum_distance_implied_belief_sample_1 <- minimum_distance_intercept + minimum_distance_slope * minimum_distance_implied_belief_sample_2

            # Append this cell's observed beliefs and minimum-distance estimates
            minimum_distance_observed_belief_data <- rbind(minimum_distance_observed_belief_data, data.frame(
              row_label = sample_pair$row_label,
              sample_1 = sample_pair$sample_1,
              sample_2 = sample_pair$sample_2,
              aggregation_method = tolower(aggregation_method_value),
              belief_measure = belief_measure_value,
              firm_id = firm_id_vector,
              belief_sample_1 = belief_vector_by_subsample[[sample_pair$sample_1]],
              belief_sample_2 = belief_vector_by_subsample[[sample_pair$sample_2]],
              minimum_distance_implied_belief_sample_1 = minimum_distance_implied_belief_sample_1,
              minimum_distance_implied_belief_sample_2 = minimum_distance_implied_belief_sample_2,
              minimum_distance_intercept = minimum_distance_intercept,
              minimum_distance_slope = minimum_distance_slope,
              signal_correlation = signal_correlation,
              wald_p_value = wald_p_value,
              minimum_distance_p_value = minimum_distance_p_value
            ))
        }
    }
}

# Signal correlations should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$signal_correlation))

# Wald p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$wald_p_value))

# Wald p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$wald_p_value, 0, 1)))

# Minimum distance p-values should be non-missing
stopifnot(!anyNA(aggregated_correlation_results$minimum_distance_p_value))

# Minimum distance p-values should be between zero and one
stopifnot(all(dplyr::between(aggregated_correlation_results$minimum_distance_p_value, 0, 1)))

# Minimum distance observed belief data should have one row per firm in each split x aggregation-method x belief-measure cell
stopifnot(nrow(minimum_distance_observed_belief_data) == length(sample_pair_list) * 2 * 2 * length(firm_id_vector))

# Minimum distance observed belief data should have non-missing observed beliefs and minimum-distance estimates
stopifnot(!anyNA(minimum_distance_observed_belief_data[c("belief_sample_1", "belief_sample_2", "minimum_distance_intercept", "minimum_distance_slope")]))

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
# Format a p-value to three decimals, printing values below 0.001 as $<$0.001
format_p_value <- function(p_value) {
  if (p_value < 0.001) "$<$0.001" else formatC(p_value, digits = 3, format = "f")
}

# Open the table
latex_lines <- c(
    "  \\centering",
    "  \\begin{tabular}{lcccccc}",
    "    \\toprule",
    "    & \\multicolumn{3}{c}{Discrimination Black (Pooled)} & \\multicolumn{3}{c}{Discrimination Female (Pooled)} \\\\",
    "    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "    & Corr & Wald p-value & CMD p-value & Corr & Wald p-value & CMD p-value \\\\",
    "    & & $H_0: \\theta_1 = \\theta_2$ & $H_0: \\rho = 1$ & & $H_0: \\theta_1 = \\theta_2$ & $H_0: \\rho = 1$ \\\\",
    "    \\midrule"
)

# Loop over the two aggregation-method panels
for (panel in list(list(aggregation_method = "ols_not_recentered", panel_label = "Panel A: Likert"), list(aggregation_method = "borda_not_recentered", panel_label = "Panel B: Borda"))) {

    # Separate the Borda panel from the Likert panel
    if (panel$aggregation_method == "borda_not_recentered") {
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
        latex_lines <- c(latex_lines, paste0("    ", sample_pair$row_label, " & ", formatC(race_results$signal_correlation, digits = 3, format = "f"), " & ", format_p_value(race_results$wald_p_value), " & ", format_p_value(race_results$minimum_distance_p_value), " & ", formatC(gender_results$signal_correlation, digits = 3, format = "f"), " & ", format_p_value(gender_results$wald_p_value), " & ", format_p_value(gender_results$minimum_distance_p_value), " \\\\"))
    }
}

# Add number of firms row
latex_lines <- c(latex_lines, "    \\midrule", paste0("    N & ", number_of_firms_in_table, " &  &  & ", number_of_firms_in_table, " &  &  \\\\"))

# Close the table
latex_lines <- c(latex_lines, "    \\bottomrule", "  \\end{tabular}")

# Write the table
writeLines(latex_lines, file.path(tables, "cross_sample_signal_corr_ols_borda_not_recentered.tex"), useBytes = TRUE)

# Announce the written table
message("🎃 Generated cross_sample_signal_corr_ols_borda_not_recentered.tex")

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the minimum distance fitted scatterplots
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each subgroup comparison
for (subgroup_comparison in list(c("Black", "White"), c("Female", "Male"))) {
    # Loop over aggregation method
    for (aggregation_method in c("ols_not_recentered")) {
        # Loop over the belief measure on the comparison's own demographic margin
        for (belief_measure in c(Black = "pooled_favor_white", Female = "pooled_favor_male")[[subgroup_comparison[1]]]) {

            # Restrict observed belief data to the given subgroup comparison
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_data |> dplyr::filter(sample_1 == subgroup_comparison[1], sample_2 == subgroup_comparison[2])

            # Keep this aggregation method
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_scatterplot_data |> dplyr::filter(aggregation_method == .env$aggregation_method)

            # Keep this belief measure
            minimum_distance_observed_belief_scatterplot_data <- minimum_distance_observed_belief_scatterplot_data |> dplyr::filter(belief_measure == .env$belief_measure)

            # Should be one row per firm
            stopifnot(nrow(minimum_distance_observed_belief_scatterplot_data) == length(firm_id_vector))

            # Should be one minimum-distance intercept for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept)) == 1)

            # Should be one minimum-distance slope for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope)) == 1)

            # Should be one signal correlation for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$signal_correlation)) == 1)

            # Should be one Wald p-value for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value)) == 1)

            # Should be one minimum-distance p-value for this scatterplot
            stopifnot(length(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value)) == 1)

            # Define scatterplot of observed beliefs i.e., the first animation stage
            minimum_distance_observed_belief_scatterplot <- ggplot(minimum_distance_observed_belief_scatterplot_data, aes(x = belief_sample_2, y = belief_sample_1)) +

                # Observed firm-level belief pairs
                geom_point(color = "darkorange", size = 2.2, alpha = 0.7) +

                # Axis labels name the belief measure and subsample
                labs(
                    x = paste0(c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)")[[belief_measure]], " for ", subgroup_comparison[2]),
                    y = paste0(c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)")[[belief_measure]], " for ", subgroup_comparison[1])
                ) +

                # Theme baseline
                theme_minimal(base_size = 11) +

                # Theme adjustments
                theme(
                    # No grid lines
                    panel.grid = element_blank(),

                    # White background
                    panel.background = element_rect(fill = "white", color = NA),
                    plot.background = element_rect(fill = "white", color = NA),

                    # Bottom and left axis spines, no ticks
                    axis.line = element_line(color = "black"),
                    axis.ticks = element_blank()
                )

            # Add the minimum-distance fit to the observed scatterplot i.e., the second animation stage
            minimum_distance_fitted_belief_scatterplot <- minimum_distance_observed_belief_scatterplot +

                # Minimum-distance fitted line
                geom_abline(
                    intercept = unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept),
                    slope = unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope),
                    color = "steelblue",
                    linewidth = 0.7
                ) +

                # Minimum-distance implied firm-level belief pairs
                geom_point(aes(x = minimum_distance_implied_belief_sample_2, y = minimum_distance_implied_belief_sample_1), color = "steelblue", size = 1.4, alpha = 0.9)

            # Add the statistic annotations to the fitted scatterplot i.e., the full figure
            minimum_distance_annotated_belief_scatterplot <- minimum_distance_fitted_belief_scatterplot +

                # Minimum-distance intercept and slope annotation
                annotation_custom(
                    grid::textGrob(
                        label = bquote("CMD (intercept, slope)" == group("(", list(.(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_intercept), digits = 3, format = "f")), .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_slope), digits = 3, format = "f"))), ")")),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.975, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Minimum-distance p-value annotation
                annotation_custom(
                    grid::textGrob(
                        label = if (unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value) < 0.001) bquote("CMD p-value"~(H[0]:~rho == 1) < 0.001) else bquote("CMD p-value"~(H[0]:~rho == 1) == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$minimum_distance_p_value), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.930, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Signal correlation annotation
                annotation_custom(
                    grid::textGrob(
                        label = bquote("Signal corr." == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$signal_correlation), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.885, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                ) +

                # Wald p-value annotation
                annotation_custom(
                    grid::textGrob(
                        label = if (unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value) < 0.001) bquote("Wald p-value"~(H[0]:~theta[1] == theta[2]) < 0.001) else bquote("Wald p-value"~(H[0]:~theta[1] == theta[2]) == .(formatC(unique(minimum_distance_observed_belief_scatterplot_data$wald_p_value), digits = 3, format = "f"))),
                        x = grid::unit(0.015, "npc"),
                        y = grid::unit(0.840, "npc"),
                        hjust = 0,
                        vjust = 1,
                        gp = grid::gpar(fontsize = 11)
                    )
                )

            # Export the scatterplot, one file per subgroup comparison x aggregation method x belief measure
            ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, ".png")), plot = minimum_distance_annotated_belief_scatterplot, width = 10, height = 6, dpi = 300, bg = "white")

            # Export the animation stages for the race comparison, pinning both stages to the full figure's axis ranges
            if (identical(subgroup_comparison, c("Black", "White"))) {

                # Extract the full figure's panel ranges, fixing the axes across the animation stages
                minimum_distance_scatterplot_panel_ranges <- ggplot_build(minimum_distance_annotated_belief_scatterplot)$layout$panel_params[[1]]

                # Export the observed-scatter stage
                ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, "_animation_1.png")), plot = minimum_distance_observed_belief_scatterplot + coord_cartesian(xlim = minimum_distance_scatterplot_panel_ranges$x.range, ylim = minimum_distance_scatterplot_panel_ranges$y.range, expand = FALSE), width = 10, height = 6, dpi = 300, bg = "white")

                # Export the fitted-scatter stage
                ggsave(file.path(figures, paste0("cross_sample_signal_corr_minimum_distance_scatterplot_", tolower(subgroup_comparison[1]), "_vs_", tolower(subgroup_comparison[2]), "_", aggregation_method, "_", belief_measure, "_animation_2.png")), plot = minimum_distance_fitted_belief_scatterplot + coord_cartesian(xlim = minimum_distance_scatterplot_panel_ranges$x.range, ylim = minimum_distance_scatterplot_panel_ranges$y.range, expand = FALSE), width = 10, height = 6, dpi = 300, bg = "white")
            }
        }
    }
}
