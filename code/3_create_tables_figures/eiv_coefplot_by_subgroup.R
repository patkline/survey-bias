# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Coefplot of the subgroup-split EIV slopes --- one figure per pooled belief x aggregation method,
# grouping the No-Controls and Industry-FE specs within each subgroup comparison and testing the slope difference
#
# Created: Nico Rotundo 2026-06-13
# Edited: Nico Rotundo 2026-06-22
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Source the errors-in-variables estimator
source("code/2_analysis/eivreg.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Construct firm -> industry crosswalk from microdata 
# -----------------------------------------------------------------------------------------------------------------------------
# Load the survey microdata 
firm_industry_crosswalk <- read.csv(file.path(processed, "long_survey_final.csv"), stringsAsFactors = FALSE)

# Check unique identifier is respondent x firm and that these are never missing 
stopifnot(!anyDuplicated(firm_industry_crosswalk[c("ResponseId", "firm_id")]), !anyNA(firm_industry_crosswalk[c("ResponseId", "firm_id")]))

# Keep necessary variables 
firm_industry_crosswalk <- firm_industry_crosswalk |> dplyr::select(firm_id, aer_naics2)

# Assert no firm is missing industry code 
stopifnot(!any(is.na(firm_industry_crosswalk$aer_naics2)))

# Collapse to one row per firm-industry pair 
firm_industry_crosswalk <- firm_industry_crosswalk |> dplyr::distinct(firm_id, aer_naics2)

# Should be one row per firm i.e., each firm mapped to one industry
stopifnot(!anyDuplicated(firm_industry_crosswalk$firm_id), !anyNA(firm_industry_crosswalk$firm_id))

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of firm-level audit gaps from the correspondence paper i.e., the regression LHS
# -----------------------------------------------------------------------------------------------------------------------------
# Load the Full_Sample coefficient sheet (audit gaps are firm-level and identical across subgroups)
firm_audit_gaps <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "Coefficients")

# Uniquely identified by sample x aggregation model x outcome x entity type x entity
stopifnot(!anyDuplicated(firm_audit_gaps[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(firm_audit_gaps[c("subset", "model", "outcome", "entity_type", "entity_id")]))

# Restrict to firm-level observations from the correspondence paper
firm_audit_gaps <- firm_audit_gaps |> dplyr::filter(model == "EXPERIMENTAL")

# Keep just audit gaps for race and gender 
firm_audit_gaps <- firm_audit_gaps |> dplyr::filter(outcome %in% c("log_dif", "log_dif_gender"))

# Should be just firm-level observations
stopifnot(all(firm_audit_gaps$entity_type == "Firm"))

# Drop firms that are missing both audit gaps
firm_audit_gaps <- firm_audit_gaps |> dplyr::filter(!is.na(estimate))

# Should be 97 firms x 2 LHS variables = 194  observations remainning 
stopifnot(nrow(firm_audit_gaps) == 194)

# Keep necessary variables 
firm_audit_gaps <- firm_audit_gaps |> dplyr::select(entity_id, entity, outcome, estimate)

# Rename variables
firm_audit_gaps <- firm_audit_gaps |> dplyr::rename(firm_id = entity_id, firm_name = entity, contact_gap_type = outcome, difference_in_log_contact_rate = estimate)

# Relabel the contact-gap type to be more descriptive
firm_audit_gaps <- firm_audit_gaps |> dplyr::mutate(contact_gap_type = dplyr::recode(contact_gap_type, "log_dif" = "white_black", "log_dif_gender" = "male_female"))

# Should be unique by firm and contact gap type
stopifnot(!anyDuplicated(firm_audit_gaps[c("firm_id", "contact_gap_type")]), !anyNA(firm_audit_gaps[c("firm_id", "contact_gap_type")]))

# Reshape wide to one row per firm, one column per contact-gap type
firm_audit_gaps <- firm_audit_gaps |> tidyr::pivot_wider(id_cols = c(firm_id, firm_name), names_from = contact_gap_type, values_from = difference_in_log_contact_rate, names_prefix = "dif_log_contact_rate_")

# Should be one row per firm, none missing
stopifnot(!anyDuplicated(firm_audit_gaps$firm_id), !anyNA(firm_audit_gaps))

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of firm-level beliefs by subgroup i.e., the regression RHS
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all subgroup beliefs 
aggregated_subgroup_beliefs <- data.frame()

# Loop over subgroup 
for (subgroup in c("White", "Black", "Female", "Male", "Looking", "Not_Looking", "Feared_Discrimination_1", "Feared_Discrimination_0", "Age_gte40", "Age_lt40")) {
    # Load in given subgroup's firm-level data 
    subgroup_beliefs <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", subgroup)), "Coefficients")

    # Uniquely identified by sample x aggregation model x outcome x entity type x entity, none missing
    stopifnot(!anyDuplicated(subgroup_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(subgroup_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]))

    # Keep OLS and Borda observations 
    subgroup_beliefs <- subgroup_beliefs |> dplyr::filter(model %in% c("OLS", "Borda"))

    # Keep just the two pooled belief measures
    subgroup_beliefs <- subgroup_beliefs |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Keep just the 97-firm sample from the correspondence paper
    subgroup_beliefs <- subgroup_beliefs |> dplyr::filter(subset == "subset97")

    # Should be just firm-level observations
    stopifnot(all(subgroup_beliefs$entity_type == "Firm"))

    # Should be 97 firms x 2 models x 2 RHS variables = 388 observations remaining
    stopifnot(nrow(subgroup_beliefs) == 388)

    # Keep necessary variables 
    subgroup_beliefs <- subgroup_beliefs |> dplyr::select(entity_id, model, outcome, estimate, njobs)

    # Rename variables to be more descriptive
    subgroup_beliefs <- subgroup_beliefs |> dplyr::rename(firm_id = entity_id, aggregation_method = model, belief_measure = outcome, belief_estimate = estimate, number_of_jobs = njobs)

    # Check that number of jobs is constant within each firm
    stopifnot(nrow(dplyr::distinct(subgroup_beliefs, firm_id, number_of_jobs)) == dplyr::n_distinct(subgroup_beliefs$firm_id))

    # Lowercase the aggregation method for the wide column suffix
    subgroup_beliefs <- subgroup_beliefs |> dplyr::mutate(aggregation_method = tolower(aggregation_method))

    # Reshape wide to one row per firm, one column per belief measure x aggregation method
    subgroup_beliefs <- subgroup_beliefs |> tidyr::pivot_wider(id_cols = c(firm_id, number_of_jobs), names_from = c(belief_measure, aggregation_method), values_from = belief_estimate)

    # Should be one row per firm, none missing
    stopifnot(!anyDuplicated(subgroup_beliefs$firm_id), !anyNA(subgroup_beliefs))

    # Define a variable to indicate the subsample
    subgroup_beliefs <- subgroup_beliefs |> dplyr::mutate(subsample = tolower(subgroup))

    # Place the subsample variable at the beginning of the dataset
    subgroup_beliefs <- subgroup_beliefs |> dplyr::select(subsample, everything())

    # Append the subgroup beliefs to the aggregated dataframe
    aggregated_subgroup_beliefs <- rbind(aggregated_subgroup_beliefs, subgroup_beliefs)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of njobs-weighted Katz belief noise by subgroup i.e., the EIV measurement-error variance
# -----------------------------------------------------------------------------------------------------------------------------
# Define dataframe to store all subgroup noise
aggregated_subgroup_noise <- data.frame()

# Loop through each subgroup
for (subgroup in c("White", "Black", "Female", "Male", "Looking", "Not_Looking", "Feared_Discrimination_1", "Feared_Discrimination_0", "Age_gte40", "Age_lt40")) {
    # Load in given subgroup's variance sheet
    subgroup_noise <- read_parquet_sheet(file.path(intermediate, paste0("Subset_", subgroup)), "variance")

    # Uniquely identified by sample x aggregation model x belief measure, none missing
    stopifnot(!anyDuplicated(subgroup_noise[c("subset", "model", "outcome")]), !anyNA(subgroup_noise[c("subset", "model", "outcome")]))

    # Keep OLS and Borda observations
    subgroup_noise <- subgroup_noise |> dplyr::filter(model %in% c("OLS", "Borda"))

    # Keep just the two pooled belief measures
    subgroup_noise <- subgroup_noise |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

    # Keep just the 97-firm sample from the correspondence paper
    subgroup_noise <- subgroup_noise |> dplyr::filter(subset == "subset97")

    # Should be 2 models x 2 belief measures = 4 observations remaining
    stopifnot(nrow(subgroup_noise) == 4)

    # Keep necessary variables
    subgroup_noise <- subgroup_noise |> dplyr::select(model, outcome, noise_njobs_weighted_katz)

    # Rename variables to be more descriptive
    subgroup_noise <- subgroup_noise |> dplyr::rename(aggregation_method = model, belief_measure = outcome, njobs_weighted_katz_noise_across_firms = noise_njobs_weighted_katz)

    # The njobs-weighted Katz measurement-error variance must be present and positive for every cell fed to eivreg
    stopifnot(!anyNA(subgroup_noise$njobs_weighted_katz_noise_across_firms), all(subgroup_noise$njobs_weighted_katz_noise_across_firms > 0))

    # Lowercase the aggregation method for the wide column suffix
    subgroup_noise <- subgroup_noise |> dplyr::mutate(aggregation_method = tolower(aggregation_method))

    # Reshape wide to one row per subgroup, one column per belief measure x aggregation method
    subgroup_noise <- subgroup_noise |> tidyr::pivot_wider(names_from = c(belief_measure, aggregation_method), values_from = njobs_weighted_katz_noise_across_firms, names_glue = "{belief_measure}_{aggregation_method}_njobs_weighted_katz_noise_across_firms")

    # Should be a single row, none missing
    stopifnot(nrow(subgroup_noise) == 1, !anyNA(subgroup_noise))

    # Define a variable to indicate the subsample
    subgroup_noise <- subgroup_noise |> dplyr::mutate(subsample = tolower(subgroup))

    # Place the subsample variable at the beginning of the dataset
    subgroup_noise <- subgroup_noise |> dplyr::select(subsample, everything())

    # Append the subgroup noise to the aggregated dataframe
    aggregated_subgroup_noise <- rbind(aggregated_subgroup_noise, subgroup_noise)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Merge the four datasets into one firm x subgroup analysis dataset
# -----------------------------------------------------------------------------------------------------------------------------
# Merge subgroup noise onto the beliefs m:1  (many firms per subsample : one noise row per subsample)
    # relationship = "many-to-one" enforces the using side is unique on subsample; errors otherwise
    # left_join keeps every master row and appends the noise columns as new variables 
firm_subgroup_data <- aggregated_subgroup_beliefs |> dplyr::left_join(aggregated_subgroup_noise, by = "subsample", relationship = "many-to-one")

# assert(3) i.e., every subsample matches on both sides
stopifnot(setequal(aggregated_subgroup_beliefs$subsample, aggregated_subgroup_noise$subsample))

# Merge firm-level audit gaps onto the analysis dataset m:1  (many subgroups per firm : one audit-gap row per firm)
    # relationship = "many-to-one" enforces the using side is unique on firm_id; errors otherwise
    # left_join keeps every master row and appends the audit-gap columns as new variables
firm_subgroup_data <- firm_subgroup_data |> dplyr::left_join(firm_audit_gaps, by = "firm_id", relationship = "many-to-one")

# assert(3) i.e., every firm_id matches on both sides
stopifnot(setequal(firm_subgroup_data$firm_id, firm_audit_gaps$firm_id))

# Merge industry onto the analysis dataset m:1  (many subgroups per firm : one industry per firm)
    # relationship = "many-to-one" enforces the using side is unique on firm_id; errors otherwise
    # left_join keeps every master row and appends aer_naics2; the crosswalk's 164 firms are a superset of the 97, so the extras drop
firm_subgroup_data <- firm_subgroup_data |> dplyr::left_join(firm_industry_crosswalk, by = "firm_id", relationship = "many-to-one")

# assert(2 3) i.e., every master firm matched an industry (no _merge==1); the extra crosswalk firms (_merge==2) are expected
stopifnot(all(firm_subgroup_data$firm_id %in% firm_industry_crosswalk$firm_id))

# Should be 10 subsamples x 97 firms = 970 observations
stopifnot(nrow(firm_subgroup_data) == 970)

# Order variables as subsample, firm id, name, industry category, number of jobs, everything else 
firm_subgroup_data <- firm_subgroup_data |> dplyr::relocate(subsample, firm_id, firm_name, aer_naics2, number_of_jobs)

# Should be uniquely identified by firm_id and subsample, and these should never be missing 
stopifnot(!anyDuplicated(firm_subgroup_data[c("firm_id", "subsample")]), !any(is.na(firm_subgroup_data[c("firm_id", "subsample")])))

# Check no variables are missing 
stopifnot(!any(is.na(firm_subgroup_data)))

# -----------------------------------------------------------------------------------------------------------------------------
# Run pairwise comparison EIV regressions using the njobs-weighted Katz noise
# -----------------------------------------------------------------------------------------------------------------------------
# Define empty regression results dataframe
eiv_regression_results <- data.frame()

# Loop over each subgroup comparison
for (subgroup_comparison in list(c("white", "black"), c("male", "female"), c("looking", "not_looking"), c("feared_discrimination_1", "feared_discrimination_0"), c("age_gte40", "age_lt40"))) {
    # Loop over aggregation method
    for (aggregation_method in c("ols", "borda")) {
        # Loop over each LHS variable
        for (lhs_variable in c("dif_log_contact_rate_white_black", "dif_log_contact_rate_male_female")) {

            # RHS belief base name implied by the LHS audit gap; race gap uses the white-favoritism belief, gender gap the male-favoritism belief
            rhs_variable_base_name <- c(dif_log_contact_rate_white_black = "pooled_favor_white", dif_log_contact_rate_male_female = "pooled_favor_male")[[lhs_variable]]

            # Full RHS belief column name; base name plus the aggregation method suffix
            rhs_variable <- paste0(rhs_variable_base_name, "_", aggregation_method)

            # Restrict to the two sides of the given subgroup comparison
            estimation_sample <- firm_subgroup_data |> dplyr::filter(subsample %in% subgroup_comparison)

            # Should be 97 firms x 2 subsamples = 194 rows, one per firm x subsample
            stopifnot(nrow(estimation_sample) == 194)

            # Keep necessary variables 
            estimation_sample <- estimation_sample |> dplyr::select(
                subsample, firm_id, aer_naics2, number_of_jobs,
                dplyr::all_of(lhs_variable),
                dplyr::all_of(rhs_variable),
                dplyr::all_of(paste0(rhs_variable, "_njobs_weighted_katz_noise_across_firms"))
            )

            # Build interaction term for rhs variable interacted with a subgroup indicator
            for (subgroup in subgroup_comparison) {
                estimation_sample <- estimation_sample |> dplyr::mutate(!!paste0(rhs_variable, "_x_", subgroup) := .data[[rhs_variable]] * (subsample == subgroup))
            }

            # Check the two interaction terms sum back to the original belief
            stopifnot(all(estimation_sample[[paste0(rhs_variable, "_x_", subgroup_comparison[1])]] + estimation_sample[[paste0(rhs_variable, "_x_", subgroup_comparison[2])]] == estimation_sample[[rhs_variable]]))

            # Assert number of jobs weight is firm-constant across subsamples
            # Since we are weighting firms in the eivreg by its number of jobs, we want to split the weight evenly between the two given subsamples 
            stopifnot(nrow(dplyr::distinct(estimation_sample, firm_id, number_of_jobs)) == dplyr::n_distinct(estimation_sample$firm_id))

            # Initialize a matrix for eivreg to subtracts the noise from the table of summed products of the two belief columns before computing the slopes; the white entry corrects the white slope for measurement error, the black entry the black slope
            # Rows = columns = the two belief columns, so eivreg lines up each noise value with the correct rhs variable column; off-diagonals stay zero since a row is either white or black, so the two columns are never both nonzero
            regressor_noise_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(paste0(rhs_variable, "_x_", subgroup_comparison), paste0(rhs_variable, "_x_", subgroup_comparison)))

            # Fill each subgroup's diagonal entry, halved because eivreg multiplies this matrix by the row count (194 stacked) before subtracting, so halving rescales to the 97-row standalone correction
            for (subgroup in subgroup_comparison) {
                regressor_noise_matrix[paste0(rhs_variable, "_x_", subgroup), paste0(rhs_variable, "_x_", subgroup)] <- unique(estimation_sample[[paste0(rhs_variable, "_njobs_weighted_katz_noise_across_firms")]][estimation_sample$subsample == subgroup]) / 2
            }

            # Convert estimation_sample to a data frame because R is insane
            estimation_sample <- as.data.frame(estimation_sample)

            # Run the stacked EIV without industry FE
                # two group intercepts (0 + subsample) plus the two separate slopes, weighted by jobs, clustered by firm, noise matrix correcting the two slopes
            eiv_no_industry_fe <- eivreg(
                as.formula(paste0(lhs_variable, " ~ 0 + subsample + ", rhs_variable, "_x_", subgroup_comparison[1], " + ", rhs_variable, "_x_", subgroup_comparison[2])),
                data = estimation_sample,
                weights = number_of_jobs,
                Sigma_error = regressor_noise_matrix,
                cluster_varname = "firm_id"
            )

            # Run the stacked EIV with industry FE
                # per-group industry intercepts (0 + subsample:factor(aer_naics2)) plus the two separate slopes, weighted by jobs, clustered by firm, noise matrix correcting the two slopes (FE dummies get zero correction since they are not named in it)
            eiv_industry_fe <- eivreg(
                as.formula(paste0(lhs_variable, " ~ 0 + subsample:factor(aer_naics2) + ", rhs_variable, "_x_", subgroup_comparison[1], " + ", rhs_variable, "_x_", subgroup_comparison[2])),
                data = estimation_sample,
                weights = number_of_jobs,
                Sigma_error = regressor_noise_matrix,
                cluster_varname = "firm_id"
            )

            ## Append (i) point estimates of belief variables, (ii) their ses, (iii) the point estimate of the slope difference, and (iv) its se into the regression results dataframe for both the no-industry FE and industry FE models
            # Loop over the two fitted specifications
            for (eiv_specification in list(eiv_no_industry_fe, eiv_industry_fe)) {

                # Industry-FE indicator names which of the two fits this is
                industry_fe_indicator <- if (identical(eiv_specification, eiv_no_industry_fe)) "no" else "yes"

                # Store rhs variable coefficient difference between the two subgroups
                rhs_variable_coefficient_difference <- as.numeric(eiv_specification$coefficients[paste0(rhs_variable, "_x_", subgroup_comparison[1])] - eiv_specification$coefficients[paste0(rhs_variable, "_x_", subgroup_comparison[2])])

                # Standard error of the rhs variable coefficient difference between the two subgroups
                rhs_variable_coefficient_difference_se <- sqrt(

                    # Variance of the rhs variable coefficient for the first subgroup
                    eiv_specification$vcov[paste0(rhs_variable, "_x_", subgroup_comparison[1]), paste0(rhs_variable, "_x_", subgroup_comparison[1])]

                    # Variance of the rhs variable coefficient for the second subgroup
                    + eiv_specification$vcov[paste0(rhs_variable, "_x_", subgroup_comparison[2]), paste0(rhs_variable, "_x_", subgroup_comparison[2])]

                    # Covariance of the rhs variable coefficient between the two subgroups
                    - 2 * eiv_specification$vcov[paste0(rhs_variable, "_x_", subgroup_comparison[1]), paste0(rhs_variable, "_x_", subgroup_comparison[2])]
                )

                # Loop over the two subgroups
                for (subgroup in subgroup_comparison) {

                    # Append the results for the current subgroup
                    eiv_regression_results <- rbind(eiv_regression_results, data.frame(
                        subgroup_comparison = paste0(subgroup_comparison[1], "_minus_", subgroup_comparison[2]),
                        subgroup = subgroup,
                        industry_fe_indicator = industry_fe_indicator,
                        lhs_variable = lhs_variable,
                        rhs_variable = rhs_variable,
                        rhs_variable_coefficient = as.numeric(eiv_specification$coefficients[paste0(rhs_variable, "_x_", subgroup)]),
                        rhs_variable_se = sqrt(eiv_specification$vcov[paste0(rhs_variable, "_x_", subgroup), paste0(rhs_variable, "_x_", subgroup)]),
                        rhs_variable_coefficient_difference = rhs_variable_coefficient_difference,
                        rhs_variable_coefficient_difference_se = rhs_variable_coefficient_difference_se
                    ))
                }
            }
        }
    }
}

# -----------------------------------------------------------------------------------------------------------------------------
# Plot the EIV coefficients as a coefplot, one figure per RHS belief measure (embeds both the LHS gap and the aggregation method)
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each RHS belief measure
for (rhs_variable_value in c("pooled_favor_white_ols", "pooled_favor_white_borda", "pooled_favor_male_ols", "pooled_favor_male_borda")) {

    # Restrict to the current RHS belief measure and aggregation method
    eiv_coefplot_data <- eiv_regression_results |> dplyr::filter(rhs_variable == rhs_variable_value)

    # Should be 5 subgroup comparisons x 2 subgroups x 2 industry-FE specs = 20 points
    stopifnot(nrow(eiv_coefplot_data) == 20)

    # Position of the subgroup within its comparison: first listed subgroup at 0, second at 1
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(within_comparison_position = ifelse(subgroup == sub("_minus_.*", "", subgroup_comparison), 0, 1))

    # Order the comparisons top-to-bottom
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(subgroup_comparison = factor(subgroup_comparison, levels = c("white_minus_black", "male_minus_female", "looking_minus_not_looking", "feared_discrimination_1_minus_feared_discrimination_0", "age_gte40_minus_age_lt40")))
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(comparison_index = as.integer(subgroup_comparison) - 1)

    # Spec label, spec block index, and single-line subgroup label; the No-Controls block sits above the Industry-FE block within each comparison
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(fe_label = factor(industry_fe_indicator, levels = c("no", "yes"), labels = c("No Controls", "Industry FE")))
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(spec_block_index = ifelse(industry_fe_indicator == "no", 0, 1))
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(subgroup_label = dplyr::recode(subgroup, "white" = "White", "black" = "Black", "female" = "Female", "male" = "Male", "looking" = "Looking", "not_looking" = "Not Looking", "feared_discrimination_1" = "Feared", "feared_discrimination_0" = "No Fear", "age_gte40" = "Age ≥40", "age_lt40" = "Age <40"))

    # Row layout: within a comparison the two specs form stacked blocks of two subgroups each, separated by a small gap; comparisons separated by a larger gap
    block_gap <- 0.8
    comparison_gap <- 1.6
    rows_in_block <- 2
    comparison_span <- 2 * rows_in_block + block_gap
    comparison_period <- comparison_span + comparison_gap
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(row_in_comparison = spec_block_index * (rows_in_block + block_gap) + within_comparison_position)
    eiv_coefplot_data <- eiv_coefplot_data |> dplyr::mutate(point_y = -(comparison_index * comparison_period + row_in_comparison))

    # Subsample label per row
    y_axis_data <- eiv_coefplot_data |> dplyr::distinct(point_y, subgroup_label)

    # Slope-difference annotation per spec block, placed just past that block's longest interval so the right margin stays free for the legend
    block_interval_max <- eiv_coefplot_data |> dplyr::group_by(comparison_index, spec_block_index) |> dplyr::summarize(block_interval_max = max(rhs_variable_coefficient + 1.96 * rhs_variable_se), .groups = "drop")
    delta_annotation_data <- eiv_coefplot_data |> dplyr::group_by(subgroup_comparison, comparison_index, fe_label, spec_block_index) |> dplyr::summarize(rhs_variable_coefficient_difference = dplyr::first(rhs_variable_coefficient_difference), rhs_variable_coefficient_difference_se = dplyr::first(rhs_variable_coefficient_difference_se), .groups = "drop")
    delta_annotation_data <- delta_annotation_data |> dplyr::left_join(block_interval_max, by = c("comparison_index", "spec_block_index"))
    delta_annotation_data <- delta_annotation_data |> dplyr::mutate(delta_y = -(comparison_index * comparison_period + spec_block_index * (rows_in_block + block_gap) + 0.5), delta_x = block_interval_max + 0.06)

    # Faint separators midway in the gap between comparisons
    separator_y <- -((seq_len(4)) * comparison_period - comparison_gap / 2)

    # Coefplot: each subgroup a point with its 95% interval, the two specs as stacked color-coded blocks within each comparison, the slope difference annotated per block
    eiv_coefplot <- ggplot(eiv_coefplot_data, aes(x = rhs_variable_coefficient, y = point_y, color = fe_label)) +

        # Vertical reference line at zero
        geom_vline(xintercept = 0, color = "grey55", linewidth = 0.3) +

        # Long-dashed separators between subgroup comparisons
        geom_hline(yintercept = separator_y, linetype = "longdash", color = "grey", linewidth = 0.4) +

        # 95% confidence intervals
        geom_errorbar(aes(xmin = rhs_variable_coefficient - 1.96 * rhs_variable_se, xmax = rhs_variable_coefficient + 1.96 * rhs_variable_se), orientation = "y", width = 0, linewidth = 0.5) +

        # Point estimates
        geom_point(size = 2.6) +

        # Slope difference per spec block, color-matched to the spec
        geom_text(data = delta_annotation_data, aes(x = delta_x, y = delta_y, color = fe_label, label = paste0("Δ = ", round(rhs_variable_coefficient_difference, 3), " (", round(rhs_variable_coefficient_difference_se, 3), ")")), hjust = 0, size = 3.3, inherit.aes = FALSE, show.legend = FALSE) +

        # Colors for the no-controls and industry-FE specs
        scale_color_manual(values = c("No Controls" = "steelblue", "Industry FE" = "darkorange")) +

        # Subsample label per row, with a little vertical headroom
        scale_y_continuous(breaks = y_axis_data$point_y, labels = y_axis_data$subgroup_label, expand = expansion(mult = c(0.02, 0.03))) +

        # Right headroom for the delta annotations
        scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +

        # X-axis names the RHS belief; no y-axis or legend title
        labs(x = paste0("EIV coefficient on ", unique(eiv_coefplot_data$rhs_variable)), y = NULL, color = NULL) +

        # Theme baseline (larger base font)
        theme_minimal(base_size = 14) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # White background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),

            # Bottom axis spine, no ticks
            axis.line.x = element_line(color = "black"),
            axis.ticks = element_blank(),

            # Legend inside the plot, top-right
            legend.position = c(0.98, 0.98),
            legend.justification = c(1, 1),
            legend.direction = "vertical",
            legend.background = element_blank(),
            legend.key = element_blank()
        )

    # Export the figure, one file per RHS belief measure
    ggsave(file.path(figures, paste0("eiv_coefplot_by_subgroup_", rhs_variable_value, ".png")), plot = eiv_coefplot, width = 14, height = 10, dpi = 300, bg = "white")
}
