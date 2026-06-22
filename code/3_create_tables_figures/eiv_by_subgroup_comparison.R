# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Stacked two-row-per-firm EIV testing whether the subgroup-split coefplot slopes are 
# statistically distinguishable
#
# Created: Nico Rotundo 2026-06-13
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
# Plot the EIV coefficients in a bar graph, one figure per RHS belief measure (embeds both the LHS gap and the aggregation method)
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each RHS belief measure
for (rhs_variable_value in c("pooled_favor_white_ols", "pooled_favor_white_borda", "pooled_favor_male_ols", "pooled_favor_male_borda")) {

    # Restrict to the current RHS belief measure and aggregation method
    eiv_bar_graph_data <- eiv_regression_results |> dplyr::filter(rhs_variable == rhs_variable_value)

    # Should be 5 subgroup comparisons x 2 subgroups x 2 industry-FE specs = 20 bars
    stopifnot(nrow(eiv_bar_graph_data) == 20)

    # Position within the pair: the two subgroup bars touch (0 and 1)
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(within_pair_position = ifelse(subgroup == sub("_minus_.*", "", subgroup_comparison), 0, 1))

    # Order the subgroup comparisons along the x-axis
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(subgroup_comparison = factor(subgroup_comparison, levels = c("white_minus_black", "male_minus_female", "looking_minus_not_looking", "feared_discrimination_1_minus_feared_discrimination_0", "age_gte40_minus_age_lt40")))

    # Label the industry-FE bars
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(fe_label = factor(industry_fe_indicator, levels = c("no", "yes"), labels = c("No Controls", "Industry FE")))

    # Readable subsample label under each bar
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(subgroup_label = dplyr::recode(subgroup, "white" = "White", "black" = "Black", "female" = "Female", "male" = "Male", "looking" = "Looking", "not_looking" = "Not\nLooking", "feared_discrimination_1" = "Feared", "feared_discrimination_0" = "No\nFear", "age_gte40" = "Age\n≥40", "age_lt40" = "Age\n<40"))

    # Estimate label vertical placement: above positive bars, below negative bars
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(label_vjust = ifelse(rhs_variable_coefficient >= 0, -0.4, 1.4))

    # Store the top of the taller error bar in each pair, so the delta annotation clears both error bars
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::group_by(subgroup_comparison, fe_label) |> dplyr::mutate(pair_max_error_bar_top = max(rhs_variable_coefficient + 1.96 * rhs_variable_se)) |> dplyr::ungroup()

    # Bar width and the three edge-to-edge whitespaces controlling the layout
    bar_width <- 1.2
    within_pair_whitespace <- 0.6
    inter_pair_whitespace <- 2.2
    edge_whitespace <- 1.1

    # Center-to-center spacings within a pair and between the two pairs
    within_pair_gap <- bar_width + within_pair_whitespace
    inter_pair_gap <- bar_width + inter_pair_whitespace

    # Distance between consecutive comparison block origins
    comparison_period <- 2 * within_pair_gap + inter_pair_gap + bar_width + 2 * edge_whitespace

    # Indicator for the industry-FE pair (the second pair in each comparison)
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(fe_pair = ifelse(industry_fe_indicator == "yes", 1, 0))

    # Origin of each comparison block along the x-axis
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(comparison_origin = (as.integer(subgroup_comparison) - 1) * comparison_period)

    # Bar x position: block origin + within-pair offset + the no-FE-to-FE pair offset
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(bar_x_position = comparison_origin + within_pair_position * within_pair_gap + fe_pair * (within_pair_gap + inter_pair_gap))

    # Center of each pair, where the delta annotation sits
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(pair_center_x = comparison_origin + fe_pair * (within_pair_gap + inter_pair_gap) + within_pair_gap / 2)

    # Estimate label just right of each whisker, left-aligned
    eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(label_x = bar_x_position + 0.2, label_hjust = 0)

    # Dashed separators a symmetric edge whitespace from the last bar of one comparison and the first bar of the next
    separator_positions <- (0:3) * comparison_period + (2 * within_pair_gap + inter_pair_gap) + bar_width / 2 + edge_whitespace

    # Bar graph: two touching bars per subgroup comparison (no-FE pair then industry-FE pair), with the coefficient difference annotated above each pair
    eiv_bar_graph <- ggplot(eiv_bar_graph_data, aes(x = bar_x_position, y = rhs_variable_coefficient, fill = fe_label)) +

        # Horizontal reference line at zero
        geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +

        # Bars
        geom_col(width = bar_width) +

        # Thin black 95% confidence interval error bars
        geom_errorbar(aes(ymin = rhs_variable_coefficient - 1.96 * rhs_variable_se, ymax = rhs_variable_coefficient + 1.96 * rhs_variable_se), width = 0.2, linewidth = 0.3, color = "black") +

        # Grey dashed separators between subgroup comparisons
        geom_vline(xintercept = separator_positions, linetype = "dashed", color = "grey") +

        # Each bar's coefficient only, without the standard error, just right of the whisker
        geom_text(aes(x = label_x, y = rhs_variable_coefficient, hjust = label_hjust, vjust = label_vjust, label = round(rhs_variable_coefficient, 3)), size = 3.85, color = "black") +

        # Coefficient difference above each pair
        geom_text(aes(x = pair_center_x, y = pair_max_error_bar_top + 0.05 * max(pair_max_error_bar_top), label = paste0("Δ = ", round(rhs_variable_coefficient_difference, 3), " (", round(rhs_variable_coefficient_difference_se, 3), ")")), size = 3.85, color = "black") +

        # Colors for without- and with-industry fe bars
        scale_fill_manual(values = c("No Controls" = "steelblue", "Industry FE" = "darkorange"), breaks = c("No Controls", "Industry FE")) +

        # Name each individual subsample under its bar, with symmetric edge margins matching the vline gaps
        scale_x_continuous(breaks = eiv_bar_graph_data$bar_x_position, labels = eiv_bar_graph_data$subgroup_label, expand = expansion(add = edge_whitespace)) +

        # Top headroom so the delta annotations clear the legend
        scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +

        # No descriptive legend title
        labs(x = "Subsample", y = paste0("EIV coefficient on ", unique(eiv_bar_graph_data$rhs_variable)), fill = NULL) +

        # Theme baseline (larger base font)
        theme_minimal(base_size = 14) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # White background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),

            # Bottom and left axis spines
            axis.line = element_line(color = "black"),

            # Y-axis tick marks
            axis.ticks.y = element_line(color = "black"),

            # Angle the per-bar subsample labels so the two within a pair do not overlap
            axis.text.x = element_text(angle = 45, hjust = 1),

            # Legend inside the plot
            legend.position = c(0.9, 0.9),
            legend.background = element_blank(),
            legend.key = element_blank()
        )

    # Export the figure, one file per RHS belief measure
    ggsave(file.path(figures, paste0("eiv_by_subgroup_comparison_", rhs_variable_value, ".png")), plot = eiv_bar_graph, width = 24, height = 8, dpi = 300, bg = "white")
}