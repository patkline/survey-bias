# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Stacked two-row-per-firm EIV testing whether the subgroup-split coefplot
#          slopes are statistically distinguishable
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
# Construct dataset of belief noise by subgroup i.e., the EIV measurement-error variance
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
    subgroup_noise <- subgroup_noise |> dplyr::select(model, outcome, noise, variance, signal, sigma2_hat)

    # Rename variables to be more descriptive
    subgroup_noise <- subgroup_noise |> dplyr::rename(aggregation_method = model, belief_measure = outcome, mean_squared_standard_error_across_firms = noise, variance_across_firms = variance, katz_corrected_signal_variance_across_firms = signal, unbiased_signal_variance_estimate_across_firms = sigma2_hat)

    # Check the unbiased signal variance estimate equals the across-firm variance minus the mean squared standard error
    stopifnot(all(abs(subgroup_noise$unbiased_signal_variance_estimate_across_firms - (subgroup_noise$variance_across_firms - subgroup_noise$mean_squared_standard_error_across_firms)) < 1e-12))

    # Check the Katz measurement-error variance (variance minus katz-corrected signal) equals the mean squared standard error minus the Katz adjustment (katz-corrected signal minus unbiased signal estimate)
    stopifnot(all(abs((subgroup_noise$variance_across_firms - subgroup_noise$katz_corrected_signal_variance_across_firms) - (subgroup_noise$mean_squared_standard_error_across_firms - (subgroup_noise$katz_corrected_signal_variance_across_firms - subgroup_noise$unbiased_signal_variance_estimate_across_firms))) < 1e-12))

    # Guard the Katz measurement-error variance: must be positive (PSD) and no larger than the raw mean squared standard error (Katz only inflates the signal)
    stopifnot(all(subgroup_noise$variance_across_firms - subgroup_noise$katz_corrected_signal_variance_across_firms > 0), all(subgroup_noise$variance_across_firms - subgroup_noise$katz_corrected_signal_variance_across_firms <= subgroup_noise$mean_squared_standard_error_across_firms + 1e-12))

    # Drop the unbiased signal variance estimate; only needed for the checks above
    subgroup_noise <- subgroup_noise |> dplyr::select(-unbiased_signal_variance_estimate_across_firms)

    # Lowercase the aggregation method for the wide column suffix
    subgroup_noise <- subgroup_noise |> dplyr::mutate(aggregation_method = tolower(aggregation_method))

    # Reshape wide to one row per subgroup, one column per belief measure x aggregation method
    subgroup_noise <- subgroup_noise |> tidyr::pivot_wider(names_from = c(belief_measure, aggregation_method), values_from = c(mean_squared_standard_error_across_firms, variance_across_firms, katz_corrected_signal_variance_across_firms), names_glue = "{belief_measure}_{aggregation_method}_{.value}")

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

View(firm_subgroup_data)
# -----------------------------------------------------------------------------------------------------------------------------
# Run pairwise comparison EIV regressions without the katz correction
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
                dplyr::all_of(paste0(rhs_variable, "_mean_squared_standard_error_across_firms")),
                dplyr::all_of(paste0(rhs_variable, "_variance_across_firms")),
                dplyr::all_of(paste0(rhs_variable, "_katz_corrected_signal_variance_across_firms"))
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

            # Loop over the measurement-error correction fed to eivreg: raw mean squared standard error (no) vs the Katz-corrected version (yes)
            for (katz_correction in c("no", "yes")) {

                # Initialize a matrix for eivreg to subtracts the noise from the table of summed products of the two belief columns before computing the slopes; the white entry corrects the white slope for measurement error, the black entry the black slope
                # Rows = columns = the two belief columns, so eivreg lines up each noise value with the correct rhs variable column; off-diagonals stay zero since a row is either white or black, so the two columns are never both nonzero
                regressor_noise_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(paste0(rhs_variable, "_x_", subgroup_comparison), paste0(rhs_variable, "_x_", subgroup_comparison)))

                # Fill each subgroup's diagonal entry, halved because eivreg multiplies this matrix by the row count (194 stacked) before subtracting, so halving rescales to the 97-row standalone correction
                for (subgroup in subgroup_comparison) {
                    if (katz_correction == "no") {
                        # No-Katz: the raw mean squared standard error across firms
                        regressor_noise_matrix[paste0(rhs_variable, "_x_", subgroup), paste0(rhs_variable, "_x_", subgroup)] <- unique(estimation_sample[[paste0(rhs_variable, "_mean_squared_standard_error_across_firms")]][estimation_sample$subsample == subgroup]) / 2
                    } else {
                        # Katz: total variance across firms minus the Katz-corrected signal variance
                        regressor_noise_matrix[paste0(rhs_variable, "_x_", subgroup), paste0(rhs_variable, "_x_", subgroup)] <- unique(estimation_sample[[paste0(rhs_variable, "_variance_across_firms")]][estimation_sample$subsample == subgroup] - estimation_sample[[paste0(rhs_variable, "_katz_corrected_signal_variance_across_firms")]][estimation_sample$subsample == subgroup]) / 2
                    }
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

                # Print the coefficients and their standard errors for each specification 
                for (eiv_specification in list(eiv_no_industry_fe, eiv_industry_fe)) {
                    print(lmtest::coeftest(eiv_specification, vcov. = eiv_specification$vcov, df = eiv_specification$cluster_num - 1))
                }

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
                            katz_correction = katz_correction,
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
}

View(eiv_regression_results)

# -----------------------------------------------------------------------------------------------------------------------------
# Plot the EIV coefficients in a bar graph for {ols, borda} x (lhs_variable, rhs_variable) x 
# {no katz correction, katz correction}
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each RHS belief measure (embeds both the LHS gap and the aggregation method)
for (rhs_variable_value in c("pooled_favor_white_ols", "pooled_favor_white_borda", "pooled_favor_male_ols", "pooled_favor_male_borda")) {

    # Loop over each Katz correction specification
    for (katz_correction_value in c("no", "yes")) {

        # Restrict to the current RHS belief measure and aggregation method
        eiv_bar_graph_data <- eiv_regression_results |> dplyr::filter(rhs_variable == rhs_variable_value)

        # LHS variable value should be unique since it is paired with the RHS variable
        stopifnot(dplyr::n_distinct(eiv_bar_graph_data$lhs_variable) == 1)

        # Pull the LHS variable off the filtered data for the export filename
        lhs_variable_value <- unique(eiv_bar_graph_data$lhs_variable)

        # Restrict to the current Katz correction specification
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::filter(katz_correction == katz_correction_value)

        # Should be 5 subgroup comparisons x 2 subgroups x 2 industry-FE specs = 20 bars
        stopifnot(nrow(eiv_bar_graph_data) == 20)

        # Position within the pair: the two subgroup bars touch (0 and 1)
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(within_pair_position = ifelse(subgroup == sub("_minus_.*", "", subgroup_comparison), 0, 1))

        # Offset for the industry-FE pair: gap between the no-FE pair and the industry-FE pair leaves room for the on-bar estimate labels
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(industry_fe_cluster_offset = ifelse(industry_fe_indicator == "no", 0, 5))

        # Order the subgroup comparisons along the x-axis
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(subgroup_comparison = factor(subgroup_comparison, levels = c("white_minus_black", "male_minus_female", "looking_minus_not_looking", "feared_discrimination_1_minus_feared_discrimination_0", "age_gte40_minus_age_lt40")))

        # Bar x position: comparison block + industry-FE cluster offset + within-pair position
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(bar_x_position = (as.integer(subgroup_comparison) - 1) * 11 + industry_fe_cluster_offset + within_pair_position)

        # Label the industry-FE bars
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(fe_label = factor(industry_fe_indicator, levels = c("no", "yes"), labels = c("No Controls", "Industry FE")))

        # Readable subsample label under each bar
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(subgroup_label = dplyr::recode(subgroup, "white" = "White", "black" = "Black", "female" = "Female", "male" = "Male", "looking" = "Looking", "not_looking" = "Not\nLooking", "feared_discrimination_1" = "Feared", "feared_discrimination_0" = "No\nFear", "age_gte40" = "Age\n≥40", "age_lt40" = "Age\n<40"))

        # Center of each touching pair, where the delta annotation sits
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(pair_center_x = bar_x_position - within_pair_position + 0.5)

        # Store the top of the taller error bar in each pair, so the delta annotation clears both error bars
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::group_by(subgroup_comparison, fe_label) |> dplyr::mutate(pair_max_error_bar_top = max(rhs_variable_coefficient + 1.96 * rhs_variable_se)) |> dplyr::ungroup()

        # Estimate label horizontal placement: left of the error bar for the first bar in the pair, right for the second
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(label_x = bar_x_position + ifelse(within_pair_position == 0, -0.15, 0.15), label_hjust = ifelse(within_pair_position == 0, 1, 0))

        # Estimate label vertical placement: above positive bars, below negative bars
        eiv_bar_graph_data <- eiv_bar_graph_data |> dplyr::mutate(label_vjust = ifelse(rhs_variable_coefficient >= 0, -0.4, 1.4))

        View(eiv_bar_graph_data)

        # Bar graph: two touching bars per subgroup comparison (no-FE pair then industry-FE pair), with the coefficient difference annotated above each pair
        eiv_bar_graph <- ggplot(eiv_bar_graph_data, aes(x = bar_x_position, y = rhs_variable_coefficient, fill = fe_label)) +

            # Horizontal reference line at zero
            geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +

            # Touching bars within each pair (no border, so the touching edge has no white gap)
            geom_col(width = 1) +

            # Thin black 95% confidence interval error bars
            geom_errorbar(aes(ymin = rhs_variable_coefficient - 1.96 * rhs_variable_se, ymax = rhs_variable_coefficient + 1.96 * rhs_variable_se), width = 0.2, linewidth = 0.3, color = "black") +

            # Grey dashed separators between subgroup comparisons
            geom_vline(xintercept = (1:4) * 11 - 2.75, linetype = "dashed", color = "grey") +

            # Each bar's coefficient (se): left of the error bar for the first bar in the pair, right for the second; above positive bars, below negative
            geom_text(aes(x = label_x, y = rhs_variable_coefficient, hjust = label_hjust, vjust = label_vjust, label = paste0(round(rhs_variable_coefficient, 3), " (", round(rhs_variable_se, 3), ")")), size = 3.5, color = "black") +

            # Coefficient difference above each pair
            geom_text(aes(x = pair_center_x, y = pair_max_error_bar_top + 0.05 * max(pair_max_error_bar_top), label = paste0("Δ = ", round(rhs_variable_coefficient_difference, 3), " (", round(rhs_variable_coefficient_difference_se, 3), ")")), size = 3.5, color = "black") +

            # Colors for without- and with-industry fe bars
            scale_fill_manual(values = c("No Controls" = "steelblue", "Industry FE" = "darkorange"), breaks = c("No Controls", "Industry FE")) +

            # Name each individual subsample under its bar
            scale_x_continuous(breaks = eiv_bar_graph_data$bar_x_position, labels = eiv_bar_graph_data$subgroup_label) +

            # Add top headroom so the legend clears the delta annotations
            scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +

            # Single "Subsample" x-axis title; y-axis names the RHS belief; legend title states the dimensions this figure is restricted to
            labs(x = "Subsample", y = paste0("EIV coefficient on ", unique(eiv_bar_graph_data$rhs_variable)), fill = paste0("Aggregation: ", sub(".*_", "", unique(eiv_bar_graph_data$rhs_variable)), "\nLHS gap: ", unique(eiv_bar_graph_data$lhs_variable), "\nKatz: ", unique(eiv_bar_graph_data$katz_correction))) +

            # Theme baseline (larger base font)
            theme_minimal(base_size = 13) +

            # Theme adjustments 
            theme(
                # No grid lines
                panel.grid = element_blank(),

                # White background
                panel.background = element_rect(fill = "white", color = NA),
                plot.background = element_rect(fill = "white", color = NA),

                # Bottom and left axis spines
                axis.line = element_line(color = "black"),

                # Angle the per-bar subsample labels so the two within a pair do not overlap
                axis.text.x = element_text(angle = 45, hjust = 1),

                # Legend inside the plot
                legend.position = c(0.9, 0.9),
                legend.background = element_blank(),
                legend.key = element_blank()
            )

        # Export the figure to the scratch folder, one file per RHS belief x Katz specification
        ggsave(file.path(code, "scratch_nico", "figures", paste0("eiv_by_subgroup_comparison_katz_correction_", katz_correction_value, "_lhs_variable_", lhs_variable_value, "_rhs_variable_", rhs_variable_value, ".png")), plot = eiv_bar_graph, width = 24, height = 8, dpi = 300, bg = "white")
    }
}