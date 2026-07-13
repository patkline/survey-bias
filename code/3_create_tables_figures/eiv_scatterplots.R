# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Scatterplots of the experimental audit contact gaps on the raw firm-level belief estimates with
# the naive and EIV-corrected regression lines
#
# Created: Nico Rotundo 2026-07-10
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Source the errors-in-variables estimator
source("code/2_analysis/eivreg.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of firm-level audit gaps from the correspondence paper i.e., the regression LHS
# -----------------------------------------------------------------------------------------------------------------------------
# Load the Full_Sample coefficient sheet
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

# Should be 97 firms x 2 LHS variables = 194 observations remaining
stopifnot(nrow(firm_audit_gaps) == 194)

# Keep necessary variables
firm_audit_gaps <- firm_audit_gaps |> dplyr::select(entity_id, entity, outcome, estimate)

# Rename variables
firm_audit_gaps <- firm_audit_gaps |> dplyr::rename(firm_id = entity_id, firm_name = entity, contact_gap_type = outcome, difference_in_log_contact_rate = estimate)

# Relabel the contact-gap type to be more descriptive
firm_audit_gaps <- firm_audit_gaps |> dplyr::mutate(contact_gap_type = dplyr::recode(contact_gap_type, "log_dif" = "white_minus_black", "log_dif_gender" = "male_minus_female"))

# Should be unique by firm and contact gap type
stopifnot(!anyDuplicated(firm_audit_gaps[c("firm_id", "contact_gap_type")]), !anyNA(firm_audit_gaps[c("firm_id", "contact_gap_type")]))

# Reshape wide to one row per firm, one column per contact-gap type
firm_audit_gaps <- firm_audit_gaps |> tidyr::pivot_wider(id_cols = c(firm_id, firm_name), names_from = contact_gap_type, values_from = difference_in_log_contact_rate, names_prefix = "dif_log_contact_rate_")

# Should be one row per firm, none missing
stopifnot(!anyDuplicated(firm_audit_gaps$firm_id), !anyNA(firm_audit_gaps))

# -----------------------------------------------------------------------------------------------------------------------------
# Construct dataset of raw firm-level beliefs i.e., the regression RHS
# -----------------------------------------------------------------------------------------------------------------------------
# Load the Full_Sample coefficient sheet
raw_firm_beliefs <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "Coefficients")

# Uniquely identified by sample x aggregation model x outcome x entity type x entity
stopifnot(!anyDuplicated(raw_firm_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(raw_firm_beliefs[c("subset", "model", "outcome", "entity_type", "entity_id")]))

# Keep the raw belief estimates i.e., firm-level Likert OLS without the estimation-sample-mean recentering
raw_firm_beliefs <- raw_firm_beliefs |> dplyr::filter(model == "OLS_not_recentered")

# Keep just the two pooled belief measures
raw_firm_beliefs <- raw_firm_beliefs |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male"))

# Keep the full 164-firm sample
raw_firm_beliefs <- raw_firm_beliefs |> dplyr::filter(subset == "all")

# Should be just firm-level observations
stopifnot(all(raw_firm_beliefs$entity_type == "Firm"))

# Should be 164 firms x 2 RHS variables = 328 observations remaining
stopifnot(nrow(raw_firm_beliefs) == 328)

# Keep necessary variables
raw_firm_beliefs <- raw_firm_beliefs |> dplyr::select(entity_id, outcome, estimate, njobs)

# Rename variables to be more descriptive
raw_firm_beliefs <- raw_firm_beliefs |> dplyr::rename(firm_id = entity_id, belief_measure = outcome, belief_estimate = estimate, number_of_jobs = njobs)

# Check that number of jobs is constant within each firm
stopifnot(nrow(dplyr::distinct(raw_firm_beliefs, firm_id, number_of_jobs)) == dplyr::n_distinct(raw_firm_beliefs$firm_id))

# Reshape wide to one row per firm, one column per belief measure
raw_firm_beliefs <- raw_firm_beliefs |> tidyr::pivot_wider(id_cols = c(firm_id, number_of_jobs), names_from = belief_measure, values_from = belief_estimate)

# Should be one row per firm with no missing beliefs; number of jobs is missing only for the 67 firms outside the audit sample
stopifnot(!anyDuplicated(raw_firm_beliefs$firm_id), !anyNA(raw_firm_beliefs[c("firm_id", "pooled_favor_white", "pooled_favor_male")]))

# -----------------------------------------------------------------------------------------------------------------------------
# Merge the audit gaps and raw beliefs into one firm-level estimation dataset
# -----------------------------------------------------------------------------------------------------------------------------
# Merge raw beliefs onto the audit gaps 1:1 (one audit-gap row per firm : one belief row per firm)
    # relationship = "one-to-one" enforces both sides are unique on firm_id; errors otherwise
    # left_join keeps the 97 audit firms and appends the belief columns as new variables; the beliefs' 164 firms are a superset of the 97, so the extras drop
firm_estimation_data <- firm_audit_gaps |> dplyr::left_join(raw_firm_beliefs, by = "firm_id", relationship = "one-to-one")

# assert(2 3) i.e., every audit firm matched a belief row (no _merge==1); the extra belief firms (_merge==2) are expected
stopifnot(all(firm_estimation_data$firm_id %in% raw_firm_beliefs$firm_id))

# Should be 97 audit firms
stopifnot(nrow(firm_estimation_data) == 97)

# Order variables as firm id, name, number of jobs, everything else
firm_estimation_data <- firm_estimation_data |> dplyr::relocate(firm_id, firm_name, number_of_jobs)

# Should be uniquely identified by firm_id, none missing
stopifnot(!anyDuplicated(firm_estimation_data$firm_id), !anyNA(firm_estimation_data$firm_id))

# Check no variables are missing i.e., every audit firm carries both gaps, both beliefs, and the job weight
stopifnot(!any(is.na(firm_estimation_data)))

# -----------------------------------------------------------------------------------------------------------------------------
# Estimate the naive regression line and pull the EIV regression line for each contact gap x belief measure panel
# -----------------------------------------------------------------------------------------------------------------------------
# Load the firm-level EIV regression results sheet
eiv_regression_results <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "EIV_firm")

# Uniquely identified by aggregation model x LHS x RHS formula x RHS variable x FE tag
stopifnot(!anyDuplicated(eiv_regression_results[c("model", "lhs", "formula", "rhs", "coef")]), !anyNA(eiv_regression_results[c("model", "lhs", "formula", "rhs", "coef")]))

# Convert estimation data to a data frame because R is insane
firm_estimation_data <- as.data.frame(firm_estimation_data)

# Define empty regression line estimates dataframe
regression_line_estimates <- data.frame()

# Loop over each LHS contact gap
for (lhs_variable in c("dif_log_contact_rate_white_minus_black", "dif_log_contact_rate_male_minus_female")) {

    # RHS belief implied by the LHS contact gap; race gap uses the white-favoritism belief, gender gap the male-favoritism belief
    rhs_variable <- c(dif_log_contact_rate_white_minus_black = "pooled_favor_white", dif_log_contact_rate_male_minus_female = "pooled_favor_male")[[lhs_variable]]

    # Run the naive number-of-jobs-weighted regression of the contact gap on the raw beliefs
        # zero noise matrix, so eivreg applies no measurement-error correction but computes the same robust standard errors as the EIV pipeline
    naive_regression <- eivreg(as.formula(paste0(lhs_variable, " ~ ", rhs_variable)), data = firm_estimation_data, Sigma_error = matrix(0, dimnames = list(rhs_variable, rhs_variable)), weights = number_of_jobs)

    # Pull the EIV row from the pipeline results: Likert OLS beliefs, bivariate spec, no-FE fit (coef == 1)
    eiv_regression_row <- eiv_regression_results |> dplyr::filter(model == "OLS", lhs == c(dif_log_contact_rate_white_minus_black = "log_dif", dif_log_contact_rate_male_minus_female = "log_dif_gender")[[lhs_variable]], formula == rhs_variable, rhs == rhs_variable, coef == 1)

    # Should be exactly one EIV row
    stopifnot(nrow(eiv_regression_row) == 1)

    # Number-of-jobs-weighted means of the contact gap and belief i.e., the weighted centroid both fitted lines pass through
    weighted_mean_contact_gap <- weighted.mean(firm_estimation_data[[lhs_variable]], firm_estimation_data$number_of_jobs)
    weighted_mean_belief <- weighted.mean(firm_estimation_data[[rhs_variable]], firm_estimation_data$number_of_jobs)

    # Check the naive intercept equals the weighted-centroid backout i.e., the property used to back out the EIV intercept below
    stopifnot(abs(naive_regression$coefficients[["(Intercept)"]] - (weighted_mean_contact_gap - naive_regression$coefficients[[rhs_variable]] * weighted_mean_belief)) < 1e-10)

    # Append the naive and EIV lines for this panel
        # EIV intercept is not stored in the pipeline, so back it out from the weighted centroid; the intercept gets no noise correction, so the weighted residuals sum to zero and the EIV line passes through the weighted means
    regression_line_estimates <- rbind(regression_line_estimates, data.frame(
        lhs_variable = lhs_variable,
        rhs_variable = rhs_variable,
        regression_line = c("naive", "eiv"),
        slope = c(naive_regression$coefficients[[rhs_variable]], eiv_regression_row$sample_est),
        slope_se = c(sqrt(naive_regression$vcov[rhs_variable, rhs_variable]), eiv_regression_row$sample_se),
        intercept = c(naive_regression$coefficients[["(Intercept)"]], weighted_mean_contact_gap - eiv_regression_row$sample_est * weighted_mean_belief)
    ))
}

# Should be 2 panels x 2 regression lines = 4 rows, none missing
stopifnot(nrow(regression_line_estimates) == 4, !anyNA(regression_line_estimates))

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the naive vs EIV fitted scatterplots
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each LHS contact gap
for (lhs_variable in c("dif_log_contact_rate_white_minus_black", "dif_log_contact_rate_male_minus_female")) {

    # RHS belief implied by the LHS contact gap; race gap uses the white-favoritism belief, gender gap the male-favoritism belief
    rhs_variable <- c(dif_log_contact_rate_white_minus_black = "pooled_favor_white", dif_log_contact_rate_male_minus_female = "pooled_favor_male")[[lhs_variable]]

    # Restrict the line estimates to this panel
    panel_line_estimates <- regression_line_estimates |> dplyr::filter(lhs_variable == .env$lhs_variable)

    # Should be the naive and EIV lines
    stopifnot(nrow(panel_line_estimates) == 2)

    # Assign the naive line estimates
    naive_line <- panel_line_estimates |> dplyr::filter(regression_line == "naive")

    # Assign the EIV line estimates
    eiv_line <- panel_line_estimates |> dplyr::filter(regression_line == "eiv")

    # Define the scatterplot of contact gaps on raw beliefs with both fitted lines
    eiv_scatterplot <- ggplot(firm_estimation_data, aes(x = .data[[rhs_variable]], y = .data[[lhs_variable]])) +

        # Observed firm-level belief-gap pairs
        geom_point(color = "darkorange", size = 2.2, alpha = 0.7) +

        # Naive weighted-OLS fitted line
        geom_abline(intercept = naive_line$intercept, slope = naive_line$slope, color = "grey40", linewidth = 0.7, linetype = "dashed") +

        # EIV fitted line
        geom_abline(intercept = eiv_line$intercept, slope = eiv_line$slope, color = "steelblue", linewidth = 0.7) +

        # Naive slope and standard error annotation, color-matched to the naive line
        annotation_custom(
            grid::textGrob(
                label = bquote("Naive slope" == .(paste0(formatC(naive_line$slope, digits = 3, format = "f"), " (", formatC(naive_line$slope_se, digits = 3, format = "f"), ")"))),
                x = grid::unit(0.015, "npc"),
                y = grid::unit(0.975, "npc"),
                hjust = 0,
                vjust = 1,
                gp = grid::gpar(fontsize = 11, col = "grey40")
            )
        ) +

        # EIV slope and standard error annotation, color-matched to the EIV line
        annotation_custom(
            grid::textGrob(
                label = bquote("EIV slope" == .(paste0(formatC(eiv_line$slope, digits = 3, format = "f"), " (", formatC(eiv_line$slope_se, digits = 3, format = "f"), ")"))),
                x = grid::unit(0.015, "npc"),
                y = grid::unit(0.930, "npc"),
                hjust = 0,
                vjust = 1,
                gp = grid::gpar(fontsize = 11, col = "steelblue")
            )
        ) +

        # Axis labels name the belief measure and the contact gap
        labs(
            x = c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)")[[rhs_variable]],
            y = c(dif_log_contact_rate_white_minus_black = "Difference in log contact rate (White − Black)", dif_log_contact_rate_male_minus_female = "Difference in log contact rate (Male − Female)")[[lhs_variable]]
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

    # Export the scatterplot, one file per contact gap x belief measure panel
    ggsave(file.path(figures, paste0("eiv_scatterplots_y_", lhs_variable, "_x_", rhs_variable, ".png")), plot = eiv_scatterplot, width = 10, height = 6, dpi = 300, bg = "white")
}