# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: For each survey measure, plot the Likert and Borda ratings for (1) the 25 highest and 25 lowest
# firms ranked by the Borda empirical Bayes within-industry deviation and (2) every industry
#
# Created: Nico Rotundo 2026-07-03
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Construct the firm-to-industry crosswalk
# -----------------------------------------------------------------------------------------------------------------------------
# Load the firm-to-industry crosswalk from the processed survey data, keeping distinct firm_id x industry code x industry name combinations
firm_industry_crosswalk <- read.csv(file.path(processed, "long_survey_final.csv")) |> dplyr::distinct(firm_id, aer_naics2, aer_naics2_name)

# One industry per firm, none missing
stopifnot(!anyDuplicated(firm_industry_crosswalk$firm_id), !anyNA(firm_industry_crosswalk))

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level rating estimates for industry aggregation
# -----------------------------------------------------------------------------------------------------------------------------
# Load the full-sample firm-level coefficient sheet
firm_level_rating_estimates <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "Coefficients")

# Uniquely identified by subset x aggregation model x survey measure x entity type x entity, none missing
stopifnot(!anyDuplicated(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]))

# Keep firm-level observations
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(entity_type == "Firm")

# Keep the full-sample estimates
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(subset == "all")

# Keep the non-recentered OLS and Borda observations i.e., the raw firm-level ratings
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

# Keep the survey measures plotted i.e., the three pooled discrimination measures and their contact and conduct arm versions
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(outcome %in% c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger"))

# Should be 164 firms x 2 aggregation methods x 7 survey measures = 2296 observations remaining
stopifnot(nrow(firm_level_rating_estimates) == 164 * 2 * 7)

# Rating estimates should be non-missing
stopifnot(!anyNA(firm_level_rating_estimates$estimate))

# Keep necessary variables
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::select(entity_id, entity, model, outcome, estimate)

# Rename variables to be more descriptive
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::rename(firm_id = entity_id, firm = entity, aggregation_method = model, survey_measure = outcome, rating_estimate = estimate)

# Attach each firm's industry code and industry name
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::left_join(firm_industry_crosswalk, by = "firm_id")

# Every firm should have an industry
stopifnot(!anyNA(firm_level_rating_estimates$aer_naics2))

# Sort by survey measure, aggregation method, and firm, fixing the row order the aggregation matrices are built on
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::arrange(survey_measure, aggregation_method, firm_id)

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level robust covariances for industry aggregation
# -----------------------------------------------------------------------------------------------------------------------------
# Firm ids sorted ascending, the order every estimate vector and covariance matrix is aligned to
firm_id_vector <- sort(unique(firm_level_rating_estimates$firm_id))

# Should be the 164 rated firms
stopifnot(length(firm_id_vector) == 164)

# Open the full-sample robust covariance sheet
firm_level_robust_covariances <- arrow::open_dataset(parquet_sheet_path(file.path(intermediate, "Full_Sample"), "rcov"))

# Keep the full-sample estimates
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(subset == "all")

# Keep the non-recentered OLS and Borda observations i.e., the raw firm-level covariances
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

# Keep the survey measures plotted i.e., the three pooled discrimination measures and their contact and conduct arm versions
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(outcome %in% c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger"))

# Keep the covariance entries among the rated firms
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(entity_id_i %in% firm_id_vector, entity_id_j %in% firm_id_vector)

# Collect the filtered covariance rows
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::collect()

# Should be 164 firms x 164 firms x 2 aggregation methods x 7 survey measures = 376544 firm pairs remaining
stopifnot(nrow(firm_level_robust_covariances) == 164 * 164 * 2 * 7)

# Uniquely identified by aggregation model x survey measure x firm pair, none missing
stopifnot(!anyDuplicated(firm_level_robust_covariances[c("model", "outcome", "entity_id_i", "entity_id_j")]), !anyNA(firm_level_robust_covariances))

# Keep necessary variables
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::select(model, outcome, entity_id_i, entity_id_j, rcov)

# Rename variables to be more descriptive
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::rename(aggregation_method = model, survey_measure = outcome, firm_id_i = entity_id_i, firm_id_j = entity_id_j, robust_covariance = rcov)

# -----------------------------------------------------------------------------------------------------------------------------
# Construct the industry aggregation matrices
# -----------------------------------------------------------------------------------------------------------------------------
# Crosswalk firms should be exactly the rated firms, so the crosswalk's industries are the plotted industries
stopifnot(setequal(firm_industry_crosswalk$firm_id, firm_id_vector))

# Industry codes sorted ascending, the order every industry-level object is aligned to
industry_id_vector <- sort(unique(firm_industry_crosswalk$aer_naics2))

# Define the firm-by-industry indicator matrix i.e., one row per firm, a 1 in its industry's column
industry_indicator_matrix <- 1 * outer(firm_industry_crosswalk$aer_naics2[match(firm_id_vector, firm_industry_crosswalk$firm_id)], industry_id_vector, FUN = "==")

# Label the indicator matrix rows by firm and columns by industry
dimnames(industry_indicator_matrix) <- list(as.character(firm_id_vector), as.character(industry_id_vector))

# Every firm should sit in exactly one industry
stopifnot(all(rowSums(industry_indicator_matrix) == 1))

# Define the industry aggregation matrix i.e., one row per industry, holding equal averaging weights on its firms
industry_aggregation_matrix <- solve(t(industry_indicator_matrix) %*% industry_indicator_matrix, t(industry_indicator_matrix))

# Every industry's averaging weights should sum to one
stopifnot(all(abs(rowSums(industry_aggregation_matrix) - 1) < 1e-12))

# Define the within-industry deviation matrix i.e., maps firm ratings to firm rating minus own-industry mean
within_industry_deviation_matrix <- diag(length(firm_id_vector)) - industry_indicator_matrix %*% industry_aggregation_matrix

# Deviations of a constant should be zero, so raw and recentered ratings give identical deviations
stopifnot(all(abs(within_industry_deviation_matrix %*% rep(1, length(firm_id_vector))) < 1e-12))

# -----------------------------------------------------------------------------------------------------------------------------
# Aggregate the firm-level ratings to industry means and within-industry deviations
# -----------------------------------------------------------------------------------------------------------------------------
# Load the empirical Bayes shrinkage function
source(file.path(analysis, "EB_procedure.R"))

# Define dataframe to store every industry-level rating estimate
industry_rating_estimates <- data.frame()

# Define dataframe to store every firm's within-industry rating deviation
within_industry_rating_deviations <- data.frame()

# Loop over each survey measure
for (survey_measure_value in c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger")) {
    # Loop over each aggregation method
    for (aggregation_method_value in c("OLS_not_recentered", "Borda_not_recentered")) {

        # Keep this survey measure's firm-level rating estimates
        working_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(survey_measure == survey_measure_value)

        # Keep this aggregation method
        working_rating_estimates <- working_rating_estimates |> dplyr::filter(aggregation_method == aggregation_method_value)

        # Check the firm_id order equals firm_id_vector
        stopifnot(nrow(working_rating_estimates) == length(firm_id_vector), all(working_rating_estimates$firm_id == firm_id_vector))

        # Keep this survey measure's firm-pair robust covariances
        working_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(survey_measure == survey_measure_value)

        # Keep this aggregation method
        working_robust_covariances <- working_robust_covariances |> dplyr::filter(aggregation_method == aggregation_method_value)

        # Should be 164 firms x 164 firms = 26896 firm pairs
        stopifnot(nrow(working_robust_covariances) == 164 * 164)

        # Define a matrix of 0s to hold this cell's robust covariance, rows and columns ordered by firm_id_vector
        firm_robust_covariance_matrix <- matrix(0, nrow = length(firm_id_vector), ncol = length(firm_id_vector), dimnames = list(as.character(firm_id_vector), as.character(firm_id_vector)))

        # Populate the robust covariance matrix from the firm-pair rows
        firm_robust_covariance_matrix[cbind(as.character(working_robust_covariances$firm_id_i), as.character(working_robust_covariances$firm_id_j))] <- working_robust_covariances$robust_covariance

        # Compute the industry mean ratings, one per industry
        industry_mean_rating_vector <- industry_aggregation_matrix %*% working_rating_estimates$rating_estimate

        # Compute the industry mean ratings' robust covariance i.e., pre- and post-multiply the firm-level covariance by the aggregation matrix
        industry_mean_robust_covariance_matrix <- industry_aggregation_matrix %*% firm_robust_covariance_matrix %*% t(industry_aggregation_matrix)

        # Compute each firm's within-industry rating deviation i.e., rating minus own-industry mean
        within_industry_deviation_vector <- within_industry_deviation_matrix %*% working_rating_estimates$rating_estimate

        # Compute the within-industry deviations' robust covariance i.e., pre- and post-multiply the firm-level covariance by the deviation matrix
        within_industry_deviation_robust_covariance_matrix <- within_industry_deviation_matrix %*% firm_robust_covariance_matrix %*% t(within_industry_deviation_matrix)

        # Shrink the industry mean ratings with two-step empirical Bayes, using each mean's robust standard error
        industry_mean_empirical_bayes_vector <- eb_two_step(theta_hat = as.numeric(industry_mean_rating_vector), s = sqrt(diag(industry_mean_robust_covariance_matrix)))$theta_eb

        # Shrink the within-industry deviations with two-step empirical Bayes, using each deviation's robust standard error
        within_industry_deviation_empirical_bayes_vector <- eb_two_step(theta_hat = as.numeric(within_industry_deviation_vector), s = sqrt(diag(within_industry_deviation_robust_covariance_matrix)))$theta_eb

        # Append this cell's industry mean ratings and their empirical Bayes estimates
        industry_rating_estimates <- rbind(industry_rating_estimates, data.frame(survey_measure = survey_measure_value, aggregation_method = aggregation_method_value, aer_naics2 = industry_id_vector, aer_naics2_name = firm_industry_crosswalk$aer_naics2_name[match(industry_id_vector, firm_industry_crosswalk$aer_naics2)], rating_estimate = as.numeric(industry_mean_rating_vector), empirical_bayes = industry_mean_empirical_bayes_vector))

        # Append this cell's within-industry rating deviations and their empirical Bayes estimates
        within_industry_rating_deviations <- rbind(within_industry_rating_deviations, data.frame(survey_measure = survey_measure_value, aggregation_method = aggregation_method_value, firm_id = working_rating_estimates$firm_id, firm = working_rating_estimates$firm, aer_naics2 = working_rating_estimates$aer_naics2, rating_deviation = as.numeric(within_industry_deviation_vector), empirical_bayes = within_industry_deviation_empirical_bayes_vector))
    }
}

# Should be 19 industries x 2 aggregation methods x 7 survey measures = 266 industry means, none missing
stopifnot(nrow(industry_rating_estimates) == 19 * 2 * 7, !anyNA(industry_rating_estimates))

# Should be 164 firms x 2 aggregation methods x 7 survey measures = 2296 deviations, none missing
stopifnot(nrow(within_industry_rating_deviations) == 164 * 2 * 7, !anyNA(within_industry_rating_deviations))

# Attach each survey measure x aggregation method's mean rating across the 164 rated firms
within_industry_rating_deviations <- within_industry_rating_deviations |> dplyr::left_join(firm_level_rating_estimates |> dplyr::group_by(survey_measure, aggregation_method) |> dplyr::summarise(mean_rating_across_firms = mean(rating_estimate), .groups = "drop"), by = c("survey_measure", "aggregation_method"))

# Add the cross-firm mean rating back to the empirical Bayes deviations, putting the plotted values on the rating scale
within_industry_rating_deviations <- within_industry_rating_deviations |> dplyr::mutate(empirical_bayes_plus_mean_across_firms = empirical_bayes + mean_rating_across_firms)

# Keep necessary variables
within_industry_rating_deviations <- within_industry_rating_deviations |> dplyr::select(firm_id, firm, aer_naics2, survey_measure, aggregation_method, empirical_bayes_plus_mean_across_firms)

# Reshape to one row per firm, one column per survey measure x aggregation method plotted rating
within_industry_rating_deviations <- within_industry_rating_deviations |> tidyr::pivot_wider(names_from = c(survey_measure, aggregation_method), values_from = empirical_bayes_plus_mean_across_firms, names_glue = "{survey_measure}_{tolower(aggregation_method)}_empirical_bayes_plus_mean_across_firms")

# Should be one row per firm
stopifnot(nrow(within_industry_rating_deviations) == 164)

# Every firm should have a plotted rating for every survey measure x aggregation method column
stopifnot(!anyNA(within_industry_rating_deviations))

# Borda ratings should be distinct within every survey measure, so the rank-based top/bottom cuts are unambiguous
stopifnot(all(sapply(within_industry_rating_deviations |> dplyr::select(dplyr::ends_with("_borda_not_recentered_empirical_bayes_plus_mean_across_firms")), function(rating_column) !anyDuplicated(rating_column))))

# Keep necessary variables
industry_rating_estimates <- industry_rating_estimates |> dplyr::select(aer_naics2, aer_naics2_name, survey_measure, aggregation_method, empirical_bayes)

# Reshape to one row per industry, one column per survey measure x aggregation method empirical Bayes rating
industry_rating_estimates <- industry_rating_estimates |> tidyr::pivot_wider(names_from = c(survey_measure, aggregation_method), values_from = empirical_bayes, names_glue = "{survey_measure}_{tolower(aggregation_method)}_empirical_bayes")

# Should be one row per industry
stopifnot(nrow(industry_rating_estimates) == 19)

# Every industry should have a rating for every survey measure x aggregation method column
stopifnot(!anyNA(industry_rating_estimates))

# Borda ratings should be distinct within every survey measure, so the industry ordering is unambiguous
stopifnot(all(sapply(industry_rating_estimates |> dplyr::select(dplyr::ends_with("_borda_not_recentered_empirical_bayes")), function(rating_column) !anyDuplicated(rating_column))))

# -----------------------------------------------------------------------------------------------------------------------------
# Define the common y axis shared with the top/bottom discrimination figures
# -----------------------------------------------------------------------------------------------------------------------------
# Define the common y-axis tick positions, fixed to the top/bottom discrimination figures' ticks
discrimination_axis_break_positions <- seq(2, 4, by = 0.5)

# Define the common y-axis limits, fixed to the top/bottom discrimination figures' limits
discrimination_axis_limits <- c(1.9, 4.1)

# Define vector to store every within-industry figure's plotted ratings
within_industry_plotted_ratings <- c()

# Loop over each survey measure, collecting the ratings its figure plots
for (survey_measure in c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger")) {
  # Keep this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  within_industry_working_data <- within_industry_rating_deviations |> dplyr::select(likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes_plus_mean_across_firms")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes_plus_mean_across_firms")))

  # Keep the 25 firms with the lowest and the 25 firms with the highest Borda ratings
  within_industry_working_data <- within_industry_working_data |> dplyr::filter(rank(borda_empirical_bayes_rating) <= 25 | rank(borda_empirical_bayes_rating) > 139)

  # Rescale the Borda ratings onto the Likert axis, matching the two series' plotted means and standard deviations
  within_industry_working_data <- within_industry_working_data |> dplyr::mutate(borda_empirical_bayes_rating_rescaled = mean(likert_empirical_bayes_rating) + sd(likert_empirical_bayes_rating) / sd(borda_empirical_bayes_rating) * (borda_empirical_bayes_rating - mean(borda_empirical_bayes_rating)))

  # Append the plotted Likert ratings and rescaled Borda ratings
  within_industry_plotted_ratings <- c(within_industry_plotted_ratings, within_industry_working_data$likert_empirical_bayes_rating, within_industry_working_data$borda_empirical_bayes_rating_rescaled)
}

# Every plotted rating should sit inside the common y-axis limits
stopifnot(all(dplyr::between(within_industry_plotted_ratings, discrimination_axis_limits[1], discrimination_axis_limits[2])))

# -----------------------------------------------------------------------------------------------------------------------------
# Plot within-industry figure for each survey measure
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each survey measure, drawing one within-industry figure per measure
for (survey_measure in c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger")) {
  # Keep the firm identifiers and this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  within_industry_plot_working_data <- within_industry_rating_deviations |> dplyr::select(firm_id, firm, likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes_plus_mean_across_firms")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes_plus_mean_across_firms")))

  # Keep the 25 firms with the lowest and the 25 firms with the highest Borda ratings
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::filter(rank(borda_empirical_bayes_rating) <= 25 | rank(borda_empirical_bayes_rating) > 139)

  # Should be 50 firms remaining
  stopifnot(nrow(within_industry_plot_working_data) == 50)

  # Order the firms ascending by Borda rating
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::arrange(borda_empirical_bayes_rating)

  # Callout list of the five lowest-rated firms, ascending
  bottom_five_callout_label <- paste(within_industry_plot_working_data$firm[1:5], collapse = "\n")

  # Callout list of the five highest-rated firms, descending i.e., rank first
  top_five_callout_label <- paste(within_industry_plot_working_data$firm[50:46], collapse = "\n")

  # Insert three unplotted spacer rows between the bottom and top firms, hidden from the axis labels
  within_industry_plot_working_data <- within_industry_plot_working_data |> tibble::add_row(firm = paste0("__gap", 1:3, "__"), .after = 25)

  # Fix the x-axis firm order to the row order i.e., bottom firms, spacer, top firms
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::mutate(firm = factor(firm, levels = firm, ordered = TRUE))

  # Compute the factor rescaling the Borda ratings onto the Likert axis i.e., the ratio of the two plotted standard deviations
  borda_to_likert_scale_factor <- sd(within_industry_plot_working_data$likert_empirical_bayes_rating, na.rm = TRUE) / sd(within_industry_plot_working_data$borda_empirical_bayes_rating, na.rm = TRUE)

  # Scale factor should be finite and positive
  stopifnot(is.finite(borda_to_likert_scale_factor), borda_to_likert_scale_factor > 0)

  # Compute the mean of the plotted Likert ratings, centering the rescaled Borda ratings
  plotted_likert_rating_mean <- mean(within_industry_plot_working_data$likert_empirical_bayes_rating, na.rm = TRUE)

  # Compute the mean of the plotted Borda ratings, centering the rescaled Borda ratings
  plotted_borda_rating_mean <- mean(within_industry_plot_working_data$borda_empirical_bayes_rating, na.rm = TRUE)

  # Rescale the Borda ratings onto the Likert axis, matching the two series' plotted means and standard deviations
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::mutate(borda_empirical_bayes_rating_rescaled = plotted_likert_rating_mean + borda_to_likert_scale_factor * (borda_empirical_bayes_rating - plotted_borda_rating_mean))

  # Compute each firm's lower vertical connector bound i.e., the smaller of its Likert and rescaled Borda ratings
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::mutate(rating_segment_lower = pmin(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Compute each firm's upper vertical connector bound i.e., the larger of its Likert and rescaled Borda ratings
  within_industry_plot_working_data <- within_industry_plot_working_data |> dplyr::mutate(rating_segment_upper = pmax(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Plotted ratings should sit inside the shared y-axis limits, since the panel draws without clipping
  stopifnot(all(dplyr::between(c(within_industry_plot_working_data$likert_empirical_bayes_rating, within_industry_plot_working_data$borda_empirical_bayes_rating_rescaled), discrimination_axis_limits[1], discrimination_axis_limits[2]), na.rm = TRUE))

  # Define the within-industry figure
  within_industry_figure <- ggplot2::ggplot(within_industry_plot_working_data, ggplot2::aes(x = firm)) +

    # Vertical connector segment between each firm's Likert and rescaled Borda ratings
    ggplot2::geom_segment(ggplot2::aes(xend = firm, y = rating_segment_lower, yend = rating_segment_upper), linewidth = 0.3, alpha = 0.35) +

    # Likert rating points
    ggplot2::geom_point(ggplot2::aes(y = likert_empirical_bayes_rating, color = "Likert Score"), size = 2.6, alpha = 0.9) +

    # Likert rating line across firms
    ggplot2::geom_line(ggplot2::aes(y = likert_empirical_bayes_rating, color = "Likert Score", group = 1), linewidth = 0.7, alpha = 0.9) +

    # Rescaled Borda rating points
    ggplot2::geom_point(ggplot2::aes(y = borda_empirical_bayes_rating_rescaled, color = "Borda Score"), size = 2.6, alpha = 0.9, shape = 17) +

    # Rescaled Borda rating line across firms
    ggplot2::geom_line(ggplot2::aes(y = borda_empirical_bayes_rating_rescaled, color = "Borda Score", group = 1), linewidth = 0.7, alpha = 0.9, linetype = "dashed") +

    # Vertical line opening the spacer gap after the 25 bottom firms
    ggplot2::geom_vline(xintercept = 25.5, linetype = "dashed", linewidth = 0.6, color = "grey55") +

    # Vertical line closing the spacer gap before the 25 top firms
    ggplot2::geom_vline(xintercept = 28.5, linetype = "dashed", linewidth = 0.6, color = "grey55") +

    # Primary axis for the Likert ratings and secondary axis unwinding the rescaled Borda ratings, with the
    # secondary tick marks placed at the primary tick positions and relabeled in Borda units
    ggplot2::scale_y_continuous(
      name = "Likert Score (EB)",
      breaks = discrimination_axis_break_positions,
      sec.axis = ggplot2::sec_axis(
        ~ (. - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
        name = "Borda Score (EB)",
        breaks = (discrimination_axis_break_positions - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
        labels = function(borda_axis_value) formatC(borda_axis_value, digits = 2, format = "f")
      )
    ) +

    # Assign each aggregation method's color, keeping the legend in Likert-then-Borda order
    ggplot2::scale_color_manual(
      values = c("Likert Score" = "steelblue", "Borda Score" = "darkorange"),
      breaks = c("Likert Score", "Borda Score")
    ) +

    # Match the legend key glyphs to each series' point shape and line type
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(
        shape     = c(16, 17),
        linetype  = c("solid", "dashed"),
        linewidth = c(0.7, 0.7),
        alpha     = c(0.9, 0.9)
      )
    )) +

    # Axis titles, with no figure title or legend title
    ggplot2::labs(title = "", x = "Firm (sorted by Borda EB within-industry deviation)", color = "") +

    # Theme baseline
    ggplot2::theme_minimal(base_size = 14) +

    # Theme adjustments
    ggplot2::theme(
      # Angled firm names on the x axis
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),

      # Thin tick marks on the two y axes only
      axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks.x      = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(7, "pt"),

      # Legend anchored inside the bottom-right corner
      legend.position      = c(0.985, 0.03),
      legend.justification = c(1, 0),

      # Legend styling: no title, small black text, translucent white background
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(size = 8, color = "black"),
      legend.key        = ggplot2::element_blank(),
      legend.key.height = grid::unit(10, "pt"),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.85), color = NA),

      # No grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # White backgrounds with no panel border
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),

      # Bottom, left, and right axis spines, leaving the top open
      axis.line.x.bottom = ggplot2::element_line(color = "black"),
      axis.line.y.left   = ggplot2::element_line(color = "black"),
      axis.line.y.right  = ggplot2::element_line(color = "black"),

      # Margins leaving room for the angled firm names and the axis titles
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 130, l = 90)
    ) +

    # Blank the spacer rows' x-axis labels and pad the axis ends
    ggplot2::scale_x_discrete(labels = function(firm_name) ifelse(grepl("^__gap\\d+__$", firm_name), "", firm_name), expand = ggplot2::expansion(add = 0.8)) +

    # Allow the angled firm names to render outside the panel, on the shared y-axis limits
    ggplot2::coord_cartesian(ylim = discrimination_axis_limits, clip = "off")

  # Compute the panel y-axis span, scaling the callout anchor offsets
  panel_axis_span <- diff(discrimination_axis_limits)

  # Compute the lowest plotted rating among the 25 top firms i.e., the ceiling for the top-five callout below the top block
  top_block_floor <- min(within_industry_plot_working_data$rating_segment_lower[29:53], na.rm = TRUE)

  # Anchor the bottom-five callout header just under the panel top, over the bottom firms
  bottom_callout_header_y <- discrimination_axis_limits[2] - 0.01 * panel_axis_span

  # Anchor the bottom-five callout names below their header, clearing the underline
  bottom_callout_names_y <- bottom_callout_header_y - 0.068 * panel_axis_span

  # Anchor the top-five callout header below the top block
  top_callout_header_y <- top_block_floor - 0.095 * panel_axis_span

  # Flip the top-five callout to the panel top when a low top block would push its names into the legend; the name block extends about 0.26 spans below the header
  if (top_callout_header_y - 0.26 * panel_axis_span < discrimination_axis_limits[1] + 0.20 * panel_axis_span) top_callout_header_y <- discrimination_axis_limits[2] - 0.01 * panel_axis_span

  # Anchor the top-five callout names below their header, clearing the underline
  top_callout_names_y <- top_callout_header_y - 0.068 * panel_axis_span

  # Loop over the exported figure versions i.e., the base figure and the version with the top/bottom-five callouts
  for (figure_version_suffix in c("", "_with_callout")) {

    # Start each version from the base figure
    exported_within_industry_figure <- within_industry_figure

    # Add the underlined bottom-five and top-five firm-name callouts
    if (figure_version_suffix == "_with_callout") {
      exported_within_industry_figure <- exported_within_industry_figure +

        # Underlined bottom-five header over the bottom firms
        ggplot2::annotate("text", x = 5, y = bottom_callout_header_y, label = "underline('Bottom 5 (ascending)')", parse = TRUE, size = 4.2, vjust = 1) +

        # Bottom-five firm names, ascending
        ggplot2::annotate("text", x = 5, y = bottom_callout_names_y, label = bottom_five_callout_label, size = 4.2, lineheight = 0.95, vjust = 1) +

        # Underlined top-five header below the top firms
        ggplot2::annotate("text", x = 48, y = top_callout_header_y, label = "underline('Top 5 (descending)')", parse = TRUE, size = 4.2, vjust = 1) +

        # Top-five firm names, descending
        ggplot2::annotate("text", x = 48, y = top_callout_names_y, label = top_five_callout_label, size = 4.2, lineheight = 0.95, vjust = 1)
    }

    # Convert the figure to its grid layout i.e., the arrangement of panel, axes, labels, and margins
    within_industry_figure_layout <- ggplot2::ggplotGrob(exported_within_industry_figure)

    # Pin the panel height, so the panel sits identically in every figure regardless of the firm name lengths below it
    within_industry_figure_layout$heights[within_industry_figure_layout$layout$t[within_industry_figure_layout$layout$name == "panel"]] <- grid::unit(4.6, "in")

    # Anchor the layout to the top of the page, so the varying firm-name block below cannot shift the panel vertically
    within_industry_figure_layout$vp <- grid::viewport(y = grid::unit(1, "npc"), just = "top", height = grid::grobHeight(within_industry_figure_layout))

    # Open the exported figure file
    png(file.path(figures, paste0("industry_ratings_dual_axis_figures_within_", survey_measure, figure_version_suffix, ".png")), width = 16, height = 7.3, units = "in", res = 300, bg = "white")

    # Draw the pinned layout into the file
    grid::grid.draw(within_industry_figure_layout)

    # Close the exported figure file
    dev.off()
  }
}

# -----------------------------------------------------------------------------------------------------------------------------
# Plot between-industry figure for each survey measure
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each survey measure, drawing one between-industry figure per measure
for (survey_measure in c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger")) {
  # Keep the industry identifiers and this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  between_industry_plot_working_data <- industry_rating_estimates |> dplyr::select(aer_naics2, aer_naics2_name, likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes")))

  # Order the industries ascending by Borda rating
  between_industry_plot_working_data <- between_industry_plot_working_data |> dplyr::arrange(borda_empirical_bayes_rating)

  # Fix the x-axis industry order to the row order
  between_industry_plot_working_data <- between_industry_plot_working_data |> dplyr::mutate(aer_naics2_name = factor(aer_naics2_name, levels = aer_naics2_name, ordered = TRUE))

  # Compute the factor rescaling the Borda ratings onto the Likert axis i.e., the ratio of the two plotted standard deviations
  borda_to_likert_scale_factor <- sd(between_industry_plot_working_data$likert_empirical_bayes_rating) / sd(between_industry_plot_working_data$borda_empirical_bayes_rating)

  # Scale factor should be finite and positive
  stopifnot(is.finite(borda_to_likert_scale_factor), borda_to_likert_scale_factor > 0)

  # Compute the mean of the plotted Likert ratings, centering the rescaled Borda ratings
  plotted_likert_rating_mean <- mean(between_industry_plot_working_data$likert_empirical_bayes_rating)

  # Compute the mean of the plotted Borda ratings, centering the rescaled Borda ratings
  plotted_borda_rating_mean <- mean(between_industry_plot_working_data$borda_empirical_bayes_rating)

  # Rescale the Borda ratings onto the Likert axis, matching the two series' plotted means and standard deviations
  between_industry_plot_working_data <- between_industry_plot_working_data |> dplyr::mutate(borda_empirical_bayes_rating_rescaled = plotted_likert_rating_mean + borda_to_likert_scale_factor * (borda_empirical_bayes_rating - plotted_borda_rating_mean))

  # Compute each industry's lower vertical connector bound i.e., the smaller of its Likert and rescaled Borda ratings
  between_industry_plot_working_data <- between_industry_plot_working_data |> dplyr::mutate(rating_segment_lower = pmin(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Compute each industry's upper vertical connector bound i.e., the larger of its Likert and rescaled Borda ratings
  between_industry_plot_working_data <- between_industry_plot_working_data |> dplyr::mutate(rating_segment_upper = pmax(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Plotted ratings should sit inside the shared y-axis limits, since the panel draws without clipping
  stopifnot(all(dplyr::between(c(between_industry_plot_working_data$likert_empirical_bayes_rating, between_industry_plot_working_data$borda_empirical_bayes_rating_rescaled), discrimination_axis_limits[1], discrimination_axis_limits[2])))

  # Define the between-industry figure
  between_industry_figure <- ggplot2::ggplot(between_industry_plot_working_data, ggplot2::aes(x = aer_naics2_name)) +

    # Vertical connector segment between each industry's Likert and rescaled Borda ratings
    ggplot2::geom_segment(ggplot2::aes(xend = aer_naics2_name, y = rating_segment_lower, yend = rating_segment_upper), linewidth = 0.3, alpha = 0.35) +

    # Likert rating points
    ggplot2::geom_point(ggplot2::aes(y = likert_empirical_bayes_rating, color = "Likert Score"), size = 2.6, alpha = 0.9) +

    # Likert rating line across industries
    ggplot2::geom_line(ggplot2::aes(y = likert_empirical_bayes_rating, color = "Likert Score", group = 1), linewidth = 0.7, alpha = 0.9) +

    # Rescaled Borda rating points
    ggplot2::geom_point(ggplot2::aes(y = borda_empirical_bayes_rating_rescaled, color = "Borda Score"), size = 2.6, alpha = 0.9, shape = 17) +

    # Rescaled Borda rating line across industries
    ggplot2::geom_line(ggplot2::aes(y = borda_empirical_bayes_rating_rescaled, color = "Borda Score", group = 1), linewidth = 0.7, alpha = 0.9, linetype = "dashed") +

    # Primary axis for the Likert ratings and secondary axis unwinding the rescaled Borda ratings, with the
    # secondary tick marks placed at the primary tick positions and relabeled in Borda units
    ggplot2::scale_y_continuous(
      name = "Likert Score (EB)",
      breaks = discrimination_axis_break_positions,
      sec.axis = ggplot2::sec_axis(
        ~ (. - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
        name = "Borda Score (EB)",
        breaks = (discrimination_axis_break_positions - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
        labels = function(borda_axis_value) formatC(borda_axis_value, digits = 2, format = "f")
      )
    ) +

    # Assign each aggregation method's color, keeping the legend in Likert-then-Borda order
    ggplot2::scale_color_manual(
      values = c("Likert Score" = "steelblue", "Borda Score" = "darkorange"),
      breaks = c("Likert Score", "Borda Score")
    ) +

    # Match the legend key glyphs to each series' point shape and line type
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(
        shape     = c(16, 17),
        linetype  = c("solid", "dashed"),
        linewidth = c(0.7, 0.7),
        alpha     = c(0.9, 0.9)
      )
    )) +

    # Axis titles, with no figure title or legend title
    ggplot2::labs(title = "", x = "Industry (sorted by Borda EB)", color = "") +

    # Theme baseline
    ggplot2::theme_minimal(base_size = 14) +

    # Theme adjustments
    ggplot2::theme(
      # Angled industry names on the x axis
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),

      # Thin tick marks on the two y axes only
      axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks.x      = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(7, "pt"),

      # Legend anchored inside the bottom-right corner
      legend.position      = c(0.985, 0.03),
      legend.justification = c(1, 0),

      # Legend styling: no title, small black text, translucent white background
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(size = 8, color = "black"),
      legend.key        = ggplot2::element_blank(),
      legend.key.height = grid::unit(10, "pt"),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.85), color = NA),

      # No grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # White backgrounds with no panel border
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),

      # Bottom, left, and right axis spines, leaving the top open
      axis.line.x.bottom = ggplot2::element_line(color = "black"),
      axis.line.y.left   = ggplot2::element_line(color = "black"),
      axis.line.y.right  = ggplot2::element_line(color = "black"),

      # Margins leaving room for the angled industry names and the axis titles
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 130, l = 90)
    ) +

    # Pad the axis ends
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.8)) +

    # Allow the angled industry names to render outside the panel, on the shared y-axis limits
    ggplot2::coord_cartesian(ylim = discrimination_axis_limits, clip = "off")

  # Convert the figure to its grid layout i.e., the arrangement of panel, axes, labels, and margins
  between_industry_figure_layout <- ggplot2::ggplotGrob(between_industry_figure)

  # Pin the panel height, so the panel sits identically in every figure regardless of the industry name lengths below it
  between_industry_figure_layout$heights[between_industry_figure_layout$layout$t[between_industry_figure_layout$layout$name == "panel"]] <- grid::unit(4.6, "in")

  # Anchor the layout to the top of the page, so the varying industry-name block below cannot shift the panel vertically
  between_industry_figure_layout$vp <- grid::viewport(y = grid::unit(1, "npc"), just = "top", height = grid::grobHeight(between_industry_figure_layout))

  # Open the exported figure file
  png(file.path(figures, paste0("industry_ratings_dual_axis_figures_between_", survey_measure, ".png")), width = 16, height = 7.3, units = "in", res = 300, bg = "white")

  # Draw the pinned layout into the file
  grid::grid.draw(between_industry_figure_layout)

  # Close the exported figure file
  dev.off()
}