# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Top/bottom firm rating figures --- for each survey measure, plot the 25 highest and 25 lowest
# firms ranked by the Borda empirical Bayes rating, with the Likert empirical Bayes rating on the primary
# axis and the Borda empirical Bayes rating on the secondary axis, rescaled to share the primary scale
#
# Created: Monica Essig Aberg 2026-05-14
# Edited: Nico Rotundo 2026-07-03
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level rating estimates for plotting
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

# Keep the survey measures plotted
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger", "FirmSelective", "discretion"))

# Should be 164 firms x 2 aggregation methods x 5 survey measures = 1640 observations remaining
stopifnot(nrow(firm_level_rating_estimates) == 164 * 2 * 5)

# Empirical Bayes ratings should be non-missing
stopifnot(!anyNA(firm_level_rating_estimates$eb))

# Keep necessary variables
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::select(entity_id, entity, model, outcome, eb)

# Rename variables to be more descriptive
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::rename(firm_id = entity_id, firm = entity, aggregation_method = model, survey_measure = outcome, empirical_bayes_rating = eb)

# Reshape to one row per firm, one column per survey measure x aggregation method empirical Bayes rating
firm_level_rating_estimates <- firm_level_rating_estimates |> tidyr::pivot_wider(names_from = c(survey_measure, aggregation_method), values_from = empirical_bayes_rating, names_glue = "{survey_measure}_{tolower(aggregation_method)}_empirical_bayes")

# Should be one row per firm 
stopifnot(nrow(firm_level_rating_estimates) == 164)

# Every firm should have a rating for every survey measure x aggregation method column
stopifnot(!anyNA(firm_level_rating_estimates))

# Borda ratings should be distinct within every survey measure, so the rank-based top/bottom cuts are unambiguous
stopifnot(all(sapply(firm_level_rating_estimates |> dplyr::select(dplyr::ends_with("_borda_not_recentered_empirical_bayes")), function(rating_column) !anyDuplicated(rating_column))))

# -----------------------------------------------------------------------------------------------------------------------------
# Plot figure for each survey measure 
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each survey measure, drawing one figure per measure
for (survey_measure in c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger", "FirmSelective", "discretion")) {
  # Keep the firm identifiers and this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  top_bottom_plot_working_data <- firm_level_rating_estimates |> dplyr::select(firm_id, firm, likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes")))

  # Keep the 25 firms with the lowest and the 25 firms with the highest Borda ratings
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::filter(rank(borda_empirical_bayes_rating) <= 25 | rank(borda_empirical_bayes_rating) > 139)

  # Should be 50 firms remaining
  stopifnot(nrow(top_bottom_plot_working_data) == 50)

  # Order the firms ascending by Borda rating
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::arrange(borda_empirical_bayes_rating)

  # Insert three unplotted spacer rows between the bottom and top firms, hidden from the axis labels
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> tibble::add_row(firm = paste0("__gap", 1:3, "__"), .after = 25)

  # Fix the x-axis firm order to the row order i.e., bottom firms, spacer, top firms
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::mutate(firm = factor(firm, levels = firm, ordered = TRUE))

  # Compute the factor rescaling the Borda ratings onto the Likert axis i.e., the ratio of the two plotted standard deviations
  borda_to_likert_scale_factor <- sd(top_bottom_plot_working_data$likert_empirical_bayes_rating, na.rm = TRUE) / sd(top_bottom_plot_working_data$borda_empirical_bayes_rating, na.rm = TRUE)

  # Scale factor should be finite and positive
  stopifnot(is.finite(borda_to_likert_scale_factor), borda_to_likert_scale_factor > 0)

  # Compute the mean of the plotted Likert ratings, centering the rescaled Borda ratings
  plotted_likert_rating_mean <- mean(top_bottom_plot_working_data$likert_empirical_bayes_rating, na.rm = TRUE)

  # Compute the mean of the plotted Borda ratings, centering the rescaled Borda ratings
  plotted_borda_rating_mean <- mean(top_bottom_plot_working_data$borda_empirical_bayes_rating, na.rm = TRUE)

  # Rescale the Borda ratings onto the Likert axis, matching the two series' plotted means and standard deviations
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::mutate(borda_empirical_bayes_rating_rescaled = plotted_likert_rating_mean + borda_to_likert_scale_factor * (borda_empirical_bayes_rating - plotted_borda_rating_mean))

  # Compute each firm's lower vertical connector bound i.e., the smaller of its Likert and rescaled Borda ratings
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::mutate(rating_segment_lower = pmin(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Compute each firm's upper vertical connector bound i.e., the larger of its Likert and rescaled Borda ratings
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::mutate(rating_segment_upper = pmax(likert_empirical_bayes_rating, borda_empirical_bayes_rating_rescaled))

  # Compute the y-axis tick positions from the plotted ratings, shared by both axes so the tick marks align
  likert_axis_break_positions <- pretty(range(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), na.rm = TRUE))

  # Define the top/bottom figure
  top_bottom_figure <- ggplot2::ggplot(top_bottom_plot_working_data, ggplot2::aes(x = firm)) +

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
      breaks = likert_axis_break_positions,
      sec.axis = ggplot2::sec_axis(
        ~ (. - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
        name = "Borda Score (EB)",
        breaks = (likert_axis_break_positions - plotted_likert_rating_mean) / borda_to_likert_scale_factor + plotted_borda_rating_mean,
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
    ggplot2::labs(title = "", x = "Firm (sorted by Borda EB)", color = "") +

    # Theme baseline
    ggplot2::theme_minimal(base_size = 14) +

    # Theme adjustments
    ggplot2::theme(
      # Angled firm names on the x axis
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),

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
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 80, l = 90)
    ) +

    # Blank the spacer rows' x-axis labels and pad the axis ends
    ggplot2::scale_x_discrete(labels = function(firm_name) ifelse(grepl("^__gap\\d+__$", firm_name), "", firm_name), expand = ggplot2::expansion(add = 0.8)) +

    # Allow the angled firm names to render outside the panel
    ggplot2::coord_cartesian(clip = "off")

  # Export the figure
  ggplot2::ggsave(file.path(figures, paste0("top_bottom_firm_ratings_dual_axis_figures_", survey_measure, ".png")), top_bottom_figure, width = 16, height = 8, dpi = 300)
}