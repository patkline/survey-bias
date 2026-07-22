# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: For each survey measure, plot the Likert and Borda ratings for the 25 highest and 25 lowest 
# firms ranked by the Borda empirical Bayes rating
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

# Keep the survey measures plotted i.e., the three pooled discrimination measures, their contact and conduct arm versions, and the two firm characteristics
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(outcome %in% c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger", "FirmSelective", "discretion"))

# Should be 164 firms x 2 aggregation methods x 9 survey measures = 2952 observations remaining
stopifnot(nrow(firm_level_rating_estimates) == 164 * 2 * 9)

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
# Compute the common y axis shared by the discrimination figures
# -----------------------------------------------------------------------------------------------------------------------------
# Discrimination survey measures i.e., the figures drawn on one common y axis
discrimination_survey_measure_vector <- c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger")

# Define vector to store every discrimination figure's plotted ratings
discrimination_plotted_ratings <- c()

# Loop over each discrimination survey measure, collecting the ratings its figure plots
for (survey_measure in discrimination_survey_measure_vector) {
  # Keep this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  discrimination_working_data <- firm_level_rating_estimates |> dplyr::select(likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes")))

  # Keep the 25 firms with the lowest and the 25 firms with the highest Borda ratings
  discrimination_working_data <- discrimination_working_data |> dplyr::filter(rank(borda_empirical_bayes_rating) <= 25 | rank(borda_empirical_bayes_rating) > 139)

  # Rescale the Borda ratings onto the Likert axis, matching the two series' plotted means and standard deviations
  discrimination_working_data <- discrimination_working_data |> dplyr::mutate(borda_empirical_bayes_rating_rescaled = mean(likert_empirical_bayes_rating) + sd(likert_empirical_bayes_rating) / sd(borda_empirical_bayes_rating) * (borda_empirical_bayes_rating - mean(borda_empirical_bayes_rating)))

  # Append the plotted Likert ratings and rescaled Borda ratings
  discrimination_plotted_ratings <- c(discrimination_plotted_ratings, discrimination_working_data$likert_empirical_bayes_rating, discrimination_working_data$borda_empirical_bayes_rating_rescaled)
}

# Compute the common y-axis tick positions from every discrimination figure's plotted ratings
discrimination_axis_break_positions <- pretty(range(discrimination_plotted_ratings))

# Define the common y-axis limits
discrimination_axis_limits <- c(1.9, 4.1)

# Every plotted rating should sit inside the common y-axis limits
stopifnot(all(dplyr::between(discrimination_plotted_ratings, discrimination_axis_limits[1], discrimination_axis_limits[2])))

# -----------------------------------------------------------------------------------------------------------------------------
# Plot figure for each survey measure
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over each survey measure, drawing one figure per measure
for (survey_measure in c("pooled_favor_white", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_male", "FirmCont_favor_male", "conduct_favor_male", "conduct_favor_younger", "FirmSelective", "discretion")) {
  # Keep the firm identifiers and this survey measure's Likert and Borda ratings, renamed to survey-measure-generic names
  top_bottom_plot_working_data <- firm_level_rating_estimates |> dplyr::select(firm_id, firm, likert_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_ols_not_recentered_empirical_bayes")), borda_empirical_bayes_rating = dplyr::all_of(paste0(survey_measure, "_borda_not_recentered_empirical_bayes")))

  # Keep the 25 firms with the lowest and the 25 firms with the highest Borda ratings
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::filter(rank(borda_empirical_bayes_rating) <= 25 | rank(borda_empirical_bayes_rating) > 139)

  # Should be 50 firms remaining
  stopifnot(nrow(top_bottom_plot_working_data) == 50)

  # Order the firms ascending by Borda rating
  top_bottom_plot_working_data <- top_bottom_plot_working_data |> dplyr::arrange(borda_empirical_bayes_rating)

  # Callout list of the five lowest-rated firms, ascending
  bottom_five_callout_label <- paste(top_bottom_plot_working_data$firm[1:5], collapse = "\n")

  # Callout list of the five highest-rated firms, descending i.e., rank first
  top_five_callout_label <- paste(top_bottom_plot_working_data$firm[50:46], collapse = "\n")

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

  # Discrimination figures' plotted ratings should sit inside the shared y-axis limits, since the panel draws without clipping
  if (survey_measure %in% discrimination_survey_measure_vector) stopifnot(all(dplyr::between(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), discrimination_axis_limits[1], discrimination_axis_limits[2]), na.rm = TRUE))

  # Compute the y-axis tick positions, shared by both axes so the tick marks align; discrimination figures share the common positions
  likert_axis_break_positions <- if (survey_measure %in% discrimination_survey_measure_vector) discrimination_axis_break_positions else pretty(range(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), na.rm = TRUE))

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

    # Allow the angled firm names to render outside the panel; discrimination figures share the common y-axis limits, and the
    # other figures pin the plotted-data range so the callout annotations cannot stretch the panel between the two exported versions
    ggplot2::coord_cartesian(ylim = if (survey_measure %in% discrimination_survey_measure_vector) discrimination_axis_limits else range(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), na.rm = TRUE), clip = "off")

  # Compute the panel y-axis limits i.e., the shared discrimination limits, or the plotted range with ggplot's 5% expansion
  panel_axis_limits <- if (survey_measure %in% discrimination_survey_measure_vector) discrimination_axis_limits else range(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), na.rm = TRUE) + c(-1, 1) * 0.05 * diff(range(c(top_bottom_plot_working_data$likert_empirical_bayes_rating, top_bottom_plot_working_data$borda_empirical_bayes_rating_rescaled), na.rm = TRUE))

  # Compute the panel y-axis span, scaling the callout anchor offsets
  panel_axis_span <- diff(panel_axis_limits)

  # Compute the lowest plotted rating among the 25 top firms i.e., the ceiling for the top-five callout below the top block
  top_block_floor <- min(top_bottom_plot_working_data$rating_segment_lower[29:53], na.rm = TRUE)

  # Anchor the bottom-five callout header just under the panel top, over the bottom firms
  bottom_callout_header_y <- panel_axis_limits[2] - 0.01 * panel_axis_span

  # Anchor the bottom-five callout names below their header, clearing the underline
  bottom_callout_names_y <- bottom_callout_header_y - 0.068 * panel_axis_span

  # Anchor the top-five callout header below the top block
  top_callout_header_y <- top_block_floor - 0.095 * panel_axis_span

  # Flip the top-five callout to the panel top when a low top block would push its names into the legend; the name block extends about 0.26 spans below the header
  if (top_callout_header_y - 0.26 * panel_axis_span < panel_axis_limits[1] + 0.20 * panel_axis_span) top_callout_header_y <- panel_axis_limits[2] - 0.01 * panel_axis_span

  # Anchor the top-five callout names below their header, clearing the underline
  top_callout_names_y <- top_callout_header_y - 0.068 * panel_axis_span

  # Loop over the exported figure versions i.e., the base figure and the version with the top/bottom-five callouts
  for (figure_version_suffix in c("", "_with_callout")) {

    # Start each version from the base figure
    exported_top_bottom_figure <- top_bottom_figure

    # Add the underlined bottom-five and top-five firm-name callouts
    if (figure_version_suffix == "_with_callout") {
      exported_top_bottom_figure <- exported_top_bottom_figure +

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
    top_bottom_figure_layout <- ggplot2::ggplotGrob(exported_top_bottom_figure)

    # Pin the panel height, so the panel sits identically in every figure regardless of the firm name lengths below it
    top_bottom_figure_layout$heights[top_bottom_figure_layout$layout$t[top_bottom_figure_layout$layout$name == "panel"]] <- grid::unit(4.6, "in")

    # Anchor the layout to the top of the page, so the varying firm-name block below cannot shift the panel vertically
    top_bottom_figure_layout$vp <- grid::viewport(y = grid::unit(1, "npc"), just = "top", height = grid::grobHeight(top_bottom_figure_layout))

    # Open the exported figure file
    png(file.path(figures, paste0("top_bottom_firm_ratings_dual_axis_figures_", survey_measure, figure_version_suffix, ".png")), width = 16, height = 7.3, units = "in", res = 300, bg = "white")

    # Draw the pinned layout into the file
    grid::grid.draw(top_bottom_figure_layout)

    # Close the exported figure file
    dev.off()
  }
}