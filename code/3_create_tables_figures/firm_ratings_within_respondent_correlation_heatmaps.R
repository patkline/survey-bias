# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Triangular heatmaps of within-respondent correlations between survey measures, under two
# estimators computed on the same respondents (all with at least two firms rated on both questions):
#   - ratio_of_averages: the equal-weighted average across respondents of each respondent's covariance
#     across the firms they rated on both questions, divided by the square root of the product of the
#     corresponding average within-respondent variances
#   - average_of_correlations: the equal-weighted average across respondents of each respondent's
#     Pearson correlation, over the respondents for whom it is defined (nonconstant scores on both)
#
# Layout matches firm_ratings_signal_correlation_heatmaps.R: Likert cells below the diagonal, Borda
# cells above it. Two structural differences from the firm-level figure:
#   - The diagonal is blank. Rankings are dense-ranked from each respondent's own Likert ratings, so
#     within a respondent the Likert x Borda correlation sits near one by construction.
#   - Grey cells are infeasible: the conduct and FirmHire/FirmCont question modules were never
#     assigned to the same respondent, so no within-respondent correlation exists for those pairs.
#
# Created: Evan K. Rose 2026-08-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Define the label names for the survey measures shown on the heatmaps + their order
# -----------------------------------------------------------------------------------------------------------------------------
# Map each survey measure to its display name
survey_measure_display_names <- c(
    "conduct_favor_white"   = "Discrimination Black (Conduct)",
    "FirmHire_favor_white"  = "Discrimination Black (Hire)",
    "FirmCont_favor_white"  = "Discrimination Black (Contact)",
    "conduct_favor_male"    = "Discrimination Female (Conduct)",
    "FirmHire_favor_male"   = "Discrimination Female (Hire)",
    "FirmCont_favor_male"   = "Discrimination Female (Contact)",
    "conduct_favor_younger" = "Discrimination Older (Conduct)",
    "FirmDesire"            = "Firm Desirability",
    "FirmSelective"         = "Firm Selectivity",
    "discretion"            = "Manager Discretion"
)

# Order the display names as they appear on the axes, top-left to bottom-right
survey_measure_display_order <- unname(survey_measure_display_names)

# -----------------------------------------------------------------------------------------------------------------------------
# Import the within-respondent correlation sheet and build the heatmap-cell dataframe --- one row per
# off-diagonal axis position carrying both estimators, with Likert cells below the diagonal and Borda
# cells above it
# -----------------------------------------------------------------------------------------------------------------------------
sheet_correlations <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "correlation_within_respondent") |>
    dplyr::filter(
        subset == "all",
        model %in% c("OLS", "Borda"),
        lhs %in% names(survey_measure_display_names),
        rhs %in% names(survey_measure_display_names)
    )

# Assert one row per unordered survey measure pair x aggregation method
stopifnot(nrow(sheet_correlations) == 10 * 9 / 2 * 2)
stopifnot(!anyDuplicated(sheet_correlations[c("lhs", "rhs", "model")]), !anyNA(sheet_correlations[c("lhs", "rhs", "model")]))

# Assert both estimators use the same two-firm overlap requirement
stopifnot(all(sheet_correlations$min_common_firms == 2L))

# The infeasible pairs: the conduct module and the FirmHire/FirmCont modules were never assigned to the
# same respondent, so those cells have no within-respondent correlation
conduct_measures <- c("conduct_favor_white", "conduct_favor_male", "conduct_favor_younger")
hire_contact_measures <- c("FirmHire_favor_white", "FirmCont_favor_white", "FirmHire_favor_male", "FirmCont_favor_male")
is_infeasible_pair <- (sheet_correlations$lhs %in% conduct_measures & sheet_correlations$rhs %in% hire_contact_measures) |
    (sheet_correlations$lhs %in% hire_contact_measures & sheet_correlations$rhs %in% conduct_measures)

# Assert the missing correlations under both estimators are exactly the infeasible pairs, in both triangles
stopifnot(all(is.finite(sheet_correlations$corr_c) == !is_infeasible_pair))
stopifnot(all(is.finite(sheet_correlations$mean_respondent_corr) == !is_infeasible_pair))
stopifnot(sum(is_infeasible_pair) == length(conduct_measures) * length(hire_contact_measures) * 2)

# Every computed correlation must be valid under both estimators
stopifnot(all(dplyr::between(sheet_correlations$corr_c[!is_infeasible_pair], -1, 1)))
stopifnot(all(dplyr::between(sheet_correlations$mean_respondent_corr[!is_infeasible_pair], -1, 1)))

# Keep necessary variables, renamed to be clearer
sheet_correlations <- sheet_correlations |>
    dplyr::select(survey_measure_1 = lhs, survey_measure_2 = rhs, aggregation_method = model,
                  ratio_of_averages = corr_c, average_of_correlations = mean_respondent_corr)

# Stack the flipped pair orientation so each survey measure pair appears in both directions
sheet_correlations <- dplyr::bind_rows(
    sheet_correlations,
    dplyr::rename(sheet_correlations, survey_measure_1 = survey_measure_2, survey_measure_2 = survey_measure_1)
)

# Map each survey measure to its axis position; the column from measure 1, the row (top to bottom) from measure 2
sheet_correlations$column_position <- match(survey_measure_display_names[sheet_correlations$survey_measure_1], survey_measure_display_order)
sheet_correlations$row_position <- match(survey_measure_display_names[sheet_correlations$survey_measure_2], survey_measure_display_order)

# Keep Likert cells below the diagonal and Borda cells above it; the diagonal stays blank
sheet_correlations <- sheet_correlations[
    (sheet_correlations$aggregation_method == "OLS" & sheet_correlations$row_position > sheet_correlations$column_position) |
    (sheet_correlations$aggregation_method == "Borda" & sheet_correlations$row_position < sheet_correlations$column_position), ]

# Assert one cell per off-diagonal axis position
stopifnot(nrow(sheet_correlations) == 10 * 9, !anyDuplicated(sheet_correlations[c("row_position", "column_position")]), !anyNA(sheet_correlations[c("row_position", "column_position")]))

# Convert each survey measure to its display-name factor; y levels reversed so axis row 1 sits at the top
sheet_correlations$display_measure_1 <- factor(survey_measure_display_names[sheet_correlations$survey_measure_1], levels = survey_measure_display_order)
sheet_correlations$display_measure_2 <- factor(survey_measure_display_names[sheet_correlations$survey_measure_2], levels = rev(survey_measure_display_order))

# Shared fill limits across both estimators so identical values render identically in every figure
shared_fill_limits <- range(c(sheet_correlations$ratio_of_averages, sheet_correlations$average_of_correlations), na.rm = TRUE)

# -----------------------------------------------------------------------------------------------------------------------------
# Define the estimator versions and, per version, one specification per heatmap figure --- the
# export-name suffix and the purple highlight cells in 1-indexed matrix coordinates (row = top to
# bottom, col = left to right)
# -----------------------------------------------------------------------------------------------------------------------------
estimator_specifications <- list(
    list(
        estimator_column = "ratio_of_averages",
        export_prefix    = "ratio_of_averages",
        caption_line     = "Cell values divide the average within-respondent covariance by the square root of the product of the average within-respondent variances."
    ),
    list(
        estimator_column = "average_of_correlations",
        export_prefix    = "average_of_correlations",
        caption_line     = "Cell values are equal-weighted averages of respondent-level Pearson correlations, over respondents for whom they are defined."
    )
)

heatmap_figure_specifications <- list(
    # Within-respondent heatmap
    list(export_suffix = "main", highlight_cells = NULL),

    # Within-respondent heatmap highlighting the race-discrimination x Firm Selectivity cells in both triangles
    list(export_suffix = "highlight_race_selectivity", highlight_cells = data.frame(row = c(9, 9, 9, 1, 2, 3), col = c(1, 2, 3, 9, 9, 9)))
)

# -----------------------------------------------------------------------------------------------------------------------------
# Draw each heatmap figure for each estimator
# -----------------------------------------------------------------------------------------------------------------------------
for (estimator_specification in estimator_specifications) {

    # Pull this estimator's cell values
    figure_cells <- sheet_correlations
    figure_cells$cell_value <- figure_cells[[estimator_specification$estimator_column]]

    # Assemble the figure note: the estimator definition plus the shared structural notes
    figure_caption <- paste0(
        estimator_specification$caption_line, "\n",
        "All respondents with at least two firms rated on both questions contribute. Grey cells: question modules never assigned to the same respondents.\n",
        "Diagonal blank: rankings are constructed from each respondent's own ratings, so within-respondent Likert x Borda correlations sit near one by construction."
    )

    # Loop over figure specifications
    for (heatmap_figure_specification in heatmap_figure_specifications) {

        # Draw the heatmap; correlation-filled tiles with infeasible cells in grey, and cell values on feasible cells only
        figure_heatmap <- ggplot(figure_cells, aes(x = display_measure_1, y = display_measure_2, fill = cell_value)) +
            geom_tile(color = "white") +
            scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, name = "Correlation",
                                 na.value = "grey80", limits = shared_fill_limits) +
            geom_text(data = figure_cells[is.finite(figure_cells$cell_value), ],
                      aes(label = sprintf("%.2f", cell_value)), size = 3) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.85, size = 10, margin = margin(t = 24)),
                axis.text.y = element_text(size = 10),
                legend.position = "right",
                panel.grid = element_blank(),
                plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12))
            ) +
            labs(title = "", x = NULL, y = NULL, caption = figure_caption)

        # Outline the union of the highlight cells when the figure specifies them
        if (!is.null(heatmap_figure_specification$highlight_cells)) {

            # Convert the highlight cells from matrix coordinates to plot coordinates
            highlight_horizontal_positions <- heatmap_figure_specification$highlight_cells$col
            highlight_vertical_positions <- 10 - heatmap_figure_specification$highlight_cells$row + 1

            # Assert every highlight cell is unique and sits on the heatmap
            stopifnot(!anyDuplicated(paste(highlight_horizontal_positions, highlight_vertical_positions)), all(highlight_horizontal_positions %in% 1:10), all(highlight_vertical_positions %in% 1:10))

            # Build the four tile edges of every highlight cell
            highlight_edges <- rbind(
                # Left edges
                data.frame(horizontal_start = highlight_horizontal_positions - 0.5, vertical_start = highlight_vertical_positions - 0.5,
                           horizontal_end   = highlight_horizontal_positions - 0.5, vertical_end   = highlight_vertical_positions + 0.5),
                # Right edges
                data.frame(horizontal_start = highlight_horizontal_positions + 0.5, vertical_start = highlight_vertical_positions - 0.5,
                           horizontal_end   = highlight_horizontal_positions + 0.5, vertical_end   = highlight_vertical_positions + 0.5),
                # Bottom edges
                data.frame(horizontal_start = highlight_horizontal_positions - 0.5, vertical_start = highlight_vertical_positions - 0.5,
                           horizontal_end   = highlight_horizontal_positions + 0.5, vertical_end   = highlight_vertical_positions - 0.5),
                # Top edges
                data.frame(horizontal_start = highlight_horizontal_positions - 0.5, vertical_start = highlight_vertical_positions + 0.5,
                           horizontal_end   = highlight_horizontal_positions + 0.5, vertical_end   = highlight_vertical_positions + 0.5)
            )

            # Keep edges appearing exactly once; an edge shared by two highlight cells is interior to the union
            highlight_edges <- highlight_edges |>
                dplyr::group_by(horizontal_start, vertical_start, horizontal_end, vertical_end) |>
                dplyr::filter(dplyr::n() == 1) |>
                dplyr::ungroup()

            # Add the purple boundary outline to the heatmap
            figure_heatmap <- figure_heatmap +
                geom_segment(data = highlight_edges, aes(x = horizontal_start, y = vertical_start, xend = horizontal_end, yend = vertical_end),
                             inherit.aes = FALSE, color = "purple", linewidth = 2, lineend = "square")
        }

        # Widen the outside margins, allow drawing beyond the panel, and add one aggregation-method arrow per triangle
        figure_heatmap <- figure_heatmap +
            theme(plot.margin = margin(t = 40, r = 24, b = 190, l = 12)) +
            coord_cartesian(clip = "off") +
            annotate("text", x = (10 + 1) / 2, y = Inf, label = "Borda ⟶", vjust = -0.7, hjust = 0.5, size = 5, fontface = "bold") +
            annotate("text", x = (10 + 1) / 2, y = -Inf, label = "⟵ Likert", vjust = 1.4, hjust = 0.5, size = 5, fontface = "bold")

        # Define a temporary export path; writing PNGs directly into the Dropbox figures directory intermittently
        # exposes zero-byte files mid-write
        heatmap_temporary_png <- tempfile(fileext = ".png")

        # Save the heatmap
        ggsave(heatmap_temporary_png, figure_heatmap, width = 12, height = 10, dpi = 300, bg = "white")

        # Trim the whitespace around the heatmap in place
        magick::image_write(magick::image_trim(magick::image_read(heatmap_temporary_png)), path = heatmap_temporary_png)

        # Copy the trimmed heatmap into the figures directory, asserting the copy succeeds
        heatmap_export_name <- paste0("firm_ratings_within_respondent_correlation_heatmaps_", estimator_specification$export_prefix, "_", heatmap_figure_specification$export_suffix, ".png")
        stopifnot(file.copy(heatmap_temporary_png, file.path(figures, heatmap_export_name), overwrite = TRUE))

        # Delete the temporary file
        unlink(heatmap_temporary_png)

        # Report the export
        message("🎃 Exported ", heatmap_export_name)
    }
}
