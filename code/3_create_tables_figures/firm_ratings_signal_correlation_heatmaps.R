# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Combined triangular heatmaps of the debiased cross-firm pairwise correlations between 
# survey measures
#
# Created: Jordan Cammarota
# Edited: Nico Rotundo 2026-07-09
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
# Import each correlation sheet and build its heatmap-cell dataframe --- one row per off-diagonal axis
# position, with Likert correlations below the diagonal and Borda correlations above it
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over correlation sheets
for (correlation_sheet in c("correlation", "correlation_between_industry", "correlation_within_industry")) {

    # Import into a dataframe
    sheet_correlations <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), correlation_sheet)

    # Assert one row per survey measure pair x aggregation method x firm subset
    stopifnot(!anyDuplicated(sheet_correlations[c("lhs", "rhs", "model", "subset")]), !anyNA(sheet_correlations[c("lhs", "rhs", "model", "subset")]))

    # Restrict to the given sheet's firm subset
    #XXNR: sub in the "all" restriction for all sheets once the unweighted between/within-industry correlations are built
    sheet_correlations <- sheet_correlations[sheet_correlations$subset == c("correlation" = "all", "correlation_between_industry" = "subset97", "correlation_within_industry" = "subset97")[[correlation_sheet]], ]

    # Assert one row per survey measure pair x aggregation method
    stopifnot(!anyDuplicated(sheet_correlations[c("lhs", "rhs", "model")]), !anyNA(sheet_correlations[c("lhs", "rhs", "model")]))

    # Define the sheet's measure-name suffix,
    measure_suffix <- c("correlation" = "", "correlation_between_industry" = "_im_w", "correlation_within_industry" = "_dm_w")[[correlation_sheet]]

    # Assert every survey measure name carries the sheet's suffix
    stopifnot(all(endsWith(sheet_correlations$lhs, measure_suffix)), all(endsWith(sheet_correlations$rhs, measure_suffix)))

    # Strip the survey measure so all sheets share the same survey measure name
    sheet_correlations$lhs <- sub(paste0(measure_suffix, "$"), "", sheet_correlations$lhs)
    sheet_correlations$rhs <- sub(paste0(measure_suffix, "$"), "", sheet_correlations$rhs)

    # Split out the cross-model rows i.e., the Likert x Borda signal correlation of each survey measure, shown on the diagonal
    sheet_diagonal_correlations <- sheet_correlations[sheet_correlations$model == "OLS_x_Borda", ]
    sheet_correlations <- sheet_correlations[sheet_correlations$model != "OLS_x_Borda", ]

    # Should be cross-model rows present, each pairing one survey measure with itself
    stopifnot(nrow(sheet_diagonal_correlations) > 0, all(sheet_diagonal_correlations$lhs == sheet_diagonal_correlations$rhs))

    # Should be the same estimation sample behind both aggregation methods of each survey measure
    stopifnot(all(sheet_diagonal_correlations$Ncommon == sheet_diagonal_correlations$N1), all(sheet_diagonal_correlations$Ncommon == sheet_diagonal_correlations$N2))

    # Keep necessary variables, renamed to be clearer
    sheet_correlations <- sheet_correlations |>
        dplyr::select(survey_measure_1 = lhs, survey_measure_2 = rhs, aggregation_method = model,
                      signal_correlation = corr_c)

    # Stack the flipped pair orientation so each survey measure pair appears in both directions
    sheet_correlations <- dplyr::bind_rows(
        sheet_correlations,
        dplyr::rename(sheet_correlations, survey_measure_1 = survey_measure_2, survey_measure_2 = survey_measure_1)
    )

    # Assert one row per ordered survey measure pair x aggregation method
    stopifnot(!anyDuplicated(sheet_correlations[c("survey_measure_1", "survey_measure_2", "aggregation_method")]), !anyNA(sheet_correlations[c("survey_measure_1", "survey_measure_2", "aggregation_method")]))

    # Keep the set of survey measures shown on the heatmaps
    sheet_correlations <- sheet_correlations[sheet_correlations$survey_measure_1 %in% names(survey_measure_display_names) & sheet_correlations$survey_measure_2 %in% names(survey_measure_display_names), ]
    
    # Should be 10 survey measures x 9 other survey measures x 2 aggregation methods = 180 rows
    stopifnot(nrow(sheet_correlations) == 10 * 9 * 2)

    # Map each survey measure to its axis position; the column from measure 1, the row (top to bottom) from measure 2
    sheet_correlations$column_position <- match(survey_measure_display_names[sheet_correlations$survey_measure_1], survey_measure_display_order)
    sheet_correlations$row_position <- match(survey_measure_display_names[sheet_correlations$survey_measure_2], survey_measure_display_order)

    # Keep Likert cells below the diagonal and Borda cells above it
    sheet_correlations <- sheet_correlations[
        (sheet_correlations$aggregation_method == "OLS" & sheet_correlations$row_position > sheet_correlations$column_position) |
        (sheet_correlations$aggregation_method == "Borda" & sheet_correlations$row_position < sheet_correlations$column_position), ]

    # Assert one cell per off-diagonal axis position
    stopifnot(nrow(sheet_correlations) == 10 * 9, !anyDuplicated(sheet_correlations[c("row_position", "column_position")]), !anyNA(sheet_correlations[c("row_position", "column_position")]))

    # Keep necessary diagonal variables, renamed to match the off-diagonal cells
    sheet_diagonal_correlations <- sheet_diagonal_correlations |>
        dplyr::select(survey_measure_1 = lhs, survey_measure_2 = rhs, aggregation_method = model, signal_correlation = corr_c)

    # Keep the set of survey measures shown on the heatmaps
    sheet_diagonal_correlations <- sheet_diagonal_correlations[sheet_diagonal_correlations$survey_measure_1 %in% names(survey_measure_display_names), ]

    # Should be one diagonal cell per displayed survey measure, none missing
    stopifnot(nrow(sheet_diagonal_correlations) == 10, !anyNA(sheet_diagonal_correlations$signal_correlation))

    # Map each diagonal cell to its axis position i.e., row equals column
    sheet_diagonal_correlations$column_position <- match(survey_measure_display_names[sheet_diagonal_correlations$survey_measure_1], survey_measure_display_order)
    sheet_diagonal_correlations$row_position <- sheet_diagonal_correlations$column_position

    # Append the diagonal cells to the off-diagonal cells
    sheet_correlations <- dplyr::bind_rows(sheet_correlations, sheet_diagonal_correlations)

    # Assert one cell per axis position: 90 off-diagonal plus 10 diagonal
    stopifnot(nrow(sheet_correlations) == 10 * 9 + 10, !anyDuplicated(sheet_correlations[c("row_position", "column_position")]), !anyNA(sheet_correlations[c("row_position", "column_position")]))

    # Store the dataframe under the figure-family name
    assign(paste0("heatmap_cells_", c("correlation" = "full_sample", "correlation_between_industry" = "between_industry", "correlation_within_industry" = "within_industry")[[correlation_sheet]]), sheet_correlations)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Define one specification per heatmap figure --- the input dataframe, the export-name suffix, and the purple highlight cells in 1-indexed matrix coordinates (row = top to bottom, col = left to right)
# -----------------------------------------------------------------------------------------------------------------------------
heatmap_figure_specifications <- list(
    # Full-sample heatmap
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample", highlight_cells = NULL),
    
    # Full-sample heatmap highlighting the Black within-measure block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_1", highlight_cells = data.frame(row = c(2, 3, 3), col = c(1, 1, 2))),
    
    # Full-sample heatmap highlighting the Female within-measure block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_2", highlight_cells = data.frame(row = c(5, 6, 6), col = c(4, 4, 5))),
    
    # Full-sample heatmap highlighting the Female x Black cross-measure block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_3", highlight_cells = data.frame(row = rep(4:6, each = 3), col = rep(1:3, times = 3))),
    
    # Full-sample heatmap highlighting the firm-characteristics x Black block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_4", highlight_cells = data.frame(row = rep(8:10, each = 3), col = rep(1:3, times = 3))),
    
    # Full-sample heatmap highlighting the Older x Black and Older x Female cells
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_5", highlight_cells = data.frame(row = rep(7, times = 6), col = 1:6)),

    # Full-sample heatmap highlighting the firm-characteristics x Female block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_6", highlight_cells = data.frame(row = rep(8:10, each = 3), col = rep(4:6, times = 3))),

    # Full-sample heatmap highlighting the firm-characteristics within-block
    list(heatmap_cells_dataframe = "heatmap_cells_full_sample", export_suffix = "full_sample_highlight_7", highlight_cells = data.frame(row = rep(8:10, each = 3), col = rep(8:10, times = 3))),

    # Between-industry heatmap
    list(heatmap_cells_dataframe = "heatmap_cells_between_industry", export_suffix = "between_industry", highlight_cells = NULL),
    
    # Within-industry heatmap
    list(heatmap_cells_dataframe = "heatmap_cells_within_industry", export_suffix = "within_industry", highlight_cells = NULL)
)

# -----------------------------------------------------------------------------------------------------------------------------
# Draw each heatmap figure
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over figure specifications
for (heatmap_figure_specification in heatmap_figure_specifications) {

    # Pull the figure's heatmap cells
    figure_cells <- get(heatmap_figure_specification$heatmap_cells_dataframe)

    # Convert each survey measure to its display-name factor; y levels reversed so axis row 1 sits at the top
    figure_cells$display_measure_1 <- factor(survey_measure_display_names[figure_cells$survey_measure_1], levels = survey_measure_display_order)
    figure_cells$display_measure_2 <- factor(survey_measure_display_names[figure_cells$survey_measure_2], levels = rev(survey_measure_display_order))

    # Draw the heatmap; correlation-filled tiles and cell values
    figure_heatmap <- ggplot(figure_cells, aes(x = display_measure_1, y = display_measure_2, fill = signal_correlation)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, name = "Correlation", na.value = "white") +
        geom_text(aes(label = sprintf("%.2f", signal_correlation)), size = 3) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.85, size = 10, margin = margin(t = 24)),
            axis.text.y = element_text(size = 10),
            legend.position = "right",
            panel.grid = element_blank()
        ) +
        labs(title = "", x = NULL, y = NULL)

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
    stopifnot(file.copy(heatmap_temporary_png, file.path(figures, paste0("firm_ratings_signal_correlation_heatmaps_", heatmap_figure_specification$export_suffix, ".png")), overwrite = TRUE))

    # Delete the temporary file
    unlink(heatmap_temporary_png)

    # Report the export
    message("🎃 Exported firm_ratings_signal_correlation_heatmaps_", heatmap_figure_specification$export_suffix, ".png")
}
