# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Combined triangular heatmaps of the debiased cross-firm pairwise correlations between 
# survey measures
#
# Created: Jordan Cammarota
# Edited: Nico Rotundo 2026-07-09
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")
source(file.path(analysis, "katz_correct.R"))
source(file.path(analysis, "eiv_functions.R"))
source(file.path(analysis, "correlation_function.R"))

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
# Build the full-sample heatmap-cell dataframe --- one row per off-diagonal axis position,
# with Likert correlations below the diagonal and Borda correlations above it
# -----------------------------------------------------------------------------------------------------------------------------
selected_outcomes <- names(survey_measure_display_names)

# Build only the 100 correlations displayed in Figure 6. This applies Katz jointly
# to each pair's two variances and covariance instead of using stored scalar-Katz ratios.
variance_input <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "variance")
covariance_input <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "covariance") |>
    dplyr::filter(
        subset == "all",
        (model %in% c("OLS", "Borda") & lhs %in% selected_outcomes & rhs %in% selected_outcomes) |
            (model == "OLS_x_Borda" & lhs == rhs & lhs %in% selected_outcomes)
    )
stopifnot(nrow(covariance_input) == 10 * 9 + 10)
sheet_correlations <- build_correlation_from_varcov(
  var_df = variance_input,
  cov_df = covariance_input
)
stopifnot(all(dplyr::between(sheet_correlations$corr_c, -1, 1)))

    # Assert one row per survey measure pair x aggregation method
    stopifnot(!anyDuplicated(sheet_correlations[c("lhs", "rhs", "model")]), !anyNA(sheet_correlations[c("lhs", "rhs", "model")]))

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

# Store the completed Figure 6 cells
heatmap_cells_full_sample <- sheet_correlations

# -----------------------------------------------------------------------------------------------------------------------------
# Draw Figure 6
# -----------------------------------------------------------------------------------------------------------------------------
# Pull the figure's heatmap cells
figure_cells <- heatmap_cells_full_sample

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
    stopifnot(file.copy(heatmap_temporary_png, file.path(figures, "firm_ratings_signal_correlation_heatmaps_full_sample.png"), overwrite = TRUE))

    # Delete the temporary file
    unlink(heatmap_temporary_png)

    # Report the export
    message("🎃 Exported firm_ratings_signal_correlation_heatmaps_full_sample.png")
