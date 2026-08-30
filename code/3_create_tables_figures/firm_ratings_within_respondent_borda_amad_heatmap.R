# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Triangular heatmaps of within-respondent cross-question Borda disagreement --- for each
# survey measure pair, comparing one respondent's pairwise firm-ordering win indicators
# (w in {0, 0.5, 1}) under the two questions, computed per respondent and averaged equally across respondents:
#   - amad: the mean absolute difference |w_1 - w_2| over all comparisons
#   - disagree_share: among the firm pairs the respondent strictly orders under BOTH questions, the
#     share whose ordering flips --- the within-respondent analog of the paper's
#     require_different_ratings disagree-share convention
# The Borda win indicator is scale-free, so cross-construct pairs are meaningful; the Likert analog
# is not shown because absolute rating differences across constructs are dominated by scale-level
# gaps.
#
# Only the Borda (upper) triangle is populated, matching the correlation heatmaps' orientation; the
# diagonal and lower triangle are left blank.
#
# Created: Evan K. Rose 2026-08-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Define the label names for the survey measures shown on the heatmap + their order
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
# Import the within-respondent Borda AMAD pairs and the between-respondent diagonal benchmark
# -----------------------------------------------------------------------------------------------------------------------------
pair_amads <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "belief_amad_within_respondent_borda_pairs") |>
    dplyr::filter(outcome_1 %in% names(survey_measure_display_names), outcome_2 %in% names(survey_measure_display_names))

# Assert one row per unordered survey measure pair
stopifnot(nrow(pair_amads) == 10 * 9 / 2, !anyDuplicated(pair_amads[c("outcome_1", "outcome_2")]))

# The infeasible pairs: the conduct module and the FirmHire/FirmCont modules were never assigned to
# the same respondent
conduct_measures <- c("conduct_favor_white", "conduct_favor_male", "conduct_favor_younger")
hire_contact_measures <- c("FirmHire_favor_white", "FirmCont_favor_white", "FirmHire_favor_male", "FirmCont_favor_male")
is_infeasible_pair <- (pair_amads$outcome_1 %in% conduct_measures & pair_amads$outcome_2 %in% hire_contact_measures) |
    (pair_amads$outcome_1 %in% hire_contact_measures & pair_amads$outcome_2 %in% conduct_measures)

# Assert the missing values under both measures are exactly the infeasible pairs
stopifnot(all(is.finite(pair_amads$borda_amad) == !is_infeasible_pair))
stopifnot(all(is.finite(pair_amads$borda_strict_disagree_share) == !is_infeasible_pair))
stopifnot(all(dplyr::between(pair_amads$borda_amad[!is_infeasible_pair], 0, 1)))
stopifnot(all(dplyr::between(pair_amads$borda_strict_disagree_share[!is_infeasible_pair], 0, 1)))

# -----------------------------------------------------------------------------------------------------------------------------
# Build the heatmap cells: within-respondent pair AMADs in the Borda (upper) triangle, with the
# diagonal and lower triangle left empty
# -----------------------------------------------------------------------------------------------------------------------------
pair_cells <- dplyr::bind_rows(
    pair_amads |> dplyr::select(outcome_1, outcome_2, borda_amad, borda_strict_disagree_share),
    pair_amads |> dplyr::select(outcome_1 = outcome_2, outcome_2 = outcome_1, borda_amad, borda_strict_disagree_share)
)

# Map each survey measure to its axis position; the column from measure 1, the row (top to bottom) from measure 2
pair_cells$column_position <- match(survey_measure_display_names[pair_cells$outcome_1], survey_measure_display_order)
pair_cells$row_position <- match(survey_measure_display_names[pair_cells$outcome_2], survey_measure_display_order)

# Keep the Borda (upper) triangle only
figure_cells <- pair_cells[pair_cells$row_position < pair_cells$column_position, ]

# Assert one cell per upper-triangle axis position
stopifnot(nrow(figure_cells) == 10 * 9 / 2, !anyDuplicated(figure_cells[c("row_position", "column_position")]))

# Convert each survey measure to its display-name factor; y levels reversed so axis row 1 sits at the top
figure_cells$display_measure_1 <- factor(survey_measure_display_names[figure_cells$outcome_1], levels = survey_measure_display_order)
figure_cells$display_measure_2 <- factor(survey_measure_display_names[figure_cells$outcome_2], levels = rev(survey_measure_display_order))

# -----------------------------------------------------------------------------------------------------------------------------
# Draw and export one heatmap per disagreement measure
# -----------------------------------------------------------------------------------------------------------------------------
disagreement_measure_specifications <- list(
    list(
        measure_column = "borda_amad",
        legend_title   = "Borda AMAD",
        export_name    = "firm_ratings_within_respondent_borda_amad_heatmap.png",
        caption_line   = "Within-respondent Borda AMAD: the mean absolute difference between one respondent's pairwise firm-ordering win indicators"
    ),
    list(
        measure_column = "borda_strict_disagree_share",
        legend_title   = "Disagree share",
        export_name    = "firm_ratings_within_respondent_borda_disagree_share_heatmap.png",
        caption_line   = "Within-respondent Borda disagree share: among firm pairs a respondent strictly orders under both questions, the share flipped"
    )
)

for (disagreement_measure_specification in disagreement_measure_specifications) {

    # Pull this measure's cell values
    figure_cells$cell_value <- figure_cells[[disagreement_measure_specification$measure_column]]

    figure_heatmap <- ggplot(figure_cells, aes(x = display_measure_1, y = display_measure_2, fill = cell_value)) +
        geom_tile(color = "white") +
        # Disagreement is a magnitude, so the fill is sequential rather than diverging
        scale_fill_gradient(low = "white", high = "red", name = disagreement_measure_specification$legend_title,
                            limits = c(0, max(figure_cells$cell_value, na.rm = TRUE)), na.value = "grey80") +
        geom_text(data = figure_cells[is.finite(figure_cells$cell_value), ],
                  aes(label = sprintf("%.2f", cell_value)), size = 3) +
        # Keep the empty first column and last row so the 10 x 10 frame matches the correlation heatmaps
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.85, size = 10, margin = margin(t = 24)),
            axis.text.y = element_text(size = 10),
            legend.position = "right",
            panel.grid = element_blank(),
            plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
            plot.margin = margin(t = 24, r = 24, b = 12, l = 12)
        ) +
        labs(title = "", x = NULL, y = NULL,
             caption = paste0(
                 disagreement_measure_specification$caption_line, "\n",
                 "under the two questions, computed per respondent and averaged equally across respondents. Grey cells: question modules never\nassigned to the same respondents."
             ))

    # Define a temporary export path; writing PNGs directly into the Dropbox figures directory intermittently
    # exposes zero-byte files mid-write
    heatmap_temporary_png <- tempfile(fileext = ".png")

    # Save the heatmap
    ggsave(heatmap_temporary_png, figure_heatmap, width = 12, height = 10, dpi = 300, bg = "white")

    # Trim the whitespace around the heatmap in place
    magick::image_write(magick::image_trim(magick::image_read(heatmap_temporary_png)), path = heatmap_temporary_png)

    # Copy the trimmed heatmap into the figures directory, asserting the copy succeeds
    stopifnot(file.copy(heatmap_temporary_png, file.path(figures, disagreement_measure_specification$export_name), overwrite = TRUE))

    # Delete the temporary file
    unlink(heatmap_temporary_png)

    # Report the export
    message("🎃 Exported ", disagreement_measure_specification$export_name)
}
