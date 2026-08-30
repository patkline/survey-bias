# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Distribution across respondents of each respondent's Pearson correlation between their own
# Likert or Borda scores on the race-discrimination Hire and Contact questions, across the firms they
# rated on both. These are the "ratios" behind the within-respondent correlation heatmaps: the
# average-of-correlations estimator is the mean of this distribution, while the ratio-of-averages
# estimator weights respondents by their score variance instead.
#
# The shape explains the gap between the two estimators: a large spike at exactly +1 (collinear answer
# patterns on a coarse scale), most mass between 0.6 and 1, and a thin near-uniform left tail from
# one-point response jitter landing on different firms across the two questions. Respondents with
# constant scores on either question have no defined correlation and are excluded here.
#
# Created: Evan K. Rose 2026-08-29
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")
source(file.path(analysis, "create_wide_rankings.R"))
source(file.path(analysis, "prep_outcomes.R"))
source(file.path(analysis, "borda_score.R"))
source(file.path(analysis, "within_respondent_correlations.R"))

# -----------------------------------------------------------------------------------------------------------------------------
# Build the respondent x firm score panels and each respondent's Hire x Contact correlation
# -----------------------------------------------------------------------------------------------------------------------------
survey_data <- read.csv(file.path(processed, "long_survey_final.csv"), stringsAsFactors = FALSE)

# Pipeline-identical Likert and Borda scores for the two race-discrimination questions
respondent_firm_scores <- build_within_respondent_scores(survey_data, c("FirmHire_favor_white", "FirmCont_favor_white"))

# One row per respondent per scoring method: their correlation across common firms
compute_respondent_correlations <- function(scores_hire, scores_contact, method_label) {
    dplyr::inner_join(scores_hire, scores_contact, by = c("resp_id", "firm_id"), suffix = c("_hire", "_contact")) |>
        dplyr::group_by(resp_id) |>
        dplyr::filter(dplyr::n() >= 2) |>
        dplyr::summarise(respondent_correlation = suppressWarnings(stats::cor(score_hire, score_contact)), .groups = "drop") |>
        dplyr::mutate(method = method_label)
}

respondent_correlations <- dplyr::bind_rows(
    compute_respondent_correlations(respondent_firm_scores$OLS$FirmHire_favor_white, respondent_firm_scores$OLS$FirmCont_favor_white, "Likert"),
    compute_respondent_correlations(respondent_firm_scores$Borda$FirmHire_favor_white, respondent_firm_scores$Borda$FirmCont_favor_white, "Borda")
)
respondent_correlations$method <- factor(respondent_correlations$method, levels = c("Likert", "Borda"))

# Assert both methods cover the same respondents who answered both questions with two-plus common firms
stopifnot(identical(sort(table(respondent_correlations$method))[[1]], sort(table(respondent_correlations$method))[[2]]))

# Keep respondents with a defined correlation; constant scores on either question leave it undefined
defined_respondent_correlations <- respondent_correlations[is.finite(respondent_correlations$respondent_correlation), ]
stopifnot(nrow(defined_respondent_correlations) > 0, all(dplyr::between(defined_respondent_correlations$respondent_correlation, -1, 1)))

# Report the pieces of the distribution the caption describes
for (method_label in levels(respondent_correlations$method)) {
    method_correlations <- respondent_correlations$respondent_correlation[respondent_correlations$method == method_label]
    message(method_label,
            ": respondents = ", length(method_correlations),
            " | undefined = ", sum(!is.finite(method_correlations)),
            " | exactly +1 = ", sum(method_correlations > 0.999, na.rm = TRUE),
            " | mean = ", round(mean(method_correlations, na.rm = TRUE), 3),
            " | median = ", round(stats::median(method_correlations, na.rm = TRUE), 3))
}

# -----------------------------------------------------------------------------------------------------------------------------
# Draw and export the histogram, one panel per scoring method
# -----------------------------------------------------------------------------------------------------------------------------
# Equal-weighted mean per method, the average-of-correlations estimator shown on the heatmaps
method_mean_correlations <- defined_respondent_correlations |>
    dplyr::group_by(method) |>
    dplyr::summarise(mean_correlation = mean(respondent_correlation), .groups = "drop")

distribution_figure <- ggplot(defined_respondent_correlations, aes(x = respondent_correlation)) +
    geom_histogram(breaks = seq(-1.025, 1.025, by = 0.05), fill = "grey35", color = "white", linewidth = 0.2) +
    facet_wrap(~method, ncol = 1, scales = "free_y") +
    geom_vline(data = method_mean_correlations, aes(xintercept = mean_correlation), color = "purple", linewidth = 0.7) +
    labs(
        x = "Within-respondent correlation, Discrimination Black: Hire vs. Contact",
        y = "Respondents",
        caption = "Respondents with at least two common firms and nonconstant scores on both questions. Purple line: equal-weighted mean."
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8, hjust = 0))

# Define a temporary export path; writing PNGs directly into the Dropbox figures directory intermittently
# exposes zero-byte files mid-write
distribution_temporary_png <- tempfile(fileext = ".png")

# Save the histogram
ggsave(distribution_temporary_png, distribution_figure, width = 8, height = 6, dpi = 300, bg = "white")

# Trim the whitespace around the histogram in place
magick::image_write(magick::image_trim(magick::image_read(distribution_temporary_png)), path = distribution_temporary_png)

# Copy the trimmed histogram into the figures directory, asserting the copy succeeds
stopifnot(file.copy(distribution_temporary_png, file.path(figures, "within_respondent_correlation_distribution_race_hire_contact.png"), overwrite = TRUE))

# Delete the temporary file
unlink(distribution_temporary_png)

# Report the export
message("🎃 Exported within_respondent_correlation_distribution_race_hire_contact.png")
