# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Histograms of the survey response durations
#
# Created: Nico Rotundo 2026-07-12
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Import the survey microdata and collapse to one row per respondent
# -----------------------------------------------------------------------------------------------------------------------------
# Load the summary-statistics survey microdata
survey_responses <- read.csv(file.path(processed, "long_survey_final_summary_stats.csv"), stringsAsFactors = FALSE)

# Uniquely identified by respondent x firm slot, none missing
stopifnot(!anyDuplicated(survey_responses[c("ResponseId", "option_number")]), !anyNA(survey_responses[c("ResponseId", "option_number")]))

# Should be 6515 respondents x 5 firm slots = 32575 rows i.e., every respondent carries slots numbered 1-5
stopifnot(nrow(survey_responses) == 32575, dplyr::n_distinct(survey_responses$ResponseId) == 6515, all(survey_responses$option_number %in% 1:5))

# Keep one row per respondent with the response duration
survey_respondents <- survey_responses |> dplyr::distinct(ResponseId, response_duration)

# Should be one row per respondent i.e., the response duration is respondent-constant
stopifnot(!anyDuplicated(survey_respondents$ResponseId), !anyNA(survey_respondents$ResponseId))

# Should be 6515 respondents, every duration present, finite, and non-negative
stopifnot(nrow(survey_respondents) == 6515, all(is.finite(survey_respondents$response_duration)), all(survey_respondents$response_duration >= 0))

# -----------------------------------------------------------------------------------------------------------------------------
# Response duration histogram i.e., the distribution of survey completion times
# -----------------------------------------------------------------------------------------------------------------------------
# Convert the response duration from seconds to minutes
survey_respondents <- survey_respondents |> dplyr::mutate(response_duration_minutes = response_duration / 60)

# Define the response duration histogram
response_duration_histogram <- ggplot(survey_respondents, aes(x = response_duration_minutes)) +

    # One-minute duration bins as a share of respondents
    geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1, boundary = 0, closed = "left", fill = "steelblue", color = "black", linewidth = 0.2) +

    # Axis labels; the share tick labels carry the percent sign
    labs(x = "Duration (in minutes)", y = "Share of Respondents") +

    # Share axis in percent
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

    # Median and interquartile annotation in the top-left corner
    annotate("text", x = -Inf, y = Inf, hjust = -0.02, vjust = 1.6, label = sprintf("Median: %.3f | P25: %.3f | P75: %.3f", median(survey_respondents$response_duration_minutes), quantile(survey_respondents$response_duration_minutes, 0.25), quantile(survey_respondents$response_duration_minutes, 0.75))) +

    # Restrict the display window without dropping bins
    coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.30)) +

    # Theme baseline
    theme_classic(base_size = 13) +

    # Theme adjustments
    theme(
        # No grid lines
        panel.grid = element_blank(),

        # Bottom and left axis spines with thin tick marks
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
    )

# Export the histogram
ggsave(file.path(figures, "summary_statistics_histograms_response_duration.png"), plot = response_duration_histogram, width = 8.5, height = 4.8, dpi = 300)
