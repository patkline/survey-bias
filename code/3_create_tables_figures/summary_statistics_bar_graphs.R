# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Bar graphs of the survey response and respondent shares 
#
# Created: Nico Rotundo 2026-07-12
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Import the survey microdata i.e., one row per respondent x firm slot
# -----------------------------------------------------------------------------------------------------------------------------
# Load the summary-statistics survey microdata
survey_responses <- read.csv(file.path(processed, "long_survey_final_summary_stats.csv"), stringsAsFactors = FALSE)

# Uniquely identified by respondent x firm slot, none missing
stopifnot(!anyDuplicated(survey_responses[c("ResponseId", "option_number")]), !anyNA(survey_responses[c("ResponseId", "option_number")]))

# Should be 6515 respondents x 5 firm slots = 32575 rows
stopifnot(nrow(survey_responses) == 32575)

# -----------------------------------------------------------------------------------------------------------------------------
# Collapse to one row per respondent for the respondent-level questions
# -----------------------------------------------------------------------------------------------------------------------------
# Flag conduct-arm respondents i.e., those with a non-missing conduct rating for any firm
survey_responses <- survey_responses |> dplyr::group_by(ResponseId) |> dplyr::mutate(conduct_arm = any(!is.na(conduct_white) | !is.na(conduct_male) | !is.na(conduct_favor_white) | !is.na(conduct_favor_male))) |> dplyr::ungroup()

# Keep one row per respondent with the respondent-level variables
survey_respondents <- survey_responses |> dplyr::distinct(ResponseId, feared_discrim, any_entry_lev_exp, fear, gender, race_recode, looking_job, information_source, conduct_arm)

# Should be one row per respondent i.e., every kept variable is respondent-constant
stopifnot(!anyDuplicated(survey_respondents$ResponseId), !anyNA(survey_respondents$ResponseId))

# Should be 6515 respondents, 3258 of them in the conduct arm
stopifnot(nrow(survey_respondents) == 6515, sum(survey_respondents$conduct_arm) == 3258)

# -----------------------------------------------------------------------------------------------------------------------------
# Belief rating bar graphs i.e., the answer-category distribution of each 1-5 rating
# -----------------------------------------------------------------------------------------------------------------------------
# Answer labels for the two 1-5 scale wordings; contact and hiring gap ratings use the more/less-likely wording, conduct level ratings the likely/unlikely wording
rating_answer_labels <- list(
    gap = c("Much More Likely", "Somewhat More Likely", "Equally Likely", "Somewhat Less Likely", "Much Less Likely"),
    level = c("Very Likely", "Somewhat Likely", "Neither Likely Nor Unlikely", "Somewhat Unlikely", "Very Unlikely")
)

# Scale wording for each rating variable
rating_scale_wordings <- c(FirmCont_white = "gap", FirmCont_black = "gap", FirmHire_white = "gap", FirmHire_black = "gap", FirmCont_male = "gap", FirmCont_female = "gap", FirmHire_male = "gap", FirmHire_female = "gap", conduct_white = "level", conduct_black = "level", conduct_female = "level", conduct_male = "level", conduct_older = "level", conduct_younger = "level")

# Loop over the belief rating variables
for (rating_variable in c("FirmCont_white", "FirmCont_black", "FirmHire_white", "FirmHire_black", "FirmCont_male", "FirmCont_female", "FirmHire_male", "FirmHire_female", "conduct_white", "conduct_black", "conduct_female", "conduct_male", "conduct_older", "conduct_younger")) {

    # Assert the rating takes only the 1-5 scale values, the -1 prefer-not-to-answer code, or missing
    stopifnot(all(survey_responses[[rating_variable]] %in% c(NA, -1, 1:5)))

    # Answer categories in display order: the five scale labels, then prefer not to answer, then missing
    answer_categories <- c(rating_answer_labels[[rating_scale_wordings[[rating_variable]]]], "Prefer not to answer", "Missing")

    # Keep every response row of respondents with at least one non-missing rating for this variable
    rating_responses <- survey_responses |> dplyr::group_by(ResponseId) |> dplyr::filter(any(!is.na(.data[[rating_variable]]))) |> dplyr::ungroup()

    # Label each response row with its answer category; kept respondents' missing rows stay in the denominator
    rating_responses <- rating_responses |> dplyr::mutate(answer_category = dplyr::case_when(
        is.na(.data[[rating_variable]]) ~ "Missing",
        .data[[rating_variable]] == -1 ~ "Prefer not to answer",
        .data[[rating_variable]] == 1 ~ answer_categories[1],
        .data[[rating_variable]] == 2 ~ answer_categories[2],
        .data[[rating_variable]] == 3 ~ answer_categories[3],
        .data[[rating_variable]] == 4 ~ answer_categories[4],
        .data[[rating_variable]] == 5 ~ answer_categories[5]
    ))

    # Count response rows by answer category
    category_shares <- rating_responses |> dplyr::count(answer_category)

    # Add zero-count rows for answer categories with no responses, in display order
    category_shares <- data.frame(answer_category = answer_categories) |> dplyr::left_join(category_shares, by = "answer_category") |> dplyr::mutate(n = dplyr::coalesce(n, 0L))

    # Should be one row per answer category, counts summing to the kept response rows
    stopifnot(nrow(category_shares) == 7, sum(category_shares$n) == nrow(rating_responses))

    # Share of response rows in each answer category, in percent, with its binomial standard error
    category_shares <- category_shares |> dplyr::mutate(proportion = n / nrow(rating_responses), share = 100 * proportion, share_standard_error = sqrt(proportion * (1 - proportion) / nrow(rating_responses)) * 100)

    # 95% confidence interval margin for each share
    category_shares <- category_shares |> dplyr::mutate(share_margin_of_error = 1.96 * share_standard_error)

    # Drop the missing category from the plot; its rows remain in the share denominator
    category_shares <- category_shares |> dplyr::filter(answer_category != "Missing")

    # Order the answer categories for display
    category_shares <- category_shares |> dplyr::mutate(answer_category = factor(answer_category, levels = answer_categories))

    # Define the rating share bar graph
    rating_bar_graph <- ggplot(category_shares, aes(x = answer_category, y = share)) +

        # Steelblue response-share bars
        geom_col(width = 0.8, fill = "steelblue") +

        # 95% confidence interval error bars
        geom_errorbar(aes(ymin = pmax(0, share - share_margin_of_error), ymax = share + share_margin_of_error), width = 0.15, linewidth = 0.6) +

        # Axis labels
        labs(x = "", y = "Share of Responses (%)") +

        # Anchor the bars at zero with headroom above
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines, no ticks
            axis.line = element_line(color = "black"),
            axis.ticks = element_blank(),

            # Angle the answer-category labels
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
        )

    # Export the bar graph, one file per rating variable
    ggsave(file.path(figures, paste0("summary_statistics_bar_graphs_", rating_variable, "_share.png")), plot = rating_bar_graph, width = 8, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
}

# -----------------------------------------------------------------------------------------------------------------------------
# Yes/no question bar graphs i.e., the answer-category distribution of each respondent-level yes/no question
# -----------------------------------------------------------------------------------------------------------------------------
# Answer categories in display order
yes_no_answer_categories <- c("No", "Yes", "Prefer not to answer", "Missing")

# Loop over the yes/no question variables
for (yes_no_variable in c("any_entry_lev_exp", "feared_discrim")) {

    # Assert the question takes only the yes/no/prefer-not answers, the empty-string missing code, or missing
    stopifnot(all(survey_respondents[[yes_no_variable]] %in% c(NA, "", "No", "Yes", "Prefer not to answer")))

    # Label each respondent with their answer category
    yes_no_respondents <- survey_respondents |> dplyr::mutate(answer_category = dplyr::case_when(
        is.na(.data[[yes_no_variable]]) | .data[[yes_no_variable]] == "" ~ "Missing",
        .data[[yes_no_variable]] == "No" ~ "No",
        .data[[yes_no_variable]] == "Yes" ~ "Yes",
        .data[[yes_no_variable]] == "Prefer not to answer" ~ "Prefer not to answer"
    ))

    # Count respondents by answer category
    category_shares <- yes_no_respondents |> dplyr::count(answer_category)

    # Add zero-count rows for answer categories with no respondents, in display order
    category_shares <- data.frame(answer_category = yes_no_answer_categories) |> dplyr::left_join(category_shares, by = "answer_category") |> dplyr::mutate(n = dplyr::coalesce(n, 0L))

    # Should be one row per answer category, counts summing to all respondents
    stopifnot(nrow(category_shares) == 4, sum(category_shares$n) == nrow(survey_respondents))

    # Share of respondents in each answer category, in percent, with its binomial standard error
    category_shares <- category_shares |> dplyr::mutate(proportion = n / nrow(survey_respondents), share = 100 * proportion, share_standard_error = sqrt(proportion * (1 - proportion) / nrow(survey_respondents)) * 100)

    # 95% confidence interval margin for each share
    category_shares <- category_shares |> dplyr::mutate(share_margin_of_error = 1.96 * share_standard_error)

    # Order the answer categories for display
    category_shares <- category_shares |> dplyr::mutate(answer_category = factor(answer_category, levels = yes_no_answer_categories))

    # Define the share bar graph
    yes_no_share_bar_graph <- ggplot(category_shares, aes(x = answer_category, y = share)) +

        # Steelblue respondent-share bars
        geom_col(width = 0.8, fill = "steelblue") +

        # 95% confidence interval error bars
        geom_errorbar(aes(ymin = pmax(0, share - share_margin_of_error), ymax = share + share_margin_of_error), width = 0.15, linewidth = 0.6) +

        # Axis labels
        labs(x = "", y = "Share of Respondents (%)") +

        # Anchor the bars at zero with headroom above
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines, no ticks
            axis.line = element_line(color = "black"),
            axis.ticks = element_blank()
        )

    # Export the share bar graph
    ggsave(file.path(figures, paste0("summary_statistics_bar_graphs_", yes_no_variable, "_share.png")), plot = yes_no_share_bar_graph, width = 8, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")

    # Define the count bar graph
    yes_no_count_bar_graph <- ggplot(category_shares, aes(x = answer_category, y = n)) +

        # Steelblue respondent-count bars
        geom_col(width = 0.8, fill = "steelblue") +

        # Axis labels
        labs(x = "", y = "Number of Respondents") +

        # Anchor the bars at zero with headroom above
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines, no ticks
            axis.line = element_line(color = "black"),
            axis.ticks = element_blank()
        )

    # Export the count bar graph
    ggsave(file.path(figures, paste0("summary_statistics_bar_graphs_", yes_no_variable, "_count.png")), plot = yes_no_count_bar_graph, width = 8, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
}

# -----------------------------------------------------------------------------------------------------------------------------
# Feared-discrimination two-way bar graphs i.e., the share fearing discrimination by race within gender and job-search groups
# -----------------------------------------------------------------------------------------------------------------------------
# Loop over the two-way split variables
for (two_way_split_variable in c("gender", "looking_job")) {

    # Assert the split variable and fear are 0/1 indicators and race takes only its three categories, none missing
    stopifnot(all(survey_respondents[[two_way_split_variable]] %in% c(0, 1)), all(survey_respondents$fear %in% c(0, 1)), all(survey_respondents$race_recode %in% c("Black", "White", "Other")))

    # Label the split groups for display
    two_way_respondents <- survey_respondents |> dplyr::mutate(split_group = list(gender = c("1" = "Female", "0" = "Male"), looking_job = c("1" = "Looking for Job", "0" = "Not Looking"))[[two_way_split_variable]][as.character(.data[[two_way_split_variable]])])

    # Share of respondents fearing discrimination by split group x race, with the respondent count
    group_shares <- two_way_respondents |> dplyr::group_by(split_group, race_recode) |> dplyr::summarise(N = dplyr::n(), fear_share = mean(fear), .groups = "drop")

    # Should be 2 split groups x 3 races = 6 bars covering all respondents, none missing
    stopifnot(nrow(group_shares) == 6, sum(group_shares$N) == nrow(survey_respondents), !anyNA(group_shares))

    # Binomial standard error of each share
    group_shares <- group_shares |> dplyr::mutate(share_standard_error = sqrt(fear_share * (1 - fear_share) / N))

    # 95% confidence interval bounds, capped at the 0-1 share range
    group_shares <- group_shares |> dplyr::mutate(share_lower_bound = pmax(0, fear_share - 1.96 * share_standard_error), share_upper_bound = pmin(1, fear_share + 1.96 * share_standard_error))

    # Order the races for the bar and legend order
    group_shares <- group_shares |> dplyr::mutate(race_recode = factor(race_recode, levels = c("Black", "White", "Other")))

    # Define the two-way bar graph
    two_way_bar_graph <- ggplot(group_shares, aes(x = split_group, y = fear_share, fill = race_recode)) +

        # Race-colored share bars, dodged within each split group
        geom_col(position = position_dodge(width = 0.75), width = 0.65) +

        # 95% confidence interval error bars
        geom_errorbar(aes(ymin = share_lower_bound, ymax = share_upper_bound), position = position_dodge(width = 0.75), width = 0.18, linewidth = 0.6) +

        # Bar color for each race
        scale_fill_manual(values = c("Black" = "steelblue", "White" = "darkorange", "Other" = "grey55")) +

        # Share axis in percent, anchored at zero with headroom above
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +

        # Axis labels and legend title
        labs(x = "", y = "Share of Respondents (%)", fill = "Race") +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines, no ticks
            axis.line = element_line(color = "black"),
            axis.ticks = element_blank(),

            # Legend to the right of the panel
            legend.position = "right"
        )

    # Export the two-way bar graph, one file per split variable
    ggsave(file.path(figures, paste0("summary_statistics_bar_graphs_y_fear_x_", two_way_split_variable, "_group_race.png")), plot = two_way_bar_graph, width = 8, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
}

# -----------------------------------------------------------------------------------------------------------------------------
# Information source bar graph i.e., the share of conduct-arm respondents citing each source about firm conduct
# -----------------------------------------------------------------------------------------------------------------------------
# Keep conduct-arm respondents with a non-empty information source selection
information_source_respondents <- survey_respondents |> dplyr::filter(conduct_arm, !is.na(information_source), information_source != "")

# Should be 804 conduct-arm respondents citing at least one source
stopifnot(nrow(information_source_respondents) == 804)

# Define dataframe of the information source categories in display order
source_shares <- data.frame(information_source_category = c("Personal experience", "Current or former coworkers", "Friends or family", "Internet or social media", "Other"), share = NA_real_)

# Fill each category share: the percent of respondents whose multi-select string contains the category; shares do not sum to 100
for (category_index in seq_len(nrow(source_shares))) {
    source_shares$share[category_index] <- mean(grepl(source_shares$information_source_category[category_index], information_source_respondents$information_source, fixed = TRUE)) * 100
}

# Should be one share per category, none missing
stopifnot(nrow(source_shares) == 5, !anyNA(source_shares$share))

# Order the source categories for display
source_shares$information_source_category <- factor(source_shares$information_source_category, levels = source_shares$information_source_category)

# Define the information source bar graph
information_source_bar_graph <- ggplot(source_shares, aes(x = information_source_category, y = share)) +

    # Steelblue source-share bars
    geom_col(width = 0.8, fill = "steelblue") +

    # Anchor the bars at zero with headroom above
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

    # Axis labels
    labs(x = "", y = "Share of Respondents (%)") +

    # Theme baseline
    theme_classic(base_size = 13) +

    # Theme adjustments
    theme(
        # No grid lines
        panel.grid = element_blank(),

        # Bottom and left axis spines, no ticks
        axis.line = element_line(color = "black"),
        axis.ticks = element_blank(),

        # Angle the source labels
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Export the bar graph
ggsave(file.path(figures, "summary_statistics_bar_graphs_information_source_share.png"), plot = information_source_bar_graph, width = 8, height = 5, dpi = 300)
