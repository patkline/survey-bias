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

# Should be 7015 respondents x 5 firm slots = 35075 rows i.e., every respondent carries slots numbered 1-5
stopifnot(nrow(survey_responses) == 35075, dplyr::n_distinct(survey_responses$ResponseId) == 7015, all(survey_responses$option_number %in% 1:5))

# -----------------------------------------------------------------------------------------------------------------------------
# Collapse to one row per respondent for the respondent-level questions
# -----------------------------------------------------------------------------------------------------------------------------
# Flag conduct-arm respondents i.e., those with a non-missing conduct rating for any firm
survey_responses <- survey_responses |> dplyr::group_by(ResponseId) |> dplyr::mutate(conduct_arm = any(!is.na(conduct_white) | !is.na(conduct_male) | !is.na(conduct_favor_white) | !is.na(conduct_favor_male))) |> dplyr::ungroup()

# Keep one row per respondent with the respondent-level variables
survey_respondents <- survey_responses |> dplyr::distinct(ResponseId, feared_discrim, any_entry_lev_exp, fear, gender, race_recode, looking_job, information_source, conduct_arm)

# Should be one row per respondent i.e., every kept variable is respondent-constant
stopifnot(!anyDuplicated(survey_respondents$ResponseId), !anyNA(survey_respondents$ResponseId))

# Should be 7015 respondents, 3549 of them in the conduct arm
stopifnot(nrow(survey_respondents) == 7015, sum(survey_respondents$conduct_arm) == 3549)

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
for (rating_variable in names(rating_scale_wordings)) {

    # Assert the rating takes only the 1-5 scale values, the -1 prefer-not-to-answer code, or missing
    stopifnot(all(survey_responses[[rating_variable]] %in% c(NA, -1, 1:5)))

    # Answer categories in display order: the five scale labels, then the don't-know/prefer-not option, then missing
    answer_categories <- c(rating_answer_labels[[rating_scale_wordings[[rating_variable]]]], "Don't Know/\nPrefer Not to Answer", "Missing")

    # Keep every response row of respondents with at least one non-missing rating for this variable
    rating_responses <- survey_responses |> dplyr::group_by(ResponseId) |> dplyr::filter(any(!is.na(.data[[rating_variable]]))) |> dplyr::ungroup()

    # Label each response row with its answer category; kept respondents' missing rows stay in the denominator
    rating_responses <- rating_responses |> dplyr::mutate(answer_category = dplyr::case_when(
        is.na(.data[[rating_variable]]) ~ "Missing",
        .data[[rating_variable]] == -1 ~ "Don't Know/\nPrefer Not to Answer",
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

    # Should be exactly five response rows per kept respondent; balance makes the row-level share equal the mean respondent-level share
    stopifnot(all(dplyr::count(rating_responses, ResponseId)$n == 5))

    # Each kept respondent's share of their five response rows in each answer category, zero-filled for categories they never used
    respondent_category_shares <- rating_responses |> dplyr::count(ResponseId, answer_category) |> tidyr::complete(ResponseId, answer_category = answer_categories, fill = list(n = 0)) |> dplyr::mutate(respondent_share = n / 5)

    # Should be one row per kept respondent x answer category
    stopifnot(nrow(respondent_category_shares) == 7 * dplyr::n_distinct(rating_responses$ResponseId))

    # Respondent-level standard error of each category share; one respondent's five ratings are correlated, so the respondent is the independent unit
    category_standard_errors <- respondent_category_shares |> dplyr::group_by(answer_category) |> dplyr::summarise(mean_respondent_share = mean(respondent_share), share_standard_error = 100 * sd(respondent_share) / sqrt(dplyr::n()), .groups = "drop")

    # Share of response rows in each answer category, in percent, with its respondent-level standard error
    category_shares <- category_shares |> dplyr::mutate(proportion = n / nrow(rating_responses), share = 100 * proportion) |> dplyr::left_join(category_standard_errors, by = "answer_category")

    # Should be the same category share from both constructions
    stopifnot(all(abs(category_shares$proportion - category_shares$mean_respondent_share) < 1e-12))

    # 95% confidence interval margin for each share
    category_shares <- category_shares |> dplyr::mutate(share_margin_of_error = 1.96 * share_standard_error) |> dplyr::select(-mean_respondent_share)

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

        # Axis labels; the tick labels carry the percent sign
        labs(x = "", y = "Share of Responses") +

        # Fix the share axis to 0-100%
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1), expand = c(0, 0)) +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines; thin tick marks on the share axis only
            axis.line = element_line(color = "black"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "black"),

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

    # Assert the question takes only the yes/no/prefer-not answers or the empty-string missing code
    stopifnot(all(survey_respondents[[yes_no_variable]] %in% c("", "No", "Yes", "Prefer not to answer")))

    # Label each respondent with their answer category, the empty-string missing code as Missing
    yes_no_respondents <- survey_respondents |> dplyr::mutate(answer_category = dplyr::if_else(.data[[yes_no_variable]] == "", "Missing", .data[[yes_no_variable]]))

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

        # Axis labels; the tick labels carry the percent sign
        labs(x = "", y = "Share of Respondents") +

        # Fix the share axis to 0-100%
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1), expand = c(0, 0)) +

        # Theme baseline
        theme_classic(base_size = 13) +

        # Theme adjustments
        theme(
            # No grid lines
            panel.grid = element_blank(),

            # Bottom and left axis spines; thin tick marks on the share axis only
            axis.line = element_line(color = "black"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "black")
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

            # Bottom and left axis spines; thin tick marks on the count axis only
            axis.line = element_line(color = "black"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "black")
        )

    # Export the count bar graph
    ggsave(file.path(figures, paste0("summary_statistics_bar_graphs_", yes_no_variable, "_count.png")), plot = yes_no_count_bar_graph, width = 8, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
}

# -----------------------------------------------------------------------------------------------------------------------------
# Feared-discrimination subsample-means bar graph i.e., the share fearing discrimination overall and within each subsample
# -----------------------------------------------------------------------------------------------------------------------------
# Assert the subsample split variables and fear are 0/1 indicators and race takes only its three categories, none missing
stopifnot(all(survey_respondents$gender %in% c(0, 1)), all(survey_respondents$looking_job %in% c(0, 1)), all(survey_respondents$fear %in% c(0, 1)), all(survey_respondents$race_recode %in% c("Black", "White", "Other")))

# Stack one row per bar: the overall mean, then the means by race, sex, and job-search status
subsample_shares <- dplyr::bind_rows(
    survey_respondents |> dplyr::summarise(subsample = "Overall", respondent_count = dplyr::n(), fear_share = mean(fear)),
    survey_respondents |> dplyr::group_by(subsample = race_recode) |> dplyr::summarise(respondent_count = dplyr::n(), fear_share = mean(fear), .groups = "drop"),
    survey_respondents |> dplyr::group_by(subsample = c("0" = "Male", "1" = "Female")[as.character(gender)]) |> dplyr::summarise(respondent_count = dplyr::n(), fear_share = mean(fear), .groups = "drop"),
    survey_respondents |> dplyr::group_by(subsample = c("0" = "Not Looking", "1" = "Looking for Job")[as.character(looking_job)]) |> dplyr::summarise(respondent_count = dplyr::n(), fear_share = mean(fear), .groups = "drop")
)

# Should be 1 overall + 3 race + 2 sex + 2 job-search = 8 bars, each split's counts covering every respondent, none missing
stopifnot(nrow(subsample_shares) == 8, sum(subsample_shares$respondent_count) == 4 * nrow(survey_respondents), !anyNA(subsample_shares))

# Binomial standard error of each share
subsample_shares <- subsample_shares |> dplyr::mutate(share_standard_error = sqrt(fear_share * (1 - fear_share) / respondent_count))

# 95% confidence interval bounds, capped at the 0-1 share range
subsample_shares <- subsample_shares |> dplyr::mutate(share_lower_bound = pmax(0, fear_share - 1.96 * share_standard_error), share_upper_bound = pmin(1, fear_share + 1.96 * share_standard_error))

# Order the bars: overall, then the race, sex, and job-search segments
subsample_shares <- subsample_shares |> dplyr::mutate(subsample = factor(subsample, levels = c("Overall", "Black", "White", "Other", "Female", "Male", "Looking for Job", "Not Looking")))

# Define the subsample-means bar graph
subsample_bar_graph <- ggplot(subsample_shares, aes(x = subsample, y = fear_share)) +

    # Steelblue share bars on a single discrete axis, so gaps within and between segments are identical
    geom_col(width = 0.6, fill = "steelblue") +

    # 95% confidence interval error bars
    geom_errorbar(aes(ymin = share_lower_bound, ymax = share_upper_bound), width = 0.15, linewidth = 0.6) +

    # Dashed separators between the overall, race, sex, and job-search segments; grey44 matches Stata gs7
    geom_vline(xintercept = c(1.5, 4.5, 6.5), linetype = "dashed", color = "grey44") +

    # Fix the share axis to 0-100%
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +

    # Axis labels name the plotted share; the tick labels carry the percent sign
    labs(x = "", y = "Share Fearing Discrimination") +

    # Theme baseline
    theme_classic(base_size = 13) +

    # Theme adjustments
    theme(
        # No grid lines
        panel.grid = element_blank(),

        # Bottom and left axis spines; thin tick marks on the share axis only
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "black")
    )

# Export the subsample-means bar graph
ggsave(file.path(figures, "summary_statistics_bar_graphs_y_fear_x_subsample_means.png"), plot = subsample_bar_graph, width = 9.5, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")

# -----------------------------------------------------------------------------------------------------------------------------
# Information source bar graph i.e., the share of conduct-arm respondents citing each source about firm conduct
# -----------------------------------------------------------------------------------------------------------------------------
# Assert the information source selection is never missing; the empty string codes no selection
stopifnot(!anyNA(survey_respondents$information_source))

# Keep conduct-arm respondents with a non-empty information source selection
information_source_respondents <- survey_respondents |> dplyr::filter(conduct_arm, information_source != "")

# Should be 820 conduct-arm respondents citing at least one source
stopifnot(nrow(information_source_respondents) == 820)

# Define dataframe of the information source categories in display order
source_shares <- data.frame(information_source_category = c("Personal experience", "Current or former coworkers", "Friends or family", "Internet or social media", "Other"), share = NA_real_)

# Fill each category share: the percent of respondents whose multi-select string contains the category; shares do not sum to 100
for (category_index in seq_len(nrow(source_shares))) {
    source_shares$share[category_index] <- mean(grepl(source_shares$information_source_category[category_index], information_source_respondents$information_source, fixed = TRUE)) * 100
}

# Assert every comma-separated selection token is one of the five known source categories i.e., a renamed or added option fails loudly
stopifnot(all(trimws(unlist(strsplit(information_source_respondents$information_source, ",", fixed = TRUE))) %in% source_shares$information_source_category))

# Order the source categories for display
source_shares$information_source_category <- factor(source_shares$information_source_category, levels = source_shares$information_source_category)

# Define the information source bar graph
information_source_bar_graph <- ggplot(source_shares, aes(x = information_source_category, y = share)) +

    # Steelblue source-share bars
    geom_col(width = 0.8, fill = "steelblue") +

    # Fix the share axis to 0-100%
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1), expand = c(0, 0)) +

    # Axis labels; the tick labels carry the percent sign
    labs(x = "", y = "Share of Respondents") +

    # Theme baseline
    theme_classic(base_size = 13) +

    # Theme adjustments
    theme(
        # No grid lines
        panel.grid = element_blank(),

        # Bottom and left axis spines; thin tick marks on the share axis only
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),

        # Angle the source labels
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Export the bar graph
ggsave(file.path(figures, "summary_statistics_bar_graphs_information_source_share.png"), plot = information_source_bar_graph, width = 8, height = 5, dpi = 300)
