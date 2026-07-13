# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Summary statistics tables of respondent demographics by sample split and of rating-confidence shares
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
# Collapse to one row per respondent
# -----------------------------------------------------------------------------------------------------------------------------
# Keep one row per respondent with the respondent-level variables
survey_respondents <- survey_responses |> dplyr::distinct(ResponseId, gender, race_recode, hispanic, age, married, educ, empstat, income, sample, confidence_age_conduct, confidence_gend_conduct, confidence_race_conduct, confidence_gend_names, confidence_race_names)

# Should be one row per respondent i.e., every kept variable is respondent-constant
stopifnot(!anyDuplicated(survey_respondents$ResponseId), !anyNA(survey_respondents$ResponseId))

# Should be 7015 respondents
stopifnot(nrow(survey_respondents) == 7015)

# -----------------------------------------------------------------------------------------------------------------------------
# Recode the demographics into the summary-table display categories
# -----------------------------------------------------------------------------------------------------------------------------
# Assert the gender and sample indicators are 0/1 and every age is at least the bottom bin edge
stopifnot(all(survey_respondents$gender %in% c(0, 1)), all(survey_respondents$sample %in% c(0, 1)), all(survey_respondents$age >= 18))

# Label the gender indicator
survey_respondents <- survey_respondents |> dplyr::mutate(gender = c("1" = "Female", "0" = "Male")[as.character(gender)])

# Relabel the other-race category
survey_respondents <- survey_respondents |> dplyr::mutate(race_recode = dplyr::recode(race_recode, "Other" = "Other or Mixed Race"))

# Bucket the Hispanic origin answers
survey_respondents <- survey_respondents |> dplyr::mutate(hispanic = dplyr::case_when(
    hispanic == "No, I am not" ~ "Not Hispanic",
    hispanic %in% c("Yes, Mexican, Mexican-American, Chicano", "Yes, Central American", "Yes, Cuban", "Yes, South American", "Yes, Puerto Rican", "Yes, Caribbean", "Yes, Other Spanish/Hispanic/Latino") ~ "Hispanic"
))

# Bucket the marital statuses
survey_respondents <- survey_respondents |> dplyr::mutate(married = dplyr::case_when(
    married == "Never married" ~ "Never Married",
    married == "Married" ~ "Married",
    married %in% c("Divorced", "Separated", "Widowed") ~ "Other"
))

# Bucket the education levels
survey_respondents <- survey_respondents |> dplyr::mutate(educ = dplyr::case_when(
    educ %in% c("No formal education", "4th grade or below", "5th or 6th grade", "7th or 8th grade", "Some years of high school") ~ "No High School Diploma",
    educ == "High school diploma" ~ "High School Diploma",
    educ %in% c("Some college, no degree", "Associate degree") ~ "Some College / Associate Degree",
    educ %in% c("Bachelor degree", "Master degree", "Professional or Doctorate degree") ~ "Bachelor's / Graduate Degree"
))

# Bucket the employment statuses
survey_respondents <- survey_respondents |> dplyr::mutate(empstat = dplyr::case_when(
    empstat == "Working  as a paid employee" ~ "Working As Paid Employee",
    empstat == "Working  selfemployed" ~ "Working Self-employed",
    empstat == "Not working  looking for work" ~ "Not Working: Looking For Work",
    empstat %in% c("Not working  disabled", "Not working  on temporary layoff from a job", "Not working  other") ~ "Not Working: Other",
    empstat == "Not working  retired" ~ "Not Working: Retired"
))

# Bucket the income ranges
survey_respondents <- survey_respondents |> dplyr::mutate(income = dplyr::case_when(
    income == "Less than $5,000" ~ "Less Than $5,000",
    income %in% c("$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999") ~ "$5,000 To $29,999",
    income %in% c("$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999") ~ "$30,000 To $59,999",
    income %in% c("$60,000 to $74,999", "$75,000 to $84,999", "$85,000 to $99,999") ~ "$60,000 To $99,999",
    income %in% c("$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", "$200,000 or more") ~ "$100,000 Or More"
))

# Bin the ages; right-open bins so age 25 lands in [25,40)
survey_respondents <- survey_respondents |> dplyr::mutate(age = as.character(cut(age, breaks = c(18, 25, 40, 65, Inf), labels = c("[18,25)", "[25,40)", "[40,65)", "65+"), right = FALSE, include.lowest = TRUE)))

# Check every respondent lands in a display category for every demographic
stopifnot(!anyNA(survey_respondents[c("gender", "race_recode", "hispanic", "age", "married", "educ", "empstat", "income")]))

# -----------------------------------------------------------------------------------------------------------------------------
# Count respondents by display category, overall and within each half of the sample split
# -----------------------------------------------------------------------------------------------------------------------------
# Demographic display categories in table order
demographic_display_order <- c("Female", "Male", "Black", "White", "Other or Mixed Race", "Hispanic", "Not Hispanic", "[18,25)", "[25,40)", "[40,65)", "65+", "Married", "Never Married", "Other", "No High School Diploma", "High School Diploma", "Some College / Associate Degree", "Bachelor's / Graduate Degree", "Working As Paid Employee", "Working Self-employed", "Not Working: Looking For Work", "Not Working: Retired", "Not Working: Other", "Less Than $5,000", "$5,000 To $29,999", "$30,000 To $59,999", "$60,000 To $99,999", "$100,000 Or More")

# Stack the eight demographics into one long frame of display categories
respondent_categories <- survey_respondents |> dplyr::select(ResponseId, sample, gender, race_recode, hispanic, age, married, educ, empstat, income) |> tidyr::pivot_longer(cols = c(gender, race_recode, hispanic, age, married, educ, empstat, income), names_to = "demographic", values_to = "display_category")

# Should be 7015 respondents x 8 demographics, every category one of the 28 display categories
stopifnot(nrow(respondent_categories) == 7015 * 8, all(respondent_categories$display_category %in% demographic_display_order))

# Should be no display category shared across demographics
stopifnot(nrow(dplyr::distinct(respondent_categories, demographic, display_category)) == 28)

# Loop over the sample splits
for (split_variable in c("sample")) {

    # Count respondents in each display category, overall and within each half of the split
    split_counts <- respondent_categories |> dplyr::group_by(display_category) |> dplyr::summarise(count_all = dplyr::n(), count_1 = sum(.data[[split_variable]] == 1), count_0 = sum(.data[[split_variable]] == 0), .groups = "drop")

    # Should be one row per display category with every cell populated, counts summing to respondents x demographics
    stopifnot(nrow(split_counts) == 28, sum(split_counts$count_all) == 7015 * 8, all(split_counts$count_1 > 0), all(split_counts$count_0 > 0))

    # Order the display categories for the table
    split_counts <- split_counts |> dplyr::arrange(match(display_category, demographic_display_order))

    # Share of each half's respondents in each display category, rounded for display
    split_counts <- split_counts |> dplyr::mutate(share_all = round(count_all / nrow(survey_respondents), 3), share_1 = round(count_1 / sum(survey_respondents[[split_variable]] == 1), 3), share_0 = round(count_0 / sum(survey_respondents[[split_variable]] == 0), 3))

    # Store the counts for this split
    assign(paste0("demographic_counts_", split_variable), split_counts)
}

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the demographic composition tables
# -----------------------------------------------------------------------------------------------------------------------------
# Sections of the demographic table: display name -> its display categories
demographic_table_sections <- list(
    "Gender" = c("Female", "Male"),
    "Race" = c("Black", "White", "Other or Mixed Race"),
    "Hispanic" = c("Hispanic", "Not Hispanic"),
    "Age" = c("[18,25)", "[25,40)", "[40,65)", "65+"),
    "Marital Status" = c("Married", "Never Married", "Other"),
    "Education" = c("No High School Diploma", "High School Diploma", "Some College / Associate Degree", "Bachelor's / Graduate Degree"),
    "Employment" = c("Working As Paid Employee", "Working Self-employed", "Not Working: Looking For Work", "Not Working: Retired", "Not Working: Other"),
    "Income" = c("Less Than $5,000", "$5,000 To $29,999", "$30,000 To $59,999", "$60,000 To $99,999", "$100,000 Or More")
)

# Specifications for the three demographic tables: sample split, column labels, table sections, export name
# The demographics_education panel keeps only the slide-displayed categories; shares stay relative to all respondents
demographic_table_specifications <- list(
    list(split_variable = "sample", column_labels = c("Probability", "Convenience"), sections = demographic_table_sections, export_name = "summary_statistics_tables_demographics_by_probability_convenience.tex"),
    list(split_variable = "sample", column_labels = c("Probability", "Convenience"), sections = list("Demographics" = c("Female", "Black", "White", "Hispanic", "[18,25)", "[25,40)", "[40,65)"), "Education" = c("High School Diploma", "Some College / Associate Degree", "Bachelor's / Graduate Degree")), export_name = "summary_statistics_tables_demographics_by_probability_convenience_panel_demographics_education.tex"),
    list(split_variable = "sample", column_labels = c("Probability", "Convenience"), sections = demographic_table_sections[c("Employment", "Income")], export_name = "summary_statistics_tables_demographics_by_probability_convenience_panel_work_income.tex")
)

# Loop over the demographic table specifications
for (table_specification in demographic_table_specifications) {

    # Assign this specification's counts
    split_counts <- get(paste0("demographic_counts_", table_specification$split_variable))

    # Define empty table body
    table_rows <- data.frame()

    # Loop over the table sections
    for (section_name in names(table_specification$sections)) {

        # Add the bolded section header row
        table_rows <- dplyr::bind_rows(table_rows, data.frame(display_category = paste0("\\textbf{", section_name, "}"), count_all = NA_integer_, share_all = NA_real_, count_1 = NA_integer_, share_1 = NA_real_, count_0 = NA_integer_, share_0 = NA_real_))

        # Add the section's display-category rows, indented under the section header
        table_rows <- dplyr::bind_rows(table_rows, split_counts |> dplyr::filter(display_category %in% table_specification$sections[[section_name]]) |> dplyr::select(display_category, count_all, share_all, count_1, share_1, count_0, share_0) |> dplyr::mutate(display_category = paste0("\\quad ", display_category)))
    }

    # Add the respondent-count row
    table_rows <- dplyr::bind_rows(table_rows, data.frame(display_category = "N. Of Respondents", count_all = nrow(survey_respondents), share_all = NA_real_, count_1 = sum(survey_respondents[[table_specification$split_variable]] == 1), share_1 = NA_real_, count_0 = sum(survey_respondents[[table_specification$split_variable]] == 0), share_0 = NA_real_))

    # Should be one row per kept category plus the header and respondent-count rows
    stopifnot(nrow(table_rows) == sum(lengths(table_specification$sections)) + length(table_specification$sections) + 1)

    # Escape the dollar signs in the income labels
    table_rows <- table_rows |> dplyr::mutate(display_category = stringr::str_replace_all(display_category, "\\$", "\\\\$"))

    # Blank the missing cells and print the numbers as text
    table_rows <- table_rows |> dplyr::mutate(dplyr::across(-display_category, ~ dplyr::coalesce(as.character(.x), "")))

    # Build the LaTeX table: booktabs body, left-aligned labels, right-aligned count and share columns
    latex_table <- kableExtra::kbl(table_rows, format = "latex", booktabs = TRUE, linesep = "", align = c("l", "r", "r", "r", "r", "r", "r"), col.names = c("", "Count", "Share", "Count", "Share", "Count", "Share"), escape = FALSE)

    # Add the All / split-label header spanning the count-share column pairs
    latex_table <- kableExtra::add_header_above(latex_table, structure(c(1, 2, 2, 2), names = c(" ", "All", table_specification$column_labels[1], table_specification$column_labels[2])))

    # Add the rule separating the respondent-count row
    latex_table <- kableExtra::row_spec(latex_table, nrow(table_rows) - 1, extra_latex_after = "\\midrule")

    # Write the table, stripping the leading blank line and the stray double backslash after the added rule
    writeLines(gsub("\\\\midrule\\\\\\\\", "\\\\midrule", sub("^\\s*\\n", "", as.character(latex_table))), file.path(tables, table_specification$export_name))
}

# -----------------------------------------------------------------------------------------------------------------------------
# Build and write the rating-confidence table i.e., confidence-level shares among the raters of each belief
# -----------------------------------------------------------------------------------------------------------------------------
# Confidence levels in display order
confidence_display_order <- c("Not at all confident", "Slightly confident", "Somewhat confident", "Very confident", "Extremely confident", "Missing")

# Specifications for the confidence table rows: row label, confidence question, and the belief rating whose raters the row is restricted to
confidence_table_specifications <- list(
    list(row_label = "Discrimination Older (Conduct)", confidence_variable = "confidence_age_conduct", rating_variable = "conduct_favor_younger"),
    list(row_label = "Discrimination Female (Conduct)", confidence_variable = "confidence_gend_conduct", rating_variable = "conduct_favor_male"),
    list(row_label = "Discrimination Black (Conduct)", confidence_variable = "confidence_race_conduct", rating_variable = "conduct_favor_white"),
    list(row_label = "Discrimination Female (Contact)", confidence_variable = "confidence_gend_names", rating_variable = "FirmCont_favor_male"),
    list(row_label = "Discrimination Black (Contact)", confidence_variable = "confidence_race_names", rating_variable = "FirmCont_favor_white")
)

# Define empty confidence table
confidence_table_rows <- data.frame()

# Loop over the confidence table row specifications
for (confidence_specification in confidence_table_specifications) {

    # Assert the confidence question takes only the five confidence levels or the empty-string missing code
    stopifnot(all(survey_respondents[[confidence_specification$confidence_variable]] %in% c("", "Not at all confident", "Slightly confident", "Somewhat confident", "Very confident", "Extremely confident")))

    # Count each respondent's non-missing ratings of the matching belief variable and their range across firms
    respondent_rating_variation <- survey_responses |> dplyr::filter(!is.na(.data[[confidence_specification$rating_variable]])) |> dplyr::group_by(ResponseId) |> dplyr::summarise(rating_count = dplyr::n(), rating_range = max(.data[[confidence_specification$rating_variable]]) - min(.data[[confidence_specification$rating_variable]]), .groups = "drop")

    # Keep respondents with at least three ratings that are not all identical
    confidence_respondents <- survey_respondents |> dplyr::filter(ResponseId %in% respondent_rating_variation$ResponseId[respondent_rating_variation$rating_count >= 3 & respondent_rating_variation$rating_range > 0])

    # Label each kept respondent with their confidence level, the empty-string missing code as Missing
    confidence_respondents <- confidence_respondents |> dplyr::mutate(confidence_category = dplyr::if_else(.data[[confidence_specification$confidence_variable]] == "", "Missing", .data[[confidence_specification$confidence_variable]]))

    # Count kept respondents by confidence level
    confidence_shares <- confidence_respondents |> dplyr::count(confidence_category)

    # Add zero-count rows for confidence levels with no respondents, in display order
    confidence_shares <- data.frame(confidence_category = confidence_display_order) |> dplyr::left_join(confidence_shares, by = "confidence_category", relationship = "one-to-one") |> dplyr::mutate(n = dplyr::coalesce(n, 0L))

    # Should be one row per confidence level, counts summing to the kept respondents
    stopifnot(nrow(confidence_shares) == 6, sum(confidence_shares$n) == nrow(confidence_respondents))

    # Share of kept respondents at each confidence level, in percent, rounded for display
    confidence_shares <- confidence_shares |> dplyr::mutate(share = round(100 * n / nrow(confidence_respondents), 1))

    # Reshape to one table row: the row label followed by one share column per confidence level
    confidence_table_rows <- dplyr::bind_rows(confidence_table_rows, confidence_shares |> dplyr::select(confidence_category, share) |> tidyr::pivot_wider(names_from = confidence_category, values_from = share) |> dplyr::mutate(Measure = confidence_specification$row_label) |> dplyr::relocate(Measure))
}

# Should be one row per confidence question, none missing
stopifnot(nrow(confidence_table_rows) == 5, !anyNA(confidence_table_rows))

# Build the LaTeX table: booktabs body, left-aligned row labels, centered share columns
confidence_table_text <- knitr::kable(confidence_table_rows, format = "latex", booktabs = TRUE, align = c("l", rep("c", 6)), col.names = c("Measure", confidence_display_order), linesep = "")

# Write the table
writeLines(confidence_table_text, file.path(tables, "summary_statistics_tables_rating_confidence_shares.tex"))
