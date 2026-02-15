# ---- Setup -------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(kableExtra)
library(stringr)
library(tibble)

make_summary_table_components <- function(
    df,
    sample_var = "sample"      # name of the 0/1 sample column
) {
  stopifnot(sample_var %in% names(df))
  require(dplyr); require(tidyr); require(kableExtra)
  require(stringr); require(tibble)

  # ---- Cleaning / recodes ----
  df <- df %>%
    mutate(income_clean = case_when(
      income %in% c("Less than $5,000") ~ "Less Than $5,000",
      income %in% c("$5,000 to $9,999", "$10,000 to $14,999",
                    "$15,000 to $19,999", "$20,000 to $24,999",
                    "$25,000 to $29,999") ~ "$5,000 To $30,000",
      income %in% c("$30,000 to $34,999", "$35,000 to $39,999",
                    "$40,000 to $49,999", "$50,000 to $59,999") ~ "$30,000 To $60,000",
      income %in% c("$60,000 to $74,999", "$75,000 to $84,999",
                    "$85,000 to $99,999") ~ "$60,000 To $100,000",
      income %in% c("$100,000 to $124,999", "$125,000 to $149,999",
                    "$150,000 to $174,999", "$175,000 to $199,999",
                    "$200,000 or more") ~ "More Than $100,000",
      TRUE ~ NA_character_
    )) %>%
    mutate(empstat_clean = case_when(
      empstat == "Working  as a paid employee" ~ "Working As Paid Employee",
      empstat == "Working  selfemployed" ~ "Working Self-employed",
      empstat == "Not working  looking for work" ~ "Not Working: Looking For Work",
      empstat %in% c("Not working  disabled", "Not working  on temporary layoff from a job","Not working  other")  ~ "Not Working: Other",
      empstat == "Not working  retired" ~ "Not Working: Retired",
      empstat == "Looking for a job" ~ "Looking For a Job",
      TRUE ~ NA_character_
    )) %>%
    mutate(educ_clean = case_when(
      educ %in% c("No formal education","4th grade or below", "5th or 6th grade", "7th or 8th grade", "Some years of high school") ~ "No High School Diploma",
      educ == "High school diploma" ~ "High School Diploma",
      educ %in% c("Some college, no degree", "Associate degree") ~ "Some College / Associate Degree",
      educ %in% c("Bachelor degree", "Master degree","Professional or Doctorate degree") ~ "Bachelor's / Graduate Degree",
      TRUE ~ NA_character_
    )) %>%
    mutate(married = case_when(
      married == "Never married" ~ "Never Married",
      married == "Married" ~ "Married",
      married %in% c("Divorced", "Separated", "Widowed") ~ "Other"
    )) %>%
    mutate(hispanic_clean = case_when(
      hispanic == "No, I am not" ~ "Not Hispanic",
      hispanic %in% c("Yes, Mexican, Mexican-American, Chicano",
                      "Yes, Central American", "Yes, Cuban", "Yes, South American",
                      "Yes, Puerto Rican", "Yes, Caribbean",
                      "Yes, Other Spanish/Hispanic/Latino") ~ "Hispanic",
      TRUE ~ NA_character_
    ))  %>%
    mutate(race_recode = case_when(
      race_recode == "White" ~ "White",
      race_recode == "Black" ~ "Black",
      race_recode == "Other" ~ "Other or Mixed Race"
    ))

  # Age: [a,b) so 25 -> [25,40)
  df$age <- suppressWarnings(as.numeric(df$age))
  df$age_clean <- cut(
    df$age, breaks = c(18, 25, 40, 65, Inf),
    labels = c("[18,25)", "[25,40)", "[40,65)", "65+"),
    right = FALSE, include.lowest = TRUE
  )

  # ---- One row per respondent ----
  if (!"ResponseId" %in% names(df)) stop("ResponseId column is required.")
  df_one <- df %>% group_by(.data$ResponseId) %>% slice(1) %>% ungroup()

  # ---- Order + sections ----
  order_levels <- c(
    "Female","Male",
    "Black","White", "Other or Mixed Race",
    "Hispanic","Not Hispanic",
    "[18,25)","[25,40)","[40,65)","65+",
    "Married","Never Married","Other",
    "No High School Diploma","High School Diploma","Some College / Associate Degree","Bachelor's / Graduate Degree",
    "Working As Paid Employee","Working Self-employed","Not Working: Looking For Work","Not Working: Retired","Not Working: Other",
    "Less Than $5,000","$5,000 To $30,000","$30,000 To $60,000","$60,000 To $100,000","More Than $100,000"
  )

  sections <- list(
    "Gender" = c("Female","Male"),
    "Race"   = c("Black","White","Other or Mixed Race"),
    "Hispanic" = c("Hispanic","Not Hispanic"),
    "Age"    = c("[18,25)","[25,40)","[40,65)","65+"),
    "Marital Status" = c("Married","Never Married","Other"),
    "Education" = c("No High School Diploma","High School Diploma",
                    "Some College / Associate Degree","Bachelor's / Graduate Degree"),
    "Employment" = c("Working As Paid Employee","Working Self-employed",
                     "Not Working: Looking For Work","Not Working: Retired","Not Working: Other"),
    "Income" = c("Less Than $5,000","$5,000 To $30,000","$30,000 To $60,000",
                 "$60,000 To $100,000","More Than $100,000")
  )

  vars <- c("gender","race_recode","hispanic_clean","age_clean",
            "married","educ_clean","empstat_clean","income_clean")

  count_levels <- function(dfx, vars, colname) {
    dfx %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "feature", values_to = "variable") %>%
      dplyr::filter(!is.na(.data$variable)) %>%
      dplyr::count(.data$variable, name = colname)
  }
  counts_all  <- count_levels(df_one, vars, "count_all")
  counts_1    <- count_levels(dplyr::filter(df_one, .data[[sample_var]] == 1), vars, "count_1")
  counts_0    <- count_levels(dplyr::filter(df_one, .data[[sample_var]] == 0), vars, "count_0")

  counts_wide <- counts_all %>%
    dplyr::full_join(counts_1, by = "variable") %>%
    dplyr::full_join(counts_0, by = "variable") %>%
    tidyr::replace_na(list(count_all = 0L, count_1 = 0L, count_0 = 0L)) %>%
    dplyr::mutate(variable = factor(.data$variable, levels = order_levels)) %>%
    dplyr::arrange(.data$variable)

  total_all <- nrow(df_one)
  total_1   <- nrow(dplyr::filter(df_one, .data[[sample_var]] == 1))
  total_0   <- nrow(dplyr::filter(df_one, .data[[sample_var]] == 0))

  # Compute shares here (per row) so they're guaranteed to be count/total
  counts_wide_fmt <- counts_wide %>%
    dplyr::mutate(
      share_all = ifelse(.data$count_all > 0, .data$count_all / total_all, NA_real_),
      share_1   = ifelse(.data$count_1   > 0, .data$count_1   / total_1,   NA_real_),
      share_0   = ifelse(.data$count_0   > 0, .data$count_0   / total_0,   NA_real_)
    ) %>%
    dplyr::select(dplyr::all_of(c("variable", "count_all", "share_all", "count_1", "share_1", "count_0", "share_0"))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("share_"), ~ round(.x, 3))) %>%
    dplyr::mutate(
      variable = as.character(.data$variable),
      count_all = as.integer(.data$count_all),
      count_1   = as.integer(.data$count_1),
      count_0   = as.integer(.data$count_0),
      share_all = as.numeric(.data$share_all),
      share_1   = as.numeric(.data$share_1),
      share_0   = as.numeric(.data$share_0)
    )

  list(
    counts_wide_fmt = counts_wide_fmt,
    sections = sections,
    totals = list(all = total_all, one = total_1, zero = total_0)
  )
}

make_summary_table <- function(
    df,
    sample_var = "sample",      # name of the 0/1 sample column
    label0 = "Convenience",     # label for value 0
    label1 = "Probability",     # label for value 1
    outfile = "summary_table.tex",
    sections_keep = NULL,         # optional: only render these panel names
  include_section_headers = TRUE,
    components = NULL             # optional: output of make_summary_table_components()
) {
  stopifnot(sample_var %in% names(df))
  require(dplyr); require(tidyr); require(kableExtra)
  require(stringr); require(tibble)

  if (is.null(components)) {
    components <- make_summary_table_components(df = df, sample_var = sample_var)
  }
  counts_wide_fmt <- components$counts_wide_fmt
  sections <- components$sections
  totals <- components$totals

  if (!is.null(sections_keep)) {
    missing_sections <- setdiff(sections_keep, names(sections))
    if (length(missing_sections) > 0) {
      stop("Unknown section(s) in sections_keep: ", paste(missing_sections, collapse = ", "))
    }
    sections <- sections[sections_keep]
  }
  
  section_block <- function(section_name, levels_vec) {
    body <- counts_wide_fmt %>% dplyr::filter(.data$variable %in% levels_vec)
    if (!isTRUE(include_section_headers)) return(body)

    header <- tibble(
      variable = paste0("\\textbf{", section_name, "}"),
      count_all = NA_integer_, share_all = NA_real_,
      count_1   = NA_integer_, share_1   = NA_real_,
      count_0   = NA_integer_, share_0   = NA_real_
    )
    dplyr::bind_rows(header, body)
  }
  
  table_df <- dplyr::bind_rows(lapply(names(sections), function(nm) section_block(nm, sections[[nm]])))
  
  n_row <- tibble(
    variable = "N. Of Respondents",
    count_all = totals$all, share_all = NA_real_,
    count_1   = totals$one, share_1   = NA_real_,
    count_0   = totals$zero, share_0   = NA_real_
  )
  table_df <- dplyr::bind_rows(table_df, n_row)
  
  table_df_print <- table_df %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "\\$", "\\\\$")) %>%
    dplyr::mutate(dplyr::across(-dplyr::all_of("variable"), ~ ifelse(is.na(.), "", as.character(.))))
  
  n_idx <- nrow(table_df_print)
  
  header_vec <- structure(c(1, 2, 2, 2),
                          names = c(" ", "All", label1, label0))
  
  latex_tab <- kbl(
    table_df_print,
    format   = "latex",
    booktabs = TRUE,
    linesep  = "",
    align    = c("l","r","r","r","r","r","r"),
    col.names = c("", "Count","Share","Count","Share","Count","Share"),
    escape   = FALSE
  ) %>%
    add_header_above(header_vec) %>%
    row_spec(n_idx - 1, extra_latex_after = "\\midrule")
  
  tex <- as.character(latex_tab)
  tex <- sub("^\\s*\\n", "", tex)
  tex <- gsub("\\\\midrule\\\\\\\\", "\\\\midrule", tex)
  writeLines(tex, outfile)
  message("Wrote LaTeX table to: ", normalizePath(outfile))
  invisible(latex_tab)
}

