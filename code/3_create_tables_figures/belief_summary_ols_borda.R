# ------------------------------------------------------------------------------
# Belief summary and AMAD tables
# ------------------------------------------------------------------------------
# Section 2 computes all respondent-pair statistics and writes:
#   - belief_amad_summary.parquet
#
# This section only reads intermediate results and writes:
#   - belief_summary_ols_borda_different_ratings.tex
# ------------------------------------------------------------------------------

source("code/3_create_tables_figures/summary_outcomes_config.R")

# Tests can point the formatter at a temporary set of intermediate sheets.
dir_path <- Sys.getenv("BELIEF_INTERMEDIATE_DIR", unset = dir_path)

BELIEF_AMAD_SUMMARY_SHEET <- "belief_amad_summary"

# ------------------------------------------------------------------------------
# Formatting helpers
# ------------------------------------------------------------------------------

latex_escape_text <- function(x) {
  out <- as.character(x)
  for (character in c("&", "%", "$", "#", "_", "{", "}")) {
    out <- gsub(character, paste0("\\", character), out, fixed = TRUE)
  }
  out
}

format_count <- function(x) {
  value <- suppressWarnings(as.integer(x))
  out <- rep("", length(value))
  ok <- !is.na(value)
  out[ok] <- formatC(value[ok], format = "d", big.mark = ",")
  out
}

grouped_table_rows <- function(outcomes,
                               display_labels,
                               formatted_data,
                               formatted_se_data = NULL) {
  grouped_outcomes <- unname(unlist(standard_outcome_groups, use.names = FALSE))
  if (!setequal(grouped_outcomes, outcomes)) {
    stop("Grouped table outcomes do not match the requested outcomes")
  }
  if (!is.null(formatted_se_data) &&
      !identical(dim(formatted_se_data), dim(formatted_data))) {
    stop("SE display data must have the same dimensions as point-estimate data")
  }

  lines <- character(0)
  group_names <- names(standard_outcome_groups)
  for (group_position in seq_along(standard_outcome_groups)) {
    row_indices <- match(
      standard_outcome_groups[[group_position]], outcomes
    )
    lines <- c(
      lines,
      paste0("\\textbf{", group_names[group_position], "} \\\\")
    )
    for (position in seq_along(row_indices)) {
      row <- row_indices[position]
      point_cells <- c(
        paste0("\\quad ", latex_escape_text(display_labels[row])),
        as.character(formatted_data[row, , drop = TRUE])
      )
      row_end <- if (position == length(row_indices) &&
                     group_position < length(standard_outcome_groups)) {
        " \\\\[0.5em]"
      } else {
        " \\\\"
      }

      if (is.null(formatted_se_data)) {
        lines <- c(lines, paste0(paste(point_cells, collapse = " & "), row_end))
      } else {
        se_cells <- c("", as.character(formatted_se_data[row, , drop = TRUE]))
        lines <- c(
          lines,
          paste0(paste(point_cells, collapse = " & "), " \\\\"),
          paste0(paste(se_cells, collapse = " & "), row_end)
        )
      }
    }
  }
  lines
}

# ------------------------------------------------------------------------------
# Average Scores and respondent-clustered SEs
# ------------------------------------------------------------------------------

average_score_se <- function(rcov_df,
                             model,
                             outcome,
                             firm_ids,
                             subset = "all") {
  firm_ids <- sort(unique(suppressWarnings(as.integer(firm_ids))))
  firm_ids <- firm_ids[!is.na(firm_ids)]
  firm_count <- length(firm_ids)
  if (firm_count < 2L) {
    stop("Avg Score SE requires at least two firm estimates")
  }

  covariance <- rcov_df %>%
    dplyr::filter(
      .data$subset == .env$subset,
      .data$model == .env$model,
      .data$outcome == .env$outcome,
      .data$entity_id_i %in% .env$firm_ids,
      .data$entity_id_j %in% .env$firm_ids
    ) %>%
    dplyr::transmute(
      entity_id_i = suppressWarnings(as.integer(.data$entity_id_i)),
      entity_id_j = suppressWarnings(as.integer(.data$entity_id_j)),
      rcov = suppressWarnings(as.numeric(.data$rcov))
    )

  if (nrow(covariance) != firm_count^2 ||
      anyDuplicated(covariance[c("entity_id_i", "entity_id_j")]) ||
      any(!is.finite(covariance$rcov))) {
    stop("Incomplete respondent-clustered covariance matrix for ",
         model, " / ", outcome)
  }

  average_variance <- sum(covariance$rcov) / firm_count^2
  tolerance <- 100 * .Machine$double.eps *
    max(1, sum(abs(covariance$rcov)))
  if (average_variance < -tolerance) {
    stop("Negative Avg Score variance for ", model, " / ", outcome)
  }
  sqrt(max(average_variance, 0))
}

select_average_score_models <- function(coef_df, outcomes) {
  model_preference <- data.frame(
    model = c("OLS_not_recentered", "OLS", "Borda_not_recentered", "Borda"),
    table_model = c("OLS", "OLS", "Borda", "Borda"),
    preference = c(1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )

  coef_df %>%
    dplyr::inner_join(model_preference, by = "model") %>%
    dplyr::filter(
      .data$subset == "all",
      .data$entity_type == "Firm",
      .data$outcome %in% outcomes
    ) %>%
    dplyr::group_by(.data$outcome, .data$table_model) %>%
    dplyr::filter(.data$preference == min(.data$preference, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      entity_id = suppressWarnings(as.integer(.data$entity_id)),
      estimate = suppressWarnings(as.numeric(.data$estimate))
    )
}

# ------------------------------------------------------------------------------
# Assemble the summary table from intermediate sheets
# ------------------------------------------------------------------------------

build_belief_summary_data <- function(dir_path,
                                      outcomes,
                                      label_mapping = NULL) {
  amad <- read_parquet_sheet(dir_path, BELIEF_AMAD_SUMMARY_SHEET)
  coef_df <- read_parquet_sheet(dir_path, "Coefficients")
  rcov_df <- read_parquet_sheet(dir_path, "rcov")

  required_amad <- c(
    "outcome", "responses", "respondents",
    "likert_amad", "likert_amad_se",
    "borda_amad", "borda_amad_se",
    "borda_tie_share", "borda_tie_share_se",
    "borda_amad_different_ratings",
    "borda_amad_different_ratings_se"
  )
  if (length(setdiff(required_amad, names(amad)))) {
    stop("Belief AMAD summary sheet is missing required columns")
  }
  if (!setequal(amad$outcome, outcomes)) {
    stop("Belief AMAD summary outcomes do not match the requested outcomes")
  }

  selected_coef <- select_average_score_models(coef_df, outcomes)
  metric_groups <- split(
    selected_coef,
    interaction(
      selected_coef$outcome,
      selected_coef$table_model,
      drop = TRUE,
      lex.order = TRUE
    )
  )
  average_scores <- dplyr::bind_rows(lapply(metric_groups, function(group_data) {
    data.frame(
      outcome = group_data$outcome[1],
      table_model = group_data$table_model[1],
      average_score = mean(group_data$estimate, na.rm = TRUE),
      average_score_se = average_score_se(
        rcov_df,
        model = group_data$model[1],
        outcome = group_data$outcome[1],
        firm_ids = group_data$entity_id
      ),
      stringsAsFactors = FALSE
    )
  })) %>%
    tidyr::pivot_wider(
      names_from = "table_model",
      values_from = c("average_score", "average_score_se")
    )

  data.frame(outcome = outcomes, stringsAsFactors = FALSE) %>%
    dplyr::left_join(amad, by = "outcome") %>%
    dplyr::left_join(average_scores, by = "outcome") %>%
    dplyr::mutate(Outcome = map_label(.data$outcome, label_mapping)) %>%
    dplyr::arrange(.data$Outcome, .data$outcome)
}

# ------------------------------------------------------------------------------
# Write the Avg Score / AMAD / share table
# ------------------------------------------------------------------------------

write_belief_summary_table <- function(
    tab,
    tables_dir,
    tex_name = "belief_summary_ols_borda_different_ratings.tex",
    latex_decimals = 3,
    se_latex_decimals = 3) {
  points <- data.frame(
    Responses = format_count(tab$responses),
    Respondents = format_count(tab$respondents),
    `Likert Avg Score` = fmt_dec(tab$average_score_OLS, latex_decimals),
    `Likert AMAD` = fmt_dec(tab$likert_amad, latex_decimals),
    `Borda Avg Score` = fmt_dec(tab$average_score_Borda, latex_decimals),
    `Borda AMAD` = fmt_dec(tab$borda_amad, latex_decimals),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  parenthesize <- function(x) {
    formatted <- fmt_dec(x, se_latex_decimals)
    ifelse(nzchar(formatted), paste0("(", formatted, ")"), "")
  }
  ses <- data.frame(
    Responses = "",
    Respondents = "",
    `Likert Avg Score` = parenthesize(tab$average_score_se_OLS),
    `Likert AMAD` = parenthesize(tab$likert_amad_se),
    `Borda Avg Score` = parenthesize(tab$average_score_se_Borda),
    `Borda AMAD` = parenthesize(tab$borda_amad_se),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  points[["Borda Tie Share"]] <- fmt_dec(
    tab$borda_tie_share, latex_decimals
  )
  ses[["Borda Tie Share"]] <- parenthesize(tab$borda_tie_share_se)

  # Conditional on both respondents giving strict rankings, Borda AMAD is
  # exactly the probability that they rank j versus k in opposite directions.
  points[["Borda Disagree Share"]] <- fmt_dec(
    tab$borda_amad_different_ratings, latex_decimals
  )
  ses[["Borda Disagree Share"]] <- parenthesize(
    tab$borda_amad_different_ratings_se
  )

  body <- grouped_table_rows(
    tab$outcome,
    tab$Outcome,
    points,
    ses
  )

  borda_header_top <- c("Avg", "", "Tie", "Disagree")
  borda_header_bottom <- c("Score", "AMAD", "Share", "Share")
  borda_column_count <- length(borda_header_top)
  header <- c(
    paste0(
      "\\begin{tabular}{l",
      paste(rep("c", 4L + borda_column_count), collapse = ""),
      "}"
    ),
    "  \\toprule",
    paste0(
      " & \\multicolumn{2}{c}{Sample} & \\multicolumn{2}{c}{Likert} & ",
      "\\multicolumn{", borda_column_count, "}{c}{Borda} \\\\"
    ),
    paste0(
      "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} ",
      "\\cmidrule(lr){6-", 5L + borda_column_count, "}"
    ),
    paste0(
      paste(c("", "", "", "Avg", "", borda_header_top), collapse = " & "),
      " \\\\"
    ),
    paste0(
      paste(c(
        "Outcome", "Responses", "Respondents", "Score", "AMAD",
        borda_header_bottom
      ), collapse = " & "),
      " \\\\"
    )
  )

  tex_path <- file.path(tables_dir, tex_name)
  writeLines(c(
    header,
    "\\midrule",
    body,
    "   \\bottomrule",
    "\\end{tabular}"
  ), tex_path)
  cat("Belief summary table saved:", basename(tex_path), "\n")
  invisible(tex_path)
}

# ------------------------------------------------------------------------------
# Write the section-3 output
# ------------------------------------------------------------------------------

write_all_belief_summary_outputs <- function(tables_dir = tables) {
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  belief_summary <- build_belief_summary_data(
    dir_path, outs, label_mapping
  )
  write_belief_summary_table(belief_summary, tables_dir)
}

belief_tables_dir <- Sys.getenv("BELIEF_TABLES_DIR", unset = tables)
write_all_belief_summary_outputs(belief_tables_dir)
