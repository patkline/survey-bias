# ------------------------------------------------------------------------------
# Purpose: Create the average-beliefs versus LinkedIn workforce-shares table
# ------------------------------------------------------------------------------

if (!exists("git_survey_bias_root", inherits = TRUE)) {
  source("code/globals.R")
}

linkedin_regressions_sheet <- "LinkedIn_belief_share_regressions"

linkedin_results <- read_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  linkedin_regressions_sheet
)

if (nrow(linkedin_results) != 12L) {
  stop("Expected 12 joint LinkedIn belief-share regression results.")
}

linkedin_table_columns <- tidyr::expand_grid(
  belief_label = c(
    "Race",
    "Gender",
    "Selectivity"
  ),
  industry_fe = c(FALSE, TRUE)
)

format_linkedin_table_number <- function(value) {
  if (!is.finite(value)) {
    stop("Cannot format a non-finite LinkedIn table value.")
  }
  if (abs(value) < 0.0005) {
    value <- 0
  }
  sprintf("%.3f", value)
}

format_linkedin_table_percent <- function(value) {
  if (!is.finite(value)) {
    stop("Cannot format a non-finite LinkedIn table percentage.")
  }
  paste0(sprintf("%.1f", 100 * value), "\\%")
}

pull_linkedin_table_result <- function(
    model_value,
    belief_value,
    industry_fe_value
) {
  result <- linkedin_results |>
    dplyr::filter(
      .data$model == model_value,
      .data$belief_label == belief_value,
      .data$industry_fe == industry_fe_value
    )

  if (nrow(result) != 1L) {
    stop("Could not identify a unique LinkedIn table result.")
  }
  result
}

make_linkedin_panel_lines <- function(panel_label, model_value) {
  column_results <- lapply(seq_len(nrow(linkedin_table_columns)), function(i) {
    pull_linkedin_table_result(
      model_value = model_value,
      belief_value = linkedin_table_columns$belief_label[i],
      industry_fe_value = linkedin_table_columns$industry_fe[i]
    )
  })

  make_estimate_row <- function(label, variable) {
    values <- vapply(
      column_results,
      function(result) format_linkedin_table_number(result[[variable]]),
      FUN.VALUE = character(1L)
    )
    paste0(label, " & ", paste(values, collapse = " & "), " \\\\")
  }

  make_standard_error_row <- function(variable) {
    values <- vapply(
      column_results,
      function(result) paste0(
        "(",
        format_linkedin_table_number(result[[variable]]),
        ")"
      ),
      FUN.VALUE = character(1L)
    )
    paste0(" & ", paste(values, collapse = " & "), " \\\\")
  }

  signal_explained <- vapply(
    column_results,
    function(result) format_linkedin_table_percent(result$signal_explained),
    FUN.VALUE = character(1L)
  )

  c(
    paste0(
      "\\multicolumn{7}{l}{\\textbf{",
      panel_label,
      "}} \\\\"
    ),
    make_estimate_row("Black Share", "black_effect_per_10pp_share"),
    make_standard_error_row("black_effect_per_10pp_se"),
    "\\addlinespace",
    make_estimate_row("Female Share", "female_effect_per_10pp_share"),
    make_standard_error_row("female_effect_per_10pp_se"),
    "\\addlinespace",
    paste0(
      "Signal Explained & ",
      paste(signal_explained, collapse = " & "),
      " \\\\"
    )
  )
}

industry_fe_cells <- ifelse(
  linkedin_table_columns$industry_fe,
  "X",
  ""
)

linkedin_latex_lines <- c(
  "\\begin{tabular}{l*{6}{c}}",
  "\\toprule",
  paste0(
    " & \\multicolumn{2}{c}{Race Beliefs}",
    " & \\multicolumn{2}{c}{Gender Beliefs}",
    " & \\multicolumn{2}{c}{Selectivity Beliefs} \\\\"
  ),
  paste0(
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} ",
    "\\cmidrule(lr){6-7}"
  ),
  paste0(
    " & ",
    paste(paste0("(", seq_len(6L), ")"), collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  make_linkedin_panel_lines("Panel A: Likert", "OLS"),
  "\\addlinespace",
  make_linkedin_panel_lines("Panel B: Borda", "Borda"),
  "\\addlinespace",
  paste0(
    "Industry FE & ",
    paste(industry_fe_cells, collapse = " & "),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

linkedin_table_path <- file.path(
  tables,
  "average_beliefs_vs_linkedin_workforce_shares.tex"
)
write_lines_checked(
  linkedin_latex_lines,
  linkedin_table_path,
  label = "average beliefs versus LinkedIn workforce shares table"
)

message("🎃 Generated average_beliefs_vs_linkedin_workforce_shares.tex")
