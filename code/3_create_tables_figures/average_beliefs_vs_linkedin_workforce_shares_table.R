# ------------------------------------------------------------------------------
# Purpose: Create the average-beliefs versus LinkedIn workforce-shares table
# ------------------------------------------------------------------------------

if (!exists("git_survey_bias_root", inherits = TRUE)) {
  source("code/globals.R")
}

linkedin_regressions_sheet <- "LinkedIn_belief_share_regressions"

yimfor_results <- read_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  linkedin_regressions_sheet
)

if (nrow(yimfor_results) != 24L) {
  stop("Expected 24 Yimfor LinkedIn belief-share regression results.")
}

yimfor_table_columns <- tidyr::expand_grid(
  model = c("OLS", "Borda"),
  belief_label = c(
    "Race Beliefs",
    "Gender Beliefs",
    "Selectivity Beliefs"
  ),
  industry_fe = c(FALSE, TRUE)
)

format_yimfor_table_number <- function(value) {
  if (!is.finite(value)) {
    stop("Cannot format a non-finite Yimfor table value.")
  }
  if (abs(value) < 0.0005) {
    value <- 0
  }
  sprintf("%.3f", value)
}

pull_yimfor_table_result <- function(
    panel_value,
    model_value,
    belief_value,
    industry_fe_value
) {
  result <- yimfor_results |>
    dplyr::filter(
      .data$panel == panel_value,
      .data$model == model_value,
      .data$belief_label == belief_value,
      .data$industry_fe == industry_fe_value
    )

  if (nrow(result) != 1L) {
    stop("Could not identify a unique Yimfor table result.")
  }
  result
}

make_yimfor_panel_lines <- function(panel_value, share_label) {
  column_results <- lapply(seq_len(nrow(yimfor_table_columns)), function(i) {
    pull_yimfor_table_result(
      panel_value = panel_value,
      model_value = yimfor_table_columns$model[i],
      belief_value = yimfor_table_columns$belief_label[i],
      industry_fe_value = yimfor_table_columns$industry_fe[i]
    )
  })

  estimates <- vapply(
    column_results,
    function(result) format_yimfor_table_number(
      result$effect_per_10pp_share
    ),
    FUN.VALUE = character(1L)
  )
  standard_errors <- vapply(
    column_results,
    function(result) paste0(
      "(",
      format_yimfor_table_number(result$effect_per_10pp_se),
      ")"
    ),
    FUN.VALUE = character(1L)
  )

  c(
    paste0(
      "\\multicolumn{13}{l}{\\textbf{",
      panel_value,
      "}} \\\\"
    ),
    paste0(share_label, " & ", paste(estimates, collapse = " & "), " \\\\"),
    paste0(" & ", paste(standard_errors, collapse = " & "), " \\\\")
  )
}

industry_fe_cells <- ifelse(
  yimfor_table_columns$industry_fe,
  "X",
  ""
)

yimfor_latex_lines <- c(
  "\\begin{tabular}{l*{12}{c}}",
  "\\toprule",
  " & \\multicolumn{6}{c}{Likert} & \\multicolumn{6}{c}{Borda} \\\\",
  "\\cmidrule(lr){2-7} \\cmidrule(lr){8-13}",
  paste0(
    " & \\multicolumn{2}{c}{Race Beliefs}",
    " & \\multicolumn{2}{c}{Gender Beliefs}",
    " & \\multicolumn{2}{c}{Selectivity Beliefs}",
    " & \\multicolumn{2}{c}{Race Beliefs}",
    " & \\multicolumn{2}{c}{Gender Beliefs}",
    " & \\multicolumn{2}{c}{Selectivity Beliefs} \\\\"
  ),
  paste0(
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} ",
    "\\cmidrule(lr){6-7} \\cmidrule(lr){8-9} ",
    "\\cmidrule(lr){10-11} \\cmidrule(lr){12-13}"
  ),
  paste0(
    " & ",
    paste(paste0("(", seq_len(12L), ")"), collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  make_yimfor_panel_lines("Panel A: Race", "Black Share"),
  "\\addlinespace",
  make_yimfor_panel_lines("Panel B: Gender", "Female Share"),
  "\\addlinespace",
  paste0(
    "Industry FE & ",
    paste(industry_fe_cells, collapse = " & "),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

yimfor_table_path <- file.path(
  tables,
  "average_beliefs_vs_linkedin_workforce_shares.tex"
)
write_lines_checked(
  yimfor_latex_lines,
  yimfor_table_path,
  label = "average beliefs versus LinkedIn workforce shares table"
)

message("🎃 Generated average_beliefs_vs_linkedin_workforce_shares.tex")
