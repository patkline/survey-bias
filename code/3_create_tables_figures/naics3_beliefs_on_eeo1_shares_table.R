# ------------------------------------------------------------------------------
# Purpose: Create the NAICS3 belief-on-EEO-1-share regression table
# ------------------------------------------------------------------------------

naics3_results <- read_parquet_sheet(
  file.path(intermediate, "Full_Sample"),
  "NAICS3_belief_share_regressions"
)

if (nrow(naics3_results) != 16L) {
  stop("Expected 16 NAICS3 belief-share regression results.")
}

format_estimate <- function(result) {
  sprintf("%.3f", result$effect_per_10pp_share)
}

format_standard_error <- function(result) {
  paste0("(", sprintf("%.3f", result$effect_per_10pp_se), ")")
}

table_columns <- tibble::tribble(
  ~belief_model, ~specification,
  "OLS", "firm_level",
  "OLS", "equal_naics3",
  "Borda", "firm_level",
  "Borda", "equal_naics3"
)

pull_result <- function(outcome, job_scope, belief_model, specification) {
  result <- naics3_results |>
    dplyr::filter(
      .data$belief_outcome == .env$outcome,
      .data$eeo1_job_scope == .env$job_scope,
      .data$belief_model == .env$belief_model,
      .data$specification == .env$specification
    )

  if (nrow(result) != 1L) {
    stop("Could not identify a unique NAICS3 regression result.")
  }
  result
}

make_specification_lines <- function(label, outcome, job_scope) {
  column_results <- lapply(seq_len(nrow(table_columns)), function(column_index) {
    pull_result(
      outcome = outcome,
      job_scope = job_scope,
      belief_model = table_columns$belief_model[column_index],
      specification = table_columns$specification[column_index]
    )
  })

  c(
    paste0(
      label,
      " & ",
      paste(vapply(column_results, format_estimate, character(1L)), collapse = " & "),
      " \\\\"
    ),
    paste0(
      " & ",
      paste(vapply(column_results, format_standard_error, character(1L)), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "NAICS3 industries & ",
      paste(vapply(column_results, function(x) as.character(x$n_naics3), character(1L)), collapse = " & "),
      " \\\\"
    ),
    paste0(
      "Firms & ",
      paste(vapply(column_results, function(x) as.character(x$n_firms), character(1L)), collapse = " & "),
      " \\\\"
    )
  )
}

latex_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Likert} & \\multicolumn{2}{c}{Borda} \\\\ ",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Firm-level & Equal NAICS3 & Firm-level & Equal NAICS3 \\\\ ",
  "\\midrule",
  "\\multicolumn{5}{l}{\\textbf{Panel A: Race}} \\\\ ",
  make_specification_lines(
    "Black share: all jobs",
    "pooled_favor_white",
    "all_jobs"
  ),
  "\\addlinespace",
  make_specification_lines(
    "Black share: front-line jobs",
    "pooled_favor_white",
    "front_line"
  ),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\textbf{Panel B: Gender}} \\\\ ",
  make_specification_lines(
    "Female share: all jobs",
    "pooled_favor_male",
    "all_jobs"
  ),
  "\\addlinespace",
  make_specification_lines(
    "Female share: front-line jobs",
    "pooled_favor_male",
    "front_line"
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

write_lines_checked(
  latex_lines,
  file.path(tables, "naics3_beliefs_on_eeo1_shares.tex"),
  label = "NAICS3 belief-share regression table"
)

message("🎃 Generated naics3_beliefs_on_eeo1_shares.tex")
