source("code/globals.R")

tables_out <- Sys.getenv("EIV_WITHIN_BETWEEN_SELECTIVITY_TABLES_DIR", unset = tables)
full_sample_dir <- file.path(intermediate, "Full_Sample")

fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

fmt_se <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", abs(x)), ")"))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

rhs_formula <- function(...) {
  vars <- unlist(list(...), use.names = FALSE)
  vars <- vars[!is.na(vars) & nzchar(vars)]
  paste(vars, collapse = " + ")
}

read_eiv_selectivity_sheet <- function(sheet) {
  path <- parquet_sheet_path(full_sample_dir, sheet)
  if (!file.exists(path)) {
    stop(
      "Missing ", basename(path), ". Re-run section 2 so the ",
      sheet, " sheet is written before creating this table.",
      call. = FALSE
    )
  }

  read_parquet_sheet(full_sample_dir, sheet)
}

pull_eiv_stat <- function(eiv_df, lhs_var, rhs_var, model_filter, formula_filter,
                          statistic = c("estimate", "se", "n")) {
  statistic <- match.arg(statistic)
  if (!nrow(eiv_df)) return(NA_real_)

  eiv_df$coef <- suppressWarnings(as.numeric(eiv_df$coef))

  out <- eiv_df |>
    dplyr::filter(
      .data$model == model_filter,
      .data$lhs == lhs_var,
      .data$rhs == rhs_var,
      .data$coef == 1L,
      .data$formula == formula_filter
    )

  if (!nrow(out)) return(NA_real_)

  if (statistic == "estimate") return(suppressWarnings(as.numeric(out$sample_est[1])))
  if (statistic == "se") return(suppressWarnings(as.numeric(out$sample_se[1])))
  suppressWarnings(as.numeric(out$n[1]))
}

make_cell_vector <- function(within_eiv, between_eiv, rhs_between, rhs_within,
                             statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)

  c(
    pull_eiv_stat(
      eiv_df = between_eiv,
      lhs_var = "log_dif_im_w",
      rhs_var = rhs_between,
      model_filter = "OLS",
      formula_filter = rhs_formula("pooled_favor_white_im_w", "FirmSelective_im_w"),
      statistic = statistic
    ),
    pull_eiv_stat(
      eiv_df = within_eiv,
      lhs_var = "log_dif_dm_w",
      rhs_var = rhs_within,
      model_filter = "OLS",
      formula_filter = rhs_formula("pooled_favor_white_dm_w", "FirmSelective_dm_w"),
      statistic = statistic
    ),
    pull_eiv_stat(
      eiv_df = between_eiv,
      lhs_var = "log_dif_im_w",
      rhs_var = rhs_between,
      model_filter = "Borda",
      formula_filter = rhs_formula("pooled_favor_white_im_w", "FirmSelective_im_w"),
      statistic = statistic
    ),
    pull_eiv_stat(
      eiv_df = within_eiv,
      lhs_var = "log_dif_dm_w",
      rhs_var = rhs_within,
      model_filter = "Borda",
      formula_filter = rhs_formula("pooled_favor_white_dm_w", "FirmSelective_dm_w"),
      statistic = statistic
    )
  )
}

make_value_row <- function(label, values, formatter) {
  as.list(stats::setNames(
    c(label, formatter(values)),
    c("row_label", paste0("col", seq_along(values)))
  ))
}

make_race_within_between_selectivity_df <- function(within_eiv, between_eiv) {
  discrimination_est <- make_cell_vector(
    within_eiv = within_eiv,
    between_eiv = between_eiv,
    rhs_between = "pooled_favor_white_im_w",
    rhs_within = "pooled_favor_white_dm_w",
    statistic = "estimate"
  )
  discrimination_se <- make_cell_vector(
    within_eiv = within_eiv,
    between_eiv = between_eiv,
    rhs_between = "pooled_favor_white_im_w",
    rhs_within = "pooled_favor_white_dm_w",
    statistic = "se"
  )
  selectivity_est <- make_cell_vector(
    within_eiv = within_eiv,
    between_eiv = between_eiv,
    rhs_between = "FirmSelective_im_w",
    rhs_within = "FirmSelective_dm_w",
    statistic = "estimate"
  )
  selectivity_se <- make_cell_vector(
    within_eiv = within_eiv,
    between_eiv = between_eiv,
    rhs_between = "FirmSelective_im_w",
    rhs_within = "FirmSelective_dm_w",
    statistic = "se"
  )
  n_values <- c(
    pull_eiv_stat(between_eiv, "log_dif_im_w", "pooled_favor_white_im_w", "OLS",
                  rhs_formula("pooled_favor_white_im_w", "FirmSelective_im_w"), "n"),
    pull_eiv_stat(within_eiv, "log_dif_dm_w", "pooled_favor_white_dm_w", "OLS",
                  rhs_formula("pooled_favor_white_dm_w", "FirmSelective_dm_w"), "n"),
    pull_eiv_stat(between_eiv, "log_dif_im_w", "pooled_favor_white_im_w", "Borda",
                  rhs_formula("pooled_favor_white_im_w", "FirmSelective_im_w"), "n"),
    pull_eiv_stat(within_eiv, "log_dif_dm_w", "pooled_favor_white_dm_w", "Borda",
                  rhs_formula("pooled_favor_white_dm_w", "FirmSelective_dm_w"), "n")
  )

  tibble::as_tibble(dplyr::bind_rows(
    make_value_row("Discrimination Beliefs", discrimination_est, fmt3),
    make_value_row("", discrimination_se, fmt_se),
    make_value_row("Selectivity Beliefs", selectivity_est, fmt3),
    make_value_row("", selectivity_se, fmt_se),
    make_value_row("N", n_values, function(x) ifelse(is.na(x), "", sprintf("%d", as.integer(x))))
  ))
}

write_race_within_between_selectivity_table <- function(within_eiv, between_eiv) {
  table_df <- make_race_within_between_selectivity_df(within_eiv, between_eiv)

  lines <- c(
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Likert} & \\multicolumn{2}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    " & Between & Within & Between & Within \\\\",
    paste(latex_escape(c("", "(1)", "(2)", "(3)", "(4)")), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_df))) {
    row_values <- unname(unlist(table_df[i, ], use.names = FALSE))
    lines <- c(
      lines,
      paste(latex_escape(row_values), collapse = " & "),
      "\\\\"
    )
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(tables_out, "EIV_race_within_between_selectivity_ols_borda.tex")
  write_lines_checked(lines, out_tex, label = "race within/between selectivity EIV table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading within/between selectivity EIV sheets from: ", full_sample_dir)
message("Writing within/between selectivity EIV table to: ", tables_out)

within_selectivity_eiv <- read_eiv_selectivity_sheet("EIV_within_selectivity")
between_selectivity_eiv <- read_eiv_selectivity_sheet("EIV_between_selectivity")

write_race_within_between_selectivity_table(
  within_eiv = within_selectivity_eiv,
  between_eiv = between_selectivity_eiv
)

message("Within/between selectivity EIV table complete.")
