# ------------------------------------------------------------------------------
# Purpose: EIV tables using EEO-1 industry-level race/gender shares
#
# Section 2 writes EIV_eeo1_industry_shares. This script formats the full-sample
# pooled-belief table with EEO-1 share controls and the full-sample table with
# EEO-1 shares as industry-level outcomes.
# ------------------------------------------------------------------------------

source("code/globals.R")

tables_out <- Sys.getenv("EEO1_EIV_TABLES_DIR", unset = tables)
full_sample_dir <- file.path(intermediate, "Full_Sample")
eeo1_eiv_sheet <- "EIV_eeo1_industry_shares"
controls_n_cols <- 8L

fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

fmt_se <- function(x) {
  x <- as.numeric(x)
  if (any(x < -sqrt(.Machine$double.eps), na.rm = TRUE)) {
    stop("Negative standard error found in EIV table inputs.", call. = FALSE)
  }
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", abs(x)), ")"))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

read_full_sample_sheet <- function(sheet) {
  read_parquet_sheet(full_sample_dir, sheet)
}

pull_eiv_value <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num,
                           formula_filter = NULL,
                           statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  if (!nrow(eiv_df)) return(NA_real_)

  eiv_df$coef <- suppressWarnings(as.numeric(eiv_df$coef))

  out <- eiv_df %>%
    dplyr::filter(
      .data$model == model_filter,
      .data$lhs == lhs_var,
      .data$rhs == rhs_var,
      .data$coef == coef_num
    )

  if (!is.null(formula_filter) && "formula" %in% names(out)) {
    out <- out %>% dplyr::filter(.data$formula == formula_filter)
  }

  if (!nrow(out)) return(NA_real_)

  if (statistic == "estimate") {
    suppressWarnings(as.numeric(out$sample_est[1]))
  } else {
    suppressWarnings(as.numeric(out$sample_se[1]))
  }
}

rhs_formula <- function(...) {
  vars <- unlist(list(...), use.names = FALSE)
  vars <- vars[!is.na(vars) & nzchar(vars)]
  paste(vars, collapse = " + ")
}

new_empty_row <- function(label = "", n_cols = controls_n_cols) {
  as.list(stats::setNames(
    c(label, rep("", n_cols)),
    c("row_label", paste0("col", seq_len(n_cols)))
  ))
}

make_value_row <- function(label, values, formatter) {
  as.list(stats::setNames(
    c(label, formatter(values)),
    c("row_label", paste0("col", seq_along(values)))
  ))
}

control_belief_cells <- function(eiv_firm, eiv_eeo1, lhs, belief_rhs,
                                 entry_share_rhs, all_share_rhs,
                                 statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, controls_n_cols)
  names(out) <- paste0("col", seq_len(controls_n_cols))

  col_index <- 1L
  for (model in c("OLS", "Borda")) {
    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_firm,
      lhs_var = lhs,
      rhs_var = belief_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = belief_rhs,
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_firm,
      lhs_var = lhs,
      rhs_var = belief_rhs,
      model_filter = model,
      coef_num = 2L,
      formula_filter = belief_rhs,
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = belief_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, entry_share_rhs),
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = belief_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, all_share_rhs),
      statistic = statistic
    )
    col_index <- col_index + 1L
  }

  out
}

control_share_cells <- function(eiv_eeo1, lhs, belief_rhs, share_rhs, target_cols,
                                statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, controls_n_cols)
  names(out) <- paste0("col", seq_len(controls_n_cols))

  for (i in seq_along(target_cols)) {
    out[target_cols[[i]]] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = share_rhs,
      model_filter = c("OLS", "Borda")[[i]],
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, share_rhs),
      statistic = statistic
    )
  }

  out
}

make_control_panel_rows <- function(panel_label, eiv_firm, eiv_eeo1, lhs,
                                    belief_rhs, entry_share_rhs, all_share_rhs) {
  belief_est <- control_belief_cells(eiv_firm, eiv_eeo1, lhs, belief_rhs,
                                     entry_share_rhs, all_share_rhs, "estimate")
  belief_se <- control_belief_cells(eiv_firm, eiv_eeo1, lhs, belief_rhs,
                                    entry_share_rhs, all_share_rhs, "se")
  entry_share_est <- control_share_cells(eiv_eeo1, lhs, belief_rhs,
                                         entry_share_rhs, c(3L, 7L),
                                         "estimate")
  entry_share_se <- control_share_cells(eiv_eeo1, lhs, belief_rhs,
                                        entry_share_rhs, c(3L, 7L), "se")
  all_share_est <- control_share_cells(eiv_eeo1, lhs, belief_rhs,
                                       all_share_rhs, c(4L, 8L),
                                       "estimate")
  all_share_se <- control_share_cells(eiv_eeo1, lhs, belief_rhs,
                                      all_share_rhs, c(4L, 8L), "se")

  tibble::as_tibble(dplyr::bind_rows(
    new_empty_row(panel_label),
    make_value_row("Beliefs", belief_est, fmt3),
    make_value_row("", belief_se, fmt_se),
    make_value_row("EEO-1 Entry Share", entry_share_est, fmt3),
    make_value_row("", entry_share_se, fmt_se),
    make_value_row("EEO-1 All-Worker Share", all_share_est, fmt3),
    make_value_row("", all_share_se, fmt_se),
    new_empty_row("")
  ))
}

make_controls_table_df <- function(eiv_firm, eiv_eeo1) {
  dplyr::bind_rows(
    make_control_panel_rows(
      panel_label = "Panel A: Race",
      eiv_firm = eiv_firm,
      eiv_eeo1 = eiv_eeo1,
      lhs = "log_dif",
      belief_rhs = "pooled_favor_white",
      entry_share_rhs = "eeo1_black_entry_level_share",
      all_share_rhs = "eeo1_black_all_jobs_share"
    ),
    make_control_panel_rows(
      panel_label = "Panel B: Gender",
      eiv_firm = eiv_firm,
      eiv_eeo1 = eiv_eeo1,
      lhs = "log_dif_gender",
      belief_rhs = "pooled_favor_male",
      entry_share_rhs = "eeo1_female_entry_level_share",
      all_share_rhs = "eeo1_female_all_jobs_share"
    ),
    tibble::as_tibble(dplyr::bind_rows(
      make_value_row("Industry FE", c("", "X", "", "", "", "X", "", ""), identity)
    ))
  )
}

write_controls_table <- function(eiv_firm, eiv_eeo1) {
  table_df <- make_controls_table_df(eiv_firm, eiv_eeo1)

  lines <- c(
    "\\begin{tabular}{lcccccccc}",
    "\\toprule",
    " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    paste(latex_escape(c("", paste0("(", seq_len(controls_n_cols), ")"))), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_df))) {
    row_label <- table_df$row_label[i]
    cells <- unname(unlist(table_df[i, paste0("col", seq_len(controls_n_cols))], use.names = FALSE))

    if (grepl("^Panel ", row_label)) {
      lines <- c(
        lines,
        paste0("\\multicolumn{9}{l}{\\textbf{", latex_escape(row_label), "}} \\\\")
      )
    } else if (!nzchar(row_label) && all(!nzchar(cells))) {
      lines <- c(lines, "\\addlinespace")
    } else {
      lines <- c(
        lines,
        paste(latex_escape(c(row_label, cells)), collapse = " & "),
        "\\\\"
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(
    tables_out,
    "EIV_eeo1_full_sample_pooled_share_controls.tex"
  )
  write_lines_checked(lines, out_tex, label = "Full-sample EEO-1 share-control EIV table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

selectivity_control_cells <- function(eiv_eeo1, lhs, belief_rhs, target_rhs,
                                      entry_share_rhs, all_share_rhs,
                                      statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, controls_n_cols)
  names(out) <- paste0("col", seq_len(controls_n_cols))

  col_index <- 1L
  for (model in c("OLS", "Borda")) {
    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, "FirmSelective"),
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 2L,
      formula_filter = rhs_formula(belief_rhs, "FirmSelective"),
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, "FirmSelective", entry_share_rhs),
      statistic = statistic
    )
    col_index <- col_index + 1L

    out[col_index] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, "FirmSelective", all_share_rhs),
      statistic = statistic
    )
    col_index <- col_index + 1L
  }

  out
}

selectivity_share_cells <- function(eiv_eeo1, lhs, belief_rhs, selectivity_rhs,
                                    share_rhs, target_cols,
                                    statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, controls_n_cols)
  names(out) <- paste0("col", seq_len(controls_n_cols))

  for (i in seq_along(target_cols)) {
    out[target_cols[[i]]] <- pull_eiv_value(
      eiv_df = eiv_eeo1,
      lhs_var = lhs,
      rhs_var = share_rhs,
      model_filter = c("OLS", "Borda")[[i]],
      coef_num = 1L,
      formula_filter = rhs_formula(belief_rhs, selectivity_rhs, share_rhs),
      statistic = statistic
    )
  }

  out
}

make_selectivity_control_panel_rows <- function(panel_label, eiv_eeo1, lhs,
                                                belief_rhs, entry_share_rhs,
                                                all_share_rhs) {
  belief_est <- selectivity_control_cells(
    eiv_eeo1, lhs, belief_rhs, belief_rhs,
    entry_share_rhs, all_share_rhs, "estimate"
  )
  belief_se <- selectivity_control_cells(
    eiv_eeo1, lhs, belief_rhs, belief_rhs,
    entry_share_rhs, all_share_rhs, "se"
  )
  selectivity_est <- selectivity_control_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    entry_share_rhs, all_share_rhs, "estimate"
  )
  selectivity_se <- selectivity_control_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    entry_share_rhs, all_share_rhs, "se"
  )
  entry_share_est <- selectivity_share_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    entry_share_rhs, c(3L, 7L), "estimate"
  )
  entry_share_se <- selectivity_share_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    entry_share_rhs, c(3L, 7L), "se"
  )
  all_share_est <- selectivity_share_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    all_share_rhs, c(4L, 8L), "estimate"
  )
  all_share_se <- selectivity_share_cells(
    eiv_eeo1, lhs, belief_rhs, "FirmSelective",
    all_share_rhs, c(4L, 8L), "se"
  )

  tibble::as_tibble(dplyr::bind_rows(
    new_empty_row(panel_label),
    make_value_row("Discrimination Beliefs", belief_est, fmt3),
    make_value_row("", belief_se, fmt_se),
    make_value_row("Selectivity Beliefs", selectivity_est, fmt3),
    make_value_row("", selectivity_se, fmt_se),
    make_value_row("EEO-1 Entry Share", entry_share_est, fmt3),
    make_value_row("", entry_share_se, fmt_se),
    make_value_row("EEO-1 All-Worker Share", all_share_est, fmt3),
    make_value_row("", all_share_se, fmt_se),
    new_empty_row("")
  ))
}

make_selectivity_controls_table_df <- function(eiv_eeo1) {
  dplyr::bind_rows(
    make_selectivity_control_panel_rows(
      panel_label = "Panel A: Race",
      eiv_eeo1 = eiv_eeo1,
      lhs = "log_dif",
      belief_rhs = "pooled_favor_white",
      entry_share_rhs = "eeo1_black_entry_level_share",
      all_share_rhs = "eeo1_black_all_jobs_share"
    ),
    make_selectivity_control_panel_rows(
      panel_label = "Panel B: Gender",
      eiv_eeo1 = eiv_eeo1,
      lhs = "log_dif_gender",
      belief_rhs = "pooled_favor_male",
      entry_share_rhs = "eeo1_female_entry_level_share",
      all_share_rhs = "eeo1_female_all_jobs_share"
    ),
    tibble::as_tibble(dplyr::bind_rows(
      make_value_row("Industry FE", c("", "X", "", "", "", "X", "", ""), identity)
    ))
  )
}

write_selectivity_controls_table <- function(eiv_eeo1) {
  table_df <- make_selectivity_controls_table_df(eiv_eeo1)

  lines <- c(
    "\\begin{tabular}{lcccccccc}",
    "\\toprule",
    " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    paste(latex_escape(c("", paste0("(", seq_len(controls_n_cols), ")"))), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_df))) {
    row_label <- table_df$row_label[i]
    cells <- unname(unlist(table_df[i, paste0("col", seq_len(controls_n_cols))], use.names = FALSE))

    if (grepl("^Panel ", row_label)) {
      lines <- c(
        lines,
        paste0("\\multicolumn{9}{l}{\\textbf{", latex_escape(row_label), "}} \\\\")
      )
    } else if (!nzchar(row_label) && all(!nzchar(cells))) {
      lines <- c(lines, "\\addlinespace")
    } else {
      lines <- c(
        lines,
        paste(latex_escape(c(row_label, cells)), collapse = " & "),
        "\\\\"
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(
    tables_out,
    "EIV_eeo1_full_sample_pooled_share_controls_selectivity.tex"
  )
  write_lines_checked(lines, out_tex, label = "Full-sample EEO-1 share-control EIV table with selectivity")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

outcome_specs <- list(
  list(
    panel_label = "Panel A: Black Share",
    lhs = "eeo1_black_entry_level_share",
    rhs_pooled = "pooled_favor_white_im_w"
  ),
  list(
    panel_label = "Panel B: Female Share",
    lhs = "eeo1_female_entry_level_share",
    rhs_pooled = "pooled_favor_male_im_w"
  )
)

make_outcome_table_df <- function(eiv_eeo1, configs = outcome_specs) {
  dplyr::bind_rows(lapply(configs, function(cfg) {
    make_cell <- function(model_filter, statistic) {
      value <- pull_eiv_value(
        eiv_df = eiv_eeo1,
        lhs_var = cfg$lhs,
        rhs_var = cfg$rhs_pooled,
        model_filter = model_filter,
        coef_num = 1L,
        formula_filter = cfg$rhs_pooled,
        statistic = statistic
      )

      if (statistic == "estimate") {
        fmt3(value)
      } else {
        fmt_se(value)
      }
    }

    tibble::tibble(
      row_label = c(cfg$panel_label, "Beliefs", ""),
      col1 = c(
        "",
        make_cell("OLS", "estimate"),
        make_cell("OLS", "se")
      ),
      col2 = c(
        "",
        make_cell("Borda", "estimate"),
        make_cell("Borda", "se")
      )
    )
  }))
}

write_outcomes_table <- function(eiv_eeo1) {
  table_df <- make_outcome_table_df(eiv_eeo1)

  lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Likert & Borda \\\\",
    paste(latex_escape(c("", "(1)", "(2)")), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_df))) {
    row_values <- unname(unlist(table_df[i, ], use.names = FALSE))

    if (grepl("^Panel ", row_values[1])) {
      if (i > 1L) lines <- c(lines, "\\addlinespace")
      lines <- c(
        lines,
        paste0("\\multicolumn{3}{l}{\\textbf{", latex_escape(row_values[1]), "}} \\\\")
      )
    } else {
      lines <- c(
        lines,
        paste(latex_escape(row_values), collapse = " & "),
        "\\\\"
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(
    tables_out,
    "EIV_eeo1_full_sample_pooled_share_outcomes.tex"
  )
  write_lines_checked(lines, out_tex, label = "Full-sample EEO-1 share-outcome EIV table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading full-sample EIV sheets from: ", full_sample_dir)
message("Writing EEO-1 EIV tables to: ", tables_out)

eiv_firm <- read_full_sample_sheet("EIV_firm")
eiv_eeo1 <- read_full_sample_sheet(eeo1_eiv_sheet)

write_controls_table(eiv_firm, eiv_eeo1)
write_selectivity_controls_table(eiv_eeo1)
write_outcomes_table(eiv_eeo1)

message("EEO-1 EIV share tables complete.")
