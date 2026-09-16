# ------------------------------------------------------------------------------
# Purpose: Table 7, pooled discrimination beliefs with selectivity controls
#
# The multivariate belief/selectivity specifications currently live in the
# EIV_eeo1_industry_shares intermediate sheet, but these rows do not use EEO-1
# variables. The EEO-1 control and outcome tables formerly written alongside
# Table 7 are not used in the draft and have been removed from Section 3.
# ------------------------------------------------------------------------------

source("code/globals.R")

tables_out <- Sys.getenv("EIV_TABLES_DIR", unset = tables)
full_sample_dir <- file.path(intermediate, "Full_Sample")
selectivity_eiv_sheet <- "EIV_eeo1_industry_shares"
table_n_cols <- 8L

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
  gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
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

  value_column <- if (statistic == "estimate") "sample_est" else "sample_se"
  suppressWarnings(as.numeric(out[[value_column]][1]))
}

new_empty_row <- function(label = "") {
  as.list(stats::setNames(
    c(label, rep("", table_n_cols)),
    c("row_label", paste0("col", seq_len(table_n_cols)))
  ))
}

make_value_row <- function(label, values, formatter) {
  as.list(stats::setNames(
    c(label, formatter(values)),
    c("row_label", paste0("col", seq_along(values)))
  ))
}

selectivity_control_cells <- function(eiv_firm, eiv_selectivity, lhs,
                                      belief_rhs, target_rhs,
                                      statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, table_n_cols)
  names(out) <- paste0("col", seq_len(table_n_cols))

  model_columns <- list(
    OLS = c(1L, 2L, 3L, 4L),
    Borda = c(5L, 6L, 7L, 8L)
  )

  for (model in names(model_columns)) {
    cols <- model_columns[[model]]

    if (target_rhs == belief_rhs) {
      out[cols[[1L]]] <- pull_eiv_value(
        eiv_df = eiv_firm,
        lhs_var = lhs,
        rhs_var = belief_rhs,
        model_filter = model,
        coef_num = 1L,
        formula_filter = belief_rhs,
        statistic = statistic
      )
      out[cols[[2L]]] <- pull_eiv_value(
        eiv_df = eiv_firm,
        lhs_var = lhs,
        rhs_var = belief_rhs,
        model_filter = model,
        coef_num = 2L,
        formula_filter = belief_rhs,
        statistic = statistic
      )
    }

    out[cols[[3L]]] <- pull_eiv_value(
      eiv_df = eiv_selectivity,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 1L,
      formula_filter = paste(belief_rhs, "FirmSelective", sep = " + "),
      statistic = statistic
    )
    out[cols[[4L]]] <- pull_eiv_value(
      eiv_df = eiv_selectivity,
      lhs_var = lhs,
      rhs_var = target_rhs,
      model_filter = model,
      coef_num = 2L,
      formula_filter = paste(belief_rhs, "FirmSelective", sep = " + "),
      statistic = statistic
    )
  }

  out
}

make_panel_rows <- function(panel_label, eiv_firm, eiv_selectivity, lhs,
                            belief_rhs) {
  belief_est <- selectivity_control_cells(
    eiv_firm, eiv_selectivity, lhs, belief_rhs, belief_rhs, "estimate"
  )
  belief_se <- selectivity_control_cells(
    eiv_firm, eiv_selectivity, lhs, belief_rhs, belief_rhs, "se"
  )
  selectivity_est <- selectivity_control_cells(
    eiv_firm, eiv_selectivity, lhs, belief_rhs, "FirmSelective", "estimate"
  )
  selectivity_se <- selectivity_control_cells(
    eiv_firm, eiv_selectivity, lhs, belief_rhs, "FirmSelective", "se"
  )

  tibble::as_tibble(dplyr::bind_rows(
    new_empty_row(panel_label),
    make_value_row("Discrimination Beliefs", belief_est, fmt3),
    make_value_row("", belief_se, fmt_se),
    make_value_row("Selectivity Beliefs", selectivity_est, fmt3),
    make_value_row("", selectivity_se, fmt_se),
    new_empty_row("")
  ))
}

make_table_df <- function(eiv_firm, eiv_selectivity) {
  dplyr::bind_rows(
    make_panel_rows(
      panel_label = "Panel A: Race",
      eiv_firm = eiv_firm,
      eiv_selectivity = eiv_selectivity,
      lhs = "log_dif",
      belief_rhs = "pooled_favor_white"
    ),
    make_panel_rows(
      panel_label = "Panel B: Gender",
      eiv_firm = eiv_firm,
      eiv_selectivity = eiv_selectivity,
      lhs = "log_dif_gender",
      belief_rhs = "pooled_favor_male"
    ),
    tibble::as_tibble(dplyr::bind_rows(
      make_value_row("Industry FE", c("", "X", "", "X", "", "X", "", "X"), identity)
    ))
  )
}

write_table <- function(eiv_firm, eiv_selectivity) {
  table_df <- make_table_df(eiv_firm, eiv_selectivity)
  lines <- c(
    "\\begin{tabular}{lcccccccc}",
    "\\toprule",
    " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    paste(latex_escape(c("", paste0("(", seq_len(table_n_cols), ")"))), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(table_df))) {
    row_label <- table_df$row_label[i]
    cells <- unname(unlist(
      table_df[i, paste0("col", seq_len(table_n_cols))],
      use.names = FALSE
    ))

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
    "EIV_full_sample_pooled_belief_selectivity_controls.tex"
  )
  write_lines_checked(
    lines,
    out_tex,
    label = "Full-sample pooled-belief EIV table with selectivity controls"
  )
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading full-sample EIV sheets from: ", full_sample_dir)
message("Writing Table 7 to: ", tables_out)

eiv_firm <- read_full_sample_sheet("EIV_firm")
eiv_selectivity <- read_full_sample_sheet(selectivity_eiv_sheet)
if ("eeo1_spec_group" %in% names(eiv_selectivity)) {
  eiv_selectivity <- eiv_selectivity %>%
    dplyr::filter(.data$eeo1_spec_group == "belief_selectivity")
}

write_table(eiv_firm, eiv_selectivity)

message("Table 7 complete.")
