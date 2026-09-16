source("code/globals.R")

root_dir <- intermediate
table8_subdir <- "Full_Sample"

# ------------------------------------------------------------------------------
# Build Table: EIV_univariate_wt_ols_borda_w_gender_sq.tex
# Purpose: Selectivity / Discretion univariate regressions, Likert and Borda
# side by side, one panel for Race and one panel for Gender, coefficient over
# standard error. No squared
# regressors (kept the original output filename since it's what's referenced
# downstream; flag if you'd like it renamed to drop the now-inaccurate "sq").
# Selectivity and Discretion each get their own column (rather than sharing a
# row's worth of columns), since each is estimated as a fully separate
# univariate model --- one column per model makes that unambiguous.
# ------------------------------------------------------------------------------

reg8_n_cols <- 8L  # (Likert, Borda) x (Selectivity, Discretion) x (No FE, Industry FE)

reg8_fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

reg8_fmt_se <- function(x) {
  x <- as.numeric(x)
  if (any(x < -sqrt(.Machine$double.eps), na.rm = TRUE)) {
    stop("Negative standard error found in EIV table inputs.", call. = FALSE)
  }
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", abs(x)), ")"))
}

reg8_latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

reg8_empty_row <- function(label = "", n_cols = reg8_n_cols) {
  as.list(stats::setNames(
    c(label, rep("", n_cols)),
    c("row_label", paste0("col", seq_len(n_cols)))
  ))
}

reg8_value_row <- function(label, values, formatter) {
  as.list(stats::setNames(
    c(label, formatter(values)),
    c("row_label", paste0("col", seq_along(values)))
  ))
}

# Single (model, coef) EIV_firm sheet lookup, returning a raw numeric estimate or SE
reg8_pull_stat <- function(lhs_var, rhs_var, coef_num, model_filter,
                           statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  dat <- tryCatch(read_parquet_sheet(file.path(root_dir, table8_subdir), "EIV_firm"),
                  error = function(e) tibble())
  if (!nrow(dat)) return(NA_real_)

  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  out <- dat[
    dat$model == model_filter &
      dat$lhs == lhs_var &
      dat$rhs == rhs_var &
      dat$formula == rhs_var &
      dat$coef == coef_num, ]

  if (!nrow(out)) return(NA_real_)

  if (statistic == "estimate") {
    suppressWarnings(as.numeric(out$sample_est[1]))
  } else {
    suppressWarnings(as.numeric(out$sample_se[1]))
  }
}

# Column layout (8 separate univariate models per panel):
# 1: Likert Selectivity No-FE   2: Likert Selectivity Industry-FE
# 3: Likert Discretion No-FE    4: Likert Discretion Industry-FE
# 5: Borda Selectivity No-FE    6: Borda Selectivity Industry-FE
# 7: Borda Discretion No-FE     8: Borda Discretion Industry-FE
reg8_selectivity_cols <- c(1L, 2L, 5L, 6L)
reg8_discretion_cols  <- c(3L, 4L, 7L, 8L)

# One regressor's 4 values (Likert No-FE, Likert Industry-FE, Borda No-FE, Borda Industry-FE)
reg8_row_cells <- function(lhs, rhs_var, statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  specs <- list(
    c(model = "OLS",   coef = "1"),
    c(model = "OLS",   coef = "2"),
    c(model = "Borda", coef = "1"),
    c(model = "Borda", coef = "2")
  )
  vapply(specs, function(spec) {
    reg8_pull_stat(lhs, rhs_var, coef_num = as.integer(spec[["coef"]]),
                   model_filter = spec[["model"]], statistic = statistic)
  }, numeric(1))
}

# Place a regressor's 4 values into its own 4 columns (Likert x Borda, No-FE x FE), NA elsewhere ---
# this is what leaves the other regressor's columns blank in that row, since each is a separate model.
reg8_sparse_row <- function(values, cols) {
  out <- rep(NA_real_, reg8_n_cols)
  out[cols] <- values
  out
}

reg8_panel_rows <- function(panel_label, lhs) {
  selectivity_est <- reg8_sparse_row(reg8_row_cells(lhs, "FirmSelective", "estimate"), reg8_selectivity_cols)
  selectivity_se  <- reg8_sparse_row(reg8_row_cells(lhs, "FirmSelective", "se"),       reg8_selectivity_cols)
  discretion_est  <- reg8_sparse_row(reg8_row_cells(lhs, "discretion", "estimate"),    reg8_discretion_cols)
  discretion_se   <- reg8_sparse_row(reg8_row_cells(lhs, "discretion", "se"),          reg8_discretion_cols)

  tibble::as_tibble(dplyr::bind_rows(
    reg8_empty_row(panel_label),
    reg8_value_row("Selectivity", selectivity_est, reg8_fmt3),
    reg8_value_row("", selectivity_se, reg8_fmt_se),
    reg8_value_row("Discretion", discretion_est, reg8_fmt3),
    reg8_value_row("", discretion_se, reg8_fmt_se),
    reg8_empty_row("")
  ))
}

table8_panel_df <- dplyr::bind_rows(
  reg8_panel_rows("Panel A: Race", "log_dif"),
  reg8_panel_rows("Panel B: Gender", "log_dif_gender"),
  tibble::as_tibble(dplyr::bind_rows(
    reg8_value_row("Industry FE", c("", "X", "", "X", "", "X", "", "X"), identity)
  ))
)

latex_lines_sq <- c(
  "\\begin{tabular}{lcccccccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  paste(reg8_latex_escape(c("", paste0("(", seq_len(reg8_n_cols), ")"))), collapse = " & "),
  "\\\\",
  "\\midrule"
)

for (i in seq_len(nrow(table8_panel_df))) {
  row_label <- table8_panel_df$row_label[i]
  cells <- unname(unlist(table8_panel_df[i, paste0("col", seq_len(reg8_n_cols))], use.names = FALSE))

  if (grepl("^Panel ", row_label)) {
    latex_lines_sq <- c(
      latex_lines_sq,
      paste0("\\multicolumn{9}{l}{\\textbf{", reg8_latex_escape(row_label), "}} \\\\")
    )
  } else if (!nzchar(row_label) && all(!nzchar(cells))) {
    latex_lines_sq <- c(latex_lines_sq, "\\addlinespace")
  } else {
    latex_lines_sq <- c(
      latex_lines_sq,
      paste(reg8_latex_escape(c(row_label, cells)), collapse = " & "),
      "\\\\"
    )
  }
}

latex_lines_sq <- c(latex_lines_sq, "\\bottomrule", "\\end{tabular}")

out_tex_sq <- file.path(tables, "EIV_univariate_wt_ols_borda_w_gender_sq.tex")
write_lines_checked(latex_lines_sq, out_tex_sq, label = "EIV selectivity/discretion regression table")
message("✓ LaTeX Table 8 (Likert/Borda panels) saved to: ", out_tex_sq)
