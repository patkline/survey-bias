source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Pull estimates from unified EIV_firm sheet with model + formula filters
pull_eiv_firm_est <- function(root, subdir, lhs_var, rhs_var, coef_num,
                              formula_var, model_filter = "OLS",
                              sheet = "EIV_firm", divide_by_100 = FALSE) {
  dir_path <- file.path(root, subdir)
  dat <- tryCatch(read_parquet_sheet(dir_path, sheet),
                  error = function(e) tibble())
  if (!nrow(dat)) return("NA (NA)")

  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  out <- dat[
    dat$model == model_filter &
      dat$lhs == lhs_var &
      dat$rhs == rhs_var &
      dat$formula == formula_var &
      dat$coef == coef_num, ]

  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est))
  se  <- suppressWarnings(as.numeric(out$sample_se))

  if (isTRUE(divide_by_100)) {
    est <- est / 100
    se  <- se  / 100
  }
  paste0(fmt3(est), " (", fmt3(se), ")")
}

root_dir <- intermediate

# ------------------------------------------------------------------------------
# Build Table: EIV_univariate_wt_ols_borda.tex
# Purpose: Likert + Borda panels; separate models only; race/gender only
# ------------------------------------------------------------------------------

table8_subdir <- "Full_Sample"

table8_runs <- list(
  ls_race = list(root = root_dir, lhs = "log_dif",        model_filter = "OLS"),
  ls_gender = list(root = root_dir, lhs = "log_dif_gender", model_filter = "OLS"),
  borda_race = list(root = root_dir, lhs = "log_dif",        model_filter = "Borda"),
  borda_gender = list(root = root_dir, lhs = "log_dif_gender", model_filter = "Borda")
)

build_table8_row <- function(cfg) {
  root <- cfg$root
  lhs  <- cfg$lhs
  model_filter <- cfg$model_filter

  # Pull univariate rows from the unified EIV_firm sheet.
  uni_nofe_sel <- pull_eiv_firm_est(root, table8_subdir, lhs, "FirmSelective", 1L,
                                    formula_var = "FirmSelective",
                                    model_filter = model_filter)
  uni_nofe_dis <- pull_eiv_firm_est(root, table8_subdir, lhs, "discretion", 1L,
                                    formula_var = "discretion",
                                    model_filter = model_filter)
  uni_fe_sel <- pull_eiv_firm_est(root, table8_subdir, lhs, "FirmSelective", 2L,
                                  formula_var = "FirmSelective",
                                  model_filter = model_filter)
  uni_fe_dis <- pull_eiv_firm_est(root, table8_subdir, lhs, "discretion", 2L,
                                  formula_var = "discretion",
                                  model_filter = model_filter)

  tibble(
    Regressor = c("Selectivity", "Discretion"),
    `Separate Models` = c(uni_nofe_sel, uni_nofe_dis),
    `Separate Models (Industry FEs)` = c(uni_fe_sel, uni_fe_dis)
  )
}

df_ls_race <- build_table8_row(table8_runs$ls_race)
df_ls_gender <- build_table8_row(table8_runs$ls_gender)
df_ls <- bind_rows(df_ls_race, df_ls_gender)

df_borda_race <- build_table8_row(table8_runs$borda_race)
df_borda_gender <- build_table8_row(table8_runs$borda_gender)
df_borda <- bind_rows(df_borda_race, df_borda_gender)

combined_df <- bind_rows(df_ls, df_borda)

n_ls_race <- nrow(df_ls_race)
n_ls_gender <- nrow(df_ls_gender)
n_ls <- n_ls_race + n_ls_gender
n_borda_race <- nrow(df_borda_race)
n_borda_gender <- nrow(df_borda_gender)
n_borda <- n_borda_race + n_borda_gender

col_names <- colnames(combined_df)
col_names <- gsub("Separate Models \\(Industry FEs\\)",
                  "\\\\shortstack{Separate Models\\\\\\\\(Industry FEs)}",
                  col_names)

tex_code_table8 <- kable(
  combined_df,
  format    = "latex",
  booktabs  = TRUE,
  align     = c("l", rep("c", ncol(combined_df) - 1)),
  col.names = col_names,
  linesep   = "",
  escape    = FALSE
) %>%
  pack_rows("Panel A: Likert", 1, n_ls) %>%
  pack_rows("\\\\textit{Race}", 1, n_ls_race, italic = TRUE, escape = FALSE) %>%
  pack_rows("\\\\textit{Gender}", n_ls_race + 1, n_ls, italic = TRUE, escape = FALSE) %>%
  pack_rows("Panel B: Borda", n_ls + 1, n_ls + n_borda) %>%
  pack_rows("\\\\textit{Race}", n_ls + 1, n_ls + n_borda_race, italic = TRUE, escape = FALSE) %>%
  pack_rows("\\\\textit{Gender}", n_ls + n_borda_race + 1, n_ls + n_borda, italic = TRUE, escape = FALSE)

out_tex_table8 <- file.path(tables, "EIV_univariate_wt_ols_borda.tex")
write_lines_checked(tex_code_table8, out_tex_table8, label = "EIV LaTeX table")
message("✓ LaTeX Table 8 saved to: ", out_tex_table8)

# ------------------------------------------------------------------------------
# Build Table: EIV_univariate_wt_ols_borda_w_gender_sq.tex
# Purpose: Selectivity / Discretion univariate regressions, Likert and Borda
# side by side, one panel for Race and one panel for Gender, coefficient over
# standard error --- matching the regression-table layout used in
# eiv_eeo1_share_tables.R's write_selectivity_controls_table(). No squared
# regressors (kept the original output filename since it's what's referenced
# downstream; flag if you'd like it renamed to drop the now-inaccurate "sq").
# ------------------------------------------------------------------------------

reg8_n_cols <- 4L  # Likert (No FE, Industry FE), Borda (No FE, Industry FE)

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

# One regressor's row of 4 cells: Likert No-FE, Likert Industry-FE, Borda No-FE, Borda Industry-FE
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

reg8_panel_rows <- function(panel_label, lhs) {
  selectivity_est <- reg8_row_cells(lhs, "FirmSelective", "estimate")
  selectivity_se  <- reg8_row_cells(lhs, "FirmSelective", "se")
  discretion_est  <- reg8_row_cells(lhs, "discretion", "estimate")
  discretion_se   <- reg8_row_cells(lhs, "discretion", "se")

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
    reg8_value_row("Industry FE", c("", "X", "", "X"), identity)
  ))
)

latex_lines_sq <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Likert} & \\multicolumn{2}{c}{Borda} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
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
      paste0("\\multicolumn{5}{l}{\\textbf{", reg8_latex_escape(row_label), "}} \\\\")
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
