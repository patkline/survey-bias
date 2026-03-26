# ------------------------------------------------------------------------------
# Purpose: Build EIV table for OLS + Borda with gender_sq columns added.
#          Reads from existing EIV_firm sheet in Full Sample xlsx.
#          Saves to: EIV_univariate_wt_ols_borda_w_gender_sq.tex
# ------------------------------------------------------------------------------
source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Pull estimates from EIV_firm sheet
pull_eiv_firm_est <- function(root, file, lhs_var, rhs_var, coef_num,
                              formula_var, model_filter = "OLS",
                              sheet = "EIV_firm", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble::tibble())
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

  if (isTRUE(divide_by_100)) { est <- est / 100; se <- se / 100 }
  paste0(fmt3(est), " (", fmt3(se), ")")
}

root_dir <- excel
full_file <- "Plackett_Luce_Full_Sample.xlsx"

# --- Build rows for one model/lhs combo ---
build_row <- function(lhs, model_filter) {
  uni_nofe_sel <- pull_eiv_firm_est(root_dir, full_file, lhs, "FirmSelective", 1L,
                                    formula_var = "FirmSelective", model_filter = model_filter)
  uni_nofe_dis <- pull_eiv_firm_est(root_dir, full_file, lhs, "discretion", 1L,
                                    formula_var = "discretion", model_filter = model_filter)
  uni_fe_sel   <- pull_eiv_firm_est(root_dir, full_file, lhs, "FirmSelective", 2L,
                                    formula_var = "FirmSelective", model_filter = model_filter)
  uni_fe_dis   <- pull_eiv_firm_est(root_dir, full_file, lhs, "discretion", 2L,
                                    formula_var = "discretion", model_filter = model_filter)
  tibble::tibble(
    Regressor = c("Selectivity", "Discretion"),
    sep       = c(uni_nofe_sel, uni_nofe_dis),
    sep_fe    = c(uni_fe_sel, uni_fe_dis)
  )
}

# --- Build each panel's sub-blocks ---
# Likert
ls_race      <- build_row("log_dif",           "OLS")
ls_gender    <- build_row("log_dif_gender",    "OLS")
ls_gender_sq <- build_row("log_dif_gender_sq", "OLS")

# Borda
b_race      <- build_row("log_dif",           "Borda")
b_gender    <- build_row("log_dif_gender",    "Borda")
b_gender_sq <- build_row("log_dif_gender_sq", "Borda")

# --- Assemble: race gets empty gender_sq columns ---
empty2 <- c("", "")

assemble_panel <- function(race, gender, gender_sq) {
  # Race rows: 4 existing cols + 2 empty gender_sq cols
  race_lines <- paste0(
    "    ", race$Regressor, " & ",
    race$sep, " & ", race$sep_fe, " & & \\\\"
  )
  # Gender rows: 4 existing cols + 2 gender_sq cols
  gender_lines <- paste0(
    "    ", gender$Regressor, " & ",
    gender$sep, " & ", gender$sep_fe, " & ",
    gender_sq$sep, " & ", gender_sq$sep_fe, " \\\\"
  )
  list(race = race_lines, gender = gender_lines)
}

ls_panel <- assemble_panel(ls_race, ls_gender, ls_gender_sq)
b_panel  <- assemble_panel(b_race, b_gender, b_gender_sq)

# --- Build LaTeX ---
latex_lines <- c(
  "  \\centering",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\shortstack{Separate\\\\Models} & \\shortstack{Separate Models\\\\(Industry FEs)} & \\shortstack{Separate Models\\\\(Squared)} & \\shortstack{Separate Models\\\\(Squared, Ind FEs)} \\\\",
  "    \\midrule",
  "    \\multicolumn{5}{l}{\\textbf{Panel A: Likert}}\\\\",
  "    \\multicolumn{5}{l}{\\textit{Race}}\\\\",
  ls_panel$race,
  "    \\multicolumn{5}{l}{\\textit{Gender}}\\\\",
  ls_panel$gender,
  "    \\addlinespace",
  "    \\multicolumn{5}{l}{\\textbf{Panel B: Borda}}\\\\",
  "    \\multicolumn{5}{l}{\\textit{Race}}\\\\",
  b_panel$race,
  "    \\multicolumn{5}{l}{\\textit{Gender}}\\\\",
  b_panel$gender,
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "EIV_univariate_wt_ols_borda_w_gender_sq.tex")
dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
writeLines(latex_lines, out_tex)
message("Saved: ", out_tex)
