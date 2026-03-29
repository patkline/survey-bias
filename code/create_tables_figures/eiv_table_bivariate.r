source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Pull estimates from unified EIV_firm sheet with model + formula filters
pull_eiv_firm_est <- function(root, file, lhs_var, rhs_var, coef_num,
                              formula_var, model_filter = "OLS",
                              sheet = "EIV_firm", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
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

root_dir <- excel

# ------------------------------------------------------------------------------
# Build Table: EIV_univariate_wt_ols_borda.tex
# Purpose: Likert + Borda panels; separate models only; race/gender only
# ------------------------------------------------------------------------------

table8_file <- "Plackett_Luce_Full_Sample.xlsx"

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
  uni_nofe_sel <- pull_eiv_firm_est(root, table8_file, lhs, "FirmSelective", 1L,
                                    formula_var = "FirmSelective",
                                    model_filter = model_filter)
  uni_nofe_dis <- pull_eiv_firm_est(root, table8_file, lhs, "discretion", 1L,
                                    formula_var = "discretion",
                                    model_filter = model_filter)
  uni_fe_sel <- pull_eiv_firm_est(root, table8_file, lhs, "FirmSelective", 2L,
                                  formula_var = "FirmSelective",
                                  model_filter = model_filter)
  uni_fe_dis <- pull_eiv_firm_est(root, table8_file, lhs, "discretion", 2L,
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
dir.create(dirname(out_tex_table8), showWarnings = FALSE, recursive = TRUE)
writeLines(tex_code_table8, out_tex_table8)
message("✓ LaTeX Table 8 saved to: ", out_tex_table8)

# ------------------------------------------------------------------------------
# Build extended Table 8 with race_sq and gender_sq columns:
#   EIV_univariate_wt_ols_borda_w_gender_sq.tex
# Adds Squared / Squared+FE columns for both race and gender rows
# ------------------------------------------------------------------------------

# Build squared-spec rows
build_sq_row <- function(lhs, model_filter) {
  build_table8_row(list(root = root_dir, lhs = lhs, model_filter = model_filter))
}

df_ls_race_sq      <- build_sq_row("log_dif_sq",        "OLS")
df_ls_gender_sq    <- build_sq_row("log_dif_gender_sq", "OLS")
df_borda_race_sq   <- build_sq_row("log_dif_sq",        "Borda")
df_borda_gender_sq <- build_sq_row("log_dif_gender_sq", "Borda")

# Assemble panel: each row gets its own squared columns
assemble_sq_panel <- function(race, gender, race_sq, gender_sq) {
  race_lines <- paste0(
    "    ", race$Regressor, " & ",
    race$`Separate Models`, " & ", race$`Separate Models (Industry FEs)`, " & ",
    race_sq$`Separate Models`, " & ", race_sq$`Separate Models (Industry FEs)`, " \\\\"
  )
  gender_lines <- paste0(
    "    ", gender$Regressor, " & ",
    gender$`Separate Models`, " & ", gender$`Separate Models (Industry FEs)`, " & ",
    gender_sq$`Separate Models`, " & ", gender_sq$`Separate Models (Industry FEs)`, " \\\\"
  )
  list(race = race_lines, gender = gender_lines)
}

ls_sq_panel    <- assemble_sq_panel(df_ls_race, df_ls_gender, df_ls_race_sq, df_ls_gender_sq)
borda_sq_panel <- assemble_sq_panel(df_borda_race, df_borda_gender, df_borda_race_sq, df_borda_gender_sq)

latex_lines_sq <- c(
  "  \\centering",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\shortstack{Separate\\\\Models} & \\shortstack{Separate Models\\\\(Industry FEs)} & \\shortstack{Separate Models\\\\(Squared)} & \\shortstack{Separate Models\\\\(Squared, Ind FEs)} \\\\",
  "    \\midrule",
  "    \\multicolumn{5}{l}{\\textbf{Panel A: Likert}}\\\\",
  "    \\multicolumn{5}{l}{\\textit{Race}}\\\\",
  ls_sq_panel$race,
  "    \\multicolumn{5}{l}{\\textit{Gender}}\\\\",
  ls_sq_panel$gender,
  "    \\addlinespace",
  "    \\multicolumn{5}{l}{\\textbf{Panel B: Borda}}\\\\",
  "    \\multicolumn{5}{l}{\\textit{Race}}\\\\",
  borda_sq_panel$race,
  "    \\multicolumn{5}{l}{\\textit{Gender}}\\\\",
  borda_sq_panel$gender,
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex_sq <- file.path(tables, "EIV_univariate_wt_ols_borda_w_gender_sq.tex")
dir.create(dirname(out_tex_sq), showWarnings = FALSE, recursive = TRUE)
writeLines(latex_lines_sq, out_tex_sq)
message("✓ LaTeX squared-specs table saved to: ", out_tex_sq)
