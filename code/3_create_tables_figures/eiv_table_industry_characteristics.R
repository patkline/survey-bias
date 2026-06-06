# ------------------------------------------------------------------------------
# Purpose: Build the firm-level industry-characteristic EIV LaTeX tables ---
# industry covariate on the LHS, beliefs on the RHS, no industry fixed effects.
# One table for the race covariates and one for the gender covariates, each with
# OLS and Borda model panels, mirroring the within/between EIV table format.
#
# Created: Nico Rotundo 2026-06-03
#
# Reads from: ${intermediate}/Full_Sample EIV_industry_characteristics parquet sheet
# Writes to:  ${tables}/EIV_industry_characteristics_race_ols_borda.tex
#             ${tables}/EIV_industry_characteristics_gender_ols_borda.tex
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Load packages
library(dplyr)
library(kableExtra)

# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Format a numeric to three decimal places
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair from the EIV_industry_characteristics sheet (coef == 1, no FE)
pull_est_no_fe <- function(root, subdir, lhs_var, rhs_var,
                           sheet = "EIV_industry_characteristics", model_filter = NULL) {
  dir_path <- file.path(root, subdir)
  dat <- read_parquet_sheet(dir_path, sheet)
  if (!nrow(dat)) stop("Empty sheet '", sheet, "' in ", dir_path, call. = FALSE)

  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  if (!is.null(model_filter) && "model" %in% names(dat)) {
    dat <- dat[dat$model == model_filter, ]
  }

  out <- dat[dat$rhs == rhs_var & dat$lhs == lhs_var & dat$coef == 1L, ]
  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est))
  se  <- suppressWarnings(as.numeric(out$sample_se))
  paste0(fmt3(est), " (", fmt3(se), ")")
}

# ------------------------------------------------------------------------------
# Industry covariates that form the table rows (display label -> sheet lhs name)
# ------------------------------------------------------------------------------
# Race covariates
race_covariates <- tibble(
  Covariate = c("\\% Black (all jobs)", "\\% Black (management)", "\\% Black (front-line)",
                "Black--Non-Black wage gap", "Non-Black wage level"),
  lhs = c("share_emp_black_all_jobs", "share_emp_black_mid_off_manager", "share_emp_black_front_line",
          "wage_gap_black_coef", "wage_level_black_coef")
)

# Gender covariates
gender_covariates <- tibble(
  Covariate = c("\\% Female (all jobs)", "\\% Female (management)", "\\% Female (front-line)",
                "Female--Male wage gap", "Male wage level"),
  lhs = c("share_emp_female_all_jobs", "share_emp_female_mid_off_manager", "share_emp_female_front_line",
          "wage_gap_sex_coef", "wage_level_sex_coef")
)

# ------------------------------------------------------------------------------
# Table builders
# ------------------------------------------------------------------------------
# Build one model's coefficient block (rows = covariates, sample fixed)
build_eiv_df_industry_characteristics <- function(cfg) {
  root          <- cfg$root
  subdir        <- cfg$subdir       %||% "Full_Sample"
  sheet_name    <- cfg$sheet_name   %||% "EIV_industry_characteristics"
  model_filter  <- cfg$model_filter
  rhs_contact   <- cfg$rhs_contact
  rhs_conduct   <- cfg$rhs_conduct
  rhs_extra     <- cfg$rhs_extra
  covariate_map <- cfg$covariate_map

  table_df <- covariate_map %>%
    rowwise() %>%
    mutate(
      `(1) Contact` = pull_est_no_fe(root, subdir, lhs, rhs_contact, sheet_name, model_filter),
      `(2) Conduct` = pull_est_no_fe(root, subdir, lhs, rhs_conduct, sheet_name, model_filter),
      `(3) Pooled`  = pull_est_no_fe(root, subdir, lhs, rhs_extra,   sheet_name, model_filter)
    ) %>%
    ungroup() %>%
    select(-lhs)

  table_df
}

# Stack two model panels into one LaTeX table
build_two_panel_industry_characteristics_table <- function(cfg_left, cfg_right, out_tex,
                                                           left_label = "Panel A: Likert",
                                                           right_label = "Panel B: Borda") {
  df_left  <- build_eiv_df_industry_characteristics(cfg_left)
  df_right <- build_eiv_df_industry_characteristics(cfg_right)

  common_cols <- colnames(df_left)
  df_left  <- df_left[, common_cols]
  df_right <- df_right[, common_cols]

  combined_df <- bind_rows(df_left, df_right)
  n_left  <- nrow(df_left)
  n_right <- nrow(df_right)

  tex_code <- kable(
    combined_df,
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", rep("c", ncol(combined_df) - 1)),
    col.names = common_cols,
    linesep   = "",
    escape    = FALSE
  ) %>%
    pack_rows(left_label, 1, n_left) %>%
    pack_rows(right_label, n_left + 1, n_left + n_right)

  write_lines_checked(tex_code, out_tex, label = "industry-characteristic EIV LaTeX table")
  message("🎃 Saved: ", out_tex)
}

# Assemble a per-model config
make_industry_characteristics_cfg <- function(root, model, covariate_map,
                                              rhs_contact, rhs_conduct, rhs_extra, sheet_name) {
  list(
    root          = root,
    sheet_name    = sheet_name,
    model_filter  = model,
    covariate_map = covariate_map,
    rhs_contact   = rhs_contact,
    rhs_conduct   = rhs_conduct,
    rhs_extra     = rhs_extra
  )
}

# ------------------------------------------------------------------------------
# Build tables
# ------------------------------------------------------------------------------
# Root directory holding the per-sample EIV sheets
root_dir <- intermediate

# Race table (OLS + Borda panels)
build_two_panel_industry_characteristics_table(
  cfg_left  = make_industry_characteristics_cfg(root_dir, "OLS",   race_covariates,
                                                "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white",
                                                "EIV_industry_characteristics"),
  cfg_right = make_industry_characteristics_cfg(root_dir, "Borda", race_covariates,
                                                "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white",
                                                "EIV_industry_characteristics"),
  out_tex   = file.path(tables, "EIV_industry_characteristics_race_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# Gender table (OLS + Borda panels)
build_two_panel_industry_characteristics_table(
  cfg_left  = make_industry_characteristics_cfg(root_dir, "OLS",   gender_covariates,
                                                "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male",
                                                "EIV_industry_characteristics"),
  cfg_right = make_industry_characteristics_cfg(root_dir, "Borda", gender_covariates,
                                                "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male",
                                                "EIV_industry_characteristics"),
  out_tex   = file.path(tables, "EIV_industry_characteristics_gender_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)
