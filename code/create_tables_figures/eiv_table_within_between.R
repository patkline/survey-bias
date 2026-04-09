source("code/globals.R")

library(dplyr)
library(kableExtra)

`%||%` <- function(x, y) if (is.null(x)) y else x

fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair from EIV_within or EIV_between sheet (coef = 1 only)
pull_est_no_fe <- function(root, subdir, lhs_var, rhs_var,
                           sheet = "EIV_within", model_filter = NULL) {
  dir_path <- file.path(root, subdir)
  dat <- read_parquet_sheet(dir_path, sheet)
  if (!nrow(dat)) stop("Empty sheet '", sheet, "' in ", dir_path)

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

# Default sample -> intermediate subdirectory map
default_filemap <- tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination",
             "40 Years or Older",
             "Less than 40 Years Old"),
  subdir = c("Full_Sample",
             "Subset_Black",
             "Subset_White",
             "Subset_Female",
             "Subset_Male",
             "Subset_Looking",
             "Subset_Not_Looking",
             "Subset_Feared_Discrimination_1",
             "Subset_Feared_Discrimination_0",
             "Subset_Age_gte40",
             "Subset_Age_lt40")
)

# ---------- BUILD DF FOR ONE WITHIN/BETWEEN CONFIG ----------

build_eiv_df_no_fe <- function(cfg) {
  root         <- cfg$root
  sheet_name   <- cfg$sheet_name
  model_filter <- cfg$model_filter
  lhs          <- cfg$lhs
  rhs_contact  <- cfg$rhs_contact
  rhs_conduct  <- cfg$rhs_conduct
  rhs_extra    <- cfg$rhs_extra
  filemap      <- cfg$filemap %||% default_filemap

  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `(1) Contact` = pull_est_no_fe(root, subdir, lhs, rhs_contact, sheet_name, model_filter),
      `(2) Conduct` = pull_est_no_fe(root, subdir, lhs, rhs_conduct, sheet_name, model_filter),
      `(3) Pooled`  = pull_est_no_fe(root, subdir, lhs, rhs_extra,   sheet_name, model_filter)
    ) %>%
    ungroup() %>%
    select(-subdir)

  table_df
}

# ---------- TWO-PANEL LATEX TABLE BUILDER ----------

build_two_panel_table_no_fe <- function(cfg_left, cfg_right, out_tex,
                                        left_label = "Panel A: Likert",
                                        right_label = "Panel B: Borda") {
  df_left  <- build_eiv_df_no_fe(cfg_left)
  df_right <- build_eiv_df_no_fe(cfg_right)

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

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("Saved: ", out_tex)
}

# ---------- CONFIG HELPER ----------

make_wb_cfg <- function(root, model, lhs, rhs_contact, rhs_conduct, rhs_extra,
                        sheet_name) {
  list(
    root         = root,
    sheet_name   = sheet_name,
    model_filter = model,
    lhs          = lhs,
    rhs_contact  = rhs_contact,
    rhs_conduct  = rhs_conduct,
    rhs_extra    = rhs_extra
  )
}

root_dir <- intermediate

# ==========================================================================
# Within-industry tables (demeaned)
# ==========================================================================

# ---- Race within ----
build_two_panel_table_no_fe(
  cfg_left  = make_wb_cfg(root_dir, "OLS",   "log_dif_dm",
                           "FirmCont_favor_white_dm", "conduct_favor_white_dm", "pooled_favor_white_dm",
                           "EIV_within"),
  cfg_right = make_wb_cfg(root_dir, "Borda", "log_dif_dm",
                           "FirmCont_favor_white_dm", "conduct_favor_white_dm", "pooled_favor_white_dm",
                           "EIV_within"),
  out_tex   = file.path(tables, "EIV_race_within_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# ---- Gender within ----
build_two_panel_table_no_fe(
  cfg_left  = make_wb_cfg(root_dir, "OLS",   "log_dif_gender_dm",
                           "FirmCont_favor_male_dm", "conduct_favor_male_dm", "pooled_favor_male_dm",
                           "EIV_within"),
  cfg_right = make_wb_cfg(root_dir, "Borda", "log_dif_gender_dm",
                           "FirmCont_favor_male_dm", "conduct_favor_male_dm", "pooled_favor_male_dm",
                           "EIV_within"),
  out_tex   = file.path(tables, "EIV_gender_within_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# ==========================================================================
# Between-industry tables (industry means)
# ==========================================================================

# ---- Race between ----
build_two_panel_table_no_fe(
  cfg_left  = make_wb_cfg(root_dir, "OLS",   "log_dif_im",
                           "FirmCont_favor_white_im", "conduct_favor_white_im", "pooled_favor_white_im",
                           "EIV_between"),
  cfg_right = make_wb_cfg(root_dir, "Borda", "log_dif_im",
                           "FirmCont_favor_white_im", "conduct_favor_white_im", "pooled_favor_white_im",
                           "EIV_between"),
  out_tex   = file.path(tables, "EIV_race_between_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# ---- Gender between ----
build_two_panel_table_no_fe(
  cfg_left  = make_wb_cfg(root_dir, "OLS",   "log_dif_gender_im",
                           "FirmCont_favor_male_im", "conduct_favor_male_im", "pooled_favor_male_im",
                           "EIV_between"),
  cfg_right = make_wb_cfg(root_dir, "Borda", "log_dif_gender_im",
                           "FirmCont_favor_male_im", "conduct_favor_male_im", "pooled_favor_male_im",
                           "EIV_between"),
  out_tex   = file.path(tables, "EIV_gender_between_ols_borda.tex"),
  left_label  = "Panel A: Likert",
  right_label = "Panel B: Borda"
)
