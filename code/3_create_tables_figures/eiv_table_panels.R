source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair from the unified EIV sheet (new pipeline)
pull_est <- function(root, subdir, lhs_var, rhs_var, coef_num,
                     sheet = "EIV_firm", model_filter = NULL,
                     formula_filter = NULL, divide_by_100 = FALSE) {
  dir_path <- file.path(root, subdir)
  dat <- tryCatch(read_parquet_sheet(dir_path, sheet),
                  error = function(e) tibble())
  if (!nrow(dat)) return("NA (NA)")

  # Convert coef column to numeric
  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  # Filter by model
  if (!is.null(model_filter) && "model" %in% names(dat)) {
    dat <- dat[dat$model == model_filter, ]
  }

  # Filter by formula (needed to distinguish univariate from bivariate)
  if (!is.null(formula_filter) && "formula" %in% names(dat)) {
    dat <- dat[dat$formula == formula_filter, ]
  }

  # Filter using base R
  out <- dat[dat$rhs == rhs_var & dat$lhs == lhs_var & dat$coef == coef_num, ]

  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est))
  se  <- suppressWarnings(as.numeric(out$sample_se))

  if (isTRUE(divide_by_100)) {
    est <- est / 100
    se  <- se  / 100
  }
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

# ---------- BUILD DF FOR ONE UNIVARIATE CONFIG ----------

build_eiv_df <- function(cfg) {
  root               <- cfg$root
  sheet_name         <- cfg$sheet_name         %||% "EIV_firm"
  between_sheet_name <- cfg$between_sheet_name %||% "EIV_between"
  model_filter       <- cfg$model_filter
  lhs                <- cfg$lhs
  rhs_contact        <- cfg$rhs_contact
  rhs_conduct        <- cfg$rhs_conduct
  rhs_extra          <- cfg$rhs_extra

  # Between-industry counterparts default to base names + "_im"
  lhs_im         <- cfg$lhs_im         %||% paste0(lhs,         "_im")
  rhs_contact_im <- cfg$rhs_contact_im %||% paste0(rhs_contact, "_im")
  rhs_conduct_im <- cfg$rhs_conduct_im %||% paste0(rhs_conduct, "_im")
  rhs_extra_im   <- cfg$rhs_extra_im   %||% paste0(rhs_extra,   "_im")

  coef1        <- cfg$coef1
  coef2        <- cfg$coef2

  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap

  col7_label  <- cfg$col7_label  %||% paste0("(7) ", rhs_extra)
  col8_label  <- cfg$col8_label  %||% paste0("(8) ", rhs_extra, " (Industry FE)")
  col9_label  <- cfg$col9_label  %||% paste0("(9) ", rhs_extra, " (Between)")

  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `(1) Contact`               = pull_est(root, subdir, lhs,    rhs_contact,    coef1, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `(2) Contact (Industry FE)` = pull_est(root, subdir, lhs,    rhs_contact,    coef2, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `(3) Contact (Between)`     = pull_est(root, subdir, lhs_im, rhs_contact_im, 1L,    between_sheet_name, model_filter, divide_by_100 = scale_by_100),
      `(4) Conduct`               = pull_est(root, subdir, lhs,    rhs_conduct,    coef1, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `(5) Conduct (Industry FE)` = pull_est(root, subdir, lhs,    rhs_conduct,    coef2, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `(6) Conduct (Between)`     = pull_est(root, subdir, lhs_im, rhs_conduct_im, 1L,    between_sheet_name, model_filter, divide_by_100 = scale_by_100),
      `__col7`                    = pull_est(root, subdir, lhs,    rhs_extra,      coef1, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `__col8`                    = pull_est(root, subdir, lhs,    rhs_extra,      coef2, sheet_name,         model_filter, divide_by_100 = scale_by_100),
      `__col9`                    = pull_est(root, subdir, lhs_im, rhs_extra_im,   1L,    between_sheet_name, model_filter, divide_by_100 = scale_by_100)
    ) %>%
    ungroup() %>%
    select(-subdir) %>%
    rename(
      !!col7_label := `__col7`,
      !!col8_label := `__col8`,
      !!col9_label := `__col9`
    )

  table_df
}

# ---------- BUILD DF FOR ONE BIVARIATE CONFIG ----------

build_eiv_df_bivariate <- function(cfg) {
  root           <- cfg$root
  sheet_name     <- cfg$sheet_name     %||% "EIV_firm"
  model_filter   <- cfg$model_filter
  lhs            <- cfg$lhs
  rhs1           <- cfg$rhs1
  rhs2           <- cfg$rhs2
  coef1          <- cfg$coef1
  coef2          <- cfg$coef2
  formula_filter <- cfg$formula_filter %||% paste(c(rhs1, rhs2), collapse = " + ")

  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap

  col1_label <- cfg$col1_label %||% "(1) Selective"
  col2_label <- cfg$col2_label %||% "(2) Selective (Industry FE)"
  col3_label <- cfg$col3_label %||% "(3) Discretion"
  col4_label <- cfg$col4_label %||% "(4) Discretion (Industry FE)"

  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `__col1` = pull_est(root, subdir, lhs, rhs1, coef1, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col2` = pull_est(root, subdir, lhs, rhs1, coef2, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col3` = pull_est(root, subdir, lhs, rhs2, coef1, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col4` = pull_est(root, subdir, lhs, rhs2, coef2, sheet_name, model_filter, formula_filter, scale_by_100)
    ) %>%
    ungroup() %>%
    select(-subdir) %>%
    rename(
      !!col1_label := `__col1`,
      !!col2_label := `__col2`,
      !!col3_label := `__col3`,
      !!col4_label := `__col4`
    )

  table_df
}

# ---------- FOUR-PANEL LATEX TABLE BUILDER ----------

build_four_panel_eiv_table <- function(cfg_pl, cfg_borda, cfg_ols, cfg_olsc,
                                       out_tex, build_fn = build_eiv_df,
                                       cfg_ol = NULL) {

  df_pl    <- build_fn(cfg_pl)
  df_borda <- build_fn(cfg_borda)
  df_ols   <- build_fn(cfg_ols)
  df_olsc  <- build_fn(cfg_olsc)
  df_ol    <- if (!is.null(cfg_ol)) build_fn(cfg_ol) else NULL

  # Force same column order/labels (use PL as reference)
  common_cols <- colnames(df_pl)
  df_pl    <- df_pl[, common_cols]
  df_borda <- df_borda[, common_cols]
  df_ols   <- df_ols[, common_cols]
  df_olsc  <- df_olsc[, common_cols]

  if (!is.null(df_ol)) {
    df_ol <- df_ol[, common_cols]
    combined_df <- bind_rows(df_pl, df_borda, df_ol, df_ols, df_olsc)
  } else {
    combined_df <- bind_rows(df_pl, df_borda, df_ols, df_olsc)
  }

  n_pl    <- nrow(df_pl)
  n_borda <- nrow(df_borda)
  n_ols   <- nrow(df_ols)
  n_olsc  <- nrow(df_olsc)

  if (!is.null(df_ol)) {
    n_ol     <- nrow(df_ol)
    cum_pl    <- n_pl
    cum_borda <- cum_pl + n_borda
    cum_ol    <- cum_borda + n_ol
    cum_ols   <- cum_ol + n_ols
    cum_olsc  <- cum_ols + n_olsc

    tex_code <- kable(
      combined_df,
      format    = "latex",
      booktabs  = TRUE,
      align     = c("l", rep("c", ncol(combined_df) - 1)),
      col.names = common_cols,
      linesep   = "",
      escape    = FALSE
    ) %>%
      pack_rows("Panel A: Plackett--Luce", 1, cum_pl) %>%
      pack_rows("Panel B: Borda", cum_pl + 1, cum_borda) %>%
      pack_rows("Panel C: Ordered Logit", cum_borda + 1, cum_ol) %>%
      pack_rows("Panel D: Likert", cum_ol + 1, cum_ols) %>%
      pack_rows("Panel E: Likert Centered", cum_ols + 1, cum_olsc)
  } else {
    cum_pl    <- n_pl
    cum_borda <- cum_pl + n_borda
    cum_ols   <- cum_borda + n_ols
    cum_olsc  <- cum_ols + n_olsc

    tex_code <- kable(
      combined_df,
      format    = "latex",
      booktabs  = TRUE,
      align     = c("l", rep("c", ncol(combined_df) - 1)),
      col.names = common_cols,
      linesep   = "",
      escape    = FALSE
    ) %>%
      pack_rows("Panel A: Plackett--Luce", 1, cum_pl) %>%
      pack_rows("Panel B: Borda", cum_pl + 1, cum_borda) %>%
      pack_rows("Panel C: Likert", cum_borda + 1, cum_ols) %>%
      pack_rows("Panel D: Likert Centered", cum_ols + 1, cum_olsc)
  }

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX table saved to: ", out_tex)
}

# ---------- TWO-PANEL LATEX TABLE BUILDER ----------

build_two_panel_eiv_table <- function(cfg_left, cfg_right, out_tex,
                                      build_fn = build_eiv_df,
                                      left_label = "Panel A: Plackett--Luce",
                                      right_label = "Panel B: Borda") {
  df_left  <- build_fn(cfg_left)
  df_right <- build_fn(cfg_right)

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
  message("✓ LaTeX table saved to: ", out_tex)
}

# ---------- CONFIG HELPERS ----------

make_uni_cfg <- function(root, model, lhs, rhs_contact, rhs_conduct, rhs_extra,
                         coef1 = 1L, coef2 = 2L, scale_by_100 = FALSE) {
  list(
    root               = root,
    sheet_name         = "EIV_firm",
    between_sheet_name = "EIV_between",
    model_filter       = model,
    lhs                = lhs,
    rhs_contact        = rhs_contact,
    rhs_conduct        = rhs_conduct,
    rhs_extra          = rhs_extra,
    scale_by_100       = scale_by_100,
    col7_label         = "(7) Pooled",
    col8_label         = "(8) Pooled (Industry FE)",
    col9_label         = "(9) Pooled (Between)",
    coef1              = coef1,
    coef2              = coef2
  )
}

make_bi_cfg <- function(root, model, lhs, rhs1 = "FirmSelective", rhs2 = "discretion",
                        coef1 = 1L, coef2 = 2L) {
  list(
    root           = root,
    sheet_name     = "EIV_firm",
    model_filter   = model,
    lhs            = lhs,
    rhs1           = rhs1,
    rhs2           = rhs2,
    formula_filter = paste(c(rhs1, rhs2), collapse = " + "),
    col1_label     = "(1) Selective",
    col2_label     = "(2) Selective (Industry FE)",
    col3_label     = "(3) Discretion",
    col4_label     = "(4) Discretion (Industry FE)",
    coef1          = coef1,
    coef2          = coef2
  )
}

# ==========================================================================
# UNIVARIATE FOUR-PANEL TABLES
# ==========================================================================

root_dir <- intermediate

if (FALSE) {
  # ---- Race ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    out_tex   = file.path(tables, "EIV_race_four_panel.tex")
  )

  # ---- Gender ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    out_tex   = file.path(tables, "EIV_gender_four_panel.tex")
  )

  # ---- Age ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    out_tex   = file.path(tables, "EIV_age_four_panel.tex")
  )

  # ---- Race (weighted) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    out_tex   = file.path(tables, "EIV_race_four_panel_wt.tex")
  )

  # ---- Gender (weighted) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    out_tex   = file.path(tables, "EIV_gender_four_panel_wt.tex")
  )

  # ---- Age (weighted) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    cfg_ol    = make_uni_cfg(root_dir, "OL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
    out_tex   = file.path(tables, "EIV_age_four_panel_wt.tex")
  )

  # ---- Race (weighted): PL + Borda (legacy name) ----
  build_two_panel_eiv_table(
    cfg_left   = make_uni_cfg(root_dir, "PL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    cfg_right  = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    out_tex    = file.path(tables, "EIV_race_two_panel_wt.tex"),
    left_label = "Panel A: Plackett--Luce",
    right_label = "Panel B: Borda"
  )

  # ---- Gender (weighted): PL + Borda (legacy name) ----
  build_two_panel_eiv_table(
    cfg_left   = make_uni_cfg(root_dir, "PL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    cfg_right  = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
    out_tex    = file.path(tables, "EIV_gender_two_panel_wt.tex"),
    left_label = "Panel A: Plackett--Luce",
    right_label = "Panel B: Borda"
  )
}

# ---- Race (weighted): OLS + Borda ----
build_two_panel_eiv_table(
  cfg_left   = make_uni_cfg(root_dir, "OLS",   "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_right  = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  out_tex    = file.path(tables, "EIV_race_two_panel_wt_ols_borda.tex"),
  left_label = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# ---- Gender (weighted): OLS + Borda ----
build_two_panel_eiv_table(
  cfg_left   = make_uni_cfg(root_dir, "OLS",   "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_right  = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  out_tex    = file.path(tables, "EIV_gender_two_panel_wt_ols_borda.tex"),
  left_label = "Panel A: Likert",
  right_label = "Panel B: Borda"
)

# ==========================================================================
# BIVARIATE FOUR-PANEL TABLES
# ==========================================================================

if (FALSE) {
  # ---- Race (bivariate) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif"),
    cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif"),
    cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif"),
    cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif"),
    cfg_ol    = make_bi_cfg(root_dir, "OL",    "log_dif"),
    out_tex   = file.path(tables, "EIV_race_bivariate_four_panel.tex"),
    build_fn  = build_eiv_df_bivariate
  )

  # ---- Gender (bivariate) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif_gender"),
    cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif_gender"),
    cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif_gender"),
    cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif_gender"),
    cfg_ol    = make_bi_cfg(root_dir, "OL",    "log_dif_gender"),
    out_tex   = file.path(tables, "EIV_gender_bivariate_four_panel.tex"),
    build_fn  = build_eiv_df_bivariate
  )

  # ---- Age (bivariate) ----
  build_four_panel_eiv_table(
    cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif_age"),
    cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif_age"),
    cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif_age"),
    cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif_age"),
    cfg_ol    = make_bi_cfg(root_dir, "OL",    "log_dif_age"),
    out_tex   = file.path(tables, "EIV_age_bivariate_four_panel.tex"),
    build_fn  = build_eiv_df_bivariate
  )
}
