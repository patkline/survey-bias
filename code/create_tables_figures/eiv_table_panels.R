source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair from the unified EIV sheet (new pipeline)
pull_est <- function(root, file, lhs_var, rhs_var, coef_num,
                     sheet = "EIV", model_filter = NULL,
                     formula_filter = NULL, divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
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

# Default sample -> file map
default_filemap <- tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination"),
  file  = c("Plackett_Luce_Full_Sample.xlsx",
            "Plackett_Luce_Subset_Black.xlsx",
            "Plackett_Luce_Subset_White.xlsx",
            "Plackett_Luce_Subset_Female.xlsx",
            "Plackett_Luce_Subset_Male.xlsx",
            "Plackett_Luce_Subset_Looking.xlsx",
            "Plackett_Luce_Subset_Not_Looking.xlsx",
            "Plackett_Luce_Subset_Feared_Discrimination_1.xlsx",
            "Plackett_Luce_Subset_Feared_Discrimination_0.xlsx")
)

# ---------- BUILD DF FOR ONE UNIVARIATE CONFIG ----------

build_eiv_df <- function(cfg) {
  root         <- cfg$root
  sheet_name   <- cfg$sheet_name   %||% "EIV"
  model_filter <- cfg$model_filter
  lhs          <- cfg$lhs
  rhs_contact  <- cfg$rhs_contact
  rhs_conduct  <- cfg$rhs_conduct
  rhs_extra    <- cfg$rhs_extra
  coef1        <- cfg$coef1
  coef2        <- cfg$coef2

  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap

  col5_label  <- cfg$col5_label  %||% paste0("(5) ", rhs_extra)
  col6_label  <- cfg$col6_label  %||% paste0("(6) ", rhs_extra, " (Industry FE)")

  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `(1) Contact`               = pull_est(root, file, lhs, rhs_contact, coef1, sheet_name, model_filter, divide_by_100 = scale_by_100),
      `(2) Contact (Industry FE)` = pull_est(root, file, lhs, rhs_contact, coef2, sheet_name, model_filter, divide_by_100 = scale_by_100),
      `(3) Conduct`               = pull_est(root, file, lhs, rhs_conduct, coef1, sheet_name, model_filter, divide_by_100 = scale_by_100),
      `(4) Conduct (Industry FE)` = pull_est(root, file, lhs, rhs_conduct, coef2, sheet_name, model_filter, divide_by_100 = scale_by_100),
      `__col5`                    = pull_est(root, file, lhs, rhs_extra,   coef1, sheet_name, model_filter, divide_by_100 = scale_by_100),
      `__col6`                    = pull_est(root, file, lhs, rhs_extra,   coef2, sheet_name, model_filter, divide_by_100 = scale_by_100)
    ) %>%
    ungroup() %>%
    select(-file) %>%
    rename(
      !!col5_label := `__col5`,
      !!col6_label := `__col6`
    )

  table_df
}

# ---------- BUILD DF FOR ONE BIVARIATE CONFIG ----------

build_eiv_df_bivariate <- function(cfg) {
  root           <- cfg$root
  sheet_name     <- cfg$sheet_name     %||% "EIV"
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
      `__col1` = pull_est(root, file, lhs, rhs1, coef1, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col2` = pull_est(root, file, lhs, rhs1, coef2, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col3` = pull_est(root, file, lhs, rhs2, coef1, sheet_name, model_filter, formula_filter, scale_by_100),
      `__col4` = pull_est(root, file, lhs, rhs2, coef2, sheet_name, model_filter, formula_filter, scale_by_100)
    ) %>%
    ungroup() %>%
    select(-file) %>%
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
                                       out_tex, build_fn = build_eiv_df) {

  df_pl    <- build_fn(cfg_pl)
  df_borda <- build_fn(cfg_borda)
  df_ols   <- build_fn(cfg_ols)
  df_olsc  <- build_fn(cfg_olsc)

  # Force same column order/labels (use PL as reference)
  common_cols <- colnames(df_pl)
  df_pl    <- df_pl[, common_cols]
  df_borda <- df_borda[, common_cols]
  df_ols   <- df_ols[, common_cols]
  df_olsc  <- df_olsc[, common_cols]

  combined_df <- bind_rows(df_pl, df_borda, df_ols, df_olsc)

  n_pl    <- nrow(df_pl)
  n_borda <- nrow(df_borda)
  n_ols   <- nrow(df_ols)
  n_olsc  <- nrow(df_olsc)

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
    pack_rows("Panel C: OLS", cum_borda + 1, cum_ols) %>%
    pack_rows("Panel D: OLS Centered", cum_ols + 1, cum_olsc)

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX four-panel table saved to: ", out_tex)
}

# ---------- CONFIG HELPERS ----------

make_uni_cfg <- function(root, model, lhs, rhs_contact, rhs_conduct, rhs_extra,
                         coef1 = 1L, coef2 = 2L, scale_by_100 = FALSE) {
  list(
    root         = root,
    sheet_name   = "EIV",
    model_filter = model,
    lhs          = lhs,
    rhs_contact  = rhs_contact,
    rhs_conduct  = rhs_conduct,
    rhs_extra    = rhs_extra,
    scale_by_100 = scale_by_100,
    col5_label   = "(5) Pooled",
    col6_label   = "(6) Pooled (Industry FE)",
    coef1        = coef1,
    coef2        = coef2
  )
}

make_bi_cfg <- function(root, model, lhs, rhs1 = "FirmSelective", rhs2 = "discretion",
                        coef1 = 1L, coef2 = 2L) {
  list(
    root           = root,
    sheet_name     = "EIV",
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

root_dir <- excel

# ---- Race ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  out_tex   = file.path(tables, "EIV_race_four_panel.tex")
)

# ---- Gender ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  out_tex   = file.path(tables, "EIV_gender_four_panel.tex")
)

# ---- Age ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  out_tex   = file.path(tables, "EIV_age_four_panel.tex")
)

# ==========================================================================
# UNIVARIATE FOUR-PANEL TABLES (WEIGHTED)
# New pipeline bakes in weights; coef 1/2 are already weighted.
# ==========================================================================

# ---- Race (weighted) ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif", "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
  out_tex   = file.path(tables, "EIV_race_four_panel_wt.tex")
)

# ---- Gender (weighted) ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_gender", "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
  out_tex   = file.path(tables, "EIV_gender_four_panel_wt.tex")
)

# ---- Age (weighted) ----
build_four_panel_eiv_table(
  cfg_pl    = make_uni_cfg(root_dir, "PL",    "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_borda = make_uni_cfg(root_dir, "Borda", "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_ols   = make_uni_cfg(root_dir, "OLS",   "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  cfg_olsc  = make_uni_cfg(root_dir, "OLSC",  "log_dif_age", "FirmCont_favor_younger", "conduct_favor_younger", "pooled_favor_younger"),
  out_tex   = file.path(tables, "EIV_age_four_panel_wt.tex")
)

# ==========================================================================
# BIVARIATE FOUR-PANEL TABLES
# ==========================================================================

# ---- Race (bivariate) ----
build_four_panel_eiv_table(
  cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif"),
  cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif"),
  cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif"),
  cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif"),
  out_tex   = file.path(tables, "EIV_race_bivariate_four_panel.tex"),
  build_fn  = build_eiv_df_bivariate
)

# ---- Gender (bivariate) ----
build_four_panel_eiv_table(
  cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif_gender"),
  cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif_gender"),
  cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif_gender"),
  cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif_gender"),
  out_tex   = file.path(tables, "EIV_gender_bivariate_four_panel.tex"),
  build_fn  = build_eiv_df_bivariate
)

# ---- Age (bivariate) ----
build_four_panel_eiv_table(
  cfg_pl    = make_bi_cfg(root_dir, "PL",    "log_dif_age"),
  cfg_borda = make_bi_cfg(root_dir, "Borda", "log_dif_age"),
  cfg_ols   = make_bi_cfg(root_dir, "OLS",   "log_dif_age"),
  cfg_olsc  = make_bi_cfg(root_dir, "OLSC",  "log_dif_age"),
  out_tex   = file.path(tables, "EIV_age_bivariate_four_panel.tex"),
  build_fn  = build_eiv_df_bivariate
)
