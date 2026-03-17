source("code/globals.R")

# Location of the input Excel files used to build this table
root_dir <- excel

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair from the unified EIV sheet (new pipeline)
pull_est <- function(root, file, lhs_var, rhs_var, coef_num,
                     sheet = "EIV_firm", model_filter = NULL, divide_by_100 = FALSE) {

  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble::tibble())

  if (!nrow(dat)) return("NA (NA)")

  dat <- dat %>%
    dplyr::mutate(coef = suppressWarnings(as.numeric(.data$coef)))

  # Filter by model
  if (!is.null(model_filter) && "model" %in% names(dat)) {
    dat <- dat %>% dplyr::filter(.data$model == model_filter)
  }

  dat <- dat %>%
    dplyr::filter(
      .data$rhs == rhs_var,
      .data$lhs == lhs_var,
      .data$coef == coef_num
    )

  if (!nrow(dat)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(dat$sample_est))
  se  <- suppressWarnings(as.numeric(dat$sample_se))

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

# ---------- BUILD DF FOR ONE PANEL (TWO COLUMNS) ----------

build_eiv_df_two_cols <- function(cfg) {
  root         <- cfg$root
  sheet_name   <- cfg$sheet_name   %||% "EIV_firm"
  model_filter <- cfg$model_filter
  lhs          <- cfg$lhs
  rhs          <- cfg$rhs
  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap
  if (is.null(cfg$coef1) || is.null(cfg$coef2)) {
    stop("cfg must specify coef1 and coef2.")
  }
  coef1 <- cfg$coef1
  coef2 <- cfg$coef2

  col1_label <- cfg$col1_label %||% "(1) Discretion"
  col2_label <- cfg$col2_label %||% "(2) Discretion (Industry FE)"

  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      !!col1_label := pull_est(root, file, lhs, rhs, coef1, sheet = sheet_name, model_filter = model_filter, divide_by_100 = scale_by_100),
      !!col2_label := pull_est(root, file, lhs, rhs, coef2, sheet = sheet_name, model_filter = model_filter, divide_by_100 = scale_by_100)
    ) %>%
    ungroup() %>%
    select(-file)

  table_df
}

# ---------- BUILD A FOUR-PANEL LATEX TABLE ----------

build_four_panel_eiv_table_two_cols <- function(cfg_pl, cfg_borda, cfg_ols, cfg_olsc, out_tex, cfg_ol = NULL) {

  df_pl    <- build_eiv_df_two_cols(cfg_pl)
  df_borda <- build_eiv_df_two_cols(cfg_borda)
  df_ols   <- build_eiv_df_two_cols(cfg_ols)
  df_olsc  <- build_eiv_df_two_cols(cfg_olsc)
  df_ol    <- if (!is.null(cfg_ol)) build_eiv_df_two_cols(cfg_ol) else NULL

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
      kable_styling(full_width = FALSE, position = "center") %>%
      pack_rows("Panel A: Plackett--Luce", 1, cum_pl) %>%
      pack_rows("Panel B: Borda", cum_pl + 1, cum_borda) %>%
      pack_rows("Panel C: Ordered Logit", cum_borda + 1, cum_ol) %>%
      pack_rows("Panel D: OLS", cum_ol + 1, cum_ols) %>%
      pack_rows("Panel E: OLS Centered", cum_ols + 1, cum_olsc)
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
      kable_styling(full_width = FALSE, position = "center") %>%
      pack_rows("Panel A: Plackett--Luce", 1, cum_pl) %>%
      pack_rows("Panel B: Borda", cum_pl + 1, cum_borda) %>%
      pack_rows("Panel C: OLS", cum_borda + 1, cum_ols) %>%
      pack_rows("Panel D: OLS Centered", cum_ols + 1, cum_olsc)
  }

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX table saved to: ", out_tex)
}

# ---------- BUILD A TWO-PANEL LATEX TABLE ----------

build_two_panel_eiv_table_two_cols <- function(cfg_left, cfg_right, out_tex,
                                               left_label = "Panel A: Plackett--Luce",
                                               right_label = "Panel B: Borda") {
  df_left  <- build_eiv_df_two_cols(cfg_left)
  df_right <- build_eiv_df_two_cols(cfg_right)

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
    kable_styling(full_width = FALSE, position = "center") %>%
    pack_rows(left_label, 1, n_left) %>%
    pack_rows(right_label, n_left + 1, n_left + n_right)

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX table saved to: ", out_tex)
}

# ---------- CONFIGS ----------

make_disc_cfg <- function(root, model, coef1 = 1L, coef2 = 2L) {
  list(
    root         = root,
    sheet_name   = "EIV_firm",
    model_filter = model,
    lhs          = "cb_central_full",
    rhs          = "discretion",
    scale_by_100 = FALSE,
    col1_label   = "(1) Discretion",
    col2_label   = "(2) Discretion (Industry FE)",
    coef1        = coef1,
    coef2        = coef2
  )
}

# ---------- BUILD TABLES ----------

# Unweighted (coef 1/2)
build_four_panel_eiv_table_two_cols(
  cfg_pl    = make_disc_cfg(root_dir, "PL"),
  cfg_borda = make_disc_cfg(root_dir, "Borda"),
  cfg_ols   = make_disc_cfg(root_dir, "OLS"),
  cfg_olsc  = make_disc_cfg(root_dir, "OLSC"),
  cfg_ol    = make_disc_cfg(root_dir, "OL"),
  out_tex   = file.path(tables, "EIV_discretion_four_panel.tex")
)

# Weighted (same coefs in new pipeline since weights are baked in)
build_four_panel_eiv_table_two_cols(
  cfg_pl    = make_disc_cfg(root_dir, "PL"),
  cfg_borda = make_disc_cfg(root_dir, "Borda"),
  cfg_ols   = make_disc_cfg(root_dir, "OLS"),
  cfg_olsc  = make_disc_cfg(root_dir, "OLSC"),
  cfg_ol    = make_disc_cfg(root_dir, "OL"),
  out_tex   = file.path(tables, "EIV_discretion_four_panel_wt.tex")
)

# Weighted two-panel legacy output name: PL + Borda
build_two_panel_eiv_table_two_cols(
  cfg_left   = make_disc_cfg(root_dir, "PL"),
  cfg_right  = make_disc_cfg(root_dir, "Borda"),
  out_tex    = file.path(tables, "EIV_discretion_two_panel_wt.tex"),
  left_label = "Panel A: Plackett--Luce",
  right_label = "Panel B: Borda"
)

# Weighted two-panel OLS + Borda version
build_two_panel_eiv_table_two_cols(
  cfg_left   = make_disc_cfg(root_dir, "OLS"),
  cfg_right  = make_disc_cfg(root_dir, "Borda"),
  out_tex    = file.path(tables, "EIV_discretion_two_panel_wt_ols_borda.tex"),
  left_label = "Panel A: OLS",
  right_label = "Panel B: Borda"
)
