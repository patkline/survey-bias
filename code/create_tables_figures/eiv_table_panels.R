source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair and (optionally) divide both by 100 for reporting
pull_est <- function(root, file, lhs_var, rhs_var, coef_num,
                     sheet = "EIV_BS", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble())
  if (!nrow(dat)) return("NA (NA)")
  
  out <- dat %>%
    mutate(coef = suppressWarnings(as.numeric(.data$coef))) %>%
    filter(.data$rhs == rhs_var,
           .data$lhs == lhs_var,
           .data$coef == coef_num)
  
  if (!nrow(out)) return("NA (NA)")
  
  est <- suppressWarnings(as.numeric(out$sample_est))
  se  <- suppressWarnings(as.numeric(out$sample_se))
  
  if (isTRUE(divide_by_100)) {
    est <- est / 100
    se  <- se  / 100
  }
  paste0(fmt3(est), " (", fmt3(se), ")")
}

# Default sample → file map (override per run if you need)
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

# ---------- BUILD DF FOR ONE CONFIG (NO LATEX YET) ----------

build_eiv_df <- function(cfg) {
  root        <- cfg$root
  sheet_name  <- cfg$sheet_name  %||% "EIV_BS"
  lhs         <- cfg$lhs
  rhs_contact <- cfg$rhs_contact
  rhs_conduct <- cfg$rhs_conduct
  rhs_extra   <- cfg$rhs_extra
  
  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap
  
  col5_label  <- cfg$col5_label  %||% paste0("(5) ", rhs_extra)
  col6_label  <- cfg$col6_label  %||% paste0("(6) ", rhs_extra, " (Industry FE)")
  
  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `(1) Contact`               = pull_est(root, file, lhs, rhs_contact, 1L, sheet_name, scale_by_100),
      `(2) Contact (Industry FE)` = pull_est(root, file, lhs, rhs_contact, 2L, sheet_name, scale_by_100),
      `(3) Conduct`               = pull_est(root, file, lhs, rhs_conduct, 1L, sheet_name, scale_by_100),
      `(4) Conduct (Industry FE)` = pull_est(root, file, lhs, rhs_conduct, 2L, sheet_name, scale_by_100),
      `__col5`                    = pull_est(root, file, lhs, rhs_extra,   1L, sheet_name, scale_by_100),
      `__col6`                    = pull_est(root, file, lhs, rhs_extra,   2L, sheet_name, scale_by_100)
    ) %>%
    ungroup() %>%
    select(-file) %>%
    rename(
      !!col5_label := `__col5`,
      !!col6_label := `__col6`
    )
  
  table_df
}

# ---------- BUILD A TWO-PANEL (PL + BORDA) LATEX TABULAR ----------

build_two_panel_eiv_table <- function(cfg_pl, cfg_borda, out_tex) {
  
  df_pl    <- build_eiv_df(cfg_pl)
  df_borda <- build_eiv_df(cfg_borda)
  
  # Force same column order/labels (use PL as reference)
  common_cols <- colnames(df_pl)
  df_pl    <- df_pl[, common_cols]
  df_borda <- df_borda[, common_cols]
  
  combined_df <- bind_rows(df_pl, df_borda)
  
  n_pl    <- nrow(df_pl)
  n_borda <- nrow(df_borda)
  
  tex_code <- kable(
    combined_df,
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", rep("c", ncol(combined_df) - 1)),
    col.names = common_cols,
    linesep   = "",
    escape    = FALSE
  ) %>%
    kable_styling(
      full_width = FALSE,
      position   = "center"
    ) %>%
    pack_rows("Panel A: Plackett--Luce", 1, n_pl) %>%
    pack_rows("Panel B: Borda", n_pl + 1, n_pl + n_borda)
  
  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX two-panel table saved to: ", out_tex)
}

# ---------- CONFIGS FOR ALL SIX RUNS ----------

root_dir <- excel

runs <- list(
  # 1. RACE – Plackett–Luce
  list(
    root        = root_dir,
    sheet_name  = "EIV_BS",
    lhs         = "log_dif",
    rhs_contact = "FirmCont_favor_white",
    rhs_conduct = "conduct_black",
    rhs_extra   = "pooled_favor_white",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  ),
  # 2. GENDER – Plackett–Luce
  list(
    root        = root_dir,
    sheet_name  = "EIV_BS",
    lhs         = "log_dif_gender",
    rhs_contact = "FirmCont_favor_male",
    rhs_conduct = "conduct_favor_male",
    rhs_extra   = "pooled_favor_male",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  ),
  # 3. AGE – Plackett–Luce
  list(
    root        = root_dir,
    sheet_name  = "EIV_BS",
    lhs         = "log_dif_age",
    rhs_contact = "FirmCont_favor_younger",
    rhs_conduct = "conduct_favor_younger",
    rhs_extra   = "pooled_favor_younger",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  ),
  # 4. RACE – Borda
  list(
    root        = root_dir,
    sheet_name  = "EIV_Bordaw",
    lhs         = "log_dif",
    rhs_contact = "FirmCont_favor_white",
    rhs_conduct = "conduct_black",
    rhs_extra   = "pooled_favor_white",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  ),
  # 5. GENDER – Borda
  list(
    root        = root_dir,
    sheet_name  = "EIV_Bordaw",
    lhs         = "log_dif_gender",
    rhs_contact = "FirmCont_favor_male",
    rhs_conduct = "conduct_favor_male",
    rhs_extra   = "pooled_favor_male",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  ),
  # 6. AGE – Borda
  list(
    root        = root_dir,
    sheet_name  = "EIV_Bordaw",
    lhs         = "log_dif_age",
    rhs_contact = "FirmCont_favor_younger",
    rhs_conduct = "conduct_favor_younger",
    rhs_extra   = "pooled_favor_younger",
    scale_by_100 = FALSE,
    col5_label  = "(5) Pooled",
    col6_label  = "(6) Pooled (Industry FE)"
  )
)

# ---------- BUILD THE THREE TWO-PANEL TABLES ----------

# Race: Panel A = Plackett–Luce (runs[[1]]), Panel B = Borda (runs[[4]])
build_two_panel_eiv_table(
  cfg_pl    = runs[[1]],
  cfg_borda = runs[[4]],
  out_tex   = file.path(tables, "EIV_race_two_panel.tex")
)

# Gender
build_two_panel_eiv_table(
  cfg_pl    = runs[[2]],
  cfg_borda = runs[[5]],
  out_tex   = file.path(tables, "EIV_gender_two_panel.tex")
)

# Age
build_two_panel_eiv_table(
  cfg_pl    = runs[[3]],
  cfg_borda = runs[[6]],
  out_tex   = file.path(tables, "EIV_age_two_panel.tex")
)
