library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(kableExtra)
library(knitr)

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

# Default sample → file map (same as yours)
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

# ---------- BUILD DF FOR ONE PANEL (NO LATEX YET) ----------
# Two columns: (1) no FE, (2) Industry FE
build_eiv_df_two_cols <- function(cfg) {
  root         <- cfg$root
  sheet_name   <- cfg$sheet_name  %||% "EIV_BS"
  lhs          <- cfg$lhs
  rhs          <- cfg$rhs
  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap
  
  col1_label <- cfg$col1_label %||% "(1) Discretion"
  col2_label <- cfg$col2_label %||% "(2) Discretion (Industry FE)"
  
  table_df <- filemap %>%
    rowwise() %>%
    mutate(
      `__col1` = pull_est(root, file, lhs, rhs, 1L, sheet = sheet_name, divide_by_100 = scale_by_100),
      `__col2` = pull_est(root, file, lhs, rhs, 2L, sheet = sheet_name, divide_by_100 = scale_by_100)
    ) %>%
    ungroup() %>%
    select(-file) %>%
    rename(
      !!col1_label := `__col1`,
      !!col2_label := `__col2`
    )
  
  table_df
}

# ---------- BUILD A TWO-PANEL (PL + BORDA) LATEX TABLE ----------
build_two_panel_eiv_table_two_cols <- function(cfg_pl, cfg_borda, out_tex) {
  
  df_pl    <- build_eiv_df_two_cols(cfg_pl)
  df_borda <- build_eiv_df_two_cols(cfg_borda)
  
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

# ---------- CONFIGS ----------

cfg_pl <- list(
  root        = root_dir,
  sheet_name  = "EIV_BS",          # PL
  lhs         = "cb_central_full",
  rhs         = "discretion",
  scale_by_100 = FALSE,
  col1_label  = "(1) Discretion",
  col2_label  = "(2) Discretion (Industry FE)"
)

cfg_borda <- list(
  root        = root_dir,
  sheet_name  = "EIV_Bordaw",      # Borda
  lhs         = "cb_central_full",
  rhs         = "discretion",
  scale_by_100 = FALSE,
  col1_label  = "(1) Discretion",
  col2_label  = "(2) Discretion (Industry FE)"
)

# ---------- BUILD THE TABLE ----------
build_two_panel_eiv_table_two_cols(
  cfg_pl    = cfg_pl,
  cfg_borda = cfg_borda,
  out_tex   = file.path(tables, "EIV_discretion_two_panel.tex")
)
