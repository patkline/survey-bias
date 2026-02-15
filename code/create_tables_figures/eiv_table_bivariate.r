source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair for bivariate models
# For bivariate: coef 1 = selectivity, coef 3 = discretion
pull_bivariate_est <- function(root, file, lhs_var, coef_num,
                               sheet = "EIV_BIVARIATE", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble())
  if (!nrow(dat)) return("NA (NA)")

  # Convert coef column to numeric
  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  # Filter using base R to avoid data masking issues in rowwise context
  # Note: rhs is the same for all rows ("FirmSelective + discretion"), so we ignore it
  out <- dat[dat$lhs == lhs_var & dat$coef == coef_num, ]

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

# ---------- BUILD DF FOR ONE CONFIG ----------
# Creates a table with rows = regressors, columns = subsets

build_bivariate_df <- function(cfg) {
  root        <- cfg$root
  sheet_name  <- cfg$sheet_name  %||% "EIV_BIVARIATE"
  lhs         <- cfg$lhs
  coef1       <- cfg$coef1 %||% 1L  # Selectivity
  coef3       <- cfg$coef3 %||% 3L  # Discretion

  scale_by_100 <- cfg$scale_by_100 %||% FALSE
  filemap      <- cfg$filemap %||% default_filemap

  # Build two rows: one for selectivity, one for discretion
  # Each row has values for each subset in columns

  # Get selectivity estimates for all subsets
  selectivity_row <- filemap %>%
    rowwise() %>%
    mutate(
      value = pull_bivariate_est(root, file, lhs, coef1, sheet_name, scale_by_100)
    ) %>%
    ungroup()

  # Get discretion estimates for all subsets
  discretion_row <- filemap %>%
    rowwise() %>%
    mutate(
      value = pull_bivariate_est(root, file, lhs, coef3, sheet_name, scale_by_100)
    ) %>%
    ungroup()

  # Pivot to wide format with subsets as columns
  selectivity_wide <- selectivity_row %>%
    select(Sample, value) %>%
    pivot_wider(names_from = Sample, values_from = value) %>%
    mutate(Regressor = "Selectivity", .before = 1)

  discretion_wide <- discretion_row %>%
    select(Sample, value) %>%
    pivot_wider(names_from = Sample, values_from = value) %>%
    mutate(Regressor = "Discretion", .before = 1)

  # Combine rows
  table_df <- bind_rows(selectivity_wide, discretion_wide)

  table_df
}

# ---------- BUILD A TWO-PANEL LATEX TABLE (PL and Borda) ----------

build_two_panel_bivariate_table <- function(cfg_pl_race, cfg_pl_gender, cfg_pl_age,
                                             cfg_borda_race, cfg_borda_gender, cfg_borda_age,
                                             out_tex) {

  # Build PL dataframes
  df_pl_race   <- build_bivariate_df(cfg_pl_race)
  df_pl_gender <- build_bivariate_df(cfg_pl_gender)
  df_pl_age    <- build_bivariate_df(cfg_pl_age)

  # Build Borda dataframes
  df_borda_race   <- build_bivariate_df(cfg_borda_race)
  df_borda_gender <- build_bivariate_df(cfg_borda_gender)
  df_borda_age    <- build_bivariate_df(cfg_borda_age)

  # Force same column order/labels (use PL race as reference)
  common_cols <- colnames(df_pl_race)
  df_pl_race      <- df_pl_race[, common_cols]
  df_pl_gender    <- df_pl_gender[, common_cols]
  df_pl_age       <- df_pl_age[, common_cols]
  df_borda_race   <- df_borda_race[, common_cols]
  df_borda_gender <- df_borda_gender[, common_cols]
  df_borda_age    <- df_borda_age[, common_cols]

  # Combine all panels
  combined_df <- bind_rows(
    df_pl_race, df_pl_gender, df_pl_age,
    df_borda_race, df_borda_gender, df_borda_age
  )

  n_pl_race      <- nrow(df_pl_race)
  n_pl_gender    <- nrow(df_pl_gender)
  n_pl_age       <- nrow(df_pl_age)
  n_borda_race   <- nrow(df_borda_race)
  n_borda_gender <- nrow(df_borda_gender)
  n_borda_age    <- nrow(df_borda_age)

  # Total rows for each main panel
  n_pl    <- n_pl_race + n_pl_gender + n_pl_age
  n_borda <- n_borda_race + n_borda_gender + n_borda_age

  # Add line breaks to longer column names for LaTeX using shortstack
  display_cols <- common_cols
  display_cols <- gsub("Not Looking for a Job", "\\\\shortstack{Not Looking\\\\\\\\for a Job}", display_cols)
  display_cols <- gsub("Looking for a Job", "\\\\shortstack{Looking for\\\\\\\\a Job}", display_cols)
  display_cols <- gsub("Feared Discrimination", "\\\\shortstack{Feared\\\\\\\\Discrimination}", display_cols)
  display_cols <- gsub("Did Not Fear Discrimination", "\\\\shortstack{Did Not Fear\\\\\\\\Discrimination}", display_cols)

  tex_code <- kable(
    combined_df,
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", rep("c", ncol(combined_df) - 1)),
    col.names = display_cols,
    linesep   = "",
    escape    = FALSE
  ) %>%
    pack_rows("Panel A: Plackett--Luce", 1, n_pl) %>%
    pack_rows("\\\\textit{Race}", 1, n_pl_race, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Gender}", n_pl_race + 1, n_pl_race + n_pl_gender, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Age}", n_pl_race + n_pl_gender + 1, n_pl, italic = TRUE, escape = FALSE) %>%
    pack_rows("Panel B: Borda", n_pl + 1, n_pl + n_borda) %>%
    pack_rows("\\\\textit{Race}", n_pl + 1, n_pl + n_borda_race, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Gender}", n_pl + n_borda_race + 1, n_pl + n_borda_race + n_borda_gender, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Age}", n_pl + n_borda_race + n_borda_gender + 1, n_pl + n_borda, italic = TRUE, escape = FALSE)

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX two-panel bivariate table (PL and Borda) saved to: ", out_tex)
}

# ---------- CONFIGS FOR PL AND BORDA RUNS ----------

root_dir <- excel

runs <- list(
  # Plackett-Luce
  pl_race = list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif",
    coef1       = 1L,  # Selectivity
    coef3       = 3L   # Discretion
  ),
  pl_gender = list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif_gender",
    coef1       = 1L,
    coef3       = 3L
  ),
  pl_age = list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif_age",
    coef1       = 1L,
    coef3       = 3L
  ),

  # Borda
  borda_race = list(
    root        = root_dir,
    sheet_name  = "EIV_BORDA_BIVARIATE",
    lhs         = "log_dif",
    coef1       = 1L,
    coef3       = 3L
  ),
  borda_gender = list(
    root        = root_dir,
    sheet_name  = "EIV_BORDA_BIVARIATE",
    lhs         = "log_dif_gender",
    coef1       = 1L,
    coef3       = 3L
  ),
  borda_age = list(
    root        = root_dir,
    sheet_name  = "EIV_BORDA_BIVARIATE",
    lhs         = "log_dif_age",
    coef1       = 1L,
    coef3       = 3L
  )
)

# ---------- UNIVARIATE FUNCTIONS ----------

# Pull univariate estimates from EIV_BS sheet
pull_univariate_est <- function(root, file, lhs_var, rhs_var, coef_num,
                                sheet = "EIV_BS", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble())
  if (!nrow(dat)) return("NA (NA)")

  # Convert coef column to numeric
  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  # Filter for specific lhs, rhs, and coef
  out <- dat[dat$lhs == lhs_var & dat$rhs == rhs_var & dat$coef == coef_num, ]

  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est))
  se  <- suppressWarnings(as.numeric(out$sample_se))

  if (isTRUE(divide_by_100)) {
    est <- est / 100
    se  <- se  / 100
  }
  paste0(fmt3(est), " (", fmt3(se), ")")
}

# ---------- BUILD COMPARISON TABLE (BIVARIATE VS UNIVARIATE) ----------

build_comparison_table <- function(cfg_race, cfg_gender, cfg_age, out_tex) {
  # This table has:
  # - 3 subpanels: Race, Gender, Age
  # - 4 columns: Bivariate No FE, Bivariate With FE, Univariate No FE, Univariate With FE
  # - Each cell shows Selectivity and Discretion stacked
  # - Only uses Full Sample

  root <- cfg_race$root
  full_sample_file <- "Plackett_Luce_Full_Sample.xlsx"

  build_panel <- function(cfg) {
    lhs <- cfg$lhs
    bivariate_sheet <- cfg$bivariate_sheet
    univariate_sheet <- cfg$univariate_sheet

    # Column 1: Bivariate No FE (coef1 = Selectivity, coef3 = Discretion)
    biv_nofe_sel <- pull_bivariate_est(root, full_sample_file, lhs, 1L, bivariate_sheet)
    biv_nofe_dis <- pull_bivariate_est(root, full_sample_file, lhs, 3L, bivariate_sheet)

    # Column 2: Bivariate With FE (coef2 = Selectivity, coef4 = Discretion)
    biv_fe_sel <- pull_bivariate_est(root, full_sample_file, lhs, 2L, bivariate_sheet)
    biv_fe_dis <- pull_bivariate_est(root, full_sample_file, lhs, 4L, bivariate_sheet)

    # Column 3: Univariate No FE (coef1)
    # NOTE: for univariate EIV_BS, weighted specs are coefs 3 (No FE) and 4 (Industry FEs)
    uni_nofe_sel <- pull_univariate_est(root, full_sample_file, lhs, "FirmSelective", 3L, univariate_sheet)
    uni_nofe_dis <- pull_univariate_est(root, full_sample_file, lhs, "discretion", 3L, univariate_sheet)

    # Column 4: Univariate With FE (coef2)
    uni_fe_sel <- pull_univariate_est(root, full_sample_file, lhs, "FirmSelective", 4L, univariate_sheet)
    uni_fe_dis <- pull_univariate_est(root, full_sample_file, lhs, "discretion", 4L, univariate_sheet)

    tibble(
      Regressor = c("Selectivity", "Discretion"),
      `Combined Model` = c(biv_nofe_sel, biv_nofe_dis),
      `Combined Model (Industry FEs)` = c(biv_fe_sel, biv_fe_dis),
      `Separate Models` = c(uni_nofe_sel, uni_nofe_dis),
      `Separate Models (Industry FEs)` = c(uni_fe_sel, uni_fe_dis)
    )
  }

  # Build three panels
  df_race   <- build_panel(cfg_race)
  df_gender <- build_panel(cfg_gender)
  df_age    <- build_panel(cfg_age)

  # Combine
  combined_df <- bind_rows(df_race, df_gender, df_age)

  n_race   <- nrow(df_race)
  n_gender <- nrow(df_gender)
  n_age    <- nrow(df_age)

  # Format column names with shortstack for longer names
  col_names <- colnames(combined_df)
  col_names <- gsub("Combined Model \\(Industry FEs\\)",
                    "\\\\shortstack{Combined Model\\\\\\\\(Industry FEs)}",
                    col_names)
  col_names <- gsub("Separate Models \\(Industry FEs\\)",
                    "\\\\shortstack{Separate Models\\\\\\\\(Industry FEs)}",
                    col_names)

  # Generate LaTeX table
  tex_code <- kable(
    combined_df,
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", rep("c", ncol(combined_df) - 1)),
    col.names = col_names,
    linesep   = "",
    escape    = FALSE
  ) %>%
    pack_rows("\\\\textit{Race}", 1, n_race, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Gender}", n_race + 1, n_race + n_gender, italic = TRUE, escape = FALSE) %>%
    pack_rows("\\\\textit{Age}", n_race + n_gender + 1, n_race + n_gender + n_age, italic = TRUE, escape = FALSE)

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX comparison table (Bivariate vs Univariate) saved to: ", out_tex)
}

# ---------- BUILD THE TWO-PANEL TABLE ----------

build_two_panel_bivariate_table(
  cfg_pl_race    = runs$pl_race,
  cfg_pl_gender  = runs$pl_gender,
  cfg_pl_age     = runs$pl_age,
  cfg_borda_race   = runs$borda_race,
  cfg_borda_gender = runs$borda_gender,
  cfg_borda_age    = runs$borda_age,
  out_tex        = file.path(tables, "EIV_bivariate_two_panel.tex")
)

# ---------- BUILD THE COMPARISON TABLE ----------

comparison_runs <- list(
  race = list(
    root = root_dir,
    lhs = "log_dif",
    bivariate_sheet = "EIV_BIVARIATE",
    univariate_sheet = "EIV_BS"
  ),
  gender = list(
    root = root_dir,
    lhs = "log_dif_gender",
    bivariate_sheet = "EIV_BIVARIATE",
    univariate_sheet = "EIV_BS"
  ),
  age = list(
    root = root_dir,
    lhs = "log_dif_age",
    bivariate_sheet = "EIV_BIVARIATE",
    univariate_sheet = "EIV_BS"
  )
)

build_comparison_table(
  cfg_race   = comparison_runs$race,
  cfg_gender = comparison_runs$gender,
  cfg_age    = comparison_runs$age,
  out_tex    = file.path(tables, "EIV_bivariate_vs_univariate_wt.tex")
)
