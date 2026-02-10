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

# ---------- BUILD A THREE-PANEL LATEX TABLE ----------

build_three_panel_bivariate_table <- function(cfg_race, cfg_gender, cfg_age, out_tex) {

  df_race   <- build_bivariate_df(cfg_race)
  df_gender <- build_bivariate_df(cfg_gender)
  df_age    <- build_bivariate_df(cfg_age)

  # Force same column order/labels (use race as reference)
  common_cols <- colnames(df_race)
  df_race   <- df_race[, common_cols]
  df_gender <- df_gender[, common_cols]
  df_age    <- df_age[, common_cols]

  # Combine all three panels
  combined_df <- bind_rows(df_race, df_gender, df_age)

  n_race   <- nrow(df_race)
  n_gender <- nrow(df_gender)
  n_age    <- nrow(df_age)

  # Add line breaks to longer column names for LaTeX using shortstack
  display_cols <- common_cols
  display_cols <- gsub("Looking for a Job", "\\\\shortstack{Looking for\\\\\\\\a Job}", display_cols)
  display_cols <- gsub("Not Looking for a Job", "\\\\shortstack{Not Looking\\\\\\\\for a Job}", display_cols)
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
    pack_rows("Panel A: Race", 1, n_race) %>%
    pack_rows("Panel B: Gender", n_race + 1, n_race + n_gender) %>%
    pack_rows("Panel C: Age", n_race + n_gender + 1, n_race + n_gender + n_age)

  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  message("✓ LaTeX three-panel bivariate table saved to: ", out_tex)
}

# ---------- CONFIGS FOR THREE RUNS ----------

root_dir <- excel

runs <- list(
  # 1. RACE
  list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif",
    coef1       = 1L,  # Selectivity
    coef3       = 3L   # Discretion
  ),
  # 2. GENDER
  list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif_gender",
    coef1       = 1L,
    coef3       = 3L
  ),
  # 3. AGE
  list(
    root        = root_dir,
    sheet_name  = "EIV_BIVARIATE",
    lhs         = "log_dif_age",
    coef1       = 1L,
    coef3       = 3L
  )
)

# ---------- BUILD THE THREE-PANEL TABLE ----------

build_three_panel_bivariate_table(
  cfg_race   = runs[[1]],
  cfg_gender = runs[[2]],
  cfg_age    = runs[[3]],
  out_tex    = file.path(tables, "EIV_bivariate_three_panel.tex")
)
