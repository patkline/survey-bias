# ------------------------------------------------------------------------------
# Purpose: Build firm rankings and item worths tables across 4 models
#          (PL, Borda, OL, OLS) for pooled_favor_white and pooled_favor_male.
#          Sorted by PL ranking. Outputs LaTeX tables.
#
# Created: 2026-03-03
# ------------------------------------------------------------------------------

# Load global paths (defines `excel`, `tables`, etc.)
source("code/globals.R")

# Path to the full-sample Excel workbook with all model outputs
full_sample_wb <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")

# The two outcomes we want to compare across models
outcomes <- c("pooled_favor_white", "pooled_favor_male")

# Formatter: 3-decimal places, or empty string if NA
fmt3 <- function(x) ifelse(is.na(x), "", formatC(x, digits = 3, format = "f"))

# Escape LaTeX special characters in firm names (e.g., & becomes \&)
escape_latex <- function(x) {
  x <- gsub("&", "\\\\&", x)    # escape ampersands
  x <- gsub("%", "\\\\%", x)    # escape percent signs
  x <- gsub("_", "\\\\_", x)    # escape underscores
  x
}

# Ensure output directory exists
dir.create(tables, showWarnings = FALSE, recursive = TRUE)

# Loop over each outcome to build ranking and worth tables
for (outcome in outcomes) {

  # ---- Read item worths from each model's Excel sheet ----

  # PL: Plackett-Luce coefficients (main Coefficients sheet)
  pl_df <- readxl::read_xlsx(full_sample_wb, sheet = "Coefficients")
  # Keep firm name, firm_id, and the outcome column
  pl_sub <- data.frame(
    firm    = pl_df[["firm"]],                # firm name
    firm_id = pl_df[["firm_id"]],             # firm identifier (merge key)
    PL      = as.numeric(pl_df[[outcome]])    # PL item worth
  )

  # Borda: Borda score means (borda_score sheet)
  borda_df <- readxl::read_xlsx(full_sample_wb, sheet = "borda_score")
  # Keep firm_id and the outcome column
  borda_sub <- data.frame(
    firm_id = borda_df[["firm_id"]],
    Borda   = as.numeric(borda_df[[outcome]])  # Borda item worth
  )

  # OL: Ordered Logit coefficients (OL_Coefficients sheet)
  # Multiply by -1 to align sign convention with PL/Borda
  ol_df <- readxl::read_xlsx(full_sample_wb, sheet = "OL_Coefficients")
  # Keep firm_id and the outcome column
  ol_sub <- data.frame(
    firm_id = ol_df[["firm_id"]],
    OL      = -1 * as.numeric(ol_df[[outcome]])  # OL item worth (sign-flipped)
  )

  # OLS: OLS firm FE coefficients (OLS_Coefficients sheet)
  # Multiply by -1 to align sign convention with PL/Borda
  ols_df <- readxl::read_xlsx(full_sample_wb, sheet = "OLS_Coefficients")
  # Keep firm_id and the outcome column
  ols_sub <- data.frame(
    firm_id = ols_df[["firm_id"]],
    OLS     = -1 * as.numeric(ols_df[[outcome]])  # OLS item worth (sign-flipped)
  )

  # ---- Merge all models on firm_id ----

  # Start with PL (has firm name), left-join the others by firm_id
  merged <- merge(pl_sub, borda_sub, by = "firm_id", all.x = TRUE)
  merged <- merge(merged,  ol_sub,   by = "firm_id", all.x = TRUE)
  merged <- merge(merged,  ols_sub,  by = "firm_id", all.x = TRUE)

  # ---- Compute rankings (higher item worth = rank 1) ----

  merged$rank_PL    <- rank(-merged$PL,    ties.method = "min", na.last = "keep")
  merged$rank_Borda <- rank(-merged$Borda, ties.method = "min", na.last = "keep")
  merged$rank_OL    <- rank(-merged$OL,    ties.method = "min", na.last = "keep")
  merged$rank_OLS   <- rank(-merged$OLS,   ties.method = "min", na.last = "keep")

  # ---- Sort by PL ranking ----

  merged <- merged[order(merged$rank_PL), ]  # sort ascending by PL rank

  # ---- Escape firm names for LaTeX ----

  firm_names_tex <- escape_latex(as.character(merged$firm))

  # ---- Build Rankings LaTeX table ----

  # Each row: Firm & PL_rank & Borda_rank & OL_rank & OLS_rank \\
  rank_rows <- paste0(
    "    ", firm_names_tex,
    " & ", merged$rank_PL,
    " & ", merged$rank_Borda,
    " & ", merged$rank_OL,
    " & ", merged$rank_OLS,
    " \\\\"
  )

  # Assemble full LaTeX tabular for rankings
  rank_latex <- c(
    "  \\centering",
    "  \\begin{tabular}{lcccc}",
    "    \\toprule",
    "    Firm & PL & Borda & OL & OLS \\\\",
    "    \\midrule",
    rank_rows,                                # one row per firm
    "    \\bottomrule",
    "  \\end{tabular}"
  )

  # Determine output filename suffix from outcome name
  suffix <- sub("pooled_favor_", "", outcome) # "white" or "male"

  # Write rankings LaTeX file
  rank_tex_path <- file.path(tables, paste0("firm_rankings_pooled_", suffix, ".tex"))
  writeLines(rank_latex, rank_tex_path)       # save to disk
  message("Saved rankings table: ", rank_tex_path)

  # Write rankings CSV
  rank_csv <- merged[, c("firm", "firm_id", "rank_PL", "rank_Borda", "rank_OL", "rank_OLS")]
  rank_csv_path <- file.path(tables, paste0("firm_rankings_pooled_", suffix, ".csv"))
  write.csv(rank_csv, rank_csv_path, row.names = FALSE, quote = FALSE)
  message("Saved rankings CSV: ", rank_csv_path)

  # ---- Build Item Worths LaTeX table ----

  # Each row: Firm & PL_worth & Borda_worth & OL_worth & OLS_worth \\
  worth_rows <- paste0(
    "    ", firm_names_tex,
    " & ", fmt3(merged$PL),
    " & ", fmt3(merged$Borda),
    " & ", fmt3(merged$OL),
    " & ", fmt3(merged$OLS),
    " \\\\"
  )

  # Assemble full LaTeX tabular for item worths
  worth_latex <- c(
    "  \\centering",
    "  \\begin{tabular}{lcccc}",
    "    \\toprule",
    "    Firm & PL & Borda & OL & OLS \\\\",
    "    \\midrule",
    worth_rows,                               # one row per firm
    "    \\bottomrule",
    "  \\end{tabular}"
  )

  # Write item worths LaTeX file
  worth_tex_path <- file.path(tables, paste0("firm_worths_pooled_", suffix, ".tex"))
  writeLines(worth_latex, worth_tex_path)     # save to disk
  message("Saved item worths table: ", worth_tex_path)

  # Write item worths CSV
  worth_csv <- merged[, c("firm", "firm_id", "PL", "Borda", "OL", "OLS")]
  worth_csv_path <- file.path(tables, paste0("firm_worths_pooled_", suffix, ".csv"))
  write.csv(worth_csv, worth_csv_path, row.names = FALSE, quote = FALSE)
  message("Saved item worths CSV: ", worth_csv_path)
}
