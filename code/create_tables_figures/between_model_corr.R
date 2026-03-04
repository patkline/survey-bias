# ------------------------------------------------------------------------------
# Purpose: Compute pairwise Pearson correlations of item worths across 4 models
#          (PL, Borda, OL, OLS) for pooled_favor_white and pooled_favor_male.
#          Outputs LaTeX correlation matrix tables.
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

# Ensure output directory exists
dir.create(tables, showWarnings = FALSE, recursive = TRUE)

# Loop over each outcome to build a separate correlation table
for (outcome in outcomes) {

  # ---- Read item worths from each model's Excel sheet ----

  # PL: Plackett-Luce coefficients (main Coefficients sheet)
  pl_df <- readxl::read_xlsx(full_sample_wb, sheet = "Coefficients")
  # Extract firm_id and the outcome column for PL
  pl_vec <- pl_df[[outcome]]                  # PL item worths vector
  firm_ids <- pl_df[["firm_id"]]              # firm identifiers (shared key)

  # Borda: Borda score means (borda_score sheet)
  borda_df <- readxl::read_xlsx(full_sample_wb, sheet = "borda_score")
  # Extract the outcome column for Borda
  borda_vec <- borda_df[[outcome]]            # Borda item worths vector

  # OL: Ordered Logit coefficients (OL_Coefficients sheet)
  # Multiply by -1 to align sign convention with PL/Borda
  ol_df <- readxl::read_xlsx(full_sample_wb, sheet = "OL_Coefficients")
  # Extract the outcome column for OL (sign-flipped)
  ol_vec <- -1 * ol_df[[outcome]]             # OL item worths vector

  # OLS: OLS firm FE coefficients (OLS_Coefficients sheet)
  # Multiply by -1 to align sign convention with PL/Borda
  ols_df <- readxl::read_xlsx(full_sample_wb, sheet = "OLS_Coefficients")
  # Extract the outcome column for OLS (sign-flipped)
  ols_vec <- -1 * ols_df[[outcome]]           # OLS item worths vector

  # ---- Combine into a single data frame for correlation ----

  # Build a data frame with all 4 model columns, one row per firm
  all_worths <- data.frame(
    PL    = pl_vec,       # Plackett-Luce item worths
    Borda = borda_vec,    # Borda item worths
    OL    = ol_vec,       # Ordered Logit item worths
    OLS   = ols_vec       # OLS item worths
  )

  # ---- Compute pairwise Pearson correlation matrix ----

  # cor() returns the 4x4 correlation matrix; use pairwise complete obs
  corr_mat <- cor(all_worths, use = "pairwise.complete.obs")

  # ---- Build LaTeX table (lower triangle + diagonal) ----

  # Model labels for rows and columns
  model_names <- c("PL", "Borda", "OL", "OLS")

  # Build each row of the LaTeX table manually
  latex_rows <- character(4)                  # one string per model row
  for (i in seq_along(model_names)) {
    # Start with the row label (model name)
    row_str <- paste0("    ", model_names[i])
    for (j in seq_along(model_names)) {
      if (j <= i) {
        # Lower triangle + diagonal: show the correlation value
        row_str <- paste0(row_str, " & ", fmt3(corr_mat[i, j]))
      } else {
        # Upper triangle: leave blank
        row_str <- paste0(row_str, " & ")
      }
    }
    # End the row with LaTeX line break
    latex_rows[i] <- paste0(row_str, " \\\\")
  }

  # Assemble full LaTeX tabular environment
  latex_lines <- c(
    "  \\centering",
    paste0("  \\caption{Between-Model Correlations: ", outcome, "}"),
    "  \\begin{tabular}{lcccc}",
    "    \\toprule",
    "     & PL & Borda & OL & OLS \\\\",
    "    \\midrule",
    latex_rows,                               # the 4 data rows
    "    \\bottomrule",
    "  \\end{tabular}"
  )

  # ---- Write the LaTeX file ----

  # Output filename based on outcome name
  out_tex <- file.path(tables, paste0("between_model_corr_", outcome, ".tex"))
  writeLines(latex_lines, out_tex)            # write LaTeX to disk

  # ---- Also save as CSV for convenience ----

  out_csv <- file.path(tables, paste0("between_model_corr_", outcome, ".csv"))
  write.csv(corr_mat, out_csv, quote = FALSE) # write CSV to disk

  # Print confirmation
  message("Saved between-model correlation table for ", outcome, ":")
  message(" - ", out_tex)
  message(" - ", out_csv)
}
