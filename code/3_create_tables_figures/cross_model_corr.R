# ------------------------------------------------------------------------------
# Cross-Model Correlation of Item Worths for Pooled Variables
# Produces two LaTeX tables (one per pooled var) showing pairwise correlations
# of firm-level item worths across models (PL, Borda, OLS, OLSC).
# ------------------------------------------------------------------------------

source("code/globals.R")

# --- Config ---
full_sample_dir <- file.path(intermediate, "Full_Sample")
#models <- c("PL", "Borda", "OL", "OLS", "OLSC")
#model_labels <- c(PL = "Plackett--Luce", Borda = "Borda", OL = "Ordered Logit", OLS = "Likert", OLSC = "Likert Centered")
models <- c("Borda", "OLS")
model_labels <- c(Borda = "Borda", OLS = "Likert")
pooled_vars <- c("pooled_favor_white", "pooled_favor_male")
var_labels <- c(
  pooled_favor_white = "Pooled Favor White",
  pooled_favor_male  = "Pooled Favor Male"
)

# --- Read firm-level subset97 coefficients from the new long Coefficients sheet ---
coef_df <- read_parquet_sheet(full_sample_dir, "Coefficients") %>%
  dplyr::filter(
    .data$subset == "subset97",
    .data$entity_type == "Firm",
    .data$model %in% models,
    .data$outcome %in% pooled_vars
  ) %>%
  dplyr::transmute(
    firm_id = as.integer(.data$entity_id),
    model   = as.character(.data$model),
    outcome = as.character(.data$outcome),
    estimate = as.numeric(.data$estimate)
  )

# --- Build correlation matrix for one variable ---
build_cross_model_corr <- function(coef_df, var, models) {
  # Filter to relevant models and select firm_id + outcome
  wide <- coef_df %>%
    dplyr::filter(.data$model %in% models, .data$outcome == var) %>%
    dplyr::select(firm_id, model, estimate) %>%
    tidyr::pivot_wider(names_from = model, values_from = estimate)

  # Compute correlation matrix (pairwise complete obs)
  mat <- cor(wide[, models], use = "pairwise.complete.obs")
  return(mat)
}

# --- Format correlation matrix as LaTeX ---
corr_to_latex <- function(mat, models, model_labels, caption_var) {
  n <- length(models)
  labs <- model_labels[models]

  # Header
  header <- paste0("\\begin{tabular}{l", paste(rep("c", n), collapse = ""), "}")
  lines <- c(
    header,
    "\\toprule",
    paste0(" & ", paste(labs, collapse = " & "), "\\\\"),
    "\\midrule"
  )

  # Body — full matrix, format to 3 decimal places
  for (i in seq_along(models)) {
    vals <- sapply(seq_along(models), function(j) {
      sprintf("%.3f", mat[i, j])
    })
    lines <- c(lines, paste0(labs[i], " & ", paste(vals, collapse = " & "), "\\\\"))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  return(paste(lines, collapse = "\n"))
}

# --- Generate tables ---
for (v in pooled_vars) {
  mat <- build_cross_model_corr(coef_df, v, models)
  tex <- corr_to_latex(mat, models, model_labels, var_labels[v])

  out_file <- file.path(tables, paste0("cross_model_corr_ols_borda_", v, ".tex"))
  writeLines(tex, out_file)
  message("Wrote: ", out_file)
}
