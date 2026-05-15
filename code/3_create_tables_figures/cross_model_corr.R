# ------------------------------------------------------------------------------
# Cross-Model Correlation of Item Worths for Pooled Variables
# Produces LaTeX tables (one per pooled var) showing pairwise correlations of
# firm-level item worths across models (PL, Borda, OLS, OLSC).
#
# Two variants are written for each pooled variable:
#   1) unweighted: full firm sample (subset == "all"), Pearson correlation.
#   2) njobs_weighted: selected firm sample (firms with valid njobs at
#      subset97), correlation weighted by njobs.
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

# --- Read Coefficients (long format from current pipeline) ---
coef_df <- read_parquet_sheet(full_sample_dir, "Coefficients")

# --- Helpers ---

# Pivot to firm x model matrix of estimates for one outcome at one subset.
# Keeps only Firm rows. Returns a data frame with columns:
#   firm_id, njobs, <one column per model>
build_firm_wide <- function(coef_df, var, models, subset_keep) {
  long <- coef_df %>%
    dplyr::filter(.data$entity_type == "Firm",
                  .data$subset == subset_keep,
                  .data$model %in% models,
                  .data$outcome == var) %>%
    dplyr::transmute(
      firm_id  = as.integer(.data$entity_id),
      model    = as.character(.data$model),
      njobs    = suppressWarnings(as.numeric(.data$njobs)),
      estimate = suppressWarnings(as.numeric(.data$estimate))
    )

  # njobs is firm-level: assert it agrees across models for the same firm,
  # then collapse to one row per firm before pivoting.
  njobs_per_firm <- long %>%
    dplyr::group_by(.data$firm_id) %>%
    dplyr::summarise(
      njobs = dplyr::first(.data$njobs[is.finite(.data$njobs)]),
      n_distinct_njobs = dplyr::n_distinct(.data$njobs[is.finite(.data$njobs)]),
      .groups = "drop"
    )
  if (any(njobs_per_firm$n_distinct_njobs > 1L, na.rm = TRUE)) {
    stop("njobs disagrees across models for the same firm_id in Coefficients sheet.")
  }

  wide <- long %>%
    dplyr::select(-njobs) %>%
    tidyr::pivot_wider(names_from = "model", values_from = "estimate")

  dplyr::left_join(wide, njobs_per_firm[, c("firm_id", "njobs")], by = "firm_id")
}

# Weighted Pearson correlation between two columns x, y given weights w.
# Rows where any of x, y, w are non-finite (or w <= 0) are dropped.
weighted_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 2L) return(NA_real_)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  W <- sum(w)
  mx <- sum(w * x) / W
  my <- sum(w * y) / W
  vx <- sum(w * (x - mx)^2) / W
  vy <- sum(w * (y - my)^2) / W
  cxy <- sum(w * (x - mx) * (y - my)) / W
  denom <- sqrt(vx * vy)
  if (!is.finite(denom) || denom == 0) NA_real_ else cxy / denom
}

# --- Build correlation matrix for one variable / variant ---
build_cross_model_corr <- function(coef_df, var, models,
                                   variant = c("unweighted", "njobs_weighted")) {
  variant <- match.arg(variant)
  subset_keep <- if (variant == "unweighted") "all" else "subset97"
  wide <- build_firm_wide(coef_df, var, models, subset_keep)

  if (variant == "njobs_weighted") {
    wide <- wide %>%
      dplyr::filter(is.finite(.data$njobs) & .data$njobs > 0)
  }

  n <- length(models)
  mat <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(models, models))
  for (i in seq_along(models)) {
    for (j in seq_along(models)) {
      xi <- wide[[models[i]]]
      yj <- wide[[models[j]]]
      mat[i, j] <- if (variant == "unweighted") {
        ok <- is.finite(xi) & is.finite(yj)
        if (sum(ok) < 2L) NA_real_ else stats::cor(xi[ok], yj[ok])
      } else {
        weighted_cor(xi, yj, wide$njobs)
      }
    }
  }
  mat
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

# --- Generate tables (both variants for each pooled var) ---
for (v in pooled_vars) {
  for (variant in c("unweighted", "njobs_weighted")) {
    mat <- build_cross_model_corr(coef_df, v, models, variant = variant)
    tex <- corr_to_latex(mat, models, model_labels, var_labels[v])

    suffix <- if (variant == "njobs_weighted") "_njobs_weighted" else ""
    out_file <- file.path(
      tables,
      paste0("cross_model_corr_ols_borda_", v, suffix, ".tex")
    )
    writeLines(tex, out_file)
    message("Wrote: ", out_file)
  }
}
