# ------------------------------------------------------------------------------
# Purpose: Calculate Correlations
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
# -------------------------------------------------------------------
# Build correlations from variance + covariance sheets
# - one output row per row in `cov_df`
# Expects:
#   var_df has:  subset, model, outcome, variance, signal
#   cov_df has:  subset, model, lhs, rhs, covariance, noise
# Returns: cov_df + variance1/2 + signal1/2 + corr/corr_den/corr_c
# -------------------------------------------------------------------
build_correlation_from_varcov <- function(
    var_df,
    cov_df,
    subset_col = "subset",
    model_col  = "model",
    outcome_col = "outcome",
    lhs_col = "lhs",
    rhs_col = "rhs",
    variance_col = "variance",
    signal_col   = "signal",
    covariance_col = "covariance",
    noise_col      = "noise"  # <-- set default to what your cov sheet currently uses
) {
  req_var <- c(subset_col, model_col, outcome_col, variance_col, signal_col)
  req_cov <- c(subset_col, model_col, lhs_col, rhs_col, covariance_col, noise_col)
  
  stopifnot(all(req_var %in% names(var_df)))
  stopifnot(all(req_cov %in% names(cov_df)))
  
  var_keep <- var_df |>
    dplyr::select(dplyr::all_of(c(subset_col, model_col, outcome_col, variance_col, signal_col)))
  
  # Join variance/signal for lhs
  out <- cov_df |>
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          lhs       = !!rlang::sym(outcome_col),
          variance1 = !!rlang::sym(variance_col),
          signal1   = !!rlang::sym(signal_col)
        ),
      by = c(subset_col, model_col, lhs_col)
    ) |>
    # Join variance/signal for rhs
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          rhs       = !!rlang::sym(outcome_col),
          variance2 = !!rlang::sym(variance_col),
          signal2   = !!rlang::sym(signal_col)
        ),
      by = c(subset_col, model_col, rhs_col)
    )
  
  out |>
    dplyr::mutate(
      denom_var = sqrt(pmax(variance1, 0)) * sqrt(pmax(variance2, 0)),
      denom_sig = sqrt(pmax(signal1,   0)) * sqrt(pmax(signal2,   0)),
      
      corr = dplyr::if_else(
        is.finite(denom_var) & denom_var > 0,
        .data[[covariance_col]] / denom_var,
        NA_real_
      ),
      
      corr_den = dplyr::if_else(
        is.finite(denom_sig) & denom_sig > 0,
        .data[[covariance_col]] / denom_sig,
        NA_real_
      ),
      
      corr_c = dplyr::if_else(
        is.finite(denom_sig) & denom_sig > 0,
        (.data[[covariance_col]] - .data[[noise_col]]) / denom_sig,
        NA_real_
      )
    ) |>
    dplyr::select(-denom_var, -denom_sig)
}