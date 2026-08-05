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
# Returns: cov_df + variance1/2 + signal1/2 + corr/corr_den/corr_c.
# When use_multivariate_katz is TRUE, corr_c is replaced by the correlation
# from the pairwise multivariate-Katz signal matrix and the former scalar-Katz
# ratio is retained as corr_c_scalar_katz.
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
    noise_col      = "noise",  # <-- set default to what your cov sheet currently uses
    use_multivariate_katz = FALSE
) {
    req_var <- c(subset_col, model_col, outcome_col, variance_col, signal_col, "noise")
    req_cov <- c(subset_col, model_col, lhs_col, rhs_col, covariance_col, noise_col)

    signal_vcov_columns <- paste0(
      "signal_vcov_",
      c("11", "12", "13", "22", "23", "33")
    )
    if (isTRUE(use_multivariate_katz)) {
      req_cov <- c(req_cov, signal_vcov_columns)
      if (!exists("compute_multivariate_katz_signal_correlation", mode = "function")) {
        stop(
          "Multivariate Katz correlation helpers are not loaded. Source eiv_functions.R first.",
          call. = FALSE
        )
      }
    }
  
  stopifnot(all(req_var %in% names(var_df)))
  stopifnot(all(req_cov %in% names(cov_df)))
  
    var_keep <- var_df |>
    dplyr::select(dplyr::all_of(c(
      subset_col, model_col, outcome_col, variance_col, signal_col, "noise"
    )))

  # Join variance/signal for lhs
  within_model_out <- cov_df |>
    dplyr::filter(.data[[model_col]] != "OLS_x_Borda") |>
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          lhs       = !!rlang::sym(outcome_col),
          variance1 = !!rlang::sym(variance_col),
          signal1   = !!rlang::sym(signal_col),
          noise1    = noise
        ),
      by = c(subset_col, model_col, lhs_col)
    ) |>
    # Join variance/signal for rhs
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          rhs       = !!rlang::sym(outcome_col),
          variance2 = !!rlang::sym(variance_col),
          signal2   = !!rlang::sym(signal_col),
          noise2    = noise
        ),
      by = c(subset_col, model_col, rhs_col)
    )

  # Cross-model rows pair the OLS and Borda estimates of one outcome
  cross_model_out <- cov_df |>
    dplyr::filter(.data[[model_col]] == "OLS_x_Borda") |>
    # Join OLS variance/signal for lhs
    dplyr::left_join(
      var_keep |>
        dplyr::filter(.data[[model_col]] == "OLS") |>
        dplyr::select(-dplyr::all_of(model_col)) |>
        dplyr::rename(
          lhs       = !!rlang::sym(outcome_col),
          variance1 = !!rlang::sym(variance_col),
          signal1   = !!rlang::sym(signal_col),
          noise1    = noise
        ),
      by = c(subset_col, lhs_col)
    ) |>
    # Join Borda variance/signal for rhs
    dplyr::left_join(
      var_keep |>
        dplyr::filter(.data[[model_col]] == "Borda") |>
        dplyr::select(-dplyr::all_of(model_col)) |>
        dplyr::rename(
          rhs       = !!rlang::sym(outcome_col),
          variance2 = !!rlang::sym(variance_col),
          signal2   = !!rlang::sym(signal_col),
          noise2    = noise
        ),
      by = c(subset_col, rhs_col)
    )

  out <- dplyr::bind_rows(within_model_out, cross_model_out)

  # Every covariance row must survive the model split and the joins exactly once
  stopifnot(nrow(out) == nrow(cov_df))
  
  out <- out |>
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
      
      corr_c_scalar_katz = dplyr::if_else(
        is.finite(denom_sig) & denom_sig > 0,
        (.data[[covariance_col]] - .data[[noise_col]]) / denom_sig,
        NA_real_
      ),
      corr_c = corr_c_scalar_katz
    ) |>
    dplyr::select(-denom_var, -denom_sig)

  if (!isTRUE(use_multivariate_katz)) return(out)

  multivariate_rows <- lapply(seq_len(nrow(out)), function(row_index) {
    row <- out[row_index, , drop = FALSE]
    variable_names <- c(
      as.character(row[[lhs_col]][1L]),
      as.character(row[[rhs_col]][1L])
    )
    observed_covariance_matrix <- matrix(
      c(
        as.numeric(row$variance1[1L]), as.numeric(row[[covariance_col]][1L]),
        as.numeric(row[[covariance_col]][1L]), as.numeric(row$variance2[1L])
      ),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(variable_names, variable_names)
    )
    raw_noise_matrix <- matrix(
      c(
        as.numeric(row$noise1[1L]), as.numeric(row[[noise_col]][1L]),
        as.numeric(row[[noise_col]][1L]), as.numeric(row$noise2[1L])
      ),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(variable_names, variable_names)
    )
    signal_vcov <- signal_vcov_from_values(
      unlist(row[1L, signal_vcov_columns, drop = FALSE], use.names = FALSE),
      variable_names
    )
    if (is.null(signal_vcov)) {
      stop(
        "Missing delta-method signal VCV for correlation row: model=",
        row[[model_col]][1L], ", subset=", row[[subset_col]][1L],
        ", lhs=", variable_names[1L], ", rhs=", variable_names[2L],
        ". Re-run the Section 2 covariance build.",
        call. = FALSE
      )
    }

    corrected <- compute_multivariate_katz_signal_correlation(
      observed_covariance_matrix = observed_covariance_matrix,
      raw_noise_matrix = raw_noise_matrix,
      signal_vcov = signal_vcov
    )
    if (is.null(corrected)) {
      stop(
        "Multivariate Katz correlation failed for: model=",
        row[[model_col]][1L], ", subset=", row[[subset_col]][1L],
        ", lhs=", variable_names[1L], ", rhs=", variable_names[2L],
        ".",
        call. = FALSE
      )
    }

    data.frame(
      corr_c_multivariate_katz = corrected$signal_correlation,
      multivariate_katz_signal_variance1 = corrected$signal_matrix[1L, 1L],
      multivariate_katz_signal_covariance = corrected$signal_matrix[1L, 2L],
      multivariate_katz_signal_variance2 = corrected$signal_matrix[2L, 2L],
      multivariate_katz_acceptance_rate = corrected$multivariate_katz_acceptance_rate,
      multivariate_katz_draws = corrected$multivariate_katz_draws,
      multivariate_katz_accepts = corrected$multivariate_katz_accepts,
      multivariate_katz_pd_rejects = corrected$multivariate_katz_pd_rejects,
      multivariate_katz_pd_rejection_rate = corrected$multivariate_katz_pd_rejection_rate
    )
  })

  multivariate_rows <- dplyr::bind_rows(multivariate_rows)
  stopifnot(nrow(multivariate_rows) == nrow(out))
  out <- dplyr::bind_cols(out, multivariate_rows)
  out$corr_c <- out$corr_c_multivariate_katz

  if (any(!is.finite(out$corr_c)) || any(abs(out$corr_c) > 1 + 1e-8)) {
    stop("Multivariate Katz correlation output failed the [-1, 1] bound check.", call. = FALSE)
  }
  out
}
