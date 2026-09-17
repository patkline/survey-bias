# ------------------------------------------------------------------------------
# Purpose: Calculate Correlations
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
# -------------------------------------------------------------------
# Build correlation rows in memory from stored variance + covariance sheets.
# Section 3 calls this helper on demand; the pipeline does not persist a
# separate correlation parquet sheet.
# - one output row per row in `cov_df`
# Expects:
#   var_df has:  subset, model, outcome, variance, noise
#   cov_df has:  subset, model, lhs, rhs, covariance, noise
# Returns: cov_df plus corr_c, the correlation from the pairwise
# multivariate-Katz signal matrix.
# -------------------------------------------------------------------
build_correlation_from_varcov <- function(
    var_df,
    cov_df
) {
  req_var <- c("subset", "model", "outcome", "variance", "noise")
  req_cov <- c("subset", "model", "lhs", "rhs", "covariance", "noise")

  signal_vcov_columns <- paste0(
    "signal_vcov_",
    c("11", "12", "13", "22", "23", "33")
  )
  req_cov <- c(req_cov, signal_vcov_columns)
  if (!exists("compute_multivariate_katz_signal_correlation", mode = "function")) {
    stop(
      "Multivariate Katz correlation helpers are not loaded. Source eiv_functions.R first.",
      call. = FALSE
    )
  }
  
  stopifnot(all(req_var %in% names(var_df)))
  stopifnot(all(req_cov %in% names(cov_df)))
  
  var_keep <- var_df |>
    dplyr::select(subset, model, outcome, variance, noise)

  # Join variance and noise for lhs.
  within_model_out <- cov_df |>
    dplyr::filter(.data$model != "OLS_x_Borda") |>
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          lhs       = outcome,
          variance1 = variance,
          noise1    = noise
        ),
      by = c("subset", "model", "lhs")
    ) |>
    # Join variance and noise for rhs.
    dplyr::left_join(
      var_keep |>
        dplyr::rename(
          rhs       = outcome,
          variance2 = variance,
          noise2    = noise
        ),
      by = c("subset", "model", "rhs")
    )

  # Cross-model rows pair the OLS and Borda estimates of one outcome
  cross_model_out <- cov_df |>
    dplyr::filter(.data$model == "OLS_x_Borda") |>
    # Join OLS variance and noise for lhs.
    dplyr::left_join(
      var_keep |>
        dplyr::filter(.data$model == "OLS") |>
        dplyr::select(-model) |>
        dplyr::rename(
          lhs       = outcome,
          variance1 = variance,
          noise1    = noise
        ),
      by = c("subset", "lhs")
    ) |>
    # Join Borda variance and noise for rhs.
    dplyr::left_join(
      var_keep |>
        dplyr::filter(.data$model == "Borda") |>
        dplyr::select(-model) |>
        dplyr::rename(
          rhs       = outcome,
          variance2 = variance,
          noise2    = noise
        ),
      by = c("subset", "rhs")
    )

  out <- dplyr::bind_rows(within_model_out, cross_model_out)

  # Every covariance row must survive the model split and the joins exactly once
  stopifnot(nrow(out) == nrow(cov_df))
  
  multivariate_rows <- lapply(seq_len(nrow(out)), function(row_index) {
    row <- out[row_index, , drop = FALSE]
    variable_names <- c(
      as.character(row$lhs[1L]),
      as.character(row$rhs[1L])
    )
    observed_covariance_matrix <- matrix(
      c(
        as.numeric(row$variance1[1L]), as.numeric(row$covariance[1L]),
        as.numeric(row$covariance[1L]), as.numeric(row$variance2[1L])
      ),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(variable_names, variable_names)
    )
    raw_noise_matrix <- matrix(
      c(
        as.numeric(row$noise1[1L]), as.numeric(row$noise[1L]),
        as.numeric(row$noise[1L]), as.numeric(row$noise2[1L])
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
        row$model[1L], ", subset=", row$subset[1L],
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
        row$model[1L], ", subset=", row$subset[1L],
        ", lhs=", variable_names[1L], ", rhs=", variable_names[2L],
        ".",
        call. = FALSE
      )
    }

    data.frame(corr_c = corrected)
  })

  multivariate_rows <- dplyr::bind_rows(multivariate_rows)
  stopifnot(nrow(multivariate_rows) == nrow(out))
  out <- dplyr::bind_cols(out, multivariate_rows)

  if (any(!is.finite(out$corr_c)) || any(abs(out$corr_c) > 1 + 1e-8)) {
    stop("Multivariate Katz correlation output failed the [-1, 1] bound check.", call. = FALSE)
  }
  dplyr::select(out, dplyr::all_of(names(cov_df)), corr_c)
}
