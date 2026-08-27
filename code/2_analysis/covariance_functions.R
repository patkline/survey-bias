# ------------------------------------------------------------------------------
# Purpose: Calculate Covariance, Noise, and the clustered sampling covariance
# of the bivariate signal-covariance estimator.
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
prepare_clustered_signal_inputs <- function(res1, res2, common_cols) {
  S1_df <- res1$mats$S
  S2_df <- res2$mats$S

  collapse_respondent_influence <- function(S_df) {
    ids <- as.character(S_df[["resp_id"]])
    if (anyNA(ids)) {
      stop("compute_clustered_signal_vcov(): respondent ids cannot be missing.")
    }
    influence <- as.matrix(S_df[, common_cols, drop = FALSE])
    had_duplicates <- anyDuplicated(ids) > 0L
    if (had_duplicates) {
      influence <- rowsum(influence, group = ids, reorder = FALSE)
      ids <- rownames(influence)
    }
    list(ids = ids, influence = influence, had_duplicates = had_duplicates)
  }
  collapsed1 <- collapse_respondent_influence(S1_df)
  collapsed2 <- collapse_respondent_influence(S2_df)
  id1 <- collapsed1$ids
  id2 <- collapsed2$ids

  firm_ids <- as.integer(sub("^entity", "", common_cols))
  beta1 <- as.numeric(res1$firm_table$estimate[match(firm_ids, res1$firm_table$entity_id)])
  beta2 <- as.numeric(res2$firm_table$estimate[match(firm_ids, res2$firm_table$entity_id)])
  J <- length(common_cols)
  C11 <- if (collapsed1$had_duplicates) {
    crossprod(collapsed1$influence)
  } else {
    as.matrix(res1$mats$rcov[common_cols, common_cols, drop = FALSE])
  }
  C22 <- if (collapsed2$had_duplicates) {
    crossprod(collapsed2$influence)
  } else {
    as.matrix(res2$mats$rcov[common_cols, common_cols, drop = FALSE])
  }
  common_ids <- intersect(id1, id2)
  if (length(common_ids)) {
    C12 <- crossprod(
      collapsed1$influence[match(common_ids, id1), , drop = FALSE],
      collapsed2$influence[match(common_ids, id2), , drop = FALSE]
    )
  } else {
    C12 <- matrix(0, nrow = J, ncol = J)
  }

  list(
    beta1 = beta1,
    beta2 = beta2,
    C = rbind(cbind(C11, C12), cbind(t(C12), C22)),
    J = J
  )
}

assert_delta_method_vcov_matches_scalar <- function(signal_vcov,
                                                    prepared_inputs,
                                                    weights,
                                                    tolerance = 1e-10) {
  J <- prepared_inputs$J
  weights <- as.numeric(weights) / sum(weights)

  index1 <- seq_len(J)
  index2 <- J + seq_len(J)

  # Explicitly zero the cross-outcome firm-by-firm blocks. The variance
  # gradients have support within one outcome, so these are the same two
  # diagonal entries produced by the main multivariate calculation.
  C_zero_cross <- prepared_inputs$C
  C_zero_cross[index1, index2] <- 0
  C_zero_cross[index2, index1] <- 0

  if (!exists("compute_njobs_weighted_signal_components", mode = "function")) {
    stop(
      "assert_delta_method_vcov_matches_scalar(): compute_njobs_weighted_signal_components() is not loaded.",
      call. = FALSE
    )
  }
  expected <- c(
    compute_njobs_weighted_signal_components(
      firm_regressor_vector = prepared_inputs$beta1,
      firm_number_of_jobs_vector = weights,
      firm_robust_covariance_matrix = C_zero_cross[index1, index1, drop = FALSE]
    )$Vhat_njobs_weighted,
    compute_njobs_weighted_signal_components(
      firm_regressor_vector = prepared_inputs$beta2,
      firm_number_of_jobs_vector = weights,
      firm_robust_covariance_matrix = C_zero_cross[index2, index2, drop = FALSE]
    )$Vhat_njobs_weighted
  )
  comparator <- "compute_njobs_weighted_signal_components()"

  observed <- c(signal_vcov[1L, 1L], signal_vcov[3L, 3L])
  absolute_difference <- abs(observed - expected)
  relative_difference <- absolute_difference / pmax(abs(expected), .Machine$double.xmin)

  if (any(!is.finite(relative_difference)) || any(relative_difference > tolerance)) {
    stop(
      "Delta-method acceptance test failed. Multivariate diagonal Vhat values: ",
      paste(signif(observed, 16L), collapse = ", "),
      "; scalar ", comparator, " Vhat values: ",
      paste(signif(expected, 16L), collapse = ", "),
      "; relative differences: ",
      paste(signif(relative_difference, 6L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  max(absolute_difference)
}

compute_clustered_signal_vcov <- function(res1 = NULL, res2 = NULL,
                                          common_cols = NULL, weights,
                                          include_cross_outcome = TRUE,
                                          prepared_inputs = NULL) {
  if (is.null(prepared_inputs)) {
    prepared_inputs <- prepare_clustered_signal_inputs(res1, res2, common_cols)
  }
  beta1 <- prepared_inputs$beta1
  beta2 <- prepared_inputs$beta2
  C <- prepared_inputs$C
  J <- prepared_inputs$J
  weights <- as.numeric(weights)

  if (anyNA(c(beta1, beta2, weights)) ||
      !all(is.finite(c(beta1, beta2, weights))) ||
      any(weights <= 0) || sum(weights) <= 0) {
    return(empty_signal_vcov())
  }
  weights <- weights / sum(weights)

  if (!isTRUE(include_cross_outcome)) {
    C[seq_len(J), J + seq_len(J)] <- 0
    C[J + seq_len(J), seq_len(J)] <- 0
  }

  # Gradients for q = (var(outcome 1), cov(outcomes 1,2), var(outcome 2)).
  # Multiplication by A = diag(w) - ww' applies the same weighted centering as
  # the observed covariance estimator without materializing the 2J x 2J B's.
  weighted_centering_quadratic_matrix <-
    diag(weights, nrow = J, ncol = J) - tcrossprod(weights)
  centered_weighted_beta1 <- weights * (beta1 - sum(weights * beta1))
  centered_weighted_beta2 <- weights * (beta2 - sum(weights * beta2))
  zero <- matrix(0, nrow = J, ncol = J)
  gradients <- cbind(
    c(2 * centered_weighted_beta1, rep(0, J)),
    c(centered_weighted_beta2, centered_weighted_beta1),
    c(rep(0, J), 2 * centered_weighted_beta2)
  )

  # This is algebraically crossprod(S_stacked %*% gradients): the sum of
  # respondent-clustered outer products after applying all three gradients.
  signal_vcov <- crossprod(gradients, C %*% gradients)

  # Plug-in quadratic-form bias correction, using A = W - ww' to account for
  # estimation of each outcome's weighted grand mean.
  apply_weighted_centering <- function(C_block) {
    weighted_centering_quadratic_matrix %*% C_block
  }
  index1 <- seq_len(J)
  index2 <- J + seq_len(J)
  P <- apply_weighted_centering(C[index1, index1, drop = FALSE])
  Q <- apply_weighted_centering(C[index1, index2, drop = FALSE])
  R <- apply_weighted_centering(C[index2, index1, drop = FALSE])
  S <- apply_weighted_centering(C[index2, index2, drop = FALSE])
  BC <- list(
    rbind(cbind(P, Q), cbind(zero, zero)),
    rbind(cbind(R / 2, S / 2), cbind(P / 2, Q / 2)),
    rbind(cbind(zero, zero), cbind(R, S))
  )
  for (i in seq_len(3L)) {
    for (j in i:3L) {
      bias_correction <- 2 * sum(BC[[i]] * t(BC[[j]]))
      signal_vcov[i, j] <- signal_vcov[i, j] - bias_correction
      signal_vcov[j, i] <- signal_vcov[i, j]
    }
  }

  component_names <- c("var1", "cov12", "var2")
  dimnames(signal_vcov) <- list(component_names, component_names)
  attr(signal_vcov, "scalar_vhat_acceptance_max_abs_diff") <-
    assert_delta_method_vcov_matches_scalar(
      signal_vcov = signal_vcov,
      prepared_inputs = prepared_inputs,
      weights = weights
    )
  signal_vcov
}

empty_signal_vcov <- function() {
  component_names <- c("var1", "cov12", "var2")
  out <- matrix(NA_real_, nrow = 3L, ncol = 3L,
                dimnames = list(component_names, component_names))
  attr(out, "scalar_vhat_acceptance_max_abs_diff") <- NA_real_
  out
}

compute_pairwise_cov_and_noise <- function(res1, res2) {
  stopifnot(!is.null(res1$mats$S), !is.null(res2$mats$S))
  
  S1_df <- res1$mats$S
  S2_df <- res2$mats$S
  
  if (!("resp_id" %in% names(S1_df)) || !("resp_id" %in% names(S2_df))) {
    stop("compute_pairwise_cov_and_noise(): mats$S must contain a resp_id column.")
  }
  
  # Always scalar N's
  N1 <- as.integer(nrow(S1_df))
  N2 <- as.integer(nrow(S2_df))
  
  id1 <- S1_df[["resp_id"]]
  id2 <- S2_df[["resp_id"]]
  if (is.null(id1)) id1 <- character(0)
  if (is.null(id2)) id2 <- character(0)
  
  overlap_ids <- intersect(id1, id2)
  Ncommon <- as.integer(length(overlap_ids))
  
  firm_cols1 <- names(S1_df)[grepl("^entity\\d+$", names(S1_df))]
  firm_cols2 <- names(S2_df)[grepl("^entity\\d+$", names(S2_df))]
  common_cols <- intersect(firm_cols1, firm_cols2)
  
  if (length(common_cols) < 2L) {
    return(list(
      J = as.integer(length(common_cols)),
      covariance = NA_real_,
      noise = NA_real_,
      covariance_njobs_weighted = NA_real_,
      noise_njobs_weighted = NA_real_,
      signal_vcov = empty_signal_vcov(),
      signal_vcov_njobs_weighted = empty_signal_vcov(),
      scalar_vhat_acceptance_max_abs_diff = NA_real_,
      scalar_vhat_njobs_weighted_acceptance_max_abs_diff = NA_real_,
      N1 = N1,
      N2 = N2,
      Ncommon = Ncommon
    ))
  }
  
  common_cols <- sort(common_cols)
  
  firm_ids <- as.integer(sub("^entity", "", common_cols))

  beta1_full <- as.numeric(res1$firm_table$estimate)
  beta2_full <- as.numeric(res2$firm_table$estimate)
  beta1 <- res1$firm_table$estimate[match(firm_ids, res1$firm_table$entity_id)] -
    mean(beta1_full, na.rm = TRUE)
  beta2 <- res2$firm_table$estimate[match(firm_ids, res2$firm_table$entity_id)] -
    mean(beta2_full, na.rm = TRUE)
  covariance <- mean(beta1 * beta2, na.rm = TRUE)

  clustered_inputs <- prepare_clustered_signal_inputs(res1, res2, common_cols)
  J <- as.integer(clustered_inputs$J)
  Theta12 <- clustered_inputs$C[seq_len(J), J + seq_len(J), drop = FALSE]
  
  noise <- if (J > 0L) sum(Matrix::diag(Theta12)) / J else NA_real_

  signal_vcov <- compute_clustered_signal_vcov(
    res1 = res1,
    res2 = res2,
    common_cols = common_cols,
    weights = rep(1 / J, J),
    prepared_inputs = clustered_inputs
  )

  covariance_njobs_weighted <- NA_real_
  noise_njobs_weighted <- NA_real_
  signal_vcov_njobs_weighted <- empty_signal_vcov()
  if ("njobs" %in% names(res1$firm_table) && "njobs" %in% names(res2$firm_table)) {
    njobs1 <- as.numeric(res1$firm_table$njobs[match(firm_ids, res1$firm_table$entity_id)])
    njobs2 <- as.numeric(res2$firm_table$njobs[match(firm_ids, res2$firm_table$entity_id)])

    if (!anyNA(njobs1) && !anyNA(njobs2) &&
        all(is.finite(njobs1)) && all(is.finite(njobs2)) &&
        all(njobs1 > 0) && all(njobs2 > 0) &&
        max(abs(njobs1 - njobs2)) < 1e-8) {
      firm_weights <- njobs1 / sum(njobs1)
      beta1_weighted <- as.numeric(res1$firm_table$estimate[match(firm_ids, res1$firm_table$entity_id)])
      beta2_weighted <- as.numeric(res2$firm_table$estimate[match(firm_ids, res2$firm_table$entity_id)])
      beta1_weighted <- beta1_weighted - sum(firm_weights * beta1_weighted)
      beta2_weighted <- beta2_weighted - sum(firm_weights * beta2_weighted)

      covariance_njobs_weighted <- sum(firm_weights * beta1_weighted * beta2_weighted)
      noise_njobs_weighted <-
        sum(firm_weights * Matrix::diag(Theta12)) -
        as.numeric(t(firm_weights) %*% Theta12 %*% firm_weights)
      signal_vcov_njobs_weighted <- compute_clustered_signal_vcov(
        res1 = res1,
        res2 = res2,
        common_cols = common_cols,
        weights = firm_weights,
        prepared_inputs = clustered_inputs
      )
    }
  }
  
  list(
    J = J,
    covariance = covariance,
    noise = noise,
    covariance_njobs_weighted = covariance_njobs_weighted,
    noise_njobs_weighted = noise_njobs_weighted,
    signal_vcov = signal_vcov,
    signal_vcov_njobs_weighted = signal_vcov_njobs_weighted,
    scalar_vhat_acceptance_max_abs_diff =
      if (is.null(attr(signal_vcov, "scalar_vhat_acceptance_max_abs_diff"))) {
        NA_real_
      } else {
        attr(signal_vcov, "scalar_vhat_acceptance_max_abs_diff")
      },
    scalar_vhat_njobs_weighted_acceptance_max_abs_diff =
      if (is.null(attr(signal_vcov_njobs_weighted, "scalar_vhat_acceptance_max_abs_diff"))) {
        NA_real_
      } else {
        attr(signal_vcov_njobs_weighted, "scalar_vhat_acceptance_max_abs_diff")
      },
    N1 = N1,
    N2 = N2,
    Ncommon = Ncommon
  )
}

pairwise_covariance_row <- function(lhs, rhs, subset, model, out) {
  data.frame(
    lhs = lhs,
    rhs = rhs,
    subset = subset,
    model = model,
    J = out$J,
    N1 = out$N1,
    N2 = out$N2,
    Ncommon = out$Ncommon,
    covariance = out$covariance,
    noise = out$noise,
    covariance_njobs_weighted = out$covariance_njobs_weighted,
    noise_njobs_weighted = out$noise_njobs_weighted,
    signal_vcov_11 = out$signal_vcov[1, 1],
    signal_vcov_12 = out$signal_vcov[1, 2],
    signal_vcov_13 = out$signal_vcov[1, 3],
    signal_vcov_22 = out$signal_vcov[2, 2],
    signal_vcov_23 = out$signal_vcov[2, 3],
    signal_vcov_33 = out$signal_vcov[3, 3],
    signal_vcov_njobs_weighted_11 = out$signal_vcov_njobs_weighted[1, 1],
    signal_vcov_njobs_weighted_12 = out$signal_vcov_njobs_weighted[1, 2],
    signal_vcov_njobs_weighted_13 = out$signal_vcov_njobs_weighted[1, 3],
    signal_vcov_njobs_weighted_22 = out$signal_vcov_njobs_weighted[2, 2],
    signal_vcov_njobs_weighted_23 = out$signal_vcov_njobs_weighted[2, 3],
    signal_vcov_njobs_weighted_33 = out$signal_vcov_njobs_weighted[3, 3],
    scalar_vhat_acceptance_max_abs_diff = out$scalar_vhat_acceptance_max_abs_diff,
    scalar_vhat_njobs_weighted_acceptance_max_abs_diff =
      out$scalar_vhat_njobs_weighted_acceptance_max_abs_diff,
    stringsAsFactors = FALSE
  )
}

write_covariance_sheet <- function(results, output_dir, sheet_name = "covariance", survey_vars) {
  # results: list(all=..., subset97=...)
  # output_dir: directory to write the parquet sheet into
  
  stopifnot(is.list(results), !is.null(results$all))
  
  models <- intersect(c("OL", "PL", "Borda", "OLS", "OLSC"), names(results$all))
  if (length(models) == 0) stop("write_covariance_sheet(): no recognized models found in results$all")
  
  # Helper: build within-level pairs
  make_pairs <- function(vars) {
    vars <- vars[!is.na(vars) & nzchar(vars)]
    vars <- unique(vars)
    if (length(vars) < 2) return(list())
    combn(vars, 2, simplify = FALSE)
  }
  
  pairs <- make_pairs(survey_vars)
  
  rows <- list()
  k <- 1L
  
  for (model in models) {
    
    # ---------- ALL ----------
    model_all <- results$all[[model]]
    if (!is.null(model_all)) {
      
      for (pair in pairs) {
        v1 <- pair[[1]]
        v2 <- pair[[2]]
        
        if (!is.null(model_all[[v1]]) && !is.null(model_all[[v2]])) {
          message("Covariance Calculation for model = ", model, ", subset = all, outcome1 = ", v1, ", outcome2 = ", v2)
          out <- compute_pairwise_cov_and_noise(model_all[[v1]], model_all[[v2]])
          rows[[k]] <- pairwise_covariance_row(v1, v2, "all", model, out)
          k <- k + 1L
        }
      }
    }
    
    # ---------- SUBSET97 ----------
    model_97 <- NULL
    if (!is.null(results$subset97) && !is.null(results$subset97[[model]])) {
      model_97 <- results$subset97[[model]]
    }
    
    if (!is.null(model_97)) {
      
      for (pair in pairs) {
        v1 <- pair[[1]]
        v2 <- pair[[2]]
        
        if (!is.null(model_97[[v1]]) && !is.null(model_97[[v2]])) {
          message("Covariance Calculation for model = ", model, ", subset = 97, outcome1 = ", v1, ", outcome2 = ", v2)
          out <- compute_pairwise_cov_and_noise(model_97[[v1]], model_97[[v2]])
          rows[[k]] <- pairwise_covariance_row(v1, v2, "subset97", model, out)
          k <- k + 1L
        }
      }
    }
  }
  
  # ---------- CROSS-MODEL: OLS x Borda on the same outcome, for the heatmap diagonal ----------
  if (all(c("OLS", "Borda") %in% models)) {

    for (level_name in c("all", "subset97")) {

      level_results <- results[[level_name]]
      if (is.null(level_results) || is.null(level_results[["OLS"]]) || is.null(level_results[["Borda"]])) next

      for (outcome_name in unique(survey_vars[!is.na(survey_vars) & nzchar(survey_vars)])) {

        if (!is.null(level_results[["OLS"]][[outcome_name]]) && !is.null(level_results[["Borda"]][[outcome_name]])) {
          message("Covariance Calculation for model = OLS_x_Borda, subset = ", sub("^subset", "", level_name), ", outcome = ", outcome_name)

          # The two aggregation methods must cover identical firm sets; the pairwise machinery silently intersects otherwise
          stopifnot(identical(
            sort(grep("^entity\\d+$", names(level_results[["OLS"]][[outcome_name]]$mats$S), value = TRUE)),
            sort(grep("^entity\\d+$", names(level_results[["Borda"]][[outcome_name]]$mats$S), value = TRUE))
          ))

          out <- compute_pairwise_cov_and_noise(level_results[["OLS"]][[outcome_name]], level_results[["Borda"]][[outcome_name]])
          rows[[k]] <- pairwise_covariance_row(
            outcome_name, outcome_name, level_name, "OLS_x_Borda", out
          )
          k <- k + 1L
        }
      }
    }
  }

  cov_df <- dplyr::bind_rows(rows)

  write_parquet_sheet(output_dir, sheet_name, cov_df)

  invisible(cov_df)
}
