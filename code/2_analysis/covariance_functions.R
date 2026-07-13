# ------------------------------------------------------------------------------
# Purpose: Calculate Covariance and Noise
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
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

  S1_full <- as.matrix(S1_df[, common_cols, drop = FALSE])
  S2_full <- as.matrix(S2_df[, common_cols, drop = FALSE])

  J <- as.integer(ncol(S1_full))

  if (Ncommon > 0L) {
    ord_ids <- sort(overlap_ids)
    map1 <- match(ord_ids, id1)
    map2 <- match(ord_ids, id2)

    S1_ovl <- S1_full[map1, , drop = FALSE]
    S2_ovl <- S2_full[map2, , drop = FALSE]

    Theta12 <- crossprod(S1_ovl, S2_ovl)
  } else {
    Theta12 <- matrix(0, nrow = J, ncol = J)
  }
  
  noise <- if (J > 0L) sum(Matrix::diag(Theta12)) / J else NA_real_

  covariance_njobs_weighted <- NA_real_
  noise_njobs_weighted <- NA_real_
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
      noise_njobs_weighted <- sum(firm_weights * Matrix::diag(Theta12))
    }
  }
  
  list(
    J = J,
    covariance = covariance,
    noise = noise,
    covariance_njobs_weighted = covariance_njobs_weighted,
    noise_njobs_weighted = noise_njobs_weighted,
    N1 = N1,
    N2 = N2,
    Ncommon = Ncommon
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
          rows[[k]] <- data.frame(
            lhs = v1,
            rhs = v2,
            subset = "all",
            model = model,
            J = out$J,
            N1 = out$N1,
            N2 = out$N2,
            Ncommon = out$Ncommon,
            covariance = out$covariance,
            noise = out$noise,
            covariance_njobs_weighted = out$covariance_njobs_weighted,
            noise_njobs_weighted = out$noise_njobs_weighted,
            stringsAsFactors = FALSE
          )
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
          rows[[k]] <- data.frame(
            lhs = v1,
            rhs = v2,
            subset = "subset97",
            model = model,
            J = out$J,
            N1 = out$N1,
            N2 = out$N2,
            Ncommon = out$Ncommon,
            covariance = out$covariance,
            noise = out$noise,
            covariance_njobs_weighted = out$covariance_njobs_weighted,
            noise_njobs_weighted = out$noise_njobs_weighted,
            stringsAsFactors = FALSE
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
