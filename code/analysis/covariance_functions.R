# ------------------------------------------------------------------------------
# Purpose: Calculate Covariance and Noise
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
compute_pairwise_cov_and_noise <- function(res1, res2) {
  stopifnot(!is.null(res1$mats$S), !is.null(res2$mats$S))
  stopifnot(!is.null(res1$mats$bread), !is.null(res2$mats$bread))
  
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
  
  firm_cols1 <- names(S1_df)[grepl("^firm\\d+$", names(S1_df))]
  firm_cols2 <- names(S2_df)[grepl("^firm\\d+$", names(S2_df))]
  common_cols <- intersect(firm_cols1, firm_cols2)
  
  if (length(common_cols) < 2L) {
    return(list(
      J = as.integer(length(common_cols)),
      covariance = NA_real_,
      noise = NA_real_,
      N1 = N1,
      N2 = N2,
      Ncommon = Ncommon
    ))
  }
  
  common_cols <- sort(common_cols)
  
  B1 <- as.matrix(res1$mats$bread)
  B2 <- as.matrix(res2$mats$bread)
  if (is.null(dimnames(B1)) || is.null(dimnames(B2))) {
    stop("compute_pairwise_cov_and_noise(): bread matrices must have dimnames firm<id>.")
  }
  
  B1c <- B1[common_cols, common_cols, drop = FALSE]
  B2c <- B2[common_cols, common_cols, drop = FALSE]
  B_block <- Matrix::bdiag(B1c, B2c)
  
  firm_ids <- as.integer(sub("^firm", "", common_cols))
  beta1 <- res1$firm_table$estimate[match(firm_ids, res1$firm_table$firm_id)]
  beta2 <- res2$firm_table$estimate[match(firm_ids, res2$firm_table$firm_id)]
  covariance <- mean(beta1 * beta2, na.rm = TRUE)
  
  S1_full <- as.matrix(S1_df[, common_cols, drop = FALSE])
  S2_full <- as.matrix(S2_df[, common_cols, drop = FALSE])
  
  # overlap block
  if (Ncommon > 0L) {
    ord_ids <- sort(overlap_ids)
    map1 <- match(ord_ids, id1)
    map2 <- match(ord_ids, id2)
    S1_ovl <- S1_full[map1, , drop = FALSE]
    S2_ovl <- S2_full[map2, , drop = FALSE]
    M12 <- crossprod(S1_ovl, S2_ovl)
  } else {
    M12 <- matrix(0, nrow = ncol(S1_full), ncol = ncol(S2_full))
  }
  
  M11 <- crossprod(S1_full)
  M22 <- crossprod(S2_full)
  
  M_block <- rbind(
    cbind(M11,    M12),
    cbind(t(M12), M22)
  )
  
  Theta_block <- B_block %*% M_block %*% B_block
  
  J <- as.integer(ncol(B1c))
  Theta12 <- Theta_block[1:J, (J + 1):(2 * J), drop = FALSE]
  
  noise <- if (J > 0L) sum(Matrix::diag(Theta12)) / J else NA_real_
  
  list(
    J = J,
    covariance = covariance,
    noise = noise,
    N1 = N1,
    N2 = N2,
    Ncommon = Ncommon
  )
}
write_covariance_sheet <- function(results, wb, sheet_name = "covariance") {
  # results: list(all=..., subset97=...)
  # wb: openxlsx workbook
  
  stopifnot(is.list(results), !is.null(results$all))
  
  models <- intersect(c("OL", "PL", "Borda", "OLS", "OLSC"), names(results$all))
  
  # infer survey_vars from what’s inside results$all
  # (assumes each model list has the same outcome names)
  survey_vars <- unique(unlist(lapply(models, function(m) names(results$all[[m]]))))
  survey_vars <- survey_vars[!is.na(survey_vars) & nzchar(survey_vars)]
  
  pairs <- combn(survey_vars, 2, simplify = FALSE)
  
  rows <- list()
  k <- 1L
  
  for (model in models) {
    for (pair in pairs) {
      v1 <- pair[[1]]
      v2 <- pair[[2]]
      
      # ---------- ALL ----------
      if (!is.null(results$all[[model]][[v1]]) && !is.null(results$all[[model]][[v2]])) {
        out <- compute_pairwise_cov_and_noise(results$all[[model]][[v1]],
                                              results$all[[model]][[v2]])
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
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
      
      # ---------- SUBSET97 ----------
      if (!is.null(results$subset97) &&
          !is.null(results$subset97[[model]]) &&
          !is.null(results$subset97[[model]][[v1]]) &&
          !is.null(results$subset97[[model]][[v2]])) {
        
        out <- compute_pairwise_cov_and_noise(results$subset97[[model]][[v1]],
                                              results$subset97[[model]][[v2]])
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
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }
  
  cov_df <- dplyr::bind_rows(rows)
  
  remove_sheet_safely(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, cov_df)
  
  invisible(cov_df)
}