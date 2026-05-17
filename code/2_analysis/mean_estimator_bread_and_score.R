# ------------------------------------------------------------------------------
# Purpose: Calculate Bread and Score for mean based estimator
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
mean_estimator_bread_and_score <- function(
    B_indiv,
    id_var   = "resp_id",
    firm_var = "firm_id",
    b_var    = "B",
    w_var    = "w"
) {
  stopifnot(all(c(id_var, firm_var, b_var) %in% names(B_indiv)))
  if (!(w_var %in% names(B_indiv))) B_indiv[[w_var]] <- 1
  
  if (!exists("recenter_objects", mode = "function")) {
    stop("recenter_objects() not found in scope. Source it before calling make_borda_bread_and_score().")
  }
  
  # stable ordering
  resp_ids <- sort(unique(B_indiv[[id_var]]))
  firm_ids <- sort(unique(B_indiv[[firm_var]]))
  
  # ---- 1) firm-level weighted mean + naive SE of weighted mean ----
  firm_base <- B_indiv |>
    dplyr::group_by(.data[[firm_var]]) |>
    dplyr::summarise(
      w_sum  = sum(.data[[w_var]], na.rm = TRUE),
      w2_sum = sum((.data[[w_var]]^2), na.rm = TRUE),
      item_worth = sum(.data[[b_var]] * .data[[w_var]], na.rm = TRUE) / w_sum,
      .groups = "drop"
    ) |>
    dplyr::rename(firm_id = !!firm_var) |>
    dplyr::arrange(firm_id)
  
  firm_means <- B_indiv |>
    dplyr::rename(firm_id = !!firm_var) |>
    dplyr::left_join(firm_base[, c("firm_id", "item_worth", "w_sum", "w2_sum")], by = "firm_id") |>
    dplyr::group_by(firm_id) |>
    dplyr::summarise(
      w_sum      = dplyr::first(w_sum),
      w2_sum     = dplyr::first(w2_sum),
      item_worth = dplyr::first(item_worth),
      wr2_sum    = sum(.data[[w_var]] * (.data[[b_var]] - item_worth)^2, na.rm = TRUE),
      sigma2_hat = dplyr::if_else(w_sum > 1, wr2_sum / (w_sum - 1), NA_real_),
      se = dplyr::if_else(
        w_sum > 0,
        sqrt(sigma2_hat * (w2_sum / (w_sum^2))),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(firm_id)
  
  mu_hat <- firm_means$item_worth
  names(mu_hat) <- firm_means$firm_id
  
  # indices for matrix filling
  i_idx <- match(B_indiv[[id_var]], resp_ids)
  j_idx <- match(B_indiv[[firm_var]], firm_ids)
  
  n <- length(resp_ids)
  J <- length(firm_ids)
  firm_cols <- paste0("firm", firm_ids)
  
  # ---- 2) W matrix: respondent x firm weights ----
  W <- matrix(0, nrow = n, ncol = J, dimnames = list(resp_ids, firm_cols))
  W[cbind(i_idx, j_idx)] <- B_indiv[[w_var]]
  
  # ---- 3) Bread (diagonal) in the raw parameterization ----
  w_colsum <- colSums(W)
  bread_raw <- diag(ifelse(w_colsum > 0, 1 / w_colsum, NA_real_))
  dimnames(bread_raw) <- list(firm_cols, firm_cols)
  
  # ---- 4) Score matrix Psi (raw) ----
  Psi_raw <- matrix(0, nrow = n, ncol = J, dimnames = list(resp_ids, firm_cols))
  mu_vec <- mu_hat[as.character(firm_ids)]              # align to firm_ids
  resid  <- B_indiv[[b_var]] - mu_vec[j_idx]
  Psi_raw[cbind(i_idx, j_idx)] <- B_indiv[[w_var]] * resid
  
  
  
  # ---- Robust covariance in raw parameterization ----
  meat_raw <- crossprod(Psi_raw)
  rcov_raw  <- bread_raw %*% meat_raw %*% bread_raw
  dimnames(rcov_raw) <- list(firm_cols, firm_cols)
  
  cov_raw <- diag(firm_means$se^2)
  
  # ==========================================================
  # 5) RECENTER INSIDE: apply C = I - 11'/J to everything
  # ==========================================================
  rec <- recenter_objects(
    beta   = mu_vec,               # length J, in firm_ids order
    Binv   = bread_raw,
    S_full = Psi_raw,
    cov = cov_raw
  )
  
  beta_c <- as.numeric(rec$beta)
  names(beta_c) <- firm_cols
  
  cov_c <- as.matrix(rec$cov)
  dimnames(cov_c) <- list(firm_cols, firm_cols)
  
  Psi_c <- as.matrix(rec$S)
  colnames(Psi_c) <- firm_cols
  rownames(Psi_c) <- resp_ids
  
  B_c <- rec$Binv
  dimnames(B_c) <- list(firm_cols, firm_cols)
  
  # Robust 
  rcov_c <- B_c %*% crossprod(Psi_c) %*% B_c
  
  # After recentering, robust SE should come from recentered cov
  se_vec_c <- sqrt(diag(cov_c))
  rse_vec_c <- sqrt(diag(rcov_c))
  
  
  firm_scores_df <- data.frame(
    firm_id     = firm_ids,
    item_worth  = beta_c,
    se          = se_vec_c,
    rse         = rse_vec_c,
    stringsAsFactors = FALSE
  )
  
  list(
    firm_scores = firm_scores_df,  # centered
    W     = W,
    bread = B_c,               # <-- NOW has row/col names
    score = Psi_c,                 # centered score
    cov   = cov_c,                  # centered robust covariance
    rcov  = rcov_c
  )
}



