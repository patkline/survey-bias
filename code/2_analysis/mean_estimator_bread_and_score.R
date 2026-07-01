# ----------------------------------------------------------------------------------------
# Purpose: Aggregate individual respondents' firm scores into firm-level 
# estimates --- for each firm and score variable compute,
#   - weighted-mean score across respondents
#   - the naive and robust standard error of the weighted-mean score 
#   - the across-firm covariance matrix of the weighted-mean score estimates
#
# Created: Jordan Cammarota 2026-03-06
# Edited: Nico Rotundo 2026-06-30
# ----------------------------------------------------------------------------------------
mean_estimator_bread_and_score <- function(
  # Long data frame, one row per respondent x firm scored
  respondent_firm_scores,

  # Name of the respondent identifier variable in respondent_firm_scores
  respondent_id_variable_name = "resp_id",

  # Name of the firm identifier variable in respondent_firm_scores
  firm_id_variable_name = "firm_id",

  # Name of the score variable in respondent_firm_scores
  score_variable_name = "B"
) {
  ## Validate inputs and fix the respondent and firm ordering
  # Require the identifier and score variables to be present
  stopifnot(all(c(respondent_id_variable_name, firm_id_variable_name, score_variable_name) %in% names(respondent_firm_scores)))

  # Distinct respondent and firm ids, sorted ascending, fixing the row and column order of every matrix built below
  respondent_ids <- sort(unique(respondent_firm_scores[[respondent_id_variable_name]]))
  firm_ids <- sort(unique(respondent_firm_scores[[firm_id_variable_name]]))
  
  # ---- 1) firm-level mean + naive SE of the mean ----
  firm_base <- respondent_firm_scores |>
    dplyr::group_by(.data[[firm_id_variable_name]]) |>
    dplyr::summarise(
      number_of_respondents = dplyr::n(),
      item_worth = sum(.data[[score_variable_name]], na.rm = TRUE) / number_of_respondents,
      .groups = "drop"
    ) |>
    dplyr::rename(firm_id = !!firm_id_variable_name) |>
    dplyr::arrange(firm_id)
  
  firm_means <- respondent_firm_scores |>
    dplyr::rename(firm_id = !!firm_id_variable_name) |>
    dplyr::left_join(firm_base[, c("firm_id", "item_worth", "number_of_respondents")], by = "firm_id") |>
    dplyr::group_by(firm_id) |>
    dplyr::summarise(
      number_of_respondents = dplyr::first(number_of_respondents),
      item_worth = dplyr::first(item_worth),
      residual_sum_of_squares = sum((.data[[score_variable_name]] - item_worth)^2, na.rm = TRUE),
      sigma2_hat = dplyr::if_else(number_of_respondents > 1, residual_sum_of_squares / (number_of_respondents - 1), NA_real_),
      se = dplyr::if_else(
        number_of_respondents > 0,
        sqrt(sigma2_hat / number_of_respondents),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(firm_id)
  
  mu_hat <- firm_means$item_worth
  names(mu_hat) <- firm_means$firm_id
  
  # indices for matrix filling
  i_idx <- match(respondent_firm_scores[[respondent_id_variable_name]], respondent_ids)
  j_idx <- match(respondent_firm_scores[[firm_id_variable_name]], firm_ids)
  
  n <- length(respondent_ids)
  J <- length(firm_ids)
  firm_cols <- paste0("firm", firm_ids)
  
  # ---- 2) W matrix: respondent x firm scoring indicator ----
  W <- matrix(0, nrow = n, ncol = J, dimnames = list(respondent_ids, firm_cols))
  W[cbind(i_idx, j_idx)] <- 1
  
  # ---- 3) Bread (diagonal) in the raw parameterization ----
  w_colsum <- colSums(W)
  bread_raw <- diag(ifelse(w_colsum > 0, 1 / w_colsum, NA_real_))
  dimnames(bread_raw) <- list(firm_cols, firm_cols)
  
  # ---- 4) Score matrix Psi (raw) ----
  Psi_raw <- matrix(0, nrow = n, ncol = J, dimnames = list(respondent_ids, firm_cols))
  mu_vec <- mu_hat[as.character(firm_ids)]              # align to firm_ids
  resid  <- respondent_firm_scores[[score_variable_name]] - mu_vec[j_idx]
  Psi_raw[cbind(i_idx, j_idx)] <- resid
  
  
  
  # ---- Robust covariance in raw parameterization ----
  meat_raw <- crossprod(Psi_raw)
  rcov_raw  <- bread_raw %*% meat_raw %*% bread_raw
  dimnames(rcov_raw) <- list(firm_cols, firm_cols)

  # Influence-function matrix. Note that crossprod(IF_raw) = rcov_raw.
  IF_raw <- Psi_raw %*% bread_raw
  rownames(IF_raw) <- respondent_ids
  colnames(IF_raw) <- firm_cols
  
  # Naive covariance
  cov_raw <- diag(firm_means$se^2)
  
  # ==========================================================
  # 5) RECENTER INSIDE: apply C = I - 11'/J to everything
  # ==========================================================
  rec <- recenter_objects(
    beta   = mu_vec,               # length J, in firm_ids order
    S_full = IF_raw,
    cov = cov_raw,
    rcov = rcov_raw
  )
  
  beta_c <- as.numeric(rec$beta)
  names(beta_c) <- firm_cols
  
  cov_c <- as.matrix(rec$cov)
  dimnames(cov_c) <- list(firm_cols, firm_cols)

  # Robut covariance after recentering
  rcov_c <- as.matrix(rec$rcov)
  dimnames(rcov_c) <- list(firm_cols, firm_cols)
  
  # Recentered influence-function matrix
  IF_c <- as.matrix(rec$S)
  rownames(IF_c) <- respondent_ids
  colnames(IF_c) <- firm_cols

  # Sanity check: recentered IF should have crossprod equal to recentered robust covariance
  stopifnot(isTRUE(all.equal(crossprod(IF_c), rcov_c, tolerance = 1e-10)))
  
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
    score = IF_c,              # centered influence function matrix
    cov   = cov_c,             # centered naive covariance
    rcov  = rcov_c             # centered robust covariance
  )
}
