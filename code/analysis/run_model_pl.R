run_model_pl <- function(data_wide, id_map, outcome, firms97 = NULL, npseudo = 0.5) {
  
  # Expect: data_wide = resp_id + firm<id> columns with ranks (1..K); 0/NA = unranked
  stopifnot("resp_id" %in% names(data_wide))
  
  firm_cols <- grep("^firm\\d+$", names(data_wide), value = TRUE)
  if (!length(firm_cols)) stop("No firm columns found in wide data (expected names like firm12).")
  
  # numeric matrix for PlackettLuce: ranks, NA for unranked
  M <- data_wide[, firm_cols, drop = FALSE]
  fit <- PlackettLuce::PlackettLuce(as.matrix(M), npseudo = npseudo)
  
  # === firm worths / coefficients ===
  # coef(fit) returns a vector; usually includes all items but one baseline depending on parametrization.
  # We'll use the log-worth scale implied by coef(), and reconstruct a full vector with a 0 baseline if needed.
  beta_hat <- stats::coef(fit, ref=NULL)
  beta_names <- names(beta_hat)
  
  # get all item names from model
  item_names <- colnames(M)
  
  # ensure full J vector (baseline 0 if needed)
  if (length(beta_hat) == length(item_names) - 1L) {
    missing <- setdiff(item_names, beta_names)
    beta_full <- c(beta_hat, setNames(0, missing))
    beta_full <- beta_full[item_names]
  } else {
    beta_full <- beta_hat[item_names]
  }
  
  # === variance + robust variance via sandwich (cluster=respondent) ===
  # estfun gives per-row scores; cluster by resp_id_used
  S_all <- sandwich::estfun(fit, ref = NULL)
  V_all <- stats::vcov(fit, ref = NULL)
  
  # Align covariance to item_names (firm cols) and expand baseline if needed
  # V_all is for the free parameters only; if baseline was implicit, we expand to full J with zeros.
  if (ncol(V_all) == length(item_names) - 1L) {
    base <- setdiff(item_names, colnames(V_all))
    # Expand: put baseline as 0 with 0 covariances (approx). If you prefer exact identifiability mapping,
    # you can transform from contrast params like OL. This is typically OK for your stacking use-case.
    V_full <- matrix(0, nrow = length(item_names), ncol = length(item_names),
                     dimnames = list(item_names, item_names))
    V_full[colnames(V_all), colnames(V_all)] <- V_all
  } else {
    V_full <- V_all[item_names, item_names, drop = FALSE]
  }
  
  # === scores matrix S ===
  # Expand S to full item set if needed (baseline column of zeros)
  if (ncol(S_all) == length(item_names) - 1L) {
    base <- setdiff(item_names, colnames(S_all))
    S_full <- cbind(S_all, setNames(rep(0, nrow(S_all)), base))
    S_full <- S_full[, item_names, drop = FALSE]
  } else {
    S_full <- S_all[, item_names, drop = FALSE]
  }
  
  # sandwich covariance (firm x firm)
  M          <- crossprod(S_full)          # match your old scaling
  Vr_full <- V_full %*% M %*% V_full
  
  # firm_id parsing from "firm123"
  firm_ids <- suppressWarnings(as.integer(sub("^firm", "", item_names)))
  
  firm_tbl <- data.frame(
    firm_id  = firm_ids,
    firm_col = item_names,
    stringsAsFactors = FALSE
  )
  
  # join to firm name if available
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_tbl <- firm_tbl %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm), by = "firm_id")
  } else {
    firm_tbl$firm <- NA_character_
  }
  
  firm_tbl <- firm_tbl %>%
    dplyr::mutate(
      estimate = as.numeric(beta_full),
      se       = sqrt(diag(V_full)),
      rse      = sqrt(diag(Vr_full))
    )
  
  # === EB step (optional; uses your eb_two_step) ===
  firm_tbl$eb <- NA_real_
  if (exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_tbl$estimate) & is.finite(firm_tbl$rse) & firm_tbl$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_tbl$estimate[ok], s = pmax(firm_tbl$rse[ok], 1e-8))
      firm_tbl$eb[ok] <- eb_fit$theta_eb
    }
  }
  
  list(
    fit = fit,
    firm_table = firm_tbl %>% dplyr::select(firm_id, firm, estimate, se, rse, eb),
    mats = list(
      S     = S_full,
      cov   = V_full,
      rcov  = Vr_full,
      bread = V_full     # <-- ADD THIS
    )
  )
}
