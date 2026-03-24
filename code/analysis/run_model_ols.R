run_model_ols <- function(
    data_long,
    id_map,
    outcome,
    scale_and_center = FALSE,
    do_eb = TRUE,
    seed = 123
) {
  set.seed(seed)
  
  stopifnot("resp_id" %in% names(data_long))
  stopifnot("firm_id" %in% names(data_long))
  
  # Ensure weights exist (default 1)
  if (!("w" %in% names(data_long))) data_long$w <- 1
  
  # Ensure rating column exists
  if (!(outcome %in% names(data_long)) && !("rating" %in% names(data_long))) {
    stop("run_model_ols(): no column named outcome='", outcome, "' or 'rating' found in data_long.")
  }
  if (!("rating" %in% names(data_long))) data_long$rating <- data_long[[outcome]]
  
  # Flip so higher = "better" in your convention (assuming 1..5)
  data_long$rating <- 6 - data_long$rating
  
  if (isTRUE(scale_and_center)) {
    mu  <- mean(data_long$rating, na.rm = TRUE)
    sdv <- stats::sd(data_long$rating, na.rm = TRUE)
    if (!is.finite(sdv) || sdv <= 0) stop("run_model_ols(): SD is zero/NA; cannot scale.")
    data_long$rating <- (data_long$rating - mu) / sdv
  }
  
  # Mean-estimator machinery (centers internally)
  out <- mean_estimator_bread_and_score(
    data_long,
    id_var   = "resp_id",
    firm_var = "firm_id",
    b_var    = "rating",
    w_var    = "w"
  )
  
  firm_scores <- out$firm_scores  # firm_id, item_worth, se, rse
  Psi         <- out$score        # n_resp x J (centered), colnames firm<id>
  cov_mat     <- out$cov          # naive covariance (centered)
  rcov_mat    <- out$rcov         # robust covariance (centered)
  bread_mat   <- out$bread        # bread (centered)
  
  # Add firm names
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_scores <- firm_scores %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm, njobs), by = "firm_id")
  } else {
    firm_scores$firm <- NA_character_
  }
  
  # Align everything to firm_scores order
  firm_ids  <- as.integer(firm_scores$firm_id)
  firm_cols <- paste0("firm", firm_ids)
  entity_cols <- paste0("entity", firm_ids)
  
  miss <- setdiff(firm_cols, colnames(Psi))
  if (length(miss)) stop("run_model_ols(): score matrix missing firm cols: ", paste(miss, collapse = ", "))
  
  Psi_mat   <- as.matrix(Psi[, firm_cols, drop = FALSE])
  cov_mat   <- as.matrix(cov_mat[firm_cols, firm_cols, drop = FALSE])
  rcov_mat  <- as.matrix(rcov_mat[firm_cols, firm_cols, drop = FALSE])
  bread_mat <- as.matrix(bread_mat[firm_cols, firm_cols, drop = FALSE])
  
  # Rename mats to entity<id> naming
  colnames(Psi_mat) <- entity_cols
  dimnames(cov_mat)   <- list(entity_cols, entity_cols)
  dimnames(rcov_mat)  <- list(entity_cols, entity_cols)
  dimnames(bread_mat) <- list(entity_cols, entity_cols)
  
  # EB step (uses robust SE)
  eb_hat <- rep(NA_real_, nrow(firm_scores))
  if (isTRUE(do_eb) && exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_scores$item_worth) & is.finite(firm_scores$rse) & firm_scores$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_scores$item_worth[ok], s = pmax(firm_scores$rse[ok], 1e-8))
      eb_hat[ok] <- eb_fit$theta_eb
    }
  }
  
  # Build entity-style table
  entity_table <- firm_scores %>%
    dplyr::transmute(
      entity_type = "Firm",
      entity_id   = as.integer(firm_id),
      entity      = firm,
      njobs       = njobs,
      estimate    = as.numeric(item_worth),
      se          = as.numeric(se),
      rse         = as.numeric(rse),
      eb          = as.numeric(eb_hat)
    ) %>%
    dplyr::arrange(entity_id)
  
  # Score df: resp_id + entity<id>
  S_df <- as.data.frame(Psi_mat)
  S_df <- cbind(resp_id = rownames(Psi_mat), S_df)
  rownames(S_df) <- NULL
  
  list(
    fit = NULL,
    firm_table = entity_table,  # keep slot name for compat; contents are entity schema
    mats = list(
      S     = S_df,
      cov   = cov_mat,
      rcov  = rcov_mat,
      bread = bread_mat
    )
  )
}