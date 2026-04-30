run_model_borda <- function(
    data_wide,
    id_map,
    outcome,
    higher_is_better = FALSE,
    normalize = TRUE,
    ref_firm_ids = NULL,
    do_eb = TRUE,
    seed = 123
) {
  set.seed(seed)
  
  stopifnot("resp_id" %in% names(data_wide))
  
  # 1) Individual-level Borda (long: resp_id x firm_id with B)
  B_indiv <- compute_borda_individual_wide(
    data_wide        = data_wide,
    id_map           = id_map,
    id_var           = "resp_id",
    higher_is_better = higher_is_better,
    normalize        = normalize,
    ref_firm_ids     = ref_firm_ids
  )
  
  # Ensure weights exist (default 1)
  if (!("w" %in% names(B_indiv))) B_indiv$w <- 1
  
  # 2) Mean-estimator machinery (centers internally)
  out <- mean_estimator_bread_and_score(
    B_indiv,
    id_var   = "resp_id",
    firm_var = "firm_id",
    b_var    = "B",
    w_var    = "w"
  )
  
  firm_scores <- out$firm_scores  # firm_id, item_worth, se, rse (centered)
  Psi         <- out$score        # n_resp x J (centered), colnames firm<id>
  bread_mat   <- out$bread        # J x J (centered)
  cov_mat     <- out$cov          # naive covariance (centered)
  rcov_mat    <- out$rcov         # robust covariance (centered)
  
  # 3) Add firm names (optional)
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_scores <- firm_scores %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm, njobs), by = "firm_id")
  } else {
    firm_scores$firm <- NA_character_
  }
  
  # Align to firm_scores ordering
  firm_ids   <- as.integer(firm_scores$firm_id)
  firm_cols  <- paste0("firm", firm_ids)
  entity_cols <- paste0("entity", firm_ids)
  
  # Guard: ensure all needed cols exist in mats
  miss_score <- setdiff(firm_cols, colnames(Psi))
  if (length(miss_score)) stop("run_model_borda(): score matrix missing cols: ", paste(miss_score, collapse = ", "))
  
  if (is.null(dimnames(bread_mat)) || is.null(dimnames(cov_mat)) || is.null(dimnames(rcov_mat))) {
    stop("run_model_borda(): bread/cov/rcov must have dimnames like firm<id>.")
  }
  miss_bread <- setdiff(firm_cols, colnames(bread_mat))
  if (length(miss_bread)) stop("run_model_borda(): bread missing cols: ", paste(miss_bread, collapse = ", "))
  miss_cov <- setdiff(firm_cols, colnames(cov_mat))
  if (length(miss_cov)) stop("run_model_borda(): cov missing cols: ", paste(miss_cov, collapse = ", "))
  miss_rcov <- setdiff(firm_cols, colnames(rcov_mat))
  if (length(miss_rcov)) stop("run_model_borda(): rcov missing cols: ", paste(miss_rcov, collapse = ", "))
  
  # Subset/reorder to firm_scores order
  Psi_mat   <- as.matrix(Psi[, firm_cols, drop = FALSE])
  bread_mat <- as.matrix(bread_mat[firm_cols, firm_cols, drop = FALSE])
  cov_mat   <- as.matrix(cov_mat  [firm_cols, firm_cols, drop = FALSE])
  rcov_mat  <- as.matrix(rcov_mat [firm_cols, firm_cols, drop = FALSE])
  
  # Rename mats to entity<id>
  colnames(Psi_mat)     <- entity_cols
  dimnames(bread_mat)   <- list(entity_cols, entity_cols)
  dimnames(cov_mat)     <- list(entity_cols, entity_cols)
  dimnames(rcov_mat)    <- list(entity_cols, entity_cols)
  
  # 4) EB step (use robust SE)
  eb_hat <- rep(NA_real_, nrow(firm_scores))
  if (isTRUE(do_eb) && exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_scores$item_worth) & is.finite(firm_scores$rse) & firm_scores$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_scores$item_worth[ok], s = pmax(firm_scores$rse[ok], 1e-8))
      eb_hat[ok] <- eb_fit$theta_eb
    }
  }
  
  # 5) Entity table
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
  
  # 6) Score df: resp_id + entity<id>
  S_df <- as.data.frame(Psi_mat)
  S_df <- cbind(resp_id = rownames(Psi_mat), S_df)
  rownames(S_df) <- NULL
  
  list(
    fit = NULL,
    firm_table = entity_table,   # slot name kept for compat; schema is entity_*
    mats = list(
      S     = S_df,
      bread = bread_mat,
      cov   = cov_mat,           # naive
      rcov  = rcov_mat           # robust
    ),
    extras = list(B_indiv = B_indiv)
  )
}