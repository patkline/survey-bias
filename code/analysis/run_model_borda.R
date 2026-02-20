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
  
  # 2) Build centered firm scores + naive SE + robust SE + centered bread/score/cov
  #    Assumes your make_borda_bread_and_score() performs sum-to-zero centering internally
  out <- make_borda_bread_and_score(
    B_indiv,
    id_var   = "resp_id",
    firm_var = "firm_id",
    b_var    = "B",
    w_var    = "w"
  )
  
  firm_scores <- out$firm_scores  # firm_id, borda_score, se, rse (centered)
  Psi         <- out$score        # n_resp x J (centered), colnames firm<id>
  rcov_mat    <- out$cov          # J x J robust covariance (centered)
  bread_mat   <- out$bread        # J x J (centered), if you want it later
  
  # 3) Add firm names (optional)
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_scores <- firm_scores %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm), by = "firm_id")
  } else {
    firm_scores$firm <- NA_character_
  }
  
  # 4) EB step (computed from robust SE by default)
  eb_hat <- rep(NA_real_, nrow(firm_scores))
  if (isTRUE(do_eb) && exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_scores$borda_score) & is.finite(firm_scores$rse) & firm_scores$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(
        theta_hat = firm_scores$borda_score[ok],
        s         = pmax(firm_scores$rse[ok], 1e-8)
      )
      eb_hat[ok] <- eb_fit$theta_eb
    }
  }
  
  # 5) Naive covariance (diagonal) for "cov" slot; robust covariance for "rcov"
  #    (keeps your PL/OL convention: cov = model/naive, rcov = clustered/robust)
  firm_cols <- colnames(rcov_mat)  # should already be firm<id>
  cov_mat <- matrix(0, nrow = length(firm_cols), ncol = length(firm_cols),
                    dimnames = list(firm_cols, firm_cols))
  # align se to firm_cols order
  firm_ids_from_cols <- suppressWarnings(as.integer(sub("^firm", "", firm_cols)))
  se_vec <- firm_scores$se[match(firm_ids_from_cols, firm_scores$firm_id)]
  diag(cov_mat) <- se_vec^2
  
  # 6) Build standardized firm_table
  firm_table <- firm_scores %>%
    dplyr::transmute(
      firm_id,
      firm,
      estimate = borda_score,
      se       = se,
      rse      = rse,
      eb       = eb_hat
    ) %>%
    dplyr::arrange(firm_id)
  
  # 7) Store S as a data.frame with resp_id + firm<id> columns (like PL/OL style)
  S_df <- as.data.frame(Psi)
  S_df <- cbind(resp_id = rownames(Psi), S_df)
  rownames(S_df) <- NULL
  
  list(
    fit = NULL,  # Borda isn't fit via clm/PlackettLuce object; keep NULL for uniformity
    firm_table = firm_table,
    mats = list(
      S     = S_df,     # score-like contributions (centered)
      cov   = cov_mat,  # naive covariance (diag(se^2))
      rcov  = rcov_mat, # robust covariance (sandwich, centered)
      bread = bread_mat # optional but often useful
    ),
    extras = list(
      B_indiv = B_indiv
    )
  )
}
