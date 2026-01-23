library(dplyr)

# ------------------------------------------------------------
# Individual-level Borda scores from WIDE rankings (with id_map)
# - data_wide: columns "resp_id", "firm1"..."firmN"; values 0 (not ranked) or 1..K
# - id_map: data.frame with columns firm_id (integer) and firm (character)
# - id_var: respondent id column name
# - higher_is_better: if FALSE (default), lower ranks are better (1 best)
# - normalize: if TRUE, divides by (# alternatives ranked - 1) per respondent
# Returns: tibble(resp_id, firm_id, firm, B) with one row per (respondent, ranked firm)
# ------------------------------------------------------------
compute_borda_individual_wide <- function(data_wide,
                                          id_map,
                                          id_var = "resp_id",
                                          higher_is_better = FALSE,
                                          normalize = TRUE,
                                          ref_firm_ids = NULL) {
  stopifnot(id_var %in% names(data_wide))
  stopifnot(all(c("firm_id", "firm") %in% names(id_map)))
  
  if (is.null(ref_firm_ids)) {
    stop("Please supply ref_firm_ids = c(...) with the firm_id values for reference firms.")
  }
  # sanity check: all ref_firm_ids must exist in id_map
  if (!all(ref_firm_ids %in% id_map$firm_id)) {
    miss <- setdiff(ref_firm_ids, id_map$firm_id)
    stop("ref_firm_ids not found in id_map$firm_id: ", paste(miss, collapse = ", "))
  }
  
  # firm columns like "firm23" (numeric IDs in names)
  firm_cols <- setdiff(names(data_wide), id_var)
  firm_ids  <- suppressWarnings(as.integer(sub("^firm", "", firm_cols)))
  if (anyNA(firm_ids)) {
    stop("All firm columns must be named like 'firm##' (e.g., firm1, firm2, ...).")
  }
  if (!all(firm_ids %in% id_map$firm_id)) {
    miss <- setdiff(firm_ids, id_map$firm_id)
    stop("id_map is missing firm_id(s): ", paste(miss, collapse = ", "))
  }
  
  # numeric matrix of ranks; 0/negatives -> NA
  X <- data_wide[, firm_cols, drop = FALSE]
  X[] <- lapply(X, function(x) suppressWarnings(as.numeric(x)))
  M <- as.matrix(X)
  M[M <= 0] <- NA_real_
  
  out_list <- vector("list", nrow(M))
  
  for (i in seq_len(nrow(M))) {
    r   <- M[i, ]
    idx <- which(is.finite(r))
    k   <- length(idx)
    if (k < 2) next  # need at least 2 alternatives to create pairwise comps
    
    v        <- r[idx]            # observed ranks for this respondent
    f_ids_i  <- firm_ids[idx]     # firm_ids for this row in the same order
    
    # Reference indicator for the firms in this respondent's set
    is_ref_idx <- f_ids_i %in% ref_firm_ids
    
    # --- 1. Full Borda over all firms (current behavior) ---
    if (higher_is_better) {
      wins_mat_full <- outer(v, v, ">")
    } else {
      wins_mat_full <- outer(v, v, "<")
    }
    ties_mat_full <- outer(v, v, "==")
    diag(wins_mat_full) <- FALSE
    diag(ties_mat_full) <- FALSE
    
    wins_full <- rowSums(wins_mat_full)
    ties_full <- rowSums(ties_mat_full)
    
    B_full <- wins_full + 0.5 * ties_full
    if (normalize) {
      B_full <- B_full / (k - 1)
    }
    
    # Start with full Borda scores
    B_out <- B_full
    
    # --- 2. Recompute Borda for non-reference firms only,
    #        using comparisons among non-reference firms ---
    idx_nonref <- which(!is_ref_idx)
    k_nonref   <- length(idx_nonref)
    
    if (k_nonref >= 2) {
      v_nonref <- v[idx_nonref]
      
      if (higher_is_better) {
        wins_mat_nr <- outer(v_nonref, v_nonref, ">")
      } else {
        wins_mat_nr <- outer(v_nonref, v_nonref, "<")
      }
      ties_mat_nr <- outer(v_nonref, v_nonref, "==")
      diag(wins_mat_nr) <- FALSE
      diag(ties_mat_nr) <- FALSE
      
      wins_nr <- rowSums(wins_mat_nr)
      ties_nr <- rowSums(ties_mat_nr)
      
      B_nr <- wins_nr + 0.5 * ties_nr
      if (normalize) {
        B_nr <- B_nr / (k_nonref - 1)
      }
      
      # Overwrite B_out *only* for non-reference firms
      B_out[idx_nonref] <- B_nr
    }
    # If k_nonref < 2, we leave B_out as B_full for all firms in the set
    
    out_list[[i]] <- tibble::tibble(
      !!id_var := data_wide[[id_var]][i],
      firm_id   = f_ids_i,
      B         = as.numeric(B_out)
    )
  }
  
  out <- dplyr::bind_rows(out_list)
  out <- out %>%
    dplyr::left_join(id_map %>% dplyr::select(firm_id, firm), by = "firm_id")
  
  out
}


# ------------------------------------------------------------
# Summarize Borda by firm: mean, N, SE = sd/sqrt(N)
# ------------------------------------------------------------
summarize_borda_by_firm <- function(B_indiv, weights = NULL) {
  if (is.null(weights)) {
    B_indiv$w <- 1
  } else {
    B_indiv$w <- weights
  }

  B_indiv %>%
    dplyr::group_by(firm_id, firm) %>%
    dplyr::summarise(
      n_obs  = dplyr::n(),                                  # explicit sample size
      mean_B = sum(B * w) / sum(w),
      var_B  = sum(w * (B - mean_B)^2) / (sum(w) * (n_obs - 1)),
      se_B   = sqrt(var_B),
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------
# Sandwich-style robust covariance for firm-level Borda means
#
# Inputs:
#   B_indiv   : data.frame with columns: id_var (e.g. resp_id), firm_id, B,
#               and optionally a weight column (weight_var)
#   firm_order: integer vector of firm_ids in the order you want Sigma_B
#   id_var    : respondent id column (default "resp_id")
#   weight_var: optional column name giving respondent-level weights.
#               If NULL, treats all weights as 1 (unweighted).
#
# Returns:
#   Sigma_B: J x J matrix, robust ("sandwich") covariance of firm means
#------------------------------------------------------------------------------
compute_borda_sandwich_cov <- function(B_indiv,
                                       firm_order = NULL,
                                       id_var = "resp_id",
                                       weight_var = NULL) {
  stopifnot(all(c(id_var, "firm_id", "B") %in% names(B_indiv)))
  
  # weights: default to 1 if not provided
  if (!is.null(weight_var)) {
    stopifnot(weight_var %in% names(B_indiv))
    B_indiv <- B_indiv %>%
      dplyr::mutate(.w = pmax(as.numeric(.data[[weight_var]]), 0))
  } else {
    B_indiv <- B_indiv %>%
      dplyr::mutate(.w = 1)
  }
  
  # Per-firm weighted counts and means
  stats_firm <- B_indiv %>%
    dplyr::group_by(firm_id) %>%
    dplyr::summarise(
      n_j = sum(.w),
      mu  = ifelse(n_j > 0, sum(.w * B) / n_j, NA_real_),
      .groups = "drop"
    )
  
  # Firm order: either provided or sorted
  if (is.null(firm_order)) {
    firm_order <- sort(unique(stats_firm$firm_id))
  }
  
  # Ensure stats_firm is in the desired order
  stats_firm <- stats_firm[match(firm_order, stats_firm$firm_id), , drop = FALSE]
  
  # Center B by firm mean and build S matrix (rows = respondents, cols = firms)
  B_cent <- B_indiv %>%
    dplyr::mutate(
      Bc = B - stats_firm$mu[match(firm_id, stats_firm$firm_id)]
    ) %>%
    dplyr::select(!!rlang::sym(id_var), firm_id, Bc, .w)
  
  # Wide by respondent, one row per respondent, columns are firm_ids
  wide <- tidyr::pivot_wider(
    B_cent,
    names_from  = firm_id,
    values_from = Bc,
    values_fill = list(Bc = 0)
  )
  
  # Ensure all firm columns present in the specified order
  miss_cols <- setdiff(as.character(firm_order), names(wide))
  for (mc in miss_cols) wide[[mc]] <- 0
  
  wide <- wide[, c(id_var, as.character(firm_order)), drop = FALSE]
  
  S_raw <- as.matrix(wide[, -1, drop = FALSE])  # rows: respondents, cols: firms
  
  # respondent-level weights (one per id)
  w_resp <- B_cent %>%
    dplyr::distinct(!!rlang::sym(id_var), .w) %>%
    dplyr::arrange(!!rlang::sym(id_var)) %>%
    dplyr::pull(.w)
  
  # align w_resp to row order of 'wide'
  id_order <- wide[[id_var]]
  w_resp   <- w_resp[match(id_order,
                           B_cent %>% dplyr::distinct(!!rlang::sym(id_var)) %>% dplyr::pull(!!rlang::sym(id_var)))]
  
  w_sqrt <- sqrt(pmax(w_resp, 0))
  W_mat  <- matrix(w_sqrt, nrow = nrow(S_raw), ncol = ncol(S_raw))
  S_w    <- S_raw * W_mat
  
  # Meat: sum of S_i S_i' over respondents, with weights
  M <- crossprod(S_w)   # J x J
  
  # Bread inverse: diag(1 / n_j)
  J <- nrow(M)
  G_inv <- diag(1 / stats_firm$n_j, nrow = J, ncol = J)
  
  # Sandwich covariance for firm-level means
  Sigma_B <- G_inv %*% M %*% G_inv
  
  Sigma_B
}





