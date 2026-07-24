# ------------------------------------------------------------------------------
# Purpose: Calculate Borda Scores
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
# ------------------------------------------------------------
# Individual-level Borda scores from WIDE rankings (with id_map)
# - data_wide: columns "resp_id", "firm1"..."firmN"; values 0 (not ranked) or 1..K
# - id_map: data.frame with columns firm_id (integer) and firm (character)
# - id_var: respondent id column name
# - higher_is_better: if FALSE (default), lower ranks are better (1 best)
# - normalize: if TRUE, divides by (# alternatives ranked - 1) per respondent
# Returns: tibble(resp_id, firm_id, firm, B, e) with one row per
# (respondent, ranked firm), where e is the number of eligible opponents used
# to normalize B. Reference firms use all other observed firms; non-reference
# firms use only other observed non-reference firms.
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
  
  N <- dim(X)[[2]] - length(ref_firm_ids)
  stopifnot(
    "Expected 161 non-reference firms for the Borda self-tie correction" =
      N == 161L
  )
  
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
    # Reference firms are compared with every other observed firm
    e_out <- rep(k - 1L, k)
    # By default retain all observed firms; the non-reference fallback below
    # may remove non-reference firms that have no eligible opponent.
    keep_idx <- seq_len(k)
    
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
        B_nr <- 1/(2*N) + (N-1)/N * (B_nr / (k_nonref - 1))
        #B_nr <- (B_nr / (k_nonref - 1))
      }
      
      # Overwrite B_out *only* for non-reference firms
      B_out[idx_nonref] <- B_nr
      e_out[idx_nonref] <- k_nonref - 1L
    } else {
      # A non-reference firm with no other non-reference opponent cannot be
      # scored on the intended scale. Retain any reference firm, whose full
      # Borda score remains well-defined, and drop the non-reference firm(s).
      keep_idx <- which(is_ref_idx)
    }
    if (length(keep_idx) == 0L) next
    
    out_list[[i]] <- tibble::tibble(
      !!id_var := data_wide[[id_var]][i],
      firm_id   = f_ids_i[keep_idx],
      B         = as.numeric(B_out[keep_idx]),
      e         = as.numeric(e_out[keep_idx])
    )
  }
  
  out <- dplyr::bind_rows(out_list)
  out <- out %>%
    dplyr::left_join(id_map %>% dplyr::select(firm_id, firm), by = "firm_id")
  
  out
}
