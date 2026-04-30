# ------------------------------------------------------------------------------
# Add industry-mean outcomes (equal-weighted + weighted) and demeaned outcomes
#
# Creates FOUR NEW outcomes per original outcome:
#   outcome_im    : industry means (equal weights)
#   outcome_dm    : firm deviations from equal-weight industry means
#   outcome_im_w  : industry means (weights from firm_table[[weight_col]])
#   outcome_dm_w  : firm deviations from weighted industry means
#
# Notes:
# - Uses ONLY Firm entities as inputs.
# - Output mats are standardized to entity<id> column names.
# - S outputs keep ONLY resp_id as an id column.
# - Weighted versions are ONLY computed if weight_col is provided AND fully populated
#   (no NA / non-finite). If require_positive_weights=TRUE, also requires all weights > 0.
# ------------------------------------------------------------------------------

add_industry_means_to_results <- function(
    results,
    industry_map,
    outcomes,
    model_names = NULL,
    which_sets = c("all", "subset97"),
    industry_col = "aer_naics2",
    suffix_dm = "_dm",
    suffix_im = "_im",
    suffix_dm_w = "_dm_w",
    suffix_im_w = "_im_w",
    weight_col = NULL,               # e.g. "njobs"
    require_positive_weights = TRUE  # if TRUE, skip weighted versions unless all w > 0
) {
  stopifnot(is.list(results))
  stopifnot("firm_id" %in% names(industry_map))
  stopifnot(industry_col %in% names(industry_map))
  
  # --- helpers from your entity-aware stack (must exist in scope) ---
  if (!exists(".coerce_entity_table", mode = "function")) stop(".coerce_entity_table() not found in scope.")
  if (!exists(".resolve_entity_cols", mode = "function")) stop(".resolve_entity_cols() not found in scope.")
  if (!exists(".make_entity_cols", mode = "function")) stop(".make_entity_cols() not found in scope.")
  
  # which models to loop over
  if (is.null(model_names)) {
    model_names <- intersect(
      c("OL", "PL", "Borda", "OLS", "OLSC"),
      unique(c(names(results$all), names(results$subset97)))
    )
  }
  
  # --- build equal-weight GAM ---
  build_GAM_equal <- function(firm_ids, industry_ids) {
    J <- length(firm_ids)
    inds <- sort(unique(industry_ids))
    K <- length(inds)
    
    G <- matrix(0, nrow = J, ncol = K)
    colnames(G) <- as.character(inds)
    for (j in seq_len(J)) {
      k <- match(industry_ids[j], inds)
      G[j, k] <- 1
    }
    
    GtG <- crossprod(G)       # KxK
    A   <- solve(GtG, t(G))   # KxJ
    M   <- diag(J) - G %*% A  # JxJ
    
    list(G = G, A = A, M = M, inds = inds)
  }
  
  # --- build weighted GAM ---
  # A_w = (G' W G)^{-1} G' W   ;  M_w = I - G A_w
  build_GAM_weighted <- function(firm_ids, industry_ids, w_vec) {
    J <- length(firm_ids)
    inds <- sort(unique(industry_ids))
    K <- length(inds)
    
    G <- matrix(0, nrow = J, ncol = K)
    colnames(G) <- as.character(inds)
    for (j in seq_len(J)) {
      k <- match(industry_ids[j], inds)
      G[j, k] <- 1
    }
    
    w_vec <- as.numeric(w_vec)
    W <- diag(w_vec, nrow = J, ncol = J)
    
    GtWG <- t(G) %*% W %*% G  # KxK
    
    # If an industry has 0 total weight, GtWG will be singular.
    # Add tiny ridge for safety (should not happen if weights are all >0).
    if (any(diag(GtWG) == 0)) {
      GtWG <- GtWG + diag(1e-12, nrow(GtWG))
    }
    
    A_w <- solve(GtWG, t(G) %*% W)  # KxJ
    M_w <- diag(J) - G %*% A_w      # JxJ
    
    list(G = G, A = A_w, M = M_w, inds = inds, w = w_vec)
  }
  
  # pull matrix columns from S given ids; S may use entity<id> or firm<id>
  pull_S_matrix <- function(S_df, ids) {
    stopifnot("resp_id" %in% names(S_df))
    s_cols <- .resolve_entity_cols(ids, names(S_df))
    as.matrix(S_df[, s_cols, drop = FALSE])
  }
  
  # pull square matrix given ids; matrices may use entity<id> or firm<id>
  pull_square <- function(M, ids) {
    M <- as.matrix(M)
    if (is.null(dimnames(M)) || is.null(rownames(M)) || is.null(colnames(M))) {
      stop("Expected square matrix with dimnames.")
    }
    cols <- .resolve_entity_cols(ids, colnames(M))
    M[cols, cols, drop = FALSE]
  }
  
  # transform one res by linear map T
  transform_result <- function(
    res,
    firm_ft, firm_ids,
    T,
    out_entity_type, out_entity_ids, out_entity_names,
    out_weight = NULL
  ) {
    beta <- as.numeric(firm_ft$estimate[match(firm_ids, firm_ft$entity_id)])
    
    bread_in <- pull_square(res$mats$bread, firm_ids)
    cov_in   <- pull_square(res$mats$cov,   firm_ids)
    rcov_in  <- pull_square(res$mats$rcov,  firm_ids)
    
    S_df <- res$mats$S
    S_in <- pull_S_matrix(S_df, firm_ids)  # n x J
    
    beta_out  <- as.numeric(T %*% beta)
    bread_out <- T %*% bread_in
    cov_out   <- T %*% cov_in  %*% t(T)
    rcov_out  <- T %*% rcov_in %*% t(T)
    S_out     <- S_in %*% t(T)
    
    out_cols <- .make_entity_cols(out_entity_ids)
    
    dimnames(cov_out)  <- list(out_cols, out_cols)
    dimnames(rcov_out) <- list(out_cols, out_cols)
    colnames(S_out)    <- out_cols
    
    # bread is Jout x Jin; set informative dimnames
    dimnames(bread_out) <- list(out_cols, .make_entity_cols(firm_ids))
    
    # rebuild S df: keep ONLY resp_id
    S_out_df <- cbind(
      resp_id = S_df[["resp_id"]],
      as.data.frame(S_out)
    )
    names(S_out_df)[2:ncol(S_out_df)] <- out_cols
    
    out_tbl <- data.frame(
      entity_type = out_entity_type,
      entity_id   = as.integer(out_entity_ids),
      entity      = as.character(out_entity_names),
      estimate    = as.numeric(beta_out),
      se          = sqrt(diag(cov_out)),
      rse         = sqrt(diag(rcov_out)),
      eb          = NA_real_,
      stringsAsFactors = FALSE
    )

    # EB shrinkage (uses eb_two_step from EB_procedure.R)
    if (exists("eb_two_step", mode = "function")) {
      ok <- is.finite(out_tbl$estimate) & is.finite(out_tbl$rse) & out_tbl$rse > 0
      if (sum(ok) >= 2) {
        eb_fit <- eb_two_step(theta_hat = out_tbl$estimate[ok], s = pmax(out_tbl$rse[ok], 1e-8))
        out_tbl$eb[ok] <- eb_fit$theta_eb
      }
    }

    # optional weight column passthrough
    if (!is.null(weight_col)) {
      out_tbl[[weight_col]] <- if (is.null(out_weight)) NA_real_ else as.numeric(out_weight)
    }
    
    out_res <- res
    out_res$firm_table <- out_tbl
    out_res$mats <- list(
      S     = S_out_df,
      bread = bread_out,
      cov   = cov_out,
      rcov  = rcov_out
    )
    out_res
  }
  
  # main loops
  for (set_name in which_sets) {
    if (is.null(results[[set_name]])) next
    
    for (model in model_names) {
      if (is.null(results[[set_name]][[model]])) next
      
      for (outcome in outcomes) {
        res <- results[[set_name]][[model]][[outcome]]
        if (is.null(res) || is.null(res$firm_table) || is.null(res$mats)) next
        
        # --- coerce and restrict to Firm entities ---
        ft_all  <- .coerce_entity_table(res$firm_table)
        firm_ft <- ft_all[ft_all$entity_type == "Firm", , drop = FALSE]
        if (nrow(firm_ft) == 0) next
        
        firm_ids <- sort(unique(as.integer(firm_ft$entity_id)))
        
        # --- industry mapping in firm_id order ---
        imap <- industry_map |>
          dplyr::select(firm_id, !!rlang::sym(industry_col)) |>
          dplyr::mutate(
            firm_id = as.integer(firm_id),
            industry_id = suppressWarnings(as.integer(.data[[industry_col]]))
          )
        
        industry_ids <- imap$industry_id[match(firm_ids, imap$firm_id)]
        if (any(is.na(industry_ids))) {
          miss <- firm_ids[is.na(industry_ids)]
          stop("Missing industry assignment for firm_id(s): ", paste(miss, collapse = ", "))
        }
        
        # --------------------------
        # Equal-weight transforms
        # --------------------------
        GAM_eq <- build_GAM_equal(firm_ids, industry_ids)
        A_eq   <- GAM_eq$A
        M_eq   <- GAM_eq$M
        inds   <- GAM_eq$inds
        
        out_ids_im   <- as.integer(inds)
        out_names_im <- paste0(industry_col, "=", out_ids_im)
        
        # optional weight column for _im (equal weights): industry firm counts
        out_w_im_eq <- NULL
        if (!is.null(weight_col)) {
          cnt <- tapply(rep(1, length(industry_ids)), industry_ids, sum)
          out_w_im_eq <- as.numeric(cnt[as.character(out_ids_im)])
        }
        
        res_im <- transform_result(
          res              = res,
          firm_ft          = firm_ft,
          firm_ids         = firm_ids,
          T                = A_eq,
          out_entity_type  = "Industry",
          out_entity_ids   = out_ids_im,
          out_entity_names = out_names_im,
          out_weight       = out_w_im_eq
        )
        
        # demeaned (_dm): firm deviations from equal-weight industry mean
        out_ids_dm   <- firm_ids
        out_names_dm <- firm_ft$entity[match(out_ids_dm, firm_ft$entity_id)]
        
        out_w_dm_eq <- NULL
        if (!is.null(weight_col) && weight_col %in% names(firm_ft)) {
          out_w_dm_eq <- firm_ft[[weight_col]][match(out_ids_dm, firm_ft$entity_id)]
        }
        
        res_dm <- transform_result(
          res              = res,
          firm_ft          = firm_ft,
          firm_ids         = firm_ids,
          T                = M_eq,
          out_entity_type  = "Firm",
          out_entity_ids   = out_ids_dm,
          out_entity_names = out_names_dm,
          out_weight       = out_w_dm_eq
        )
        
        results[[set_name]][[model]][[paste0(outcome, suffix_im)]] <- res_im
        results[[set_name]][[model]][[paste0(outcome, suffix_dm)]] <- res_dm
        
        # --------------------------
        # Weighted transforms (_im_w / _dm_w)
        # Only compute if weight_col is present AND fully populated (no NA/non-finite),
        # and (optionally) strictly positive.
        # --------------------------
        do_weighted <- !is.null(weight_col) && (weight_col %in% names(firm_ft))
        
        if (do_weighted) {
          w_raw <- firm_ft[[weight_col]][match(firm_ids, firm_ft$entity_id)]
          w_raw <- as.numeric(w_raw)
          
          # If any NA / non-finite -> SKIP weighted versions
          if (anyNA(w_raw) || any(!is.finite(w_raw))) {
            message("Skipping weighted industry means for outcome=", outcome,
                    " model=", model, " set=", set_name,
                    " because ", weight_col, " has NA/non-finite values.")
            do_weighted <- FALSE
          }
          
          # Optional: require strictly positive weights
          if (do_weighted && isTRUE(require_positive_weights) && any(w_raw <= 0)) {
            message("Skipping weighted industry means for outcome=", outcome,
                    " model=", model, " set=", set_name,
                    " because ", weight_col, " has non-positive values.")
            do_weighted <- FALSE
          }
        }
        
        if (do_weighted) {
          w_vec <- w_raw
          
          GAM_w <- build_GAM_weighted(firm_ids, industry_ids, w_vec)
          A_w   <- GAM_w$A
          M_w   <- GAM_w$M
          
          # weights aggregated to industries for _im_w: sum of firm weights in industry
          out_w_im_w <- NULL
          if (!is.null(weight_col)) {
            sumw <- tapply(w_vec, industry_ids, sum)
            out_w_im_w <- as.numeric(sumw[as.character(out_ids_im)])
          }
          
          res_im_w <- transform_result(
            res              = res,
            firm_ft          = firm_ft,
            firm_ids         = firm_ids,
            T                = A_w,
            out_entity_type  = "Industry",
            out_entity_ids   = out_ids_im,
            out_entity_names = out_names_im,
            out_weight       = out_w_im_w
          )
          
          # firm weights unchanged for _dm_w
          out_w_dm_w <- NULL
          if (!is.null(weight_col)) out_w_dm_w <- w_vec[match(out_ids_dm, firm_ids)]
          
          res_dm_w <- transform_result(
            res              = res,
            firm_ft          = firm_ft,
            firm_ids         = firm_ids,
            T                = M_w,
            out_entity_type  = "Firm",
            out_entity_ids   = out_ids_dm,
            out_entity_names = out_names_dm,
            out_weight       = out_w_dm_w
          )
          
          results[[set_name]][[model]][[paste0(outcome, suffix_im_w)]] <- res_im_w
          results[[set_name]][[model]][[paste0(outcome, suffix_dm_w)]] <- res_dm_w
        }
      }
    }
  }
  
  results
}