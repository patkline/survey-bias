# ------------------------------------------------------------------------------
# Purpose: Run Model Helper (entity-aware tables)
# Created: Jordan Cammarota 03-06-2026
# Updated: entity_type/entity_id/entity schema
# ------------------------------------------------------------------------------

should_run_model <- function(model, run_borda, run_ols) {
  switch(model,
         "Borda" = isTRUE(run_borda),
         "OLS"   = isTRUE(run_ols),
         FALSE)
}

# ------------------------------------------------------------------------------
# Small utilities (entity-aware, backward compatible)
# ------------------------------------------------------------------------------

# Convert legacy firm_table (firm_id/firm) to entity schema if needed
.coerce_entity_table <- function(ft) {
  if (is.null(ft)) return(ft)
  
  # already new schema
  if (all(c("entity_type","entity_id","entity") %in% names(ft))) {
    if (!("eb" %in% names(ft))) ft$eb <- NA_real_
    if (!("njobs" %in% names(ft))) ft$njobs <- NA_real_
    return(ft)
  }
  
  # legacy schema: firm_id / firm
  if (all(c("firm_id","firm") %in% names(ft))) {
    if (!("eb" %in% names(ft))) ft$eb <- NA_real_
    out <- ft
    out$entity_type <- "Firm"
    out$entity_id   <- as.integer(out$firm_id)
    out$entity      <- as.character(out$firm)
    out$firm_id <- NULL
    out$firm    <- NULL
    if (!("njobs" %in% names(out))) out$njobs <- NA_real_
    return(out)
  }
  
  stop(
    "firm_table is missing required columns. Need either (entity_type, entity_id, entity) ",
    "or legacy (firm_id, firm). Found: ", paste(names(ft), collapse = ", ")
  )
}

# Given an id vector, find matching column names in matrices/S:
# tries entity<id>, then firm<id>
.resolve_entity_cols <- function(ids, available_names) {
  ids_chr <- as.character(ids)
  cand_entity <- paste0("entity", ids_chr)
  cand_firm   <- paste0("firm", ids_chr)
  
  if (all(cand_entity %in% available_names)) return(cand_entity)
  if (all(cand_firm   %in% available_names)) return(cand_firm)
  
  missing_entity <- setdiff(cand_entity, available_names)
  missing_firm   <- setdiff(cand_firm,   available_names)
  
  stop(
    "Could not resolve entity columns. Missing entity* cols: ",
    paste(missing_entity, collapse = ", "),
    " ; Missing firm* cols: ",
    paste(missing_firm, collapse = ", ")
  )
}

# Standardize output matrix column names to entity<id>
.make_entity_cols <- function(ids) paste0("entity", as.character(ids))


# ------------------------------------------------------------------------------
# run_models(): base outcomes + subset97 (no writing)
# ------------------------------------------------------------------------------
run_models <- function(
    survey_vars,
    data_wide_list,
    data_long_list,
    id_map_list,
    run_borda = TRUE,
    run_ols = TRUE,

    firms97 = NULL,
    seed = 123,
    build_subset97 = TRUE
) {
  set.seed(seed)
  
  models <- c("Borda", "OLS")

  results <- list(
    all      = list(Borda = list(), OLS = list()),
    subset97 = list(Borda = list(), OLS = list())
  )
  
  # ---- Step 1: Run base outcomes ----
  for (outcome in survey_vars) {
    id_map <- id_map_list[[outcome]]
    d_wide <- data_wide_list[[outcome]]
    d_long <- data_long_list[[outcome]]
    
    for (model in models) {
      if (!should_run_model(model, run_borda, run_ols)) next

      # Aggregate to firm level, returning the recentered and non-recentered model estimates
      model_estimates <- construct_firm_level_estimates(
        aggregation_method            = model,
        respondent_firm_ratings_long  = d_long,
        respondent_firm_rankings_wide = d_wide,
        firm_names_and_job_counts     = id_map
      )

      # Store each returned model's estimates under its model name
      for (model_name in names(model_estimates)) {
        results$all[[model_name]][[outcome]] <- model_estimates[[model_name]]
      }
    }
  }
  
  # ---- Step 2: subset97 (no writing) ----
  if (isTRUE(build_subset97) && !is.null(firms97) && length(firms97) > 0) {
    results <- build_subset97_and_write(
      results           = results,
      survey_vars       = survey_vars,
      firms97_vec       = firms97
    )
  }
  
  invisible(results)
}

# ------------------------------------------------------------------------------
# build_subset97_and_write(): now entity-aware, and only subsets Firm entities
# ------------------------------------------------------------------------------
build_subset97_and_write <- function(
    results, survey_vars, firms97_vec
) {
  stopifnot(is.list(results), "all" %in% names(results), "subset97" %in% names(results))
  if (is.null(firms97_vec) || length(firms97_vec) == 0) return(results)
  
  firms97_vec <- sort(unique(as.integer(firms97_vec)))
  
  # The 97-firm EIV sample uses the centered Borda/OLS estimates only.
  # Non-recentered estimates are consumed exclusively from the full firm set.
  for (model in intersect(c("Borda", "OLS"), names(results$all))) {
    for (outcome in survey_vars) {
      res_all <- results$all[[model]][[outcome]]
      if (is.null(res_all)) next
      
      res97 <- recenter_model_result_to_firms97(res_all, firms97_vec)
      results$subset97[[model]][[outcome]] <- res97
    }
  }
  
  results
}

# ------------------------------------------------------------------------------
# recenter_model_result_to_firms97(): entity-aware
# - Only applies to Firm entities (industries are not "firms97")
# ------------------------------------------------------------------------------
recenter_model_result_to_firms97 <- function(res_all, firms97) {
  stopifnot(!is.null(res_all$mats$S))
  stopifnot(is.data.frame(res_all$mats$S))
  stopifnot("resp_id" %in% names(res_all$mats$S))
  
  if (!exists("recenter_objects", mode = "function")) {
    stop("recenter_objects() not found in scope.")
  }
  
  firms97 <- sort(unique(as.integer(firms97)))
  
  ft <- .coerce_entity_table(res_all$firm_table)
  ft <- ft[ft$entity_type == "Firm", , drop = FALSE]
  
  # restrict to firms97
  ft <- ft %>%
    dplyr::filter(entity_id %in% firms97) %>%
    dplyr::arrange(entity_id)
  
  entity_ids <- ft$entity_id
  entity_cols97_out <- .make_entity_cols(entity_ids)
  
  # --- mats restricted ---
  S_df_all <- res_all$mats$S
  
  # keep id cols if present
  id_cols <- intersect(c("resp_id", "firm_id"), names(S_df_all))
  
  # resolve incoming column names (firm* vs entity*)
  in_cols <- .resolve_entity_cols(entity_ids, names(S_df_all))
  
  S_full97 <- as.matrix(S_df_all[, in_cols, drop = FALSE])
  
  cov_all   <- as.matrix(res_all$mats$cov)
  rcov_all  <- as.matrix(res_all$mats$rcov)
  
  if (is.null(dimnames(cov_all)) || is.null(dimnames(rcov_all))) {
    stop("recenter_model_result_to_firms97(): cov/rcov must have dimnames.")
  }
  
  in_cols_b <- .resolve_entity_cols(entity_ids, colnames(cov_all))
  
  cov97   <- cov_all  [in_cols_b, in_cols_b, drop = FALSE]
  rcov97  <- rcov_all [in_cols_b, in_cols_b, drop = FALSE]
  
  # --- recenter in 97-space ---
  beta97 <- as.numeric(ft$estimate)
  
  rec <- recenter_objects(
    beta   = beta97,
    cov    = cov97,
    rcov   = rcov97,
    S_full = S_full97
  )
  
  beta_c <- as.numeric(rec$beta)
  cov_c  <- as.matrix(rec$cov)
  rcov_c <- as.matrix(rec$rcov)
  S_c    <- as.matrix(rec$S)

  stopifnot(isTRUE(all.equal(crossprod(S_c), rcov_c, tolerance = 1e-10)))

  
  # standardize dimnames to entity<id>
  dimnames(cov_c)   <- list(entity_cols97_out, entity_cols97_out)
  dimnames(rcov_c)  <- list(entity_cols97_out, entity_cols97_out)
  colnames(S_c)     <- entity_cols97_out
  
  # write back into entity table
  ft$estimate <- beta_c
  ft$se       <- sqrt(diag(cov_c))
  ft$rse      <- sqrt(diag(rcov_c))
  
  # rebuild S df
  S_df <- cbind(S_df_all[, id_cols, drop = FALSE], as.data.frame(S_c))
  names(S_df)[(ncol(S_df) - length(entity_cols97_out) + 1):ncol(S_df)] <- entity_cols97_out
  
  res_97 <- res_all
  res_97$firm_table <- ft
  res_97$mats <- list(
    S     = S_df,
    cov   = cov_c,
    rcov  = rcov_c
  )
  
  res_97
}

# ------------------------------------------------------------------------------
# recenter_objects(): unchanged (still generic)
# ------------------------------------------------------------------------------
recenter_objects <- function(beta, S_full = NULL, ...) {
  J <- length(beta)
  C <- diag(J) - matrix(1 / J, J, J)
  
  mats <- list(...)
  
  out <- list(beta = as.numeric(C %*% beta))
  
  if (length(mats)) {
    for (nm in names(mats)) {
      M <- mats[[nm]]
      if (is.null(M)) {
        out[[nm]] <- NULL
      } else {
        M <- as.matrix(M)
        stopifnot(nrow(M) == J, ncol(M) == J)
        out[[nm]] <- C %*% M %*% t(C)
      }
    }
  }
  
  if (!is.null(S_full)) {
    S_full <- as.matrix(S_full)
    stopifnot(ncol(S_full) == J)
    out$S <- S_full %*% t(C)
  } else {
    out$S <- NULL
  }
  
  out
}

# ------------------------------------------------------------------------------
# Write one long sheet (entity-aware)
# - Uses firm_table entity schema directly
# - Experimental outcomes appended as entity_type="Firm"
# ------------------------------------------------------------------------------
write_coefficients_long_sheet <- function(
    results,
    output_dir,
    sheet_name = "coefficients_long",
    include_sets = c("all", "subset97"),
    include_models = NULL,
    include_outcomes = NULL,
    data_for_experimental = NULL,
    experimental_vars = NULL
) {
  stopifnot(is.list(results), all(c("all","subset97") %in% names(results)))
  
  if (is.null(include_models)) {
    include_models <- unique(c(names(results$all), names(results$subset97)))
  }
  
  rows <- list()
  k <- 1L
  
  # ----------------------------
  # Pull model-produced results
  # ----------------------------
  for (set_nm in include_sets) {
    if (is.null(results[[set_nm]])) next
    
    for (m in include_models) {
      if (is.null(results[[set_nm]][[m]])) next
      model_list <- results[[set_nm]][[m]]
      
      outs <- names(model_list)
      outs <- outs[!is.na(outs) & nzchar(outs)]
      if (!is.null(include_outcomes)) outs <- intersect(outs, include_outcomes)
      
      for (outcome in outs) {
        res <- model_list[[outcome]]
        if (is.null(res) || is.null(res$firm_table)) next
        
        ft <- .coerce_entity_table(res$firm_table)
        
        if (!("estimate" %in% names(ft))) next
        if (!("se" %in% names(ft)))    ft$se    <- NA_real_
        if (!("rse" %in% names(ft)))   ft$rse   <- NA_real_
        if (!("eb" %in% names(ft)))    ft$eb    <- NA_real_
        if (!("njobs" %in% names(ft))) ft$njobs <- NA_real_
        if (!("firm_number_of_respondents" %in% names(ft))) ft$firm_number_of_respondents <- NA_integer_
        if (!("total_number_of_respondents" %in% names(ft))) ft$total_number_of_respondents <- NA_integer_
        
        rows[[k]] <- ft %>%
          dplyr::transmute(
            subset  = set_nm,
            model   = m,
            outcome = outcome,
            entity_type = as.character(entity_type),
            entity_id   = as.integer(entity_id),
            entity      = as.character(entity),
            njobs    = suppressWarnings(as.numeric(njobs)),
            firm_number_of_respondents = suppressWarnings(as.integer(firm_number_of_respondents)),
            total_number_of_respondents = suppressWarnings(as.integer(total_number_of_respondents)),
            estimate = as.numeric(estimate),
            se       = as.numeric(se),
            rse      = as.numeric(rse),
            eb       = as.numeric(eb)
          )
        k <- k + 1L
      }
    }
  }
  
  coef_long <- dplyr::bind_rows(rows)
  
  # Experimental firm-level outcomes used as EIV dependent variables.
  if (!is.null(experimental_vars) && length(experimental_vars) > 0) {
    stopifnot(!is.null(data_for_experimental))
    
    exp_df <- clean_experimental(data_for_experimental, experimental_vars)
    stopifnot(all(c("firm_id","firm") %in% names(exp_df)))
    
    # firm-level njobs lookup (from raw data)
    njobs_by_firm <- data_for_experimental %>%
      dplyr::select(firm_id, njobs) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        firm_id = as.integer(firm_id),
        njobs   = suppressWarnings(as.numeric(njobs))
      )
    
    # Base experimental firm-level rows (keep njobs; may include NA here)
    exp_long <- exp_df %>%
      dplyr::mutate(firm_id = as.integer(firm_id)) %>%
      dplyr::left_join(njobs_by_firm, by = "firm_id") %>%
      dplyr::select(firm_id, firm, njobs, dplyr::all_of(experimental_vars)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(experimental_vars),
        names_to = "outcome",
        values_to = "estimate"
      ) %>%
      dplyr::mutate(
        subset = "subset97",
        model  = "EXPERIMENTAL",
        entity_type = "Firm",
        entity_id   = as.integer(firm_id),
        entity      = as.character(firm),
        se  = NA_real_,
        rse = NA_real_,
        eb  = NA_real_
      ) %>%
      dplyr::select(subset, model, outcome, entity_type, entity_id, entity,
                    njobs, estimate, se, rse, eb)
    
    coef_long <- dplyr::bind_rows(coef_long, exp_long)
  }
  
  # write to parquet
  write_parquet_sheet(output_dir, sheet_name, coef_long)

  invisible(coef_long)
}

# ------------------------------------------------------------------------------
# Write the per-fit J x J robust covariance matrix in long format.
# One row per (subset, model, outcome, entity_id_i, entity_id_j).
# Skips fits that don't have a usable mats$rcov (e.g., PL).
# ------------------------------------------------------------------------------
write_rcov_long_sheet <- function(
    results,
    output_dir,
    sheet_name = "rcov",
    include_sets = c("all", "subset97"),
    include_models = c("Borda", "OLS", "Borda_not_recentered", "OLS_not_recentered")
) {
  stopifnot(is.list(results), all(c("all", "subset97") %in% names(results)))

  rows <- list()
  k <- 1L

  for (set_nm in include_sets) {
    if (is.null(results[[set_nm]])) next
    for (m in include_models) {
      if (is.null(results[[set_nm]][[m]])) next
      model_list <- results[[set_nm]][[m]]
      outs <- names(model_list)
      outs <- outs[!is.na(outs) & nzchar(outs)]
      for (outcome in outs) {
        res <- model_list[[outcome]]
        if (is.null(res) || is.null(res$mats) || is.null(res$mats$rcov)) next
        M <- as.matrix(res$mats$rcov)
        if (nrow(M) != ncol(M) || is.null(rownames(M)) || is.null(colnames(M))) next
        ids <- suppressWarnings(as.integer(sub("^entity", "", rownames(M))))
        if (anyNA(ids)) {
          stop("write_rcov_long_sheet(): non-numeric entity ids in rcov rownames for ",
               set_nm, "/", m, "/", outcome)
        }
        rows[[k]] <- tibble::tibble(
          subset      = set_nm,
          model       = m,
          outcome     = outcome,
          entity_id_i = rep(ids, times = ncol(M)),
          entity_id_j = rep(ids, each  = nrow(M)),
          rcov        = as.numeric(M)
        )
        k <- k + 1L
      }
    }
  }

  rcov_long <- dplyr::bind_rows(rows)
  write_parquet_sheet(output_dir, sheet_name, rcov_long)
  invisible(rcov_long)
}
