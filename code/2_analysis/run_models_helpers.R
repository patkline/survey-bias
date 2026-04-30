# ------------------------------------------------------------------------------
# Purpose: Run Model Helper (entity-aware tables)
# Created: Jordan Cammarota 03-06-2026
# Updated: entity_type/entity_id/entity schema
# ------------------------------------------------------------------------------

should_run_model <- function(model, run_ol, run_pl, run_borda, run_ols, run_ols_centered) {
  switch(model,
         "OL"    = isTRUE(run_ol),
         "PL"    = isTRUE(run_pl),
         "Borda" = isTRUE(run_borda),
         "OLS"   = isTRUE(run_ols),
         "OLSC"  = isTRUE(run_ols_centered),
         FALSE)
}

model_prefix <- function(model) {
  switch(model,
         "OL"    = "ol",
         "PL"    = "pl",
         "Borda" = "b",
         "OLS"   = "ols",
         "OLSC"  = "olsc",
         stop("Unknown model: ", model))
}

# ------------------------------------------------------------------------------
# Dispatch: call the correct model runner
# NOTE: You should update each model runner to return firm_table with:
#   entity_type, entity_id, entity, estimate, se, rse, eb
# These helpers are backward-compatible if old runners still return firm_id/firm.
# ------------------------------------------------------------------------------
run_model_dispatch <- function(model,
                               d_long,
                               d_wide,
                               id_map,
                               outcome,
                               respondent_col,
                               firms97 = NULL,
                               seed = 123,
                               borda_args = list(
                                 higher_is_better = FALSE,
                                 normalize = TRUE,
                                 ref_firm_ids = c(38, 76, 90),
                                 do_eb = TRUE
                               )) {
  
  if (model == "OL") {
    return(run_model_ol(dat_long = d_long,
                        outcome = outcome,
                        respondent_col = respondent_col,
                        id_map = id_map))
  }
  
  if (model == "PL") {
    return(run_model_pl(data_wide = d_wide,
                        id_map = id_map,
                        outcome = outcome,
                        firms97 = firms97))
  }
  
  if (model == "Borda") {
    return(run_model_borda(
      data_wide        = d_wide,
      id_map           = id_map,
      outcome          = outcome,
      higher_is_better = borda_args$higher_is_better,
      normalize        = borda_args$normalize,
      ref_firm_ids     = borda_args$ref_firm_ids,
      do_eb            = borda_args$do_eb,
      seed             = seed
    ))
  }
  
  if (model == "OLS") {
    return(run_model_ols(
      d_long,
      id_map,
      outcome,
      scale_and_center = FALSE,
      do_eb = TRUE,
      seed = seed
    ))
  }
  
  if (model == "OLSC") {
    return(run_model_ols(
      d_long,
      id_map,
      outcome,
      scale_and_center = TRUE,
      do_eb = TRUE,
      seed = seed
    ))
  }
  
  stop("Unknown model: ", model)
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
# NEW helper: combine two outcome results into a single combined outcome
# (equal weighting; assumes Firm entities)
# ------------------------------------------------------------------------------
combine_results_equal_weight <- function(res1, res2, new_outcome, w = 0.5) {
  
  stopifnot(!is.null(res1$firm_table), !is.null(res2$firm_table))
  stopifnot(!is.null(res1$mats$S), !is.null(res2$mats$S))
  stopifnot(!is.null(res1$mats$bread), !is.null(res2$mats$bread))
  stopifnot(!is.null(res1$mats$cov), !is.null(res2$mats$cov))
  
  w1 <- w
  w2 <- 1 - w
  
  ft1 <- .coerce_entity_table(res1$firm_table)
  ft2 <- .coerce_entity_table(res2$firm_table)
  
  # we only combine Firm entities here
  ft1 <- ft1[ft1$entity_type == "Firm", , drop = FALSE]
  ft2 <- ft2[ft2$entity_type == "Firm", , drop = FALSE]
  
  # make sure njobs exists (so downstream code can always assume it’s there)
  if (!("njobs" %in% names(ft1))) ft1$njobs <- NA_real_
  if (!("njobs" %in% names(ft2))) ft2$njobs <- NA_real_
  
  stopifnot("estimate" %in% names(ft1), "estimate" %in% names(ft2))
  common_ids <- intersect(ft1$entity_id, ft2$entity_id)
  common_ids <- sort(as.integer(common_ids))
  
  if (length(common_ids) < 2L) {
    out <- res1
    out$firm_table <- ft1[ft1$entity_id %in% common_ids, , drop = FALSE]
    out$mats <- list(
      S     = res1$mats$S[0, , drop = FALSE],
      bread = matrix(NA_real_, 0, 0),
      cov   = matrix(NA_real_, 0, 0),
      rcov  = matrix(NA_real_, 0, 0)
    )
    return(out)
  }
  
  # resolve matrix column naming (firm* vs entity*)
  B1 <- as.matrix(res1$mats$bread)
  B2 <- as.matrix(res2$mats$bread)
  V1 <- as.matrix(res1$mats$cov)
  V2 <- as.matrix(res2$mats$cov)
  
  if (is.null(dimnames(B1)) || is.null(dimnames(B2)) ||
      is.null(dimnames(V1)) || is.null(dimnames(V2))) {
    stop("combine_results_equal_weight(): bread/cov must have dimnames.")
  }
  
  cols1 <- .resolve_entity_cols(common_ids, colnames(B1))
  cols2 <- .resolve_entity_cols(common_ids, colnames(B2))
  
  B1c <- B1[cols1, cols1, drop = FALSE]
  V1c <- V1[cols1, cols1, drop = FALSE]
  B2c <- B2[cols2, cols2, drop = FALSE]
  V2c <- V2[cols2, cols2, drop = FALSE]
  
  # build beta vectors aligned
  b1 <- ft1$estimate[match(common_ids, ft1$entity_id)]
  b2 <- ft2$estimate[match(common_ids, ft2$entity_id)]
  beta_cmb <- w1 * b1 + w2 * b2
  
  # combine bread/cov (independent samples assumption)
  bread_cmb <- w1 * B1c + w2 * B2c
  cov_cmb   <- (w1^2) * V1c + (w2^2) * V2c
  
  # combine scores by row-binding
  S1_df <- res1$mats$S
  S2_df <- res2$mats$S
  stopifnot("resp_id" %in% names(S1_df), "resp_id" %in% names(S2_df))
  
  s_cols1 <- .resolve_entity_cols(common_ids, names(S1_df))
  s_cols2 <- .resolve_entity_cols(common_ids, names(S2_df))
  
  S1_mat <- as.matrix(S1_df[, s_cols1, drop = FALSE])
  S2_mat <- as.matrix(S2_df[, s_cols2, drop = FALSE])
  
  S_cmb_mat <- rbind(w1 * S1_mat, w2 * S2_mat)
  
  # rebuild S_df with harmonized id columns
  id_cols1 <- intersect(c("resp_id", "firm_id"), names(S1_df))
  id_cols2 <- intersect(c("resp_id", "firm_id"), names(S2_df))
  ids1 <- S1_df[, id_cols1, drop = FALSE]
  ids2 <- S2_df[, id_cols2, drop = FALSE]
  all_id_cols <- union(names(ids1), names(ids2))
  for (cn in all_id_cols) {
    if (!(cn %in% names(ids1))) ids1[[cn]] <- NA
    if (!(cn %in% names(ids2))) ids2[[cn]] <- NA
  }
  ids1 <- ids1[, all_id_cols, drop = FALSE]
  ids2 <- ids2[, all_id_cols, drop = FALSE]
  ids_cmb <- dplyr::bind_rows(ids1, ids2)
  
  out_cols <- .make_entity_cols(common_ids)
  
  S_cmb_df <- cbind(ids_cmb, as.data.frame(S_cmb_mat))
  names(S_cmb_df)[(ncol(S_cmb_df) - length(out_cols) + 1):ncol(S_cmb_df)] <- out_cols
  
  # standardize dimnames to entity<id>
  dimnames(bread_cmb) <- list(out_cols, out_cols)
  dimnames(cov_cmb)   <- list(out_cols, out_cols)
  
  # robust covariance (your convention)
  rcov_cmb <- bread_cmb %*% crossprod(S_cmb_mat) %*% bread_cmb
  dimnames(rcov_cmb) <- list(out_cols, out_cols)
  
  # build output entity table (include njobs)
  ent_name <- ft1$entity[match(common_ids, ft1$entity_id)]
  if (all(is.na(ent_name))) ent_name <- ft2$entity[match(common_ids, ft2$entity_id)]
  
  nj1 <- ft1$njobs[match(common_ids, ft1$entity_id)]
  nj2 <- ft2$njobs[match(common_ids, ft2$entity_id)]
  # prefer ft1 unless missing, then ft2
  njobs_out <- dplyr::coalesce(nj1, nj2)
  
  ft_out <- data.frame(
    entity_type = "Firm",
    entity_id   = common_ids,
    entity      = ent_name,
    njobs       = as.numeric(njobs_out),
    estimate    = as.numeric(beta_cmb),
    se          = sqrt(diag(cov_cmb)),
    rse         = sqrt(diag(rcov_cmb)),
    eb          = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (exists("eb_two_step", mode = "function")) {
    ok <- is.finite(ft_out$estimate) & is.finite(ft_out$rse) & ft_out$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = ft_out$estimate[ok], s = pmax(ft_out$rse[ok], 1e-8))
      ft_out$eb[ok] <- eb_fit$theta_eb
    }
  }
  
  out <- res1
  out$firm_table <- ft_out
  out$mats <- list(
    S     = S_cmb_df,
    bread = bread_cmb,
    cov   = cov_cmb,
    rcov  = rcov_cmb
  )
  
  out
}

# ------------------------------------------------------------------------------
# run_models(): base + combine_valences + subset97 (no writing)
# ------------------------------------------------------------------------------
run_models <- function(
    survey_vars,
    respondent_col,
    
    data_wide_list,
    data_long_list,
    id_map_list,
    
    experimental_vars = NULL,
    data_for_experimental = NULL,
    
    run_ol = TRUE,
    run_pl = TRUE,
    run_borda = TRUE,
    run_ols = TRUE,
    run_ols_centered = TRUE,
    
    firms97 = NULL,
    seed = 123,
    
    borda_args = list(
      higher_is_better = FALSE,
      normalize = TRUE,
      ref_firm_ids = c(38, 76, 90),
      do_eb = TRUE
    ),
    
    build_subset97 = TRUE,
    
    combine_valences = FALSE,
    valence_triples = NULL
) {
  set.seed(seed)
  
  models <- c("OL", "PL", "Borda", "OLS", "OLSC")
  
  results <- list(
    all      = list(OL = list(), PL = list(), Borda = list(), OLS = list(), OLSC = list()),
    subset97 = list(OL = list(), PL = list(), Borda = list(), OLS = list(), OLSC = list())
  )
  
  # ---- Step 1: Run base outcomes ----
  for (outcome in survey_vars) {
    message("Outcome: ", outcome)
    
    id_map <- id_map_list[[outcome]]
    d_wide <- data_wide_list[[outcome]]
    d_long <- data_long_list[[outcome]]
    
    for (model in models) {
      if (!should_run_model(model, run_ol, run_pl, run_borda, run_ols, run_ols_centered)) next
      
      message("Running ", model, " (all): ", outcome)
      
      res <- run_model_dispatch(
        model          = model,
        d_long         = d_long,
        d_wide         = d_wide,
        id_map         = id_map,
        outcome        = outcome,
        respondent_col = respondent_col,
        firms97        = firms97,
        seed           = seed,
        borda_args     = borda_args
      )
      
      # coerce firm_table to entity schema (for internal consistency)
      if (!is.null(res$firm_table)) res$firm_table <- .coerce_entity_table(res$firm_table)
      
      results$all[[model]][[outcome]] <- res
    }
  }
  
  # ---- Step 1a: Combine valences (ALL) ----
  combined_outcomes <- character(0)
  
  if (isTRUE(combine_valences)) {
    if (is.null(valence_triples) || length(valence_triples) == 0) {
      warning("combine_valences=TRUE but valence_triples is empty; skipping.")
    } else {
      for (tri in valence_triples) {
        v1  <- tri$valence1
        v2  <- tri$valence2
        out <- tri$new_outcome
        
        combined_outcomes <- c(combined_outcomes, out)
        
        for (model in models) {
          if (!should_run_model(model, run_ol, run_pl, run_borda, run_ols, run_ols_centered)) next
          
          res1 <- results$all[[model]][[v1]]
          res2 <- results$all[[model]][[v2]]
          
          if (is.null(res1) || is.null(res2)) {
            warning("Skipping combine for model=", model, " outcome=", out,
                    " because missing one of: ", v1, ", ", v2)
            next
          }
          
          message("Combining valences (all): ", model, "  ", v1, " + ", v2, " -> ", out)
          
          results$all[[model]][[out]] <- combine_results_equal_weight(
            res1, res2, new_outcome = out, w = 0.5
          )
        }
      }
    }
  }
  
  combined_outcomes <- unique(combined_outcomes)
  
  # ---- Step 2: subset97 (no writing) ----
  if (isTRUE(build_subset97) && !is.null(firms97) && length(firms97) > 0) {
    outcomes_for_97 <- unique(c(survey_vars, combined_outcomes))
    
    results <- build_subset97_and_write(
      results           = results,
      survey_vars       = outcomes_for_97,
      firms97_vec       = firms97,
      data              = data_for_experimental,
      experimental_vars = experimental_vars,
      prefix_map        = list(OL = "ol", PL = "pl", Borda = "b", OLS = "ols", OLSC = "olsc")
    )
  }
  
  invisible(results)
}

# ------------------------------------------------------------------------------
# build_subset97_and_write(): now entity-aware, and only subsets Firm entities
# ------------------------------------------------------------------------------
build_subset97_and_write <- function(
    results, survey_vars, firms97_vec,
    data, experimental_vars = NULL, prefix_map = NULL
) {
  stopifnot(is.list(results), "all" %in% names(results), "subset97" %in% names(results))
  if (is.null(firms97_vec) || length(firms97_vec) == 0) return(results)
  
  firms97_vec <- sort(unique(as.integer(firms97_vec)))
  
  for (model in names(results$all)) {
    if (!is.null(prefix_map) && is.null(prefix_map[[model]])) next
    
    for (outcome in survey_vars) {
      res_all <- results$all[[model]][[outcome]]
      if (is.null(res_all)) next
      
      message("Recentering results for 97 firms: ", model, " + ", outcome)
      
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
  
  bread_all <- as.matrix(res_all$mats$bread)
  cov_all   <- as.matrix(res_all$mats$cov)
  rcov_all  <- as.matrix(res_all$mats$rcov)
  
  if (is.null(dimnames(bread_all)) || is.null(dimnames(cov_all)) || is.null(dimnames(rcov_all))) {
    stop("recenter_model_result_to_firms97(): bread/cov/rcov must have dimnames.")
  }
  
  in_cols_b <- .resolve_entity_cols(entity_ids, colnames(bread_all))
  
  bread97 <- bread_all[in_cols_b, in_cols_b, drop = FALSE]
  cov97   <- cov_all  [in_cols_b, in_cols_b, drop = FALSE]
  rcov97  <- rcov_all [in_cols_b, in_cols_b, drop = FALSE]
  
  # --- recenter in 97-space ---
  beta97 <- as.numeric(ft$estimate)
  
  rec <- recenter_objects(
    beta   = beta97,
    Binv   = bread97,
    cov    = cov97,
    S_full = S_full97
  )
  
  beta_c  <- as.numeric(rec$beta)
  bread_c <- as.matrix(rec$Binv)
  cov_c   <- as.matrix(rec$cov)
  S_c     <- as.matrix(rec$S)
  
  # robust covariance
  rcov_c <- bread_c %*% crossprod(S_c) %*% bread_c
  
  # standardize dimnames to entity<id>
  dimnames(bread_c) <- list(entity_cols97_out, entity_cols97_out)
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
    bread = bread_c,
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
    experimental_vars = NULL,
    industry_map = NULL,
    industry_col = "aer_naics2",
    suffix_dm = "_dm",
    suffix_im = "_im",
    suffix_dm_w = "_dm_w",
    suffix_im_w = "_im_w"
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
        
        rows[[k]] <- ft %>%
          dplyr::transmute(
            subset  = set_nm,
            model   = m,
            outcome = outcome,
            entity_type = as.character(entity_type),
            entity_id   = as.integer(entity_id),
            entity      = as.character(entity),
            njobs    = suppressWarnings(as.numeric(njobs)),
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
  
  # ---------------------------------------------------------
  # Experimental outcomes (Firm) + derived (_im/_dm/_im_w/_dm_w)
  # ---------------------------------------------------------
  if (!is.null(experimental_vars) && length(experimental_vars) > 0) {
    stopifnot(!is.null(data_for_experimental))
    
    exp_res <- clean_experimental(data_for_experimental, experimental_vars)
    exp_df  <- exp_res$coefficients
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
    
    # If industry_map provided: create im/dm and im_w/dm_w
    if (!is.null(industry_map)) {
      stopifnot(all(c("firm_id", industry_col) %in% names(industry_map)))
      
      # Attach industry + njobs; then restrict to firms with non-NA njobs
      exp_base <- exp_df %>%
        dplyr::mutate(firm_id = as.integer(firm_id)) %>%
        dplyr::left_join(njobs_by_firm, by = "firm_id") %>%
        dplyr::left_join(
          industry_map %>%
            dplyr::transmute(
              firm_id = as.integer(firm_id),
              industry_id = suppressWarnings(as.integer(.data[[industry_col]]))
            ),
          by = "firm_id"
        ) %>%
        dplyr::filter(!is.na(industry_id)) %>%
        dplyr::filter(!is.na(njobs))   # KEY: ensures no NA in njobs for _w versions too
      
      # If nothing remains, just skip derived experimental im/dm
      if (nrow(exp_base) > 0) {
        
        # Long firm-outcome form (restricted set; njobs guaranteed non-NA)
        exp_long_firm <- exp_base %>%
          dplyr::select(firm_id, firm, njobs, industry_id, dplyr::all_of(experimental_vars)) %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(experimental_vars),
            names_to = "outcome",
            values_to = "estimate"
          )
        
        # --------------------------
        # Equal-weight industry mean
        # --------------------------
        exp_im <- exp_long_firm %>%
          dplyr::group_by(industry_id, outcome) %>%
          dplyr::summarise(
            estimate = mean(estimate, na.rm = TRUE),
            njobs    = sum(njobs, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            subset = "subset97",
            model  = "EXPERIMENTAL",
            outcome = paste0(outcome, suffix_im),
            entity_type = "Industry",
            entity_id   = as.integer(industry_id),
            entity      = as.character(industry_id),
            se  = NA_real_,
            rse = NA_real_,
            eb  = NA_real_
          ) %>%
          dplyr::select(subset, model, outcome, entity_type, entity_id, entity,
                        njobs, estimate, se, rse, eb)
        
        exp_im_key <- exp_im %>%
          dplyr::transmute(
            industry_id  = entity_id,
            outcome_base = sub(paste0(suffix_im, "$"), "", outcome),
            ind_mean     = estimate
          )
        
        exp_dm <- exp_long_firm %>%
          dplyr::left_join(
            exp_im_key,
            by = c("industry_id" = "industry_id", "outcome" = "outcome_base")
          ) %>%
          dplyr::mutate(
            estimate = estimate - ind_mean,
            subset = "subset97",
            model  = "EXPERIMENTAL",
            outcome = paste0(outcome, suffix_dm),
            entity_type = "Firm",
            entity_id   = as.integer(firm_id),
            entity      = as.character(firm),
            se  = NA_real_,
            rse = NA_real_,
            eb  = NA_real_
          ) %>%
          dplyr::select(subset, model, outcome, entity_type, entity_id, entity,
                        njobs, estimate, se, rse, eb)
        
        # --------------------------
        # Weighted industry mean
        # --------------------------
        # njobs is guaranteed non-NA here because we filtered exp_base
        exp_im_w <- exp_long_firm %>%
          dplyr::group_by(industry_id, outcome) %>%
          dplyr::summarise(
            estimate = stats::weighted.mean(estimate, w = njobs, na.rm = TRUE),
            njobs    = sum(njobs, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            subset = "subset97",
            model  = "EXPERIMENTAL",
            outcome = paste0(outcome, suffix_im_w),
            entity_type = "Industry",
            entity_id   = as.integer(industry_id),
            entity      = as.character(industry_id),
            se  = NA_real_,
            rse = NA_real_,
            eb  = NA_real_
          ) %>%
          dplyr::select(subset, model, outcome, entity_type, entity_id, entity,
                        njobs, estimate, se, rse, eb)
        
        exp_im_w_key <- exp_im_w %>%
          dplyr::transmute(
            industry_id  = entity_id,
            outcome_base = sub(paste0(suffix_im_w, "$"), "", outcome),
            ind_mean_w   = estimate
          )
        
        exp_dm_w <- exp_long_firm %>%
          dplyr::left_join(
            exp_im_w_key,
            by = c("industry_id" = "industry_id", "outcome" = "outcome_base")
          ) %>%
          dplyr::mutate(
            estimate = estimate - ind_mean_w,
            subset = "subset97",
            model  = "EXPERIMENTAL",
            outcome = paste0(outcome, suffix_dm_w),
            entity_type = "Firm",
            entity_id   = as.integer(firm_id),
            entity      = as.character(firm),
            se  = NA_real_,
            rse = NA_real_,
            eb  = NA_real_
          ) %>%
          dplyr::select(subset, model, outcome, entity_type, entity_id, entity,
                        njobs, estimate, se, rse, eb)
        
        coef_long <- dplyr::bind_rows(coef_long, exp_im, exp_dm, exp_im_w, exp_dm_w)
      }
    }
  }
  
  # write to parquet
  write_parquet_sheet(output_dir, sheet_name, coef_long)

  invisible(coef_long)
}