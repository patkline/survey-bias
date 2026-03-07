# ------------------------------------------------------------------------------
# Purpose: Run Model Helper
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------

should_run_model <- function(model, run_ol, run_pl, run_borda, run_ols, run_ols_centered) {
  switch(model,
         "OL"    = isTRUE(run_ol),
         "PL"    = isTRUE(run_pl),
         "Borda" = isTRUE(run_borda),
         "OLS"   = isTRUE(run_ols),
         "OLSC" = isTRUE(run_ols_centered),
         FALSE)
}

model_prefix <- function(model) {
  switch(model,
         "OL"    = "ol",
         "PL"    = "pl",
         "Borda" = "b",
         "OLS"   = "ols",
         "OLSC" = "olsc",
         stop("Unknown model: ", model))
}

# Dispatch: call the correct model runner
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

# ==============================================================================
# run_models(): ONE CALL that does everything inside your commented loop
#   - loops outcomes x models
#   - calls run_model_dispatch()
#   - stores results$all
#   - calls collect_and_write()
#   - then builds subset97 + writes "Coefficients (97)" (via your existing helper)
# ==============================================================================

run_models <- function(
    wb,
    output_path,
    
    survey_vars,
    respondent_col,
    
    data_wide_list,
    data_long_list,
    id_map_list,
    
    experimental_vars = NULL,
    data_for_experimental = NULL,   # pass `data` only if your subset97 writer needs it
    
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
    
    # if TRUE, build subset97 and write "Coefficients (97)" etc.
    build_subset97 = TRUE
) {
  set.seed(seed)
  
  models <- c("OL", "PL", "Borda", "OLS", "OLSC")
  
  # results containers
  results <- list(
    all = list(OL = list(), PL = list(), Borda = list(), OLS = list(), OLSC = list()),
    subset97 = list(OL = list(), PL = list(), Borda = list(), OLS = list(), OLSC = list())
  )
  
  # collectors (long; to_wide later)
  coef_long <- list()
  se_long   <- list()
  rse_long  <- list()
  eb_long   <- list()
  
  # ----------------------------
  # Step 1: Run ALL models + write per-outcome matrices via collect_and_write
  # ----------------------------
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
      
      results$all[[model]][[outcome]] <- res
      
      tmp <- collect_and_write(
        res       = res,
        outcome   = outcome,
        wb        = wb,
        prefix    = model_prefix(model),
        model     = model,
        coef_long = coef_long,
        se_long   = se_long,
        rse_long  = rse_long,
        eb_long   = eb_long
      )
      
      coef_long <- tmp$coef_long
      se_long   <- tmp$se_long
      rse_long  <- tmp$rse_long
      eb_long   <- tmp$eb_long
    }
  }
  
  # ----------------------------
  # Step 1b: Write ALL master sheets (NO experimental columns)
  # ----------------------------
  coef_df <- to_wide(dplyr::bind_rows(coef_long))
  se_df   <- to_wide(dplyr::bind_rows(se_long))
  rse_df  <- to_wide(dplyr::bind_rows(rse_long))
  eb_df   <- to_wide(dplyr::bind_rows(eb_long))
  
  remove_sheet_safely(wb, "Coefficients")
  openxlsx::addWorksheet(wb, "Coefficients")
  openxlsx::writeData(wb, "Coefficients", coef_df)
  
  remove_sheet_safely(wb, "Standard_Errors")
  openxlsx::addWorksheet(wb, "Standard_Errors")
  openxlsx::writeData(wb, "Standard_Errors", se_df)
  
  remove_sheet_safely(wb, "Robust SEs")
  openxlsx::addWorksheet(wb, "Robust SEs")
  openxlsx::writeData(wb, "Robust SEs", rse_df)
  
  remove_sheet_safely(wb, "EB Coefficients")
  openxlsx::addWorksheet(wb, "EB Coefficients")
  openxlsx::writeData(wb, "EB Coefficients", eb_df)
  
  # ----------------------------
  # Step 2: subset97 (loop over results$all INSIDE helper)
  # - writes "Coefficients (97)" and stores results$subset97
  # ----------------------------
  if (isTRUE(build_subset97) && !is.null(firms97) && length(firms97) > 0) {
    prefix_map <- list(OL = "ol", PL = "pl", Borda = "b", OLS = "ols", OLSC = "olsc")
    
    results <- build_subset97_and_write(
      results           = results,
      wb                = wb,
      survey_vars       = survey_vars,
      firms97_vec       = firms97,
      data              = data_for_experimental,  # only needed if helper attaches experimental outcomes
      experimental_vars = experimental_vars,
      prefix_map        = prefix_map
    )
  }
  
  invisible(results)
}

collect_and_write <- function(
    res, outcome, wb,
    prefix,      # "b", "pl", "ol", "ols", "olsc"
    model,       # "OL" / "PL" / "Borda" / "OLS" / "OLSC"
    coef_long, se_long, rse_long, eb_long
) {
  ft <- res$firm_table %>% dplyr::mutate(outcome = outcome)
  
  coef_long[[length(coef_long) + 1L]] <- add_model(
    ft %>% dplyr::transmute(firm_id, firm, outcome, value = estimate),
    model
  )
  se_long[[length(se_long) + 1L]] <- add_model(
    ft %>% dplyr::transmute(firm_id, firm, outcome, value = se),
    model
  )
  rse_long[[length(rse_long) + 1L]] <- add_model(
    ft %>% dplyr::transmute(firm_id, firm, outcome, value = rse),
    model
  )
  eb_long[[length(eb_long) + 1L]] <- add_model(
    ft %>% dplyr::transmute(firm_id, firm, outcome, value = eb),
    model
  )
  
  write_matrix_sheet(wb, paste0("S_",    prefix, "_", outcome), res$mats$S)
  write_matrix_sheet(wb, paste0("Binv_", prefix, "_", outcome), res$mats$bread)
  write_matrix_sheet(wb, paste0("cov_",  prefix, "_", outcome), res$mats$cov)
  write_matrix_sheet(wb, paste0("rcov_", prefix, "_", outcome), res$mats$rcov)
  
  list(coef_long = coef_long, se_long = se_long, rse_long = rse_long, eb_long = eb_long)
}


to_wide <- function(df_long,
                    id_cols   = c("firm_id","firm","model"),
                    name_col  = "outcome",
                    value_col = "value") {
  stopifnot(all(c(id_cols, name_col, value_col) %in% names(df_long)))
  
  dup <- df_long %>%
    dplyr::count(dplyr::across(dplyr::all_of(c(id_cols, name_col)))) %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup) > 0) {
    stop(
      "to_wide(): duplicate rows for the same (",
      paste(c(id_cols, name_col), collapse = ", "),
      "). De-duplicate upstream."
    )
  }
  
  tidyr::pivot_wider(
    df_long,
    id_cols     = dplyr::all_of(id_cols),
    names_from  = dplyr::all_of(name_col),
    values_from = dplyr::all_of(value_col)
  )
}

get_experimental_wide <- function(data, experimental_vars) {
  exp_results <- clean_experimental(data, experimental_vars)
  exp_coef_wide <- exp_results$coefficients %>%
    dplyr::select(firm_id, firm, dplyr::all_of(experimental_vars))
  list(coef = exp_coef_wide)
}

add_model <- function(df, model) {
  df %>% dplyr::mutate(model = model)
}

build_subset97_and_write <- function(
    results, wb, survey_vars, firms97_vec,
    data, experimental_vars = NULL, prefix_map = NULL
) {
  stopifnot(is.list(results), "all" %in% names(results), "subset97" %in% names(results))
  if (is.null(firms97_vec) || length(firms97_vec) == 0) return(results)
  
  firms97_vec <- sort(unique(as.integer(firms97_vec)))
  coef97_long <- list()
  
  for (model in names(results$all)) {
    if (is.null(prefix_map[[model]])) {
      warning("No prefix_map entry for model=", model, "; skipping.")
      next
    }
    
    for (outcome in survey_vars) {
      res_all <- results$all[[model]][[outcome]]
      if (is.null(res_all)) next
      
      message("Recentering results for 97 firms: ", model, " + ", outcome)
      
      # recenter + restrict to firms97
      res97 <- recenter_model_result_to_firms97(res_all, firms97_vec)
      
      # store
      results$subset97[[model]][[outcome]] <- res97
      
      # write matrices + collect coefficients (97 only)
      tmp <- collect_and_write(
        res       = res97,
        outcome   = outcome,
        wb        = wb,
        prefix    = paste0(prefix_map[[model]], "97"),
        model     = model,          # <-- CHANGE #1
        coef_long = coef97_long,
        se_long   = list(),
        rse_long  = list(),
        eb_long   = list()
      )
      coef97_long <- tmp$coef_long
    }
  }
  
  # write Coefficients (97)
  if (length(coef97_long)) {
    coef97_df <- to_wide(
      dplyr::bind_rows(coef97_long),
      id_cols = c("firm_id", "firm", "model")   # <-- CHANGE #2
    )
    
    # add experimental outcomes ONLY here
    if (!is.null(experimental_vars) && length(experimental_vars) > 0) {
      exp_wide <- get_experimental_wide(data, experimental_vars)
      if (!is.null(exp_wide$coef)) {
        coef97_df <- coef97_df %>%
          dplyr::left_join(exp_wide$coef, by = c("firm_id", "firm"))
      }
    }
    
    remove_sheet_safely(wb, "Coefficients (97)")
    openxlsx::addWorksheet(wb, "Coefficients (97)")
    openxlsx::writeData(wb, "Coefficients (97)", coef97_df)
  } else {
    warning("No subset97 coefficients produced (did you run any models + provide firms97?)")
  }
  
  results
}

recenter_model_result_to_firms97 <- function(res_all, firms97) {
  stopifnot(!is.null(res_all$mats$S))
  stopifnot(is.data.frame(res_all$mats$S))
  stopifnot("resp_id" %in% names(res_all$mats$S))
  
  if (!exists("recenter_objects", mode = "function")) {
    stop("recenter_objects() not found in scope.")
  }
  
  firms97 <- sort(unique(as.integer(firms97)))
  
  # --- firm_table restricted to 97 ---
  ft <- res_all$firm_table %>%
    dplyr::filter(firm_id %in% firms97) %>%
    dplyr::arrange(firm_id)
  
  firm_cols97 <- paste0("firm", ft$firm_id)
  
  # --- mats restricted to 97 ---
  S_df_all <- res_all$mats$S
  
  # keep id cols if present
  id_cols <- intersect(c("resp_id", "firm_id"), names(S_df_all))
  
  # restrict score columns
  miss_cols <- setdiff(firm_cols97, names(S_df_all))
  if (length(miss_cols)) {
    stop("S is missing firm columns: ", paste(miss_cols, collapse = ", "))
  }
  
  S_full97 <- as.matrix(S_df_all[, firm_cols97, drop = FALSE])
  
  bread97 <- as.matrix(res_all$mats$bread[firm_cols97, firm_cols97, drop = FALSE])
  cov97   <- as.matrix(res_all$mats$cov  [firm_cols97, firm_cols97, drop = FALSE])
  rcov97  <- as.matrix(res_all$mats$rcov [firm_cols97, firm_cols97, drop = FALSE])
  
  # --- recenter in 97-space (sum-to-zero on this 97-set) ---
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
  
  # keep your convention: rcov = cov * (S'S) * cov
  rcov_c <- bread_c %*% crossprod(S_c) %*% bread_c
  
  # bread / cov / rcov dimnames
  dimnames(bread_c) <- list(firm_cols97, firm_cols97)
  dimnames(cov_c)   <- list(firm_cols97, firm_cols97)
  dimnames(rcov_c)  <- list(firm_cols97, firm_cols97)
  
  # --- write back into firm_table ---
  ft <- ft %>%
    dplyr::mutate(
      estimate = beta_c,
      se  = sqrt(diag(cov_c)),
      rse = sqrt(diag(rcov_c))
    )
  
  # --- rebuild S df (resp_id + optional firm_id + firm<id> columns) ---
  S_df <- cbind(S_df_all[, id_cols, drop = FALSE], as.data.frame(S_c))
  names(S_df)[(ncol(S_df) - length(firm_cols97) + 1):ncol(S_df)] <- firm_cols97
  
  # --- return a res object with same schema ---
  res_97 <- res_all
  res_97$firm_table <- ft
  res_97$mats <- list(
    S     = S_df,      # df w/ resp_id
    bread = bread_c,   # JxJ
    cov   = cov_c,     # JxJ
    rcov  = rcov_c     # JxJ
  )
  
  res_97
}


recenter_objects <- function(beta, S_full = NULL, ...) {
  # ... can include any number of square JxJ matrices (cov=, rcov=, bread=, etc.)
  J <- length(beta)
  C <- diag(J) - matrix(1 / J, J, J)
  
  mats <- list(...)
  
  # center beta
  out <- list(beta = as.numeric(C %*% beta))
  
  # center any provided JxJ matrices
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
  
  # center score-like matrix (n x J): S -> S C'
  if (!is.null(S_full)) {
    S_full <- as.matrix(S_full)
    stopifnot(ncol(S_full) == J)
    out$S <- S_full %*% t(C)
  } else {
    out$S <- NULL
  }
  
  out
}

