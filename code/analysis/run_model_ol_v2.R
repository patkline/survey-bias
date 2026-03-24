run_model_ol <- function(dat_long, outcome, respondent_col, id_map = NULL) {
  
  sanitize_resp_col <- function(df) {
    if ("resp_id" %in% names(df)) return("resp_id")
    if (respondent_col %in% names(df)) return(respondent_col)
    stop("No respondent id column found (expected resp_id or respondent_col).")
  }
  
  get_rating_col <- function(df, outcome) {
    if (outcome %in% names(df)) return(outcome)
    if ("rating" %in% names(df)) return("rating")
    stop("No rating column found for outcome ", outcome, " (expected '", outcome, "' or 'rating').")
  }
  
  ensure_firm_id <- function(df, id_map) {
    if ("firm_id" %in% names(df)) return(df)
    if (!is.null(id_map) &&
        all(c("firm", "firm_id") %in% names(id_map)) &&
        ("firm" %in% names(df))) {
      return(dplyr::left_join(df, dplyr::distinct(id_map, firm, firm_id), by = "firm"))
    }
    stop("firm_id not found and could not be constructed from id_map.")
  }
  
  resp_col   <- sanitize_resp_col(dat_long)
  rating_col <- get_rating_col(dat_long, outcome)
  
  dat_long <- ensure_firm_id(dat_long, id_map)
  
  dat_m <- dat_long %>%
    dplyr::select(dplyr::all_of(c(resp_col, "firm_id", rating_col))) %>%
    dplyr::filter(!is.na(.data[[resp_col]]),
                  !is.na(.data[["firm_id"]]),
                  !is.na(.data[[rating_col]]))
  
  # firm_id as factor with stable ordering
  dat_m[[resp_col]] <- factor(dat_m[[resp_col]])
  dat_m$firm_id     <- factor(dat_m$firm_id)
  
  # Treatment coding => firm 1 (first level) omitted
  # options(contrasts = c("contr.sum", "contr.poly"))
  contrasts(dat_m$firm_id) <- stats::contr.treatment(nlevels(dat_m$firm_id), base = 1)
  # contrasts(dat_m$firm_id) <- contr.sum(nlevels(dat_m$firm_id))
  
  # ordered outcome
  if (!is.ordered(dat_m[[rating_col]])) {
    if (is.numeric(dat_m[[rating_col]]) || is.integer(dat_m[[rating_col]])) {
      levs <- sort(unique(dat_m[[rating_col]]))
      dat_m[[rating_col]] <- ordered(dat_m[[rating_col]], levels = levs)
    } else {
      dat_m[[rating_col]] <- ordered(dat_m[[rating_col]])
    }
  }
  
  fml <- stats::as.formula(paste0(rating_col, " ~ firm_id"))
  fit <- ordinal::clm(formula = fml, data = dat_m, link = "logit", Hess = TRUE)
  
  # --- Get full (theta-space) covariance + scores ---
  V_all <- as.matrix(stats::vcov(fit))
  S_all <- tryCatch(sandwich::estfun(fit), error = function(e) NULL)
  if (is.null(S_all)) stop("sandwich::estfun(clm) failed; cannot build score matrix.")
  
  # firm params only (drop thresholds)
  firm_theta_names <- grep("^firm_id", names(stats::coef(fit)), value = TRUE)
  firm_theta_names <- firm_theta_names[!grepl("\\|", firm_theta_names)]
  
  firm_levels <- levels(dat_m$firm_id)
  J <- length(firm_levels)
  if (J < 2) stop("Not enough firm levels for ordered logit.")
  
  # Under contr.treatment(base=1), we expect J-1 firm coefficients
  if (length(firm_theta_names) != (J - 1)) {
    warning("Expected J-1 firm parameters under contr.treatment; got ",
            length(firm_theta_names), ". Check clm parameter naming.")
  }
  
  # Extract (J-1) objects
  V_th <- V_all[firm_theta_names, firm_theta_names, drop = FALSE]
  S_th <- S_all[, firm_theta_names, drop = FALSE]
  theta_hat <- as.numeric(stats::coef(fit)[firm_theta_names])
  
  # ---------------------------------------------------------
  # Expand to J by inserting 0 for omitted firm (base=1)
  # ---------------------------------------------------------
  # Map each theta name to a firm level index (2..J)
  # e.g. "firm_id2" -> 2
  lev_idx <- suppressWarnings(as.integer(sub("^firm_id", "", firm_theta_names)))
  if (anyNA(lev_idx)) {
    stop("Could not parse firm indices from firm_theta_names: ",
         paste(firm_theta_names, collapse = ", "),
         ". Adjust the parsing rule for your clm naming.")
  }
  
  # Build full theta (length J)
  theta_full <- rep(0, J)
  names(theta_full) <- firm_levels
  # Fill firms 2..J (or whatever indices are present)
  theta_full[lev_idx] <- theta_hat
  
  # Build full V (J x J) with 0 row/col for firm 1
  V_full <- matrix(0, nrow = J, ncol = J, dimnames = list(firm_levels, firm_levels))
  V_full[lev_idx, lev_idx] <- V_th
  
  # Build full S (n x J) with 0 column for firm 1
  S_full <- matrix(0, nrow = nrow(S_th), ncol = J,
                   dimnames = list(rownames(S_th), firm_levels))
  S_full[, lev_idx] <- S_th
  
  # If you want "firm<id>" column names for downstream compatibility:
  firm_cols <- paste0("firm", firm_levels)  # only if firm_levels are numeric IDs
  
  # Recenter
  rec <- recenter_objects(
    beta   = theta_full,
    cov    = V_full,
    S_full = S_full
  )
  
  beta_c <- rec$beta
  cov_c  <- rec$cov
  S_c    <- rec$S
  
  # name firms by actual levels (these are *levels* of firm_id factor)
  firm_levels <- levels(dat_m$firm_id)
  firm_ids_chr <- as.character(firm_levels)
  firm_cols <- paste0("firm", firm_ids_chr)
  
  names(beta_c) <- firm_cols
  colnames(cov_c) <- rownames(cov_c) <- firm_cols
  colnames(S_c) <- firm_cols
  
  # Robust covariance (sandwich, using your "score then crossprod" convention)
  rcov_c <- cov_c %*% crossprod(S_c) %*% cov_c
  colnames(rcov_c) <- rownames(rcov_c) <- firm_cols
  
  # firm lookup
  firm_tbl <- data.frame(firm_id = as.integer(firm_ids_chr), stringsAsFactors = FALSE)
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    firm_tbl <- firm_tbl %>%
      dplyr::left_join(dplyr::distinct(id_map, firm_id, firm), by = "firm_id") %>%
      dplyr::arrange(match(as.character(firm_id), firm_ids_chr))
  } else {
    firm_tbl$firm <- NA_character_
  }
  
  firm_tbl <- firm_tbl %>%
    dplyr::mutate(
      estimate = beta_c,
      se  = sqrt(diag(cov_c)),
      rse = sqrt(diag(rcov_c))
    )
  
  # EB step (optional; uses robust SE)
  firm_tbl$eb <- NA_real_
  if (exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_tbl$estimate) & is.finite(firm_tbl$rse) & firm_tbl$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_tbl$estimate[ok], s = pmax(firm_tbl$rse[ok], 1e-8))
      firm_tbl$eb[ok] <- eb_fit$theta_eb
    }
  }
  
  # Store S with ids (same convention you used)
  S_df <- as.data.frame(S_c)
  S_df <- cbind(
    resp_id = dat_m[[resp_col]],
    firm_id = dat_m$firm_id,
    S_df
  )
  
  list(
    fit = fit,
    firm_table = firm_tbl %>% dplyr::select(firm_id, firm, estimate, se, rse, eb),
    mats = list(
      S     = S_df,
      cov   = cov_c,
      rcov  = rcov_c,
      bread = cov_c
    )
    
  )
}
