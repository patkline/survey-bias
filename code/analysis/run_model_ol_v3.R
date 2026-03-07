run_model_ol <- function(dat_long, outcome, respondent_col, id_map = NULL,
                         do_recenter = FALSE) {
  
  sanitize_resp_col <- function(df) {
    if ("resp_id" %in% names(df)) return("resp_id")
    if (respondent_col %in% names(df)) return(respondent_col)
    stop("No respondent id column found (expected resp_id or respondent_col).")
  }
  
  get_rating_col <- function(df, outcome) {
    if (outcome %in% names(df)) return(outcome)
    if ("rating" %in% names(df)) return("rating")
    stop("No rating column found for outcome ", outcome,
         " (expected '", outcome, "' or 'rating').")
  }
  
  ensure_firm_id <- function(df, id_map) {
    if ("firm_id" %in% names(df)) return(df)
    if (!is.null(id_map) &&
        all(c("firm", "firm_id") %in% names(id_map)) &&
        ("firm" %in% names(df))) {
      return(dplyr::left_join(df,
                              dplyr::distinct(id_map, firm, firm_id),
                              by = "firm"))
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
  
  # factors (stable ordering)
  dat_m[[resp_col]] <- factor(dat_m[[resp_col]])
  dat_m$firm_id     <- factor(dat_m$firm_id)
  
  firm_levels <- levels(dat_m$firm_id)
  J <- length(firm_levels)
  if (J < 2) stop("Not enough firm levels for ordered logit.")
  
  # impose sum-to-zero identification at estimation time
  contrasts(dat_m$firm_id) <- stats::contr.sum(J)
  
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
  
  # Full vcov + score
  V_all <- as.matrix(stats::vcov(fit))
  S_all <- tryCatch(sandwich::estfun(fit), error = function(e) NULL)
  if (is.null(S_all)) stop("sandwich::estfun(clm) failed; cannot build score matrix.")
  
  # Firm parameters only (drop thresholds like "1|2")
  cf <- stats::coef(fit)
  firm_theta_names <- grep("^firm_id", names(cf), value = TRUE)
  firm_theta_names <- firm_theta_names[!grepl("\\|", firm_theta_names)]
  
  if (length(firm_theta_names) != (J - 1)) {
    warning("Expected J-1 firm parameters under contr.sum; got ",
            length(firm_theta_names), ". Check clm parameter naming.")
  }
  
  # Extract theta-space objects: (J-1)
  theta_hat <- unname(cf[firm_theta_names])
  V_th      <- V_all[firm_theta_names, firm_theta_names, drop = FALSE]
  S_th      <- S_all[, firm_theta_names, drop = FALSE]  # n x (J-1)
  
  # -----------------------------
  # Expand to J via linear map A
  # -----------------------------
  # Standard contr.sum mapping: beta[1:(J-1)] = theta, beta[J] = -sum(theta)
  A <- rbind(diag(J - 1), rep(-1, J - 1))  # J x (J-1)
  
  # Align columns of A to the model's theta ordering.
  # If names look like firm_id2, firm_id3, ..., parse indices; otherwise assume 1:(J-1).
  idx <- suppressWarnings(as.integer(sub("^firm_id", "", firm_theta_names)))
  
  if (all(is.finite(idx)) && length(unique(idx)) == length(idx)) {
    # Some ordinal::clm builds names firm_id2..J; use those indices.
    A_aligned <- A[, idx, drop = FALSE]
    colnames(A_aligned) <- firm_theta_names
  } else {
    # Fall back: assume theta order corresponds to levels 1..(J-1)
    A_aligned <- A
    colnames(A_aligned) <- firm_theta_names
  }
  
  # Expanded beta, bread/cov, score
  beta_full  <- as.numeric(A_aligned %*% theta_hat)                 # length J
  V_full     <- A_aligned %*% V_th %*% t(A_aligned)                 # J x J
  S_full     <- S_th %*% t(A_aligned)                               # n x J
  
  # Name everything as firm<id> (firm_levels are the factor levels; typically numeric firm_ids)
  firm_ids_chr <- as.character(firm_levels)
  firm_cols    <- paste0("firm", firm_ids_chr)
  
  names(beta_full) <- firm_cols
  dimnames(V_full) <- list(firm_cols, firm_cols)
  colnames(S_full) <- firm_cols
  
  # Robust sandwich in J-space using your convention: bread * (S'S) * bread
  bread_full <- V_full
  rcov_full  <- bread_full %*% crossprod(S_full) %*% bread_full
  dimnames(rcov_full) <- list(firm_cols, firm_cols)
  
  # firm lookup table
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
      estimate = as.numeric(beta_full),
      se  = sqrt(diag(V_full)),
      rse = sqrt(diag(rcov_full))
    )
  
  # EB step (optional; uses robust SE)
  firm_tbl$eb <- NA_real_
  if (exists("eb_two_step", mode = "function")) {
    ok <- is.finite(firm_tbl$estimate) & is.finite(firm_tbl$rse) & firm_tbl$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = firm_tbl$estimate[ok],
                            s = pmax(firm_tbl$rse[ok], 1e-8))
      firm_tbl$eb[ok] <- eb_fit$theta_eb
    }
  }
  
  # Store S with ids
  S_df <- as.data.frame(S_full)
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
      bread = bread_full,   # JxJ with firm<id> dimnames
      cov   = V_full,       # JxJ naive covariance (here = bread_full)
      rcov  = rcov_full     # JxJ robust sandwich
    )
  )
}