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
  
  dat_m[[resp_col]] <- factor(dat_m[[resp_col]])
  dat_m$firm_id     <- factor(dat_m$firm_id)
  
  # sum-to-zero contrasts => (J-1) free firm parameters
  contrasts(dat_m$firm_id) <- contr.sum(nlevels(dat_m$firm_id))
  
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
  
  J <- nlevels(dat_m$firm_id)
  if (J < 2) stop("Not enough firm levels for ordered logit.")
  if (length(firm_theta_names) != (J - 1)) {
    warning("Expected J-1 firm parameters under contr.sum; got ", length(firm_theta_names),
            ". Check parameter naming from ordinal::clm.")
  }
  
  V_th <- V_all[firm_theta_names, firm_theta_names, drop = FALSE]
  S_th <- S_all[, firm_theta_names, drop = FALSE]
  
  # --- Expand theta -> full J-vector (no centering yet): alpha_J = -sum_{1..J-1} alpha_j ---
  build_A_sum0 <- function(J) {
    A <- matrix(0, nrow = J, ncol = J - 1)
    A[1:(J - 1), 1:(J - 1)] <- diag(J - 1)
    A[J, ] <- -1
    A
  }
  A <- build_A_sum0(J)
  
  theta_hat <- as.numeric(stats::coef(fit)[firm_theta_names])
  beta_full <- as.numeric(A %*% theta_hat)              # length J
  cov_full  <- A %*% V_th %*% t(A)                      # J x J
  S_full    <- S_th %*% t(A)                            # n_obs x J
  
  # name firms by actual levels (these are *levels* of firm_id factor)
  firm_levels <- levels(dat_m$firm_id)
  firm_ids_chr <- as.character(firm_levels)
  firm_cols <- paste0("firm", firm_ids_chr)
  
  names(beta_full) <- firm_cols
  colnames(cov_full) <- rownames(cov_full) <- firm_cols
  colnames(S_full) <- firm_cols
  
  # --- Recenter via your helper: sum-to-zero projector C ---
  # recenter_objects(beta, Binv, cov, S_full) returns list(beta, Binv, cov, S)
  # We don't use Binv in OL, so pass a placeholder and ignore it on return.
  if (!exists("recenter_objects", mode = "function")) {
    stop("recenter_objects() not found in scope. Source it before calling run_model_ol().")
  }
  Binv_placeholder <- diag(length(beta_full))
  rec <- recenter_objects(beta = beta_full, Binv = Binv_placeholder, cov = cov_full, S_full = S_full)
  
  beta_c <- as.numeric(rec$beta)
  names(beta_c) <- firm_cols
  
  cov_c <- as.matrix(rec$cov)
  colnames(cov_c) <- rownames(cov_c) <- firm_cols
  
  S_c <- as.matrix(rec$S)
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
      cov   = cov_alpha,
      rcov  = rcov_alpha,
      bread = cov_alpha   # <-- ADD THIS
    )
  )
}
