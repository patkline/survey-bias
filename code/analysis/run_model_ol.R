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
  
  # keep needed cols + drop NAs + flip scale: 6 - rating
  dat_m <- dat_long %>%
    dplyr::select(dplyr::all_of(c(resp_col, "firm_id", rating_col))) %>%
    dplyr::filter(!is.na(.data[[resp_col]]),
                  !is.na(.data[["firm_id"]]),
                  !is.na(.data[[rating_col]])) %>%
    dplyr::mutate(!!rating_col := 6 - .data[[rating_col]])
  
  # factorize
  dat_m[[resp_col]] <- factor(dat_m[[resp_col]])
  
  # IMPORTANT: make firm_id factor levels equal to numeric firm ids in sorted order
  firm_levels <- sort(unique(as.integer(dat_m$firm_id)))
  dat_m$firm_id <- factor(as.integer(dat_m$firm_id), levels = firm_levels)
  
  J <- length(firm_levels)
  if (J < 2) stop("Not enough firm levels for ordered logit.")
  
  # Impose sum-to-zero from the start
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
  
  # full vcov + estfun (includes thresholds + firm coefs)
  V_all <- as.matrix(stats::vcov(fit))
  S_all <- tryCatch(sandwich::estfun(fit), error = function(e) NULL)
  if (is.null(S_all)) stop("sandwich::estfun(clm) failed; cannot build score matrix.")
  
  # firm params only (drop thresholds)
  firm_theta_names <- grep("^firm_id", names(stats::coef(fit)), value = TRUE)
  firm_theta_names <- firm_theta_names[!grepl("\\|", firm_theta_names)]
  
  # Under contr.sum, clm reports J-1 firm coefficients
  if (length(firm_theta_names) != (J - 1)) {
    warning("Expected J-1 firm parameters under contr.sum; got ",
            length(firm_theta_names), ". Check clm parameter naming.")
  }
  
  # Extract theta/V/S in the reported parameterization (J-1)
  V_th <- V_all[firm_theta_names, firm_theta_names, drop = FALSE]
  S_th <- S_all[, firm_theta_names, drop = FALSE]
  theta_hat <- as.numeric(stats::coef(fit)[firm_theta_names])
  
  # Expand to J using contr.sum identity: last level = -sum(others)
  theta_full <- c(theta_hat, -sum(theta_hat))
  names(theta_full) <- as.character(firm_levels)
  
  # Expansion matrix R: (J x (J-1))
  R <- rbind(diag(J - 1), rep(-1, J - 1))
  
  # Expand covariance: V_full = R V_th R'
  V_full <- R %*% V_th %*% t(R)
  
  # Expand score: S_full = S_th %*% R'
  S_full <- as.matrix(S_th) %*% t(R)
  
  # Rename to entity<id> convention
  entity_ids  <- firm_levels
  entity_cols <- paste0("entity", entity_ids)
  
  colnames(S_full) <- entity_cols
  dimnames(V_full) <- list(entity_cols, entity_cols)
  
  # Robust covariance
  rcov_full <- V_full %*% crossprod(S_full) %*% V_full
  dimnames(rcov_full) <- list(entity_cols, entity_cols)
  
  # Entity names + njobs from id_map
  entity_names <- rep(NA_character_, J)
  entity_njobs <- rep(NA_real_, J)  # NEW
  
  if (!is.null(id_map) && all(c("firm_id", "firm") %in% names(id_map))) {
    tmp <- id_map %>%
      dplyr::select(dplyr::any_of(c("firm_id", "firm", "njobs"))) %>%
      dplyr::distinct()
    entity_names <- tmp$firm[match(entity_ids, tmp$firm_id)]
    if ("njobs" %in% names(tmp)) {  # NEW
      entity_njobs <- tmp$njobs[match(entity_ids, tmp$firm_id)]
    }
  }
  
  entity_tbl <- data.frame(
    entity_type = "Firm",
    entity_id   = as.integer(entity_ids),
    entity      = entity_names,
    njobs       = as.numeric(entity_njobs),   # NEW
    estimate    = as.numeric(theta_full),
    se          = sqrt(diag(V_full)),
    rse         = sqrt(diag(rcov_full)),
    eb          = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # EB step (optional; uses robust SE)
  if (exists("eb_two_step", mode = "function")) {
    ok <- is.finite(entity_tbl$estimate) & is.finite(entity_tbl$rse) & entity_tbl$rse > 0
    if (sum(ok) >= 2) {
      eb_fit <- eb_two_step(theta_hat = entity_tbl$estimate[ok], s = pmax(entity_tbl$rse[ok], 1e-8))
      entity_tbl$eb[ok] <- eb_fit$theta_eb
    }
  }
  
  # Score df: resp_id + entity<id>
  S_df <- as.data.frame(S_full)
  S_df <- cbind(resp_id = dat_m[[resp_col]], S_df)
  
  list(
    fit = fit,
    firm_table = entity_tbl,
    mats = list(
      S     = S_df,
      cov   = V_full,
      rcov  = rcov_full,
      bread = V_full
    )
  )
}