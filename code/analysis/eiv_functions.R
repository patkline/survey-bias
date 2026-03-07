# ------------------------------------------------------------------------------
# Purpose: Run EIV (univariate or bivariate)
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
run_eiv_one <- function(
    coef97_df,
    industry_map,
    noise_mat,
    model_value,
    lhs_var,
    rhs_vars,
    weights_df = NULL,
    weights_col = "weights"
) {
  stopifnot(is.data.frame(coef97_df))
  stopifnot("firm_id" %in% names(coef97_df))
  stopifnot("model" %in% names(coef97_df))
  stopifnot(all(c("firm_id", "aer_naics2") %in% names(industry_map)))
  stopifnot(length(rhs_vars) >= 1)
  
  # --- filter to model ---
  df_m <- coef97_df |> dplyr::filter(.data$model == model_value)
  
  # --- keep needed columns + join industry ---
  need_cols <- unique(c("firm_id", lhs_var, rhs_vars))
  miss_cols <- setdiff(need_cols, names(df_m))
  if (length(miss_cols) > 0) {
    stop("run_eiv_one(): Missing columns in coef97_df for model ", model_value, ": ",
         paste(miss_cols, collapse = ", "))
  }
  
  reg_data <- df_m |>
    dplyr::select(dplyr::all_of(need_cols)) |>
    dplyr::left_join(industry_map |> dplyr::select(firm_id, aer_naics2), by = "firm_id") |>
    dplyr::filter(!is.na(.data[[lhs_var]]), !is.na(.data[["aer_naics2"]]))
  
  # --- weights (optional) ---
  if (!is.null(weights_df)) {
    stopifnot("firm_id" %in% names(weights_df))
    stopifnot(weights_col %in% names(weights_df))
    reg_data <- reg_data |>
      dplyr::left_join(weights_df |> dplyr::select(firm_id, dplyr::all_of(weights_col)), by = "firm_id")
    reg_data[[weights_col]] <- ifelse(is.na(reg_data[[weights_col]]), 1, reg_data[[weights_col]])
  } else {
    reg_data[[weights_col]] <- 1
  }
  
  # --- FE dummies (industry) ---
  fe_mat <- stats::model.matrix(~ factor(reg_data$aer_naics2) - 1)
  colnames(fe_mat) <- paste0("fe_", seq_len(ncol(fe_mat)))
  reg_data <- dplyr::bind_cols(reg_data, as.data.frame(fe_mat))
  fe_cols <- colnames(fe_mat)
  
  # --- Build Sigma_error for RHS vars ---
  # noise_mat must have row/colnames matching RHS var names
  if (is.null(dimnames(noise_mat)) || is.null(rownames(noise_mat)) || is.null(colnames(noise_mat))) {
    stop("run_eiv_one(): noise_mat must have rownames/colnames equal to variable names.")
  }
  if (!all(rhs_vars %in% rownames(noise_mat)) || !all(rhs_vars %in% colnames(noise_mat))) {
    missN <- setdiff(rhs_vars, intersect(rownames(noise_mat), colnames(noise_mat)))
    stop("run_eiv_one(): RHS vars missing from noise_mat dimnames: ", paste(missN, collapse = ", "))
  }
  
  Sigma1 <- as.matrix(noise_mat[rhs_vars, rhs_vars, drop = FALSE])
  
  # With FE: expand to include FE cols with 0 measurement error
  Sigma2 <- matrix(0,
                   nrow = length(rhs_vars) + length(fe_cols),
                   ncol = length(rhs_vars) + length(fe_cols),
                   dimnames = list(c(rhs_vars, fe_cols), c(rhs_vars, fe_cols)))
  Sigma2[rhs_vars, rhs_vars] <- Sigma1
  
  # --- formulas ---
  rhs_part <- paste(rhs_vars, collapse = " + ")
  f_no_fe  <- stats::as.formula(paste(lhs_var, "~", rhs_part))
  f_fe     <- stats::as.formula(paste(lhs_var, "~ 0 +", paste(c(rhs_vars, fe_cols), collapse = " + ")))
  
  # --- helper to extract coef + se for each RHS var ---
  extract_coef_rows <- function(fit, coef_tag) {
    if (is.null(fit) || is.null(fit$vcov)) {
      return(data.frame())
    }
    cf <- stats::coef(fit)
    V  <- fit$vcov
    
    out <- lapply(rhs_vars, function(rv) {
      est <- if (rv %in% names(cf)) unname(cf[[rv]]) else NA_real_
      se  <- if (!is.null(dim(V)) && rv %in% rownames(V) && rv %in% colnames(V)) sqrt(V[rv, rv]) else NA_real_
      data.frame(
        model       = model_value,
        lhs         = lhs_var,
        formula     = rhs_part,      # "rhs1 + rhs2 + ..."
        rhs         = rv,            # the coefficient being reported
        coef        = coef_tag,      # 1=no FE, 2=FE
        sample_est  = est,
        sample_se   = se,
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(out)
  }
  
  message("Running EIV: Model = ", model_value, ", formula = ", f_no_fe)
  
  # --- run EIV (no FE) ---
  fit1 <- tryCatch(
    eivreg(f_no_fe, data = reg_data, Sigma_error = Sigma1, weights = reg_data[[weights_col]]),
    error = function(e) NULL
  )
  
  # --- run EIV (with FE) ---
  fit2 <- tryCatch(
    eivreg(f_fe, data = reg_data, Sigma_error = Sigma2, weights = reg_data[[weights_col]]),
    error = function(e) NULL
  )
  
  rows1 <- extract_coef_rows(fit1, coef_tag = 1L)
  rows2 <- extract_coef_rows(fit2, coef_tag = 2L)
  
  dplyr::bind_rows(rows1, rows2)
}
# ==============================================================================
# Outer function you call: run_eiv_suite()
# (Assumes you already have an inner function named run_eiv_once() in your codebase
#  that returns the row-format you described.)
# If you DON'T have it yet, keep this outer call and I’ll match it to your inner
# function once you paste your current run_eiv_once().
# ==============================================================================
# ==============================================================================
# Outer runner: loops over regressions x models and calls run_eiv_one()
# ==============================================================================

run_eiv_suite <- function(
    regs,
    coef97_df,
    industry_map,
    noise_mats_97,
    models = names(noise_mats_97),
    weights_df = NULL,
    weights_col = "weights"
) {
  stopifnot(is.list(regs), is.data.frame(coef97_df), is.data.frame(industry_map))
  stopifnot(is.list(noise_mats_97))
  
  # default models = intersection of requested models and available noise mats
  models <- intersect(models, names(noise_mats_97))
  if (length(models) == 0) stop("run_eiv_suite(): no models to run (models not in noise_mats_97).")
  
  rows <- list()
  k <- 1L
  
  for (spec in regs) {
    lhs <- spec$lhs
    rhs <- spec$rhs
    
    for (m in models) {
      nm <- noise_mats_97[[m]]
      if (is.null(nm)) next
      
      out_df <- run_eiv_one(
        coef97_df    = coef97_df,
        industry_map = industry_map,
        noise_mat    = nm,
        model_value  = m,
        lhs_var      = lhs,
        rhs_vars     = rhs,
        weights_df   = weights_df,
        weights_col  = weights_col
      )
      
      if (!is.null(out_df) && nrow(out_df) > 0) {
        rows[[k]] <- out_df
        k <- k + 1L
      }
    }
  }
  
  dplyr::bind_rows(rows)
}

# ==============================================================================
# Convenience writer to Excel
# ==============================================================================

write_eiv_sheet <- function(
    wb,
    sheet_name = "EIV",
    regs,
    coef97_df,
    industry_map,
    noise_mats_97,
    models = names(noise_mats_97),
    weights_df = NULL,
    weights_col = "weights"
) {
  eiv_df <- run_eiv_suite(
    regs         = regs,
    coef97_df    = coef97_df,
    industry_map = industry_map,
    noise_mats_97 = noise_mats_97,
    models       = models,
    weights_df   = weights_df,
    weights_col  = weights_col
  )
  
  remove_sheet_safely(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, eiv_df)
  
  invisible(eiv_df)
}


# -------------------------------------------------------------------
# Build a JxJ NOISE matrix from variance_df + covariance_df
#  - diag(j)   = variance_df$noise for outcome j
#  - offdiag   = covariance_df$noise for (lhs, rhs)
#  - symmetric, with dimnames = outcomes
# -------------------------------------------------------------------
build_noise_matrix <- function(
    variance_df,
    covariance_df,
    outcomes = NULL,
    subset_value = "all",
    model_value  = "OL",
    subset_col = "subset",
    model_col  = "model",
    outcome_col = "outcome",
    lhs_col = "lhs",
    rhs_col = "rhs"
) {
  stopifnot(is.data.frame(variance_df), is.data.frame(covariance_df))
  stopifnot(all(c(subset_col, model_col, outcome_col, "noise") %in% names(variance_df)))
  stopifnot(all(c(subset_col, model_col, lhs_col, rhs_col, "noise") %in% names(covariance_df)))
  
  # determine outcome universe
  if (is.null(outcomes)) {
    outcomes_var <- unique(variance_df[[outcome_col]])
    outcomes_cov <- unique(c(covariance_df[[lhs_col]], covariance_df[[rhs_col]]))
    outcomes <- sort(unique(c(outcomes_var, outcomes_cov)))
    outcomes <- outcomes[!is.na(outcomes) & nzchar(outcomes)]
  }
  
  J <- length(outcomes)
  N <- matrix(NA_real_, nrow = J, ncol = J, dimnames = list(outcomes, outcomes))
  
  # --- fill diagonal from variance_df ---
  vsub <- variance_df |>
    dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value) |>
    dplyr::select(dplyr::all_of(c(outcome_col, "noise")))
  
  diag(N) <- as.numeric(vsub$noise[match(outcomes, vsub[[outcome_col]])])
  
  # --- fill off-diagonals from covariance_df ---
  csub <- covariance_df |>
    dplyr::filter(.data[[subset_col]] == subset_value, .data[[model_col]] == model_value) |>
    dplyr::select(dplyr::all_of(c(lhs_col, rhs_col, "noise")))
  
  if (nrow(csub) > 0) {
    for (i in seq_len(nrow(csub))) {
      a <- as.character(csub[[lhs_col]][i])
      b <- as.character(csub[[rhs_col]][i])
      val <- as.numeric(csub$noise[i])
      
      if (!is.na(a) && !is.na(b) && a %in% outcomes && b %in% outcomes) {
        N[a, b] <- val
        N[b, a] <- val
      }
    }
  }
  
  N
}