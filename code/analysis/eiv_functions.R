# ------------------------------------------------------------------------------
# Purpose: Run EIV (univariate or bivariate)
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
run_eiv_one <- function(
    coef_df_wide,
    noise_mat,
    model_value,
    lhs_var,
    rhs_vars,
    id_col = "entity_id",      # set "firm_id" if needed
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = NULL,        # e.g. "njobs" or "weights"; if NULL => equal weights
    use_fe = TRUE              # set FALSE for within/between industry EIV (no FE)
) {
  stopifnot(is.data.frame(coef_df_wide))
  stopifnot(model_col %in% names(coef_df_wide))
  if (use_fe) stopifnot(fe_col %in% names(coef_df_wide))
  stopifnot(id_col %in% names(coef_df_wide))
  stopifnot(length(rhs_vars) >= 1)
  
  # --- 1) Restrict to correct model ---
  df_m <- coef_df_wide |> dplyr::filter(.data[[model_col]] == model_value)
  
  if (!is.null(weights_col)) {
    # If you want to ALSO require non-missing weights, uncomment the filter line.
    # reg_data <- reg_data |> dplyr::filter(!is.na(.data[[weights_col]]))
    df_m[["weight"]] <- df_m[[weights_col]]
  } else {
    df_m[["weight"]] <- 1
  }
  
  # --- required columns check ---
  need_cols <- unique(c(id_col, if (use_fe) fe_col, lhs_var, rhs_vars, "weight"))
  miss_cols <- setdiff(need_cols, names(df_m))
  if (length(miss_cols) > 0) {
    stop(
      "run_eiv_one(): Missing columns in coef_df_wide for model ", model_value, ": ",
      paste(miss_cols, collapse = ", ")
    )
  }



  reg_data <- df_m |>
    dplyr::select(dplyr::all_of(need_cols)) |>
    dplyr::filter(!is.na(.data[[lhs_var]]))

  if (use_fe) {
    reg_data <- reg_data |> dplyr::filter(!is.na(.data[[fe_col]]))
  }
  
  for (rv in rhs_vars) {
    reg_data <- reg_data |> dplyr::filter(!is.na(.data[[rv]]))
  }
  
  
  # --- Build Sigma_error for RHS vars ---
  if (is.null(dimnames(noise_mat)) || is.null(rownames(noise_mat)) || is.null(colnames(noise_mat))) {
    stop("run_eiv_one(): noise_mat must have rownames/colnames equal to variable names.")
  }
  if (!all(rhs_vars %in% rownames(noise_mat)) || !all(rhs_vars %in% colnames(noise_mat))) {
    missN <- setdiff(rhs_vars, intersect(rownames(noise_mat), colnames(noise_mat)))
    stop("run_eiv_one(): RHS vars missing from noise_mat dimnames: ", paste(missN, collapse = ", "))
  }

  Sigma1 <- as.matrix(noise_mat[rhs_vars, rhs_vars, drop = FALSE])

  # --- formulas ---
  rhs_part <- paste(rhs_vars, collapse = " + ")
  f_no_fe  <- stats::as.formula(paste(lhs_var, "~", rhs_part))

  # --- FE dummies and FE formula (only when use_fe = TRUE) ---
  if (use_fe) {
    fe_mat <- stats::model.matrix(
      stats::as.formula(paste0("~ factor(", fe_col, ") - 1")),
      data = reg_data
    )
    colnames(fe_mat) <- paste0("fe_", seq_len(ncol(fe_mat)))
    reg_data <- dplyr::bind_cols(reg_data, as.data.frame(fe_mat))
    fe_cols <- colnames(fe_mat)

    Sigma2 <- matrix(
      0,
      nrow = length(rhs_vars) + length(fe_cols),
      ncol = length(rhs_vars) + length(fe_cols),
      dimnames = list(c(rhs_vars, fe_cols), c(rhs_vars, fe_cols))
    )
    Sigma2[rhs_vars, rhs_vars] <- Sigma1

    f_fe <- stats::as.formula(paste(lhs_var, "~ 0 +", paste(c(rhs_vars, fe_cols), collapse = " + ")))
  }
  
  # --- helper to extract coef + se for RHS vars only ---
  extract_coef_rows <- function(fit, coef_tag) {
    if (is.null(fit) || is.null(fit$vcov)) return(data.frame())
    cf <- stats::coef(fit)
    V  <- fit$vcov
    
    out <- lapply(rhs_vars, function(rv) {
      est <- if (rv %in% names(cf)) unname(cf[[rv]]) else NA_real_
      se  <- if (!is.null(dim(V)) && rv %in% rownames(V) && rv %in% colnames(V)) sqrt(V[rv, rv]) else NA_real_
      data.frame(
        model      = model_value,
        lhs        = lhs_var,
        formula    = rhs_part,
        rhs        = rv,
        coef       = coef_tag,   # 1=no FE, 2=FE
        sample_est = est,
        sample_se  = se,
        n          = nrow(reg_data),
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(out)
  }
  
  message("Running EIV: Model = ", model_value, ", LHS = ", lhs_var, ", RHS = ", rhs_part)
  
  fit1 <- tryCatch(
    eivreg(f_no_fe, data = reg_data, Sigma_error = Sigma1, weights = reg_data[["weight"]]),
    error = function(e) NULL
  )
  
  if (use_fe) {
    fit2 <- tryCatch(
      eivreg(f_fe, data = reg_data, Sigma_error = Sigma2, weights = reg_data[["weight"]]),
      error = function(e) NULL
    )

    dplyr::bind_rows(
      extract_coef_rows(fit1, coef_tag = 1L),
      extract_coef_rows(fit2, coef_tag = 2L)
    )
  } else {
    extract_coef_rows(fit1, coef_tag = 1L)
  }
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
    coef_df_wide,
    noise_mats_97,
    models = names(noise_mats_97),
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = NULL,
    use_fe = TRUE
) {
  stopifnot(is.list(regs), is.data.frame(coef_df_wide))
  stopifnot(is.list(noise_mats_97))
  
  models <- intersect(models, names(noise_mats_97))
  if (length(models) == 0) stop("run_eiv_suite(): no models to run (models not in noise_mats_97).")
  
  rows <- list()
  k <- 1L
  
  for (spec in regs) {
    lhs_var <- spec$lhs
    rhs_vars <- spec$rhs
    
    for (model_value in models) {
      noise_mat <- noise_mats_97[[model_value]]
      if (is.null(noise_mat)) next
      
      out_df <- run_eiv_one(
        coef_df_wide = coef_df_wide,
        noise_mat    = noise_mat,
        model_value  = model_value,
        lhs_var      = lhs_var,
        rhs_vars     = rhs_vars,
        id_col       = id_col,
        model_col    = model_col,
        fe_col       = fe_col,
        weights_col  = weights_col,
        use_fe       = use_fe
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
    coef_df_wide,
    noise_mats_97,
    models = names(noise_mats_97),
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = NULL,
    use_fe = TRUE
) {
  eiv_df <- run_eiv_suite(
    regs         = regs,
    coef_df_wide = coef_df_wide,
    noise_mats_97 = noise_mats_97,
    models       = models,
    id_col       = id_col,
    model_col    = model_col,
    fe_col       = fe_col,
    weights_col  = weights_col,
    use_fe       = use_fe
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