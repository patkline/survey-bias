bs_eiv_run <- function(filepath, industry_map, lhs_var, rhs_var,
                       borda = FALSE,
                       n_cores = min(32, max(1, parallel::detectCores(logical = TRUE) - 1)),
                       use_psock_on_windows = FALSE,
                       center = FALSE, iterations = 200, weights = NULL) { 
  
  # ---- helpers ---------------------------------------------------------------
  read_baseline <- function(sheet, col) {
    read.xlsx(filepath, sheet = sheet) %>%
      dplyr::select(firm, firm_id, !!rlang::sym(col))
  }
  
  # ---- 1) Read LHS (coefficients / Borda means) + its SEs (for lhs_iters) ----
  coeffs <- read.xlsx(filepath, sheet = "Coefficients") %>%
    dplyr::select(firm, firm_id, !!rlang::sym(lhs_var))
  
  lhs_se_col <- paste0(lhs_var, "_se")
  ses <- read.xlsx(filepath, sheet = "Standard_Errors") %>%
    dplyr::select(firm, firm_id, !!rlang::sym(lhs_se_col)) %>%
    dplyr::pull(!!rlang::sym(lhs_se_col))
  
  lhs_vec <- coeffs %>% dplyr::pull(!!rlang::sym(lhs_var))
  
  # LHS bootstrap (iter_lhs_0..B based on its SEs)
  set.seed(123)
  n_firms      <- length(lhs_vec)
  n_draws      <- iterations
  normal_draws <- matrix(stats::rnorm(n_firms * n_draws),
                         nrow = n_firms, ncol = n_draws)
  
  boot_mat <- matrix(lhs_vec, nrow = n_firms, ncol = n_draws) +
    matrix(ses,     nrow = n_firms, ncol = n_draws) * normal_draws
  
  boot_df <- cbind(lhs_vec, boot_mat) %>% as.data.frame()
  colnames(boot_df)[1:(n_draws + 1)] <- paste0("iter_lhs_", 0:n_draws)
  boot_df$firm_id <- coeffs$firm_id
  
  # We'll return this so caller still has access to lhs iters
  lhs_boot <- boot_df
  
  # ---- 2) Read RHS bootstrap draws + signal sheet ----------------------------
  signal_df <- NULL
  
  if (!borda) {
    # PL path: RHS from bs_<rhs_var>, signal from pl_signal_<rhs_var>
    bs_rhs <- read.xlsx(filepath, sheet = paste0("bs_", rhs_var))
    # Expect cols: firm_id, firm, iter_0, iter_1, ..., iter_B
    signal_df <- read.xlsx(filepath, sheet = paste0("pl_s_", rhs_var)) %>% filter(all_firms == FALSE)
    
  } else {
    # Borda path: RHS from borda_* sheets, signal from borda_signal_<rhs_var>
    
    # baseline RHS (iter_0) from Borda score
    rhs0 <- read_baseline("borda_score", rhs_var) %>%
      dplyr::rename(iter_0 = !!rlang::sym(rhs_var))
    
    # bootstrap RHS means (iter_1..B) from borda_<rhs_var>
    bs_rhs <- read.xlsx(filepath, sheet = paste0("borda_", rhs_var))
    
    # If borda_<rhs_var> doesn't contain iter_0, join it from rhs0
    if (!"iter_0" %in% names(bs_rhs)) {
      bs_rhs <- dplyr::left_join(rhs0, bs_rhs, by = c("firm","firm_id"))
    }
    
    signal_df <- read.xlsx(filepath, sheet = paste0("b_s_", rhs_var)) %>% filter(all_firms == FALSE)
  }
  
  # ---- 3) Merge LHS, RHS and industry map -----------------------------------
  reg_data <- coeffs %>%
    dplyr::left_join(bs_rhs,  by = c("firm","firm_id")) %>%
    dplyr::left_join(boot_df, by = "firm_id") %>%
    dplyr::left_join(industry_map, by = "firm_id") %>%
    dplyr::filter(!is.na(.data[[lhs_var]])) %>%
    dplyr::left_join(weights, by = "firm_id") %>%
    dplyr::filter(!is.na(.data[["aer_naics2"]]))
  
  # ---- 4) Industry dummies ---------------------------------------------------
  fe_mat <- stats::model.matrix(~ factor(reg_data$aer_naics2) - 1)
  colnames(fe_mat) <- paste0("dummy", seq_len(ncol(fe_mat)))
  reg_data <- dplyr::bind_cols(reg_data, as.data.frame(fe_mat))
  dummies  <- colnames(fe_mat)
  
  # ---- 5) Formulas -----------------------------------------------------------
  f1 <- stats::as.formula(paste(lhs_var, "~", rhs_var))
  rhs_and_fe <- if (length(dummies)) {
    paste(rhs_var, "+", paste(dummies, collapse = " + "))
  } else {
    rhs_var
  }
  f2 <- stats::as.formula(paste(lhs_var, "~ 0 +", rhs_and_fe))
  
  # ---- 6) Iterations to run --------------------------------------------------
  rhs_iters  <- grep("^iter_\\d+$",      names(reg_data), value = TRUE)   # iter_0..B
  lhs_iters  <- grep("^iter_lhs_\\d+$",  names(reg_data), value = TRUE)   # iter_lhs_0..B
  
  if (!length(rhs_iters)) stop("No RHS bootstrap columns found (iter_#).")
  
  max_iter <- max(as.integer(sub("^iter_", "", rhs_iters)))
  iters    <- 0:max_iter
  
  # Keep only what's actually needed
  keep_cols <- unique(c(
    "firm","firm_id", "weights",
    lhs_var,
    rhs_iters,
    lhs_iters,
    dummies
  ))
  reg_data <- reg_data[, intersect(keep_cols, names(reg_data)), drop = FALSE]
  
  gc()
  
  # ---- 7) One iteration ------------------------------------------------------
  eiv_iter <- function(i) {
    message("EIV iteration: ", i)
    
    iter_rhs <- reg_data[[paste0("iter_",     i)]]
    iter_lhs <- reg_data[[paste0("iter_lhs_", i)]]
    weights <- reg_data[[paste0("weights")]]
    
    if (center) {
      reg_data[[rhs_var]] <- iter_rhs - mean(iter_rhs, na.rm = TRUE)
      iter_rhs <- reg_data[[rhs_var]]
    } else {
      reg_data[[rhs_var]] <- iter_rhs
    }
    
    reg_data[[lhs_var]] <- iter_lhs
    
    # ---- TOTAL VARIANCE of RHS ----------------------------------------------
    tot_var <- stats::var(iter_rhs, na.rm = TRUE)
    
    # ---- Pull Katz-corrected signal sigma2_dot for this iteration -----------
    sigma2_dot_i <- NA_real_
    if (!is.null(signal_df) &&
        all(c("iter", "sigma2_dot") %in% names(signal_df))) {
      j <- match(i, signal_df$iter)
      if (!is.na(j)) sigma2_dot_i <- signal_df$sigma2_dot[j]
    }
    
    # Conservative fallback: if no sigma2_dot found, treat signal as 0
    if (is.na(sigma2_dot_i)) {
      sigma2_dot_i <- 0
    }
    
    # ---- Measurement-error variance (sigma_error): tot_var - sigma2_dot -----
    sigma_error <- tot_var - sigma2_dot_i
    if (is.na(sigma_error) || sigma_error < 0) sigma_error <- 0
    
    # Sigma_error is scalar variance for RHS; embed in Sigma matrices
    Sigma1 <- matrix(sigma_error, 1, 1, dimnames = list(rhs_var, rhs_var))
    p2     <- 1 + length(dummies)
    Sigma2 <- matrix(0, p2, p2,
                     dimnames = list(c(rhs_var, dummies),
                                     c(rhs_var, dummies)))
    Sigma2[1,1] <- sigma_error
    
    # ---- EIV regressions -----------------------------------------------------
    m1_vals <- tryCatch({
      m1 <- eivreg(f1, data = reg_data, Sigma_error = Sigma1)
      list(coef = coef(m1)[rhs_var],
           se   = sqrt(m1$vcov[rhs_var, rhs_var]))
    }, error = function(e) list(coef = NA_real_, se = NA_real_))
    
    m1_vals_w <- tryCatch({
      m1 <- eivreg(f1, data = reg_data, Sigma_error = Sigma1, weights = weights)
      list(coef = coef(m1)[rhs_var],
           se   = sqrt(m1$vcov[rhs_var, rhs_var]))
    }, error = function(e) list(coef = NA_real_, se = NA_real_))
    
    m2_vals <- tryCatch({
      m2 <- eivreg(f2, data = reg_data, Sigma_error = Sigma2)
      list(coef = coef(m2)[rhs_var],
           se   = sqrt(m2$vcov[rhs_var, rhs_var]))
    }, error = function(e) list(coef = NA_real_, se = NA_real_))
    
    m2_vals_w <- tryCatch({
      m2 <- eivreg(f2, data = reg_data, Sigma_error = Sigma2, weights = weights)
      list(coef = coef(m2)[rhs_var],
           se   = sqrt(m2$vcov[rhs_var, rhs_var]))
    }, error = function(e) list(coef = NA_real_, se = NA_real_))
    
    data.frame(
      lhs         = lhs_var,
      rhs         = rhs_var,
      iter        = i,
      total_var   = tot_var,
      sigma2_dot  = sigma2_dot_i,
      sigma_error = sigma_error,
      check_lhs   = mean(reg_data[[lhs_var]], na.rm = TRUE),
      check_rhs   = mean(reg_data[[rhs_var]], na.rm = TRUE),
      coef1       = m1_vals$coef, se1 = m1_vals$se,
      coef2       = m2_vals$coef, se2 = m2_vals$se,
      coef3     = m1_vals_w$coef, se3 = m1_vals_w$se,
      coef4     = m2_vals_w$coef, se4 = m2_vals_w$se,
      stringsAsFactors = FALSE
    )
  }
  
  # ---- 8) Run over all iterations -------------------------------------------
  out_list <- lapply(iters, eiv_iter)
  
  list(
    eiv      = dplyr::bind_rows(out_list),
    lhs_boot = lhs_boot
  )
}
