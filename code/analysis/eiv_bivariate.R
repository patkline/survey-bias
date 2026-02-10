eiv_run <- function(filepath, industry_map, lhs_var, rhs_var1, rhs_var2,
                       borda = FALSE,
                       center = FALSE, weights = NULL) { 
  
  rhs_vars <- c(rhs_var1, rhs_var2)
  stopifnot(length(rhs_vars) %in% c(1, 2))
  

  
  # ---- helpers ---------------------------------------------------------------
  read_coeff <- function(df, col) {
     df %>%
      dplyr::select(firm_id, !!rlang::sym(col))
  }
  
  read_noise <- function(df, var1, var2 = NULL, col) {
    if (is.null(var2)) {
      # Use base R subsetting to avoid data masking issues
      subset_df <- df[df$all_firms == FALSE & df$lhs == var1, ]
      unique(subset_df[[col]])
    } else {
      # Use base R subsetting to avoid data masking issues
      subset_df <- df[df$all_firms == FALSE &
                      ((df$lhs == var1 & df$rhs == var2) |
                       (df$lhs == var2 & df$rhs == var1)), ]
      subset_df[[col]]
    }
  }
  
  # Read in data
  rhs_sheet <- if (!borda) "Coefficients" else "borda_score"
  var_sheet <- if (!borda) "pairwise_summary" else "pairwise_summary_borda"
  coeff_df <- read.xlsx(filepath, sheet = rhs_sheet)
  noise_df <- read.xlsx(filepath, sheet = var_sheet)
  lhs_df <- read.xlsx(filepath, "Coefficients") %>% select(firm_id, !!rlang::sym(lhs_var))
  
  # Read in Coefficients & Signal
  rhs_df <- list()
  noise <- list()
  variance <- list()
  for (i in 1:length(rhs_vars)) {
    rhs_df[[i]] <- read_coeff(coeff_df, rhs_vars[[i]])
    noise[[i]]  <- read_noise(noise_df, var1 = rhs_vars[[i]], col = "noise1")
  }
  noise[[3]] <- read_noise(noise_df, var1 = rhs_vars[[1]], var2 = rhs_vars[[2]], col = "noise12")
  
  # Temp lines to check if program is working
  
  # Construct data set for eivreg
  reg_data <- lhs_df %>%
    dplyr::left_join(rhs_df[[1]],  by = "firm_id") %>%
    dplyr::left_join(industry_map, by = "firm_id") %>%
    dplyr::left_join(rhs_df[[2]], by = "firm_id")

  # Filter using base R to avoid data masking issues
  reg_data <- reg_data[!is.na(reg_data[[lhs_var]]) & !is.na(reg_data[["aer_naics2"]]), ]
  
  if (!is.null(weights)) {
    reg_data <- reg_data %>% dplyr::left_join(weights, by = "firm_id")
  } else {
    reg_data <- reg_data %>% dplyr::mutate(weights = 1)
  }
  
  fe_mat <- stats::model.matrix(~ factor(reg_data$aer_naics2) - 1)
  colnames(fe_mat) <- paste0("dummy", seq_len(ncol(fe_mat)))
  reg_data <- dplyr::bind_cols(reg_data, as.data.frame(fe_mat))
  dummies  <- colnames(fe_mat)
  
  rhs_part <- paste(rhs_vars, collapse = " + ")
  f1 <- stats::as.formula(paste(lhs_var, "~", rhs_part))
  f2 <-  stats::as.formula(paste(lhs_var, "~ 0 +", paste(rhs_part, "+", paste(dummies, collapse = " + "))))
  Sigma1 <- matrix(c(noise[[1]],noise[[3]],noise[[3]],noise[[2]]), 2, 2, dimnames = list(rhs_vars, rhs_vars))
  Sigma2 <- matrix(0, 2+length(dummies), 2+length(dummies),dimnames = list(c(rhs_vars, dummies), c(rhs_vars, dummies)))
  Sigma2[rhs_vars, rhs_vars] <- Sigma1
  
  
  # ---- EIV regressions -----------------------------------------------------
  m1_vals <- tryCatch({
    m1 <- eivreg(f1, data = reg_data, Sigma_error = Sigma1)
    list(coef1 = coef(m1)[rhs_vars[[1]]], 
         coef2 = coef(m1)[rhs_vars[[2]]],
         se1   = sqrt(m1$vcov[rhs_vars[[1]], rhs_vars[[1]]]),
         se2   = sqrt(m1$vcov[rhs_vars[[2]], rhs_vars[[2]]]))
  }, error = function(e) list(coef1 = NA_real_, coef2 = NA_real_, se1 = NA_real_, se2 = NA_real_))
  
  m2_vals <- tryCatch({
    m1 <- eivreg(f2, data = reg_data, Sigma_error = Sigma2)
    list(coef1 = coef(m1)[rhs_vars[[1]]], 
         coef2 = coef(m1)[rhs_vars[[2]]],
         se1   = sqrt(m1$vcov[rhs_vars[[1]], rhs_vars[[1]]]),
         se2   = sqrt(m1$vcov[rhs_vars[[2]], rhs_vars[[2]]]))
  }, error = function(e) list(coef1 = NA_real_, coef2 = NA_real_, se1 = NA_real_, se2 = NA_real_))
    
  x<-data.frame(
    lhs         = lhs_var,
    rhs1        = rhs_vars[[1]],
    rhs2        = rhs_vars[[2]],
    iter        = 0,
    noise1      = noise[[1]],
    noise2      = noise[[2]],
    noise12.    = noise[[3]],
    coef1     = m1_vals$coef1, se1 = m1_vals$se1,
    coef2     = m2_vals$coef1, se2 = m2_vals$se1,
    coef3     = m1_vals$coef2, se3 = m1_vals$se2,
    coef4     = m2_vals$coef2, se4 = m2_vals$se2,
    stringsAsFactors = FALSE
  )
}


