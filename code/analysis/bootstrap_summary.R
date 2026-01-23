library(readxl)
library(dplyr)

bs_summary <- function(path, sheet, lhs, rhs, na_tol = 0.02) {
  ## 1) read the sheet
  dat <- read_excel(path, sheet = sheet)
  
  ## 2) split full-sample vs leave-one-out
  full_row <- filter(dat, iter == 0)
  loo_rows <- filter(dat, iter > 0)
  n_total  <- nrow(loo_rows)  # total number of reps (for NA share)
  
  ## 3) helper that makes the 4 numbers for one column
  one_coef <- function(col_coef, col_se, i) {
    theta_hat <- pull(full_row, {{ col_coef }})
    se_hat    <- pull(full_row, {{ col_se }})
    
    theta_loo_all <- pull(loo_rows, {{ col_coef }})
    na_prop <- if (length(theta_loo_all) == 0) 1 else mean(is.na(theta_loo_all))
    
    if (length(theta_loo_all) == 0) {
      theta_se <- NA_real_
      warning(sprintf("No bootstrap/jackknife repetitions found for coef %s; bs_se set to NA.", i))
    } else if (na_prop <= na_tol) {
      theta_loo <- theta_loo_all[!is.na(theta_loo_all)]
      m <- length(theta_loo)
      if (m > 1) {
        theta_bar <- mean(theta_loo)
        ## sd of reps (bootstrap-style) or jackknife-like using m-1 denominator
        theta_se  <- sqrt(sum((theta_loo - theta_bar)^2) / (m - 1))
      } else {
        theta_se <- NA_real_
        warning(sprintf(
          "≤2%% NA allowed, but only %d non-NA rep for coef %s; bs_se set to NA.", m, i
        ))
      }
      if (na_prop > 0)
        message(sprintf("Coef %s: %.2f%% NA reps ignored in bs_se.", i, 100 * na_prop))
    } else {
      theta_se <- NA_real_
      warning(sprintf(
        "Coef %s: %.2f%% NA reps (> %.2f%% tolerance). bs_se set to NA.",
        i, 100 * na_prop, 100 * na_tol
      ))
    }
    
    tibble(
      lhs        = lhs,
      rhs        = rhs,
      coef       = i,
      sample_est = theta_hat,
      sample_se  = se_hat,
      bs_se      = theta_se
    )
  }
  
  ## 4) bind the two coefficients’ results
  bind_rows(
    one_coef(coef1, se1, 1),
    one_coef(coef2, se2, 2)
  )
}

