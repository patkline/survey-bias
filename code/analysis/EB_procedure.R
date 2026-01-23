# Precision-dependent Empirical Bayes (two-step)
# Inputs:
#   theta_hat : numeric vector of estimates \hat{theta}_j
#   s         : numeric vector of standard errors s_j (>0)
# Returns:
#   list with psi1, all hyperparameter estimates, and EB posterior means theta_eb

eb_two_step <- function(theta_hat, s) {
  stopifnot(length(theta_hat) == length(s))
  stopifnot(all(is.finite(theta_hat)), all(is.finite(s)), all(s > 0))
  
  df <- data.frame(theta_hat = as.numeric(theta_hat),
                   s         = as.numeric(s))
  df$log_s <- log(df$s)
  
  ## ---- Step 1: E[theta_hat | s] = psi0 + psi1 * log(s) ----
  fit1 <- lm(theta_hat ~ log_s, data = df)
  psi0 <- unname(coef(fit1)[1])
  psi1 <- unname(coef(fit1)[2])
  
  # residuals and "variance moment" target
  resid1 <- df$theta_hat - (psi0 + psi1 * df$log_s)
  y <- resid1^2 - df$s^2          # target for sigma_r^2 * s^(2*psi2)
  
  ## ---- Step 2: estimate psi2 and sigma_r^2 via least squares ----
  # objective on transformed params (a = log sigma_r^2 => sigma_r^2 = exp(a))
  obj <- function(par) {
    a    <- par[1]                 # log(sigma_r^2), enforces positivity
    psi2 <- par[2]
    mu   <- exp(a) * (df$s^(2 * psi2))
    sum((y - mu)^2, na.rm = TRUE)
  }
  
  # a few starts for robustness
  starts <- rbind(
    c(log(var(pmax(y, 1e-8), na.rm = TRUE)), 0.5),
    c(log(mean(pmax(y, 1e-8), na.rm = TRUE)), 0.0),
    c(log(var(abs(y), na.rm = TRUE) + 1e-6),  1.0)
  )
  
  best <- NULL; bestval <- Inf
  for (i in seq_len(nrow(starts))) {
    res <- try(optim(starts[i,], obj, method = "BFGS"), silent = TRUE)
    if (!inherits(res, "try-error") && res$value < bestval) {
      best <- res; bestval <- res$value
    }
  }
  if (is.null(best)) stop("Step-2 optimization failed.")
  
  log_sigma2_r <- best$par[1]
  psi2         <- best$par[2]
  sigma2_r     <- exp(log_sigma2_r)
  
  ## ---- EB shrinkage for residuals and posterior means (eqs. 56–57) ----
  r_hat   <- resid1 / (df$s^psi2)
  shrink  <- sigma2_r / (sigma2_r + df$s^(2 * (1 - psi2)))
  r_star  <- shrink * r_hat
  theta_eb <- psi0 + psi1 * df$log_s + (df$s^psi2) * r_star
  
  list(
    psi1       = psi1,
    estimates  = list(psi0 = psi0, psi1 = psi1, psi2 = psi2, sigma2_r = sigma2_r),
    theta_eb   = theta_eb,
    shrink     = shrink,
    lm_step1   = fit1
  )
}
