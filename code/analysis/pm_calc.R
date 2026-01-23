# variance noise (mean robust variance)
pm_calc <- function(iter_rhs, iter_se, center = FALSE) {
  ## Calculate var_star as posterior mean 
  theta_hat <- iter_rhs
  if (center) {
    theta_hat <- iter_rhs - mean(iter_rhs)
  }
  theta_hat2 <- theta_hat^2
  theta_hat4 <- theta_hat^4
  sigma2 <- iter_se^2
  sigma4 <- iter_se^4
  Y <- mean(theta_hat2)
  S2 <- mean(sigma2)
  Q4 <- mean(sigma4)
  S4 <- S2^2 
  delta4 <- Q4 - S4 
  
  n <- length(theta_hat)
  tau <- Y - S2
  
  m4_vec <- theta_hat4 - 6 *theta_hat2* sigma2 + 3 * sigma4
  m4_mean <- mean(m4_vec)
  
  beta2_fun <- function(tau) {
    sqrt( pmax(m4_mean - tau^2, 1/n) )
  }
  beta2_tau <- beta2_fun(tau)
  
  numerator0 <- mean( (theta_hat2 - sigma2) * (sigma2 - S2) )
  rho_fun <- function(tau) {
    denom <- beta2_fun(tau) * sqrt(delta4)
    rho <- numerator0 / denom
    rho
  }
  rho_tau <- rho_fun(tau)
  
  post_mode <- sqrt(tau^2 + (2/n * (S2 + rho_fun(tau)*beta2_fun(tau)*sqrt(delta4)))^2) - (2/n * (S2 + rho_fun(tau)*beta2_fun(tau)*sqrt(delta4)))
  list(post_mode = post_mode, Y = Y, S2 = S2, tau = tau, delta4 = delta4, beta2_tau = beta2_tau, rho_tau = rho_tau)
}