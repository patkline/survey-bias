katz_correct <- function(sigma2_hat, Vhat) {
  # Guard against tiny negative variance from rounding
  se <- sqrt(pmax(Vhat, 0))
  
  # Default: no correction if se = 0 or NA
  z <- sigma2_hat/se
  sigma2_dot <- sigma2_hat + se * dnorm(z) / pnorm(z)
  sigma2_dot
}

var_component_with_var <- function(theta_hat, Sigma) {
  theta_hat <- as.numeric(theta_hat)
  J <- length(theta_hat)
  
  stopifnot(is.matrix(Sigma),
            nrow(Sigma) == J,
            ncol(Sigma) == J)
  
  # σ̂^2
  tr_Sigma   <- sum(diag(Sigma))
  sigma2_hat <- (sum(theta_hat^2) - tr_Sigma) / J
  
  # V̂(σ̂^2)
  quad      <- as.numeric(t(theta_hat) %*% Sigma %*% theta_hat)
  tr_Sigma2 <- sum(Sigma * Sigma)   # == tr(Sigma %*% Sigma)
  Vhat      <- (4 * quad - 2 * tr_Sigma2) / (J^2)
  
  list(
    sigma2_hat = sigma2_hat,
    Vhat       = Vhat,
    se_sigma2  = sqrt(pmax(Vhat, 0))  # guard tiny negatives from rounding
  )
}
