recenter_pl_objects <- function(beta, Binv, cov, S_full) {
  J <- length(beta)
  C <- diag(J) - matrix(1 / J, J, J)
  
  list(
    beta = as.numeric(C %*% beta),
    Binv = C %*% Binv %*% t(C),
    cov  = C %*% cov  %*% t(C),
    S    = S_full %*% t(C)
  )
}
