recenter_objects <- function(beta, S_full = NULL, ...) {
  # ... can include any number of square JxJ matrices (cov=, rcov=, bread=, etc.)
  J <- length(beta)
  C <- diag(J) - matrix(1 / J, J, J)
  
  mats <- list(...)
  
  # center beta
  out <- list(beta = as.numeric(C %*% beta))
  
  # center any provided JxJ matrices
  if (length(mats)) {
    for (nm in names(mats)) {
      M <- mats[[nm]]
      if (is.null(M)) {
        out[[nm]] <- NULL
      } else {
        M <- as.matrix(M)
        stopifnot(nrow(M) == J, ncol(M) == J)
        out[[nm]] <- C %*% M %*% t(C)
      }
    }
  }
  
  # center score-like matrix (n x J): S -> S C'
  if (!is.null(S_full)) {
    S_full <- as.matrix(S_full)
    stopifnot(ncol(S_full) == J)
    out$S <- S_full %*% t(C)
  } else {
    out$S <- NULL
  }
  
  out
}
