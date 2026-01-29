score_function <- function(mod, ref_firm = NULL) {
  estfun_original <- estfun(mod, ref = ref_firm)
  sample_size <- dim(estfun_original)[[1]]
  scaling_factor <- sqrt(sample_size)
  estfun_corrected <- estfun_original
  
  score_means_corrected <- colMeans(estfun_corrected)
  
  max_score_mean <- max(abs(score_means_corrected))
  if (max_score_mean > 0.01) {
    # cat("⚠ Large score means detected - applying additional centering\n")
    # Center the scores (this addresses the tie parameter issue we found earlier)
    estfun_final <- scale(estfun_corrected, center = TRUE, scale = FALSE)
  } else {
    estfun_final <- estfun_corrected
    # cat("✓ Scores are adequately centered\n")
  }
  
  return(estfun_final)
}