library(parallel)
library(dplyr)
library(tidyr)
library(PlackettLuce)
library(sandwich)
library(prefmod)
library(tibble)

score_function <- function(mod, ref_firm = NULL) {
  estfun_original <- estfun(mod, ref = ref_firm)
  sample_size <- dim(estfun_original)[[1]]
  scaling_factor <- sqrt(sample_size)
  estfun_corrected <- estfun_original * scaling_factor
  
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


bootstrap_pairwise_manual <- function(wide_mat,
                             id_map,
                             B    = 10,   # number of bootstrap replications
                             seed = NULL, 
                             weights = NULL)  # set a seed for reproducibility
{
  if (!is.null(seed)) set.seed(seed)
  
  n <- nrow(wide_mat)
  
  ## ------------------------------------------------------------
  ## helper that estimates a PL model and extracts coefficients,
  ## model-based SEs and robust (sandwich) SEs
  ## ------------------------------------------------------------
  run_pl <- function(mat, k, weights = NULL) {
    print(k)
    
    mat <- mat %>% dplyr::select(-resp_id)
    
    if (is.null(weights)) {
      mod <- PlackettLuce(mat, npseudo = 0.5)
    } else {
      mod <- PlackettLuce(mat, npseudo = 0.5, weights = weights)
    }
    
    smry <- summary(mod,
                    vcov. = vcov(mod, type = "observed", ref = NULL),
                    ref   = NULL)$coefficients
    
    ## --- point estimates and model-based SEs -------------------
    coeff_df <- as.data.frame(smry) %>%
      tibble::rownames_to_column("firm_id") %>%
      filter(!grepl("^tie", firm_id)) %>%
      mutate(firm_id = as.integer(sub("firm", "", firm_id))) %>%
      select(firm_id, Estimate, `Std. Error`) %>%
      rename(!!paste0("iter_", k) := Estimate,
             !!paste0("se_",   k) := `Std. Error`)
    
    K <- dim(coeff_df)[1]
    
    ## --- robust SEs -------------------------------------------
    S     <- score_function(mod)                       # estfun × √n
    B_inv <- vcov(mod, type = "observed", ref = NULL)
    M           <- crossprod(S) / nrow(S)
    robust_cov  <- B_inv %*% M %*% B_inv
    robust_se   <- sqrt(diag(robust_cov))[1:K]
    
    robust_df <- enframe(robust_se,
                         name  = "firm_id",
                         value = paste0("robust_", k)) %>%
      mutate(firm_id = as.integer(sub("^firm", "", firm_id))) %>%
      arrange(firm_id)
    
    ## merge and return
    coeff_df <- coeff_df %>%
      left_join(robust_df, by = "firm_id")
  }
  
  ## ------------------------------------------------------------
  ## 1) full-sample estimates
  ## ------------------------------------------------------------
  full_df <- run_pl(wide_mat, k = 0)
  
  ## ------------------------------------------------------------
  ## 2) bootstrap replications
  ## ------------------------------------------------------------
  cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
  
  # bs_list <- lapply(seq_len(B), function(b) {
  #   w_b <- weights %>% pull(paste0("w_",b))                 # ← use the b-th column
  #   run_pl(wide_mat,
  #          k       = b,                     # label this draw
  #          weights = w_b)                   # pass the weights
  # })
  
  bs_list <- lapply(
      seq_len(B),
      function(b) {
        idx <- sample.int(n, n, replace = TRUE)
        run_pl(wide_mat[idx, , drop = FALSE], k = b)
      }
    )
  
  # if (cores > 1L && .Platform$OS.type != "windows") {
  #   parallel::mclapply(
  #     seq_len(B),
  #     function(b) {
  #       idx     <- sample.int(n, n, replace = TRUE)
  #       run_pl(wide_mat[idx, , drop = FALSE], k = b)
  #     },
  #     mc.cores       = cores,
  #     mc.preschedule = TRUE
  #   )
  # } else {

  ## ------------------------------------------------------------
  ## 3) combine full-sample + bootstrap draws
  ## ------------------------------------------------------------
  combined <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "firm_id"),
    c(list(full_df), bs_list)
  )
  
  ## ------------------------------------------------------------
  ## 4) split into the three tidy output tables
  ## ------------------------------------------------------------
  iter_cols   <- grep("^iter_",   names(combined), value = TRUE)
  se_cols     <- grep("^se_",     names(combined), value = TRUE)
  robust_cols <- grep("^robust_", names(combined), value = TRUE)
  
  iter_df <- combined %>%
    select(firm_id, all_of(iter_cols)) %>%
    left_join(id_map, by = "firm_id")
  
  se_df <- combined %>%
    select(firm_id, all_of(se_cols)) %>%
    left_join(id_map, by = "firm_id")
  
  robust_df <- combined %>%
    select(firm_id, all_of(robust_cols)) %>%
    left_join(id_map, by = "firm_id")
  
  list(iter_df   = iter_df,
       se_df     = se_df,
       robust_df = robust_df)
}
