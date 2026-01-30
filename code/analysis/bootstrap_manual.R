bootstrap_manual <- function(wide_mat,
                             id_map,
                             firms97,          # <-- NEW: 97-firm subset
                             B    = 10,        # number of bootstrap replications
                             seed = NULL,
                             weights = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- nrow(wide_mat)
  
  ## ------------------------------------------------------------
  ## helper that estimates a PL model and extracts:
  ##  - coefficients, model-based SEs, robust SEs
  ##  - variance components for:
  ##       * ALL firms
  ##       * 97-firm subset (firms97)
  ## ------------------------------------------------------------
  run_pl <- function(mat, k, weights = NULL) {
    message("Bootstrap iteration: ", k)
    
    mat_narrow <- mat %>% dplyr::select(-resp_id)
    
    if (is.null(weights)) {
      mod <- PlackettLuce(mat_narrow, npseudo = 0.5)
    } else {
      mod <- PlackettLuce(mat_narrow, npseudo = 0.5, weights = weights)
    }
    
    ## --- point estimates and model-based SEs -------------------
    smry <- summary(
      mod,
      vcov. = vcov(mod, type = "observed", ref = NULL),
      ref   = NULL
    )$coefficients
    
    coeff_df <- as.data.frame(smry) %>%
      tibble::rownames_to_column("firm_id") %>%
      dplyr::filter(!grepl("^tie", firm_id)) %>%
      dplyr::mutate(firm_id = as.integer(sub("firm", "", firm_id))) %>%
      dplyr::select(firm_id, Estimate, `Std. Error`) %>%
      dplyr::rename(!!paste0("iter_", k) := Estimate,
                    !!paste0("se_",   k) := `Std. Error`) %>%
      dplyr::arrange(firm_id)
    
    K <- nrow(coeff_df)
    
    ## --- robust SEs -------------------------------------------
    S     <- score_function(mod)                       # estfun × √n
    B_inv <- vcov(mod, type = "observed", ref = NULL) # full vcov (incl. ties)
    M           <- crossprod(S)
    robust_cov  <- B_inv %*% M %*% B_inv
    robust_se   <- sqrt(diag(robust_cov))[1:K]
    
    robust_df <- tibble::enframe(robust_se,
                                 name  = "firm_id",
                                 value = paste0("robust_", k)) %>%
      dplyr::mutate(firm_id = as.integer(sub("^firm", "", firm_id))) %>%
      dplyr::arrange(firm_id)
    
    coeff_df <- coeff_df %>%
      dplyr::left_join(robust_df, by = "firm_id")
    
    ## --- variance components: ALL firms + 97-firm subset -------
    coefs      <- coef(mod, ref = NULL)          # includes tie parameters
    par_names  <- names(coefs)
    firm_mask  <- !grepl("^tie", par_names)
    firm_names <- par_names[firm_mask]       # e.g. "firm1", "firm2", ...
    
    firm_ids   <- as.integer(sub("firm", "", firm_names))
    ord        <- order(firm_ids)
    firm_ids_ord <- firm_ids[ord]
    
    alpha_hat  <- as.numeric(coefs[firm_mask][ord])
    Sigma_alpha <- B_inv[firm_names[ord], firm_names[ord], drop = FALSE]
    
    ## ---- ALL firms ----
    vc_all <- tryCatch(
      var_component_with_var(theta_hat = alpha_hat,
                             Sigma      = Sigma_alpha),
      error = function(e) NULL
    )
    
    if (is.null(vc_all)) {
      var_comp_all <- c(
        tot_var    = NA_real_,
        Y          = NA_real_,
        noise      = NA_real_,
        sigma2_hat = NA_real_,
        Vhat       = NA_real_,
        se_sigma2  = NA_real_,
        sigma2_dot = NA_real_
      )
    } else {
      tot_var_all    <- stats::var(alpha_hat)
      Y_all          <- mean(alpha_hat^2)
      noise_all      <- mean(diag(Sigma_alpha))
      sigma2_dot_all <- katz_correct(vc_all$sigma2_hat, vc_all$Vhat)
      
      var_comp_all <- c(
        tot_var    = tot_var_all,
        Y          = Y_all,
        noise      = noise_all,
        sigma2_hat = vc_all$sigma2_hat,
        Vhat       = vc_all$Vhat,
        se_sigma2  = vc_all$se_sigma2,
        sigma2_dot = sigma2_dot_all
      )
    }
    
    ## ---- 97-firm subset ----
    mask97 <- firm_ids_ord %in% firms97
    
    if (sum(mask97) >= 2) {
      # raw subset
      alpha_97_raw <- alpha_hat[mask97]
      Sigma_97_raw <- Sigma_alpha[mask97, mask97, drop = FALSE]
      
      # centering matrix C = I - 11'/J
      J97 <- length(alpha_97_raw)
      C97 <- diag(J97) - matrix(1 / J97, J97, J97)
      
      # centered alpha and Sigma
      alpha_97  <- as.numeric(C97 %*% alpha_97_raw)
      Sigma_97  <- C97 %*% Sigma_97_raw %*% t(C97)
      
      vc_97 <- tryCatch(
        var_component_with_var(theta_hat = alpha_97,
                               Sigma      = Sigma_97),
        error = function(e) NULL
      )
        
      if (is.null(vc_97)) {
        var_comp_97 <- c(
          tot_var    = NA_real_,
          Y          = NA_real_,
          noise      = NA_real_,
          sigma2_hat = NA_real_,
          Vhat       = NA_real_,
          se_sigma2  = NA_real_,
          sigma2_dot = NA_real_
        )
      } else {
        tot_var_97    <- stats::var(alpha_97)
        Y_97          <- mean(alpha_97^2)
        noise_97      <- mean(diag(Sigma_97))
        sigma2_dot_97 <- katz_correct(vc_97$sigma2_hat, vc_97$Vhat)
        
        var_comp_97 <- c(
          tot_var    = tot_var_97,
          Y          = Y_97,
          noise      = noise_97,
          sigma2_hat = vc_97$sigma2_hat,
          Vhat       = vc_97$Vhat,
          se_sigma2  = vc_97$se_sigma2,
          sigma2_dot = sigma2_dot_97
        )
      }
    } else {
      # not enough firms in 97-subset for this bootstrap draw
      var_comp_97 <- c(
        tot_var    = NA_real_,
        Y          = NA_real_,
        noise      = NA_real_,
        sigma2_hat = NA_real_,
        Vhat       = NA_real_,
        se_sigma2  = NA_real_,
        sigma2_dot = NA_real_
      )
    }
    
    list(
      coeff_df     = coeff_df,
      var_comp_all = var_comp_all,
      var_comp_97  = var_comp_97
    )
  }
  
  ## ------------------------------------------------------------
  ## 1) full-sample estimates  (k = 0)
  ## ------------------------------------------------------------
  full_res <- run_pl(wide_mat, k = 0)
  full_df  <- full_res$coeff_df
  
  ## ------------------------------------------------------------
  ## 2) bootstrap replications
  ## ------------------------------------------------------------
  bs_list <- lapply(seq_len(B), function(b) {
    w_b <- if (is.null(weights)) NULL else {
      weights %>% dplyr::pull(paste0("w_", b))
    }
    run_pl(wide_mat, k = b, weights = w_b)
  })
  
  ## ------------------------------------------------------------
  ## 3) combine full-sample + bootstrap draws (coefficients)
  ## ------------------------------------------------------------
  coeff_list <- c(list(full_df), lapply(bs_list, `[[`, "coeff_df"))
  
  combined <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "firm_id"),
    coeff_list
  )
  
  iter_cols   <- grep("^iter_",   names(combined), value = TRUE)
  se_cols     <- grep("^se_",     names(combined), value = TRUE)
  robust_cols <- grep("^robust_", names(combined), value = TRUE)
  
  iter_df <- combined %>%
    dplyr::select(firm_id, dplyr::all_of(iter_cols)) %>%
    dplyr::left_join(id_map, by = "firm_id")
  
  se_df <- combined %>%
    dplyr::select(firm_id, dplyr::all_of(se_cols)) %>%
    dplyr::left_join(id_map, by = "firm_id")
  
  robust_df <- combined %>%
    dplyr::select(firm_id, dplyr::all_of(robust_cols)) %>%
    dplyr::left_join(id_map, by = "firm_id")
  
  ## ------------------------------------------------------------
  ## 4) collect variance-component results into data frames
  ## ------------------------------------------------------------
  var_all_mat <- do.call(
    rbind,
    c(list(full_res$var_comp_all), lapply(bs_list, `[[`, "var_comp_all"))
  )
  
  var_97_mat <- do.call(
    rbind,
    c(list(full_res$var_comp_97), lapply(bs_list, `[[`, "var_comp_97"))
  )
  
  var_comp_all_df <- as.data.frame(var_all_mat)
  var_comp_all_df$iter <- 0:B
  var_comp_all_df <- var_comp_all_df %>%
    dplyr::relocate(iter)
  
  var_comp_97_df <- as.data.frame(var_97_mat)
  var_comp_97_df$iter <- 0:B
  var_comp_97_df <- var_comp_97_df %>%
    dplyr::relocate(iter)
  
  list(
    iter_df      = iter_df,
    se_df        = se_df,
    robust_df    = robust_df,
    var_comp_all = var_comp_all_df,
    var_comp_97  = var_comp_97_df,
    var_comp     = var_comp_all_df   # optional alias for backward compatibility
  )
}
