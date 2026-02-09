# Run globals
source("code/globals.R")

# -----------------------------------------------
# Define Wrapper Function
# -----------------------------------------------
run_analysis_pipeline <- function(data, respondent_col, firm_col, survey_vars, experimental_vars,
                                  outcome_types, subset_var = NULL, subset_value = NULL, firms97 = NULL, 
                                  output_path, industry_map_path, process_outcomes = FALSE, 
                                  run_jackknife = FALSE, run_bootstrap = FALSE, sim_pl_to_borda = FALSE,
                                  exact_pl_to_borda = FALSE, sum_signal_noise = FALSE,
                                  run_bs_eiv = FALSE, eiv_bivariate = FALSE, eiv_summary = FALSE,
                                  run_pairwise_process = FALSE,
                                  diagnostic = FALSE, borda_score = FALSE,
                                  borda_bs_w = FALSE, borda_bs = FALSE,
                                  borda_eiv = FALSE,
                                  run_borda_eiv = FALSE,
                                  borda_eiv_summary = FALSE,
                                  borda_eiv_bivariate = FALSE,
                                  run_pairwise_process_borda = FALSE,
                                  generate_wide = TRUE,
                                  seed = 123,
                                  B = 200) {
  
  print(paste0("Running Analysis for: Subset = ", subset_var, ", Value = ", subset_value))
  # Combine Vars
  outcome_vars <- c(survey_vars, experimental_vars)
  
  # Subset data if a subset variable and value are provided
  if (!is.null(subset_var) & !is.null(subset_value)) {
    data <- data %>% filter(!!sym(subset_var) == subset_value)
  }
  
  # Map from firms to industries
  industry_map <- openxlsx::read.xlsx(industry_map_path, sheet = 1) %>%
    dplyr::select(firm_id, aer_naics2) %>%
    dplyr::mutate(firm_id = as.integer(firm_id))
  
  # Check if the workbook already exists
  if (file.exists(output_path)) {
    wb <- loadWorkbook(output_path)  # Load existing workbook
  } else {
    wb <- createWorkbook()  # Create a new workbook
  }
  
  ################################################################################
  # Section I: Create Clean Ranked Data Sets
  ################################################################################
  if (generate_wide) {
    set.seed(seed)
    # empty lists to store results
    data_list <- list()
    id_map_list <- list()
    
    # 1) prep once and store
    for (outcome in survey_vars) {
      cat("Preparing:", outcome, "\n")
      prep <- prepare_pltree_data(
        data            = data,
        rank_col        = outcome,
        subgroup_var    = NULL,
        subgroup_filter = NULL
      )
      
      data_list[[outcome]]   <- prep$data_wide_pltree
      id_map_list[[outcome]] <- prep$id_map %>% filter(!is.na(firm), firm!="nan")
    
    }

  }
  
  
  ################################################################################
  # Section I: Plackett Luce Model
  ################################################################################
  
  # Section IA: Process Outcomes
  # This section runs the Plackett Luce Model
  # Stores Item Worths, SEs, Robust SEs, EB Item Worths, Average Ratings, and 
  # a count of Ratings at Firm Level
  # This code also calculates strict wins, ties, and strict losses from all
  ################################################################################
  # Section I: Plackett Luce Model
  ################################################################################
  
  if (process_outcomes) {
    set.seed(seed)
    
    S_list    <- list()
    Binv_list <- list()
    cov_list  <- list()
    
    # containers for PL outputs
    coeff_df       <- data.frame()
    se_df          <- data.frame()
    robust_df      <- data.frame()
    coeff_df_eb    <- data.frame()
    coeff_df_eb_cw <- data.frame()   # NEW EB (V2) with robust-SE precision shrinkage
    avg_rating_df  <- data.frame()
    obs_df         <- data.frame()
    
    # collect psi_1 and its SE per outcome
    psi_rows <- list()
    
    # NEW: collect log-likelihood + # params per outcome
    lr_rows <- list()
    
    # helper to sanitize Excel sheet names (<=31 chars, no []:/\?*)
    sanitize_sheet <- function(x) {
      x <- gsub("[\\\\/*?:\\[\\]]", "_", x)
      substr(x, 1, 31)
    }
    
    # remove legacy combined sheet (if present)
    remove_sheet_safely(wb, "Win_Share")
    
    for (outcome in survey_vars) {
      cat("Processing outcome:", outcome, "\n")
      
      data_wide <-  data_list[[outcome]]   # resp_id + firm1..firmN (0 / 1..K)
      id_map    <- id_map_list[[outcome]]
      
      # ---------- Plackett–Luce estimation ----------
      pl <- run_plackett_luce(data_wide, id_map, outcome, firms97)
      
      S_list[[outcome]]    <- pl$S          # data.frame: resp_id + firm*
      Binv_list[[outcome]] <- pl$Binv       # firm-only J×J
      cov_list[[outcome]]  <- pl$robust_cov # firm-only J×J
      
      # -----------------------------
      # Save to Excel (optional)
      # -----------------------------
      if (!is.null(pl$robust_cov)) {
        sheet_cov <- sanitize_sheet(paste0("cov_", outcome))
        remove_sheet_safely(wb, sheet_cov)
        addWorksheet(wb, sheet_cov)
        writeData(wb, sheet_cov, pl$robust_cov)
        
        sheet_S <- sanitize_sheet(paste0("S_", outcome))
        remove_sheet_safely(wb, sheet_S)
        addWorksheet(wb, sheet_S)
        writeData(wb, sheet_S, pl$S)
        
        sheet_B <- sanitize_sheet(paste0("Binv_", outcome))
        remove_sheet_safely(wb, sheet_B)
        addWorksheet(wb, sheet_B)
        writeData(wb, sheet_B, pl$Binv)
      }
      
      # NEW: store log-likelihood + number of parameters
      lr_rows[[length(lr_rows) + 1L]] <- data.frame(
        Outcome    = outcome,
        logLik_mle = pl$logLik_mle,
        n_par_mle  = pl$n_par_mle,
        stringsAsFactors = FALSE
      )
      
      # ---------- Precision-dependent EB using ROBUST SE ----------
      theta_tbl <- pl$coeff %>%
        dplyr::select(firm_id, firm, theta_hat = !!rlang::sym(outcome))
      
      se_col <- paste0(outcome, "_rse")
      if (is.null(pl$rse)) {
        stop("pl$rse is NULL; robust SEs required for EB. Outcome: ", outcome)
      }
      if (!(se_col %in% names(pl$rse))) {
        avail <- setdiff(names(pl$rse), c("firm_id","firm"))
        stop("Robust SE column '", se_col, "' not found in pl$rse. Available: ",
             paste(avail, collapse = ", "))
      }
      
      se_tbl <- pl$rse %>%
        dplyr::select(firm_id, firm, s = !!rlang::sym(se_col))
      
      eb_input <- theta_tbl %>%
        dplyr::inner_join(se_tbl, by = c("firm_id","firm")) %>%
        dplyr::mutate(
          theta_hat = as.numeric(theta_hat),
          s         = pmax(as.numeric(s), 1e-8)
        )
      
      valid    <- is.finite(eb_input$theta_hat) & is.finite(eb_input$s) & eb_input$s > 0
      theta_eb <- rep(NA_real_, nrow(eb_input))
      psi1_est <- NA_real_
      psi1_se  <- NA_real_
      
      if (sum(valid) >= 2) {
        eb_fit <- eb_two_step(theta_hat = eb_input$theta_hat[valid],
                              s         = eb_input$s[valid])
        theta_eb[valid] <- eb_fit$theta_eb
        
        if (!is.null(eb_fit$psi1)) psi1_est <- as.numeric(eb_fit$psi1)
        
        if (!is.null(eb_fit$lm_step1)) {
          co <- tryCatch(coef(summary(eb_fit$lm_step1)), error = function(e) NULL)
          if (!is.null(co)) {
            rn <- rownames(co)
            if ("log_s" %in% rn) {
              psi1_se <- co["log_s", "Std. Error"]
              if (is.na(psi1_est)) psi1_est <- co["log_s", "Estimate"]
            } else if ("log(s)" %in% rn) {
              psi1_se <- co["log(s)", "Std. Error"]
              if (is.na(psi1_est)) psi1_est <- co["log(s)", "Estimate"]
            }
          }
        }
        
        if (is.na(psi1_se) || is.na(psi1_est)) {
          dfv <- transform(
            data.frame(theta_hat = eb_input$theta_hat[valid], s = eb_input$s[valid]),
            log_s = log(s)
          )
          fit1b <- lm(theta_hat ~ log_s, data = dfv)
          sm1b  <- coef(summary(fit1b))
          psi1_est <- as.numeric(sm1b["log_s", "Estimate"])
          psi1_se  <- as.numeric(sm1b["log_s", "Std. Error"])
        }
      } else {
        warning("EB step skipped for ", outcome, " (not enough valid firms).")
      }
      
      psi_rows[[length(psi_rows) + 1L]] <- data.frame(
        outcome  = outcome,
        psi_1    = psi1_est,
        se_psi_1 = psi1_se,
        stringsAsFactors = FALSE
      )
      
      res_df <- eb_input %>%
        dplyr::select(firm_id, firm) %>%
        dplyr::mutate(!!outcome := theta_eb)
      
      # ---------- Combine across outcomes ----------
      if (nrow(coeff_df) == 0) {
        coeff_df       <- pl$coeff
        se_df          <- pl$se
        robust_df      <- pl$rse
        coeff_df_eb    <- pl$coeff_eb
        coeff_df_eb_cw <- res_df
        avg_rating_df  <- pl$avg_ratings
        obs_df         <- pl$obs
      } else {
        coeff_df       <- dplyr::left_join(coeff_df,       pl$coeff,       by = c("firm","firm_id"))
        se_df          <- dplyr::left_join(se_df,          pl$se,          by = c("firm","firm_id"))
        robust_df      <- dplyr::left_join(robust_df,      pl$rse,         by = c("firm","firm_id"))
        coeff_df_eb    <- dplyr::left_join(coeff_df_eb,    pl$coeff_eb,    by = c("firm","firm_id"))
        coeff_df_eb_cw <- dplyr::left_join(coeff_df_eb_cw, res_df,         by = c("firm","firm_id"))
        avg_rating_df  <- dplyr::left_join(avg_rating_df,  pl$avg_ratings, by = c("firm","firm_id"))
        obs_df         <- dplyr::left_join(obs_df,         pl$obs,         by = c("firm","firm_id"))
      }
      
      # ---------- Optional PL→Borda mappings (unchanged) ----------
      if (isTRUE(sim_pl_to_borda)) {
        print("Sim PL to Borda")
        fit_mle <- PlackettLuce(dplyr::select(data_wide, -resp_id), npseudo = 0.5)
        
        sim_res <- simulate_borda_iterations(
          fit       = fit_mle,
          data_wide = data_wide,
          id_map    = id_map,
          B         = 1000,
          id_var    = "resp_id",
          normalize = TRUE,
          seed      = seed,
          return_iter_matrix = FALSE
        )
        
        sheet_borda <- sanitize_sheet(paste0("pl_to_borda_s_", outcome))
        remove_sheet_safely(wb, sheet_borda)
        addWorksheet(wb, sheet_borda)
        writeData(wb, sheet_borda, sim_res$firm_summary)
      }
      
      if (exact_pl_to_borda) {
        print("Exact PL to Borda")
        
        fit_mle <- PlackettLuce(dplyr::select(data_wide, -resp_id), npseudo = 0.5)
        results <- pl_to_borda(data_wide, id_map, fit_mle)
        
        sheet_borda <- sanitize_sheet(paste0("pl_to_borda_", outcome))
        remove_sheet_safely(wb, sheet_borda)
        addWorksheet(wb, sheet_borda)
        writeData(wb, sheet_borda, results)
      }
      
      # ---------- Run bootstrap + PL signal (ALL vs 97) ----------
      if (run_bootstrap) {
        set.seed(seed)
        n  <- nrow(data_wide)
        
        weights_mat <- generate_weights(n, B, seed = seed)
        colnames(weights_mat) <- paste0("w_", seq_len(B))
        weights_df <- cbind(
          resp_id = data_wide$resp_id,
          as.data.frame(weights_mat)
        )
        
        sheet_nm <- paste0("bs_w_", outcome)
        remove_sheet_safely(wb, sheet_nm)
        addWorksheet(wb, sheet_nm)
        writeData(wb, sheet = sheet_nm, x = weights_df)
        
        bs_res <- try(
          bootstrap_manual(data_wide, id_map, B = B, firms97 = firms97, seed = seed, weights = weights_df),
          silent = TRUE
        )
        
        if (inherits(bs_res, "try-error")) {
          cat("⚠️ Bootstrap for", outcome, "failed with error:\n",
              conditionMessage(attr(bs_res, "condition")), "\n")
        } else {
          remove_sheet_safely(wb, paste0("bs_", outcome))
          addWorksheet(wb, paste0("bs_", outcome))
          writeData(wb, sheet = paste0("bs_", outcome), x = bs_res$iter_df)
          
          remove_sheet_safely(wb, paste0("bs_se_", outcome))
          addWorksheet(wb, paste0("bs_se_", outcome))
          writeData(wb, sheet = paste0("bs_se_", outcome), x = bs_res$se_df)
          
          remove_sheet_safely(wb, paste0("bs_r_", outcome))
          addWorksheet(wb, paste0("bs_r_", outcome))
          writeData(wb, sheet = paste0("bs_r_", outcome), x = bs_res$robust_df)
          
          # ---------- NEW: PL signal sheet (all firms + 97 firms, stacked) ----------
          if (!is.null(bs_res$var_comp_all) && !is.null(bs_res$var_comp_97)) {
            
            vc_all <- bs_res$var_comp_all
            vc_97  <- bs_res$var_comp_97
            
            # ensure all needed columns exist
            ensure_cols <- function(df) {
              if (!("iter" %in% names(df))) {
                df$iter <- seq_len(nrow(df)) - 1L
              }
              needed <- c("tot_var","Y","noise",
                          "sigma2_hat","Vhat","se_sigma2","sigma2_dot")
              for (nm in needed) {
                if (!(nm %in% names(df))) df[[nm]] <- NA_real_
              }
              df[, c("iter", needed), drop = FALSE]
            }
            
            vc_all <- ensure_cols(vc_all)
            vc_97  <- ensure_cols(vc_97)
            
            # overwrite iter 0 with full-sample PL signal from run_plackett_luce
            set_iter0 <- function(df, vc_full) {
              i0 <- which(df$iter == 0)
              if (length(i0) == 1 && !is.null(vc_full)) {
                df$tot_var[i0]    <- vc_full$tot_var
                df$Y[i0]          <- vc_full$Y
                df$noise[i0]      <- vc_full$noise
                df$sigma2_hat[i0] <- vc_full$sigma2_hat
                df$Vhat[i0]       <- vc_full$Vhat
                df$se_sigma2[i0]  <- vc_full$se_sigma2
                df$sigma2_dot[i0] <- vc_full$sigma2_dot
              }
              df
            }
            
            vc_all <- set_iter0(vc_all, pl$var_comp_alpha_all)
            vc_97  <- set_iter0(vc_97,  pl$var_comp_alpha_97)
            
            vc_all$all_firms <- TRUE
            vc_97$all_firms  <- FALSE
            
            vc_out <- dplyr::bind_rows(vc_all, vc_97)
            
            sheet_sig <- sanitize_sheet(paste0("pl_s_", outcome))
            remove_sheet_safely(wb, sheet_sig)
            addWorksheet(wb, sheet_sig)
            writeData(wb, sheet = sheet_sig, x = vc_out)
          } else {
            warning("bootstrap_manual did not return var_comp_all / var_comp_97; ",
                    "PL signal sheet for ", outcome, " skipped.")
          }
          
          cat("✅ Bootstrap Saved:", outcome, "\n")
        }
      }
      
      # ---------- Pairwise strict wins ----------
      pw_tbl <- compute_pairwise_strictwins_wide(
        data_wide        = data_wide,
        id_map           = id_map,
        id_var           = "resp_id",
        higher_is_better = FALSE,
        include_ids      = TRUE
      ) |>
        dplyr::mutate(outcome = outcome, .before = 1) |>
        dplyr::arrange(firmA, firmB)
      
      worth_ml <- pl$coeff |>
        dplyr::select(firm_id, firm, !!rlang::sym(outcome)) |>
        dplyr::rename(worth_ml = !!rlang::sym(outcome))
      
      worth_eb <- pl$coeff_eb |>
        dplyr::select(firm_id, firm, !!rlang::sym(outcome)) |>
        dplyr::rename(worth_eb = !!rlang::sym(outcome))
      
      pw_enriched <- pw_tbl |>
        dplyr::mutate(
          observed_prob = dplyr::if_else(
            total_comparisons > 0, strict_wins / total_comparisons, NA_real_
          )
        ) |>
        dplyr::left_join(worth_ml |> dplyr::select(firm_id, worth_ml) |>
                           dplyr::rename(firmA_id = firm_id, worthA_ml = worth_ml),
                         by = "firmA_id") |>
        dplyr::left_join(worth_eb |> dplyr::select(firm_id, worth_eb) |>
                           dplyr::rename(firmA_id = firm_id, worthA_eb = worth_eb),
                         by = "firmA_id") |>
        dplyr::left_join(worth_ml |> dplyr::select(firm_id, worth_ml) |>
                           dplyr::rename(firmB_id = firm_id, worthB_ml = worth_ml),
                         by = "firmB_id") |>
        dplyr::left_join(worth_eb |> dplyr::select(firm_id, worth_eb) |>
                           dplyr::rename(firmB_id = firm_id, worthB_eb = worth_eb),
                         by = "firmB_id") |>
        dplyr::mutate(
          implied_prob_ml = exp(worthA_ml) / (exp(worthA_ml) + exp(worthB_ml)),
          implied_prob_eb = exp(worthA_eb) / (exp(worthA_eb) + exp(worthB_eb))
        )
      
      sheet_name <- sanitize_sheet(paste0("Win_Share_", outcome))
      remove_sheet_safely(wb, sheet_name)
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, pw_enriched)
    } # end for outcomes
    
    # ---------- Experimental sheets ----------
    exp_results <- clean_experimental(data, experimental_vars)
    
    coefficients    <- dplyr::full_join(coeff_df, exp_results$coefficients,     by = c("firm_id","firm"))
    standard_errors <- dplyr::full_join(se_df,    exp_results$standard_errors,  by = c("firm_id","firm"))
    
    remove_sheet_safely(wb, "Coefficients")
    addWorksheet(wb, "Coefficients")
    writeData(wb, "Coefficients", coefficients)
    
    remove_sheet_safely(wb, "Coefficients (EB)")
    addWorksheet(wb, "Coefficients (EB)")
    writeData(wb, "Coefficients (EB)", coeff_df_eb)
    
    remove_sheet_safely(wb, "Coefficients (EB) V2")
    addWorksheet(wb, "Coefficients (EB) V2")
    writeData(wb, "Coefficients (EB) V2", coeff_df_eb_cw)
    
    remove_sheet_safely(wb, "Average Ratings")
    addWorksheet(wb, "Average Ratings")
    writeData(wb, "Average Ratings", avg_rating_df)
    
    remove_sheet_safely(wb, "Ratings Observations")
    addWorksheet(wb, "Ratings Observations")
    writeData(wb, "Ratings Observations", obs_df)
    
    remove_sheet_safely(wb, "Standard_Errors")
    addWorksheet(wb, "Standard_Errors")
    writeData(wb, "Standard_Errors", standard_errors)
    
    remove_sheet_safely(wb, "Robust SEs")
    addWorksheet(wb, "Robust SEs")
    writeData(wb, "Robust SEs", robust_df)
    
    psi_df <- dplyr::bind_rows(psi_rows)
    remove_sheet_safely(wb, "EB_Psi1")
    addWorksheet(wb, "EB_Psi1")
    writeData(wb, "EB_Psi1", psi_df)
    
    # NEW: write likelihood_ratio sheet
    lr_df <- dplyr::bind_rows(lr_rows)
    remove_sheet_safely(wb, "likelihood_ratio")
    addWorksheet(wb, "likelihood_ratio")
    writeData(wb, "likelihood_ratio", lr_df)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    cat("✅ Plackett–Luce results, Bootstrap sheets, EB (V1 & V2), EB_Psi1, PL signal sheets, per-outcome Win_Share sheets, and likelihood_ratio saved\n")
  }
  
  
  # Section IC: This section runs the runs the EIV model for the full sample and
  # each bootstrap iteration. It then summarizes the coefficient, SE, and bootstrap
  # standard error for each EIV model. 
  
  if (run_bs_eiv) {
    set.seed(seed)
    
    pairs <- list(
      c("cb_central_full",              "discretion", "EIVBS_central_discretion"),
      c("log_dif",        "FirmCont_favor_white", "EIVBS_logdif_cont_white"),
      c("log_dif",        "conduct_favor_white",        "EIVBS_logdif_cond_white"),
      c("log_dif",        "pooled_favor_white",   "EIVBS_logdif_pool_white"),
      c("log_dif",        "FirmSelective", "EIVBS_logdifbw_select"),
      c("log_dif",        "discretion", "EIVBS_logdifbw_discret"),
      c("log_dif_gender", "FirmCont_favor_male",  "EIVBS_logdif_cont_male"),
      c("log_dif_gender", "conduct_favor_male",   "EIVBS_logdif_cond_male"),
      c("log_dif_gender", "pooled_favor_male",    "EIVBS_logdif_pool_male"),
      c("log_dif_gender",        "FirmSelective", "EIVBS_logdifmf_select"),
      c("log_dif_gender",        "discretion", "EIVBS_logdifmf_discret"),
      c("log_dif_age",    "conduct_favor_younger","EIVBS_logdif_cond_young"),
      c("log_dif_age",        "FirmSelective", "EIVBS_logdifyo_select"),
      c("log_dif_age",        "discretion", "EIVBS_logdifyo_discret")
    )
    
    # 2. Loop over that list
    eiv_rows <- list()      # <- will hold the per-pair summary tibbles
    
    for(pair in pairs) {
      lhs_var <- pair[1]
      rhs_var <- pair[2]
      sheet_nm <- pair[3]
      sheet_nm2 <- paste0("bs_",lhs_var)
      
      print(paste0("Running BS EIV Procedure: ", lhs_var, " ~ ", rhs_var))
      weights <- data %>% select(firm_id, njobs) %>% rename(weights = njobs) %>% select(firm_id,weights) %>% distinct()
      results <- bs_eiv_run(output_path, industry_map, lhs_var, rhs_var, borda = FALSE, iterations = B, weights = weights)
      eiv <- results$eiv
      boot_df <- results$lhs_boot
      
      remove_sheet_safely(wb, sheet_nm)
      addWorksheet(wb, sheet_nm)
      writeData(wb, sheet_nm, eiv)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS EIV Results Saved: ",lhs_var," ~ ",rhs_var))
      
      remove_sheet_safely(wb, sheet_nm2)
      addWorksheet(wb, sheet_nm2)
      writeData(wb, sheet_nm2, boot_df)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS LHS Saved"))
      
      jk <- bs_summary(output_path, sheet_nm,
                       lhs = lhs_var,
                       rhs = rhs_var)
      
      eiv_rows[[sheet_nm]] <- jk          # keep in the list
    }

    eiv_all <- dplyr::bind_rows(eiv_rows)
    print(eiv_all)
    
    remove_sheet_safely(wb, "EIV_BS")     # MEA edit: rename temporarily to not write over old results
    addWorksheet(wb, "EIV_BS")
    writeData(wb, "EIV_BS", eiv_all)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    message("✅  Combined EIV / BS Summary saved in sheet “EIV_BS")
  }
  
  if (eiv_summary) {
    # Only rebuild the combined summary from sheets written earlier by run_bs_eiv
    # (no recompute). Safe to run on its own.
    
    pairs <- list(
      c("cb_central_full",              "discretion", "EIVBS_central_discretion"),
      c("log_dif",        "FirmCont_favor_white", "EIVBS_logdif_cont_white"),
      c("log_dif",        "conduct_favor_white",        "EIVBS_logdif_cond_white"),
      c("log_dif",        "pooled_favor_white",   "EIVBS_logdif_pool_white"),
      c("log_dif",        "FirmSelective", "EIVBS_logdifbw_select"),
      c("log_dif",        "discretion", "EIVBS_logdifbw_discret"),
      c("log_dif_gender", "FirmCont_favor_male",  "EIVBS_logdif_cont_male"),
      c("log_dif_gender", "conduct_favor_male",   "EIVBS_logdif_cond_male"),
      c("log_dif_gender", "pooled_favor_male",    "EIVBS_logdif_pool_male"),
      c("log_dif_gender",        "FirmSelective", "EIVBS_logdifmf_select"),
      c("log_dif_gender",        "discretion", "EIVBS_logdifmf_discret"),
      c("log_dif_age",    "conduct_favor_younger","EIVBS_logdif_cond_young"),
      c("log_dif_age",        "FirmSelective", "EIVBS_logdifyo_select"),
      c("log_dif_age",        "discretion", "EIVBS_logdifyo_discret")
    )
    
    eiv_rows <- list()
    for (pair in pairs) {
      lhs_var  <- pair[1]
      rhs_var  <- pair[2]
      sheet_nm <- pair[3]
      
      jk <- try(
        bs_summary(output_path, sheet_nm, lhs = lhs_var, rhs = rhs_var),
        silent = TRUE
      )
      if (inherits(jk, "try-error")) {
        message("⚠️ Skipping ", sheet_nm, " (sheet missing or unreadable).")
        next
      }
      eiv_rows[[sheet_nm]] <- jk
    }
    
    if (length(eiv_rows)) {
      eiv_all <- dplyr::bind_rows(eiv_rows)
      remove_sheet_safely(wb, "EIV_BS")
      addWorksheet(wb, "EIV_BS")
      writeData(wb, "EIV_BS", eiv_all)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      message("✅  EIV_BS summary rebuilt (no recompute).")
    } else {
      message("⚠️  No component EIVBS_* sheets found to summarize.")
    }
  }
  
  if (eiv_bivariate) {
    
    pairs <- list(
      c("log_dif","FirmSelective", "discretion", "EIVBS_logdifbw_sel_dis"),
      c("log_dif_gender", "FirmSelective", "discretion", "EIVBS_logdifmf_sel_dis"),
      c("log_dif_age","FirmSelective", "discretion", "EIVBS_logdifyo_sel_dis")
    )
    
    # 2. Loop over that list
    eiv_rows <- list()      # <- will hold the per-pair summary tibbles
    
    for(pair in pairs) {
      lhs_var <- pair[1]
      rhs_var1 <- pair[2]
      rhs_var2 <- pair[3]
      sheet_nm <- pair[4]
      
      print(paste0("Running EIV Procedure: ", lhs_var, " ~ ", rhs_var1, " + ", rhs_var2))
      weights <- data %>% select(firm_id, njobs) %>% rename(weights = njobs) %>% select(firm_id,weights) %>% distinct()
      eiv <- eiv_run(output_path, industry_map, lhs_var, rhs_var1, rhs_var2, borda = FALSE, weights = weights)
      
      remove_sheet_safely(wb, sheet_nm)
      addWorksheet(wb, sheet_nm)
      writeData(wb, sheet_nm, eiv)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS EIV Results Saved: ",lhs_var," ~ ",rhs_var1, " + ", rhs_var2))
      
      jk <- bs_summary(output_path, sheet_nm,
                       lhs = lhs_var,
                       rhs = paste0(rhs_var1, " + ", rhs_var2))
      
      eiv_rows[[sheet_nm]] <- jk          # keep in the list
    }
    
    
    eiv_all <- dplyr::bind_rows(eiv_rows)
    print(eiv_all)
    
    remove_sheet_safely(wb, "EIV_BIVARIATE")     # MEA edit: rename temporarily to not write over old results
    addWorksheet(wb, "EIV_BIVARIATE")
    writeData(wb, "EIV_BIVARIATE", eiv_all)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    message("✅  Combined EIV / BS Summary saved in sheet “EIV_BS")
    
  }
  
  
  # Section ID: This section calculates covariance noise for each pair of outcomes.
  # The covariance noise is calculated using the sandwich formula.
  if (run_pairwise_process) {
    set.seed(seed)
    
    temp <- data %>% filter(!is.na(dif))
    unique_firms <- unique(temp$firm_id)
    
    pairs <- combn(survey_vars, 2, simplify = FALSE)
    pairwise_rows <- vector("list", length(pairs) * 2)
    i <- 1L
    
    for (pair in pairs) {
      var1 <- pair[[1]]
      var2 <- pair[[2]]
      
      # retrieve stored PL objects
      S1    <- S_list[[var1]]
      S2    <- S_list[[var2]]
      Binv1 <- Binv_list[[var1]]
      Binv2 <- Binv_list[[var2]]
      cov1  <- cov_list[[var1]]
      cov2  <- cov_list[[var2]]
      
      # -------------------------
      # Restricted (unique firms)
      # -------------------------
      coeff_sub <- coeff_df %>%
        dplyr::filter(firm_id %in% unique_firms) %>%
        dplyr::arrange(match(firm_id, unique_firms))
      
      firm_cols <- paste0("firm", coeff_sub$firm_id)
      
      S1_sub   <- S1[, c("resp_id", firm_cols), drop = FALSE]
      S2_sub   <- S2[, c("resp_id", firm_cols), drop = FALSE]
      Binv1_sub <- Binv1[firm_cols, firm_cols, drop = FALSE]
      Binv2_sub <- Binv2[firm_cols, firm_cols, drop = FALSE]
      cov1_sub  <- cov1[firm_cols, firm_cols, drop = FALSE]
      cov2_sub  <- cov2[firm_cols, firm_cols, drop = FALSE]
      
      res_restricted <- pairwise_process(
        S1 = S1_sub,
        S2 = S2_sub,
        Binv1 = Binv1_sub,
        Binv2 = Binv2_sub,
        robust_cov1 = cov1_sub,
        robust_cov2 = cov2_sub,
        coeff_df = coeff_sub,
        var1 = var1,
        var2 = var2
      )
      
      pairwise_rows[[i]] <- tibble::as_tibble(res_restricted) %>%
        dplyr::mutate(
          lhs = var1,
          rhs = var2,
          all_firms = FALSE,
          .before = 1
        )
      i <- i + 1L
      
      # -------------------------
      # All firms
      # -------------------------
      res_all <- pairwise_process(
        S1 = S1,
        S2 = S2,
        Binv1 = Binv1,
        Binv2 = Binv2,
        robust_cov1 = cov1,
        robust_cov2 = cov2,
        coeff_df = coeff_df,
        var1 = var1,
        var2 = var2
      )
      
      pairwise_rows[[i]] <- tibble::as_tibble(res_all) %>%
        dplyr::mutate(
          lhs = var1,
          rhs = var2,
          all_firms = TRUE,
          .before = 1
        )
      i <- i + 1L
    }
    
    pairwise_summary <- dplyr::bind_rows(pairwise_rows)
    
    remove_sheet_safely(wb, "pairwise_summary")
    openxlsx::addWorksheet(wb, "pairwise_summary")
    openxlsx::writeData(wb, "pairwise_summary", pairwise_summary)
    openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
    
    message("✅ Pairwise summary saved to Excel")
  }
  
  
  
  # Section IE: This section tests the IIA assumption of the Plackett Luce Model
  # To do this, we compare the unrestricted model: 3 models (1 for each anchor firm) 
  # To the restricted model a single model.
  # This is done for all outcomes in the data.
  # Results are summarized in IIA_summary and IIA_detail.
  
  if (diagnostic) {
    set.seed(seed)
    reference_firms <- c(38, 76, 90)
    
    iia_summary_list <- list()
    iia_detail_list  <- list()
    
    .fit_pl_ll <- function(M_num) {
      # keep rows with >=2 finite ranks
      row_ok <- rowSums(is.finite(M_num)) >= 2
      M_num  <- M_num[row_ok, , drop = FALSE]
      # keep columns that appear at least once
      col_ok <- colSums(is.finite(M_num)) > 0
      M_num  <- M_num[, col_ok, drop = FALSE]
      
      J <- ncol(M_num)
      if (nrow(M_num) == 0L || J < 2L) {
        return(list(loglik = NA_real_, J = J, p = NA_integer_, tie_params = NA_integer_))
      }
      
      # Fit PL; (uses whatever tie handling the data implies)
      mod <- PlackettLuce(M_num, npseudo = 0.5)
      
      # Parameter count = number of free coefficients returned
      p <- length(coef(mod)) - 1 # includes (J-1) item-worth params + tie params (if any)
      
      # How many of those are tie params (for info only)
      tie_params <- if (is.finite(p)) max(0L, p - (J - 1L)) else NA_integer_
      
      list(
        loglik     = as.numeric(mod$loglik),
        J          = J,
        p          = p,
        tie_params = tie_params,
        n_rows     = nrow(M_num)
      )
    }
    
    as_matrix_num <- function(wide_df) {
      firm_cols <- grep("^firm\\d+$", names(wide_df), value = TRUE)
      X <- wide_df[, firm_cols, drop = FALSE]
      X[] <- lapply(X, function(v) { v <- suppressWarnings(as.numeric(v)); v[v <= 0] <- NA_real_; v })
      as.matrix(X)
    }
    
    for (outcome in survey_vars) {
      message("IIA diagnostic: ", outcome)

      M_pool  <- as_matrix_num(data_list[[outcome]])
      fit_pool <- .fit_pl_ll(M_pool)
      
      ## ---------- Anchor-specific models (re-prep per anchor subset) ----------
      detail_rows <- list()
      ll_vec <- numeric(0)
      p_vec  <- integer(0)
      J_vec  <- integer(0)
      
      for (a_id in reference_firms) {
        # Respondents who faced anchor a_id (for this outcome)
        anchor_ids <- data %>%
          dplyr::filter(firm_id == a_id) %>%
          dplyr::filter(!is.na(.data[[outcome]]), .data[[outcome]] > 0) %>%
          dplyr::distinct(ResponseId) %>%
          dplyr::pull(ResponseId)
        
        if (length(anchor_ids) == 0L) {
          detail_rows[[length(detail_rows)+1]] <- data.frame(
            outcome  = outcome, anchor_id = a_id,
            Jg       = NA_integer_, pg = NA_integer_, tie_params_g = NA_integer_,
            loglik   = NA_real_, n_rows = 0L, stringsAsFactors = FALSE
          )
          next
        }
        
        data_a <- dplyr::filter(data, ResponseId %in% anchor_ids)
        
        prep_a <- prepare_pltree_data(
          data            = data_a,
          rank_col        = outcome,
          subgroup_var    = NULL,
          subgroup_filter = NULL
        )
        
        M_a   <- as_matrix_num(prep_a$data_wide_pltree)
        fit_a <- .fit_pl_ll(M_a)
        
        ll_vec <- c(ll_vec, fit_a$loglik)
        p_vec  <- c(p_vec,  fit_a$p)
        J_vec  <- c(J_vec,  fit_a$J)
        
        detail_rows[[length(detail_rows)+1]] <- data.frame(
          outcome      = outcome,
          anchor_id    = a_id,
          Jg           = fit_a$J,
          pg           = fit_a$p,
          tie_params_g = fit_a$tie_params,
          loglik       = fit_a$loglik,
          n_rows       = fit_a$n_rows,
          stringsAsFactors = FALSE
        )
      }
      
      detail_df <- dplyr::bind_rows(detail_rows)
      
      ## ---------- LR test with tie-aware df ----------
      L0   <- fit_pool$loglik                 # pooled (restricted)
      p0   <- fit_pool$p                      # params in pooled model
      J0   <- fit_pool$J
      ties0<- fit_pool$tie_params
      
      if (!is.finite(L0) || !any(is.finite(ll_vec)) || !is.finite(p0)) {
        iia_summary_list[[outcome]] <- data.frame(
          outcome = outcome,
          pooled_loglik = L0,
          L1_sum = sum(ll_vec, na.rm = TRUE),
          LR = NA_real_, df = NA_integer_, p_value = NA_real_,
          J_pooled = J0, p_pooled = p0, tie_params_pooled = ties0,
          J_sub_sum = sum(J_vec, na.rm = TRUE), p_sub_sum = sum(p_vec, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
        iia_detail_list[[outcome]] <- detail_df
        next
      }
      
      L1   <- sum(ll_vec, na.rm = TRUE)       # unrestricted = sum of anchor fits
      p1   <- sum(p_vec,  na.rm = TRUE)       # total params across anchor fits
      dfLR <- p1 - p0                         # <-- tie-aware df
      LR   <- 2 * (L1 - L0)
      pv   <- if (is.finite(LR) && is.finite(dfLR) && dfLR > 0)
        pchisq(LR, df = dfLR, lower.tail = FALSE) else NA_real_
      
      iia_summary_list[[outcome]] <- data.frame(
        outcome = outcome,
        pooled_loglik = L0,
        L1_sum = L1,
        LR = LR,
        df = dfLR,
        p_value = pv,
        J_pooled = J0,
        p_pooled = p0,
        tie_params_pooled = ties0,
        J_sub_sum = sum(J_vec, na.rm = TRUE),
        p_sub_sum = p1,
        stringsAsFactors = FALSE
      )
      
      iia_detail_list[[outcome]] <- detail_df
    }
    
    iia_summary <- dplyr::bind_rows(iia_summary_list)
    iia_detail  <- dplyr::bind_rows(iia_detail_list)
    
    remove_sheet_safely(wb, "IIA_Summary")
    addWorksheet(wb, "IIA_Summary")
    writeData(wb, "IIA_Summary", iia_summary)
    
    remove_sheet_safely(wb, "IIA_Detail")
    addWorksheet(wb, "IIA_Detail")
    writeData(wb, "IIA_Detail", iia_detail)
    
    cat("✅ IIA diagnostic written to sheets: IIA_Summary, IIA_Detail\n")
  }
  
  
  ################################################################################
  # Section II: Borda Scores
  ################################################################################
  # Section IIA: This section calculates the Borda score for the full sample
  if (borda_score) {
    set.seed(seed)
    
    # Containers: one row per firm; columns = outcomes
    borda_mean_df <- data.frame()
    borda_se_df   <- data.frame()
    borda_eb_df   <- data.frame()   # EB posterior means (wide by outcome)
    
    # collect psi1 and its SE per outcome
    psi_rows_borda <- list()
    
    # helper to sanitize Excel sheet names (<=31 chars, no []:/\?*)
    sanitize_sheet <- function(x) {
      x <- gsub("[\\\\/*?:\\[\\]]", "_", x)
      substr(x, 1, 31)
    }
    
    for (outcome in survey_vars) {
      cat("Computing Borda scores for outcome:", outcome, "\n")
      
      data_wide <-  data_list[[outcome]]  # resp_id + firm1..firmN (0 / 1..K)
      id_map    <- id_map_list[[outcome]]
      
      # -------- Individual-level Borda --------
      # lower ranks are better (1 best), so higher_is_better = FALSE
      reference_firms <- c(38, 76, 90)
      B_indiv <- compute_borda_individual_wide(
        data_wide        = data_wide,
        id_map           = id_map,
        id_var           = "resp_id",
        higher_is_better = FALSE,
        normalize        = TRUE,
        ref_firm_ids     = reference_firms
      )
      
      # Center B_indiv (global shift; later centering by C is invariant to this)
      B_indiv_c <- B_indiv %>%
        dplyr::mutate(
          B = B - mean(B, na.rm = TRUE)
        )
      
      # -------- Aggregate to firm means & SE --------
      B_sum <- summarize_borda_by_firm(B_indiv_c) %>%
        dplyr::arrange(firm_id)
      
      # Firm order (ALL firms) used consistently
      firm_ids_all <- B_sum$firm_id
      
      # 97-firm subset (intersection with firms97)
      firm_ids_97 <- firm_ids_all[firm_ids_all %in% firms97]
      
      # Precompute centering matrices for ALL firms and 97 firms
      J_all <- length(firm_ids_all)
      C_all <- diag(J_all) - matrix(1 / J_all, J_all, J_all)
      
      J_97 <- length(firm_ids_97)
      C_97 <- if (J_97 >= 2) diag(J_97) - matrix(1 / J_97, J_97, J_97) else NULL
      
      # -------- Sandwich Σ_B for ALL firms (FULL SAMPLE, unweighted) -------------
      Sigma_B_all_full <- compute_borda_sandwich_cov(
        B_indiv    = B_indiv_c,
        firm_order = firm_ids_all,
        id_var     = "resp_id",
        weight_var = NULL
      )
      
      B_sum_all_ord <- B_sum[match(firm_ids_all, B_sum$firm_id), , drop = FALSE]
      theta_all_raw <- B_sum_all_ord$mean_B
      
      # NEW: recenter ALL-firm means and adjust Sigma with C_all
      theta_all_full <- as.numeric(C_all %*% theta_all_raw)
      Sigma_B_all_c  <- C_all %*% Sigma_B_all_full %*% t(C_all)
      
      vc_all_full <- var_component_with_var(
        theta_hat = theta_all_full,
        Sigma     = Sigma_B_all_c
      )
      
      var_comp_B_all_full <- list(
        tot_var    = var(theta_all_full),
        Y          = mean(theta_all_full^2),
        noise      = mean(diag(Sigma_B_all_c)),
        sigma2_hat = vc_all_full$sigma2_hat,
        Vhat       = vc_all_full$Vhat,
        se_sigma2  = vc_all_full$se_sigma2,
        sigma2_dot = katz_correct(vc_all_full$sigma2_hat, vc_all_full$Vhat)
      )
      
      # -------- Sandwich Σ_B for 97-firm subset (FULL SAMPLE, unweighted) --------
      if (J_97 >= 2) {
        Sigma_B_97_full <- compute_borda_sandwich_cov(
          B_indiv    = B_indiv_c,
          firm_order = firm_ids_97,
          id_var     = "resp_id",
          weight_var = NULL
        )
        
        B_sum_97_ord <- B_sum[match(firm_ids_97, B_sum$firm_id), , drop = FALSE]
        theta_97_raw <- B_sum_97_ord$mean_B
        
        # NEW: recenter 97-firm means and adjust Sigma with C_97
        theta_97_full <- as.numeric(C_97 %*% theta_97_raw)
        Sigma_B_97_c  <- C_97 %*% Sigma_B_97_full %*% t(C_97)
        
        vc_97_full <- var_component_with_var(
          theta_hat = theta_97_full,
          Sigma     = Sigma_B_97_c
        )
        
        var_comp_B_97_full <- list(
          tot_var    = var(theta_97_full),
          Y          = mean(theta_97_full^2),
          noise      = mean(diag(Sigma_B_97_c)),
          sigma2_hat = vc_97_full$sigma2_hat,
          Vhat       = vc_97_full$Vhat,
          se_sigma2  = vc_97_full$se_sigma2,
          sigma2_dot = katz_correct(vc_97_full$sigma2_hat, vc_97_full$Vhat)
        )
      } else {
        var_comp_B_97_full <- list(
          tot_var    = NA_real_,
          Y          = NA_real_,
          noise      = NA_real_,
          sigma2_hat = NA_real_,
          Vhat       = NA_real_,
          se_sigma2  = NA_real_,
          sigma2_dot = NA_real_
        )
      }
      
      # -------- Stack means & SEs across outcomes (wide) -------------------------
      mean_col <- B_sum %>%
        dplyr::select(firm_id, firm, !!rlang::sym(outcome) := mean_B)
      se_col   <- B_sum %>%
        dplyr::select(firm_id, firm, !!rlang::sym(outcome) := se_B)
      
      if (nrow(borda_mean_df) == 0) {
        borda_mean_df <- mean_col
        borda_se_df   <- se_col
      } else {
        borda_mean_df <- dplyr::left_join(borda_mean_df, mean_col, by = c("firm_id","firm"))
        borda_se_df   <- dplyr::left_join(borda_se_df,   se_col,   by = c("firm_id","firm"))
      }
      
      # ================================
      #   Two-step EB on Borda
      # ================================
      eb_input <- B_sum %>%
        dplyr::transmute(
          firm_id, firm,
          theta_hat = as.numeric(mean_B),
          s         = pmax(as.numeric(se_B), 1e-8)  # guard against zeros
        )
      
      valid    <- is.finite(eb_input$theta_hat) & is.finite(eb_input$s) & eb_input$s > 0
      theta_eb <- rep(NA_real_, nrow(eb_input))
      psi1_est <- NA_real_
      psi1_se  <- NA_real_
      
      if (sum(valid) >= 2) {
        eb_fit <- eb_two_step(theta_hat = eb_input$theta_hat[valid],
                              s         = eb_input$s[valid])
        theta_eb[valid] <- eb_fit$theta_eb
        
        # pull psi_1 and its SE from the step-1 regression (or refit if needed)
        if (!is.null(eb_fit$lm_step1)) {
          co <- tryCatch(coef(summary(eb_fit$lm_step1)), error = function(e) NULL)
          if (!is.null(co)) {
            rn <- rownames(co)
            if ("log_s" %in% rn) {
              psi1_est <- co["log_s", "Estimate"]
              psi1_se  <- co["log_s", "Std. Error"]
            } else if ("log(s)" %in% rn) {
              psi1_est <- co["log(s)", "Estimate"]
              psi1_se  <- co["log(s)", "Std. Error"]
            }
          }
        }
        if (is.na(psi1_est) || is.na(psi1_se)) {
          dfv <- transform(
            data.frame(theta_hat = eb_input$theta_hat[valid], s = eb_input$s[valid]),
            log_s = log(s)
          )
          fit1b  <- lm(theta_hat ~ log_s, data = dfv)
          sm1b   <- coef(summary(fit1b))
          psi1_est <- as.numeric(sm1b["log_s", "Estimate"])
          psi1_se  <- as.numeric(sm1b["log_s", "Std. Error"])
        }
      } else {
        warning("Borda EB step skipped for ", outcome, " (not enough valid firms).")
      }
      
      # store EB posterior means (wide by outcome)
      res_df_eb <- eb_input %>%
        dplyr::select(firm_id, firm) %>%
        dplyr::mutate(!!outcome := theta_eb)
      
      if (nrow(borda_eb_df) == 0) {
        borda_eb_df <- res_df_eb
      } else {
        borda_eb_df <- dplyr::left_join(borda_eb_df, res_df_eb, by = c("firm_id","firm"))
      }
      
      # store psi_1 row for this outcome
      psi_rows_borda[[length(psi_rows_borda) + 1L]] <- data.frame(
        outcome  = outcome,
        psi_1    = psi1_est,
        se_psi_1 = psi1_se,
        stringsAsFactors = FALSE
      )
      
      # ================================
      # Bootstrap with respondent-level weights
      # ================================
      if (borda_bs_w) {
        set.seed(seed)
        
        # 1) Unique respondents (stable order)
        resp_ids <- B_indiv %>%
          dplyr::distinct(resp_id) %>%
          dplyr::arrange(resp_id) %>%
          dplyr::pull(resp_id)
        
        n <- length(resp_ids)   # number of respondents
        
        # 2) Generate respondent-level weights: n x B matrix
        weights_mat <- generate_weights(n, B, seed = seed)
        colnames(weights_mat) <- paste0("w_", seq_len(B))
        
        # 3) Build the respondent-level weights table
        weights_df <- cbind(resp_id = resp_ids, as.data.frame(weights_mat))
        
        # 4) Save respondent weights to Excel (one row per resp_id)
        sheet_nm <- paste0("borda_w_", outcome)
        remove_sheet_safely(wb, sheet_nm)
        addWorksheet(wb, sheet_nm)
        writeData(wb, sheet = sheet_nm, x = weights_df)
        
        # 5) Merge weights back so each row inherits respondent weights
        B_indiv_w <- B_indiv %>%
          dplyr::left_join(weights_df, by = "resp_id")
        
        # Preallocate storage: one row per firm, one col per replicate
        firms <- dplyr::distinct(B_indiv, firm_id, firm) %>%
          dplyr::arrange(firm_id)
        mean_mat <- matrix(NA_real_, nrow = nrow(firms), ncol = B,
                           dimnames = list(firms$firm, paste0("iter_", seq_len(B))))
        se_mat   <- matrix(NA_real_, nrow = nrow(firms), ncol = B,
                           dimnames = list(firms$firm, paste0("se_", seq_len(B))))
        
        # Preallocate signal storage for ALL firms and 97 firms (iter 0..B)
        tot_var_all_vec    <- numeric(B + 1L)
        Y_all_vec          <- numeric(B + 1L)
        noise_all_vec      <- numeric(B + 1L)
        sigma2_hat_all_vec <- numeric(B + 1L)
        Vhat_all_vec       <- numeric(B + 1L)
        se_sigma2_all_vec  <- numeric(B + 1L)
        sigma2_dot_all_vec <- numeric(B + 1L)
        
        tot_var_97_vec    <- numeric(B + 1L)
        Y_97_vec          <- numeric(B + 1L)
        noise_97_vec      <- numeric(B + 1L)
        sigma2_hat_97_vec <- numeric(B + 1L)
        Vhat_97_vec       <- numeric(B + 1L)
        se_sigma2_97_vec  <- numeric(B + 1L)
        sigma2_dot_97_vec <- numeric(B + 1L)
        
        # iter 0 = full-sample Borda signals (already centered via C_all/C_97)
        tot_var_all_vec[1]    <- var_comp_B_all_full$tot_var
        Y_all_vec[1]          <- var_comp_B_all_full$Y
        noise_all_vec[1]      <- var_comp_B_all_full$noise
        sigma2_hat_all_vec[1] <- var_comp_B_all_full$sigma2_hat
        Vhat_all_vec[1]       <- var_comp_B_all_full$Vhat
        se_sigma2_all_vec[1]  <- var_comp_B_all_full$se_sigma2
        sigma2_dot_all_vec[1] <- var_comp_B_all_full$sigma2_dot
        
        tot_var_97_vec[1]    <- var_comp_B_97_full$tot_var
        Y_97_vec[1]          <- var_comp_B_97_full$Y
        noise_97_vec[1]      <- var_comp_B_97_full$noise
        sigma2_hat_97_vec[1] <- var_comp_B_97_full$sigma2_hat
        Vhat_97_vec[1]       <- var_comp_B_97_full$Vhat
        se_sigma2_97_vec[1]  <- var_comp_B_97_full$se_sigma2
        sigma2_dot_97_vec[1] <- var_comp_B_97_full$sigma2_dot
        
        # Loop over bootstrap replicates
        for (i in seq_len(B)) {
          message("Borda bootstrap iter: ", i)
          w_col <- paste0("w_", i)
          
          # Summaries using the bootstrap weights
          B_sum_b <- summarize_borda_by_firm(B_indiv_w, weights = B_indiv_w[[w_col]])
          
          # Align to ALL-firms order
          B_sum_b_all <- B_sum_b[match(firm_ids_all, B_sum_b$firm_id), , drop = FALSE]
          
          # store mean & se matrices aligned with 'firms' (all firms)
          idx_all <- match(firms$firm_id, B_sum_b$firm_id)
          mean_mat[, i] <- B_sum_b$mean_B[idx_all]
          se_mat[,   i] <- B_sum_b$se_B[idx_all]
          
          # Recompute Sigma_B for THIS bootstrap rep (ALL firms)
          Sigma_B_all_b <- compute_borda_sandwich_cov(
            B_indiv    = B_indiv_w,
            firm_order = firm_ids_all,
            id_var     = "resp_id",
            weight_var = w_col
          )
          
          theta_all_raw_b <- B_sum_b_all$mean_B
          theta_all_b     <- as.numeric(C_all %*% theta_all_raw_b)
          Sigma_B_all_b_c <- C_all %*% Sigma_B_all_b %*% t(C_all)
          
          vc_all_b <- var_component_with_var(
            theta_hat = theta_all_b,
            Sigma     = Sigma_B_all_b_c
          )
          
          tot_var_all_vec[i + 1L]    <- var(theta_all_b)
          Y_all_vec[i + 1L]          <- mean(theta_all_b^2)
          noise_all_vec[i + 1L]      <- mean(diag(Sigma_B_all_b_c))
          sigma2_hat_all_vec[i + 1L] <- vc_all_b$sigma2_hat
          Vhat_all_vec[i + 1L]       <- vc_all_b$Vhat
          se_sigma2_all_vec[i + 1L]  <- vc_all_b$se_sigma2
          sigma2_dot_all_vec[i + 1L] <- katz_correct(vc_all_b$sigma2_hat, vc_all_b$Vhat)
          
          # 97-firm subset for this bootstrap rep
          if (J_97 >= 2) {
            B_sum_b_97 <- B_sum_b[match(firm_ids_97, B_sum_b$firm_id), , drop = FALSE]
            
            Sigma_B_97_b <- compute_borda_sandwich_cov(
              B_indiv    = B_indiv_w,
              firm_order = firm_ids_97,
              id_var     = "resp_id",
              weight_var = w_col
            )
            
            theta_97_raw_b <- B_sum_b_97$mean_B
            theta_97_b     <- as.numeric(C_97 %*% theta_97_raw_b)
            Sigma_B_97_b_c <- C_97 %*% Sigma_B_97_b %*% t(C_97)
            
            vc_97_b <- var_component_with_var(
              theta_hat = theta_97_b,
              Sigma     = Sigma_B_97_b_c
            )
            
            tot_var_97_vec[i + 1L]    <- var(theta_97_b)
            Y_97_vec[i + 1L]          <- mean(theta_97_b^2)
            noise_97_vec[i + 1L]      <- mean(diag(Sigma_B_97_b_c))
            sigma2_hat_97_vec[i + 1L] <- vc_97_b$sigma2_hat
            Vhat_97_vec[i + 1L]       <- vc_97_b$Vhat
            se_sigma2_97_vec[i + 1L]  <- vc_97_b$se_sigma2
            sigma2_dot_97_vec[i + 1L] <- katz_correct(vc_97_b$sigma2_hat, vc_97_b$Vhat)
          } else {
            tot_var_97_vec[i + 1L]    <- NA_real_
            Y_97_vec[i + 1L]          <- NA_real_
            noise_97_vec[i + 1L]      <- NA_real_
            sigma2_hat_97_vec[i + 1L] <- NA_real_
            Vhat_97_vec[i + 1L]       <- NA_real_
            se_sigma2_97_vec[i + 1L]  <- NA_real_
            sigma2_dot_97_vec[i + 1L] <- NA_real_
          }
        }
        
        # Bind firm info back in for means and SEs
        mean_df <- cbind(firms, as.data.frame(mean_mat))
        se_df   <- cbind(firms, as.data.frame(se_mat))
        
        # Write mean & SE to Excel
        sheet_mean <- paste0("borda_", outcome)
        remove_sheet_safely(wb, sheet_mean)
        addWorksheet(wb, sheet_mean)
        writeData(wb, sheet_mean, mean_df)
        
        sheet_se <- paste0("borda_se_", outcome)
        remove_sheet_safely(wb, sheet_se)
        addWorksheet(wb, sheet_se)
        writeData(wb, sheet_se, se_df)
        
        # Write Borda signal (variance component + Katz) for iter 0..B
        var_comp_all_df <- data.frame(
          iter       = 0:B,
          tot_var    = tot_var_all_vec,
          Y          = Y_all_vec,
          noise      = noise_all_vec,
          sigma2_hat = sigma2_hat_all_vec,
          Vhat       = Vhat_all_vec,
          se_sigma2  = se_sigma2_all_vec,
          sigma2_dot = sigma2_dot_all_vec,
          all_firms  = TRUE
        )
        
        var_comp_97_df <- data.frame(
          iter       = 0:B,
          tot_var    = tot_var_97_vec,
          Y          = Y_97_vec,
          noise      = noise_97_vec,
          sigma2_hat = sigma2_hat_97_vec,
          Vhat       = Vhat_97_vec,
          se_sigma2  = se_sigma2_97_vec,
          sigma2_dot = sigma2_dot_97_vec,
          all_firms  = FALSE
        )
        
        var_comp_B_df <- rbind(var_comp_all_df, var_comp_97_df)
        
        sheet_sig <- sanitize_sheet(paste0("b_s_", outcome))
        remove_sheet_safely(wb, sheet_sig)
        addWorksheet(wb, sheet_sig)
        writeData(wb, sheet_sig, var_comp_B_df)
      }  # end if (borda_bs_w)
    } # end for outcomes
    
    # -------- Write final Borda tables --------
    remove_sheet_safely(wb, "borda_score")
    addWorksheet(wb, "borda_score")
    writeData(wb, "borda_score", borda_mean_df)
    
    remove_sheet_safely(wb, "borda_score_se")
    addWorksheet(wb, "borda_score_se")
    writeData(wb, "borda_score_se", borda_se_df)
    
    # EB posterior means for Borda
    remove_sheet_safely(wb, "borda_score_eb")
    addWorksheet(wb, "borda_score_eb")
    writeData(wb, "borda_score_eb", borda_eb_df)
    
    # psi_1 + SE per outcome for Borda EB
    psi_df_borda <- dplyr::bind_rows(psi_rows_borda)
    remove_sheet_safely(wb, "borda_EB_psi1")
    addWorksheet(wb, "borda_EB_psi1")
    writeData(wb, "borda_EB_psi1", psi_df_borda)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    cat("✅ Borda Score (means) -> 'borda_score', SEs -> 'borda_score_se', EB -> 'borda_score_eb', EB hyperparameters -> 'borda_EB_psi1', and Borda signal sheets 'b_s_[outcome]' (all_firms TRUE/FALSE) saved\n")
  }
  
  
  # Section IIC: This section runs eiv for the borda score
  if (run_borda_eiv) {
    set.seed(seed)
    
    pairs <- list(
      c("cb_central_full",              "discretion", "EIVBordaw_central_discretion"),
      c("log_dif",                      "FirmCont_favor_white", "EIVBordaw_logdif_cont_white"),
      c("log_dif",                      "conduct_favor_white", "EIVBordaw_logdif_cond_white"),
      c("log_dif",                      "pooled_favor_white", "EIVBordaw_logdif_pool_white"),
      c("log_dif_gender",               "FirmCont_favor_male" , "EIVBordaw_logdif_cont_male"),
      c("log_dif_gender",               "conduct_favor_male" , "EIVBordaw_logdif_cond_male"),
      c("log_dif_gender",               "pooled_favor_male" , "EIVBordaw_logdif_pool_male"),
      c("log_dif_age",                  "conduct_favor_younger" , "EIVBordaw_logdif_cond_young")
    )


    # 2. Loop over that list
    eiv_rows <- list()      # <- will hold the per-pair summary tibbles
    
    for(pair in pairs) {
      lhs_var <- pair[1]
      rhs_var <- pair[2]
      sheet_nm <- pair[3]
      sheet_nm2 <- paste0("bs_",lhs_var)
      
      print(paste0("Running BS EIV Procedure: ", lhs_var, " ~ ", rhs_var))
      weights <- data %>% select(firm_id, njobs) %>% rename(weights = njobs) %>% select(firm_id,weights) %>% distinct()
      results <- bs_eiv_run(output_path, industry_map, lhs_var, rhs_var, borda =TRUE, center = TRUE, weights = weights)
      eiv <- results$eiv
      boot_df <- results$lhs_boot
      
      remove_sheet_safely(wb, sheet_nm)
      addWorksheet(wb, sheet_nm)
      writeData(wb, sheet_nm, eiv)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS EIV Results Saved: ",lhs_var," ~ ",rhs_var))
      
      remove_sheet_safely(wb, sheet_nm2)
      addWorksheet(wb, sheet_nm2)
      writeData(wb, sheet_nm2, boot_df)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS LHS Saved"))
      
      jk <- bs_summary(output_path, sheet_nm,
                       lhs = lhs_var,
                       rhs = rhs_var)
      
      eiv_rows[[sheet_nm]] <- jk          # keep in the list
    }
    
    eiv_all <- dplyr::bind_rows(eiv_rows)
    print(eiv_all)
    
    remove_sheet_safely(wb, "EIV_Bordaw")
    addWorksheet(wb, "EIV_Bordaw")
    writeData(wb, "EIV_Bordaw", eiv_all)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    message("✅  Combined EIV / BS Summary saved in sheet “EIV_Bordaw")  
    }
  
  ##
  if (borda_eiv_summary) {
    # Only rebuild the combined summary for the Borda EIV from saved sheets
    
    pairs <- list(
      c("cb_central_full",              "discretion", "EIVBordaw_central_discretion"),
      c("log_dif",        "FirmCont_favor_white", "EIVBordaw_logdif_cont_white"),
      c("log_dif",        "conduct_favor_white",        "EIVBordaw_logdif_cond_white"),
      c("log_dif",        "pooled_favor_white",   "EIVBordaw_logdif_pool_white"),
      c("log_dif_gender", "FirmCont_favor_male",  "EIVBordaw_logdif_cont_male"),
      c("log_dif_gender", "conduct_favor_male",   "EIVBordaw_logdif_cond_male"),
      c("log_dif_gender", "pooled_favor_male",    "EIVBordaw_logdif_pool_male"),
      c("log_dif_age",    "conduct_favor_younger","EIVBordaw_logdif_cond_young")
    )
    
    eiv_rows <- list()
    for (pair in pairs) {
      lhs_var  <- pair[1]
      rhs_var  <- pair[2]
      sheet_nm <- pair[3]
      
      jk <- try(
        bs_summary(output_path, sheet_nm, lhs = lhs_var, rhs = rhs_var),
        silent = TRUE
      )
      if (inherits(jk, "try-error")) {
        message("⚠️ Skipping ", sheet_nm, " (sheet missing or unreadable).")
        next
      }
      eiv_rows[[sheet_nm]] <- jk
    }
    
    if (length(eiv_rows)) {
      eiv_all <- dplyr::bind_rows(eiv_rows)
      remove_sheet_safely(wb, "EIV_Bordaw")
      addWorksheet(wb, "EIV_Bordaw")
      writeData(wb, "EIV_Bordaw", eiv_all)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      message("✅  EIV_Bordaw summary rebuilt (no recompute).")
    } else {
      message("⚠️  No component EIVBordaw_* sheets found to summarize.")
    }
  }
  
  # Section IID: This section runs pairwise processes borda score outcomes
  # The covariance noise is calculated using the sandwich formula.
  if (run_pairwise_process_borda) {
    set.seed(seed)
    
    # empty lists to store results
    Bindiv_list <- list()
    temp <- data %>% filter(!is.na(dif))
    unique_firms <- unique(temp$firm_id)
    
    # 1) prep once and store
    for (outcome in survey_vars) {
      cat("Preparing:", outcome, "\n")
      data_wide <-  data_list[[outcome]]   # resp_id + firm1..firmN (0 / 1..K)
      id_map    <- id_map_list[[outcome]]

      B_indiv <- compute_borda_individual_wide(
        data_wide        = data_wide,
        id_map           = id_map,
        id_var           = "resp_id",
        higher_is_better = FALSE,
        normalize        = TRUE,  # set TRUE if you prefer 0..1 normalization
        ref_firm_ids     = reference_firms
      )
      
      Bindiv_list[[outcome]]   <- B_indiv
    }
    
    pairs <- combn(survey_vars, 2, simplify = FALSE)
    pairwise_rows <- vector("list", length(pairs))
    i<-1
    for (pair in pairs) {
      var1 <- pair[[1]]
      var2 <- pair[[2]]
      
      B_indiv1 <- Bindiv_list[[var1]]
      B_indiv2 <- Bindiv_list[[var2]]
      
      id_map1 <- id_map_list[[var1]]
      id_map2 <- id_map_list[[var2]]
      
      res <- pairwise_borda_process(B_indiv1 = B_indiv1, B_indiv2 = B_indiv2, unique_firms = unique_firms)
      J <- length(res$firms)
      
      pairwise_rows[[i]] <- tibble::tibble(
        lhs           = var1,
        rhs           = var2,
        N1            = res$N1,
        N2            = res$N2,
        Ncommon       = res$Ncommon,
        Ntot          = res$Ntot,
        overlap_frac  = res$overlap_frac,
        J_firms       = J,
        variance1     = res$variance1,
        variance2     = res$variance2,
        covariance    = res$covariance,
        noise1        = res$noise1,
        noise2        = res$noise2,
        noise12       = res$noise12,              # trace(Σ12)
        cov_denoised  = res$covariance - res$noise12,
        corr          = res$corr,
        corr_c        = res$corr_c,
        corr_den      = res$corr_den,
        all_firms     = res$all_firms
      )
      i<-i+1
      
      res <- pairwise_borda_process(B_indiv1 = B_indiv1, B_indiv2 = B_indiv2)
      J <- length(res$firms)
      
      pairwise_rows[[i]] <- tibble::tibble(
        lhs           = var1,
        rhs           = var2,
        N1            = res$N1,
        N2            = res$N2,
        Ncommon       = res$Ncommon,
        Ntot          = res$Ntot,
        overlap_frac  = res$overlap_frac,
        J_firms       = J,
        variance1     = res$variance1,
        variance2     = res$variance2,
        covariance    = res$covariance,
        noise1        = res$noise1,
        noise2        = res$noise2,
        noise12       = res$noise12,              # trace(Σ12)
        cov_denoised  = res$covariance - res$noise12,
        corr          = res$corr,
        corr_c        = res$corr_c,
        corr_den      = res$corr_den,
        all_firms     = res$all_firms
      )
      i<-i+1
    }
    
    pairwise_summary <- dplyr::bind_rows(pairwise_rows)
    
    # Write to Excel
    remove_sheet_safely(wb, "pairwise_summary_borda")
    openxlsx::addWorksheet(wb, "pairwise_summary_borda")
    openxlsx::writeData(wb, sheet = "pairwise_summary_borda", x = pairwise_summary)
    
    openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
    message("✅  Pairwise summary saved to Excel")
  }
  
  
  if (borda_eiv_bivariate) {
    
    pairs <- list(
      c("log_dif","FirmSelective", "discretion", "EIVBorda_logdifbw_sel_dis"),
      c("log_dif_gender", "FirmSelective", "discretion", "EIVBorda_logdifmf_sel_dis"),
      c("log_dif_age","FirmSelective", "discretion", "EIVBorda_logdifyo_sel_dis")
    )
    
    # 2. Loop over that list
    eiv_rows <- list()      # <- will hold the per-pair summary tibbles
    
    for(pair in pairs) {
      lhs_var <- pair[1]
      rhs_var1 <- pair[2]
      rhs_var2 <- pair[3]
      sheet_nm <- pair[4]
      
      print(paste0("Running EIV Procedure: ", lhs_var, " ~ ", rhs_var1, " + ", rhs_var2))
      weights <- data %>% select(firm_id, njobs) %>% rename(weights = njobs) %>% select(firm_id,weights) %>% distinct()
      eiv <- eiv_run(output_path, industry_map, lhs_var, rhs_var1, rhs_var2, borda = TRUE, weights = weights)
      
      remove_sheet_safely(wb, sheet_nm)
      addWorksheet(wb, sheet_nm)
      writeData(wb, sheet_nm, eiv)
      saveWorkbook(wb, output_path, overwrite = TRUE)
      print(paste0("BS EIV Results Saved: ",lhs_var," ~ ",rhs_var1, " + ", rhs_var2))
      
      jk <- bs_summary(output_path, sheet_nm,
                       lhs = lhs_var,
                       rhs = paste0(rhs_var1, " + ", rhs_var2))
      
      eiv_rows[[sheet_nm]] <- jk          # keep in the list
    }
    
    
    eiv_all <- dplyr::bind_rows(eiv_rows)
    print(eiv_all)
    
    remove_sheet_safely(wb, "EIV_BORDA_BIVARIATE")     # MEA edit: rename temporarily to not write over old results
    addWorksheet(wb, "EIV_BORDA_BIVARIATE")
    writeData(wb, "EIV_BORDA_BIVARIATE", eiv_all)
    
    saveWorkbook(wb, output_path, overwrite = TRUE)
    message("✅  Combined EIV / BS Summary saved in sheet “EIV_BS")
    
  }
  
  
  
  ## ---- FINAL: Summarize Signal/Noise across outcomes ----
  if (isTRUE(sum_signal_noise)) {
    # Ensure the latest sheets exist on disk before reading
    openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
    
    # Excel-safe sheet names (<=31 chars; replace forbidden chars)
    sanitize_sheet <- function(x) {
      x <- gsub("[\\\\/*?:\\[\\]]", "_", x)
      substr(x, 1, 31)
    }
    
    sheets_on_disk <- tryCatch(openxlsx::getSheetNames(output_path),
                               error = function(e) character())
    
    out_rows <- list()
    
    # Helper: extract BOTH all_firms == TRUE/FALSE at iter == 0
    extract_metrics <- function(df, oc, is_borda = FALSE, is_pl = FALSE) {
      req_cols <- c("iter", "tot_var", "noise", "sigma2_dot", "all_firms", "Vhat", "sigma2_hat")
      if (is.null(df) || !all(req_cols %in% names(df))) {
        return(NULL)
      }
      
      df0 <- df[df$iter == 0, req_cols, drop = FALSE]
      if (!nrow(df0)) return(NULL)
      
      df0$variance    <- suppressWarnings(as.numeric(df0$tot_var))
      df0$noise_level <- suppressWarnings(as.numeric(df0$noise))
      df0$signal      <- suppressWarnings(as.numeric(df0$sigma2_dot))
      
      # Clean non-finite values
      df0$variance[!is.finite(df0$variance)]         <- NA_real_
      df0$noise_level[!is.finite(df0$noise_level)]   <- NA_real_
      df0$signal[!is.finite(df0$signal)]             <- NA_real_
      
      df0$reliability <- ifelse(
        is.finite(df0$signal),
        (df0$signal / 1.64) / (1 + df0$signal / 1.64),
        NA_real_
      )
      df0$reliability[!is.finite(df0$reliability)] <- NA_real_
      
      # Build output frame (one row per all_firms value present)
      out <- df0[, c("all_firms", "variance", "noise_level", "signal", "reliability", "Vhat", "sigma2_hat")]
      out$outcome <- oc
      out$Borda   <- is_borda
      out$PL      <- is_pl
      
      # Reorder columns nicely
      out <- out[, c("outcome", "all_firms", "variance", "noise_level",
                     "signal", "reliability", "Vhat", "sigma2_hat", "Borda", "PL")]
      out
    }
    
    ## B) Borda: from b_s_<outcome> signal sheets
    for (oc in survey_vars) {
      sheet_b <- sanitize_sheet(paste0("b_s_", oc))
      if (sheet_b %in% sheets_on_disk) {
        df_b <- tryCatch(openxlsx::read.xlsx(output_path, sheet = sheet_b),
                         error = function(e) NULL)
        rows_b <- extract_metrics(df_b, oc, is_borda = TRUE, is_pl = FALSE)
        if (!is.null(rows_b)) {
          out_rows[[length(out_rows) + 1L]] <- rows_b
        } else {
          message("⚠️ Borda signal rows missing or malformed for outcome ", oc,
                  " in sheet ", sheet_b, ".")
        }
      } else {
        message("⚠️ Borda signal sheet ", sheet_b, " not found; skipping outcome ", oc, ".")
      }
    }
    
    ## C) PL: from pl_s_<outcome> signal sheets
    for (oc in survey_vars) {
      sheet_pl <- sanitize_sheet(paste0("pl_s_", oc))
      if (sheet_pl %in% sheets_on_disk) {
        df_pl <- tryCatch(openxlsx::read.xlsx(output_path, sheet = sheet_pl),
                          error = function(e) NULL)
        rows_pl <- extract_metrics(df_pl, oc, is_borda = FALSE, is_pl = TRUE)
        if (!is.null(rows_pl)) {
          out_rows[[length(out_rows) + 1L]] <- rows_pl
        } else {
          message("⚠️ PL signal rows missing or malformed for outcome ", oc,
                  " in sheet ", sheet_pl, ".")
        }
      } else {
        message("⚠️ PL signal sheet ", sheet_pl, " not found; skipping outcome ", oc, ".")
      }
    }
    
    # D) Write summary sheet
    if (length(out_rows)) {
      sum_sn <- dplyr::bind_rows(out_rows)
      remove_sheet_safely(wb, "sum_signal_noise")
      openxlsx::addWorksheet(wb, "sum_signal_noise")
      openxlsx::writeData(wb, "sum_signal_noise", sum_sn)
      openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
      cat("✅ Wrote 'sum_signal_noise' (", nrow(sum_sn), " rows)\n")
    } else {
      message("⚠️ No rows produced for 'sum_signal_noise' (missing signal sheets?).")
    }
  }
  
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("✅ Analysis pipeline completed. Results saved in", output_path, "\n")
}