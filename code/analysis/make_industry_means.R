################################################################################
# Create industry means + SEs (jobs-weighted) for:
#   - Plackett–Luce (Coefficients + Robust SEs)
#   - Ordered Logit (OL_Coefficients + OL_Robust_SEs)
#   - Borda (borda_score + borda_score_se)
#
# Writes to Excel:
#   - ind_mean_PL, ind_mean_OL, ind_mean_Borda
#
# Requires: dplyr, tidyr, openxlsx
################################################################################

make_industry_means <- function(output_path, industry_map, weights_df = NULL,
                                pl_coef_sheet = "Coefficients",
                                pl_se_sheet   = "Robust SEs",
                                ol_coef_sheet = "OL_Coefficients",
                                ol_se_sheet   = "OL_Robust_SEs",
                                borda_coef_sheet = "borda_score",
                                borda_se_sheet   = "borda_score_se",
                                out_prefix = "ind_mean") {
  
  # ---- helpers ----
  safe_read <- function(sheet) {
    if (!(sheet %in% openxlsx::getSheetNames(output_path))) return(NULL)
    tryCatch(openxlsx::read.xlsx(output_path, sheet = sheet), error = function(e) NULL)
  }
  
  # weights: default to njobs inside weights_df if provided; else try to read from data?
  # Here we assume weights_df has columns firm_id, weights (njobs)
  if (!is.null(weights_df)) {
    w_tbl <- weights_df %>%
      dplyr::select(firm_id, weights) %>%
      dplyr::distinct() %>%
      dplyr::mutate(firm_id = as.integer(firm_id),
                    weights = as.numeric(weights))
  } else {
    stop("Provide weights_df with columns firm_id and weights (njobs).")
  }
  
  ind_map <- industry_map %>%
    dplyr::select(firm_id, aer_naics2) %>%
    dplyr::distinct() %>%
    dplyr::mutate(firm_id = as.integer(firm_id))
  
  # Compute industry mean + propagated measurement-error SE:
  #   a_f = w_f / sum_g w
  #   mu_g = sum a_f x_f
  #   se_g = sqrt( sum a_f^2 se_f^2 )
  summarize_to_industry <- function(df_coef, df_se, label) {
    if (is.null(df_coef)) return(NULL)
    
    id_cols <- intersect(c("firm_id","firm"), names(df_coef))
    if (!("firm_id" %in% id_cols)) stop(label, ": firm_id missing in coef sheet.")
    
    # outcomes = numeric columns excluding id cols
    out_cols <- setdiff(names(df_coef), id_cols)
    out_cols <- out_cols[sapply(df_coef[out_cols], is.numeric)]
    
    if (!length(out_cols)) return(NULL)
    
    # SEs: if missing or column not present, fill NA
    if (is.null(df_se)) {
      df_se <- df_coef[, c(id_cols, out_cols), drop = FALSE]
      df_se[out_cols] <- NA_real_
    } else {
      # ensure same set of cols; missing -> NA
      keep_se <- intersect(c(id_cols, out_cols), names(df_se))
      df_se2 <- df_coef[, c(id_cols), drop = FALSE]
      for (oc in out_cols) {
        if (oc %in% names(df_se)) df_se2[[oc]] <- as.numeric(df_se[[oc]])
        else df_se2[[oc]] <- NA_real_
      }
      df_se <- df_se2
    }
    
    # long for coef
    coef_long <- df_coef %>%
      dplyr::select(dplyr::all_of(c("firm_id", out_cols))) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(out_cols),
                          names_to = "outcome", values_to = "x_hat") %>%
      dplyr::mutate(firm_id = as.integer(firm_id),
                    x_hat = as.numeric(x_hat))
    
    # long for se
    se_long <- df_se %>%
      dplyr::select(dplyr::all_of(c("firm_id", out_cols))) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(out_cols),
                          names_to = "outcome", values_to = "x_se") %>%
      dplyr::mutate(firm_id = as.integer(firm_id),
                    x_se = as.numeric(x_se))
    
    long <- coef_long %>%
      dplyr::left_join(se_long, by = c("firm_id","outcome")) %>%
      dplyr::left_join(ind_map, by = "firm_id") %>%
      dplyr::left_join(w_tbl,   by = "firm_id") %>%
      dplyr::filter(!is.na(aer_naics2), !is.na(weights), is.finite(weights)) %>%
      dplyr::mutate(weights = pmax(weights, 0))
    
    # drop firms with missing coef for a given outcome
    long <- long %>% dplyr::filter(is.finite(x_hat))
    
    # compute within-industry normalized weights
    ind_sum <- long %>%
      dplyr::group_by(aer_naics2, outcome) %>%
      dplyr::mutate(wsum = sum(weights, na.rm = TRUE),
                    a = dplyr::if_else(wsum > 0, weights / wsum, NA_real_)) %>%
      dplyr::summarise(
        ind_mean = sum(a * x_hat, na.rm = TRUE),
        ind_se   = sqrt(sum((a^2) * (x_se^2), na.rm = TRUE)),  # propagated SE
        n_firms  = dplyr::n_distinct(firm_id),
        wsum     = max(wsum, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(approach = label, .before = 1)
    
    # wide out (mean + se)
    mean_wide <- ind_sum %>%
      dplyr::select(approach, aer_naics2, outcome, ind_mean) %>%
      tidyr::pivot_wider(names_from = outcome, values_from = ind_mean)
    
    se_wide <- ind_sum %>%
      dplyr::select(approach, aer_naics2, outcome, ind_se) %>%
      tidyr::pivot_wider(names_from = outcome,
                         values_from = ind_se,
                         names_glue = "{outcome}_se")
    
    meta <- ind_sum %>%
      dplyr::select(approach, aer_naics2, outcome, n_firms, wsum) %>%
      dplyr::group_by(approach, aer_naics2) %>%
      dplyr::summarise(
        n_firms = max(n_firms, na.rm = TRUE),
        wsum    = max(wsum, na.rm = TRUE),
        .groups = "drop"
      )
    
    out <- meta %>%
      dplyr::left_join(mean_wide, by = c("approach","aer_naics2")) %>%
      dplyr::left_join(se_wide,   by = c("approach","aer_naics2"))
    
    out
  }
  
  # ---- read sheets ----
  pl_coef <- safe_read(pl_coef_sheet)
  pl_se   <- safe_read(pl_se_sheet)
  
  ol_coef <- safe_read(ol_coef_sheet)
  ol_se   <- safe_read(ol_se_sheet)
  
  borda_coef <- safe_read(borda_coef_sheet)
  borda_se   <- safe_read(borda_se_sheet)
  
  # ---- compute ----
  out_pl    <- summarize_to_industry(pl_coef,    pl_se,    "PL")
  out_ol    <- summarize_to_industry(ol_coef,    ol_se,    "OL")
  out_borda <- summarize_to_industry(borda_coef, borda_se, "Borda")
  
  # ---- write ----
  wb <- openxlsx::loadWorkbook(output_path)
  
  write_sheet <- function(name, df) {
    if (is.null(df)) return(invisible(NULL))
    remove_sheet_safely(wb, name)
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df)
  }
  
  write_sheet(paste0(out_prefix, "_PL"),    out_pl)
  write_sheet(paste0(out_prefix, "_OL"),    out_ol)
  write_sheet(paste0(out_prefix, "_Borda"), out_borda)
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  invisible(list(PL = out_pl, OL = out_ol, Borda = out_borda))
}

################################################################################
# Example call (inside run_analysis_pipeline AFTER EIV weights are created):
################################################################################
# weights <- data %>% dplyr::select(firm_id, njobs) %>%
#   dplyr::rename(weights = njobs) %>% dplyr::distinct()
#
# make_industry_means(
#   output_path   = output_path,
#   industry_map  = industry_map,
#   weights_df    = weights,
#   out_prefix    = "ind_mean_jobs"
# )
################################################################################
