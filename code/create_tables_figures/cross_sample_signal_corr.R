source("code/globals.R")

# -------------------------------------------------------------------
# 0. User inputs
# -------------------------------------------------------------------

# Full-sample PL workbook that contains the experimental outcome "dif"
full_sample_file <- "Plackett_Luce_Full_Sample.xlsx"

# Map subsample label -> workbook file (same files used for PL + Borda)
sample_filemap <- tibble::tibble(
  sample = c("Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination",
             "40 Years or Older",
             "Less than 40 Years Old",
             "At Least Some College",
             "HS Diploma or Less",
             "Convenience",
             "Probability"),
  file   = c("Plackett_Luce_Subset_Black.xlsx",
             "Plackett_Luce_Subset_White.xlsx",
             "Plackett_Luce_Subset_Female.xlsx",
             "Plackett_Luce_Subset_Male.xlsx",
             "Plackett_Luce_Subset_Looking.xlsx",
             "Plackett_Luce_Subset_Not_Looking.xlsx",
             "Plackett_Luce_Subset_Feared_Discrimination_1.xlsx",
             "Plackett_Luce_Subset_Feared_Discrimination_0.xlsx",
             "Plackett_Luce_Subset_Age_gte40.xlsx",
             "Plackett_Luce_Subset_Age_lt40.xlsx",
             "Plackett_Luce_Subset_College.xlsx",
             "Plackett_Luce_Subset_No_College.xlsx",
             "Plackett_Luce_Subset_Convenience.xlsx",
             "Plackett_Luce_Subset_Probability.xlsx")
)

# Pairs of subsamples to correlate / test
sample_pairs <- tibble::tibble(
  sample1 = c("Black",
              "Male",
              "Looking for a Job",
              "Feared Discrimination",
              "40 Years or Older",
              "At Least Some College",
              "Convenience"),
  sample2 = c("White",
              "Female",
              "Not Looking for a Job",
              "Did Not Fear Discrimination",
              "Less than 40 Years Old",
              "HS Diploma or Less",
              "Probability")
)

# Outcomes of interest
outcomes <- c("pooled_favor_white", "pooled_favor_male")

# -------------------------------------------------------------------
# 0a. Extract experimental firm IDs from full-sample "dif" column
# -------------------------------------------------------------------

exp_coef_path <- file.path(excel, full_sample_file)

exp_firm_ids <- readxl::read_xlsx(exp_coef_path, sheet = "Coefficients") %>%
  dplyr::filter(!is.na(.data$dif)) %>%   # firms with experimental data
  dplyr::pull(firm_id) %>%
  unique() %>%
  sort()

cat("Identified", length(exp_firm_ids),
    "experimental firms from 'dif' in Coefficients sheet.\n")

# -------------------------------------------------------------------
# 1. Helper: read theta (no SEs needed now)
# -------------------------------------------------------------------
read_theta <- function(excel, file, outcome, model = c("pl", "borda")) {
  model <- match.arg(model)
  path  <- file.path(excel, file)
  
  if (model == "pl") {
    coef_sheet <- "Coefficients"
    coef_col   <- outcome
  } else {
    coef_sheet <- "borda_score"
    coef_col   <- outcome
  }
  
  readxl::read_xlsx(path, sheet = coef_sheet) %>%
    dplyr::select(firm_id, !!sym(coef_col)) %>%
    dplyr::rename(theta = !!sym(coef_col))
}

# -------------------------------------------------------------------
# 2. Helper: read signal & tot_var from pl_s_/b_s_ sheets
# -------------------------------------------------------------------
read_signal_info <- function(excel, file, outcome, model = c("pl", "borda")) {
  model <- match.arg(model)
  path  <- file.path(excel, file)
  
  if (model == "pl") {
    sheet_name <- paste0("pl_s_", outcome)
  } else {
    sheet_name <- paste0("b_s_", outcome)
  }
  
  sig_df <- readxl::read_xlsx(path, sheet = sheet_name)
  
  # we need iter 0 row, both all_firms TRUE and FALSE
  sig_df %>%
    dplyr::filter(iter == 0) %>%
    dplyr::select(all_firms, tot_var, sigma2_dot) %>%
    dplyr::rename(signal = sigma2_dot)
}

# -------------------------------------------------------------------
# 3. Helpers for likelihood ratio tests
# -------------------------------------------------------------------

# read one row (Outcome == outcome) from likelihood_ratio sheet
read_lr_row <- function(excel, file, outcome) {
  path <- file.path(excel, file)
  df <- readxl::read_xlsx(path, sheet = "likelihood_ratio")
  df <- df %>% dplyr::filter(.data$Outcome == outcome)
  if (nrow(df) == 0) return(NULL)
  df[1, c("Outcome", "logLik_mle", "n_par_mle")]
}

# compute LR stat, df, and p-value comparing:
#  H0: pooled (full sample) vs H1: separate models in the two subsamples
compute_lr_for_pair <- function(excel,
                                full_sample_file,
                                sample_filemap,
                                outcome,
                                sample1,
                                sample2) {
  # Get filenames
  f1 <- sample_filemap$file[sample_filemap$sample == sample1]
  f2 <- sample_filemap$file[sample_filemap$sample == sample2]
  if (length(f1) != 1L || length(f2) != 1L) {
    warning("LR: file not found for sample(s): ", sample1, " or ", sample2)
    return(list(LR_stat = NA_real_, LR_df = NA_integer_, LR_pval = NA_real_))
  }
  
  full_row <- read_lr_row(excel, full_sample_file, outcome)
  row1     <- read_lr_row(excel, f1, outcome)
  row2     <- read_lr_row(excel, f2, outcome)
  
  if (is.null(full_row) || is.null(row1) || is.null(row2)) {
    warning("LR rows missing for outcome ", outcome,
            " (", sample1, " vs ", sample2, ")")
    return(list(LR_stat = NA_real_, LR_df = NA_integer_, LR_pval = NA_real_))
  }
  
  ll_full <- as.numeric(full_row$logLik_mle)
  k_full  <- as.numeric(full_row$n_par_mle)
  
  ll1 <- as.numeric(row1$logLik_mle)
  k1  <- as.numeric(row1$n_par_mle)
  
  ll2 <- as.numeric(row2$logLik_mle)
  k2  <- as.numeric(row2$n_par_mle)
  
  LR_stat <- 2 * ((ll1 + ll2) - ll_full)
  df      <- (k1 + k2 - k_full)
  
  if (!is.finite(LR_stat) || df <= 0) {
    LR_pval <- NA_real_
  } else {
    LR_pval <- 1 - stats::pchisq(LR_stat, df = df)
  }
  
  list(LR_stat = LR_stat, LR_df = df, LR_pval = LR_pval)
}

# -------------------------------------------------------------------
# 4. Helper: compute covariance-based correlation using signals
# -------------------------------------------------------------------
compute_corr_row <- function(theta1, theta2,
                             signal1_df, signal2_df,
                             outcome, model, sample1, sample2,
                             all_firms_flag = TRUE,
                             exp_firm_ids = NULL) {
  
  # common firms
  common_ids <- intersect(theta1$firm_id, theta2$firm_id)
  if (!all_firms_flag && !is.null(exp_firm_ids)) {
    common_ids <- intersect(common_ids, exp_firm_ids)
  }
  common_ids <- sort(common_ids)
  J <- length(common_ids)
  if (J < 2L) {
    return(NULL)
  }
  
  idx1 <- match(common_ids, theta1$firm_id)
  idx2 <- match(common_ids, theta2$firm_id)
  
  t1  <- theta1$theta[idx1]
  t2  <- theta2$theta[idx2]
  
  # center
  t1c <- t1 - mean(t1, na.rm = TRUE)
  t2c <- t2 - mean(t2, na.rm = TRUE)
  
  covariance <- mean(t1c * t2c, na.rm = TRUE)
  
  # pick matching signal rows for all_firms_flag
  sig1_row <- signal1_df %>% dplyr::filter(all_firms == all_firms_flag)
  sig2_row <- signal2_df %>% dplyr::filter(all_firms == all_firms_flag)
  
  if (nrow(sig1_row) != 1L || nrow(sig2_row) != 1L) {
    warning("Signal rows not found uniquely for ", sample1, " vs ", sample2,
            ", outcome ", outcome, ", model ", model,
            ", all_firms_flag=", all_firms_flag)
    return(NULL)
  }
  
  tot_var1 <- sig1_row$tot_var[1]
  tot_var2 <- sig2_row$tot_var[1]
  signal1  <- sig1_row$signal[1]
  signal2  <- sig2_row$signal[1]
  
  denom <- sqrt(signal1 * signal2)
  corr_c <- if (denom > 0) covariance / denom else NA_real_
  
  tibble::tibble(
    model      = model,
    outcome    = outcome,
    sample1    = sample1,
    sample2    = sample2,
    J_firms    = J,
    covariance = covariance,
    tot_var1   = tot_var1,
    tot_var2   = tot_var2,
    signal1    = signal1,
    signal2    = signal2,
    all_firms  = all_firms_flag,
    corr_c     = corr_c
  )
}

# -------------------------------------------------------------------
# 5. Main loop: build tables for PL and Borda (with LR for PL)
# -------------------------------------------------------------------
build_corr_table <- function(model = c("pl", "borda"),
                             excel,
                             sample_filemap,
                             sample_pairs,
                             outcomes,
                             exp_firm_ids = NULL,
                             full_sample_file) {
  model <- match.arg(model)
  
  rows <- list()
  row_id <- 1L
  
  for (outcome in outcomes) {
    for (i in seq_len(nrow(sample_pairs))) {
      s1 <- sample_pairs$sample1[i]
      s2 <- sample_pairs$sample2[i]
      
      f1 <- sample_filemap$file[sample_filemap$sample == s1]
      f2 <- sample_filemap$file[sample_filemap$sample == s2]
      if (length(f1) != 1L || length(f2) != 1L) {
        warning("File not found for sample(s): ", s1, " or ", s2)
        next
      }
      
      theta1 <- read_theta(excel, f1, outcome, model = model)
      theta2 <- read_theta(excel, f2, outcome, model = model)
      
      sig1   <- read_signal_info(excel, f1, outcome, model = model)
      sig2   <- read_signal_info(excel, f2, outcome, model = model)
      
      # --- all firms ---
      row_all <- compute_corr_row(
        theta1, theta2,
        signal1_df = sig1, signal2_df = sig2,
        outcome = outcome,
        model   = model,
        sample1 = s1, sample2 = s2,
        all_firms_flag = TRUE,
        exp_firm_ids   = exp_firm_ids
      )
      if (!is.null(row_all)) {
        # attach LR only for PL (and only once per pair/outcome)
        if (model == "pl") {
          lr_res <- compute_lr_for_pair(
            excel         = excel,
            full_sample_file = full_sample_file,
            sample_filemap   = sample_filemap,
            outcome          = outcome,
            sample1          = s1,
            sample2          = s2
          )
          row_all$LR_stat <- lr_res$LR_stat
          row_all$LR_df   <- lr_res$LR_df
          row_all$LR_pval <- lr_res$LR_pval
        } else {
          row_all$LR_stat <- NA_real_
          row_all$LR_df   <- NA_integer_
          row_all$LR_pval <- NA_real_
        }
        
        rows[[row_id]] <- row_all
        row_id <- row_id + 1L
      }
      
      # --- restricted to experimental firms (corr only; LR same as above or NA) ---
      if (!is.null(exp_firm_ids)) {
        row_97 <- compute_corr_row(
          theta1, theta2,
          signal1_df = sig1, signal2_df = sig2,
          outcome = outcome,
          model   = model,
          sample1 = s1, sample2 = s2,
          all_firms_flag = FALSE,
          exp_firm_ids   = exp_firm_ids
        )
        if (!is.null(row_97)) {
          # LR is defined on full sample, so leave NA here
          row_97$LR_stat <- NA_real_
          row_97$LR_df   <- NA_integer_
          row_97$LR_pval <- NA_real_
          
          rows[[row_id]] <- row_97
          row_id <- row_id + 1L
        }
      }
    }
  }
  
  dplyr::bind_rows(rows)
}

# -------------------------------------------------------------------
# 6. Run and write to Excel
# -------------------------------------------------------------------

pl_corr <- build_corr_table("pl",    excel, sample_filemap, sample_pairs,
                            outcomes, exp_firm_ids, full_sample_file)
borda_corr <- build_corr_table("borda", excel, sample_filemap, sample_pairs,
                               outcomes, exp_firm_ids, full_sample_file)

out_path <- file.path(excel, "subset_correlations_signal_based_with_LR.xlsx")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "PL")
openxlsx::writeData(wb, "PL", pl_corr)
openxlsx::addWorksheet(wb, "Borda")
openxlsx::writeData(wb, "Borda", borda_corr)
openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

cat("Saved correlation + LR workbook to:", out_path, "\n")

# -------------------------------------------------------------------
# 7. Build LaTeX table (using all_firms == TRUE and p-values from PL only)
# -------------------------------------------------------------------

dir.create("tables", showWarnings = FALSE, recursive = TRUE)

fmt_corr <- function(x) {
  if (is.na(x)) return("")
  formatC(x, digits = 3, format = "f")
}

# p-value formatter: "$<0.001$" if < 0.001, else 3 decimals
fmt_pval <- function(x) {
  if (is.na(x)) return("")
  if (!is.finite(x)) return("")
  if (x < 0.001) {
    "$<0.001$"
  } else {
    formatC(x, digits = 3, format = "f")
  }
}

# Row labels and pairs (must match sample_filemap$sample)
row_specs <- tibble::tribble(
  ~row_label,                        ~s1,                      ~s2,
  "Black vs White",                  "Black",                  "White",
  "Female vs Male",                  "Female",                 "Male",
  "Looking for a Job vs Not",        "Looking for a Job",      "Not Looking for a Job",
  "Feared Discrimination vs Not",    "Feared Discrimination",  "Did Not Fear Discrimination",
  "Age $>=$ 40 vs $<$ 40",               "40 Years or Older",      "Less than 40 Years Old",
  "At Least Some College vs HS Diploma or less", "At Least Some College", "HS Diploma or Less",
  "Convenience vs Probability",      "Convenience",            "Probability"
)

# Outcomes and their display labels
outcome_map <- tibble::tribble(
  ~outcome,             ~col_label,
  "pooled_favor_white", "Discrimination Black",
  "pooled_favor_male",  "Discrimination Female"
)

# Extract a corr + p-value matrix for one model/panel
# df_corr: correlation data for that model (PL or Borda)
# df_lr:   data frame where LR p-values live (we'll use pl_corr for p-values)
# with_pvalues: TRUE for PL panel, FALSE for Borda panel
get_panel_matrix <- function(df_corr,
                             df_lr,
                             all_firms_flag = TRUE,
                             with_pvalues = TRUE) {
  
  out_list <- list()
  
  for (i in seq_len(nrow(row_specs))) {
    rlab <- row_specs$row_label[i]
    s1   <- row_specs$s1[i]
    s2   <- row_specs$s2[i]
    
    row_vals <- c(Row = rlab)
    
    for (j in seq_len(nrow(outcome_map))) {
      oc       <- outcome_map$outcome[j]
      col_lab  <- outcome_map$col_label[j]
      p_lab    <- paste0("p (", col_lab, ")")
      
      tmp_corr <- df_corr %>%
        dplyr::filter(
          outcome   == oc,
          all_firms == all_firms_flag,
          (sample1 == s1 & sample2 == s2) |
            (sample1 == s2 & sample2 == s1)
        )
      
      corr_val <- if (nrow(tmp_corr) == 0) NA_real_ else tmp_corr$corr_c[1]
      row_vals[col_lab] <- fmt_corr(corr_val)
      
      if (with_pvalues) {
        tmp_lr <- df_lr %>%
          dplyr::filter(
            outcome   == oc,
            all_firms == all_firms_flag,
            (sample1 == s1 & sample2 == s2) |
              (sample1 == s2 & sample2 == s1)
          )
        p_val <- if (nrow(tmp_lr) == 0) NA_real_ else tmp_lr$LR_pval[1]
        row_vals[p_lab] <- fmt_pval(p_val)
      } else {
        # leave p-value blank for Borda
        row_vals[p_lab] <- ""
      }
    }
    
    out_list[[i]] <- row_vals
  }
  
  panel_df <- as.data.frame(do.call(rbind, out_list), stringsAsFactors = FALSE)
  rownames(panel_df) <- NULL
  panel_df
}

# Use all_firms == TRUE in the LaTeX table
panelA <- get_panel_matrix(pl_corr,    pl_corr,    all_firms_flag = TRUE, with_pvalues = TRUE)
panelB <- get_panel_matrix(borda_corr, pl_corr,    all_firms_flag = TRUE, with_pvalues = FALSE)

# LaTeX writer ---------------------------------------------------------
latex_lines <- c(
  "  \\centering",
  "  \\begin{tabular}{lcccc}",
  "    \\toprule",
  "    & \\multicolumn{2}{c}{Discrimination Black} & \\multicolumn{2}{c}{Discrimination Female} \\\\",
  "    & Corr & p-value & Corr & p-value \\\\",
  "    \\midrule",
  "    \\multicolumn{5}{l}{\\textbf{Panel A: Plackett--Luce}}\\\\",
  paste0("    ", panelA$Row, " & ",
         panelA$`Discrimination Black`, " & ",
         panelA$`p (Discrimination Black)`, " & ",
         panelA$`Discrimination Female`, " & ",
         panelA$`p (Discrimination Female)`, " \\\\"),
  "    \\addlinespace",
  "    \\multicolumn{5}{l}{\\textbf{Panel B: Borda}}\\\\",
  paste0("    ", panelB$Row, " & ",
         panelB$`Discrimination Black`, " & ",
         panelB$`p (Discrimination Black)`, " & ",
         panelB$`Discrimination Female`, " & ",
         panelB$`p (Discrimination Female)`, " \\\\"),
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "pl_borda_corr_with_LR_pvals.tex")
writeLines(latex_lines, out_tex)
message("LaTeX table written to: ", out_tex)
