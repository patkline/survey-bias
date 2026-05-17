# -------------------------------------------------------------------
# Purpose: Placebo test for cross-sample signal correlations.
#          Identical to cross_sample_signal_corr.R except that firm
#          IDs in the second sample are randomly permuted before
#          matching, breaking the true firm link.  Correlations
#          should be near zero if the real correlations are genuine.
# -------------------------------------------------------------------

source("code/globals.R")

set.seed(42)

# -------------------------------------------------------------------
# 0. User inputs  (mirrored from cross_sample_signal_corr.R)
# -------------------------------------------------------------------

# Resolve firm identifier column across old/new pipeline formats
get_firm_id_col <- function(df) {
  if ("firm_id" %in% names(df)) return("firm_id")
  if ("entity_id" %in% names(df)) return("entity_id")
  stop("Could not find firm identifier column (expected `firm_id` or `entity_id`).")
}

full_sample_subdir <- "Full_Sample"

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
             "Probability",
             "Not Confident (Gender)",
             "Confident (Gender)",
             "Not Confident (Race)",
             "Confident (Race)"),
  subdir = c("Subset_Black",
             "Subset_White",
             "Subset_Female",
             "Subset_Male",
             "Subset_Looking",
             "Subset_Not_Looking",
             "Subset_Feared_Discrimination_1",
             "Subset_Feared_Discrimination_0",
             "Subset_Age_gte40",
             "Subset_Age_lt40",
             "Subset_College",
             "Subset_No_College",
             "Subset_Convenience",
             "Subset_Probability",
             "Subset_Conf_Gender_N",
             "Subset_Conf_Gender_Y",
             "Subset_Conf_Race_N",
             "Subset_Conf_Race_Y")
)

sample_pairs <- tibble::tibble(
  sample1 = c("Black",
              "Male",
              "Looking for a Job",
              "Feared Discrimination",
              "40 Years or Older",
              "At Least Some College",
              "Convenience",
              "Not Confident (Gender)",
              "Not Confident (Race)"),
  sample2 = c("White",
              "Female",
              "Not Looking for a Job",
              "Did Not Fear Discrimination",
              "Less than 40 Years Old",
              "HS Diploma or Less",
              "Probability",
              "Confident (Gender)",
              "Confident (Race)")
)

outcomes <- c("pooled_favor_white", "pooled_favor_male")

# -------------------------------------------------------------------
# 0a. Extract experimental firm IDs (same as real script)
# -------------------------------------------------------------------

exp_coef_dir <- file.path(intermediate, full_sample_subdir)
exp_coef_raw <- read_parquet_sheet(exp_coef_dir, "Coefficients")
id_col <- get_firm_id_col(exp_coef_raw)

if ("model" %in% names(exp_coef_raw)) {
  exp_coef_raw <- exp_coef_raw %>%
    dplyr::filter(.data$model == dplyr::first(unique(.data$model)))
}
if ("entity_type" %in% names(exp_coef_raw)) {
  exp_coef_raw <- exp_coef_raw %>%
    dplyr::filter(tolower(as.character(.data$entity_type)) == "firm")
}
if ("subset" %in% names(exp_coef_raw)) {
  exp_coef_raw <- exp_coef_raw %>% dplyr::filter(.data$subset == "all")
}

if (all(c("outcome", "estimate") %in% names(exp_coef_raw))) {
  if (any(exp_coef_raw$outcome == "dif")) {
    exp_firm_ids <- exp_coef_raw %>%
      dplyr::filter(.data$outcome == "dif", !is.na(.data$estimate)) %>%
      dplyr::pull(!!rlang::sym(id_col)) %>%
      unique() %>% sort()
  } else {
    exp_firm_ids <- exp_coef_raw %>%
      dplyr::pull(!!rlang::sym(id_col)) %>%
      unique() %>% sort()
  }
} else if ("dif" %in% names(exp_coef_raw)) {
  exp_firm_ids <- exp_coef_raw %>%
    dplyr::filter(!is.na(.data$dif)) %>%
    dplyr::pull(!!rlang::sym(id_col)) %>%
    unique() %>% sort()
} else {
  exp_firm_ids <- exp_coef_raw %>%
    dplyr::pull(!!rlang::sym(id_col)) %>%
    unique() %>% sort()
}

if (length(exp_firm_ids) == 0L) {
  exp_firm_ids <- exp_coef_raw %>%
    dplyr::pull(!!rlang::sym(id_col)) %>%
    unique() %>% sort()
}

cat("Identified", length(exp_firm_ids),
    "experimental firms from Coefficients sheet.\n")

# -------------------------------------------------------------------
# 1. Helper: read theta (identical to real script)
# -------------------------------------------------------------------
read_theta <- function(root, subdir, outcome,
                       model = c("pl", "borda", "ol", "ols", "olsc")) {
  model <- match.arg(model)
  dir_path <- file.path(root, subdir)

  model_map <- list(
    pl    = list(filter = "PL",    old_sheet = "Coefficients"),
    borda = list(filter = "Borda", old_sheet = "borda_score"),
    ol    = list(filter = "OL",    old_sheet = "Coefficients"),
    ols   = list(filter = "OLS",   old_sheet = "Coefficients"),
    olsc  = list(filter = "OLSC",  old_sheet = "Coefficients")
  )

  coef_sheet <- "Coefficients"
  df <- read_parquet_sheet(dir_path, coef_sheet)

  if ("model" %in% names(df)) {
    model_val <- model_map[[model]]$filter
    df <- df %>% dplyr::filter(.data$model == model_val)
  } else if (model == "borda") {
    df <- read_parquet_sheet(dir_path, "borda_score")
  }

  id_col <- get_firm_id_col(df)

  if (all(c("outcome", "estimate") %in% names(df))) {
    if ("entity_type" %in% names(df)) {
      df <- df %>%
        dplyr::filter(tolower(as.character(.data$entity_type)) == "firm")
    }
    if ("subset" %in% names(df)) {
      df <- df %>% dplyr::filter(.data$subset == "all")
    }

    out <- df %>%
      dplyr::filter(.data$outcome == !!outcome) %>%
      dplyr::select(!!rlang::sym(id_col), estimate) %>%
      dplyr::rename(
        firm_id = !!rlang::sym(id_col),
        theta   = estimate
      )
  } else {
    out <- df %>%
      dplyr::select(!!rlang::sym(id_col), !!rlang::sym(outcome)) %>%
      dplyr::rename(
        firm_id = !!rlang::sym(id_col),
        theta   = !!rlang::sym(outcome)
      )
  }

  out %>%
    dplyr::mutate(
      firm_id = suppressWarnings(as.numeric(.data$firm_id)),
      theta   = suppressWarnings(as.numeric(.data$theta))
    ) %>%
    dplyr::filter(!is.na(.data$firm_id)) %>%
    dplyr::distinct(.data$firm_id, .keep_all = TRUE)
}

# -------------------------------------------------------------------
# 2. Helper: read signal & tot_var (identical to real script)
# -------------------------------------------------------------------
read_signal_info <- function(root, subdir, outcome,
                             model = c("pl", "borda", "ol", "ols", "olsc")) {
  model <- match.arg(model)
  dir_path <- file.path(root, subdir)

  model_filter_map <- c(pl = "PL", borda = "Borda", ol = "OL", ols = "OLS", olsc = "OLSC")

  var_df <- read_parquet_sheet(dir_path, "variance")

  model_val <- model_filter_map[model]
  var_df %>%
    dplyr::filter(
      .data$model == model_val,
      .data$outcome == !!outcome
    ) %>%
    dplyr::transmute(
      all_firms = (.data$subset == "all"),
      tot_var   = as.numeric(.data$variance),
      signal    = as.numeric(.data$signal)
    )
}

# -------------------------------------------------------------------
# 3. Placebo version of compute_corr_row
#    Permutes firm_id in theta2 before matching
# -------------------------------------------------------------------
compute_corr_row_placebo <- function(theta1, theta2,
                                     signal1_df, signal2_df,
                                     outcome, model, sample1, sample2,
                                     all_firms_flag = TRUE,
                                     exp_firm_ids = NULL) {

  # Permute firm IDs in the second sample
  theta2_placebo <- theta2
  theta2_placebo$firm_id <- sample(theta2_placebo$firm_id)

  # Common firms after permutation
  common_ids <- intersect(theta1$firm_id, theta2_placebo$firm_id)
  if (!all_firms_flag && !is.null(exp_firm_ids)) {
    common_ids <- intersect(common_ids, exp_firm_ids)
  }
  common_ids <- sort(common_ids)
  J <- length(common_ids)
  if (J < 2L) {
    return(NULL)
  }

  idx1 <- match(common_ids, theta1$firm_id)
  idx2 <- match(common_ids, theta2_placebo$firm_id)

  t1 <- theta1$theta[idx1]
  t2 <- theta2_placebo$theta[idx2]

  t1c <- t1 - mean(t1, na.rm = TRUE)
  t2c <- t2 - mean(t2, na.rm = TRUE)

  covariance <- mean(t1c * t2c, na.rm = TRUE)

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
  corr_c <- if (is.finite(denom) && denom > 0) covariance / denom else NA_real_

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
# 4. Build placebo correlation table (no LR tests needed)
# -------------------------------------------------------------------
build_corr_table_placebo <- function(model = c("pl", "borda", "ol", "ols", "olsc"),
                                     root,
                                     sample_filemap,
                                     sample_pairs,
                                     outcomes,
                                     exp_firm_ids = NULL) {
  model <- match.arg(model)

  rows <- list()
  row_id <- 1L

  for (outcome in outcomes) {
    for (i in seq_len(nrow(sample_pairs))) {
      s1 <- sample_pairs$sample1[i]
      s2 <- sample_pairs$sample2[i]

      d1 <- sample_filemap$subdir[sample_filemap$sample == s1]
      d2 <- sample_filemap$subdir[sample_filemap$sample == s2]
      if (length(d1) != 1L || length(d2) != 1L) {
        warning("Directory not found for sample(s): ", s1, " or ", s2)
        next
      }

      theta1 <- read_theta(root, d1, outcome, model = model)
      theta2 <- read_theta(root, d2, outcome, model = model)

      sig1 <- read_signal_info(root, d1, outcome, model = model)
      sig2 <- read_signal_info(root, d2, outcome, model = model)

      # --- all firms ---
      row_all <- compute_corr_row_placebo(
        theta1, theta2,
        signal1_df = sig1, signal2_df = sig2,
        outcome = outcome,
        model   = model,
        sample1 = s1, sample2 = s2,
        all_firms_flag = TRUE,
        exp_firm_ids   = exp_firm_ids
      )
      if (!is.null(row_all)) {
        rows[[row_id]] <- row_all
        row_id <- row_id + 1L
      }

      # --- restricted to experimental firms ---
      if (!is.null(exp_firm_ids)) {
        row_97 <- compute_corr_row_placebo(
          theta1, theta2,
          signal1_df = sig1, signal2_df = sig2,
          outcome = outcome,
          model   = model,
          sample1 = s1, sample2 = s2,
          all_firms_flag = FALSE,
          exp_firm_ids   = exp_firm_ids
        )
        if (!is.null(row_97)) {
          rows[[row_id]] <- row_97
          row_id <- row_id + 1L
        }
      }
    }
  }

  dplyr::bind_rows(rows)
}

# -------------------------------------------------------------------
# 5. Run placebo for all models
# -------------------------------------------------------------------

pl_corr    <- build_corr_table_placebo("pl",    intermediate, sample_filemap, sample_pairs,
                                       outcomes, exp_firm_ids)
borda_corr <- build_corr_table_placebo("borda", intermediate, sample_filemap, sample_pairs,
                                       outcomes, exp_firm_ids)
ol_corr    <- build_corr_table_placebo("ol",    intermediate, sample_filemap, sample_pairs,
                                       outcomes, exp_firm_ids)
ols_corr   <- build_corr_table_placebo("ols",   intermediate, sample_filemap, sample_pairs,
                                       outcomes, exp_firm_ids)
olsc_corr  <- build_corr_table_placebo("olsc",  intermediate, sample_filemap, sample_pairs,
                                       outcomes, exp_firm_ids)

# -------------------------------------------------------------------
# 6. Build LaTeX table (same structure as real, no p-value columns)
# -------------------------------------------------------------------

fmt_corr <- function(x) {
  if (is.na(x)) return("")
  formatC(x, digits = 3, format = "f")
}

row_specs <- tibble::tribble(
  ~row_label,                        ~s1,                      ~s2,
  "Black vs White",                  "Black",                  "White",
  "Female vs Male",                  "Female",                 "Male",
  "Looking for a Job vs Not",        "Looking for a Job",      "Not Looking for a Job",
  "Feared Discrimination vs Not",    "Feared Discrimination",  "Did Not Fear Discrimination",
  "Age $>=$ 40 vs $<$ 40",          "40 Years or Older",      "Less than 40 Years Old",
  "At Least Some College vs HS Diploma or less", "At Least Some College", "HS Diploma or Less",
  "Convenience vs Probability",      "Convenience",            "Probability",
  "Confident vs Not (Gender)",       "Not Confident (Gender)", "Confident (Gender)",
  "Confident vs Not (Race)",         "Not Confident (Race)",   "Confident (Race)"
)

outcome_map <- tibble::tribble(
  ~outcome,             ~col_label,
  "pooled_favor_white", "Discrimination Black",
  "pooled_favor_male",  "Discrimination Female"
)

get_panel_matrix_placebo <- function(df_corr, all_firms_flag = TRUE) {

  out_list <- list()

  for (i in seq_len(nrow(row_specs))) {
    rlab <- row_specs$row_label[i]
    s1   <- row_specs$s1[i]
    s2   <- row_specs$s2[i]

    row_vals <- c(Row = rlab)

    for (j in seq_len(nrow(outcome_map))) {
      oc      <- outcome_map$outcome[j]
      col_lab <- outcome_map$col_label[j]

      tmp_corr <- df_corr %>%
        dplyr::filter(
          outcome   == oc,
          all_firms == all_firms_flag,
          (sample1 == s1 & sample2 == s2) |
            (sample1 == s2 & sample2 == s1)
        )

      corr_val <- if (nrow(tmp_corr) == 0) NA_real_ else tmp_corr$corr_c[1]
      row_vals[col_lab] <- fmt_corr(corr_val)
    }

    out_list[[i]] <- row_vals
  }

  panel_df <- as.data.frame(do.call(rbind, out_list), stringsAsFactors = FALSE)
  rownames(panel_df) <- NULL
  panel_df
}

panelA <- get_panel_matrix_placebo(pl_corr,    all_firms_flag = TRUE)
panelB <- get_panel_matrix_placebo(borda_corr, all_firms_flag = TRUE)
panelC <- get_panel_matrix_placebo(ol_corr,    all_firms_flag = TRUE)
panelD <- get_panel_matrix_placebo(ols_corr,   all_firms_flag = TRUE)
panelE <- get_panel_matrix_placebo(olsc_corr,  all_firms_flag = TRUE)

panel_rows_corr_only <- function(panel) {
  paste0("    ", panel$Row, " & ",
         panel$`Discrimination Black`, " & ",
         panel$`Discrimination Female`, " \\\\")
}

latex_lines <- c(
  "  \\centering",
  "  \\begin{tabular}{lcc}",
  "    \\toprule",
  "    & Discrimination Black & Discrimination Female \\\\",
  "    \\midrule",
  "    \\multicolumn{3}{l}{\\textbf{Panel A: Plackett--Luce (Placebo)}}\\\\",
  panel_rows_corr_only(panelA),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel B: Borda (Placebo)}}\\\\",
  panel_rows_corr_only(panelB),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel C: Ordered Logit (Placebo)}}\\\\",
  panel_rows_corr_only(panelC),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel D: Likert (Placebo)}}\\\\",
  panel_rows_corr_only(panelD),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel E: Likert Centered (Placebo)}}\\\\",
  panel_rows_corr_only(panelE),
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "cross_sample_corr_placebo.tex")
writeLines(latex_lines, out_tex)
message("Placebo LaTeX table written to: ", out_tex)

# -------------------------------------------------------------------
# 7. Generate OLS + Borda version (corr-only; no p-value columns)
# -------------------------------------------------------------------

panelA_ols_borda <- panelD
panelB_ols_borda <- panelB

latex_lines_ols_borda <- c(
  "  \\centering",
  "  \\begin{tabular}{lcc}",
  "    \\toprule",
  "    & Discrimination Black & Discrimination Female \\\\",
  "    \\midrule",
  "    \\multicolumn{3}{l}{\\textbf{Panel A: Likert (Placebo)}}\\\\",
  panel_rows_corr_only(panelA_ols_borda),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel B: Borda (Placebo)}}\\\\",
  panel_rows_corr_only(panelB_ols_borda),
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex_ols_borda <- file.path(tables, "cross_sample_corr_placebo_ols_borda.tex")
writeLines(latex_lines_ols_borda, out_tex_ols_borda)
message("Placebo OLS+Borda LaTeX table written to: ", out_tex_ols_borda)
