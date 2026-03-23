# -------------------------------------------------------------------
# Purpose: Cross-sample correlations using raw (naive) correlations
#          instead of debiased correlations. Same structure as
#          cross_sample_signal_corr.R but uses cor() on matched
#          firm estimates directly, without signal correction.
# -------------------------------------------------------------------

source("code/globals.R")

# -------------------------------------------------------------------
# 0. User inputs  (mirrored from cross_sample_signal_corr.R)
# -------------------------------------------------------------------

get_firm_id_col <- function(df) {
  if ("firm_id" %in% names(df)) return("firm_id")
  if ("entity_id" %in% names(df)) return("entity_id")
  stop("Could not find firm identifier column (expected `firm_id` or `entity_id`).")
}

full_sample_file <- "Plackett_Luce_Full_Sample.xlsx"

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

outcomes <- c("pooled_favor_white", "pooled_favor_male")

# -------------------------------------------------------------------
# 1. Helper: read theta (identical to real script)
# -------------------------------------------------------------------
read_theta <- function(excel, file, outcome,
                       model = c("pl", "borda", "ol", "ols", "olsc")) {
  model <- match.arg(model)
  path  <- file.path(excel, file)

  model_map <- list(
    pl    = list(filter = "PL",    old_sheet = "Coefficients"),
    borda = list(filter = "Borda", old_sheet = "borda_score"),
    ol    = list(filter = "OL",    old_sheet = "Coefficients"),
    ols   = list(filter = "OLS",   old_sheet = "Coefficients"),
    olsc  = list(filter = "OLSC",  old_sheet = "Coefficients")
  )

  coef_sheet <- "Coefficients"
  df <- readxl::read_xlsx(path, sheet = coef_sheet)

  if ("model" %in% names(df)) {
    model_val <- model_map[[model]]$filter
    df <- df %>% dplyr::filter(.data$model == model_val)
  } else if (model == "borda") {
    df <- readxl::read_xlsx(path, sheet = "borda_score")
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
# 2. Compute raw correlation on matched firms
# -------------------------------------------------------------------
compute_corr_row_raw <- function(theta1, theta2,
                                 outcome, model, sample1, sample2) {

  common_ids <- intersect(theta1$firm_id, theta2$firm_id)
  common_ids <- sort(common_ids)
  J <- length(common_ids)
  if (J < 2L) return(NULL)

  idx1 <- match(common_ids, theta1$firm_id)
  idx2 <- match(common_ids, theta2$firm_id)

  t1 <- theta1$theta[idx1]
  t2 <- theta2$theta[idx2]

  raw_corr <- cor(t1, t2, use = "complete.obs")

  tibble::tibble(
    model    = model,
    outcome  = outcome,
    sample1  = sample1,
    sample2  = sample2,
    J_firms  = J,
    raw_corr = raw_corr
  )
}

# -------------------------------------------------------------------
# 3. Build raw correlation table
# -------------------------------------------------------------------
build_corr_table_raw <- function(model = c("pl", "borda", "ol", "ols", "olsc"),
                                 excel,
                                 sample_filemap,
                                 sample_pairs,
                                 outcomes) {
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

      row_out <- compute_corr_row_raw(
        theta1, theta2,
        outcome = outcome,
        model   = model,
        sample1 = s1, sample2 = s2
      )
      if (!is.null(row_out)) {
        rows[[row_id]] <- row_out
        row_id <- row_id + 1L
      }
    }
  }

  dplyr::bind_rows(rows)
}

# -------------------------------------------------------------------
# 4. Run for all models
# -------------------------------------------------------------------

pl_corr    <- build_corr_table_raw("pl",    excel, sample_filemap, sample_pairs, outcomes)
borda_corr <- build_corr_table_raw("borda", excel, sample_filemap, sample_pairs, outcomes)
ol_corr    <- build_corr_table_raw("ol",    excel, sample_filemap, sample_pairs, outcomes)
ols_corr   <- build_corr_table_raw("ols",   excel, sample_filemap, sample_pairs, outcomes)
olsc_corr  <- build_corr_table_raw("olsc",  excel, sample_filemap, sample_pairs, outcomes)

# -------------------------------------------------------------------
# 5. Build LaTeX table
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
  "Convenience vs Probability",      "Convenience",            "Probability"
)

outcome_map <- tibble::tribble(
  ~outcome,             ~col_label,
  "pooled_favor_white", "Discrimination Black",
  "pooled_favor_male",  "Discrimination Female"
)

get_panel_matrix_raw <- function(df_corr) {

  out_list <- list()

  for (i in seq_len(nrow(row_specs))) {
    rlab <- row_specs$row_label[i]
    s1   <- row_specs$s1[i]
    s2   <- row_specs$s2[i]

    row_vals <- c(Row = rlab)

    for (j in seq_len(nrow(outcome_map))) {
      oc      <- outcome_map$outcome[j]
      col_lab <- outcome_map$col_label[j]

      tmp <- df_corr %>%
        dplyr::filter(
          outcome == oc,
          (sample1 == s1 & sample2 == s2) |
            (sample1 == s2 & sample2 == s1)
        )

      corr_val <- if (nrow(tmp) == 0) NA_real_ else tmp$raw_corr[1]
      row_vals[col_lab] <- fmt_corr(corr_val)
    }

    out_list[[i]] <- row_vals
  }

  panel_df <- as.data.frame(do.call(rbind, out_list), stringsAsFactors = FALSE)
  rownames(panel_df) <- NULL
  panel_df
}

panelA <- get_panel_matrix_raw(pl_corr)
panelB <- get_panel_matrix_raw(borda_corr)
panelC <- get_panel_matrix_raw(ol_corr)
panelD <- get_panel_matrix_raw(ols_corr)
panelE <- get_panel_matrix_raw(olsc_corr)

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
  "    \\multicolumn{3}{l}{\\textbf{Panel A: Plackett--Luce (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panelA),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel B: Borda (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panelB),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel C: Ordered Logit (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panelC),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel D: Likert Score (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panelD),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel E: Likert Score Centered (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panelE),
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "cross_sample_corr_raw.tex")
writeLines(latex_lines, out_tex)
message("Raw correlation LaTeX table written to: ", out_tex)
