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
                                 root,
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

      d1 <- sample_filemap$subdir[sample_filemap$sample == s1]
      d2 <- sample_filemap$subdir[sample_filemap$sample == s2]
      if (length(d1) != 1L || length(d2) != 1L) {
        warning("Directory not found for sample(s): ", s1, " or ", s2)
        next
      }

      theta1 <- read_theta(root, d1, outcome, model = model)
      theta2 <- read_theta(root, d2, outcome, model = model)

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

  if (length(rows) == 0L) {
    return(tibble::tibble(
      model = character(),
      outcome = character(),
      sample1 = character(),
      sample2 = character(),
      J_firms = integer(),
      raw_corr = numeric()
    ))
  }
  dplyr::bind_rows(rows)
}

# -------------------------------------------------------------------
# 4. Run for OLS + Borda only
# -------------------------------------------------------------------

borda_corr <- build_corr_table_raw("borda", intermediate, sample_filemap, sample_pairs, outcomes)
ols_corr   <- build_corr_table_raw("ols",   intermediate, sample_filemap, sample_pairs, outcomes)

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
  "Convenience vs Probability",      "Convenience",            "Probability",
  "Confident vs Not (Gender)",       "Not Confident (Gender)", "Confident (Gender)",
  "Confident vs Not (Race)",         "Not Confident (Race)",   "Confident (Race)"
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

panel_ols   <- get_panel_matrix_raw(ols_corr)
panel_borda <- get_panel_matrix_raw(borda_corr)

panel_rows_corr_only <- function(panel) {
  paste0("    ", panel$Row, " & ",
         panel$`Discrimination Black`, " & ",
         panel$`Discrimination Female`, " \\\\")
}

latex_lines_ols_borda <- c(
  "  \\centering",
  "  \\begin{tabular}{lcc}",
  "    \\toprule",
  "    & Discrimination Black & Discrimination Female \\\\",
  "    \\midrule",
  "    \\multicolumn{3}{l}{\\textbf{Panel A: Likert (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panel_ols),
  "    \\addlinespace",
  "    \\multicolumn{3}{l}{\\textbf{Panel B: Borda (Raw Correlation)}}\\\\",
  panel_rows_corr_only(panel_borda),
  "    \\bottomrule",
  "  \\end{tabular}"
)

write_latex_lines_checked <- function(latex_lines, out_tex) {
  if (length(latex_lines) == 0L) {
    stop("Refusing to write empty LaTeX output: ", out_tex)
  }

  tmp_tex <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(out_tex)), "_"),
                      fileext = ".tex")
  on.exit(unlink(tmp_tex), add = TRUE)

  writeLines(latex_lines, tmp_tex, useBytes = TRUE)
  tmp_size <- file.info(tmp_tex)$size
  if (is.na(tmp_size) || tmp_size == 0L) {
    stop("Temporary LaTeX output is empty before copy: ", tmp_tex)
  }

  if (file.exists(out_tex)) {
    unlink_status <- unlink(out_tex)
    if (unlink_status != 0L && file.exists(out_tex)) {
      stop("Could not remove existing LaTeX output before overwrite: ", out_tex)
    }
  }

  if (!file.copy(tmp_tex, out_tex, overwrite = TRUE)) {
    stop("Could not copy LaTeX output into place: ", out_tex)
  }

  out_size <- file.info(out_tex)$size
  if (is.na(out_size) || out_size == 0L) {
    stop("LaTeX output is empty after write: ", out_tex)
  }

  invisible(out_tex)
}

for (out_tex in c(
  file.path(tables, "cross_sample_corr_raw.tex"),
  file.path(tables, "cross_sample_corr_raw_ols_borda.tex")
)) {
  write_latex_lines_checked(latex_lines_ols_borda, out_tex)
  message("Raw correlation OLS+Borda LaTeX table written to: ", out_tex)
}
