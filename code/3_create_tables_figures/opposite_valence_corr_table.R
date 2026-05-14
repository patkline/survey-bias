source("code/globals.R")

# Build a row-wise table of opposite-valence pair correlations across models
# using corr_c from the new-pipeline correlation sheet.

full_sample_dir <- file.path(intermediate, "Full_Sample")
all_firms_flag <- TRUE

opposite_pairs <- tibble::tribble(
  ~pair_label,                                 ~lhs,               ~rhs,
  "Firm Contact: Black vs White",              "FirmCont_black",   "FirmCont_white",
  "Firm Hire: Black vs White",                 "FirmHire_black",   "FirmHire_white",
  "Conduct: Black vs White",                   "conduct_black",    "conduct_white",
  "Firm Contact: Male vs Female",              "FirmCont_male",    "FirmCont_female",
  "Firm Hire: Male vs Female",                 "FirmHire_male",    "FirmHire_female",
  "Conduct: Male vs Female",                   "conduct_male",     "conduct_female",
  "Conduct: Younger vs Older",                 "conduct_younger",  "conduct_older"
)

pair_key <- function(lhs, rhs) paste(pmin(lhs, rhs), pmax(lhs, rhs), sep = "||")

to_logical_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(tolower(x) %in% c("true", "t", "1"))
  as.logical(x)
}

read_pairwise_corr <- function(dir_path, model_value, model_col, all_firms_value = TRUE) {
  df <- read_parquet_sheet(dir_path, "correlation")
  req <- c("lhs", "rhs", "corr_c")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("Sheet 'correlation' is missing required columns: ",
         paste(miss, collapse = ", "))
  }

  if ("model" %in% names(df)) {
    df <- df[df$model == model_value, , drop = FALSE]
  }
  if ("all_firms" %in% names(df)) {
    flags <- to_logical_flag(df$all_firms)
    df <- df[!is.na(flags) & flags == all_firms_value, , drop = FALSE]
  } else if ("subset" %in% names(df)) {
    subset_value <- if (isTRUE(all_firms_value)) "all" else "subset97"
    df <- df[df$subset == subset_value, , drop = FALSE]
  }

  df %>%
    dplyr::transmute(
      key = pair_key(as.character(lhs), as.character(rhs)),
      corr_c = suppressWarnings(as.numeric(corr_c))
    ) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(
      !!model_col := {
        vals <- corr_c[is.finite(corr_c)]
        if (length(vals) > 0) vals[1] else NA_real_
      },
      .groups = "drop"
    )
}

ols_corr <- read_pairwise_corr(
  dir_path = full_sample_dir,
  model_value = "OLS",
  model_col = "OLS",
  all_firms_value = all_firms_flag
)

borda_corr <- read_pairwise_corr(
  dir_path = full_sample_dir,
  model_value = "Borda",
  model_col = "Borda",
  all_firms_value = all_firms_flag
)

fmt3 <- function(x) ifelse(is.na(x), "", formatC(x, digits = 3, format = "f"))

table_long <- opposite_pairs %>%
  dplyr::mutate(key = pair_key(lhs, rhs)) %>%
  dplyr::left_join(ols_corr, by = "key") %>%
  dplyr::left_join(borda_corr, by = "key") %>%
  dplyr::select(pair_label, OLS, Borda)

table_fmt <- table_long %>%
  dplyr::mutate(
    OLS = fmt3(OLS),
    Borda = fmt3(Borda)
  )

table_out <- as.data.frame(table_fmt[, c("OLS", "Borda")], stringsAsFactors = FALSE)
rownames(table_out) <- table_fmt$pair_label

dir.create(tables, showWarnings = FALSE, recursive = TRUE)

out_csv <- file.path(tables, "opposite_valence_corr_ols_borda.csv")
write.csv(table_out, out_csv, row.names = TRUE, quote = FALSE)

latex_rows <- paste0(
  "    ", table_fmt$pair_label, " & ", table_fmt$OLS, " & ", table_fmt$Borda, " \\\\"
)

latex_lines <- c(
  "  \\centering",
  "  \\begin{tabular}{lcc}",
  "    \\toprule",
  "     & Likert & Borda \\\\",
  "    \\midrule",
  latex_rows,
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "opposite_valence_corr_ols_borda.tex")
writeLines(latex_lines, out_tex)

message("Saved opposite-valence corr table to:")
message(" - ", out_csv)
message(" - ", out_tex)
