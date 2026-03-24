source("code/globals.R")

# Build a row-wise table of opposite-valence pair correlations across models
# using corr_c from each pairwise summary sheet.

full_sample_wb <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")
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

read_pairwise_corr <- function(path, sheet, model_col, all_firms_value = TRUE) {
  df <- readxl::read_xlsx(path, sheet = sheet)
  req <- c("lhs", "rhs", "corr_c")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("Sheet '", sheet, "' is missing required columns: ",
         paste(miss, collapse = ", "))
  }

  if ("all_firms" %in% names(df)) {
    flags <- to_logical_flag(df$all_firms)
    df <- df[!is.na(flags) & flags == all_firms_value, , drop = FALSE]
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

ol_corr <- read_pairwise_corr(
  path = full_sample_wb,
  sheet = "pairwise_summary_ol",
  model_col = "OL",
  all_firms_value = all_firms_flag
)

pl_corr <- read_pairwise_corr(
  path = full_sample_wb,
  sheet = "pairwise_summary",
  model_col = "PL",
  all_firms_value = all_firms_flag
)

borda_corr <- read_pairwise_corr(
  path = full_sample_wb,
  sheet = "pairwise_summary_borda",
  model_col = "Borda",
  all_firms_value = all_firms_flag
)

fmt3 <- function(x) ifelse(is.na(x), "", formatC(x, digits = 3, format = "f"))

table_long <- opposite_pairs %>%
  dplyr::mutate(key = pair_key(lhs, rhs)) %>%
  dplyr::left_join(ol_corr, by = "key") %>%
  dplyr::left_join(pl_corr, by = "key") %>%
  dplyr::left_join(borda_corr, by = "key") %>%
  dplyr::select(pair_label, OL, PL, Borda)

table_fmt <- table_long %>%
  dplyr::mutate(
    OL = fmt3(OL),
    PL = fmt3(PL),
    Borda = fmt3(Borda)
  )

table_out <- as.data.frame(table_fmt[, c("OL", "PL", "Borda")], stringsAsFactors = FALSE)
rownames(table_out) <- table_fmt$pair_label

dir.create(tables, showWarnings = FALSE, recursive = TRUE)

out_csv <- file.path(tables, "opposite_valence_corr_ol_pl_borda.csv")
write.csv(table_out, out_csv, row.names = TRUE, quote = FALSE)

latex_rows <- paste0(
  "    ", table_fmt$pair_label, " & ", table_fmt$OL, " & ", table_fmt$PL, " & ", table_fmt$Borda, " \\\\"
)

latex_lines <- c(
  "  \\centering",
  "  \\begin{tabular}{lccc}",
  "    \\toprule",
  "     & OL & PL & Borda \\\\",
  "    \\midrule",
  latex_rows,
  "    \\bottomrule",
  "  \\end{tabular}"
)

out_tex <- file.path(tables, "opposite_valence_corr_ol_pl_borda.tex")
writeLines(latex_lines, out_tex)

message("Saved opposite-valence corr table to:")
message(" - ", out_csv)
message(" - ", out_tex)
