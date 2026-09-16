# ------------------------------------------------------------------------------
# Purpose: Compare latest-filing EEO-1 and Yimfor workforce shares
# ------------------------------------------------------------------------------

report_eeo1_yimfor_spearman <- function(
    eeo1_path = file.path(processed, "eeo1_latest_firm_shares.csv"),
    linkedin_output_dir = output_dir
) {
  eeo1_shares <- readr::read_csv(
    eeo1_path,
    col_types = readr::cols(
      firm = readr::col_character(),
      black_share = readr::col_double(),
      female_share = readr::col_double()
    ),
    show_col_types = FALSE
  )
  yimfor_shares <- read_parquet_sheet(
    linkedin_output_dir,
    linkedin_firm_shares_sheet
  ) |>
    dplyr::filter(.data$new_matched) |>
    dplyr::transmute(
      firm = .data$firm,
      yimfor_black_share = .data$new_share_black,
      yimfor_female_share = .data$new_share_female
    )
  matched_shares <- dplyr::inner_join(eeo1_shares, yimfor_shares, by = "firm")

  required_columns <- c(
    "black_share",
    "female_share",
    "yimfor_black_share",
    "yimfor_female_share"
  )
  if (nrow(matched_shares) != 114L ||
      dplyr::n_distinct(matched_shares$firm) != 114L ||
      any(!is.finite(as.matrix(matched_shares[required_columns])))) {
    stop("Expected 114 firms with complete EEO-1 and Yimfor shares.")
  }

  results <- tibble::tibble(
    measure = c("Black share", "Female share"),
    n_firms = nrow(matched_shares),
    spearman_correlation = c(
      stats::cor(
        matched_shares$black_share,
        matched_shares$yimfor_black_share,
        method = "spearman"
      ),
      stats::cor(
        matched_shares$female_share,
        matched_shares$yimfor_female_share,
        method = "spearman"
      )
    )
  )
  print(results)
  invisible(results)
}

eeo1_yimfor_spearman <- report_eeo1_yimfor_spearman()
