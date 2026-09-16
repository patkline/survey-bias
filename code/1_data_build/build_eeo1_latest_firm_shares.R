# ------------------------------------------------------------------------------
# Purpose: Build latest-filing EEO-1 Black and female shares for matched firms
# ------------------------------------------------------------------------------

build_eeo1_latest_firm_shares <- function(
    raw_dir = file.path(dropbox_survey_bias_root, "external", "eeo1_ofccp"),
    output_path = file.path(processed, "eeo1_latest_firm_shares.csv")
) {
  crosswalk_path <- file.path(raw_dir, "eeo1_latest_filing_crosswalk.csv")
  key_columns <- c("YEAR", "COMPANY", "UNIT")
  count_columns <- c("TOTAL10", "FT10", "BLKF10", "BLKM10")

  clean_eeo1_id <- function(x) {
    out <- trimws(as.character(x))
    out[is.na(out)] <- ""
    sub("\\.0$", "", out)
  }

  crosswalk <- readr::read_csv(
    crosswalk_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      firm = .data$firm,
      source_file = .data$source_file,
      YEAR = clean_eeo1_id(.data$year),
      COMPANY = clean_eeo1_id(.data$company_id),
      UNIT = clean_eeo1_id(.data$unit_id),
      expected_total10 = clean_eeo1_id(.data$expected_total10)
    )

  matched <- lapply(unique(crosswalk$source_file), function(source_file) {
    raw_path <- file.path(raw_dir, source_file)
    raw <- if (grepl("\\.xlsx$", source_file, ignore.case = TRUE)) {
      readxl::read_excel(
        raw_path,
        sheet = "Responsive EEO-1 Reports ",
        col_types = "text"
      )
    } else {
      readr::read_csv(
        raw_path,
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(encoding = "Latin1"),
        show_col_types = FALSE,
        progress = FALSE
      )
    }

    missing_columns <- setdiff(c(key_columns, count_columns), names(raw))
    if (length(missing_columns)) {
      stop(
        "EEO-1 source is missing columns: ",
        paste(missing_columns, collapse = ", ")
      )
    }

    raw <- raw |>
      dplyr::select(dplyr::all_of(c(key_columns, count_columns))) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(key_columns), clean_eeo1_id)
      )
    keys <- crosswalk |>
      dplyr::filter(.data$source_file == .env$source_file)
    rows <- dplyr::left_join(keys, raw, by = key_columns)

    if (nrow(rows) != nrow(keys) || any(is.na(rows$TOTAL10))) {
      stop("Unmatched or duplicated EEO-1 crosswalk row in ", source_file)
    }
    if (any(clean_eeo1_id(rows$TOTAL10) != rows$expected_total10)) {
      stop("EEO-1 employee-total validation failed in ", source_file)
    }
    rows
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_columns), ~ as.numeric(.x))
    )

  if (nrow(matched) != 117L ||
      dplyr::n_distinct(matched$firm) != 117L ||
      any(!is.finite(as.matrix(matched[count_columns]))) ||
      any(matched$TOTAL10 <= 0)) {
    stop("Expected 117 unique firms with valid latest EEO-1 filings.")
  }

  output <- matched |>
    dplyr::transmute(
      firm = .data$firm,
      black_share = (.data$BLKF10 + .data$BLKM10) / .data$TOTAL10,
      female_share = .data$FT10 / .data$TOTAL10
    ) |>
    dplyr::arrange(tolower(.data$firm))

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(output, output_path)
  message("Wrote ", nrow(output), " latest-filing EEO-1 shares to ", output_path)
  invisible(output)
}

eeo1_latest_firm_shares <- build_eeo1_latest_firm_shares()
