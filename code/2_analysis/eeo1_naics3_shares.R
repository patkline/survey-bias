# ------------------------------------------------------------------------------
# Purpose: Load the firm-to-NAICS3 crosswalk and national EEO-1 NAICS3 shares
# ------------------------------------------------------------------------------

naics3_is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

naics3_as_numeric_suppressed <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

naics3_row_sum_strict <- function(data, variables) {
  values <- vapply(
    variables,
    function(variable) naics3_as_numeric_suppressed(data[[variable]]),
    FUN.VALUE = numeric(nrow(data))
  )
  rowSums(values, na.rm = FALSE)
}

load_firm_naics3_crosswalk <- function(
    path = file.path(processed, "firm_naics3.csv")
) {
  crosswalk <- readr::read_csv(
    path,
    col_types = readr::cols(
      firm_name = readr::col_character(),
      naics3 = readr::col_integer(),
      naics3_name = readr::col_character()
    ),
    show_col_types = FALSE
  )

  if (!identical(names(crosswalk), c("firm_name", "naics3", "naics3_name")) ||
      nrow(crosswalk) != 164L ||
      anyDuplicated(crosswalk$firm_name) ||
      dplyr::n_distinct(crosswalk$naics3) != 48L ||
      any(naics3_is_blank(crosswalk$firm_name)) ||
      any(naics3_is_blank(crosswalk$naics3_name))) {
    stop("The firm-to-NAICS3 crosswalk is not the expected 164-firm input.")
  }

  crosswalk
}

load_eeo1_naics3_shares <- function(
    path = file.path(external, "EEO1_2023_PUF.xlsx")
) {
  eeo1_raw <- readxl::read_excel(
    path,
    sheet = "Data",
    col_types = "text"
  )

  national_rows <- Reduce(
    `&`,
    lapply(c("Region", "Division", "State", "CBSA", "County"), function(variable) {
      naics3_is_blank(eeo1_raw[[variable]])
    })
  )

  shares <- eeo1_raw |>
    dplyr::filter(national_rows, !naics3_is_blank(.data$NAICS3)) |>
    dplyr::mutate(
      naics3 = as.integer(.data$NAICS3),
      eeo1_total_employment_all_jobs =
        naics3_as_numeric_suppressed(.data$TOTAL10),
      eeo1_black_employment_all_jobs =
        naics3_as_numeric_suppressed(.data$BLKT10),
      eeo1_female_employment_all_jobs =
        naics3_as_numeric_suppressed(.data$FT10),
      eeo1_total_employment_front_line = naics3_row_sum_strict(
        dplyr::pick(dplyr::everything()),
        c("TOTAL4", "TOTAL5", "TOTAL6", "TOTAL8", "TOTAL9")
      ),
      eeo1_black_employment_front_line = naics3_row_sum_strict(
        dplyr::pick(dplyr::everything()),
        c("BLKT4", "BLKT5", "BLKT6", "BLKT8", "BLKT9")
      ),
      eeo1_female_employment_front_line = naics3_row_sum_strict(
        dplyr::pick(dplyr::everything()),
        c("FT4", "FT5", "FT6", "FT8", "FT9")
      ),
      eeo1_black_all_jobs_share =
        .data$eeo1_black_employment_all_jobs /
          .data$eeo1_total_employment_all_jobs,
      eeo1_female_all_jobs_share =
        .data$eeo1_female_employment_all_jobs /
          .data$eeo1_total_employment_all_jobs,
      eeo1_black_front_line_share =
        .data$eeo1_black_employment_front_line /
          .data$eeo1_total_employment_front_line,
      eeo1_female_front_line_share =
        .data$eeo1_female_employment_front_line /
          .data$eeo1_total_employment_front_line
    ) |>
    dplyr::select(
      naics3,
      eeo1_black_all_jobs_share,
      eeo1_black_front_line_share,
      eeo1_female_all_jobs_share,
      eeo1_female_front_line_share
    )

  if (anyDuplicated(shares$naics3)) {
    stop("National EEO-1 rows are not unique by NAICS3.")
  }

  shares
}

add_eeo1_naics3_shares_to_firms <- function(
    data,
    firm_name_col = "entity",
    crosswalk = load_firm_naics3_crosswalk(),
    shares = load_eeo1_naics3_shares()
) {
  if (!firm_name_col %in% names(data)) {
    stop("Firm-name column is missing from the firm-level data: ", firm_name_col)
  }

  out <- data |>
    dplyr::left_join(
      crosswalk,
      by = stats::setNames("firm_name", firm_name_col)
    ) |>
    dplyr::left_join(shares, by = "naics3")

  if (any(is.na(out$naics3))) {
    stop("At least one firm is missing from the NAICS3 crosswalk.")
  }
  if (any(is.na(out$eeo1_black_all_jobs_share)) ||
      any(is.na(out$eeo1_female_all_jobs_share))) {
    stop("At least one firm NAICS3 is missing its all-jobs EEO-1 shares.")
  }

  out
}
