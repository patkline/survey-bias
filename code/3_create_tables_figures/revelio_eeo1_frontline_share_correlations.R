# ------------------------------------------------------------------------------
# Purpose: Compare firm-level Revelio workforce shares to EEO-1 industry shares
#
# Builds a dataset with one row per survey firm. Each row contains the firm's
# Revelio entry-level and all-workforce race/gender shares, plus EEO-1
# race/gender shares for the firm's industry.
# ------------------------------------------------------------------------------

find_project_root <- function(start_path = getwd()) {
  current_path <- normalizePath(start_path, mustWork = TRUE)

  repeat {
    if (dir.exists(file.path(current_path, ".git")) &&
        dir.exists(file.path(current_path, "code"))) {
      return(current_path)
    }

    parent_path <- dirname(current_path)
    if (identical(parent_path, current_path)) {
      stop("Could not find survey-bias project root.", call. = FALSE)
    }
    current_path <- parent_path
  }
}

project_root <- find_project_root()

dropbox_roots_by_user <- c(
  nicorotundo = "/Users/nicorotundo/Library/CloudStorage/Dropbox/Survey/consolidated_code",
  monicahea = "/Users/monicahea/Dropbox/Survey/consolidated_code",
  jordancammarota = "/Users/jordancammarota/Dropbox/consolidated_code",
  "anh-huynguyen" = "/Users/anh-huynguyen/Dropbox/Survey/consolidated_code"
)

user <- tolower(Sys.info()[["user"]])
dropbox_root <- unname(dropbox_roots_by_user[user])

candidate_data_roots <- c(
  if (!is.na(dropbox_root)) {
    file.path(dropbox_root, "github_data_and_output_mirrors", "data")
  },
  file.path(project_root, "data")
)

first_existing_file <- function(relative_path, label) {
  candidate_files <- file.path(candidate_data_roots, relative_path)
  existing_files <- candidate_files[file.exists(candidate_files)]

  if (!length(existing_files)) {
    stop(
      label, " file not found. Checked: ",
      paste(candidate_files, collapse = "; "),
      call. = FALSE
    )
  }

  existing_files[[1L]]
}

revelio_entry_all_file <- first_existing_file(
  file.path("external", "revelio_company_race_gender_salary_2023_entry_and_all_with_parent_subsidiaries.csv"),
  "Revelio entry/all firm measures"
)

survey_file <- first_existing_file(
  file.path("processed", "long_survey_final.csv"),
  "Long survey data"
)

eeo1_industry_shares_file <- first_existing_file(
  file.path("dump", "industry_emp_share_by_demographic_eeo1.csv"),
  "EEO-1 industry shares"
)

revelio_eeo1_firm_share_output_file <- file.path(
  dirname(eeo1_industry_shares_file),
  "revelio_eeo1_frontline_share_firm_data.csv"
)

figures_dir <- if (!is.na(dropbox_root)) {
  file.path(dropbox_root, "github_data_and_output_mirrors", "output", "figures")
} else {
  file.path(project_root, "output", "figures")
}

read_required_csv <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " file not found: ", path, call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

industry_key <- function(x) {
  trimws(as.character(x))
}

industry_name_key <- function(x) {
  trimws(as.character(x))
}

require_unique_key <- function(data, key, label) {
  duplicated_key <- data[[key]][duplicated(data[[key]])]
  if (length(duplicated_key) > 0L) {
    stop(
      label, " has duplicated ", key, " values: ",
      paste(unique(duplicated_key), collapse = ", "),
      call. = FALSE
    )
  }
}

drop_words <- c(
  "inc",
  "incorporated",
  "corp",
  "corporation",
  "co",
  "company",
  "companies",
  "cos",
  "group",
  "holdings",
  "holding",
  "plc",
  "llc",
  "ltd",
  "limited",
  "the"
)

company_aliases <- c(
  adp = "automatic data processing",
  carrier = "carrier global",
  costco = "costco wholesale",
  disney = "walt disney",
  dowdupont = "dow",
  "dr pepper snapple" = "keurig dr pepper",
  "est ee lauder" = "estee lauder",
  "estee lauder" = "estee lauder",
  "general electric" = "ge aerospace",
  ibm = "international business machines",
  "icahn enterprises" = "icahn enterprises lp",
  "jpmorgan chase" = "jpmorgan chase and",
  "laboratory of america" = "labcorp",
  merck = "merck and",
  otis = "otis worldwide",
  sears = "sears roebuck and",
  synnex = "td synnex",
  "united continental" = "united airlines",
  ups = "united parcel service",
  verizon = "verizon communications",
  "victoria s secret" = "victoria s secret and",
  "wells fargo" = "wells fargo and",
  "xpo logistics" = "xpo"
)

normalize_company_name_one <- function(value) {
  if (is.na(value)) return("")

  value <- iconv(as.character(value), from = "", to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(gsub("&", " and ", value, fixed = TRUE))
  value <- gsub("[^a-z0-9]+", " ", value)

  tokens <- strsplit(trimws(value), "\\s+")[[1L]]
  if (length(tokens) == 1L && identical(tokens, "")) return("")

  key <- trimws(paste(tokens[!(tokens %in% drop_words)], collapse = " "))
  if (key %in% names(company_aliases)) {
    unname(company_aliases[[key]])
  } else {
    key
  }
}

normalize_company_name <- function(value) {
  vapply(value, normalize_company_name_one, character(1L), USE.NAMES = FALSE)
}

build_revelio_lookup <- function(revelio_raw) {
  revelio_base <- revelio_raw
  names(revelio_base)[names(revelio_base) == "company"] <- "revelio_company"
  names(revelio_base)[names(revelio_base) == "company_name"] <- "revelio_company_name"

  lookup_parts <- list()
  for (field in c("revelio_company", "revelio_company_name")) {
    lookup_piece <- revelio_base
    lookup_piece$revelio_key <- normalize_company_name(lookup_piece[[field]])
    lookup_piece$revelio_match_field <- if (field == "revelio_company") {
      "company"
    } else {
      "company_name"
    }
    lookup_piece$revelio_match_priority <- if (field == "revelio_company") 1L else 2L
    lookup_parts[[field]] <- lookup_piece
  }

  lookup_all <- unique(do.call(rbind, lookup_parts))
  lookup_all <- lookup_all[
    !is.na(lookup_all$revelio_key) & lookup_all$revelio_key != "",
  ]

  key_firm_pairs <- unique(lookup_all[, c("revelio_key", "analysis_firm_key")])
  duplicate_counts <- table(key_firm_pairs$revelio_key)
  duplicate_keys <- names(duplicate_counts)[duplicate_counts > 1L]
  if (length(duplicate_keys) > 0L) {
    stop(
      "Duplicate normalized Revelio match keys found: ",
      paste(duplicate_keys, collapse = ", "),
      call. = FALSE
    )
  }

  lookup_all <- lookup_all[
    order(lookup_all$revelio_key, lookup_all$revelio_match_priority),
  ]
  lookup_all[!duplicated(lookup_all$revelio_key), ]
}

survey_data <- read_required_csv(
  survey_file,
  "Long survey data"
)

revelio_raw <- read_required_csv(
  revelio_entry_all_file,
  "Revelio entry/all firm measures"
)

eeo1_industry_shares <- read_required_csv(
  eeo1_industry_shares_file,
  "EEO-1 industry shares"
)

survey_firms <- unique(data.frame(
  firm_id = as.integer(survey_data$firm_id),
  firm = as.character(survey_data$firm),
  aer_naics2 = industry_key(survey_data$aer_naics2),
  aer_naics2_name = industry_name_key(survey_data$aer_naics2_name),
  stringsAsFactors = FALSE
))
survey_firms <- survey_firms[!is.na(survey_firms$firm) & survey_firms$firm != "nan", ]
survey_firms$survey_key <- normalize_company_name(survey_firms$firm)

require_unique_key(
  survey_firms,
  "firm_id",
  "Survey firm-level data"
)

eeo1_industry_share_measures <- data.frame(
  aer_naics2 = industry_key(eeo1_industry_shares$sic_two_digit_bin_aer),
  eeo1_industry_name = industry_name_key(eeo1_industry_shares$sic_two_digit_bin_title_aer),
  eeo1_black_all_jobs_share = as.numeric(eeo1_industry_shares$share_emp_black_all_jobs),
  eeo1_black_mid_off_manager_share = as.numeric(eeo1_industry_shares$share_emp_black_mid_off_manager),
  eeo1_black_front_line_share = as.numeric(eeo1_industry_shares$share_emp_black_front_line),
  eeo1_female_all_jobs_share = as.numeric(eeo1_industry_shares$share_emp_female_all_jobs),
  eeo1_female_mid_off_manager_share = as.numeric(eeo1_industry_shares$share_emp_female_mid_off_manager),
  eeo1_female_front_line_share = as.numeric(eeo1_industry_shares$share_emp_female_front_line),
  stringsAsFactors = FALSE
)

require_unique_key(
  eeo1_industry_share_measures,
  "aer_naics2",
  "EEO-1 industry shares"
)

require_unique_key(
  eeo1_industry_share_measures,
  "eeo1_industry_name",
  "EEO-1 industry shares"
)

revelio_lookup <- build_revelio_lookup(revelio_raw)

revelio_match_index <- match(survey_firms$survey_key, revelio_lookup$revelio_key)
revelio_matched <- revelio_lookup[revelio_match_index, ]

revelio_eeo1_firm_share_data <- data.frame(
  firm_id = survey_firms$firm_id,
  firm = survey_firms$firm,
  survey_key = survey_firms$survey_key,
  matched_to_revelio = !is.na(revelio_matched$analysis_firm_key),
  revelio_match_field = revelio_matched$revelio_match_field,
  analysis_firm_key = revelio_matched$analysis_firm_key,
  requested_rcid = revelio_matched$requested_rcid,
  workforce_source_rcid = revelio_matched$workforce_source_rcid,
  workforce_source_company = revelio_matched$workforce_source_company,
  revelio_company = revelio_matched$revelio_company,
  revelio_company_name = revelio_matched$revelio_company_name,
  parent_company = revelio_matched$parent_company,
  workforce_match_type = revelio_matched$workforce_match_type,
  entry_weighted_n_users = as.numeric(revelio_matched$entry_weighted_n_users),
  all_weighted_n_users = as.numeric(revelio_matched$all_weighted_n_users),
  revelio_entry_black_share = as.numeric(revelio_matched$entry_black_share),
  revelio_all_black_share = as.numeric(revelio_matched$all_black_share),
  revelio_entry_female_share = as.numeric(revelio_matched$entry_female_share),
  revelio_all_female_share = as.numeric(revelio_matched$all_female_share),
  aer_naics2 = survey_firms$aer_naics2,
  aer_naics2_name = survey_firms$aer_naics2_name,
  stringsAsFactors = FALSE
)

eeo1_match_index <- match(
  industry_name_key(revelio_eeo1_firm_share_data$aer_naics2_name),
  eeo1_industry_share_measures$eeo1_industry_name
)

revelio_eeo1_firm_share_data$eeo1_industry_name <-
  eeo1_industry_share_measures$eeo1_industry_name[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_black_all_jobs_share <-
  eeo1_industry_share_measures$eeo1_black_all_jobs_share[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_black_mid_off_manager_share <-
  eeo1_industry_share_measures$eeo1_black_mid_off_manager_share[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_black_front_line_share <-
  eeo1_industry_share_measures$eeo1_black_front_line_share[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_female_all_jobs_share <-
  eeo1_industry_share_measures$eeo1_female_all_jobs_share[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_female_mid_off_manager_share <-
  eeo1_industry_share_measures$eeo1_female_mid_off_manager_share[eeo1_match_index]
revelio_eeo1_firm_share_data$eeo1_female_front_line_share <-
  eeo1_industry_share_measures$eeo1_female_front_line_share[eeo1_match_index]

revelio_eeo1_firm_share_data <- revelio_eeo1_firm_share_data[
  order(revelio_eeo1_firm_share_data$firm_id),
]

if (nrow(revelio_eeo1_firm_share_data) != 164L) {
  stop(
    "Expected 164 firm rows; found ",
    nrow(revelio_eeo1_firm_share_data),
    ".",
    call. = FALSE
  )
}

require_unique_key(
  revelio_eeo1_firm_share_data,
  "firm_id",
  "Revelio x EEO-1 firm share dataset"
)

missing_eeo1 <- revelio_eeo1_firm_share_data[
  is.na(revelio_eeo1_firm_share_data$eeo1_black_all_jobs_share) |
    is.na(revelio_eeo1_firm_share_data$eeo1_black_mid_off_manager_share) |
    is.na(revelio_eeo1_firm_share_data$eeo1_black_front_line_share) |
    is.na(revelio_eeo1_firm_share_data$eeo1_female_all_jobs_share) |
    is.na(revelio_eeo1_firm_share_data$eeo1_female_mid_off_manager_share) |
    is.na(revelio_eeo1_firm_share_data$eeo1_female_front_line_share),
]

if (nrow(missing_eeo1) > 0L) {
  stop(
    "Some firm industries do not match the EEO-1 industry-share file: ",
    paste(unique(missing_eeo1$aer_naics2), collapse = ", "),
    call. = FALSE
  )
}

share_correlation <- function(data, revelio_share, eeo1_share, comparison) {
  complete_rows <- is.finite(data[[revelio_share]]) & is.finite(data[[eeo1_share]])

  data.frame(
    comparison = comparison,
    revelio_share = revelio_share,
    eeo1_share = eeo1_share,
    n_firms = sum(complete_rows),
    correlation = stats::cor(
      data[[revelio_share]][complete_rows],
      data[[eeo1_share]][complete_rows]
    ),
    stringsAsFactors = FALSE
  )
}

revelio_eeo1_share_correlations <- rbind(
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_black_share",
    "eeo1_black_all_jobs_share",
    "Entry Black share: EEO-1 all jobs"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_black_share",
    "eeo1_black_mid_off_manager_share",
    "Entry Black share: EEO-1 mid off/manager"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_black_share",
    "eeo1_black_front_line_share",
    "Entry Black share: EEO-1 front line"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_black_share",
    "eeo1_black_all_jobs_share",
    "All-workforce Black share: EEO-1 all jobs"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_black_share",
    "eeo1_black_mid_off_manager_share",
    "All-workforce Black share: EEO-1 mid off/manager"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_black_share",
    "eeo1_black_front_line_share",
    "All-workforce Black share: EEO-1 front line"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_female_share",
    "eeo1_female_all_jobs_share",
    "Entry female share: EEO-1 all jobs"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_female_share",
    "eeo1_female_mid_off_manager_share",
    "Entry female share: EEO-1 mid off/manager"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_entry_female_share",
    "eeo1_female_front_line_share",
    "Entry female share: EEO-1 front line"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_female_share",
    "eeo1_female_all_jobs_share",
    "All-workforce female share: EEO-1 all jobs"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_female_share",
    "eeo1_female_mid_off_manager_share",
    "All-workforce female share: EEO-1 mid off/manager"
  ),
  share_correlation(
    revelio_eeo1_firm_share_data,
    "revelio_all_female_share",
    "eeo1_female_front_line_share",
    "All-workforce female share: EEO-1 front line"
  )
)

weighted_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

build_revelio_industry_black_shares <- function(firm_share_data) {
  industry_names <- sort(unique(firm_share_data$aer_naics2_name))

  do.call(rbind, lapply(industry_names, function(industry_name) {
    industry_data <- firm_share_data[firm_share_data$aer_naics2_name == industry_name, ]

    data.frame(
      aer_naics2_name = industry_name,
      n_survey_firms = length(unique(industry_data$firm_id)),
      n_revelio_entry_black_firms = sum(
        is.finite(industry_data$revelio_entry_black_share) &
          is.finite(industry_data$entry_weighted_n_users) &
          industry_data$entry_weighted_n_users > 0
      ),
      n_revelio_all_black_firms = sum(
        is.finite(industry_data$revelio_all_black_share) &
          is.finite(industry_data$all_weighted_n_users) &
          industry_data$all_weighted_n_users > 0
      ),
      revelio_entry_black_share = weighted_mean(
        industry_data$revelio_entry_black_share,
        industry_data$entry_weighted_n_users
      ),
      revelio_all_black_share = weighted_mean(
        industry_data$revelio_all_black_share,
        industry_data$all_weighted_n_users
      ),
      revelio_entry_weighted_n_users = sum(
        industry_data$entry_weighted_n_users[
          is.finite(industry_data$revelio_entry_black_share) &
            is.finite(industry_data$entry_weighted_n_users) &
            industry_data$entry_weighted_n_users > 0
        ],
        na.rm = TRUE
      ),
      revelio_all_weighted_n_users = sum(
        industry_data$all_weighted_n_users[
          is.finite(industry_data$revelio_all_black_share) &
            is.finite(industry_data$all_weighted_n_users) &
            industry_data$all_weighted_n_users > 0
        ],
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }))
}

revelio_industry_black_shares <- build_revelio_industry_black_shares(
  revelio_eeo1_firm_share_data
)

eeo1_black_industry_share_measures <- data.frame(
  aer_naics2_name = eeo1_industry_share_measures$eeo1_industry_name,
  eeo1_black_all_jobs_share = eeo1_industry_share_measures$eeo1_black_all_jobs_share,
  eeo1_black_mid_off_manager_share = eeo1_industry_share_measures$eeo1_black_mid_off_manager_share,
  eeo1_black_front_line_share = eeo1_industry_share_measures$eeo1_black_front_line_share,
  stringsAsFactors = FALSE
)

revelio_industry_match_index <- match(
  eeo1_black_industry_share_measures$aer_naics2_name,
  revelio_industry_black_shares$aer_naics2_name
)

revelio_eeo1_industry_black_share_data <- eeo1_black_industry_share_measures
revelio_eeo1_industry_black_share_data$n_survey_firms <-
  revelio_industry_black_shares$n_survey_firms[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$n_revelio_entry_black_firms <-
  revelio_industry_black_shares$n_revelio_entry_black_firms[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$n_revelio_all_black_firms <-
  revelio_industry_black_shares$n_revelio_all_black_firms[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$revelio_entry_black_share <-
  revelio_industry_black_shares$revelio_entry_black_share[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$revelio_all_black_share <-
  revelio_industry_black_shares$revelio_all_black_share[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$revelio_entry_weighted_n_users <-
  revelio_industry_black_shares$revelio_entry_weighted_n_users[revelio_industry_match_index]
revelio_eeo1_industry_black_share_data$revelio_all_weighted_n_users <-
  revelio_industry_black_shares$revelio_all_weighted_n_users[revelio_industry_match_index]

plot_black_share_scatter <- function(data, revelio_share, eeo1_share,
                                     revelio_label, eeo1_label, output_file) {
  ok <- is.finite(data[[revelio_share]]) & is.finite(data[[eeo1_share]])
  plot_data <- data[ok, ]
  if (nrow(plot_data) < 2L) {
    stop("Not enough complete industry rows to plot ", output_file, call. = FALSE)
  }

  x <- plot_data[[eeo1_share]]
  y <- plot_data[[revelio_share]]
  correlation <- stats::cor(x, y)
  abs_diff <- abs(y - x)
  median_abs_diff <- stats::median(abs_diff)
  p95_abs_diff <- as.numeric(stats::quantile(abs_diff, 0.95, names = FALSE))
  axis_max <- max(c(x, y), na.rm = TRUE) * 1.12
  axis_max <- max(axis_max, 0.05)

  png(output_file, width = 1150, height = 719, res = 120)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)

  par(mar = c(5.2, 5.3, 4.3, 1.2))
  plot(
    x,
    y,
    xlim = c(0, axis_max),
    ylim = c(0, axis_max),
    pch = 19,
    col = "#1f4e79",
    cex = 1.05,
    xlab = paste0("EEO-1 Black Employment Share, ", eeo1_label),
    ylab = paste0("Revelio Black Employment Share, ", revelio_label),
    main = paste0("Industry Black Share: EEO-1 vs Revelio (", revelio_label, ")")
  )
  abline(0, 1, col = "gray55", lty = 2, lwd = 1.4)
  text(
    x,
    y,
    labels = plot_data$aer_naics2_name,
    pos = 4,
    cex = 0.55,
    col = "gray20",
    xpd = NA
  )
  legend(
    "topleft",
    legend = c(
      "45-degree line",
      paste0("Correlation = ", sprintf("%.2f", correlation)),
      paste0("|Delta| median = ", sprintf("%.3f", median_abs_diff)),
      paste0("|Delta| p95 = ", sprintf("%.3f", p95_abs_diff))
    ),
    lty = c(2, NA, NA, NA),
    lwd = c(1.4, NA, NA, NA),
    pch = c(NA, NA, NA, NA),
    col = c("gray55", "black", "black", "black"),
    bty = "n",
    cex = 0.85
  )

  invisible(data.frame(
    plot = basename(output_file),
    revelio_share = revelio_share,
    eeo1_share = eeo1_share,
    n_industries = nrow(plot_data),
    correlation = correlation,
    median_abs_diff = median_abs_diff,
    p95_abs_diff = p95_abs_diff,
    stringsAsFactors = FALSE
  ))
}

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

black_scatter_configs <- list(
  list(
    revelio_share = "revelio_entry_black_share",
    revelio_label = "Entry Level",
    eeo1_share = "eeo1_black_all_jobs_share",
    eeo1_label = "All Jobs",
    output_file = "scatter_eeo1_black_all_jobs_revelio_entry_black_share.png"
  ),
  list(
    revelio_share = "revelio_entry_black_share",
    revelio_label = "Entry Level",
    eeo1_share = "eeo1_black_mid_off_manager_share",
    eeo1_label = "First/Mid-Level Officials and Managers",
    output_file = "scatter_eeo1_black_mid_off_manager_revelio_entry_black_share.png"
  ),
  list(
    revelio_share = "revelio_entry_black_share",
    revelio_label = "Entry Level",
    eeo1_share = "eeo1_black_front_line_share",
    eeo1_label = "Front-Line Jobs",
    output_file = "scatter_eeo1_black_front_line_revelio_entry_black_share.png"
  ),
  list(
    revelio_share = "revelio_all_black_share",
    revelio_label = "All Workforce",
    eeo1_share = "eeo1_black_all_jobs_share",
    eeo1_label = "All Jobs",
    output_file = "scatter_eeo1_black_all_jobs_revelio_all_black_share.png"
  ),
  list(
    revelio_share = "revelio_all_black_share",
    revelio_label = "All Workforce",
    eeo1_share = "eeo1_black_mid_off_manager_share",
    eeo1_label = "First/Mid-Level Officials and Managers",
    output_file = "scatter_eeo1_black_mid_off_manager_revelio_all_black_share.png"
  ),
  list(
    revelio_share = "revelio_all_black_share",
    revelio_label = "All Workforce",
    eeo1_share = "eeo1_black_front_line_share",
    eeo1_label = "Front-Line Jobs",
    output_file = "scatter_eeo1_black_front_line_revelio_all_black_share.png"
  )
)

black_scatter_summaries <- do.call(rbind, lapply(black_scatter_configs, function(config) {
  plot_black_share_scatter(
    data = revelio_eeo1_industry_black_share_data,
    revelio_share = config$revelio_share,
    eeo1_share = config$eeo1_share,
    revelio_label = config$revelio_label,
    eeo1_label = config$eeo1_label,
    output_file = file.path(figures_dir, config$output_file)
  )
}))

write.csv(
  revelio_eeo1_firm_share_data,
  revelio_eeo1_firm_share_output_file,
  row.names = FALSE
)

message(
  "Built Revelio x EEO-1 share dataset: ",
  nrow(revelio_eeo1_firm_share_data),
  " firm rows."
)
message(
  "Matched ",
  sum(revelio_eeo1_firm_share_data$matched_to_revelio),
  " of ",
  nrow(revelio_eeo1_firm_share_data),
  " survey firms to the Revelio entry/all file."
)
message("Wrote firm-level comparison CSV: ", revelio_eeo1_firm_share_output_file)
message(
  "Pearson correlations between firm Revelio shares and matched EEO-1 ",
  "industry shares:"
)
print(revelio_eeo1_share_correlations)
message("Wrote industry-level black-share scatterplots to: ", figures_dir)
print(black_scatter_summaries)
