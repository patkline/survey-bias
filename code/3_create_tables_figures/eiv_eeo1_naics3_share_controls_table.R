# ------------------------------------------------------------------------------
# Purpose: EIV contact-gap table with EEO-1 NAICS3 share controls
# ------------------------------------------------------------------------------

naics3_eiv_full_sample_dir <- file.path(intermediate, "Full_Sample")
naics3_eiv_firm <- read_parquet_sheet(
  naics3_eiv_full_sample_dir,
  "EIV_firm"
)
naics3_eiv_share_controls <- read_parquet_sheet(
  naics3_eiv_full_sample_dir,
  "EIV_eeo1_naics3_shares"
)

naics3_eiv_columns <- tibble::tribble(
  ~model, ~specification,
  "OLS", "baseline",
  "OLS", "front_line",
  "OLS", "all_jobs",
  "Borda", "baseline",
  "Borda", "front_line",
  "Borda", "all_jobs"
)

naics3_eiv_pull_result <- function(data, lhs, formula, rhs, model) {
  result <- data |>
    dplyr::mutate(coef = suppressWarnings(as.integer(.data$coef))) |>
    dplyr::filter(
      .data$lhs == .env$lhs,
      .data$formula == .env$formula,
      .data$rhs == .env$rhs,
      .data$model == .env$model,
      .data$coef == 1L
    )

  if (nrow(result) != 1L) {
    stop(
      "Could not identify a unique EIV result for ",
      model, ": ", lhs, " ~ ", formula, "; coefficient ", rhs
    )
  }

  result
}

naics3_eiv_column_result <- function(lhs, belief_rhs, front_line_share_rhs,
                                     all_jobs_share_rhs, model,
                                     specification) {
  if (specification == "baseline") {
    data <- naics3_eiv_firm
    formula <- belief_rhs
    share_rhs <- NA_character_
  } else if (specification == "front_line") {
    data <- naics3_eiv_share_controls
    formula <- paste(belief_rhs, front_line_share_rhs, sep = " + ")
    share_rhs <- front_line_share_rhs
  } else if (specification == "all_jobs") {
    data <- naics3_eiv_share_controls
    formula <- paste(belief_rhs, all_jobs_share_rhs, sep = " + ")
    share_rhs <- all_jobs_share_rhs
  } else {
    stop("Unknown EIV table specification: ", specification)
  }

  list(
    belief = naics3_eiv_pull_result(
      data = data,
      lhs = lhs,
      formula = formula,
      rhs = belief_rhs,
      model = model
    ),
    share = if (is.na(share_rhs)) {
      NULL
    } else {
      naics3_eiv_pull_result(
        data = data,
        lhs = lhs,
        formula = formula,
        rhs = share_rhs,
        model = model
      )
    }
  )
}

naics3_eiv_fmt_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

naics3_eiv_fmt_standard_error <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", as.numeric(x)), ")"))
}

naics3_eiv_panel_lines <- function(panel_label, lhs, belief_rhs,
                                   front_line_share_rhs,
                                   all_jobs_share_rhs,
                                   front_line_label, all_jobs_label) {
  results <- lapply(seq_len(nrow(naics3_eiv_columns)), function(column_index) {
    naics3_eiv_column_result(
      lhs = lhs,
      belief_rhs = belief_rhs,
      front_line_share_rhs = front_line_share_rhs,
      all_jobs_share_rhs = all_jobs_share_rhs,
      model = naics3_eiv_columns$model[column_index],
      specification = naics3_eiv_columns$specification[column_index]
    )
  })

  belief_estimates <- vapply(
    results,
    function(result) as.numeric(result$belief$sample_est),
    numeric(1L)
  )
  belief_standard_errors <- vapply(
    results,
    function(result) as.numeric(result$belief$sample_se),
    numeric(1L)
  )
  front_line_estimates <- vapply(seq_along(results), function(column_index) {
    if (naics3_eiv_columns$specification[column_index] == "front_line") {
      as.numeric(results[[column_index]]$share$sample_est)
    } else {
      NA_real_
    }
  }, numeric(1L))
  front_line_standard_errors <- vapply(seq_along(results), function(column_index) {
    if (naics3_eiv_columns$specification[column_index] == "front_line") {
      as.numeric(results[[column_index]]$share$sample_se)
    } else {
      NA_real_
    }
  }, numeric(1L))
  all_jobs_estimates <- vapply(seq_along(results), function(column_index) {
    if (naics3_eiv_columns$specification[column_index] == "all_jobs") {
      as.numeric(results[[column_index]]$share$sample_est)
    } else {
      NA_real_
    }
  }, numeric(1L))
  all_jobs_standard_errors <- vapply(seq_along(results), function(column_index) {
    if (naics3_eiv_columns$specification[column_index] == "all_jobs") {
      as.numeric(results[[column_index]]$share$sample_se)
    } else {
      NA_real_
    }
  }, numeric(1L))
  firm_counts <- vapply(
    results,
    function(result) as.integer(result$belief$n),
    integer(1L)
  )

  c(
    paste0("\\multicolumn{7}{l}{\\textbf{", panel_label, "}} \\\\"),
    paste0(
      paste(
        c("Discrimination beliefs", naics3_eiv_fmt_estimate(belief_estimates)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c("", naics3_eiv_fmt_standard_error(belief_standard_errors)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c(front_line_label, naics3_eiv_fmt_estimate(front_line_estimates)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c("", naics3_eiv_fmt_standard_error(front_line_standard_errors)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c(all_jobs_label, naics3_eiv_fmt_estimate(all_jobs_estimates)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c("", naics3_eiv_fmt_standard_error(all_jobs_standard_errors)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(paste(c("Firms", firm_counts), collapse = " & "), " \\\\ ")
  )
}

naics3_eiv_latex_lines <- c(
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Likert} & \\multicolumn{3}{c}{Borda} \\\\ ",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & No share & Front-line share & All-jobs share & No share & Front-line share & All-jobs share \\\\ ",
  "\\midrule",
  naics3_eiv_panel_lines(
    panel_label = "Panel A: Race",
    lhs = "log_dif",
    belief_rhs = "pooled_favor_white",
    front_line_share_rhs = "eeo1_black_front_line_share",
    all_jobs_share_rhs = "eeo1_black_all_jobs_share",
    front_line_label = "Black share: front-line jobs",
    all_jobs_label = "Black share: all jobs"
  ),
  "\\addlinespace",
  naics3_eiv_panel_lines(
    panel_label = "Panel B: Gender",
    lhs = "log_dif_gender",
    belief_rhs = "pooled_favor_male",
    front_line_share_rhs = "eeo1_female_front_line_share",
    all_jobs_share_rhs = "eeo1_female_all_jobs_share",
    front_line_label = "Female share: front-line jobs",
    all_jobs_label = "Female share: all jobs"
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

write_lines_checked(
  naics3_eiv_latex_lines,
  file.path(tables, "EIV_eeo1_naics3_full_sample_pooled_share_controls.tex"),
  label = "Full-sample EEO-1 NAICS3 share-control EIV table"
)

message("🎃 Generated EIV_eeo1_naics3_full_sample_pooled_share_controls.tex")
