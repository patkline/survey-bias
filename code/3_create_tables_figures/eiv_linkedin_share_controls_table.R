# ------------------------------------------------------------------------------
# Purpose: EIV contact-gap table with Yimfor LinkedIn workforce-share controls
#
# This replaces the earlier EEO-1 NAICS3 share-control table.
# ------------------------------------------------------------------------------

linkedin_eiv_full_sample_dir <- file.path(intermediate, "Full_Sample")
linkedin_eiv_firm <- read_parquet_sheet(
  linkedin_eiv_full_sample_dir,
  "EIV_firm"
)
linkedin_eiv_share_controls <- read_parquet_sheet(
  linkedin_eiv_full_sample_dir,
  "EIV_linkedin_shares"
)

linkedin_eiv_columns <- tibble::tribble(
  ~model, ~specification,
  "OLS", "baseline",
  "OLS", "linkedin_share",
  "Borda", "baseline",
  "Borda", "linkedin_share"
)

linkedin_eiv_pull_result <- function(data, lhs, formula, rhs, model) {
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

linkedin_eiv_column_result <- function(
    lhs,
    belief_rhs,
    share_rhs,
    model,
    specification
) {
  if (specification == "baseline") {
    data <- linkedin_eiv_firm
    formula <- belief_rhs
    control_rhs <- NA_character_
  } else if (specification == "linkedin_share") {
    data <- linkedin_eiv_share_controls
    formula <- paste(belief_rhs, share_rhs, sep = " + ")
    control_rhs <- share_rhs
  } else {
    stop("Unknown EIV table specification: ", specification)
  }

  list(
    belief = linkedin_eiv_pull_result(
      data = data,
      lhs = lhs,
      formula = formula,
      rhs = belief_rhs,
      model = model
    ),
    share = if (is.na(control_rhs)) {
      NULL
    } else {
      linkedin_eiv_pull_result(
        data = data,
        lhs = lhs,
        formula = formula,
        rhs = control_rhs,
        model = model
      )
    }
  )
}

linkedin_eiv_fmt_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

linkedin_eiv_fmt_standard_error <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", as.numeric(x)), ")"))
}

# LinkedIn workforce shares are coded from zero to one. Report their EIV
# coefficients and standard errors as the effect of a 10-percentage-point
# increase
linkedin_eiv_scaling <- 0.1

linkedin_eiv_panel_lines <- function(panel_label, lhs, belief_rhs, share_rhs,
                                     share_label) {
  results <- lapply(seq_len(nrow(linkedin_eiv_columns)), function(column_index) {
    linkedin_eiv_column_result(
      lhs = lhs,
      belief_rhs = belief_rhs,
      share_rhs = share_rhs,
      model = linkedin_eiv_columns$model[column_index],
      specification = linkedin_eiv_columns$specification[column_index]
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
  share_estimates <- vapply(results, function(result) {
    if (is.null(result$share)) NA_real_ else as.numeric(result$share$sample_est)
  }, numeric(1L)) * linkedin_eiv_scaling
  share_standard_errors <- vapply(results, function(result) {
    if (is.null(result$share)) NA_real_ else as.numeric(result$share$sample_se)
  }, numeric(1L)) * linkedin_eiv_scaling

  c(
    paste0("\\multicolumn{5}{l}{\\textbf{", panel_label, "}} \\\\"),
    paste0(
      paste(
        c("Discrimination Beliefs", linkedin_eiv_fmt_estimate(belief_estimates)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c("", linkedin_eiv_fmt_standard_error(belief_standard_errors)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c(share_label, linkedin_eiv_fmt_estimate(share_estimates)),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(
      paste(
        c("", linkedin_eiv_fmt_standard_error(share_standard_errors)),
        collapse = " & "
      ),
      " \\\\"
    )
  )
}

linkedin_eiv_latex_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Likert} & \\multicolumn{2}{c}{Borda} \\\\ ",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4) \\\\ ",
  "\\midrule",
  linkedin_eiv_panel_lines(
    panel_label = "Panel A: Race",
    lhs = "log_dif",
    belief_rhs = "pooled_favor_white",
    share_rhs = "analysis_share_black",
    share_label = "Black Share"
  ),
  "\\addlinespace",
  linkedin_eiv_panel_lines(
    panel_label = "Panel B: Gender",
    lhs = "log_dif_gender",
    belief_rhs = "pooled_favor_male",
    share_rhs = "analysis_share_female",
    share_label = "Female Share"
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

write_lines_checked(
  linkedin_eiv_latex_lines,
  file.path(tables, "EIV_linkedin_full_sample_pooled_share_controls.tex"),
  label = "Full-sample Yimfor LinkedIn share-control EIV table"
)

message("🎃 Generated EIV_linkedin_full_sample_pooled_share_controls.tex")
