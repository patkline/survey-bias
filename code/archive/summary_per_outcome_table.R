# -------------------------------------------------------------------
# Per-outcome firm-level summary table.
# For each outcome, writes:
#     summary_table_<outcome>.csv / .tex
# Columns:
#     firm | N | Item Worth (<model>) | Item Worth (<model> EB) |
#     Avg Rating (sd) | Win Share
# `model` is parameterized (default "OLS" for the current Borda+OLS
# pipeline); pass "PL" to display Plackett--Luce coefficients when
# those become available again in the parquet sheets.
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))

write_per_outcome_summary <- function(dir_path,
                                      outcomes,
                                      tables_dir,
                                      label_mapping = NULL,
                                      model = "OLS",
                                      ws_type = NULL) {
  df_coef_raw <- tryCatch(read_parquet_sheet(dir_path, "Coefficients"),
                          error = function(e) NULL)
  if (is.null(df_coef_raw)) {
    stop("Could not read 'Coefficients' sheet from: ", dir_path)
  }

  df_coef <- to_wide_coef(df_coef_raw, model, value_col = "estimate")
  if (is.null(df_coef)) {
    message("⚠️  Model '", model, "' not found in Coefficients sheet at: ",
            dir_path, " — skipping per-outcome summary tables.")
    return(invisible(NULL))
  }
  df_eb <- to_wide_coef(df_coef_raw, model, value_col = "eb")

  df_avg <- tryCatch(read_parquet_sheet(dir_path, "Average Ratings"),
                     error = function(e) NULL)
  df_n   <- tryCatch(read_parquet_sheet(dir_path, "Ratings Observations"),
                     error = function(e) NULL)

  df_ws_combined <- tryCatch(read_parquet_sheet(dir_path, "Win_Share"),
                             error = function(e) NULL)
  if (!is.null(df_ws_combined) && !is.null(ws_type) &&
      ("type" %in% names(df_ws_combined))) {
    df_ws_combined <- dplyr::filter(df_ws_combined, .data$type == ws_type)
  }
  suffix <- if (!is.null(ws_type) && nzchar(ws_type)) paste0("_", ws_type) else ""
  cap_ws <- if (!is.null(ws_type) && nzchar(ws_type)) paste("(WS:", ws_type, ")") else ""

  for (new_outcome in outcomes) {
    cols_needed <- c("firm", "firm_id", new_outcome)

    if (!all(cols_needed %in% names(df_coef))) {
      message("Skipping outcome ", new_outcome,
              ": not found in Coefficients sheet for model '", model, "'.")
      next
    }
    coeff_df <- df_coef[, cols_needed]
    names(coeff_df) <- c("firm", "firm_id", "Estimate")

    if (!is.null(df_eb) && all(cols_needed %in% names(df_eb))) {
      eb_df <- df_eb[, cols_needed]
      names(eb_df) <- c("firm", "firm_id", "Estimate_eb")
    } else {
      eb_df <- coeff_df %>%
        dplyr::transmute(firm, firm_id, Estimate_eb = NA_real_)
    }

    if (!is.null(df_avg) && all(cols_needed %in% names(df_avg))) {
      avg_df <- df_avg[, cols_needed]
      names(avg_df) <- c("firm", "firm_id", "avg_rating")
    } else {
      avg_df <- coeff_df %>%
        dplyr::transmute(firm, firm_id, avg_rating = NA_real_)
    }

    if (!is.null(df_n) && all(cols_needed %in% names(df_n))) {
      nobs_df <- df_n[, cols_needed]
      names(nobs_df) <- c("firm", "firm_id", "num_observations")
    } else {
      nobs_df <- coeff_df %>%
        dplyr::transmute(firm, firm_id, num_observations = NA_integer_)
    }

    merged_df <- coeff_df %>%
      dplyr::left_join(eb_df,   by = c("firm", "firm_id")) %>%
      dplyr::left_join(avg_df,  by = c("firm", "firm_id")) %>%
      dplyr::left_join(nobs_df, by = c("firm", "firm_id"))

    # Win share (per-outcome sheet preferred, else combined)
    df_ws_outcome <- tryCatch(
      read_parquet_sheet(dir_path, paste0("Win_Share_", new_outcome)),
      error = function(e) NULL)
    ws_df <- if (!is.null(df_ws_outcome)) df_ws_outcome
             else if (!is.null(df_ws_combined) && "outcome" %in% names(df_ws_combined))
               dplyr::filter(df_ws_combined, .data$outcome == !!new_outcome)
             else NULL

    if (!is.null(ws_df) && new_outcome %in% names(ws_df)) {
      ws_df2 <- ws_df %>%
        dplyr::select(firm, firm_id, !!rlang::sym(new_outcome)) %>%
        dplyr::rename(win_share = !!rlang::sym(new_outcome))
      merged_df <- merged_df %>%
        dplyr::left_join(ws_df2, by = c("firm", "firm_id")) %>%
        dplyr::mutate(win_share = suppressWarnings(as.numeric(win_share)))
    } else {
      merged_df$win_share <- NA_real_
    }

    if (!"std_dev" %in% names(merged_df)) merged_df$std_dev <- NA_real_

    iw_label    <- paste0("Item Worth (", model, ")")
    iw_eb_label <- paste0("Item Worth (", model, " EB)")
    summary_table <- merged_df %>%
      dplyr::mutate(
        N                 = as.integer(num_observations),
        !!iw_label       := sprintf("%.2f", Estimate),
        !!iw_eb_label    := sprintf("%.2f", Estimate_eb),
        `Avg Rating`      = ifelse(is.na(std_dev),
                                   sprintf("%.2f", avg_rating),
                                   sprintf("%.2f (%.2f)", avg_rating, std_dev)),
        `Win Share`       = ifelse(!is.na(win_share),
                                   sprintf("%.3f", win_share),
                                   "")
      ) %>%
      dplyr::select(firm, N,
                    !!rlang::sym(iw_label),
                    !!rlang::sym(iw_eb_label),
                    `Avg Rating`, `Win Share`)

    outcome_label <- if (!is.null(label_mapping) &&
                         new_outcome %in% names(label_mapping)) {
      label_mapping[[new_outcome]]
    } else new_outcome

    table_file_tex <- file.path(tables_dir,
      paste0("summary_table_", new_outcome, "_", tolower(model), suffix, ".tex"))
    print(xtable::xtable(summary_table,
                         caption = paste("Summary Table -", outcome_label, cap_ws)),
          file = table_file_tex, include.rownames = FALSE)

    table_file_csv <- file.path(tables_dir,
      paste0("summary_table_", new_outcome, "_", tolower(model), suffix, ".csv"))
    utils::write.csv(summary_table, table_file_csv, row.names = FALSE)

    cat("✅ Tables saved:", basename(table_file_tex),
        "and", basename(table_file_csv), "\n")
  }
}

for (m in setdiff(models, "Borda")) {
  write_per_outcome_summary(
    dir_path      = dir_path,
    outcomes      = outs,
    tables_dir    = tables,
    label_mapping = label_mapping,
    model         = m
  )
}
