# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Standalone table of the overall mean and (Katz bias-corrected) signal standard deviation of the
# three key discrimination measures --- pooled_favor_white (Race), pooled_favor_male (Gender), and
# conduct_favor_younger (Age) --- for the Full Sample and for each of the demographic subgroups used in the
# cross-sample signal correlation table (Table 4), in both Likert and Borda units. One row per subgroup,
# grouped by comparison pair. Wide (16 columns) --- intended for a landscape page.
#
# Created: Nico Rotundo 2026-07-20
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Shared outcome config: fmt_dec(), map_label()
source(file.path(create_tables_figures, "summary_outcomes_config.R"))

# -----------------------------------------------------------------------------------------------------------------------------
# Small formatting helpers (duplicated locally rather than sourcing summary_variance_table.R, which has
# top-level side effects; this keeps the script runnable on its own)
# -----------------------------------------------------------------------------------------------------------------------------
latex_escape_text <- function(x) {
  out <- as.character(x)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    out <- gsub(ch, paste0("\\", ch), out, fixed = TRUE)
  }
  out
}

format_count <- function(x) {
  z <- suppressWarnings(as.integer(x))
  out <- rep("", length(z))
  ok <- !is.na(z)
  out[ok] <- formatC(z[ok], format = "d", big.mark = ",")
  out
}

# Build grouped LaTeX rows: the first cell of each group is a bold group header
# row, each member row's first cell is indented, and a small vertical gap
# follows every group except the last. Generic over any row-key vector
# (outcomes, subgroup names, ...).
grouped_summary_table_rows <- function(row_keys,
                                       display_labels,
                                       formatted_data,
                                       row_groups,
                                       group_gap = "0.15em") {
  grouped_keys <- unname(unlist(row_groups, use.names = FALSE))
  missing_keys <- setdiff(grouped_keys, row_keys)
  extra_keys   <- setdiff(row_keys, grouped_keys)
  if (length(missing_keys) || length(extra_keys)) {
    stop(
      "Grouped summary-table row keys do not match table rows. Missing: ",
      paste(missing_keys, collapse = ", "),
      "; extra: ", paste(extra_keys, collapse = ", ")
    )
  }

  lines <- character(0)
  group_names <- names(row_groups)
  for (g in seq_along(row_groups)) {
    group_keys <- row_groups[[g]]
    idx <- match(group_keys, row_keys)
    lines <- c(lines, paste0("\\textbf{", group_names[g], "} \\\\"))
    for (j in seq_along(idx)) {
      i <- idx[j]
      cells <- c(
        paste0("\\quad ", latex_escape_text(display_labels[i])),
        as.character(formatted_data[i, , drop = TRUE])
      )
      row_end <- if (j == length(idx) && g < length(row_groups)) {
        paste0(" \\\\[", group_gap, "]")
      } else {
        " \\\\"
      }
      lines <- c(lines, paste0(paste(cells, collapse = " & "), row_end))
    }
  }
  lines
}

# -----------------------------------------------------------------------------------------------------------------------------
# Define the subgroups --- same 18 demographic subsamples (9 comparison pairs) used by the cross-sample
# signal correlation table (Table 4) --- and their display labels / pair groupings. The Full Sample row is
# intentionally omitted; it is already reported elsewhere.
# -----------------------------------------------------------------------------------------------------------------------------
sample_definition_table <- data.frame(
  sample = c(
    "Black", "White",
    "Female", "Male",
    "Looking", "Not_Looking",
    "Feared_Discrimination_1", "Feared_Discrimination_0",
    "Age_gte40", "Age_lt40",
    "College", "No_College",
    "Convenience", "Probability",
    "Conf_Gender_Y", "Conf_Gender_N",
    "Conf_Race_Y", "Conf_Race_N"
  ),
  display_label = c(
    "Black", "White",
    "Female", "Male",
    "Looking for a Job", "Not Looking for a Job",
    "Feared Discrimination", "Did Not Fear Discrimination",
    "Age \\(\\geq\\) 40", "Age \\(<\\) 40",
    "At Least Some College", "HS Diploma or Less",
    "Convenience Sample", "Probability Sample",
    "Confident (Gender)", "Not Confident (Gender)",
    "Confident (Race)", "Not Confident (Race)"
  ),
  # Intermediate output directory holding this subgroup's Coefficients/variance sheets
  output_dir_name = c(
    "Subset_Black", "Subset_White",
    "Subset_Female", "Subset_Male",
    "Subset_Looking", "Subset_Not_Looking",
    "Subset_Feared_Discrimination_1", "Subset_Feared_Discrimination_0",
    "Subset_Age_gte40", "Subset_Age_lt40",
    "Subset_College", "Subset_No_College",
    "Subset_Convenience", "Subset_Probability",
    "Subset_Conf_Gender_Y", "Subset_Conf_Gender_N",
    "Subset_Conf_Race_Y", "Subset_Conf_Race_N"
  ),
  stringsAsFactors = FALSE
)

# Should be 18 rows (9 comparison pairs), none missing, no duplicated sample names
stopifnot(nrow(sample_definition_table) == 18, !anyDuplicated(sample_definition_table$sample), !anyNA(sample_definition_table))

# Row grouping, mirroring the comparison pairs in cross_sample_signal_corr.R's sample_pair_list
subgroup_row_groups <- list(
  Race                    = c("Black", "White"),
  Gender                  = c("Female", "Male"),
  `Job Search`            = c("Looking", "Not_Looking"),
  `Feared Discrimination` = c("Feared_Discrimination_1", "Feared_Discrimination_0"),
  Age                     = c("Age_gte40", "Age_lt40"),
  Education               = c("College", "No_College"),
  `Sample Type`           = c("Convenience", "Probability"),
  `Confidence (Gender)`   = c("Conf_Gender_Y", "Conf_Gender_N"),
  `Confidence (Race)`     = c("Conf_Race_Y", "Conf_Race_N")
)

# Every sample should appear in exactly one row group
stopifnot(setequal(unlist(subgroup_row_groups, use.names = FALSE), sample_definition_table$sample))

# -----------------------------------------------------------------------------------------------------------------------------
# The three key discrimination measures
# -----------------------------------------------------------------------------------------------------------------------------
key_outcomes <- c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger")

outcome_display_names <- c(
  pooled_favor_white    = "Race: Discrimination Black (Pooled)",
  pooled_favor_male     = "Gender: Discrimination Female (Pooled)",
  conduct_favor_younger = "Age: Discrimination Older (Conduct)"
)

# -----------------------------------------------------------------------------------------------------------------------------
# Per-subgroup mean + signal SD, in both Likert (OLS) and Borda units
# -----------------------------------------------------------------------------------------------------------------------------
# Prefer the raw (not-recentered) firm-level estimates when both exist, matching build_belief_summary_ols_borda_data()
# in summary_variance_table.R; the variance/signal sheet only ever stores the recentered OLS/Borda model rows, since
# signal variance is identical across the recentered and non-recentered estimates.
model_preference <- data.frame(
  model = c("OLS_not_recentered", "OLS", "Borda_not_recentered", "Borda"),
  table_model = c("Likert", "Likert", "Borda", "Borda"),
  preference = c(1L, 2L, 1L, 2L),
  stringsAsFactors = FALSE
)

# Compute one subgroup's mean + signal SD for the three key outcomes, in both units
compute_subgroup_stats <- function(dir_path, outcomes) {
  coef_df <- read_parquet_sheet(dir_path, "Coefficients")

  required_coef_cols <- c("subset", "model", "outcome", "entity_type", "estimate", "total_number_of_respondents")
  missing_coef_cols <- setdiff(required_coef_cols, names(coef_df))
  if (length(missing_coef_cols)) {
    stop("Coefficients sheet at ", dir_path, " missing columns: ", paste(missing_coef_cols, collapse = ", "))
  }

  mean_stats <- coef_df %>%
    dplyr::inner_join(model_preference, by = "model") %>%
    dplyr::filter(
      .data$subset == "all",
      .data$entity_type == "Firm",
      .data$outcome %in% outcomes
    ) %>%
    dplyr::group_by(.data$outcome, .data$table_model) %>%
    dplyr::filter(.data$preference == min(.data$preference, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(estimate = suppressWarnings(as.numeric(.data$estimate))) %>%
    dplyr::group_by(.data$outcome, .data$table_model) %>%
    dplyr::summarise(
      mean        = mean(.data$estimate, na.rm = TRUE),
      respondents = dplyr::first(.data$total_number_of_respondents),
      n_firms     = dplyr::n(),
      .groups = "drop"
    )

  # Every (outcome, table_model) cell should be built from all 164 rated firms
  stopifnot(all(mean_stats$n_firms == 164))

  var_df <- read_parquet_sheet(dir_path, "variance")

  required_var_cols <- c("subset", "model", "outcome", "signal")
  missing_var_cols <- setdiff(required_var_cols, names(var_df))
  if (length(missing_var_cols)) {
    stop("variance sheet at ", dir_path, " missing columns: ", paste(missing_var_cols, collapse = ", "))
  }

  signal_stats <- var_df %>%
    dplyr::filter(
      .data$subset == "all",
      .data$model %in% c("OLS", "Borda"),
      .data$outcome %in% outcomes
    ) %>%
    dplyr::mutate(
      table_model = ifelse(.data$model == "OLS", "Likert", "Borda"),
      signal_sd   = sqrt(pmax(suppressWarnings(as.numeric(.data$signal)), 0))
    ) %>%
    dplyr::select(outcome, table_model, signal_sd)

  tab <- mean_stats %>%
    dplyr::left_join(signal_stats, by = c("outcome", "table_model"))

  # Every outcome x model cell should have both a mean and a signal SD
  stopifnot(!anyNA(tab$mean), !anyNA(tab$signal_sd))

  tab %>% dplyr::select(outcome, table_model, respondents, mean, signal_sd)
}

# Loop over every subgroup, collecting long-format (sample, outcome, table_model) rows
aggregated_subgroup_stats <- data.frame()

for (i in seq_len(nrow(sample_definition_table))) {
  sample_name <- sample_definition_table$sample[i]
  dir_path_i  <- file.path(intermediate, sample_definition_table$output_dir_name[i])

  message("Computing subgroup mean / signal SD: ", sample_name)

  subgroup_tab <- compute_subgroup_stats(dir_path_i, key_outcomes)
  subgroup_tab$sample <- sample_name

  aggregated_subgroup_stats <- rbind(aggregated_subgroup_stats, subgroup_tab)
}

# Should be 18 subgroups x 3 outcomes x 2 aggregation methods = 108 rows, none missing
stopifnot(nrow(aggregated_subgroup_stats) == 18 * 3 * 2, !anyNA(aggregated_subgroup_stats))

# -----------------------------------------------------------------------------------------------------------------------------
# Reshape to one row per subgroup, columns for each outcome x aggregation method x {mean, signal SD}
# -----------------------------------------------------------------------------------------------------------------------------
subgroup_wide <- aggregated_subgroup_stats %>%
  dplyr::select(-respondents) %>%
  tidyr::pivot_wider(
    names_from  = c(outcome, table_model),
    values_from = c(mean, signal_sd),
    names_glue  = "{.value}_{outcome}_{table_model}"
  )

# Respondent counts differ by outcome --- pooled_favor_white and pooled_favor_male are each coalesced from
# a different pair of question framings (see sample_prep.R), so they are answered by different numbers of
# respondents (e.g. Full Sample: 4,706 for race vs. 6,251 for gender). Report one respondent count per
# outcome rather than a single shared count. Likert and Borda share the same respondent pool per outcome
# (confirmed via total_number_of_respondents), so only one aggregation method needs to be looked up.
respondents_lookup <- aggregated_subgroup_stats %>%
  dplyr::filter(.data$table_model == "Likert") %>%
  dplyr::select(sample, outcome, respondents) %>%
  tidyr::pivot_wider(
    names_from  = outcome,
    values_from = respondents,
    names_glue  = "respondents_{outcome}"
  )

subgroup_wide <- subgroup_wide %>%
  dplyr::left_join(respondents_lookup, by = "sample") %>%
  dplyr::left_join(sample_definition_table %>% dplyr::select(sample, display_label), by = "sample")

# One row per subgroup, none missing
stopifnot(nrow(subgroup_wide) == 18, !anyNA(subgroup_wide))

# -----------------------------------------------------------------------------------------------------------------------------
# Export machine-readable CSV (raw numeric values, unformatted)
# -----------------------------------------------------------------------------------------------------------------------------
csv_out <- subgroup_wide %>%
  dplyr::transmute(
    sample,
    display_label,
    race_respondents         = .data$respondents_pooled_favor_white,
    race_likert_mean         = .data$mean_pooled_favor_white_Likert,
    race_likert_signal_sd    = .data$signal_sd_pooled_favor_white_Likert,
    race_borda_mean          = .data$mean_pooled_favor_white_Borda,
    race_borda_signal_sd     = .data$signal_sd_pooled_favor_white_Borda,
    gender_respondents       = .data$respondents_pooled_favor_male,
    gender_likert_mean       = .data$mean_pooled_favor_male_Likert,
    gender_likert_signal_sd  = .data$signal_sd_pooled_favor_male_Likert,
    gender_borda_mean        = .data$mean_pooled_favor_male_Borda,
    gender_borda_signal_sd   = .data$signal_sd_pooled_favor_male_Borda,
    age_respondents           = .data$respondents_conduct_favor_younger,
    age_likert_mean           = .data$mean_conduct_favor_younger_Likert,
    age_likert_signal_sd      = .data$signal_sd_conduct_favor_younger_Likert,
    age_borda_mean            = .data$mean_conduct_favor_younger_Borda,
    age_borda_signal_sd       = .data$signal_sd_conduct_favor_younger_Borda
  )

write.csv(csv_out, file.path(tables, "subgroup_belief_mean_signal_variance.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------------------------------------------------------
# Build LaTeX table: one row per subgroup, grouped by comparison pair, columns for Race/Gender x Likert/Borda
# -----------------------------------------------------------------------------------------------------------------------------
latex_decimals <- 3

latex_df <- data.frame(
  `Race Respondents`     = format_count(subgroup_wide$respondents_pooled_favor_white),
  `Race Likert Mean`     = fmt_dec(subgroup_wide$mean_pooled_favor_white_Likert, latex_decimals),
  `Race Likert Sig. SD`  = fmt_dec(subgroup_wide$signal_sd_pooled_favor_white_Likert, latex_decimals),
  `Race Borda Mean`      = fmt_dec(subgroup_wide$mean_pooled_favor_white_Borda, latex_decimals),
  `Race Borda Sig. SD`   = fmt_dec(subgroup_wide$signal_sd_pooled_favor_white_Borda, latex_decimals),
  `Gender Respondents`     = format_count(subgroup_wide$respondents_pooled_favor_male),
  `Gender Likert Mean`     = fmt_dec(subgroup_wide$mean_pooled_favor_male_Likert, latex_decimals),
  `Gender Likert Sig. SD`  = fmt_dec(subgroup_wide$signal_sd_pooled_favor_male_Likert, latex_decimals),
  `Gender Borda Mean`      = fmt_dec(subgroup_wide$mean_pooled_favor_male_Borda, latex_decimals),
  `Gender Borda Sig. SD`   = fmt_dec(subgroup_wide$signal_sd_pooled_favor_male_Borda, latex_decimals),
  `Age Respondents`     = format_count(subgroup_wide$respondents_conduct_favor_younger),
  `Age Likert Mean`     = fmt_dec(subgroup_wide$mean_conduct_favor_younger_Likert, latex_decimals),
  `Age Likert Sig. SD`  = fmt_dec(subgroup_wide$signal_sd_conduct_favor_younger_Likert, latex_decimals),
  `Age Borda Mean`      = fmt_dec(subgroup_wide$mean_conduct_favor_younger_Borda, latex_decimals),
  `Age Borda Sig. SD`   = fmt_dec(subgroup_wide$signal_sd_conduct_favor_younger_Borda, latex_decimals),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

body_lines <- grouped_summary_table_rows(
  row_keys       = subgroup_wide$sample,
  display_labels = subgroup_wide$display_label,
  formatted_data = latex_df,
  row_groups     = subgroup_row_groups
)

# Build the column layout generically over key_outcomes, so adding/removing an outcome only
# requires updating key_outcomes / outcome_display_names above --- not hand-recounted column indices.
cols_per_outcome <- 5L  # N resp., Likert Mean, Likert Sig. SD, Borda Mean, Borda Sig. SD
n_outcomes <- length(key_outcomes)

align_str <- paste0("l", strrep("c", cols_per_outcome * n_outcomes))

top_header_parts   <- character(0)
top_cmidrule_parts <- character(0)
sub_header_parts   <- character(0)
sub_cmidrule_parts <- character(0)
col_start <- 2L
for (oc in key_outcomes) {
  col_end <- col_start + cols_per_outcome - 1L
  top_header_parts   <- c(top_header_parts, sprintf("\\multicolumn{%d}{c}{%s}", cols_per_outcome, outcome_display_names[[oc]]))
  top_cmidrule_parts <- c(top_cmidrule_parts, sprintf("\\cmidrule(lr){%d-%d}", col_start, col_end))

  likert_start <- col_start + 1L
  likert_end   <- likert_start + 1L
  borda_start  <- likert_end + 1L
  borda_end    <- borda_start + 1L
  sub_header_parts   <- c(sub_header_parts, "", "\\multicolumn{2}{c}{Likert}", "\\multicolumn{2}{c}{Borda}")
  sub_cmidrule_parts <- c(
    sub_cmidrule_parts,
    sprintf("\\cmidrule(lr){%d-%d}", likert_start, likert_end),
    sprintf("\\cmidrule(lr){%d-%d}", borda_start, borda_end)
  )

  col_start <- col_end + 1L
}

colname_parts <- rep(c("N resp.", "Mean", "\\shortstack{Signal\\\\Std Dev}", "Mean", "\\shortstack{Signal\\\\Std Dev}"), n_outcomes)

# Every latex_df column should map to exactly one of the generated column-name cells
stopifnot(ncol(latex_df) == length(colname_parts), ncol(latex_df) == cols_per_outcome * n_outcomes)

latex_lines <- c(
  sprintf("\\begin{tabular}{%s}", align_str),
  "  \\toprule",
  paste0(" & ", paste(top_header_parts, collapse = " & "), " \\\\"),
  paste(top_cmidrule_parts, collapse = " "),
  paste0(" & ", paste(sub_header_parts, collapse = " & "), " \\\\"),
  paste(sub_cmidrule_parts, collapse = " "),
  paste0("Subgroup & ", paste(colname_parts, collapse = " & "), " \\\\"),
  "\\midrule",
  body_lines,
  "   \\bottomrule",
  "\\end{tabular}"
)

tex_out_path <- file.path(tables, "subgroup_belief_mean_signal_variance.tex")
write_lines_checked(latex_lines, tex_out_path, label = "subgroup belief mean / signal SD LaTeX")

cat("Subgroup belief mean / signal SD table saved:", basename(tex_out_path), "\n")
cat("Subgroup belief mean / signal SD CSV saved:", "subgroup_belief_mean_signal_variance.csv", "\n")
