# ------------------------------------------------------------------------------
# Purpose: Firm-level histograms of pooled discrimination Likert scores
# ------------------------------------------------------------------------------
source(file.path("code", "3_create_tables_figures", "summary_outcomes_config.R"))

histogram_outcomes <- c(
  pooled_favor_white = "Discrimination Black (Pooled)",
  pooled_favor_male  = "Discrimination Gender (Pooled)"
)

# Use the same preferred Likert specification as
# belief_mean_signal_sd_ols_borda.tex: non-recentered OLS when available.
coef_df <- read_parquet_sheet(dir_path, "Coefficients")
variance_df <- read_parquet_sheet(dir_path, "variance")

required_coef_cols <- c(
  "subset", "model", "outcome", "entity_type", "entity_id", "estimate"
)
missing_coef_cols <- setdiff(required_coef_cols, names(coef_df))
if (length(missing_coef_cols)) {
  stop("Coefficients sheet missing columns: ", paste(missing_coef_cols, collapse = ", "))
}

required_variance_cols <- c("subset", "model", "outcome", "variance", "signal")
missing_variance_cols <- setdiff(required_variance_cols, names(variance_df))
if (length(missing_variance_cols)) {
  stop("variance sheet missing columns: ", paste(missing_variance_cols, collapse = ", "))
}

likert_model_preference <- data.frame(
  model = c("OLS_not_recentered", "OLS"),
  preference = c(1L, 2L),
  stringsAsFactors = FALSE
)

firm_likert_scores <- coef_df |>
  dplyr::inner_join(likert_model_preference, by = "model") |>
  dplyr::filter(
    .data$subset == "all",
    tolower(as.character(.data$entity_type)) == "firm",
    .data$outcome %in% names(histogram_outcomes)
  ) |>
  dplyr::group_by(.data$outcome) |>
  dplyr::filter(.data$preference == min(.data$preference, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    outcome = as.character(.data$outcome),
    entity_id = as.integer(.data$entity_id),
    average_likert_score = suppressWarnings(as.numeric(.data$estimate))
  )

if (anyDuplicated(firm_likert_scores[c("outcome", "entity_id")])) {
  stop("Firm Likert scores are not unique by outcome and entity_id")
}
if (any(!is.finite(firm_likert_scores$average_likert_score))) {
  stop("Firm Likert scores contain missing or non-finite values")
}
if (!setequal(unique(firm_likert_scores$outcome), names(histogram_outcomes))) {
  stop("One or more pooled discrimination outcomes lack firm-level Likert scores")
}

# Sample and signal standard deviations come directly from the same variance
# rows used by belief_mean_signal_sd_ols_borda.tex. The variance model is the
# centered OLS parameterization; recentering does not alter either SD.
histogram_stats <- variance_df |>
  dplyr::filter(
    .data$subset == "all",
    .data$model == "OLS",
    .data$outcome %in% names(histogram_outcomes)
  ) |>
  dplyr::transmute(
    outcome = as.character(.data$outcome),
    sample_sd = sqrt(pmax(suppressWarnings(as.numeric(.data$variance)), 0)),
    signal_sd = sqrt(pmax(suppressWarnings(as.numeric(.data$signal)), 0))
  )

if (anyDuplicated(histogram_stats$outcome) ||
    !setequal(histogram_stats$outcome, names(histogram_outcomes)) ||
    any(!is.finite(histogram_stats$sample_sd)) ||
    any(!is.finite(histogram_stats$signal_sd))) {
  stop("Expected one finite OLS variance row for each pooled discrimination outcome")
}

histogram_stats <- firm_likert_scores |>
  dplyr::group_by(.data$outcome) |>
  dplyr::summarise(mean = mean(.data$average_likert_score), .groups = "drop") |>
  dplyr::left_join(histogram_stats, by = "outcome")

histogram_file_stems <- c(
  pooled_favor_white = "firm_average_likert_histogram_discrimination_black_pooled",
  pooled_favor_male  = "firm_average_likert_histogram_discrimination_gender_pooled"
)

for (outcome_name in names(histogram_outcomes)) {
  plot_data <- dplyr::filter(firm_likert_scores, .data$outcome == outcome_name)
  plot_stats <- dplyr::filter(histogram_stats, .data$outcome == outcome_name)

  annotation_text <- sprintf(
    "Mean: %.3f\nSample Std Dev: %.3f\nSignal Std Dev: %.3f",
    plot_stats$mean,
    plot_stats$sample_sd,
    plot_stats$signal_sd
  )

  firm_average_likert_histogram <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$average_likert_score)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
      binwidth = 0.1,
      boundary = 0,
      closed = "left",
      fill = "steelblue",
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = Inf,
      label = annotation_text,
      hjust = 1.05,
      vjust = 1.2,
      lineheight = 1.15
    ) +
    ggplot2::scale_x_continuous(breaks = 1:5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::coord_cartesian(xlim = c(1, 5), clip = "on") +
    ggplot2::labs(
      x = "Firm Average Likert Score",
      y = "Share of Firms"
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black")
    )

  ggplot2::ggsave(
    file.path(figures, paste0(unname(histogram_file_stems[outcome_name]), ".png")),
    plot = firm_average_likert_histogram,
    width = 8.5,
    height = 4.8,
    dpi = 300,
    device = ragg::agg_png,
    bg = "white"
  )
}
