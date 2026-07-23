# ------------------------------------------------------------------------------
# Purpose: Firm-level histograms of pooled discrimination Likert scores
# ------------------------------------------------------------------------------
# Run the shared summary-outcomes config, which sources globals
source(file.path("code", "3_create_tables_figures", "summary_outcomes_config.R"))

# Named character vector, one element per pooled discrimination measure; names are outcome codes, values display labels
histogram_outcomes <- c(
  pooled_favor_white = "Discrimination Black (Pooled)",
  pooled_favor_male  = "Discrimination Gender (Pooled)"
)

# ------------------------------------------------------------------------------
# Load and check the coefficient and variance sheets
# ------------------------------------------------------------------------------
# Load the full-sample firm-level coefficient sheet
coef_df <- read_parquet_sheet(dir_path, "Coefficients")

# Load the full-sample variance decomposition sheet
variance_df <- read_parquet_sheet(dir_path, "variance")

# Coefficients sheet should contain the identifying and estimate columns
required_coef_cols <- c(
  "subset", "model", "outcome", "entity_type", "entity_id", "estimate"
)
missing_coef_cols <- setdiff(required_coef_cols, names(coef_df))
if (length(missing_coef_cols)) {
  stop("Coefficients sheet missing columns: ", paste(missing_coef_cols, collapse = ", "))
}

# variance sheet should contain the identifying, variance, and signal columns
required_variance_cols <- c("subset", "model", "outcome", "variance", "signal")
missing_variance_cols <- setdiff(required_variance_cols, names(variance_df))
if (length(missing_variance_cols)) {
  stop("variance sheet missing columns: ", paste(missing_variance_cols, collapse = ", "))
}

# ------------------------------------------------------------------------------
# Build firm-level Likert scores and per-measure histogram stats
# ------------------------------------------------------------------------------
# Dataframe, one row per Likert aggregation model; rank 1 prefers the
# non-recentered OLS ratings used in the belief summary table.
likert_model_preference <- data.frame(
  model = c("OLS_not_recentered", "OLS"),
  preference = c(1L, 2L),
  stringsAsFactors = FALSE
)

# Dataframe, one row per pooled measure x firm, holding the firm's preferred-model average Likert score
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

# Firm scores should be uniquely identified by measure x firm
if (anyDuplicated(firm_likert_scores[c("outcome", "entity_id")])) {
  stop("Firm Likert scores are not unique by outcome and entity_id")
}

# Firm scores should all be finite
if (any(!is.finite(firm_likert_scores$average_likert_score))) {
  stop("Firm Likert scores contain missing or non-finite values")
}

# Both pooled measures should have firm scores
if (!setequal(unique(firm_likert_scores$outcome), names(histogram_outcomes))) {
  stop("One or more pooled discrimination outcomes lack firm-level Likert scores")
}

# Sample and signal standard deviations come directly from the centered OLS
# variance rows; recentering does not alter either SD.
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

# Each pooled measure should have exactly one variance row with finite sample and signal SDs
if (anyDuplicated(histogram_stats$outcome) ||
    !setequal(histogram_stats$outcome, names(histogram_outcomes)) ||
    any(!is.finite(histogram_stats$sample_sd)) ||
    any(!is.finite(histogram_stats$signal_sd))) {
  stop("Expected one finite OLS variance row for each pooled discrimination outcome")
}

# Add each pooled measure's firm-mean Likert score to its SD row
histogram_stats <- firm_likert_scores |>
  dplyr::group_by(.data$outcome) |>
  dplyr::summarise(mean = mean(.data$average_likert_score), .groups = "drop") |>
  dplyr::left_join(histogram_stats, by = "outcome")

# ------------------------------------------------------------------------------
# Plot one histogram per pooled discrimination measure
# ------------------------------------------------------------------------------
# Named character vector, one output file stem per pooled measure
histogram_file_stems <- c(
  pooled_favor_white = "firm_average_likert_histogram_discrimination_black_pooled",
  pooled_favor_male  = "firm_average_likert_histogram_discrimination_gender_pooled"
)

for (outcome_name in names(histogram_outcomes)) {
  # Keep this measure's firm scores
  plot_data <- dplyr::filter(firm_likert_scores, .data$outcome == outcome_name)

  # Keep this measure's mean and SD row
  plot_stats <- dplyr::filter(histogram_stats, .data$outcome == outcome_name)

  # Annotation text listing the mean, sample SD, and signal SD
  annotation_text <- sprintf(
    "Mean: %.3f\nSample Std Dev: %.3f\nSignal Std Dev: %.3f",
    plot_stats$mean,
    plot_stats$sample_sd,
    plot_stats$signal_sd
  )

  # Histogram of firm average Likert scores, share of firms on the y axis
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
    # Dashed vertical line at the firm-mean Likert score
    ggplot2::geom_vline(
      xintercept = plot_stats$mean,
      linetype = "dashed",
      color = "darkorange",
      linewidth = 0.6
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

  # Save the histogram PNG
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
