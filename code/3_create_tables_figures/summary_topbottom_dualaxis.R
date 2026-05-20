# -------------------------------------------------------------------
# Top/Bottom N firms by Borda EB, with one model's EB on the primary
# axis and Borda EB on the secondary axis (rescaled to share scale).
# Model-agnostic: pass model = "OLS" today, model = "PL" if/when PL
# coefficients are available again in the EB Coefficients sheet.
# Output: topbottom_by_bordaEB_dualaxis_<outcome>_<model>_borda.png
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))

MODEL_AXIS_LABELS <- c(
  PL    = "Plackett--Luce Item Worth (EB)",
  OLS   = "Likert Score (EB)",
  OLSC  = "Likert Centered Score (EB)",
  OL    = "Ordered Logit (EB)",
  Borda = "Borda Score (EB)"
)
MODEL_LEGEND_LABELS <- c(
  PL    = "Plackett-Luce",
  OLS   = "Likert Score",
  OLSC  = "Likert Centered",
  OL    = "Ordered Logit",
  Borda = "Borda Score"
)

write_topbottom_dualaxis <- function(dir_path,
                                     outcomes,
                                     plots_dir,
                                     model = "OLS",
                                     n_keep_overlay = 25,
                                     gap_width = 3,
                                     borda_mult = 1) {
  df_coef_raw <- tryCatch(read_parquet_sheet(dir_path, "Coefficients"),
                          error = function(e) NULL)
  if (is.null(df_coef_raw)) {
    message("⚠️  Coefficients sheet not found at ", dir_path,
            " — skipping Top/Bottom dual-axis plots.")
    return(invisible(NULL))
  }

  df_eb_model <- to_wide_coef(df_coef_raw, model,   value_col = "eb")
  df_eb_borda <- to_wide_coef(df_coef_raw, "Borda", value_col = "eb")

  if (is.null(df_eb_model)) {
    message("⚠️  Model '", model,
            "' EB coefficients absent — skipping Top/Bottom dual-axis plots.")
    return(invisible(NULL))
  }
  if (is.null(df_eb_borda)) {
    message("⚠️  Borda EB coefficients absent — skipping Top/Bottom dual-axis plots.")
    return(invisible(NULL))
  }

  primary_axis_name <- if (model %in% names(MODEL_AXIS_LABELS))
                         MODEL_AXIS_LABELS[[model]] else paste0(model, " (EB)")
  primary_legend    <- if (model %in% names(MODEL_LEGEND_LABELS))
                         MODEL_LEGEND_LABELS[[model]] else model
  borda_legend      <- MODEL_LEGEND_LABELS[["Borda"]]
  primary_color_key <- paste0(primary_legend, " (EB)")
  borda_color_key   <- "Borda (EB)"

  hide_gap_labels <- function(x) ifelse(grepl("^__gap\\d+__$", x), "", x)

  for (new_outcome in outcomes) {
    cols_needed <- c("firm", "firm_id", new_outcome)
    if (!all(cols_needed %in% names(df_eb_model))) {
      message("⚠️  Outcome '", new_outcome, "' missing for model '", model,
              "' EB — skipping.")
      next
    }
    if (!all(cols_needed %in% names(df_eb_borda))) {
      message("⚠️  Outcome '", new_outcome, "' missing for Borda EB — skipping.")
      next
    }

    eb_model <- df_eb_model[, cols_needed]
    names(eb_model) <- c("firm", "firm_id", "EB_model")

    eb_borda <- df_eb_borda[, cols_needed]
    names(eb_borda) <- c("firm", "firm_id", "Borda_EB")
    eb_borda$Borda_EB <- as.numeric(eb_borda$Borda_EB) * borda_mult

    eb_dual <- eb_model %>%
      dplyr::left_join(eb_borda, by = c("firm", "firm_id"))

    if (all(is.na(eb_dual$EB_model)) || all(is.na(eb_dual$Borda_EB))) {
      message("⚠️  Missing ", model, " EB or Borda EB for ", new_outcome,
              " — skipping.")
      next
    }

    ranked <- eb_dual %>% dplyr::mutate(.rank_key = Borda_EB)
    topN <- ranked %>% dplyr::slice_max(order_by = .rank_key,
                                        n = n_keep_overlay, with_ties = FALSE)
    botN <- ranked %>% dplyr::slice_min(order_by = .rank_key,
                                        n = n_keep_overlay, with_ties = FALSE)
    botN <- botN %>% dplyr::arrange(Borda_EB, firm)
    topN <- topN %>% dplyr::arrange(Borda_EB, firm)

    spacer <- tibble::tibble(
      firm               = paste0("__gap", seq_len(gap_width), "__"),
      firm_id            = NA_integer_,
      EB_model           = NA_real_,
      Borda_EB           = NA_real_,
      Borda_scaled       = NA_real_
    )
    subset_dual <- dplyr::bind_rows(botN, spacer, topN)

    s_pri <- stats::sd(subset_dual$EB_model, na.rm = TRUE)
    if (!is.finite(s_pri) || s_pri == 0) s_pri <- 1
    s_bd <- stats::sd(subset_dual$Borda_EB, na.rm = TRUE)
    if (!is.finite(s_bd) || s_bd == 0) s_bd <- 1
    a <- s_pri / s_bd

    subset_dual <- subset_dual %>%
      dplyr::mutate(
        firm = factor(firm, levels = firm, ordered = TRUE),
        Borda_scaled = a * Borda_EB
      )
    inv_to_borda <- function(y) (y) / a

    guides_dual <- subset_dual %>%
      dplyr::group_by(firm) %>%
      dplyr::summarise(
        ymin = suppressWarnings(pmin(EB_model, Borda_scaled, na.rm = TRUE)),
        ymax = suppressWarnings(pmax(EB_model, Borda_scaled, na.rm = TRUE)),
        .groups = "drop"
      )

    n_bottom <- nrow(botN)
    gap_start <- n_bottom + 0.5
    gap_end   <- n_bottom + gap_width + 0.5

    color_values <- setNames(c("steelblue", "darkorange"),
                             c(primary_color_key, borda_color_key))
    color_labels <- setNames(c(primary_legend, borda_legend),
                             c(primary_color_key, borda_color_key))

    p <- ggplot2::ggplot(subset_dual, ggplot2::aes(x = firm)) +
      ggplot2::geom_segment(data = guides_dual,
                            ggplot2::aes(x = firm, xend = firm,
                                         y = ymin, yend = ymax),
                            inherit.aes = FALSE,
                            linewidth = 0.3, alpha = 0.35) +
      ggplot2::geom_point(ggplot2::aes(y = EB_model,
                                       color = primary_color_key),
                          size = 2.6, alpha = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = EB_model,
                                      color = primary_color_key, group = 1),
                         linewidth = 0.7, alpha = 0.9) +
      ggplot2::geom_point(ggplot2::aes(y = Borda_scaled,
                                       color = borda_color_key),
                          size = 2.6, alpha = 0.9, shape = 17) +
      ggplot2::geom_line(ggplot2::aes(y = Borda_scaled,
                                      color = borda_color_key, group = 1),
                         linewidth = 0.7, alpha = 0.9, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      ggplot2::geom_vline(xintercept = gap_start, linetype = "dashed",
                          linewidth = 0.6, color = "grey55") +
      ggplot2::geom_vline(xintercept = gap_end, linetype = "dashed",
                          linewidth = 0.6, color = "grey55") +
      ggplot2::scale_y_continuous(
        name = primary_axis_name,
        sec.axis = ggplot2::sec_axis(~ inv_to_borda(.), name = "Borda Score (EB)")
      ) +
      ggplot2::scale_color_manual(
        values = color_values,
        breaks = c(primary_color_key, borda_color_key),
        labels = color_labels
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(
        override.aes = list(
          shape     = c(16, 17),
          linetype  = c("solid", "dashed"),
          linewidth = c(0.7, 0.7),
          alpha     = c(0.9, 0.9)
        )
      )) +
      ggplot2::labs(title = "", x = "Firm (sorted by Borda EB)", color = "") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        legend.position      = c(0.985, 0.03),
        legend.justification = c(1, 0),
        legend.title         = ggplot2::element_blank(),
        legend.text          = ggplot2::element_text(size = 8, color = "black"),
        legend.key           = ggplot2::element_blank(),
        legend.key.height    = grid::unit(10, "pt"),
        legend.background    = ggplot2::element_rect(
          fill = scales::alpha("white", 0.85), color = NA),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = "black"),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        plot.margin      = ggplot2::margin(t = 10, r = 20, b = 80, l = 90)
      ) +
      ggplot2::scale_x_discrete(labels = hide_gap_labels,
                                expand = ggplot2::expansion(add = 0.8)) +
      ggplot2::coord_cartesian(clip = "off")

    out_file <- file.path(plots_dir,
      paste0("topbottom_by_bordaEB_dualaxis_",
             new_outcome, "_", tolower(model), "_borda.png"))
    ggplot2::ggsave(out_file, p, width = 16, height = 8, dpi = 300)
    cat("✅ Saved:", basename(out_file), "\n")
  }
}

# Loop over every non-Borda model in the config (Borda is always the
# secondary-axis comparator).
for (m in setdiff(models, "Borda")) {
  write_topbottom_dualaxis(
    dir_path  = dir_path,
    outcomes  = ols_borda_dualaxis_outcomes,
    plots_dir = figures,
    model     = m
  )
}
