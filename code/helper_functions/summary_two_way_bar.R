library(dplyr)
library(ggplot2)
library(rlang)
library(scales)
library(ggpattern)

two_way_bar <- function(data,
                        x, group, y,
                        weight = NULL,          # optional weight column
                        fun = mean,             # aggregation for y if percent=FALSE & no weight
                        percent = FALSE,        # TRUE if y is 0/1 and you want shares
                        x_order = NULL,         # vector of x levels OR "asc_by_mean"/"desc_by_mean"
                        group_order = NULL,     # vector of group levels in desired order
                        x_lab = NULL, y_lab = NULL,
                        legend_title = NULL, title = NULL,
                        dodge_width = 0.75, bar_width = 0.65,
                        outfile = NULL, width = 8, height = 5, dpi = 300) {
  
  xq  <- enquo(x);   gq <- enquo(group);   yq <- enquo(y);  wq <- enquo(weight)
  x_name <- as_name(xq); g_name <- as_name(gq)
  
  ## ---- Aggregate ----------------------------------------------------------
  if (percent) {
    if (quo_is_null(wq)) {
      df_plot <- data %>%
        dplyr::filter(!is.na(!!xq), !is.na(!!gq), !is.na(!!yq)) %>%
        dplyr::group_by(!!xq, !!gq) %>%
        dplyr::summarise(
          N     = n(),
          p_hat = mean(!!yq, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(se = sqrt(p_hat * (1 - p_hat) / pmax(N, 1)),
                      ci = 1.96 * se)
    } else {
      df_plot <- data %>%
        dplyr::filter(!is.na(!!xq), !is.na(!!gq), !is.na(!!yq), !is.na(!!wq)) %>%
        dplyr::group_by(!!xq, !!gq) %>%
        dplyr::summarise(
          W     = sum((!!wq), na.rm = TRUE),
          p_hat = sum((!!yq) * (!!wq), na.rm = TRUE) / pmax(W, 1e-12),
          .groups = "drop"
        ) %>%
        dplyr::mutate(se = sqrt(p_hat * (1 - p_hat) / pmax(W, 1)),
                      ci = 1.96 * se) %>%
        dplyr::rename(N = W)
    }
    df_plot <- df_plot %>%
      dplyr::mutate(y_val = .data$p_hat,
                    ymin  = pmax(0, .data$y_val - .data$ci),
                    ymax  = pmin(1, .data$y_val + .data$ci))
  } else {
    if (quo_is_null(wq)) {
      df_plot <- data %>%
        dplyr::filter(!is.na(!!xq), !is.na(!!gq), !is.na(!!yq)) %>%
        dplyr::group_by(!!xq, !!gq) %>%
        dplyr::summarise(y_val = fun(!!yq, na.rm = TRUE), .groups = "drop")
    } else {
      df_plot <- data %>%
        dplyr::filter(!is.na(!!xq), !is.na(!!gq), !is.na(!!yq), !is.na(!!wq)) %>%
        dplyr::group_by(!!xq, !!gq) %>%
        dplyr::summarise(
          y_val = sum((!!yq) * (!!wq), na.rm = TRUE) /
            pmax(sum((!!wq), na.rm = TRUE), 1e-12),
          .groups = "drop"
        )
    }
    df_plot <- df_plot %>% dplyr::mutate(ymin = NA_real_, ymax = NA_real_)
  }
  
  ## ---- Ordering ------------------------------------------------------------
  # Order x
  if (!is.null(x_order)) {
    if (length(x_order) == 1 && x_order %in% c("asc_by_mean","desc_by_mean")) {
      x_means <- df_plot %>%
        dplyr::group_by(.data[[x_name]]) %>%
        dplyr::summarise(m = mean(.data$y_val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(if (x_order == "asc_by_mean") .data$m else dplyr::desc(.data$m))
      x_levels <- x_means[[x_name]]
    } else {
      x_levels <- x_order
    }
    df_plot[[x_name]] <- factor(df_plot[[x_name]], levels = x_levels)
  }
  
  # Order legend/groups
  if (!is.null(group_order)) {
    df_plot[[g_name]] <- factor(df_plot[[g_name]], levels = group_order)
  }
  
  ## ---- Pattern mapping by group --------------------------------------------
  group_levels <- levels(df_plot[[g_name]])
  if (is.null(group_levels)) {
    group_levels <- unique(df_plot[[g_name]])
  }
  n_levels <- length(group_levels)
  
  base_patterns <- c("none", "stripe", "crosshatch")
  if (n_levels <= length(base_patterns)) {
    pattern_vals <- base_patterns[seq_len(n_levels)]
  } else {
    extra <- rep(base_patterns[-1], length.out = n_levels - length(base_patterns))
    pattern_vals <- c(base_patterns, extra)
  }
  pattern_map <- setNames(pattern_vals, group_levels)
  
  ## ---- Labels --------------------------------------------------------------
  if (is.null(x_lab)) x_lab <- as_name(xq)
  if (is.null(y_lab)) y_lab <- if (percent) "Share" else as_name(yq)
  if (is.null(legend_title)) legend_title <- as_name(gq)
  
  ## ---- Plot ----------------------------------------------------------------
  p <- ggplot(
    df_plot,
    aes(x = !!xq, y = y_val, fill = !!gq, pattern = !!gq)
  ) +
    ggpattern::geom_col_pattern(
      position                 = position_dodge(width = dodge_width),
      width                    = bar_width,
      color                    = "black",
      pattern_color            = "black",
      pattern_fill             = "white",    # white fill, patterns + grey fill underlay
      pattern_density          = 0.4,
      pattern_key_scale_factor = 0.8,
      show.legend              = TRUE
    ) +
    {
      if (percent)
        geom_errorbar(
          aes(ymin = ymin, ymax = ymax),
          position = position_dodge(width = dodge_width),
          width = 0.18, linewidth = 0.6
        )
    } +
    # Grayscale fill scale
    scale_fill_grey(start = 0.85, end = 0.35) +
    scale_pattern_manual(values = pattern_map) +
    scale_y_continuous(
      limits = if (percent) c(0, 1) else NULL,
      breaks = if (percent) seq(0, 1, by = 0.1) else waiver(),
      labels = if (percent) percent_format(accuracy = 1) else waiver(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x       = "",
      y       = y_lab,
      fill    = legend_title,
      pattern = legend_title,
      title   = title
    ) +
    guides(
      fill    = guide_legend(title = legend_title),
      pattern = guide_legend(title = legend_title)
    ) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid      = element_blank(),
      axis.line       = element_blank(),
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right"
    )
  
  if (!is.null(outfile)) {
    ggsave(outfile, plot = p, width = width, height = height, dpi = dpi,
           device = ragg::agg_png, bg = "white")
  }
  
  p
}
