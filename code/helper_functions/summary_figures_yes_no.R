plot_yes_no_pna_both <- function(df, var,
                                 outfile_base = NULL,   # e.g., "…/any_entry_lev_exp"
                                 ci_yes_no_only = FALSE, # now default: show CIs for all categories
                                 width = 8, height = 5, dpi = 300,
                                 palette = c("No"="#2c7fb8","Yes"="#d67f2e",
                                             "Prefer not to answer"="#3a8f33","missing"="#bdbdbd")) {
  require(dplyr); require(ggplot2); require(stringr)
  
  # 1) One row per respondent
  stopifnot("ResponseId" %in% names(df))
  df_first <- df %>% dplyr::group_by(ResponseId) %>% dplyr::slice(1) %>% dplyr::ungroup()
  
  N <- nrow(df_first)
  if (N == 0) stop("No respondents after collapsing to one row per ResponseId.")
  
  # 2) Standardize values -> No / Yes / Prefer not to answer / missing
  vals <- df_first %>%
    dplyr::transmute(value_raw = .data[[var]]) %>%
    dplyr::mutate(
      v = stringr::str_squish(tolower(as.character(value_raw))),
      value = dplyr::case_when(
        is.na(v) | v == ""                          ~ "missing",
        v %in% c("no","0")                          ~ "No",
        v %in% c("yes","1")                         ~ "Yes",
        stringr::str_detect(v, "^prefer")           ~ "Prefer not to answer",
        TRUE                                        ~ "missing"
      )
    ) %>% dplyr::select(value)
  
  # 3) Counts (ensure all four levels exist, in fixed order)
  order_levels <- c("No","Yes","Prefer not to answer","missing")
  all_levels_tbl <- tibble::tibble(value = factor(order_levels, levels = order_levels))
  
  counts <- vals %>%
    dplyr::mutate(value = factor(value, levels = order_levels)) %>%
    dplyr::count(value, name = "n") %>%
    dplyr::right_join(all_levels_tbl, by = "value") %>%
    dplyr::mutate(
      n     = dplyr::coalesce(n, 0L),
      p     = n / N,
      share = 100 * p,
      se    = sqrt(p * (1 - p) / N) * 100,
      ci    = 1.96 * se,
      # control which categories get error bars
      ci_plot = if (ci_yes_no_only) ifelse(value %in% c("No","Yes"), ci, NA_real_) else ci
    )
  
  # 4) SHARE plot (0–100% y-axis)
  p_share <- ggplot(counts, aes(x = value, y = share, fill = value)) +
    geom_col(width = 0.8, color = "black") +
    geom_errorbar(
      data = counts %>% dplyr::filter(!is.na(ci_plot)),
      aes(ymin = pmax(0, share - ci_plot), ymax = pmin(100, share + ci_plot)),
      width = 0.15, linewidth = 0.6
    ) +
    scale_fill_manual(values = palette, guide = "none") +
    labs(x = "Values", y = "Share of respondents (%)") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
    theme_classic(base_size = 13) +
    theme(
      panel.grid  = element_blank(),
      axis.line   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  # 5) COUNT plot (raw counts)
  ymax_ct <- max(counts$n, na.rm = TRUE)
  p_count <- ggplot(counts, aes(x = value, y = n, fill = value)) +
    geom_col(width = 0.8, color = "black") +
    scale_fill_manual(values = palette, guide = "none") +
    labs(x = "Values", y = "N. of observations") +
    scale_y_continuous(limits = c(0, ymax_ct), expand = c(0, 0)) +
    theme_classic(base_size = 13) +
    theme(
      panel.grid  = element_blank(),
      axis.line   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  # 6) Optional saves
  if (!is.null(outfile_base)) {
    ggplot2::ggsave(paste0(outfile_base, "_share.png"), plot = p_share,
                    width = width, height = height, dpi = dpi,
                    device = ragg::agg_png, bg = "white")
    ggplot2::ggsave(paste0(outfile_base, "_count.png"), plot = p_count,
                    width = width, height = height, dpi = dpi,
                    device = ragg::agg_png, bg = "white")
  }
  
  list(share = p_share, count = p_count)
}
