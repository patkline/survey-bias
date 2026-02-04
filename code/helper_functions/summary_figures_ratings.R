plot_rating_1to5_restrict <- function(df, var, id_var = "ResponseId",
                                      label_version = c("gap", "level"),
                                      outfile = NULL,
                                      width = 8, height = 5, dpi = 300,
                                      ymax = 60,   # kept in signature but no longer controls axis
                                      bar_color = "#636363") {  # <-- medium gray
  require(dplyr); require(ggplot2); require(stringr); require(tibble)
  
  stopifnot(id_var %in% names(df))
  stopifnot(var %in% names(df))
  
  label_version <- match.arg(label_version)
  
  # Define label sets for the two versions
  if (label_version == "gap") {
    # 1–5: Much More/Less Likely scale
    lab1 <- "Much More Likely"
    lab2 <- "Somewhat More Likely"
    lab3 <- "Equally Likely"
    lab4 <- "Somewhat Less Likely"
    lab5 <- "Much Less Likely"
  } else if (label_version == "level") {
    # 1–5: Very/Somewhat Likely/Unlikely scale
    lab1 <- "Very Likely"
    lab2 <- "Somewhat Likely"
    lab3 <- "Neither Likely Nor Unlikely"
    lab4 <- "Somewhat Unlikely"
    lab5 <- "Very Unlikely"
  }
  
  lab_pna <- "Prefer not to answer"
  lab_mis <- "Missing"
  
  order_levels <- c(lab1, lab2, lab3, lab4, lab5, lab_pna, lab_mis)
  
  # Keep ONLY respondents with >=1 non-missing rating for this var
  ids_keep <- df %>%
    dplyr::group_by(id = .data[[id_var]]) %>%
    dplyr::summarise(any_answer = any(!is.na(.data[[var]])), .groups = "drop") %>%
    dplyr::filter(any_answer) %>%
    dplyr::pull(id)
  
  dat_keep <- df %>% dplyr::filter(.data[[id_var]] %in% ids_keep)
  if (nrow(dat_keep) == 0) stop("No respondents with at least one non-missing rating.")
  
  # Label each row
  lab_df <- dat_keep %>%
    dplyr::mutate(
      rating_num = suppressWarnings(as.numeric(.data[[var]])),
      label = dplyr::case_when(
        is.na(rating_num)        ~ lab_mis,
        rating_num == -1         ~ lab_pna,
        rating_num == 1          ~ lab1,
        rating_num == 2          ~ lab2,
        rating_num == 3          ~ lab3,
        rating_num == 4          ~ lab4,
        rating_num == 5          ~ lab5,
        TRUE                     ~ lab_mis
      )
    )
  
  # Counts + shares + 95% CI over rows
  N_rows <- nrow(lab_df)
  
  counts <- lab_df %>%
    dplyr::count(label, name = "n") %>%
    dplyr::right_join(
      tibble::tibble(label = factor(order_levels, levels = order_levels)),
      by = "label"
    ) %>%
    dplyr::mutate(
      n     = dplyr::coalesce(n, 0L),
      p     = n / N_rows,
      share = 100 * p,
      se    = sqrt(p * (1 - p) / N_rows) * 100,   # percentage points
      ci    = 1.96 * se,
      label = factor(label, levels = order_levels)
    )
  
  # Use the full counts for summary, but DROP "Missing" from the plot
  counts_plot <- counts %>%
    dplyr::filter(label != lab_mis)
  
  # Plot: share (%) with error bars; y-axis 0–100
  p <- ggplot2::ggplot(counts_plot, ggplot2::aes(x = label, y = share)) +
    ggplot2::geom_col(width = 0.8, color = "black", fill = bar_color) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, share - ci),
                   ymax = pmin(100, share + ci)),
      width = 0.15, linewidth = 0.6
    ) +
    ggplot2::labs(x = "", y = "Share of Responses (%)") +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                breaks = seq(0, 100, by = 10),
                                expand = c(0, 0)) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.line    = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  if (!is.null(outfile)) {
    ggplot2::ggsave(outfile, plot = p,
                    width = width, height = height, dpi = dpi,
                    device = ragg::agg_png, bg = "white")
  }
  
  list(plot = p, summary = counts, n_rows_used = N_rows,
       n_respondents_kept = dplyr::n_distinct(dat_keep[[id_var]]))
}
