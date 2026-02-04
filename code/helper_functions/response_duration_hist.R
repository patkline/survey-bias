# Packages
library(dplyr)
library(ggplot2)

#' Make response-duration histogram (share on y-axis)
#' @param data    data.frame with columns ResponseId and response_duration (seconds)
#' @param outfile full path to save (e.g., "~/.../figures/response_duration_hist.png")
#' @param binwidth histogram bin width in minutes (default 1)
#' @param xlim     x-axis limits in minutes (default c(0, 20))
#' @param ylim     y-axis limits for share (default c(0, 0.30))
#' @param width,height,dpi ggsave parameters (defaults match your script)
#' @return the ggplot object (invisibly)
make_response_duration_hist <- function(
    data,
    outfile,
    binwidth = 1,
    xlim = c(0, 20),
    ylim = c(0, 0.30),
    width = 8.5,
    height = 4.8,
    dpi = 300
) {
  stopifnot("ResponseId" %in% names(data), "response_duration" %in% names(data))
  
  df_first <- data |>
    dplyr::group_by(ResponseId) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(response_duration = as.numeric(response_duration / 60)) |>
    dplyr::filter(is.finite(response_duration), response_duration >= 0)
  
  if (nrow(df_first) == 0L) stop("No finite, non-negative durations after preprocessing.")
  
  stats <- df_first |>
    dplyr::summarise(
      mean   = mean(response_duration, na.rm = TRUE),
      median = median(response_duration, na.rm = TRUE),
      q1     = stats::quantile(response_duration, 0.25, na.rm = TRUE),
      q3     = stats::quantile(response_duration, 0.75, na.rm = TRUE)
    )
  
  label_txt <- sprintf("Median: %.3f | P25: %.3f | P75: %.3f",
                       stats$median, stats$q1, stats$q3)
  
  p <- ggplot2::ggplot(df_first, ggplot2::aes(x = response_duration)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(count / sum(count))),
      binwidth = binwidth, boundary = 0, closed = "left",
      fill = "#c7dceb", color = "#6b7d8a", alpha = 0.7
    ) +
    ggplot2::labs(x = "Duration (in minutes)", y = "Share of responses") +
    ggplot2::annotate("text", x = -Inf, y = Inf, hjust = -0.02, vjust = 1.6, label = label_txt) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.line    = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  ggplot2::ggsave(filename = outfile, plot = p, width = width, height = height, dpi = dpi)
  invisible(p)
}

dir <- "~/Documents/consolidated_code_server/processed/"
excel_dir <- "~/Documents/consolidated_code_server/excel/"
file_path <- file.path(dir, "long_survey_final.csv")


