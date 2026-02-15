# --------------------------------------------------------------------
# Combined PL (lower-left) / Borda (upper-right) triangular heatmap
# Centered outside labels; auto-trims whitespace after saving.
# --------------------------------------------------------------------
source("code/globals.R")

ensure_symmetry <- function(df) {
  stopifnot(all(c("lhs","rhs") %in% names(df)))
  flipped <- df %>%
    mutate(lhs_new = rhs, rhs_new = lhs) %>%
    select(-lhs, -rhs) %>%
    rename(lhs = lhs_new, rhs = rhs_new)
  flipped <- flipped[names(df)]
  bind_rows(df, flipped) %>% distinct(lhs, rhs, .keep_all = TRUE)
}

  compute_highlight_segments <- function(highlight_cells, n) {
    if (is.null(highlight_cells)) return(NULL)

    cells <- as.data.frame(highlight_cells)
    if (!all(c("row", "col") %in% names(cells))) {
      stop("highlight_cells must have columns 'row' and 'col' (1-indexed matrix coords: row=top..bottom, col=left..right).")
    }

    cells <- cells %>%
      dplyr::transmute(
        row = suppressWarnings(as.integer(.data$row)),
        col = suppressWarnings(as.integer(.data$col))
      ) %>%
      dplyr::filter(!is.na(.data$row), !is.na(.data$col))

    if (nrow(cells) == 0) return(NULL)
    if (any(cells$row < 1 | cells$row > n | cells$col < 1 | cells$col > n)) {
      stop("highlight_cells contains out-of-range coordinates. Valid row/col are 1..n where n=length(custom_order).")
    }

    # Map matrix-style coordinates -> ggplot discrete positions.
    # x increases left->right; y increases bottom->top.
    # With y factor levels = rev(custom_order), matrix row 1 (top) corresponds to y = n.
    cells <- cells %>%
      dplyr::distinct(.data$row, .data$col) %>%
      dplyr::mutate(x = .data$col, y = n - .data$row + 1)

    edges <- bind_rows(
      # left edge
      cells %>% dplyr::transmute(x1 = x - 0.5, y1 = y - 0.5, x2 = x - 0.5, y2 = y + 0.5),
      # right edge
      cells %>% dplyr::transmute(x1 = x + 0.5, y1 = y - 0.5, x2 = x + 0.5, y2 = y + 0.5),
      # bottom edge
      cells %>% dplyr::transmute(x1 = x - 0.5, y1 = y - 0.5, x2 = x + 0.5, y2 = y - 0.5),
      # top edge
      cells %>% dplyr::transmute(x1 = x - 0.5, y1 = y + 0.5, x2 = x + 0.5, y2 = y + 0.5)
    ) %>%
      dplyr::mutate(
        kx1 = pmin(x1, x2), ky1 = pmin(y1, y2),
        kx2 = pmax(x1, x2), ky2 = pmax(y1, y2),
        key = paste(kx1, ky1, kx2, ky2, sep = "|")
      )

    boundary_edges <- edges %>%
      dplyr::count(.data$key, .data$x1, .data$y1, .data$x2, .data$y2, name = "n") %>%
      dplyr::filter(.data$n == 1) %>%
      dplyr::transmute(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2)

    if (nrow(boundary_edges) == 0) return(NULL)
    boundary_edges
  }

create_combined_tri_heatmap <- function(file_path,
                                        sheet_pl           = "pairwise_summary",
                                        sheet_borda        = "pairwise_summary_borda",
                                        all_flag           = TRUE,
                                        title              = "",
                                        filename           = "heatmap_combined.png",
                                        label_mapping,           # named vector: raw -> pretty
                                        custom_order,            # pretty labels order for axes
                                        
                                        # one knob for BOTH arrow+text sizes
                                        label_size     = 5,
                                        
                                        # vertical offsets (smaller = closer to panel)
                                        top_vjust      = -0.7,
                                        bot_vjust      =  1.4,
                                        
                                        # plot margins (points) -- can stay generous; we'll trim later
                                        top_margin_pt    = 40,
                                        right_margin_pt  = 24,
                                        bottom_margin_pt = 190,
                                        left_margin_pt   = 12,

                                        # optional: highlight union of selected cells with a continuous outline
                                        # data.frame(row=..., col=...) using 1-indexed matrix coords (row=top..bottom, col=left..right)
                                        highlight_cells      = NULL,
                                        highlight_color      = "red",
                                        highlight_linewidth  = 1.2,
                                        highlight_alpha      = 1,
                                        
                                        # post-process trim
                                        trim_whitespace  = TRUE) {
  
  # --- read & prep ---
  pl_df    <- read.xlsx(file_path, sheet = sheet_pl)
  borda_df <- read.xlsx(file_path, sheet = sheet_borda)
  
  if ("all_firms" %in% names(pl_df))    pl_df    <- dplyr::filter(pl_df,   all_firms == all_flag)
  if ("all_firms" %in% names(borda_df)) borda_df <- dplyr::filter(borda_df, all_firms == all_flag)
  
  pl_df    <- ensure_symmetry(pl_df)
  borda_df <- ensure_symmetry(borda_df)
  
  pl_df    <- pl_df    %>% transmute(lhs = as.character(lhs), rhs = as.character(rhs),
                                     corr_pl = suppressWarnings(as.numeric(corr_c)))
  borda_df <- borda_df %>% transmute(lhs = as.character(lhs), rhs = as.character(rhs),
                                     corr_bd = suppressWarnings(as.numeric(corr_c)))
  
  comb <- dplyr::full_join(pl_df, borda_df, by = c("lhs","rhs")) %>%
    dplyr::mutate(
      out1  = unname(label_mapping[as.character(lhs)]),
      out2  = unname(label_mapping[as.character(rhs)]),
      c_idx = match(out1, custom_order),   # columns
      r_idx = match(out2, custom_order)    # rows
    ) %>%
    dplyr::filter(!is.na(out1), !is.na(out2), !is.na(c_idx), !is.na(r_idx)) %>%
    dplyr::mutate(
      corr_c = dplyr::case_when(
        r_idx > c_idx ~ corr_pl,   # PL lower-left
        r_idx < c_idx ~ corr_bd,   # Borda upper-right
        TRUE          ~ NA_real_   # diagonal blank
      ),
      Outcome1 = factor(out1, levels = custom_order),
      Outcome2 = factor(out2, levels = rev(custom_order))
    )
  
  plot_df <- dplyr::filter(comb, is.finite(corr_c))
  n <- length(custom_order)
  
  # --- base heatmap ---
  p <- ggplot(plot_df, aes(x = Outcome1, y = Outcome2, fill = corr_c)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "green",
                         midpoint = 0, name = "Correlation", na.value = "white") +
    geom_text(aes(label = sprintf("%.2f", corr_c)), size = 3) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 0.85, size = 10,
                                  margin = margin(t = 24)),
      axis.text.y  = element_text(size = 10),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(title = title, x = NULL, y = NULL)

  # --- optional highlight outline (continuous around the selected cell set) ---
  highlight_segments <- compute_highlight_segments(highlight_cells, n)
  if (!is.null(highlight_segments)) {
    p <- p +
      geom_segment(
        data = highlight_segments,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        color = highlight_color,
        linewidth = highlight_linewidth,
        alpha = highlight_alpha,
        lineend = "square"
      )
  }
  
  # --- centered outside labels with LONG arrows tight to text ---
  cx <- (n + 1) / 2
  p <- p +
    theme(plot.margin = margin(t = top_margin_pt, r = right_margin_pt,
                               b = bottom_margin_pt, l = left_margin_pt)) +
    coord_cartesian(clip = "off") +
    annotate("text",
             x = cx, y = Inf,
             label = "Borda \u27F6",     # long rightwards arrow
             vjust = top_vjust, hjust = 0.5,
             size = label_size, fontface = "bold") +
    annotate("text",
             x = cx, y = -Inf,
             label = "\u27F5 Plackett\u2013Luce",  # long leftwards arrow
             vjust = bot_vjust, hjust = 0.5,
             size = label_size, fontface = "bold")
  
  # save first
  ggsave(filename, p, width = 12, height = 10, dpi = 300, bg = "white")
  
  # then trim the whitespace so LaTeX shows no extra bottom margin
  if (isTRUE(trim_whitespace)) {
    if (!requireNamespace("magick", quietly = TRUE)) {
      warning("Package 'magick' not installed; skipping trim. Install with install.packages('magick').")
    } else {
      img <- magick::image_read(filename)
      img <- magick::image_trim(img)      # auto-crop empty margins
      magick::image_write(img, path = filename)
    }
  }
  
  message("✅ Saved (and trimmed) heatmap: ", normalizePath(filename))
}









# ------------------------------------------------------------
# Example wiring (same mapping/order you used for the figure)
# ------------------------------------------------------------
label_mapping <- c(
  "discretion" = "Manager Discretion",
  "FirmSelective" = "Firm Selectivity",
  "FirmDesire" = "Firm Desirability",
  "conduct_favor_white" = "Discrimination Black (Conduct)",
  "conduct_favor_younger" = "Discrimination Older (Conduct)",
  "conduct_favor_male" = "Discrimination Female (Conduct)",
  "FirmHire_favor_male" = "Discrimination Female (Hire)",
  "FirmHire_favor_white" = "Discrimination Black (Hire)",
  "FirmCont_favor_male" = "Discrimination Female (Contact)",
  "FirmCont_favor_white" = "Discrimination Black (Contact)"
)

custom_order_non <- c(
  "Discrimination Black (Conduct)",
  "Discrimination Black (Hire)",
  "Discrimination Black (Contact)",
  "Discrimination Female (Conduct)",
  "Discrimination Female (Hire)",
  "Discrimination Female (Contact)",
  "Discrimination Older (Conduct)",
  "Firm Desirability",
  "Firm Selectivity",
  "Manager Discretion"
)

# Example call (full sample)
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = TRUE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non
)

# Highlighted version (cells are 1-indexed: row=top..bottom, col=left..right)
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = TRUE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full_highlight_1.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non,
  highlight_cells = data.frame(
    row = c(2, 3, 3),
    col = c(1, 1, 2)
  ),
  highlight_color = "purple",
  highlight_linewidth = 2
)

# Highlighted version (cells are 1-indexed: row=top..bottom, col=left..right)
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = TRUE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full_highlight_2.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non,
  highlight_cells = data.frame(
    row = c(5, 6, 6),
    col = c(4, 4, 5)
  ),
  highlight_color = "purple",
  highlight_linewidth = 2
)

# Highlighted version (cells are 1-indexed: row=top..bottom, col=left..right)
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = TRUE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full_highlight_3.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non,
  highlight_cells = data.frame(
    row = c(4, 4, 4, 5, 5, 5, 6, 6, 6),
    col = c(1, 2, 3, 1, 2, 3, 1, 2, 3)
  ),
  highlight_color = "purple",
  highlight_linewidth = 2
)

# Highlighted version (cells are 1-indexed: row=top..bottom, col=left..right)
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = TRUE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full_highlight_4.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non,
  highlight_cells = data.frame(
    row = c(8, 8, 8, 9, 9, 9, 10, 10, 10),
    col = c(1, 2, 3, 1, 2, 3, 1, 2, 3)
  ),
  highlight_color = "purple",
  highlight_linewidth = 2
)

# Overlap-only
create_combined_tri_heatmap(
  file_path   = file.path(excel, "Plackett_Luce_Full_Sample.xlsx"),
  sheet_pl    = "pairwise_summary",
  sheet_borda = "pairwise_summary_borda",
  all_flag    = FALSE,
  title       = "",
  filename    = file.path(figures, "heatmap_combined_full_overlap.png"),
  label_mapping = label_mapping,
  custom_order  = custom_order_non
)
