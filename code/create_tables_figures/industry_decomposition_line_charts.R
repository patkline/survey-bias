# ------------------------------------------------------------------------------
# Between-industry and within-industry dual-axis line charts (OLS "Likert" + Borda)
#
# For each outcome:
#   - "between": top/bottom 25 firms by demeaned (_dm) EB values, sorted by OLS
#   - "within":  all industries by industry mean (_im) EB values, sorted by OLS
#
# EB shrinkage (toward zero): EB_i = sigma2 / (sigma2 + se_i^2) * estimate_i
#   where sigma2 = max(0, (sum(est^2) - sum(se^2)) / J)
# ------------------------------------------------------------------------------
source("code/globals.R")

# --- configuration ---
excel_path <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")
industry_map_path <- file.path(processed, "industry_map.xlsx")

outcomes_dm <- c("pooled_favor_white_dm", "pooled_favor_male_dm", "conduct_favor_younger_dm")
outcomes_im <- c("pooled_favor_white_im", "pooled_favor_male_im", "conduct_favor_younger_im")

n_keep <- 25
gap_width <- 3

# --- read data ---
coef <- openxlsx::read.xlsx(excel_path, sheet = "Coefficients")
ind_map <- readxl::read_xlsx(industry_map_path)

# industry code -> name lookup
ind_names <- unique(ind_map[, c("aer_naics2", "sic_code_aggregated_two_digit_harmonized_names_aer")])
ind_name_lookup <- setNames(
  ind_names$sic_code_aggregated_two_digit_harmonized_names_aer,
  as.character(ind_names$aer_naics2)
)

# --- EB shrinkage toward zero ---
eb_shrink <- function(estimate, se) {
  J <- length(estimate)
  sigma2 <- max(0, (sum(estimate^2) - sum(se^2)) / J)
  shrink <- sigma2 / (sigma2 + se^2)
  shrink * estimate
}

# --- helper: extract data for one outcome/model, compute EB ---
get_eb <- function(outcome, mdl, etype) {
  d <- coef[coef$outcome == outcome &
              coef$model == mdl &
              coef$entity_type == etype &
              coef$subset == "all", ]
  stopifnot(nrow(d) > 0)
  d$eb <- eb_shrink(d$estimate, d$se)
  d
}

# --- helper: dual-axis line chart (matches summary_item_worths format) ---
make_dual_axis_chart <- function(df, ols_col, borda_col, name_col,
                                 x_label, file_name, has_gap = FALSE,
                                 gap_start = NULL, gap_end = NULL) {
  # scaling: match SDs so both series are visually comparable
  s_ols <- stats::sd(df[[ols_col]], na.rm = TRUE)
  s_bd  <- stats::sd(df[[borda_col]], na.rm = TRUE)
  stopifnot(is.finite(s_ols) && s_ols > 0)
  stopifnot(is.finite(s_bd) && s_bd > 0)

  mean_borda <- mean(df[[borda_col]], na.rm = TRUE)
  a <- s_ols / s_bd
  b <- -a * mean_borda

  df$Borda_scaled <- a * df[[borda_col]] + b
  inv_to_borda <- function(y) (y - b) / a

  df[[name_col]] <- factor(df[[name_col]], levels = df[[name_col]], ordered = TRUE)

  guides_df <- data.frame(
    name = df[[name_col]],
    ymin = pmin(df[[ols_col]], df$Borda_scaled, na.rm = TRUE),
    ymax = pmax(df[[ols_col]], df$Borda_scaled, na.rm = TRUE)
  )

  hide_gap_labels <- function(x) ifelse(grepl("^__gap\\d+__$", x), "", x)

  p <- ggplot(df, aes(x = .data[[name_col]])) +
    geom_segment(data = guides_df,
                 aes(x = name, xend = name, y = ymin, yend = ymax),
                 inherit.aes = FALSE, linewidth = 0.3, alpha = 0.35) +
    geom_point(aes(y = .data[[ols_col]], color = "Likert (EB)"), size = 2.6, alpha = 0.9) +
    geom_line(aes(y = .data[[ols_col]], color = "Likert (EB)", group = 1),
              linewidth = 0.7, alpha = 0.9) +
    geom_point(aes(y = Borda_scaled, color = "Borda (EB)"), size = 2.6, alpha = 0.9, shape = 17) +
    geom_line(aes(y = Borda_scaled, color = "Borda (EB)", group = 1),
              linewidth = 0.7, alpha = 0.9, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    scale_y_continuous(
      name     = "Likert (EB)",
      sec.axis = sec_axis(~ inv_to_borda(.), name = "Borda (EB)")
    ) +
    scale_color_manual(
      values = c("Likert (EB)" = "steelblue", "Borda (EB)" = "darkorange"),
      breaks = c("Likert (EB)", "Borda (EB)"),
      labels = c("Likert (EB)" = "Likert", "Borda (EB)" = "Borda")
    ) +
    guides(color = guide_legend(
      override.aes = list(
        shape = c(16, 17),
        linetype = c("solid", "dashed"),
        linewidth = c(0.7, 0.7),
        alpha = c(0.9, 0.9)
      )
    )) +
    labs(title = "", x = x_label, color = "") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = c(0.985, 0.03),
      legend.justification = c(1, 0),
      legend.title = element_blank(),
      legend.text = element_text(size = 8, color = "black"),
      legend.key = element_blank(),
      legend.key.height = grid::unit(10, "pt"),
      legend.background = element_rect(fill = scales::alpha("white", 0.85), color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = "black"),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(t = 10, r = 20, b = 80, l = 90)
    ) +
    scale_x_discrete(labels = hide_gap_labels, expand = expansion(add = 0.8)) +
    coord_cartesian(clip = "off")

  if (has_gap) {
    p <- p +
      geom_vline(xintercept = gap_start, linetype = "dashed", linewidth = 0.6, color = "grey55") +
      geom_vline(xintercept = gap_end,   linetype = "dashed", linewidth = 0.6, color = "grey55")
  }

  ggsave(file.path(figures, file_name), p, width = 16, height = 8, dpi = 300)
  cat("Saved:", file_name, "\n")
}

# ==============================================================================
# Between-industry (demeaned): top/bottom 25 firms, sorted by Borda
# ==============================================================================
for (outcome_dm in outcomes_dm) {
  ols_dm  <- get_eb(outcome_dm, "OLS",   "Firm")
  borda_dm <- get_eb(outcome_dm, "Borda", "Firm")

  merged <- merge(
    ols_dm[, c("entity_id", "entity", "eb")],
    borda_dm[, c("entity_id", "eb")],
    by = "entity_id", suffixes = c("_ols", "_borda")
  )
  names(merged) <- c("firm_id", "firm", "EB_OLS", "EB_Borda")

  # top/bottom by Borda EB
  ranked <- merged[order(merged$EB_Borda), ]
  botN <- head(ranked, n_keep)
  topN <- tail(ranked, n_keep)

  spacer <- data.frame(
    firm_id  = rep(NA_integer_, gap_width),
    firm     = paste0("__gap", seq_len(gap_width), "__"),
    EB_OLS   = NA_real_,
    EB_Borda = NA_real_
  )

  subset_df <- rbind(botN, spacer, topN)
  gap_start <- n_keep + 0.5
  gap_end   <- n_keep + gap_width + 0.5

  # strip outcome base name for file naming
  outcome_base <- sub("_dm$", "", outcome_dm)
  fname <- paste0("between_industry_dualaxis_", outcome_base, ".png")

  make_dual_axis_chart(
    df = subset_df,
    ols_col = "EB_OLS",
    borda_col = "EB_Borda",
    name_col = "firm",
    x_label = "Firm (sorted by Borda EB)",
    file_name = fname,
    has_gap = TRUE,
    gap_start = gap_start,
    gap_end = gap_end
  )
}

# ==============================================================================
# Within-industry (industry means): all industries, sorted by Borda
# ==============================================================================
for (outcome_im in outcomes_im) {
  ols_im  <- get_eb(outcome_im, "OLS",   "Industry")
  borda_im <- get_eb(outcome_im, "Borda", "Industry")

  merged <- merge(
    ols_im[, c("entity_id", "entity", "eb")],
    borda_im[, c("entity_id", "eb")],
    by = "entity_id", suffixes = c("_ols", "_borda")
  )
  names(merged) <- c("industry_id", "industry_code", "EB_OLS", "EB_Borda")

  # map codes to readable names
  merged$industry <- ind_name_lookup[as.character(merged$industry_id)]
  stopifnot(!any(is.na(merged$industry)))

  # sort by Borda EB
  merged <- merged[order(merged$EB_Borda), ]

  outcome_base <- sub("_im$", "", outcome_im)
  fname <- paste0("within_industry_dualaxis_", outcome_base, ".png")

  make_dual_axis_chart(
    df = merged,
    ols_col = "EB_OLS",
    borda_col = "EB_Borda",
    name_col = "industry",
    x_label = "Industry (sorted by Borda EB)",
    file_name = fname,
    has_gap = FALSE
  )
}
