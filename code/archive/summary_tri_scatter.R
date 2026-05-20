# -------------------------------------------------------------------
# Tri-series scatter: <model> MLE, <model> EB, and Borda (rescaled) on
# the same dual-axis figure, plotted against Borda EB on the x-axis.
# Includes per-series OLS best-fit lines + a 45-degree reference.
# Model-agnostic: pass model = "OLS" today, model = "PL" if/when PL
# coefficients are available again.
# Output: tri_dualaxis_bordaEBx_<outcome>_<model>.png
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))

MODEL_PRIMARY_AXIS <- c(
  PL    = "Plackett--Luce Item Worth",
  OLS   = "Likert Score",
  OLSC  = "Likert Centered Score",
  OL    = "Ordered Logit",
  Borda = "Borda Score"
)
MODEL_LEGEND_PRETTY <- c(
  PL    = "Plackett Luce",
  OLS   = "Likert",
  OLSC  = "Likert Centered",
  OL    = "Ordered Logit",
  Borda = "Borda"
)

write_tri_scatter <- function(dir_path,
                              outcomes,
                              plots_dir,
                              model = "OLS",
                              borda_mult = 1) {
  df_coef_raw <- tryCatch(read_parquet_sheet(dir_path, "Coefficients"),
                          error = function(e) NULL)
  if (is.null(df_coef_raw)) {
    message("⚠️  Coefficients sheet missing — skipping tri-series scatter.")
    return(invisible(NULL))
  }

  df_coef_model <- to_wide_coef(df_coef_raw, model,   value_col = "estimate")
  df_eb_model   <- to_wide_coef(df_coef_raw, model,   value_col = "eb")
  df_coef_borda <- to_wide_coef(df_coef_raw, "Borda", value_col = "estimate")
  df_eb_borda   <- to_wide_coef(df_coef_raw, "Borda", value_col = "eb")

  if (is.null(df_coef_model) || is.null(df_eb_model)) {
    message("⚠️  Model '", model,
            "' MLE/EB coefficients absent — skipping tri-series scatter.")
    return(invisible(NULL))
  }
  if (is.null(df_coef_borda) || is.null(df_eb_borda)) {
    message("⚠️  Borda MLE/EB coefficients absent — skipping tri-series scatter.")
    return(invisible(NULL))
  }

  primary_axis_name <- if (model %in% names(MODEL_PRIMARY_AXIS))
                         MODEL_PRIMARY_AXIS[[model]] else model
  primary_legend    <- if (model %in% names(MODEL_LEGEND_PRETTY))
                         MODEL_LEGEND_PRETTY[[model]] else model

  mle_key <- paste0(primary_legend, " (MLE)")
  eb_key  <- paste0(primary_legend, " (EB)")
  bd_key  <- "Borda Score"

  for (new_outcome in outcomes) {
    cols_needed <- c("firm", "firm_id", new_outcome)
    have_all <- all(cols_needed %in% names(df_coef_model)) &&
                all(cols_needed %in% names(df_eb_model))   &&
                all(cols_needed %in% names(df_coef_borda)) &&
                all(cols_needed %in% names(df_eb_borda))
    if (!have_all) {
      message("⚠️  Outcome '", new_outcome,
              "' missing in one or more sheets — skipping tri-series.")
      next
    }

    coef_m <- df_coef_model[, cols_needed]
    names(coef_m) <- c("firm", "firm_id", "Estimate")

    eb_m <- df_eb_model[, cols_needed]
    names(eb_m) <- c("firm", "firm_id", "EB_model")

    coef_b <- df_coef_borda[, cols_needed]
    names(coef_b) <- c("firm", "firm_id", "Borda")
    coef_b$Borda <- as.numeric(coef_b$Borda) * borda_mult

    eb_b <- df_eb_borda[, cols_needed]
    names(eb_b) <- c("firm", "firm_id", "Borda_EB")
    eb_b$Borda_EB <- as.numeric(eb_b$Borda_EB) * borda_mult

    tri_df <- coef_m %>%
      dplyr::left_join(eb_m,    by = c("firm", "firm_id")) %>%
      dplyr::left_join(coef_b,  by = c("firm", "firm_id")) %>%
      dplyr::left_join(eb_b,    by = c("firm", "firm_id")) %>%
      dplyr::filter(is.finite(Estimate), is.finite(EB_model),
                    is.finite(Borda),    is.finite(Borda_EB))

    if (nrow(tri_df) == 0) {
      message("⚠️  No usable rows for tri-series scatter: ", new_outcome)
      next
    }

    s_pri <- stats::sd(tri_df$Estimate, na.rm = TRUE)
    if (!is.finite(s_pri) || s_pri == 0) s_pri <- 1
    m_pri <- mean(tri_df$Estimate, na.rm = TRUE)
    s_bd  <- stats::sd(tri_df$Borda, na.rm = TRUE)
    if (!is.finite(s_bd) || s_bd == 0) s_bd <- 1
    m_bd  <- mean(tri_df$Borda, na.rm = TRUE)

    tri_df <- tri_df %>%
      dplyr::mutate(Borda_scaled = (Borda - m_bd) / s_bd * s_pri + m_pri)
    inv_to_borda <- function(y) (y - m_pri) / s_pri * s_bd + m_bd
    slope45     <- if (s_bd > 0) s_pri / s_bd else 0
    intercept45 <- m_pri - slope45 * m_bd

    color_values <- setNames(c("#cc7a00", "#4b72a6", "#800000"),
                             c(eb_key, mle_key, bd_key))

    p <- ggplot2::ggplot(tri_df, ggplot2::aes(x = Borda_EB)) +
      ggplot2::geom_point(ggplot2::aes(y = EB_model, color = eb_key),
                          alpha = 0.6, size = 1.8) +
      ggplot2::geom_point(ggplot2::aes(y = Estimate, color = mle_key),
                          alpha = 0.6, size = 1.8) +
      ggplot2::geom_point(ggplot2::aes(y = Borda_scaled, color = bd_key),
                          alpha = 0.6, size = 1.8) +
      ggplot2::geom_smooth(ggplot2::aes(y = EB_model, color = eb_key),
                           method = "lm", se = FALSE, linewidth = 0.9) +
      ggplot2::geom_smooth(ggplot2::aes(y = Estimate, color = mle_key),
                           method = "lm", se = FALSE, linewidth = 0.9) +
      ggplot2::geom_smooth(ggplot2::aes(y = Borda_scaled, color = bd_key),
                           method = "lm", se = FALSE, linewidth = 0.9) +
      ggplot2::geom_abline(slope = slope45, intercept = intercept45,
                           linetype = "dashed", color = "black") +
      ggplot2::scale_y_continuous(
        name = primary_axis_name,
        sec.axis = ggplot2::sec_axis(~ inv_to_borda(.), name = "Borda Score")
      ) +
      ggplot2::scale_x_continuous(name = "Borda Score (EB)", expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = color_values) +
      ggplot2::labs(color = "") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        legend.position  = "top",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = "black"),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA)
      )

    out_file <- file.path(plots_dir,
      paste0("tri_dualaxis_bordaEBx_", new_outcome, "_", tolower(model), ".png"))
    ggplot2::ggsave(out_file, p, width = 12, height = 7, dpi = 300)
    cat("✅ Saved:", basename(out_file), "\n")
  }
}

# Loop over every non-Borda model in the config (Borda is always the
# third series + secondary-axis comparator).
for (m in setdiff(models, "Borda")) {
  write_tri_scatter(
    dir_path  = dir_path,
    outcomes  = outs,
    plots_dir = figures,
    model     = m
  )
}
