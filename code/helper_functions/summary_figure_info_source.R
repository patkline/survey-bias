make_information_source_hist <- function(df,
                                         id_var  = "ResponseId",
                                         outfile = "information_source_hist.png") {
  # Require needed columns
  if (!id_var %in% names(df)) {
    stop("ID variable '", id_var, "' not found in data.")
  }
  if (!"information_source" %in% names(df)) {
    stop("'information_source' column not found in data.")
  }
  
  # Identify conduct-arm respondents using any available conduct-based columns
  conduct_candidates <- c("conduct_favor_white", "conduct_favor_male",
                          "conduct_white", "conduct_male")
  conduct_vars <- intersect(conduct_candidates, names(df))
  if (length(conduct_vars) == 0L) {
    stop("No conduct-arm indicator columns found (e.g. conduct_favor_white / conduct_favor_male).")
  }
  
  # Filter to rows where ANY of the conduct_vars is non-missing
  df_cond <- df %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(conduct_vars), ~ !is.na(.)))
  
  if (nrow(df_cond) == 0L) {
    stop("No respondents remain after restricting to the conduct arm.")
  }
  
  # One row per respondent
  df_resp <- df_cond %>%
    dplyr::arrange(.data[[id_var]]) %>%
    dplyr::distinct(.data[[id_var]], .keep_all = TRUE)
  
  # Normalize information_source: turn NA into ""
  df_resp <- df_resp %>%
    dplyr::mutate(
      info_raw = dplyr::if_else(is.na(.data$information_source),
                                "",
                                .data$information_source)
    )
  
  # Drop respondents with no information_source response
  df_nonmiss <- df_resp %>%
    dplyr::filter(info_raw != "")
  
  if (nrow(df_nonmiss) == 0L) {
    stop("No non-missing information_source responses in the conduct arm.")
  }
  
  # Categories to track
  categories <- c(
    "Personal experience",
    "Current or former coworkers",
    "Friends or family",
    "Internet or social media",
    "Other"
  )
  
  # Create indicator columns (1 if category selected, 0 otherwise)
  indicator_cols <- character(length(categories))
  for (i in seq_along(categories)) {
    cat_label <- categories[i]
    col_name  <- paste0("cat_", i)
    indicator_cols[i] <- col_name
    
    df_nonmiss[[col_name]] <- ifelse(
      grepl(cat_label, df_nonmiss$info_raw, fixed = TRUE),
      1L, 0L
    )
  }
  
  # Shares for each of the 5 options (percent of non-missing respondents)
  shares_cat <- df_nonmiss %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(indicator_cols),
                    ~ mean(.x, na.rm = TRUE) * 100)
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "cat_name",
      values_to = "share"
    ) %>%
    dplyr::mutate(
      category = categories[match(cat_name, indicator_cols)]
    ) %>%
    dplyr::select(category, share)
  
  # Final plotting data (no "Missing" category)
  plot_df <- shares_cat %>%
    dplyr::mutate(
      category = factor(category, levels = categories)
    )
  
  # Bar chart of shares with y-range 0–100
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = category, y = share)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                breaks = seq(0, 100, by = 10),
                                expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL,
      y = "Share of Respondents (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.x = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(outfile, p, width = 8, height = 5, dpi = 300)
  
  p
}
