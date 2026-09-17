# ------------------------------------------------------------------------------
# Purpose: Identify respondents eligible for at least one firm-rating outcome
# ------------------------------------------------------------------------------

prepare_sample_eligibility_data <- function(
    data,
    rank_col,
    subgroup_var = NULL,
    subgroup_filter = NULL
) {
  if (!rank_col %in% names(data)) {
    stop("The input data must contain the requested outcome column.")
  }

  if (!is.null(subgroup_var) && !is.null(subgroup_filter)) {
    data <- data %>% dplyr::filter(.data[[subgroup_var]] == subgroup_filter)
  }

  if (!is.numeric(data[[rank_col]])) {
    stop(paste("The column", rank_col, "is not numeric. Please check the data."))
  }

  rank_sym <- rlang::sym(rank_col)
  data_ranked <- data %>%
    dplyr::filter(!is.na(!!rank_sym)) %>%
    dplyr::group_by(resp_id) %>%
    dplyr::filter(dplyr::n() > 2) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(resp_id) %>%
    dplyr::mutate(Rank = dplyr::dense_rank(!!rank_sym)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Rank, firm_id, resp_id)

  respondents_to_drop <- data_ranked %>%
    dplyr::group_by(resp_id, firm_id) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) %>%
    dplyr::pull(resp_id)

  data_wide <- data_ranked %>%
    dplyr::filter(!resp_id %in% respondents_to_drop) %>%
    tidyr::pivot_wider(
      names_from = firm_id,
      values_from = Rank,
      names_prefix = "firm"
    )

  data_wide[is.na(data_wide)] <- 0
  firm_cols <- grep("^firm[0-9]+$", names(data_wide), value = TRUE)
  firm_cols <- firm_cols[order(as.integer(sub("^firm", "", firm_cols)))]
  data_wide <- data_wide[
    , c(firm_cols, setdiff(names(data_wide), firm_cols)), drop = FALSE
  ]

  data_wide
}
