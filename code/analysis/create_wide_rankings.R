# ------------------------------------------------------------------------------
# Purpose: Create Analysis Data
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
prepare_pltree_data <- function(data, rank_col, subgroup_var, subgroup_filter) {
  
  # Step 1: Ensure input data has necessary columns
  required_cols <- c(rank_col)
  if (!all(required_cols %in% names(data))) {
    stop("The input data must contain the required columns.")
  }
  
  # Step 2: Restrict to subgroup if specified
  if (!is.null(subgroup_var) && !is.null(subgroup_filter)) {
    data <- data %>% dplyr::filter(.data[[subgroup_var]] == subgroup_filter)
  }
  
  if (!is.numeric(data[[rank_col]])) {
    stop(paste("The column", rank_col, "is not numeric. Please check the data."))
  }
  
  # Step 5: Create ID Map
  id_map <- data %>% dplyr::select(firm_id, firm) %>% dplyr::distinct()
  
  # Step 6: Filter data to respondents with rankings for 3+ firms (n() > 2)
  rank_sym <- rlang::sym(rank_col)
  data <- data %>%
    dplyr::filter(!is.na(!!rank_sym)) %>%
    dplyr::group_by(resp_id) %>%
    dplyr::filter(dplyr::n() > 2) %>%
    dplyr::mutate(
      min_rank = min(!!rank_sym, na.rm = TRUE),
      max_rank = max(!!rank_sym, na.rm = TRUE)
    ) %>%
    dplyr::filter(min_rank != max_rank) %>%
    dplyr::ungroup()
  
  # Rating data (RAW)
  data_rating <- data %>%
    dplyr::mutate(rating = !!rank_sym) %>%
    dplyr::select(rating, firm_id, resp_id)
  
  # Step 7: Assign Ranks from Ratings
  data_ranked <- data %>%
    dplyr::group_by(resp_id) %>%
    dplyr::mutate(Rank = dplyr::dense_rank(!!rank_sym)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Rank, firm_id, resp_id)
  
  # Drop respondents with the same firm ranked multiple times
  respondents_to_drop <- data_ranked %>%
    dplyr::group_by(resp_id, firm_id) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) %>%
    dplyr::pull(resp_id)
  
  data_ranked <- data_ranked %>% dplyr::filter(!resp_id %in% respondents_to_drop)
  data_rating <- data_rating %>% dplyr::filter(!resp_id %in% respondents_to_drop)
  
  # Restrict to Leave out Connected Set
  firm_set <- leave_in_connected_set(data_ranked)
  data_ranked <- data_ranked %>% dplyr::filter(firm_id %in% firm_set)
  data_rating <- data_rating %>% dplyr::filter(firm_id %in% firm_set)
  
  # --- NEW: Center + scale ratings (global) ---
  mu <- mean(data_rating$rating, na.rm = TRUE)
  sdv <- stats::sd(data_rating$rating, na.rm = TRUE)
  
  if (!is.finite(mu) || !is.finite(sdv) || sdv <= 0) {
    stop("prepare_pltree_data(): rating standard deviation is 0/NA after filtering; cannot z-score.")
  }
  
  data_rating_z <- data_rating %>%
    dplyr::mutate(rating = (rating - mu) / sdv) 
  
  # Step 8: Pivot to Wide and Clean
  data_wide_pltree <- data_ranked %>%
    tidyr::pivot_wider(names_from = firm_id, values_from = Rank, names_prefix = "firm")
  
  # Replace NA values with 0
  data_wide_pltree[is.na(data_wide_pltree)] <- 0
  
  # Reorder columns by firm ID (NOTE: this will warn if you have non-firm columns like resp_id)
  data_wide_pltree <- data_wide_pltree[, order(as.numeric(sub("firm", "", names(data_wide_pltree))))]
  
  # Step 10: Return
  return(list(
    data_wide_pltree    = data_wide_pltree,
    id_map              = id_map,
    data_rating_long    = data_rating,      # raw rating
    rating_center_mu    = mu,               # <- optional, handy for debugging/repro
    rating_center_sd    = sdv               # <- optional
  ))
}