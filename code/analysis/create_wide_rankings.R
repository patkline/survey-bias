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
  
  #reference_firm_name <- "KFC"
  
  # Check for reference firm
  # if (!toupper(reference_firm_name) %in% data$firm) {
  #   stop(paste("Reference firm '", reference_firm_name, "' not found in the dataset after cleaning."))
  # }
  
  if (!is.numeric(data[[rank_col]])) {
    stop(paste("The column", rank_col, "is not numeric. Please check the data."))
  }
  
  # Step 5: Create ID Map
  id_map <- data %>% select(firm_id, firm) %>% distinct()
  
  # Step 6: Filter data to respondents with rankings for 5 firms
  rank_sym <- sym(rank_col)
  data <- data %>%
    dplyr::filter(!is.na(!!rank_sym)) %>%
    group_by(resp_id) %>%
    dplyr::filter(n() > 2) %>%
    dplyr::mutate(
      min_rank = min(!!rank_sym, na.rm = TRUE),
      max_rank = max(!!rank_sym, na.rm = TRUE)
    ) %>%
    dplyr::filter(min_rank != max_rank) %>%
    ungroup() 
  
  # Step 7: Assign Ranks from Ratings
  data_ranked <- data %>%
    group_by(resp_id) %>%
    mutate(Rank = dense_rank(!!rank_sym)) %>%
    ungroup() %>%
    select(Rank, firm_id, resp_id)
  
  # Drop respondents with the same firm ranked multiple times
  respondents_to_drop <- data_ranked %>%
    group_by(resp_id, firm_id) %>%
    summarise(n = n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) %>%
    pull(resp_id)
  
  data_ranked <- data_ranked %>%
    dplyr::filter(!resp_id %in% respondents_to_drop)
  
  # Restrict to Leave out Connected Set
  firm_set <- leave_in_connected_set(data_ranked)
  data_ranked <- data_ranked %>% dplyr::filter(firm_id %in% firm_set)

  # Step 8: Pivot to Wide and Clean
  data_wide_pltree <- data_ranked %>%
    pivot_wider(names_from = firm_id, values_from = Rank, names_prefix = "firm")
  
  # Replace NA values with 0
  data_wide_pltree[is.na(data_wide_pltree)] <- 0
  
  # Reorder columns by firm ID
  data_wide_pltree <- data_wide_pltree[, order(as.numeric(sub("firm", "", names(data_wide_pltree))))]
  
  # Step 10: Return
  return(list(data_wide_pltree = data_wide_pltree, id_map = id_map))
}
