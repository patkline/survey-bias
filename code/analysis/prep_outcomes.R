prep_outcomes <- function(data, survey_vars) {
  data_wide_list <- list()
  data_long_list <- list()
  id_map_list    <- list()
  
  for (outcome in survey_vars) {
    cat("Preparing:", outcome, "\n")
    prep <- prepare_pltree_data(
      data            = data,
      rank_col        = outcome,
      subgroup_var    = NULL,
      subgroup_filter = NULL
    )
    data_wide_list[[outcome]] <- prep$data_wide_pltree
    data_long_list[[outcome]] <- prep$data_rating_long
    id_map_list[[outcome]]    <- prep$id_map %>% dplyr::filter(!is.na(firm), firm != "nan")
  }
  
  list(wide = data_wide_list, long = data_long_list, id_map = id_map_list)
}
