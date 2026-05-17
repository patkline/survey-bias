# -------------------------------
# 1) Leave-in connected set (LCC with everyone included)
# -------------------------------
leave_in_connected_set <- function(data) {
  # expects long data with columns: firm_id, resp_id
  firms <- data %>% select(firm_id, resp_id)
  
  edge_df <- full_join(firms, firms, by = "resp_id", relationship = "many-to-many") %>%
    dplyr::filter(firm_id.x < firm_id.y) %>%
    count(firm_id.x, firm_id.y, name = "weight") %>%
    rename(from = firm_id.x, to = firm_id.y)
  
  if (nrow(edge_df) == 0) return(integer(0))
  
  g <- graph_from_data_frame(edge_df, directed = FALSE)
  comp <- components(g)
  lcc  <- which.max(comp$csize)
  
  as.integer(V(g)$name[comp$membership == lcc])
}