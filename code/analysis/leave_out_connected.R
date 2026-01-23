# library(dplyr)
# library(igraph)
# 
# ## ---------------------------------------------------------------------------
# ## Your existing helper (unchanged)
# ## ---------------------------------------------------------------------------
# leave_out_connected <- function(data) {
#   
#   firms   <- data %>% select(firm_id, resp_id)
#   
#   edge_df <- full_join(firms, firms, by = "resp_id",
#                        relationship = "many-to-many") %>% 
#     filter(firm_id.x < firm_id.y) %>%                         # undirected edge
#     count(firm_id.x, firm_id.y, name = "weight") %>%          # #appearances
#     rename(from = firm_id.x, to = firm_id.y)
#   
#   g <- graph_from_data_frame(edge_df, directed = FALSE)
#   
#   comp <- components(g)
#   lcc  <- which.max(comp$csize)                 # id of largest component
#   
#   as.integer(V(g)$name[comp$membership == lcc]) # return the firm‑id vector
# }
# 
# ## ---------------------------------------------------------------------------
# ## NEW:  leave_out_connected_set()   – intersection across leave‑one‑outs
# ## ---------------------------------------------------------------------------
# leave_out_connected_set <- function(data, verbose = TRUE) {
#   
#   # 1) Largest CC in the *full* data --------------------------
#   keep_set <- leave_out_connected(data)
#   if (verbose) cat("Full sample LCC size  :", length(keep_set), "\n")
#   
#   # 2) Loop over respondents, leave one out each time ---------
#   for (r in unique(data$resp_id)) {
#     
#     tmp <- data %>% filter(resp_id != r)         # drop one respondent
#     
#     lcc_r <- leave_out_connected(tmp)            # LCC of reduced data
#     
#     # shrink intersection
#     keep_set <- intersect(keep_set, lcc_r)
#     
#     if (verbose)
#       cat(sprintf(" after dropping resp %-5s : |intersection| = %d\n",
#                   r, length(keep_set)))
#     
#     # early stop – if empty there is no leave‑out connected set
#     if (length(keep_set) == 0) break
#   }
#   
#   return(keep_set)                               # vector of firm_ids
# }

library(parallel)
library(dplyr)
library(igraph)

# your existing helper that, given a long‐format slice, returns the firms
# in the largest connected component
leave_out_connected <- function(data) {
  firms <- data %>% select(firm_id, resp_id)
  edge_df <- full_join(firms, firms, by="resp_id", relationship="many-to-many") %>%
    filter(firm_id.x < firm_id.y) %>%
    count(firm_id.x, firm_id.y, name="weight") %>%
    rename(from=firm_id.x, to=firm_id.y)
  g <- graph_from_data_frame(edge_df, directed=FALSE)
  comp <- components(g)
  big <- which.max(comp$csize)
  as.integer(V(g)$name[comp$membership == big])
}

leave_out_connected_set <- function(data, verbose = FALSE) {
  # figure out how many workers
  cores <- detectCores() - 1
  
  # 1) full‐sample LCC
  keep_set <- leave_out_connected(data)
  if (verbose) cat("Full‐sample LCC size:", length(keep_set), "\n")
  
  # 2) for each respondent, drop them & compute LCC
  resp_ids <- unique(data$resp_id)
  # if (cores > 1 && .Platform$OS.type!="windows") {
  #   lcc_list <- mclapply(resp_ids, function(r) {
  #     leave_out_connected(filter(data, resp_id != r))
  #   }, mc.cores = cores)
  # } else {
  #   lcc_list <- lapply(resp_ids, function(r) {
  #     leave_out_connected(filter(data, resp_id != r))
  #   })
  # }
  
  lcc_list <- lapply(resp_ids, function(r) {
    leave_out_connected(filter(data, resp_id != r))})
  
  # 3) intersect them all
  keep_set <- Reduce(intersect, lcc_list, init=keep_set)
  
  # 4) verbose per‐resp reporting
  if (verbose) {
    for (i in seq_along(resp_ids)) {
      cat(sprintf(" after dropping resp %-5s: LCC ∩ size = %d\n",
                  resp_ids[i], length(lcc_list[[i]])))
    }
    cat("Final leave‐out‐connected set size:", length(keep_set), "\n")
  }
  
  return(keep_set)
}



