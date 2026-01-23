# ------------------------------------------------------------
# Pairwise strict wins from WIDE rankings (with id_map)
# - data_wide: columns "resp_id", "firm1"..."firmN"; values 0 (not ranked) or 1..5
# - id_map: data.frame with columns firm_id (integer) and firm (character label)
# - id_var: respondent id column name
# - higher_is_better: if FALSE, lower ranks are better (default)
# - include_ids: if TRUE, also return firmA_id and firmB_id
# Returns:
#   if include_ids=FALSE:
#     tibble(firmA, firmB, strict_wins, strict_losses, ties, total_comparisons)
#   if include_ids=TRUE:
#     tibble(firmA, firmB, firmA_id, firmB_id, strict_wins, strict_losses, ties, total_comparisons)
#
# Notes:
#   - "strict_wins" is from firmA's perspective.
#   - "total_comparisons" counts all pairwise observations where both ranks are present,
#     i.e., wins + losses + ties (ties ARE included in the denominator).
# ------------------------------------------------------------
compute_pairwise_strictwins_wide <- function(data_wide,
                                             id_map,
                                             id_var = "resp_id",
                                             higher_is_better = FALSE,
                                             include_ids = FALSE) {
  stopifnot(id_var %in% names(data_wide))
  stopifnot(all(c("firm_id","firm") %in% names(id_map)))
  
  # Firm columns present in the wide data
  firm_cols <- setdiff(names(data_wide), id_var)
  
  # Extract numeric firm IDs from column names like "firm23"
  firm_ids <- suppressWarnings(as.integer(sub("^firm", "", firm_cols)))
  if (anyNA(firm_ids)) {
    stop("All firm columns must be named like 'firm##' (e.g., firm1, firm2, ...).")
  }
  
  # Ensure id_map covers the firms actually present
  if (!all(firm_ids %in% id_map$firm_id)) {
    miss <- setdiff(firm_ids, id_map$firm_id)
    stop("id_map is missing firm_id(s): ", paste(miss, collapse = ", "))
  }
  
  # Numeric matrix of rankings; treat 0/negative as missing
  X <- data_wide[, firm_cols, drop = FALSE]
  X[] <- lapply(X, function(x) suppressWarnings(as.numeric(x)))
  M <- as.matrix(X)
  M[M <= 0] <- NA_real_
  
  # All unordered pairs of firm columns
  pairs <- combn(seq_along(firm_cols), 2)
  
  one_pair <- function(i, j) {
    a <- M[, i]; b <- M[, j]
    ok <- is.finite(a) & is.finite(b)
    if (!any(ok)) return(NULL)
    
    a_ok <- a[ok]; b_ok <- b[ok]
    gt <- a_ok > b_ok
    lt <- a_ok < b_ok
    eq <- a_ok == b_ok
    
    if (higher_is_better) {
      winsA   <- sum(gt)
      lossesA <- sum(lt)
    } else {
      winsA   <- sum(lt)
      lossesA <- sum(gt)
    }
    tiesA  <- sum(eq)
    total  <- length(a_ok)  # includes ties
    
    list(
      firmA_id          = firm_ids[i],
      firmB_id          = firm_ids[j],
      strict_wins       = as.integer(winsA),
      strict_losses     = as.integer(lossesA),
      ties              = as.integer(tiesA),
      total_comparisons = as.integer(total)
    )
  }
  
  res_list <- vector("list", ncol(pairs))
  for (k in seq_len(ncol(pairs))) {
    i <- pairs[1, k]; j <- pairs[2, k]
    res_list[[k]] <- one_pair(i, j)
  }
  res_list <- Filter(Negate(is.null), res_list)
  out <- bind_rows(res_list)
  
  # Attach readable names
  out <- out %>%
    left_join(id_map %>% select(firmA_id = firm_id, firmA = firm), by = "firmA_id") %>%
    left_join(id_map %>% select(firmB_id = firm_id, firmB = firm), by = "firmB_id")
  
  if (include_ids) {
    out <- out %>%
      select(firmA, firmB, firmA_id, firmB_id,
             strict_wins, strict_losses, ties, total_comparisons)
  } else {
    out <- out %>%
      select(firmA, firmB,
             strict_wins, strict_losses, ties, total_comparisons)
  }
  
  out
}
