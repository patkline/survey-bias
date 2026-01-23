# ============================================================
# Helpers: extract worths (alpha) and tie params (delta) from a PlackettLuce fit
# ============================================================
pluck_alpha_delta <- function(fit) {
  a <- itempar(fit, log = FALSE)              # worths on natural scale
  nm_raw <- names(a)
  nm_num <- sub("^\\D+", "", nm_raw)          # strip "firm" etc to keep numeric ids as well
  alpha_vec <- as.numeric(a)
  
  alpha <- setNames(alpha_vec, nm_raw)
  alpha2 <- setNames(alpha_vec, nm_num)
  alpha <- c(alpha, alpha2)
  alpha <- alpha[!duplicated(names(alpha), fromLast = TRUE)]  # ensure unique names
  
  co <- coef(summary(fit))
  tie_rows <- grep("^tie\\d+$", rownames(co))
  delta <- c("1" = 1)
  if (length(tie_rows)) {
    d <- exp(co[tie_rows, "Estimate"])        # natural scale
    names(d) <- sub("^tie", "", rownames(co)[tie_rows])
    delta <- c(delta, d)
  }
  list(alpha = alpha, delta = delta)
}

# ============================================================
# Robust subset generator for a remaining set R and size k
# (guards against combn() returning a vector)
# ============================================================
.subsets_of_size <- function(R, k) {
  if (k == 1L) return(lapply(R, function(x) as.integer(x)))
  if (k > length(R)) return(list())
  idx <- combn(seq_along(R), k, simplify = TRUE)
  if (is.null(dim(idx))) idx <- matrix(idx, nrow = k, ncol = 1L)
  lapply(seq_len(ncol(idx)), function(j) as.integer(R[idx[, j]]))
}

# ============================================================
# Simulate ONE ordered partition (weak ordering) for 'items'
# under PL-with-ties (Butler–Fienberg parameterization)
# ============================================================
simulate_ordered_partition <- function(items, alpha, delta) {
  items <- as.integer(items)
  R <- items
  blocks <- list()
  
  names(alpha) <- as.character(names(alpha))
  get_a <- function(s) prod(alpha[as.character(s)])
  
  max_k <- max(as.integer(names(delta)))
  get_delta <- function(k) {
    if (k == 1L) return(1)
    val <- suppressWarnings(as.numeric(delta[as.character(k)]))
    if (is.na(val)) 0 else val
  }
  
  while (length(R) > 0L) {
    m <- length(R)
    cand_S <- list(); cand_w <- numeric(0)
    
    for (k in 1:min(max_k, m)) {
      dk <- get_delta(k); if (dk <= 0) next
      SS <- .subsets_of_size(R, k)
      if (!length(SS)) next
      wts <- vapply(SS, function(S) dk * (get_a(S))^(1/k), numeric(1))
      cand_S <- c(cand_S, SS)
      cand_w <- c(cand_w, wts)
    }
    
    tot <- sum(cand_w)
    if (!is.finite(tot) || tot <= 0) {
      stop("Simulation denominator collapsed; check worth and tie parameters.")
    }
    pick <- sample.int(length(cand_w), size = 1L, prob = cand_w)
    S <- as.integer(cand_S[[pick]])
    blocks[[length(blocks) + 1L]] <- sort(S)
    R <- setdiff(R, S)
  }
  blocks
}

# ============================================================
# Borda from ordered partition for a single choice set
# (normalize = TRUE divides by k-1)
# ============================================================
borda_from_blocks <- function(items, blocks, normalize = TRUE) {
  items <- as.integer(items)
  k <- length(items)
  rmap <- integer(k); names(rmap) <- as.character(items)
  for (g in seq_along(blocks)) rmap[as.character(blocks[[g]])] <- g
  r <- rmap[as.character(items)]
  wins <- vapply(r, function(ri) sum(r > ri), numeric(1))
  ties <- vapply(r, function(ri) sum(r == ri) - 1L, numeric(1))
  B <- wins + 0.5 * ties
  if (normalize && k > 1) B <- B / (k - 1)
  setNames(as.numeric(B), as.character(items))
}

# ============================================================
# Extract observed choice sets from WIDE rankings
# (resp_id + firm1..firmN; values > 0 mean "ranked/seen")
# ============================================================
extract_choice_sets <- function(data_wide, id_var = "resp_id") {
  firm_cols <- grep("^firm\\d+$", names(data_wide), value = TRUE)
  X <- data_wide[, firm_cols, drop = FALSE]
  X[] <- lapply(X, function(v) suppressWarnings(as.numeric(v)))
  apply(X, 1, function(row) {
    cols <- names(row)[which(is.finite(row) & row > 0)]
    sort(as.integer(sub("^firm", "", cols)))
  })
}

# ============================================================
# MAIN: Iteration-level simulation of firm Borda
#  - Each iteration draws ONE ranking per respondent's observed choice set
#  - Aggregates to firm-level mean Borda for that iteration
#  - Returns mean & SD across iterations (+ optional per-iteration matrix)
# ============================================================
simulate_borda_iterations <- function(fit,
                                      data_wide,
                                      id_map = NULL,
                                      B = 500,
                                      id_var = "resp_id",
                                      normalize = TRUE,
                                      seed = NULL,
                                      return_iter_matrix = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  pars <- pluck_alpha_delta(fit)
  alpha <- pars$alpha
  delta <- pars$delta
  
  # observed choice sets (one per row / respondent)
  choice_sets <- extract_choice_sets(data_wide, id_var = id_var)
  
  # universe of firms actually appearing in any set
  all_items <- sort(unique(as.integer(unlist(choice_sets))))
  J <- length(all_items)
  item_ch <- as.character(all_items)
  
  # Precompute which respondents saw each firm (index lists)
  resp_saw <- lapply(item_ch, function(fid) {
    which(vapply(choice_sets, function(cs) as.character(fid) %in% as.character(cs), logical(1)))
  })
  names(resp_saw) <- item_ch
  
  # sanity: worths exist for all items?
  missing <- setdiff(item_ch, names(alpha))
  if (length(missing)) {
    stop("Missing worths for items: ", paste(missing, collapse = ", "),
         ". Ensure itempar(fit) names match firm IDs (e.g., 'firm12' or '12').")
  }
  
  # container for per-iteration firm means: B x J
  iter_mat <- matrix(NA_real_, nrow = B, ncol = J, dimnames = list(NULL, item_ch))
  
  # ---- simulation loop over iterations ----
  for (b in seq_len(B)) {
    # simulate ONE ranking for EACH respondent's observed set
    # and accumulate Borda for firms they saw
    # We'll store respondent-level Borda vectors in a list, then aggregate.
    resp_B_list <- vector("list", length(choice_sets))
    
    for (i in seq_along(choice_sets)) {
      items_i <- choice_sets[[i]]
      if (!length(items_i)) { resp_B_list[[i]] <- NULL; next }
      blocks <- simulate_ordered_partition(items_i, alpha, delta)
      B_vec  <- borda_from_blocks(items_i, blocks, normalize = normalize)  # named by firm_id (char)
      resp_B_list[[i]] <- B_vec
    }
    
    # Aggregate to firm-level mean (over respondents who saw the firm)
    # Efficiently: for each firm j, pull the B value from those respondents
    firm_means_b <- numeric(J); names(firm_means_b) <- item_ch
    for (j in seq_along(item_ch)) {
      fid <- item_ch[j]
      idx <- resp_saw[[j]]
      if (!length(idx)) { firm_means_b[j] <- NA_real_; next }
      vals <- vapply(idx, function(ii) {
        v <- resp_B_list[[ii]]
        if (is.null(v)) return(NA_real_)
        v[fid]
      }, numeric(1))
      firm_means_b[j] <- mean(vals, na.rm = TRUE)
    }
    
    iter_mat[b, ] <- firm_means_b
  }
  
  # summary across iterations
  mean_B <- colMeans(iter_mat, na.rm = TRUE)
  sd_B   <- apply(iter_mat, 2, sd, na.rm = TRUE)
  
  firm_summary <- data.frame(
    firm_id = as.integer(names(mean_B)),
    mean_B  = as.numeric(mean_B),
    sd_B    = as.numeric(sd_B),
    mc_se   = as.numeric(sd_B) / sqrt(B),
    stringsAsFactors = FALSE
  )
  
  # attach firm names if id_map supplied
  if (!is.null(id_map) && all(c("firm_id","firm") %in% names(id_map))) {
    firm_summary <- dplyr::left_join(firm_summary,
                                     id_map[, c("firm_id","firm")],
                                     by = "firm_id")
  }
  
  out <- list(firm_summary = firm_summary)
  if (isTRUE(return_iter_matrix)) out$iter_matrix <- iter_mat
  out
}
