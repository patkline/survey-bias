source("~/Documents/consolidated_code_server/code/1_preprocessing_v3.R")

## ---------- 1) ONE comb_id -> ordered partition R ----------
## df_one must have columns: item, ranking (just a single comb_id)
## Returns: list(C1, C2, ..., CJ) where each Cj is an integer vector of items

# ----- helper: all k-subsets of a vector R (robust to k=1) -----
.subsets_of_size <- function(R, k) {
  if (k == 1L) return(lapply(R, function(x) x))
  if (k > length(R)) return(list())
  idx <- combn(seq_along(R), k, simplify = TRUE)
  if (is.null(dim(idx))) idx <- matrix(idx, nrow = k, ncol = 1L)
  lapply(seq_len(ncol(idx)), function(j) R[idx[, j]])
}

# ----- f(S) = delta_|S| * (prod alpha_i)^(1/|S|) -----
f_PL <- function(S, alpha, delta) {
  if (length(S) == 0L) return(1)          # by convention, empty -> 1
  k <- length(S)
  # lookup worths; allow integer or character IDs
  a <- as.numeric(alpha[as.character(S)])
  if (any(is.na(a))) {
    miss <- S[is.na(a)]
    stop("Missing worth(s) for item(s): ", paste(miss, collapse = ", "))
  }
  d_k <- if (k == 1L) 1 else suppressWarnings(as.numeric(delta[as.character(k)]))
  if (is.na(d_k)) d_k <- 0               # tie size not allowed -> weight 0
  d_k * (prod(a))^(1 / k)
}

# ----- P(R): probability of ordered partition R = list(C1, C2, ..., CJ) -----
# R: list of vectors; each vector is the (tied) set chosen at that stage
# items: optional full set of available items; if NULL, uses the union of R
# alpha: named vector of worths (names = item ids)
# delta: named vector of tie parameters; names "1","2","3",...
# D: maximum allowed tie order (if NULL, uses max name(delta))
P_PL <- function(R, items = NULL, alpha, delta, D = NULL) {
  # coerce item IDs to character once (for stable name-based lookup)
  names(alpha) <- as.character(names(alpha))
  names(delta) <- as.character(names(delta))
  
  # set universe of items
  if (is.null(items)) {
    items <- unique(unlist(R, use.names = FALSE))
  }
  items <- as.vector(items)
  
  # basic checks: R must be a partition of 'items'
  if (length(unique(unlist(R))) != length(unlist(R))) {
    stop("Blocks in R overlap: an item appears in more than one block.")
  }
  if (!setequal(as.character(unlist(R)), as.character(items))) {
    stop("Union of blocks in R does not equal 'items'.")
  }
  
  # maximum tie order allowed
  Dmax <- if (is.null(D)) {
    max(1L, suppressWarnings(max(as.integer(names(delta)), na.rm = TRUE)))
  } else as.integer(D)
  
  remain <- items
  prob <- 1
  
  for (j in seq_along(R)) {
    Cj <- R[[j]]
    # numerator
    num <- f_PL(Cj, alpha, delta)
    
    # denominator: sum over k = 1..min(|remain|, Dmax) of sum_{S subset size k} f(S)
    Dj <- length(remain)
    Kmax <- min(Dj, Dmax)
    denom <- 0
    for (k in 1:Kmax) {
      SS <- .subsets_of_size(remain, k)
      if (!length(SS)) next
      denom <- denom + sum(vapply(SS, f_PL, numeric(1), alpha = alpha, delta = delta))
    }
    
    if (!is.finite(denom) || denom <= 0) return(0)  # impossible under current params
    prob <- prob * (num / denom)
    
    # remove chosen set for next stage
    remain <- setdiff(remain, Cj)
  }
  prob
}


long_to_R_one <- function(df_one) {
  stopifnot(all(c("item","ranking") %in% names(df_one)))
  # enforce integers & sort nicely
  df <- df_one[, c("item","ranking")]
  df$item    <- as.integer(df$item)
  df$ranking <- as.integer(df$ranking)
  
  # make ranks consecutive (e.g., {2,4} -> {1,2})
  rks <- sort(unique(df$ranking))
  rk_map <- setNames(seq_along(rks), rks)
  df$rk <- rk_map[as.character(df$ranking)]
  
  # split by rank; items within a block sorted
  split_df <- split(df$item, df$rk)
  lapply(split_df, function(v) sort(as.integer(v)))
}

## ---------- 2) Choice set with MANY comb_id's -> list of R's ----------
## long_df must have: comb_id, item, ranking (one choice set at a time)
## Returns: named list, each element is an R for that comb_id
long_to_R_list <- function(long_df) {
  stopifnot(all(c("comb_id","item","ranking") %in% names(long_df)))
  sp <- split(long_df[, c("item","ranking")], long_df$comb_id)
  out <- lapply(sp, long_to_R_one)
  # name by comb_id (as integers if possible)
  nm <- names(out)
  names(out) <- if (all(!is.na(as.integer(nm)))) as.integer(nm) else nm
  out
}

## ---------- 3) Items vector for the choice set (for P_PL 'items=') ----------
## long_df must have: item (one choice set at a time)
extract_items_from_long <- function(long_df) {
  sort(unique(as.integer(long_df$item)))
}


# ---- Pure-R: generate all set partitions via restricted growth strings (RGS) ----
# Returns a matrix m x Bell(m); each column is a labeling in 1..k indicating blocks.
setparts_pureR <- function(m) {
  if (m < 1) stop("m must be >= 1")
  if (m == 1) return(matrix(1L, nrow = 1, ncol = 1))
  
  res <- vector("list", 0L)
  a <- integer(m); a[1] <- 1L
  
  backtrack <- function(i, maxlab) {
    if (i > m) {
      res[[length(res) + 1L]] <<- a
      return(invisible(NULL))
    }
    for (lab in 1:(maxlab + 1L)) {
      a[i] <<- lab
      backtrack(i + 1L, max(maxlab, lab))
    }
  }
  backtrack(2L, 1L)
  mat <- do.call(cbind, res)
  storage.mode(mat) <- "integer"
  mat
}

# ---- Weak orderings (ordered partitions) for a given set of item IDs (e.g. c(10,12,18,25,31)) ----
# No external packages. Works fine for m <= ~6 (Fubini(6)=46,206).
weak_orderings_from_items <- function(items) {
  items <- as.integer(items)
  m <- length(items)
  if (m == 0L) return(list(list()))
  if (m == 1L) return(list(list(items)))
  
  # all set partitions as labelings 1..k (columns)
  sp <- setparts_pureR(m)
  
  # tiny permutations helper: permutations of 1..k as a matrix
  perm_mat <- function(k) {
    if (k <= 1L) return(matrix(1L, nrow = 1L))
    res <- list()
    used <- rep(FALSE, k)
    cur <- integer(k)
    rec <- function(d) {
      if (d > k) { res[[length(res)+1L]] <<- cur; return() }
      for (v in 1:k) if (!used[v]) {
        used[v] <<- TRUE; cur[d] <<- v
        rec(d+1L)
        used[v] <<- FALSE
      }
    }
    rec(1L)
    do.call(rbind, lapply(res, function(v) matrix(v, nrow = 1L)))
  }
  
  out <- vector("list", 0L)
  for (j in seq_len(ncol(sp))) {
    lab <- sp[, j]           # length m, labels 1..k
    k <- max(lab)
    P <- perm_mat(k)
    for (r in seq_len(nrow(P))) {
      perm <- P[r, ]
      blocks <- vector("list", k)
      for (pos in seq_len(k)) {
        lbl <- perm[pos]
        blocks[[pos]] <- items[lab == lbl]
      }
      # sort within blocks for a canonical representation
      out[[length(out) + 1L]] <- lapply(blocks, sort)
    }
  }
  out
}

# ---- Tidy DF for ONE choice set: columns comb_id, item, rank (rank 1 = top block) ----
weak_orderings_df_one <- function(items) {
  ords <- weak_orderings_from_items(items)
  res <- vector("list", length(ords))
  for (i in seq_along(ords)) {
    blk <- ords[[i]]
    rows <- lapply(seq_along(blk), function(rk) {
      data.frame(comb_id = i, item = as.integer(blk[[rk]]), rank = rk)
    })
    res[[i]] <- do.call(rbind, rows)
  }
  do.call(rbind, res)
}


# Long DF for ONE set of items
# columns: comb_id, item, ranking  (ranking = 1 for first group, 2 for second, ...)
weak_orderings_long_one <- function(items) {
  ords <- weak_orderings_from_items(items)  # your function
  out <- vector("list", length(ords))
  for (i in seq_along(ords)) {
    blocks <- ords[[i]]
    out[[i]] <- do.call(rbind, lapply(seq_along(blocks), function(g) {
      data.frame(comb_id = i,
                 item    = as.integer(blocks[[g]]),
                 ranking = g)
    }))
  }
  do.call(rbind, out)
}

extract_choice_sets <- function(data_wide) {
  firm_cols <- grep("^firm\\d+$", names(data_wide), value = TRUE)
  X <- data_wide[, firm_cols, drop = FALSE]
  
  # ensure numeric
  X[] <- lapply(X, function(v) suppressWarnings(as.numeric(v)))
  
  # for each row, collect firm ids with ranking > 0
  sets_list <- apply(X, 1, function(row) {
    present_cols <- names(row)[which(is.finite(row) & row > 0)]
    sort(as.integer(sub("^firm", "", present_cols)))
  })
  
  # return list of integer vectors (duplicates allowed)
  sets_list
}

extract_alpha_delta <- function(fit) {
  # item worths on natural scale
  a_raw <- itempar(fit, log = FALSE)      # named vector, names like "firm12" etc
  alpha_vec <- as.numeric(a_raw)
  nm_raw <- names(a_raw)
  
  # also provide numeric-id names (e.g., "12") in addition to "firm12"
  nm_num <- suppressWarnings(as.integer(sub("\\D+", "", nm_raw)))
  alpha_named1 <- setNames(alpha_vec, nm_raw)
  alpha_named2 <- setNames(alpha_vec, as.character(nm_num))
  alpha <- c(alpha_named1, alpha_named2)
  alpha <- alpha[!duplicated(names(alpha), fromLast = TRUE)]
  
  # tie parameters: pull from coef(summary(fit)) and exponentiate
  co <- as.data.frame(coef(summary(fit)))
  rn <- rownames(co)
  
  # rows that look like "tie2", "tie3", ... OR "phi" notation (e.g., "log(phi.2)")
  tie_idx <- grep("^tie\\d+$|phi", rn)
  delta <- c("1" = 1)  # 1-way tie (singleton) is 1 by definition
  
  if (length(tie_idx)) {
    est <- co[tie_idx, "Estimate"]
    labs <- rn[tie_idx]
    
    # map labels -> k (tie order)
    k <- suppressWarnings(as.integer(gsub("\\D", "", labs)))
    # natural scale
    d_vals <- exp(est)
    names(d_vals) <- as.character(k)
    # keep the largest k for any duplicates
    keep <- !duplicated(names(d_vals), fromLast = TRUE)
    d_vals <- d_vals[keep]
    
    delta <- c(delta, d_vals)
    delta <- delta[!duplicated(names(delta), fromLast = TRUE)]
  }
  
  list(alpha = alpha, delta = delta)
}

probs_for_choice_long <- function(fit, long_choice_df) {
  pars   <- extract_alpha_delta(fit)
  alpha  <- pars$alpha
  delta  <- pars$delta
  
  items  <- extract_items_from_long(long_choice_df)
  R_list <- long_to_R_list(long_choice_df)
  
  probs <- vapply(R_list,
                  function(R) P_PL(R, items = items, alpha = alpha, delta = delta),
                  numeric(1))
  data.frame(comb_id = as.integer(names(probs)),
             probability = as.numeric(probs),
             row.names = NULL)
}

pl_to_borda <- function(data_wide, id_map, mod_mle) {
  data_wide <- data_wide %>% select(-resp_id)
  n <- nrow(data_wide)
  
  totals <-NULL
  
  for (i in 1:n) {
    print(i)
    sets_list <- extract_choice_sets(data_wide[i,])
    df_choice<-weak_orderings_long_one(sets_list)
    
    df_choice_wide <- df_choice %>%
      tidyr::pivot_wider(names_from = item, values_from = ranking, names_prefix = "firm")
    df_choice_borda <- compute_borda_individual_wide(df_choice_wide, id_map, id_var = "comb_id") 
    
    items_vec <- extract_items_from_long(df_choice)           # items
    R_list    <- long_to_R_list(df_choice)                    # each comb_id -> R
    df_choice_prob <- probs_for_choice_long(mod_mle, df_choice)
    
    df_merged <- left_join(df_choice_borda, df_choice_prob, by="comb_id")
    df_merged <- df_merged %>% mutate(weighted_borda = B*probability)
    firm_totals <- df_merged %>%
      dplyr::group_by(firm_id) %>%
      dplyr::summarise(weighted_borda_sum = sum(weighted_borda, na.rm = TRUE),
                       .groups = "drop")
    
    # append firm totals to existing data frame
    if (is.null(totals)){
      totals <- firm_totals
    } else {
      totals <- dplyr::bind_rows(totals,firm_totals)
    }
  }
  
  # Summarize firm totals by taking the average of weighted_borda_sum by firm in the stacked firm totals data set
  sum_totals <- totals %>% dplyr::group_by(firm_id) %>%
    dplyr::summarise(borda_score = mean(weighted_borda_sum, na.rm = TRUE),
                     .groups = "drop")
    
  # return
  return(sum_totals)
}