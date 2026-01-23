# -------------------------------------------------------------------
# Borda bootstrap by resampling respondents (rows) with replacement
# Inputs:
#   - data_wide: wide ranking matrix (resp_id + firm1..firmJ)
#   - id_map: data.frame with firm_id, firm
#   - B: number of bootstrap replicates
#   - seed: (optional) integer for reproducibility
#   - id_var: respondent id column ("resp_id" by default)
#   - higher_is_better: FALSE for usual rank semantics (1 = best)
#   - normalize: TRUE -> Borda normalized by (k-1)
#   - progress: print progress every 10 reps
#   - parallel: use mclapply on non-Windows
#   - cores: number of cores if parallel=TRUE
#
# Outputs (list):
#   - iter_df: firm_id, firm, iter_0, iter_1, ..., iter_B   (Borda means)
#   - se_df:   firm_id, firm, se_0,   se_1,   ..., se_B     (within-rep SEs)
#
# Relies on your existing:
#   - compute_borda_individual_wide()
#   - summarize_borda_by_firm()
# -------------------------------------------------------------------
borda_bootstrap_resample <- function(data_wide,
                                     id_map,
                                     B                  = 200,
                                     seed               = NULL,
                                     id_var             = "resp_id",
                                     higher_is_better   = FALSE,
                                     normalize          = TRUE,
                                     progress           = TRUE,
                                     parallel           = FALSE,
                                     cores              = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(id_var %in% names(data_wide))
  n <- nrow(data_wide)
  
  # firm universe (stable)
  firms <- id_map %>%
    dplyr::select(firm_id, firm) %>%
    dplyr::distinct() %>%
    dplyr::arrange(firm_id)
  
  # ---- helper: run Borda on a given wide matrix, return (firm_id, iter, se)
  .borda_one <- function(dw) {
    B_indiv <- compute_borda_individual_wide(
      data_wide        = dw,
      id_map           = id_map,
      id_var           = id_var,
      higher_is_better = higher_is_better,
      normalize        = normalize
    )
    B_sum <- summarize_borda_by_firm(B_indiv)  # expects your existing function
    # align to (firm_id, firm)
    out <- dplyr::right_join(
      B_sum %>% dplyr::select(firm_id, firm, mean_B, se_B),
      firms, by = c("firm_id","firm")
    ) %>%
      dplyr::arrange(firm_id)
    # rename to generic iter/se for later labeling
    dplyr::transmute(out, firm_id, iter = mean_B, se = se_B)
  }
  
  # ---- full sample (iter_0, se_0)
  full <- .borda_one(data_wide)
  full_iter <- dplyr::rename(full, !!"iter_0" := iter)
  full_se   <- dplyr::rename(full, !!"se_0"   := se)
  
  # ---- bootstrap replicates (iter_1..B, se_1..B)
  .do_rep <- function(b) {
    if (progress && (b %% 10 == 0)) message("Borda bootstrap: ", b, "/", B)
    idx <- sample.int(n, n, replace = TRUE)
    res <- .borda_one(data_wide[idx, , drop = FALSE])
    res %>%
      dplyr::rename(!!paste0("iter_", b) := iter,
                    !!paste0("se_",   b) := se)
  }
  
  rep_list <- if (parallel && .Platform$OS.type != "windows") {
    if (is.null(cores)) cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
    parallel::mclapply(seq_len(B), .do_rep, mc.cores = cores)
  } else {
    lapply(seq_len(B), .do_rep)
  }
  
  # ---- combine into wide tables (means + SEs)
  iter_df <- Reduce(function(x, y) dplyr::full_join(x, y, by = "firm_id"),
                    c(list(full_iter), rep_list)) %>%
    dplyr::left_join(firms, by = "firm_id") %>%
    dplyr::select(firm_id, firm, dplyr::starts_with("iter_"))
  
  se_df <- Reduce(function(x, y) dplyr::full_join(x, y, by = "firm_id"),
                  c(list(full_se), rep_list)) %>%
    dplyr::left_join(firms, by = "firm_id") %>%
    dplyr::select(firm_id, firm, dplyr::starts_with("se_"))
  
  list(iter_df = iter_df, se_df = se_df)
}
