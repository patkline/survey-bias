# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: Calibration check for the Wald and CMD bootstrap p-values implemented in
# code/3_create_tables_figures/cross_sample_signal_corr.R --- repeatedly split the real
# respondent pool into two RANDOM groups (so the null "both groups share the same firm-level
# beliefs" is true by construction), run the production bootstrap machinery on each split, and
# check that the resulting p-values are approximately Uniform(0,1) across splits.
#
# This sources the actual bootstrap helper functions out of cross_sample_signal_corr.R (rather
# than reimplementing them) so the check exercises the exact production code path. It does NOT
# source code/globals.R (which requires a Dropbox mount) --- run with:
#   Rscript --no-init-file bootstrap_calibration_check.R
# -----------------------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(igraph)
  library(dplyr)
  library(tidyr)
  library(rlang)
})

repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))

# -----------------------------------------------------------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------------------------------------------------------
n_splits            <- as.integer(Sys.getenv("CALIBRATION_N_SPLITS", "100"))
bootstrap_reps       <- as.integer(Sys.getenv("CALIBRATION_BOOTSTRAP_REPS", "99"))
max_attempt_multiplier <- 5L
seed                 <- as.integer(Sys.getenv("CALIBRATION_SEED", "20260713"))
outcome              <- Sys.getenv("CALIBRATION_OUTCOME", "pooled_favor_white")
aggregation_method   <- Sys.getenv("CALIBRATION_METHOD", "OLS_not_recentered")
out_dir              <- Sys.getenv(
  "CALIBRATION_OUT_DIR",
  results_dir
)
out_csv              <- file.path(out_dir, "bootstrap_calibration_check_results.csv")

set.seed(seed)

# -----------------------------------------------------------------------------------------------------------------------------
# Load the lightweight analysis helpers the bootstrap functions depend on
# -----------------------------------------------------------------------------------------------------------------------------
source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"))
source(file.path(repo_root, "code/2_analysis/borda_score.R"))

# -----------------------------------------------------------------------------------------------------------------------------
# Extract only the pure bootstrap-helper function definitions from
# cross_sample_signal_corr.R, without executing that file's top-level (Dropbox-dependent)
# script body. This guarantees the calibration check exercises the exact production logic.
# -----------------------------------------------------------------------------------------------------------------------------
src_file <- file.path(repo_root, "code/3_create_tables_figures/cross_sample_signal_corr.R")
exprs <- parse(src_file, keep.source = FALSE)
fn_names <- c(
  "safe_solve", "compute_wald_statistic", "compute_minimum_distance_fit",
  "prepare_bootstrap_score_input", "compute_weighted_firm_mean_result",
  "compute_bootstrap_null_p_values"
)
extracted_env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]]) %in% fn_names) {
    eval(e, envir = extracted_env)
  }
}
stopifnot(setequal(ls(extracted_env), fn_names))

# `compute_bootstrap_null_p_values()` looks up score inputs by sample name via
# `get_bootstrap_score_input()`. In the production script that function re-derives the score
# matrix from a demographic subset lookup table; here we just serve whichever two score inputs
# are current for this split.
current_score_inputs <- list()
get_bootstrap_score_input <- function(sample_name, outcome, aggregation_method, firm_id_vector) {
  current_score_inputs[[sample_name]]
}
environment(get_bootstrap_score_input) <- environment()
assign("get_bootstrap_score_input", get_bootstrap_score_input, envir = extracted_env)

# -----------------------------------------------------------------------------------------------------------------------------
# Load respondent-level data once
# -----------------------------------------------------------------------------------------------------------------------------
survey_data <- read.csv(file.path(repo_root, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)
all_resp_ids <- unique(survey_data$resp_id)
message("Loaded ", length(all_resp_ids), " respondents; running ", n_splits,
        " random splits x ", bootstrap_reps, " bootstrap reps for ", outcome, " / ", aggregation_method)

# -----------------------------------------------------------------------------------------------------------------------------
# Run the calibration loop
# -----------------------------------------------------------------------------------------------------------------------------
results <- vector("list", n_splits)

for (split_index in seq_len(n_splits)) {
  t_split <- Sys.time()

  # Randomly partition respondents into two equal-sized groups; the null (equal firm-level
  # beliefs / perfect correlation) holds by construction since both groups are drawn from the
  # same population.
  shuffled_resp_ids <- sample(all_resp_ids)
  half <- floor(length(shuffled_resp_ids) / 2)
  group_a_ids <- shuffled_resp_ids[seq_len(half)]
  group_b_ids <- shuffled_resp_ids[(half + 1):length(shuffled_resp_ids)]

  data_a <- survey_data[survey_data$resp_id %in% group_a_ids, ]
  data_b <- survey_data[survey_data$resp_id %in% group_b_ids, ]

  # Firm set common to both groups' post-filtering (leave-in-connected-set, 3+ rankings, no
  # duplicate-firm ranks) data for this outcome.
  prep_a <- suppressWarnings(prepare_pltree_data(data_a, rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
  prep_b <- suppressWarnings(prepare_pltree_data(data_b, rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
  firm_id_vector <- sort(intersect(unique(prep_a$data_rating_long$firm_id), unique(prep_b$data_rating_long$firm_id)))

  score_input_a <- extracted_env$prepare_bootstrap_score_input(data_a, outcome, aggregation_method, firm_id_vector)
  score_input_b <- extracted_env$prepare_bootstrap_score_input(data_b, outcome, aggregation_method, firm_id_vector)

  observed_a <- extracted_env$compute_weighted_firm_mean_result(score_input_a, rep(1, nrow(score_input_a$score_matrix)))
  observed_b <- extracted_env$compute_weighted_firm_mean_result(score_input_b, rep(1, nrow(score_input_b$score_matrix)))
  if (is.null(observed_a) || is.null(observed_b)) {
    warning("Split ", split_index, ": observed firm-mean reconstruction failed; skipping.")
    next
  }

  observed_wald_statistic <- extracted_env$compute_wald_statistic(
    observed_a$firm_estimates - observed_b$firm_estimates,
    observed_a$robust_covariance_matrix + observed_b$robust_covariance_matrix
  )

  cmd_fit <- extracted_env$compute_minimum_distance_fit(
    belief_sample_1 = observed_a$firm_estimates,
    belief_sample_2 = observed_b$firm_estimates,
    robust_covariance_sample_1 = observed_a$robust_covariance_matrix,
    robust_covariance_sample_2 = observed_b$robust_covariance_matrix
  )
  if (!is.finite(observed_wald_statistic) || is.null(cmd_fit)) {
    warning("Split ", split_index, ": observed Wald/CMD statistic failed; skipping.")
    next
  }

  current_score_inputs <- list(A = score_input_a, B = score_input_b)

  bootstrap_p_values <- extracted_env$compute_bootstrap_null_p_values(
    sample_1 = "A", sample_2 = "B",
    outcome = outcome, aggregation_method = aggregation_method,
    observed_belief_sample_1 = observed_a$firm_estimates,
    observed_belief_sample_2 = observed_b$firm_estimates,
    observed_wald_statistic = observed_wald_statistic,
    observed_cmd_statistic = cmd_fit$statistic,
    cmd_fitted_belief_sample_1 = cmd_fit$fitted_belief_sample_1,
    cmd_fitted_belief_sample_2 = cmd_fit$fitted_belief_sample_2,
    firm_id_vector = firm_id_vector,
    bootstrap_reps = bootstrap_reps,
    max_attempt_multiplier = max_attempt_multiplier,
    progress_label = paste0("split ", split_index),
    progress_interval = 0L
  )

  results[[split_index]] <- data.frame(
    split_index = split_index,
    n_a = nrow(score_input_a$score_matrix),
    n_b = nrow(score_input_b$score_matrix),
    n_firms = length(firm_id_vector),
    wald_statistic = observed_wald_statistic,
    wald_p_chisq = pchisq(observed_wald_statistic, df = length(firm_id_vector), lower.tail = FALSE),
    wald_p_boot = bootstrap_p_values$wald_p_value,
    wald_reps_used = bootstrap_p_values$wald_reps_used,
    cmd_statistic = cmd_fit$statistic,
    cmd_p_chisq = pchisq(cmd_fit$statistic, df = length(firm_id_vector) - 2, lower.tail = FALSE),
    cmd_p_boot = bootstrap_p_values$cmd_p_value,
    cmd_reps_used = bootstrap_p_values$cmd_reps_used,
    attempts = bootstrap_p_values$attempts,
    elapsed_seconds = as.numeric(Sys.time() - t_split, units = "secs")
  )

  if (split_index %% 5 == 0 || split_index == 1) {
    message(
      "Split ", split_index, "/", n_splits,
      " (", round(results[[split_index]]$elapsed_seconds, 1), "s) ",
      "wald_p_boot=", round(bootstrap_p_values$wald_p_value, 3),
      " cmd_p_boot=", round(bootstrap_p_values$cmd_p_value, 3)
    )
  }
}

results_df <- do.call(rbind, results)
write.csv(results_df, out_csv, row.names = FALSE)
message("Wrote ", nrow(results_df), " splits to ", out_csv)

# -----------------------------------------------------------------------------------------------------------------------------
# Calibration summary: under the (true, by-construction) null, bootstrap p-values should be
# approximately Uniform(0,1) across splits
# -----------------------------------------------------------------------------------------------------------------------------
summarize_calibration <- function(p_values, label) {
  p_values <- p_values[is.finite(p_values)]
  ks <- suppressWarnings(ks.test(p_values, "punif"))
  cat(sprintf(
    "\n%s (n = %d)\n  mean p-value:        %.3f  (expect ~0.500)\n  share p < 0.05:      %.3f  (expect ~0.050)\n  share p < 0.10:      %.3f  (expect ~0.100)\n  KS test vs Unif(0,1): D = %.3f, p = %.3f\n",
    label, length(p_values), mean(p_values),
    mean(p_values < 0.05), mean(p_values < 0.10),
    ks$statistic, ks$p.value
  ))
}

summarize_calibration(results_df$wald_p_boot, "Wald bootstrap p-value")
summarize_calibration(results_df$cmd_p_boot, "CMD bootstrap p-value")
summarize_calibration(results_df$wald_p_chisq, "Wald chi-squared p-value (for comparison)")
summarize_calibration(results_df$cmd_p_chisq, "CMD chi-squared p-value (for comparison)")

# QQ-plots of sorted p-values against the Uniform(0,1) quantiles
png(file.path(out_dir, "bootstrap_calibration_check_qq.png"), width = 900, height = 450)
par(mfrow = c(1, 2))
qqplot(ppoints(nrow(results_df)), sort(results_df$wald_p_boot), main = "Wald bootstrap p-value vs Uniform(0,1)",
       xlab = "Uniform(0,1) quantile", ylab = "Observed p-value")
abline(0, 1, col = "red")
qqplot(ppoints(nrow(results_df)), sort(results_df$cmd_p_boot), main = "CMD bootstrap p-value vs Uniform(0,1)",
       xlab = "Uniform(0,1) quantile", ylab = "Observed p-value")
abline(0, 1, col = "red")
dev.off()
message("Wrote QQ-plot to ", file.path(out_dir, "bootstrap_calibration_check_qq.png"))
