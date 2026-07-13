suppressMessages({ library(igraph); library(dplyr); library(tidyr); library(rlang) })
repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))
mirror <- mirror_root
source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"))
source(file.path(repo_root, "code/2_analysis/borda_score.R"))

src_file <- file.path(repo_root, "code/3_create_tables_figures/cross_sample_signal_corr.R")
exprs <- parse(src_file, keep.source = FALSE)
fn_names <- c("safe_solve", "compute_wald_statistic", "compute_minimum_distance_fit",
              "prepare_bootstrap_score_input", "compute_weighted_firm_mean_result",
              "compute_bootstrap_null_p_values")
extracted_env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]]) %in% fn_names) {
    eval(e, envir = extracted_env)
  }
}
current_score_inputs <- list()
get_bootstrap_score_input <- function(sample_name, outcome, aggregation_method, firm_id_vector) current_score_inputs[[sample_name]]
environment(get_bootstrap_score_input) <- environment()
assign("get_bootstrap_score_input", get_bootstrap_score_input, envir = extracted_env)

set.seed(20260713)

propensity <- read.csv(file.path(results_dir, "dk_propensity_scores.csv"), stringsAsFactors = FALSE)
survey_data <- read.csv(file.path(mirror, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)

high_ids <- propensity$resp_id[propensity$group == "High_DK_Propensity"]
low_ids  <- propensity$resp_id[propensity$group == "Low_DK_Propensity"]
data_high <- survey_data |> dplyr::filter(resp_id %in% high_ids)
data_low  <- survey_data |> dplyr::filter(resp_id %in% low_ids)

results <- data.frame()
for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
  prep_high <- suppressWarnings(prepare_pltree_data(data_high, rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
  prep_low  <- suppressWarnings(prepare_pltree_data(data_low,  rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
  firm_id_vector <- sort(intersect(unique(prep_high$data_rating_long$firm_id), unique(prep_low$data_rating_long$firm_id)))

  for (aggregation_method in c("OLS_not_recentered", "Borda_not_recentered")) {
    score_input_high <- extracted_env$prepare_bootstrap_score_input(data_high, outcome, aggregation_method, firm_id_vector)
    score_input_low  <- extracted_env$prepare_bootstrap_score_input(data_low,  outcome, aggregation_method, firm_id_vector)

    obs_high <- extracted_env$compute_weighted_firm_mean_result(score_input_high, rep(1, nrow(score_input_high$score_matrix)))
    obs_low  <- extracted_env$compute_weighted_firm_mean_result(score_input_low,  rep(1, nrow(score_input_low$score_matrix)))

    wald_statistic <- extracted_env$compute_wald_statistic(
      obs_high$firm_estimates - obs_low$firm_estimates,
      obs_high$robust_covariance_matrix + obs_low$robust_covariance_matrix
    )
    wald_df <- length(firm_id_vector)
    wald_p_chisq <- pchisq(wald_statistic, df = wald_df, lower.tail = FALSE)

    cmd_fit <- extracted_env$compute_minimum_distance_fit(
      belief_sample_1 = obs_high$firm_estimates, belief_sample_2 = obs_low$firm_estimates,
      robust_covariance_sample_1 = obs_high$robust_covariance_matrix,
      robust_covariance_sample_2 = obs_low$robust_covariance_matrix
    )
    cmd_df <- length(firm_id_vector) - 2
    cmd_p_chisq <- pchisq(cmd_fit$statistic, df = cmd_df, lower.tail = FALSE)

    raw_corr <- cor(obs_high$firm_estimates, obs_low$firm_estimates)

    current_score_inputs <- list(High = score_input_high, Low = score_input_low)
    boot <- extracted_env$compute_bootstrap_null_p_values(
      sample_1 = "High", sample_2 = "Low", outcome = outcome, aggregation_method = aggregation_method,
      observed_belief_sample_1 = obs_high$firm_estimates, observed_belief_sample_2 = obs_low$firm_estimates,
      observed_wald_statistic = wald_statistic, observed_cmd_statistic = cmd_fit$statistic,
      cmd_fitted_belief_sample_1 = cmd_fit$fitted_belief_sample_1, cmd_fitted_belief_sample_2 = cmd_fit$fitted_belief_sample_2,
      firm_id_vector = firm_id_vector, bootstrap_reps = 499, max_attempt_multiplier = 5,
      progress_label = paste("High vs Low DK propensity", outcome, aggregation_method), progress_interval = 0
    )

    results <- rbind(results, data.frame(
      outcome = outcome, aggregation_method = aggregation_method, n_firms = length(firm_id_vector),
      n_high = nrow(score_input_high$score_matrix), n_low = nrow(score_input_low$score_matrix),
      raw_correlation = raw_corr,
      wald_statistic = wald_statistic, wald_df = wald_df, wald_p_chisq = wald_p_chisq, wald_p_boot = boot$wald_p_value,
      cmd_statistic = cmd_fit$statistic, cmd_df = cmd_df, cmd_p_chisq = cmd_p_chisq, cmd_p_boot = boot$cmd_p_value,
      wald_reps_used = boot$wald_reps_used, cmd_reps_used = boot$cmd_reps_used, attempts = boot$attempts
    ))
    message("done: ", outcome, " / ", aggregation_method)
  }
}
print(results)
write.csv(results, file.path(results_dir, "dk_propensity_cross_sample_corr_results.csv"), row.names = FALSE)
