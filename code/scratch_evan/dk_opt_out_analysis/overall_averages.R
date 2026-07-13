suppressMessages({ library(igraph); library(dplyr); library(tidyr); library(rlang) })
repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))
mirror <- mirror_root
source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"))
source(file.path(repo_root, "code/2_analysis/borda_score.R"))

src_file <- file.path(repo_root, "code/3_create_tables_figures/cross_sample_signal_corr.R")
exprs <- parse(src_file, keep.source = FALSE)
fn_names <- c("prepare_bootstrap_score_input", "compute_weighted_firm_mean_result")
extracted_env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]]) %in% fn_names) {
    eval(e, envir = extracted_env)
  }
}

survey_data <- read.csv(file.path(mirror, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)

stored <- read.csv(file.path(results_dir, "firm_dk_vs_estimate_by_method.csv"), stringsAsFactors = FALSE) |>
  dplyr::select(firm_id, model, outcome, estimate) |> dplyr::distinct()

results <- list()
for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
  # Respondent-firm-level (raw 1-5 scale, as stored in long_survey_final.csv)
  raw_vals <- survey_data[[outcome]]
  raw_mean <- mean(raw_vals, na.rm = TRUE)
  n_raw <- sum(!is.na(raw_vals))

  for (aggregation_method in c("OLS_not_recentered", "Borda_not_recentered")) {
    stored_sub <- stored |> dplyr::filter(model == aggregation_method, outcome == !!outcome) |> dplyr::arrange(firm_id)
    firm_id_vector <- stored_sub$firm_id

    score_input <- extracted_env$prepare_bootstrap_score_input(survey_data, outcome, aggregation_method, firm_id_vector)
    score_matrix <- score_input$score_matrix

    respondent_level_mean <- mean(score_matrix, na.rm = TRUE)   # simple mean over every respondent-firm score, unweighted by firm
    n_scores <- sum(is.finite(score_matrix))
    firm_level_mean <- mean(stored_sub$estimate)                # mean of the 164 already-computed firm-level estimates (firm-equal-weighted)

    results[[paste(aggregation_method, outcome)]] <- data.frame(
      outcome = outcome, aggregation_method = aggregation_method,
      raw_1_to_5_scale_mean = raw_mean, n_raw_responses = n_raw,
      respondent_level_score_mean = respondent_level_mean, n_respondent_firm_scores = n_scores,
      firm_level_equal_weighted_mean = firm_level_mean
    )
  }
}
out <- do.call(rbind, results)
rownames(out) <- NULL
print(out)
write.csv(out, file.path(results_dir, "overall_sample_averages.csv"), row.names = FALSE)
