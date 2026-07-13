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

# Stored ("official") firm-level estimates, already pulled from Coefficients.parquet via Python
stored <- read.csv(file.path(results_dir, "firm_dk_vs_estimate_by_method.csv"), stringsAsFactors = FALSE) |>
  dplyr::select(firm_id, model, outcome, estimate) |>
  dplyr::distinct()

# Full analysis sample -- this is Full_Sample / subset == "all", i.e. the whole
# long_survey_final.csv, no demographic subsetting
survey_data <- read.csv(file.path(mirror, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)
cat("Loaded", length(unique(survey_data$resp_id)), "respondents from the mirror's long_survey_final.csv\n\n")

results <- list()
for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
  for (aggregation_method in c("OLS_not_recentered", "Borda_not_recentered")) {
    stored_sub <- stored |> dplyr::filter(model == aggregation_method, outcome == !!outcome) |> dplyr::arrange(firm_id)
    firm_id_vector <- stored_sub$firm_id
    stopifnot(length(firm_id_vector) == 164)

    score_input <- extracted_env$prepare_bootstrap_score_input(survey_data, outcome, aggregation_method, firm_id_vector)
    recon <- extracted_env$compute_weighted_firm_mean_result(score_input, rep(1, nrow(score_input$score_matrix)))

    comparison <- data.frame(
      firm_id = firm_id_vector,
      stored_estimate = stored_sub$estimate,
      reconstructed_estimate = recon$firm_estimates
    )
    comparison$diff <- comparison$reconstructed_estimate - comparison$stored_estimate

    cat(sprintf(
      "%-22s / %-19s : n_respondents_used=%d  max|diff|=%.6f  mean|diff|=%.6f  cor=%.6f\n",
      aggregation_method, outcome, nrow(score_input$score_matrix),
      max(abs(comparison$diff)), mean(abs(comparison$diff)),
      cor(comparison$stored_estimate, comparison$reconstructed_estimate)
    ))
    results[[paste(aggregation_method, outcome)]] <- comparison
  }
}

all_comparisons <- do.call(rbind, lapply(names(results), function(k) {
  df <- results[[k]]; df$key <- k; df
}))
write.csv(all_comparisons, file.path(results_dir, "reconstruction_vs_stored_comparison.csv"), row.names = FALSE)
cat("\nWrote full comparison to reconstruction_vs_stored_comparison.csv\n")
