suppressMessages({ library(igraph); library(dplyr); library(tidyr); library(rlang) })
repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))
mirror <- mirror_root

source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"))
source(file.path(repo_root, "code/2_analysis/borda_score.R"))
source(file.path(repo_root, "code/2_analysis/katz_correct.R"))
source(file.path(repo_root, "code/2_analysis/eivreg.R"))

# eiv_functions.R starts with source("code/globals.R"), which requires a Dropbox
# path registered for your user -- strip that line and eval the rest directly
# instead of keeping a separate static copy that could drift out of sync.
eiv_functions_src <- readLines(file.path(repo_root, "code/2_analysis/eiv_functions.R"))
eiv_functions_src <- eiv_functions_src[eiv_functions_src != 'source("code/globals.R")']
eval(parse(text = eiv_functions_src), envir = .GlobalEnv)

src_file <- file.path(repo_root, "code/3_create_tables_figures/cross_sample_signal_corr.R")
exprs <- parse(src_file, keep.source = FALSE)
extracted_env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      as.character(e[[2]]) %in% c("prepare_bootstrap_score_input", "compute_weighted_firm_mean_result")) {
    eval(e, envir = extracted_env)
  }
}

survey_data <- read.csv(file.path(mirror, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)
raw_dk_data <- read.csv(file.path(mirror, "data/processed/long_survey_final_summary_stats.csv"), stringsAsFactors = FALSE)

firm_id_vector <- sort(unique(survey_data$firm_id))
stopifnot(length(firm_id_vector) == 164)

# Firm-constant covariates
firm_covars <- survey_data |>
  dplyr::select(firm_id, aer_naics2, njobs, log_dif, log_dif_gender) |>
  dplyr::distinct()
stopifnot(nrow(firm_covars) == 164)

# Firm-level DK rate, same administered/DK logic used throughout
firm_dk_rate <- function(df, v1, v2) {
  a <- df[[v1]]; b <- df[[v2]]
  stopifnot(sum(!is.na(a) & !is.na(b)) == 0)
  administered <- !is.na(a) | !is.na(b)
  raw_response <- ifelse(!is.na(a), a, b)
  dk <- administered & raw_response == -1
  agg <- aggregate(dk[administered] ~ df$firm_id[administered], FUN = mean)
  names(agg) <- c("firm_id", "dk_rate")
  agg
}
dk_white <- firm_dk_rate(raw_dk_data, "FirmCont_favor_white", "conduct_favor_white")
dk_male  <- firm_dk_rate(raw_dk_data, "FirmCont_favor_male",  "conduct_favor_male")

median_dk_white <- median(dk_white$dk_rate)
median_dk_male  <- median(dk_male$dk_rate)
below_median_white_firms <- dk_white$firm_id[dk_white$dk_rate <= median_dk_white]
below_median_male_firms  <- dk_male$firm_id[dk_male$dk_rate  <= median_dk_male]
cat("Median DK rate (pooled_favor_white):", round(median_dk_white, 4), " -> ", length(below_median_white_firms), "/ 164 firms below/at median\n")
cat("Median DK rate (pooled_favor_male): ", round(median_dk_male, 4),  " -> ", length(below_median_male_firms), "/ 164 firms below/at median\n\n")

# Build firm-level estimates + robust covariance for each (outcome, method), reconstructed
# from raw data via the same production functions, validated earlier to match the stored
# Coefficients.parquet exactly.
firm_estimates <- list()
robust_cov <- list()
for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
  for (aggregation_method in c("OLS_not_recentered", "Borda_not_recentered")) {
    score_input <- extracted_env$prepare_bootstrap_score_input(survey_data, outcome, aggregation_method, firm_id_vector)
    obs <- extracted_env$compute_weighted_firm_mean_result(score_input, rep(1, nrow(score_input$score_matrix)))
    key <- paste(outcome, aggregation_method)
    firm_estimates[[key]] <- setNames(obs$firm_estimates, firm_id_vector)
    robust_cov[[key]] <- obs$robust_covariance_matrix
    dimnames(robust_cov[[key]]) <- list(firm_id_vector, firm_id_vector)
  }
}

# Recompute the njobs-weighted Katz noise for a given firm subset
recompute_katz_noise <- function(outcome, aggregation_method, firms_subset) {
  key <- paste(outcome, aggregation_method)
  est <- firm_estimates[[key]][as.character(firms_subset)]
  cov_sub <- robust_cov[[key]][as.character(firms_subset), as.character(firms_subset)]
  njobs_sub <- firm_covars$njobs[match(firms_subset, firm_covars$firm_id)]
  compute_njobs_weighted_signal_components(est, njobs_sub, cov_sub)
}

run_one_eiv_spec <- function(outcome, lhs_var, firms_subset, sample_label) {
  # Restrict to firms actually usable in the regression (non-missing njobs weight and
  # non-missing administrative LHS measure) before computing the Katz noise, so the
  # noise recomputation matches the estimation sample exactly.
  eligible <- firm_covars$firm_id[!is.na(firm_covars$njobs) & !is.na(firm_covars[[lhs_var]])]
  firms_subset <- intersect(firms_subset, eligible)

  results <- list()
  for (aggregation_method in c("OLS_not_recentered", "Borda_not_recentered")) {
    key <- paste(outcome, aggregation_method)
    signal <- recompute_katz_noise(outcome, aggregation_method, firms_subset)

    coef_df <- firm_covars |>
      dplyr::filter(firm_id %in% firms_subset) |>
      dplyr::mutate(
        model = aggregation_method,
        !!outcome := firm_estimates[[key]][as.character(firm_id)]
      ) |>
      dplyr::rename(entity_id = firm_id)

    noise_mat <- matrix(signal$noise_njobs_weighted_katz, 1, 1, dimnames = list(outcome, outcome))
    attr(noise_mat, "raw_noise_matrix") <- matrix(signal$noise_njobs_weighted, 1, 1, dimnames = list(outcome, outcome))

    out <- run_eiv_one(
      coef_df_wide = coef_df, noise_mat = noise_mat, model_value = aggregation_method,
      lhs_var = lhs_var, rhs_vars = outcome, id_col = "entity_id", model_col = "model",
      fe_col = "aer_naics2", weights_col = "njobs", use_fe = TRUE
    )
    out$sample <- sample_label
    out$n_firms_in_subset <- length(firms_subset)
    out$katz_noise <- signal$noise_njobs_weighted_katz
    out$raw_noise <- signal$noise_njobs_weighted
    results[[aggregation_method]] <- out
  }
  do.call(rbind, results)
}

race_full       <- run_one_eiv_spec("pooled_favor_white", "log_dif",        firm_id_vector,           "Full sample (164 firms)")
race_belowmed   <- run_one_eiv_spec("pooled_favor_white", "log_dif",        below_median_white_firms, "Below-median DK firms")
gender_full     <- run_one_eiv_spec("pooled_favor_male",  "log_dif_gender", firm_id_vector,           "Full sample (164 firms)")
gender_belowmed <- run_one_eiv_spec("pooled_favor_male",  "log_dif_gender", below_median_male_firms,  "Below-median DK firms")

# ---- Drop the top 10 DK-rate firms among the 97 log_dif-eligible firms ----
eligible_white_97 <- firm_covars$firm_id[!is.na(firm_covars$njobs) & !is.na(firm_covars$log_dif)]
eligible_male_97  <- firm_covars$firm_id[!is.na(firm_covars$njobs) & !is.na(firm_covars$log_dif_gender)]

dk_white_97 <- dk_white[dk_white$firm_id %in% eligible_white_97, ] |> dplyr::arrange(dplyr::desc(dk_rate))
dk_male_97  <- dk_male[dk_male$firm_id %in% eligible_male_97, ]  |> dplyr::arrange(dplyr::desc(dk_rate))

top10_white_firms <- dk_white_97$firm_id[1:10]
top10_male_firms  <- dk_male_97$firm_id[1:10]

cat("\nTop 10 DK-rate firms among the 97 (race):\n")
print(dk_white_97[1:10, ])
cat("\nTop 10 DK-rate firms among the 97 (gender):\n")
print(dk_male_97[1:10, ])

drop_top10_white_firms <- setdiff(eligible_white_97, top10_white_firms)
drop_top10_male_firms  <- setdiff(eligible_male_97, top10_male_firms)
cat("\nRemaining firms after dropping top 10: race =", length(drop_top10_white_firms), ", gender =", length(drop_top10_male_firms), "\n\n")

race_droptop10   <- run_one_eiv_spec("pooled_favor_white", "log_dif",        drop_top10_white_firms, "Drop top 10 DK firms (of 97)")
gender_droptop10 <- run_one_eiv_spec("pooled_favor_male",  "log_dif_gender", drop_top10_male_firms,  "Drop top 10 DK firms (of 97)")

all_results <- rbind(race_full, race_belowmed, race_droptop10, gender_full, gender_belowmed, gender_droptop10)
all_results$coef_label <- ifelse(all_results$coef == 1, "No FE", "Industry FE")
print(all_results[, c("lhs","rhs","model","sample","n_firms_in_subset","n","coef_label","sample_est","sample_se","katz_noise","raw_noise")], row.names = FALSE)
write.csv(all_results, file.path(results_dir, "eiv_below_median_dk_results.csv"), row.names = FALSE)
