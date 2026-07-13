suppressMessages({ library(igraph); library(dplyr); library(tidyr); library(rlang) })
repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))
source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"))
source(file.path(repo_root, "code/2_analysis/borda_score.R"))

src_file <- file.path(repo_root, "code/3_create_tables_figures/cross_sample_signal_corr.R")
exprs <- parse(src_file, keep.source = FALSE)
extracted_env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]]) == "prepare_bootstrap_score_input") {
    eval(e, envir = extracted_env)
  }
}

survey_data <- read.csv(file.path(mirror_root, "data/processed/long_survey_final.csv"), stringsAsFactors = FALSE)
ref_firm_ids <- c(38, 76, 90)

for (outcome in c("pooled_favor_white", "pooled_favor_male")) {
  prep <- suppressWarnings(prepare_pltree_data(survey_data, rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
  scores <- compute_borda_individual_wide(data_wide = prep$data_wide_pltree, id_map = prep$id_map, ref_firm_ids = ref_firm_ids)

  is_ref <- scores$firm_id %in% ref_firm_ids
  cat(sprintf(
    "%-20s: overall mean=%.4f (n=%d) | reference firms mean=%.4f (n=%d) | non-reference firms mean=%.4f (n=%d)\n",
    outcome, mean(scores$B), nrow(scores),
    mean(scores$B[is_ref]), sum(is_ref),
    mean(scores$B[!is_ref]), sum(!is_ref)
  ))
}
