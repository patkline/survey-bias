suppressMessages({
  library(igraph); library(dplyr); library(tidyr); library(rlang)
})

repo_root <- Sys.getenv("DK_ANALYSIS_REPO_ROOT", "/Users/evanrose/Documents/GitHub/survey-bias")
source(file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/_config.R"))

# Load leave_in_connected_set (unchanged across old/new)
source(file.path(repo_root, "code/2_analysis/leave_in_connected.R"))

# --- OLD prepare_pltree_data (pre-#80, from main / commit 478177e), with the per-outcome
# straightline filter (drop respondent's rows for THIS outcome if they gave the identical
# rating to every rated firm: min_rank == max_rank) ---
old_src <- system("git show 478177e:code/helper_functions/1_preprocessing_v3.R", intern = TRUE, ignore.stderr = TRUE)
old_env <- new.env()
eval(parse(text = old_src), envir = old_env)
stopifnot(is.function(old_env$prepare_pltree_data))

# --- NEW prepare_pltree_data (current branch, no per-outcome straightline filter) ---
new_env <- new.env()
source(file.path(repo_root, "code/2_analysis/create_wide_rankings.R"), local = new_env)
stopifnot(is.function(new_env$prepare_pltree_data))

# --- Minimal reproduction of sample_prep.R's variable construction, just enough for
# prepare_pltree_data() to run (resp_id, firm_id, firm, the outcome columns) ---
raw <- read.csv(file.path(repo_root, "data/processed/long_survey.csv"), stringsAsFactors = FALSE)
data <- raw %>%
  select(-firm) %>%
  rename(firm = firm_clean) %>%
  mutate(resp_id = as.integer(as.factor(ResponseId)))

survey_vars <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male",  "FirmHire_favor_male",  "conduct_favor_male",
  "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire"
)

run_union <- function(prep_fn) {
  ids <- integer(0)
  for (outcome in survey_vars) {
    data_temp <- data
    data_temp[[outcome]] <- na_if(data_temp[[outcome]], -1)
    prep <- suppressWarnings(prep_fn(data = data_temp, rank_col = outcome, subgroup_var = NULL, subgroup_filter = NULL))
    ids <- union(ids, prep$data_wide_pltree$resp_id)
  }
  ids
}

cat("Raw respondents in long_survey.csv:", length(unique(data$resp_id)), "\n\n")

old_union <- run_union(old_env$prepare_pltree_data)
cat("OLD prepare_pltree_data (per-outcome straightline filter present)\n")
cat("  union-of-outcomes eligible respondents:", length(old_union), "\n")
cat("  (main branch applied NO further straightline-across-all-outcomes step, so this is its final N)\n\n")

new_union <- run_union(new_env$prepare_pltree_data)
cat("NEW prepare_pltree_data (per-outcome straightline filter removed)\n")
cat("  union-of-outcomes eligible respondents:", length(new_union), "\n")

# Replicate the current branch's additional "straightlined ALL 10 outcomes" drop
all_outcome_straightline_resp_ids <- data %>%
  select(resp_id, all_of(survey_vars)) %>%
  pivot_longer(cols = all_of(survey_vars), names_to = "outcome", values_to = "response") %>%
  mutate(response = na_if(response, -1)) %>%
  filter(!is.na(response)) %>%
  group_by(resp_id, outcome) %>%
  summarise(n_valid = n(), n_distinct_response = n_distinct(response), .groups = "drop") %>%
  mutate(straightlined_outcome = n_valid > 2 & n_distinct_response == 1L) %>%
  group_by(resp_id) %>%
  summarise(n_eligible_outcomes = sum(n_valid > 2), n_straightlined_outcomes = sum(straightlined_outcome), .groups = "drop") %>%
  filter(n_eligible_outcomes == length(survey_vars), n_straightlined_outcomes == length(survey_vars)) %>%
  pull(resp_id)

cat("  respondents straightlined on ALL", length(survey_vars), "outcomes (newly dropped instead):", length(all_outcome_straightline_resp_ids), "\n")
final_new <- setdiff(new_union, all_outcome_straightline_resp_ids)
cat("  final N after that drop:", length(final_new), " (actual long_survey_final.csv has 6515)\n")
