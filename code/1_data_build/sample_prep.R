# ------------------------------------------------------------------------------
# Purpose: Prepare sample for analysis
#
# Created: Jordan Cammarota
# Edited: Nico Rotundo 2026-01-23
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# ------------------------------------------------------------------------------
# Source necessary functions
# ------------------------------------------------------------------------------

# Source necessary functions
source(file.path(helper_functions, "sample_eligibility_helpers.R"))

# ------------------------------------------------------------------------------
# Prepare Sample
# ------------------------------------------------------------------------------

# Define file path for long survey data
file_path <- file.path(processed, "long_survey.csv")

# Create Necessary Variables
data <- read.csv(file_path, stringsAsFactors = FALSE)

# --- small earlier fix (keep this where you define columns) ---
data <- data %>%
  mutate(
    looking_job = ifelse(looking_job == "Yes", 1, 0),
    gender = ifelse(gender == "Female", 1, 0),
    race = ifelse(race_recode == "Black", 1, 0),
    educ_0_1 = ifelse(educ %in% c("Some college, no degree", "Bachelor degree", "Associate degree", "Master degree", "Professional or Doctorate degree"), 1, 0),
    age_gt40 = ifelse(age >= 40, 1, 0),
    fear = ifelse(feared_discrim == "Yes", 1, 0)
  ) %>%
  select(-firm) %>%
  rename(
    firm = firm_clean, 
    FirmCont_black = FirmContRace_wfirst0,
    FirmCont_white = FirmContRace_wfirst1,
    FirmHire_black = FirmHireRace_wfirst0,
    FirmHire_white = FirmHireRace_wfirst1,
    FirmCont_female = FirmContGend_mfirst0,
    FirmCont_male = FirmContGend_mfirst1,
    FirmHire_female = FirmHireGend_mfirst0,
    FirmHire_male = FirmHireGend_mfirst1
  ) %>%
  mutate(
    resp_id = as.integer(as.factor(ResponseId)),
    firm_id = as.integer(as.factor(firm))
)

data <- data %>%
  mutate(
    pooled_favor_white = coalesce(na_if(FirmCont_favor_white, -1),
                                  na_if(conduct_favor_white,  -1)),
    pooled_favor_male  = coalesce(na_if(FirmCont_favor_male,  -1),
                                  na_if(conduct_favor_male,   -1))
  )

# Read industry map and keep firm-name key, archived aer_naics2 variable, and AER bin name
industry_map <- read_excel(file.path(processed, "industry_map.xlsx")) %>%
  select(firm_clean, aer_naics2,
         aer_naics2_name = sic_code_aggregated_two_digit_harmonized_names_aer)

# Merge aer_naics2 and the AER bin name onto sample data by firm name
data <- left_join(data, industry_map, by = c("firm" = "firm_clean"))

# Keep observations with non-missing and non-literal-"nan" firm values for merge and non-missing aer_naics2 checks
data_non_missing_firm <- data[!is.na(data$firm) & data$firm != "nan", ]

# Assert all observations with non-missing firm values match to industry map by firm name
stopifnot(
  nrow(
    anti_join(
      data_non_missing_firm %>% distinct(firm),
      industry_map %>% distinct(firm_clean),
      by = c("firm" = "firm_clean")
    )
  ) == 0
)

# Assert all observations with non-missing firm values have non-missing aer_naics2 after merge
stopifnot(sum(is.na(data_non_missing_firm$aer_naics2)) == 0)

# Define the recoding mapping
confidence_levels <- c(
  "Extremely confident" = 5,
  "Very confident" = 4,
  "Somewhat confident" = 3,
  "Slightly confident" = 2,
  "Not at all confident" = 1
)

data <- data %>%
  mutate(confidence_race_names_numeric = recode(confidence_race_names, !!!confidence_levels, .default = NA_real_),
         confidence_gend_names_numeric = recode(confidence_gend_names, !!!confidence_levels, .default = NA_real_),
         confidence_race_conduct_numeric = recode(confidence_race_conduct, !!!confidence_levels, .default = NA_real_),
         confidence_gend_conduct_numeric = recode(confidence_gend_conduct, !!!confidence_levels, .default = NA_real_)) %>%
  mutate(confidence_race_names_gt_median = ifelse(confidence_race_names_numeric > 3,1,0),
         confidence_gend_names_gt_median = ifelse(confidence_gend_names_numeric > 3,1,0),
         confidence_race_conduct_gt_median = ifelse(confidence_race_conduct_numeric > 3,1,0),
         confidence_gend_conduct_gt_median = ifelse(confidence_gend_conduct_numeric > 3,1,0)) %>%
  mutate(
    confidence_gend = coalesce(na_if(confidence_gend_conduct_gt_median, -1),
                                  na_if(confidence_gend_names_gt_median,  -1)),
    confidence_race  = coalesce(na_if(confidence_race_names_gt_median,  -1),
                                  na_if(confidence_race_conduct_gt_median,   -1))
  )

# --- outcomes list (unchanged names) ---
survey_vars <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male",  "FirmHire_favor_male",  "conduct_favor_male",
  "conduct_favor_younger","discretion",           "FirmSelective",
  "FirmDesire"
)

# Include the raw white-framed race-conduct outcome only when checking for
# straightlining; it remains excluded from the analysis outcome list above.
straightline_vars <- c(survey_vars, "conduct_white")

all_outcome_straightline_resp_ids <- data %>%
  dplyr::select(resp_id, dplyr::all_of(straightline_vars)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(straightline_vars),
    names_to = "outcome",
    values_to = "response"
  ) %>%
  dplyr::mutate(response = dplyr::na_if(response, -1)) %>%
  dplyr::filter(!is.na(response)) %>%
  dplyr::group_by(resp_id, outcome) %>%
  dplyr::summarise(
    n_valid = dplyr::n(),
    n_distinct_response = dplyr::n_distinct(response),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    straightlined_outcome = n_valid > 2 & n_distinct_response == 1L
  ) %>%
  dplyr::group_by(resp_id) %>%
  dplyr::summarise(
    n_eligible_outcomes = sum(n_valid > 2),
    n_straightlined_outcomes = sum(straightlined_outcome),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n_eligible_outcomes > 0,
    n_straightlined_outcomes == n_eligible_outcomes
  ) %>%
  dplyr::pull(resp_id)

message(
  "Dropping respondents who straightlined every eligible firm-specific outcome: ",
  length(all_outcome_straightline_resp_ids)
)

# --- collect union of resp_ids that survive 'cleaning' for ANY outcome ---
resp_ids_union <- integer(0)

for (outcome in survey_vars) {
  # Replace -1 with NA in-place for the current outcome
  data_temp <- data
  data_temp[[outcome]] <- dplyr::na_if(data_temp[[outcome]], -1)
  
  # Run your prep using the original outcome column
  eligible_data <- prepare_sample_eligibility_data(
    data           = data_temp,
    rank_col       = outcome,       # keep the original column name
    subgroup_var   = NULL,
    subgroup_filter= NULL
  )
  
  # Collect respondent IDs that appear in the prepared (kept) data
  resp_ids_this <- eligible_data %>% dplyr::pull(resp_id)
  resp_ids_union <- union(resp_ids_union, resp_ids_this)
}

# --- restrict sample to anyone who appears in ANY outcome's cleaned set ---
restricted_sample <- data %>%
  dplyr::filter(
    resp_id %in% resp_ids_union,
    !resp_id %in% all_outcome_straightline_resp_ids
  )

# --- export to the same folder as the import, new name ---
write.csv(restricted_sample, file.path(processed, "long_survey_final_summary_stats.csv"), row.names = FALSE)

# --- analysis version: convert -1 to NA everywhere ---
restricted_sample_analysis <- restricted_sample %>%
  mutate(across(
    everything(),
    ~ {
      # If numeric/integer: convert -1 to NA
      if (is.numeric(.) || is.integer(.)) return(na_if(., -1))
      
      # If character: convert "-1" to NA (in case it got read as text)
      if (is.character(.)) return(na_if(., "-1"))
      
      # Otherwise (factors, lists, etc): leave unchanged
      .
    }
  ))

restricted_sample_analysis <- restricted_sample_analysis %>%
  mutate(educ = educ_0_1,
         age = age_gt40)

# Flip Valences so they have same meaning
# 1 = Very Likely to Discriminate against Black/Female/Older Candidates
# 5 = Very Unlikely to Discriminate against Black/Female/Older Candidates
restricted_sample_analysis <- restricted_sample_analysis %>%
  mutate(conduct_white = 6 - conduct_white,
         conduct_male = 6 - conduct_male,
         conduct_younger = 6 - conduct_younger,
         FirmCont_black = 6 - FirmCont_black,
         FirmHire_black = 6 - FirmHire_black,
         FirmCont_female = 6 - FirmCont_female,
         FirmHire_female = 6 - FirmHire_female
         ) 

# --- export analysis-ready version (NO -1s) ---
write.csv(restricted_sample_analysis,
          file.path(processed, "long_survey_final.csv"),
          row.names = FALSE)
