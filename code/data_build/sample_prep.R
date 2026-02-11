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
source(file.path(helper_functions, "leave_in_connected.R"))
source(file.path(helper_functions, "1_preprocessing_v3.R"))

# ------------------------------------------------------------------------------
# Prepare Sample
# ------------------------------------------------------------------------------

# Define file path for long survey data
file_path <- file.path(processed, "long_survey.csv")

# Create Necessary Variables
data <- read.csv(file_path, stringsAsFactors = FALSE) %>%
  rename(dif_gender_se = dif_se_gender) %>%
  rename(log_dif_gender_se = log_dif_se_gender) %>%
  rename(log_dif_age_se = log_dif_se_age) %>%
  rename(dif_age_se = dif_se_age) 

# --- small earlier fix (keep this where you define columns) ---
data <- data %>%
  mutate(
    looking_job = ifelse(looking_job == "Yes", 1, 0),
    gender = ifelse(gender == "Female", 1, 0),
    race = ifelse(race_recode == "Black", 1, 0),
    educ_0_1 = ifelse(educ %in% c("Some college, no degree", "Bachelor degree", "Associate degree", "Master degree", "Professional or Doctorate degree"), 1, 0),
    age_gt40 = ifelse(age >= 40, 1, 0),
    democrat = ifelse(party_affil == "Democrat", 1, 0),
    republican = ifelse(party_affil == "Republican", 1, 0),
    entry = ifelse(any_entry_lev_exp == "Yes", 1, 0), 
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
  cb_central_full_se = as.numeric(cb_central_full_se),
  firm_id = as.integer(as.factor(firm))
)

data <- data %>%
  mutate(
    pooled_favor_white = coalesce(na_if(FirmCont_favor_white, -1),
                                  na_if(conduct_favor_white,  -1)),
    pooled_favor_male  = coalesce(na_if(FirmCont_favor_male,  -1),
                                  na_if(conduct_favor_male,   -1))
  )

industry_map <- read_excel(file.path(processed, "industry_map.xlsx")) %>% 
  select(firm_id, aer_naics2)

data <- left_join(data, industry_map, by="firm_id")

temp <- data %>% select(resp_id, response_duration) %>%
unique()
median_response_duration <- temp %>%
  summarise(median_duration = median(response_duration, na.rm = TRUE)) %>%
  pull(median_duration)

data <- data %>% mutate(long = ifelse(response_duration > median_response_duration, 1, 0))

# Define the recoding mapping
confidence_levels <- c(
  "Extremely confident" = 5,
  "Very confident" = 4,
  "Somewhat confident" = 3,
  "Slightly confident" = 2,
  "Not at all confident" = 1
)

# List of confidence variables
confidence_vars <- c("confidence_race_names", "confidence_gend_names",
                     "confidence_race_conduct", "confidence_gend_conduct",
                     "confidence_age_conduct")

data <- data %>%
  mutate(confidence_race_names_numeric = recode(confidence_race_names, !!!confidence_levels, .default = NA_real_),
         confidence_gend_names_numeric = recode(confidence_gend_names, !!!confidence_levels, .default = NA_real_),
         confidence_race_conduct_numeric = recode(confidence_race_conduct, !!!confidence_levels, .default = NA_real_),
         confidence_gend_conduct_numeric = recode(confidence_gend_conduct, !!!confidence_levels, .default = NA_real_),
         confidence_age_conduct_numeric = recode(confidence_age_conduct, !!!confidence_levels, .default = NA_real_)) %>%
  mutate(confidence_race_names_gt_median = ifelse(confidence_race_names_numeric > median(confidence_race_names_numeric, na.rm=TRUE),1,0),
         confidence_gend_names_gt_median = ifelse(confidence_gend_names_numeric > median(confidence_gend_names_numeric, na.rm=TRUE),1,0),
         confidence_race_conduct_gt_median = ifelse(confidence_race_conduct_numeric > median(confidence_race_conduct_numeric, na.rm=TRUE),1,0),
         confidence_gend_conduct_gt_median = ifelse(confidence_gend_conduct_numeric > median(confidence_gend_conduct_numeric, na.rm=TRUE),1,0),
         confidence_age_conduct_gt_median = ifelse(confidence_age_conduct_numeric > median(confidence_age_conduct_numeric, na.rm=TRUE),1,0))

# --- outcomes list (unchanged names) ---
survey_vars <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male",  "FirmHire_favor_male",  "conduct_favor_male",
  "conduct_favor_younger","discretion",           "FirmSelective",
  "FirmDesire"
)

# --- collect union of resp_ids that survive 'cleaning' for ANY outcome ---
resp_ids_union <- integer(0)

for (outcome in survey_vars) {
  print(outcome)
  # Replace -1 with NA in-place for the current outcome
  data_temp <- data
  data_temp[[outcome]] <- dplyr::na_if(data_temp[[outcome]], -1)
  
  # Run your prep using the original outcome column
  prep <- prepare_pltree_data(
    data           = data_temp,
    rank_col       = outcome,       # keep the original column name
    subgroup_var   = NULL,
    subgroup_filter= NULL
  )
  
  # Collect respondent IDs that appear in the prepared (kept) data
  resp_ids_this <- prep$data_wide_pltree %>% dplyr::pull(resp_id)
  resp_ids_union <- union(resp_ids_union, resp_ids_this)
}

# --- restrict sample to anyone who appears in ANY outcome's cleaned set ---
restricted_sample <- data %>% dplyr::filter(resp_id %in% resp_ids_union)

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

# --- export analysis-ready version (NO -1s) ---
write.csv(restricted_sample_analysis,
          file.path(processed, "long_survey_final.csv"),
          row.names = FALSE)