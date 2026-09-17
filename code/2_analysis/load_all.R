# New for cleaner pipeline
source(file.path(analysis, "analysis_pipeline.R"))
source(file.path(analysis, "construct_firm_level_estimates.R"))
source(file.path(analysis, "run_models_helpers.R"))
source(file.path(analysis, "prep_outcomes.R"))
source(file.path(analysis, "create_wide_rankings.R"))
source(file.path(analysis, "variance_functions.R"))
source(file.path(analysis, "covariance_functions.R"))
source(file.path(analysis, "experimental.R"))
source(file.path(analysis, "borda_score.R"))
source(file.path(analysis, "katz_correct.R")) # Noise Correction
source(file.path(analysis, "EB_procedure.R")) # EB Procedure
source(file.path(analysis, "eivreg.R")) # EIV
source(file.path(analysis, "eiv_functions.R")) # EIV
source(file.path(analysis, "belief_selectivity_eiv.R")) # Table 7 belief/selectivity EIV
source(file.path(analysis, "compute_firm_mean_ratings.R")) 
source(file.path(analysis, "belief_summary_amad.R")) # Belief AMAD respondent-pair statistics
source(file.path(analysis, "eeo1_naics3_shares.R")) # EEO-1 NAICS3 share helpers
source(file.path(analysis, "make_industry_means.R")) # EIV
