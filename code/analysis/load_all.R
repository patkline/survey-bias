# New for cleaner pipeline
source(file.path(analysis, "analysis_pipeline.R"))
source(file.path(analysis, "sheet_functions.R"))
source(file.path(analysis, "run_model_pl.R"))
source(file.path(analysis, "run_model_ol.R"))
source(file.path(analysis, "run_model_borda.R"))
source(file.path(analysis, "run_model_ols.R"))
source(file.path(analysis, "run_models_helpers.R"))
source(file.path(analysis, "prep_outcomes.R"))
source(file.path(analysis, "create_wide_rankings.R"))
source(file.path(analysis, "leave_in_connected.R"))
source(file.path(analysis, "variance_functions.R"))
source(file.path(analysis, "covariance_functions.R"))
source(file.path(analysis, "correlation_function.R"))
source(file.path(analysis, "experimental.R"))
source(file.path(analysis, "borda_score.R"))
source(file.path(analysis, "katz_correct.R")) # Noise Correction
source(file.path(analysis, "EB_procedure.R")) # EB Procedure
source(file.path(analysis, "eivreg.R")) # EIV
source(file.path(analysis, "eiv_functions.R")) # EIV
source(file.path(analysis, "mean_estimator_bread_and_score.R")) 

## Sample Prep
# source(file.path(analysis, "extra_functions.R"))


## PL to Borda
source(file.path(analysis, "expected_borda_score.R"))
source(file.path(analysis, "pl_to_borda.R"))


