# New for cleaner pipeline
source(file.path(analysis, "sheet_functions.R"))
source(file.path(analysis, "run_model_pl.R"))
source(file.path(analysis, "run_model_ol.R"))
source(file.path(analysis, "run_model_borda.R"))
source(file.path(analysis, "prep_outcomes.R"))

## Sample Prep
source(file.path(analysis, "extra_functions.R"))
source(file.path(analysis, "leave_in_connected.R"))
source(file.path(analysis, "create_wide_rankings.R"))

## Core Results
source(file.path(analysis, "plackett_luce.R"))
source(file.path(analysis, "experimental.R"))
source(file.path(analysis, "borda_score.R"))

## Bootstrap
source(file.path(analysis, "bootstrap_manual.R"))
source(file.path(analysis, "bootstrap_pairwise_manual.R"))
source(file.path(analysis, "borda_bootstrap.R"))

## EIV
source(file.path(analysis, "eiv_bootstrap.R"))
source(file.path(analysis, "bootstrap_summary.R"))
source(file.path(analysis, "eiv_bivariate.R"))

## PL to Borda
source(file.path(analysis, "expected_borda_score.R"))
source(file.path(analysis, "pl_to_borda.R"))

## Pairwise Process
source(file.path(analysis, "pairwise_process_borda.R")) # Pairwise Process
source(file.path(analysis, "pairwise_process.R")) # Pairwise Process

# Other
source(file.path(analysis, "katz_correct.R")) # Noise Correction
source(file.path(analysis, "win_share.R")) # Win Share
source(file.path(analysis, "EB_procedure.R")) # EB Procedure
source(file.path(analysis, "score_function.R")) # Score Function
source(file.path(analysis, "eivreg.R")) # EIV
source(file.path(analysis, "pm_calc.R")) # EIV
source(file.path(analysis, "recenter.R")) # Not currently used