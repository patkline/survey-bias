<!------------------------------------------------------------------------------
Purpose: README file for survey bias project

Created: Nico Rotundo 2026-01-06
Edited: Anh-Huy Nguyen 2026-06-07
# ----------------------------------------------------------------------------->

# Getting started 

## Setup instructions

### Prerequisites
- R 
- Python 3

### If you haven't cloned the repository yet,
1. Install Git LFS at https://git-lfs.com/ 

2. Clone the repository via GitHub desktop interface or by typing `git clone https://github.com/pat-kline/survey-bias.git` in your terminal within whatever directory you want to store the project in on your machine

### If you already cloned the repository,
1. Install Git LFS at https://git-lfs.com/

2. Download the actual data files by running `git lfs pull` in the `/survey-bias` directory in your terminal

### Final setup (all users),
1. Add your Dropbox folder path to `globals.R` and `globals.py`
   1. Run `whoami` in your terminal to get your username
   1. In `globals.R`,
      1. Under the `Define user-specific Dropbox root path` section, add an analagous set of lines for your username and Dropbox path as the existing ones for Nico, Monica, and Jordan (i.e., `"your_username" =  "your_dropbox_path"`)
   2. In `globals.py`, 
      1. Do the same as in `globals.R`
      
2. Open an R terminal in VScode in the project directory and the `.Rprofile` will automatically,
   - Activate `renv` for package management
   - Set the working directory
   - Install necessary R packages

3. Confirm the following message displayed in your terminal "🎃 .Rprofile was successfully sourced" 

4. With the above, everything else should be automatically set up when you run any code file (i.e., Python virtual environment, Python packages, etc...)

## Important general notes 

### I. Data and output storage system

Bulk data are tracked with Git LFS and also mirrored in Dropbox. Generated output is not versioned: the repository's `output/` directory is ignored and can be deleted and recreated safely.

1. Set `data_and_output_storage_location` consistently in `globals.R`, `globals.py`, and `globals.do`. The current setting is `dropbox`.
2. In `dropbox` mode, the `data` and `output` path variables resolve beneath `/Survey/consolidated_code/github_data_and_output_mirrors/`.
3. In `github` mode, `data` resolves to the repository's Git-LFS-backed `data/` directory and generated results are written to the ignored local `output/` directory.
4. Two curated Yimfor inputs are deliberately repository-owned and are read from `data/external/` even in Dropbox mode: `yimfor_firm_crosswalk.csv` and `race_shares_fortune1000_kline97.xlsx`.
5. To refresh the GitHub data snapshot from Dropbox, copy only the reviewed `data/` contents into the repository, inspect the changes, and commit the resulting Git LFS pointers. Do not copy generated `output/` files into version control.

References to `data/...` and `output/...` below mean paths beneath the storage root selected by these globals unless a script explicitly uses the repository root.

### II. Sourcing globals at the top of scripts

- *For R scripts, run `source("code/globals.R")` at the top of your script to load global variables and packages*
- *For Stata scripts, run `do "${github}/survey-bias/code/globals.do"` at the top of your script. This requires `${github}` to be defined in your personal `profile.do` (pointing at your local GitHub root, e.g. `global github "/Users/<username>/GitHub"`). Place `profile.do` in Stata's personal ado directory --- run `display c(sysdir_personal)` in Stata to find the path.*

---
# For any issues on the above or below, reach out to me at `nrotundo@berkeley.edu`

## Todo
1. We should get package management working properly for both R and Python (i.e., `renv` for R and virtual environment for Python) so that we can ensure reproducibility across time

2. Separately, need to figure out the EML integration and adjust the codebase accordingly

## Code structure

The pipeline runs in three stages, each with its own metafile. The trees below show source/call hierarchy: indented files are sourced (or invoked) by the parent.

### 1. Data build --- `code/1_data_build/`

**Input:** `data/raw/` (Qualtrics survey CSVs), `data/external/` (RefUSA, AER replication package)
**Output:** `data/processed/long_survey_final.csv` + firm--industry crosswalks

- `!metafile.R` --- runs Python crosswalk scripts then R sample prep
  - `clean_raw_qualtrics_data.py` --- raw Qualtrics export → cleaned long survey
  - `create_firm_industry_crosswalk_aer_replication_package.py` --- AER package → SIC mapping
  - `create_firm_industry_crosswalk_refusa.py` --- RefUSA → SIC mapping
  - `create_firm_industry_crosswalk_industry_map.py` --- harmonizes across sources, writes final crosswalk
  - `sample_prep.R` --- applies sample restrictions; writes `long_survey_final.csv`
    - `helper_functions/sample_eligibility_helpers.R` --- identifies respondents with at least three valid firm ratings for an outcome
  - `build_eeo1_latest_firm_shares.R` --- builds the latest-filing firm EEO-1 shares used in the EEO-1/Yimfor comparison

### 2. Analysis --- `code/2_analysis/`

**Input:** `data/processed/long_survey_final.csv`; the Yimfor analysis also uses the curated `data/external/yimfor_firm_crosswalk.csv` and `data/external/race_shares_fortune1000_kline97.xlsx`
**Output:** `output/intermediate/{Full_Sample, Subset_*}/*.parquet`

- Each `Subset_*` run writes only `Coefficients` (with MLE `estimate` and EB-shrunk `eb` columns), `rcov`, and `variance`, and only for the pooled race, pooled gender, and conduct-age outcomes used by subgroup exhibits.
- The `Full_Sample` run writes those three sheets for the full outcome set and additionally writes `covariance`, `EIV_firm`, `belief_amad_summary`, `belief_likert_amad_firm`, `LinkedIn_firm_shares`, `LinkedIn_belief_share_regressions`, `EIV_linkedin_shares`, and `EIV_belief_selectivity`.
- Correlations are not stored as an intermediate sheet. Section 3 builds the needed correlation rows on demand from the full-sample `variance` and `covariance` sheets.

**Models implemented and enabled:** **Borda + OLS**. Downstream `3_create_tables_figures/` scripts use the models present in the `variance` and `Coefficients` sheets.

- `!metafile.R` --- runs the full-sample pipeline and AMAD/Yimfor add-ons, runs the reduced pipeline for 18 subgroups, then writes the full-sample belief-selectivity EIV sheet
  - `load_all.R` --- sources every helper below (no work itself)
    - `analysis_pipeline.R` --- writes coefficients, robust covariance matrices, and variance components for every run; only the full-sample run continues to pairwise covariance and `EIV_firm`
    - **Model fitting**
      - `run_models_helpers.R` --- orchestrates Borda and OLS estimation across outcomes and samples
      - `construct_firm_level_estimates.R` --- constructs firm estimates, covariance matrices, and empirical-Bayes estimates
      - `compute_firm_mean_ratings.R` --- mean-estimator and influence-function calculations
    - **Data prep**
      - `prep_outcomes.R` --- per-model outcome construction
      - `create_wide_rankings.R` --- long ranks → wide ranking matrix
    - **Score helpers**
      - `borda_score.R` --- per-respondent Borda computation with reference-firm normalization
    - **Variance / covariance / correlation** (post-fit aggregation across firms)
      - `variance_functions.R` --- per-outcome `variance`, `noise`, `signal`
      - `covariance_functions.R` --- pairwise covariance + cross-sample noise
      - `correlation_function.R` --- builds `corr`, `corr_den`, and noise-corrected `corr_c` in memory when a Section 3 exhibit requests them
      - `katz_correct.R` --- positivity correction for variance components
      - `EB_procedure.R` --- two-step empirical Bayes shrinkage of firm estimates (writes the `eb` column on `Coefficients`)
      - `belief_summary_amad.R` --- writes `belief_amad_summary` and `belief_likert_amad_firm` from arm/framing-restricted Likert and Borda respondent-pair statistics
    - **EIV regressions**
      - `eivreg.R` --- measurement-error regression with `Σ_error`
      - `eiv_functions.R` --- shared EIV reshaping, measurement-error, and regression helpers
      - `belief_selectivity_eiv.R` --- full-sample belief/selectivity EIV regressions with AER industry fixed effects used by Table 7
      - `eeo1_naics3_shares.R` --- loads the national EEO-1 NAICS3 shares used for three Yimfor fallback firms
      - `make_industry_means.R` --- constructs equal-weight industry means and within-industry firm deviations
    - **Misc**
      - `experimental.R` --- validates and selects the retained firm-level race and gender log contact gaps
      - `helper_functions/sheet_functions.R` --- `read_parquet_sheet` / `write_parquet_sheet`
  - `linkedin_share_analysis.R` --- writes the full-sample Yimfor share, belief/share-regression, and EIV-control sheets
  - `eeo1_yimfor_correlations.R` --- reports the latest-filing EEO-1/Yimfor correlations used in the draft footnote

### 3. Tables and figures --- `code/3_create_tables_figures/`

**Input:** `output/intermediate/*/*.parquet`, plus the processed survey and external LinkedIn workforce-composition data used by the tables
**Output:** `output/tables/*.tex`, `output/figures/*.png`

- `!metafile.R` --- sources each table/figure script in order
  - `summary_statistics_tables.R` --- respondent demographic composition table and rating-confidence share table
  - `summary_statistics_histograms.R` --- response duration histogram
  - `summary_statistics_bar_graphs.R` --- draft survey-response bar graphs: contact/conduct ratings, feared-discrimination shares by race within each subsample, and conduct-arm information sources
  - `belief_summary_ols_borda.R` --- belief-summary table by aggregation method
  - `summary_variance_table.R` --- bias-corrected SD / signal-SD / reliability / t-stat tables for the standard and alternate outcomes
  - `firm_ratings_signal_correlation_heatmaps.R` --- full-sample across-measure signal-correlation heatmap (Likert lower / Borda upper)
  - `cross_sample_signal_corr.R` --- signal correlation across paired subsamples + Wald test
  - `top_bottom_firm_ratings_dual_axis_figures.R` --- draft Likert + Borda dual-axis ratings for the 25 highest / 25 lowest firms by Borda EB
  - `industry_ratings_dual_axis_figures.R` --- Likert + Borda dual-axis ratings for every industry and the within-industry top/bottom-25 firms
  - `average_beliefs_vs_linkedin_workforce_shares_table.R` --- average beliefs versus Yimfor LinkedIn workforce shares
  - `eiv_pooled_belief_selectivity_controls_table.R` --- pooled-belief EIV table with belief-selectivity controls
  - `eiv_table_selectivity_discretion.R` --- univariate selectivity/discretion EIV table (`EIV_univariate_wt_ols_borda_w_gender_sq.tex`)
  - `valence_correlation_bars.R` --- bar chart of `corr_c` for valence pairs
  - `eiv_coefplot_by_subgroup.R` --- coefplot of subgroup-split EIV slopes (njobs-weighted Katz noise), with slope-difference annotations
  - `firm_likert_amad_scatterplots.R` --- firm-level Likert rating versus AMAD appendix figures
  - `summary_variance_within_between.R` --- variance table decomposed into within- vs between-industry panels
  - `subgroup_belief_mean_signal_variance_table.R` --- subgroup means and signal-variance appendix table
  - `cross_sample_signal_corr_raw.R` --- cross-sample correlation table without noise correction
  - `eiv_contact_conduct_subsamples_appendix.R` --- contact vs. conduct EIV appendix table (`EIV_contact_conduct_subsamples_appendix.tex`)
  - `eiv_linkedin_share_controls_table.R` --- full-sample EIV contact-gap table with 2023 Yimfor LinkedIn workforce-share controls and NAICS3-clustered standard errors in the share-control columns
  - `firm_belief_estimates_by_aggregation_method_table.do` --- appendix table of firm-level belief estimates by aggregation method (Stata, run in batch mode via the metafile's `run_stata_fail_fast`)

`summary_outcomes_config.R` is a sourced helper containing the shared outcome lists, labels, and formatting functions used by the variance-table scripts.

## Documentation on codebase

1. Infrastructure code folders/files 
   
   1. `/.Rprofile` --- File that auto-executes upon R startup (analogous to Stata profile.do file); does the following,
      1. Activates renv (which manages R package versions for the project)
      2. Checks if necessary R packages are installed (and installs them if not)
         1. `here` --- Allows for the relative file paths in the codebase to function without setting manually 
         2. `jsonlite, rlang` --- Necessary for renv to function properly
      3. Sets working to the project root directory (i.e., /survey-bias) and checks that this was successful (since all file paths in the codebase are relative to this root directory)
   
   2. `/code/globals.R` --- File that should be placed at the top of all R scripts in the codebase (XXcould put this in .Rprofile instead, but then any changes to globals.R would require restarting R to take effect); does the following, 
      1. Defines R global variables used across the codebase (i.e., file paths currently, but could include e.g., constants, formatting, etc... later on) 
         1. Filepaths to various folders in the project (e.g., data, code, results, etc...)
            1. Any changes to filepaths in `globals.R` should be mirrored in `globals.py`
      2. Checks installation (and auto-installs if missing) required packages for R into the `renv` folder and loads them 
      3. Initializes Python virtual environment in the `.venv` folder (if not already created)
         1. `.venv` is analagous to R's `renv` 
         2. However, unlike `renv`, Python virtual environments should not be saved to version control (and I think is automatically not)
         3. Thus, it has to be initialized on each user's machine when they first run the codebase, and the code does this automatically
      4. Checks installation (and auto-installs if missing) required packages for Python into the `.venv` virtual environment
      
   3. `/code/globals.py` --- File that defines Python global variables used across the codebase
      1. Compared to `globals.R`, you do not run this file, but instead import the specific filepaths you need into your Python scripts
      2. Any changes to filepaths in `globals.R` should be mirrored here (XXaiming to automate this in the future, perhaps using a JSON/YAML file as the single source of truth for filepaths that gets fed into `globals.R` and `globals.py`)
      
2. Masterfiles 
   1. `/code/metafile.` --- File that will eventually execute the entire project top-to-bottom (and is useful generally for documenting the project workflow)
      1. Currently executes sub-metafiles for each major step of the project, which are housed in subdirectories of `/code/`; currently --- data build, analysis, results
      2. If an additional major step is added to the project that does not fit into these existing categories, a new subdirectory and sub-metafile should be created for it
   
   2. `/code/XX/metafile.R` --- File that executes all R scripts in a given subdirectory in the correct order
      1. Whenever a new code file is created, it should be added to the appropriate sub-metafile in the correct order

3. Git LFS hook policy (fail-closed)
   1. This repo uses fail-closed hooks in `.githooks/` for `post-checkout`, `post-merge`, and `pre-push`
   2. If a hook cannot parse/validate `data_and_output_storage_location` in `code/globals.R` or cannot apply the expected LFS mode, the Git operation is blocked
   3. Hook changes are repo-local (`git lfs install --local --skip-repo ...` and `git config --local ...`), so this does not modify global Git behavior on a machine 
   
4. External data files (i.e., not generated by the codebase here) --- stored in the `/data/external` folder
   1. `2019_Business_Academic_QCQ.txt.gz` --- RefUSA data used for the `A Discrimination Report Card` AER industry code classifications

5. Archive files in `/data/archive`
   1. `industry_map.xlsx` is a deprecated file that we used to draw our industry codes, `aer_naics2`, from
      1. However, it was unclear where this file and these industry codes came from, so added a file in `data_build` that draws the industry codes directly from the RefUSA data
      2. Added this file to the archive folder for comparison 
      3. See `https://github.com/patkline/survey-bias/issues/48` for more details on what we did here 
