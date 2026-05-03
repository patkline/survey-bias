/* -----------------------------------------------------------------------------------------------------------
Purpose: Metafile for the QJE 2022 (Kline-Rose-Walters) Figure 9 replication.
Reproduces the CPS-derived covariates of Figure 9 (Panel A non-Black-Black,
Panel B male-female) under both the rr_misc cleaning baseline and the proposed
production-style new-cleaning approach, then validates against the paper's
shipped Figure 9 PDFs.

Created: Ghost 2026-04-24

XX: The firm-level NAICS shell comes from Dropbox Survey
    consolidated_code/external/qje_data.dta because the lifted replication
    data.dta omits naics/naics3/sic_code.
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Confirm code files are present (in code/qje_2022_replication/)
----------------------------------------------------------------------------------------------------------- */
foreach file in ///
    "wage_regressions.do" ///
    "wage_regressions_new_cleaning_approach.do" ///
    "create_figure_9_industry_shell.do" ///
    "create_figure_9_cps.do" ///
    "create_figure_9_cps_new_cleaning.do" ///
    "compare_cps_points_to_paper.py" ///
    "render_figure_pdfs_to_png.py" {

    cap confirm file "${qje_2022_replication_code}/`file'"
    if _rc {
        di as error "🪦 Missing replication code file: ${qje_2022_replication_code}/`file'"
        exit 601
    }
}

/* -----------------------------------------------------------------------------------------------------------
Confirm input data files are present
----------------------------------------------------------------------------------------------------------- */
* IPUMS CPS extract lives at the data_and_outputs root
cap confirm file "${qje_2022_data_and_outputs}/cps_00086.dta"
if _rc {
    di as error "🪦 Missing CPS extract: ${qje_2022_data_and_outputs}/cps_00086.dta"
    exit 601
}

* Paper-shipped artifacts live inside the paper_replication_package subfolder
foreach file in ///
    "dump/sic_names.csv" ///
    "dump/white_posterior_features.csv" ///
    "dump/male_posterior_features.csv" ///
    "figures/figure9_white.pdf" ///
    "figures/figure9_male.pdf" {

    cap confirm file "${qje_2022_replication_package}/`file'"
    if _rc {
        di as error "🪦 Missing paper-shipped input file: ${qje_2022_replication_package}/`file'"
        exit 601
    }
}

* Confirm original QJE industry shell source is present in Dropbox external
cap confirm file "${dropbox_survey_bias_root}/external/qje_data.dta"
if _rc {
    di as error "🪦 Missing QJE industry shell source: ${dropbox_survey_bias_root}/external/qje_data.dta"
    exit 601
}

/* -----------------------------------------------------------------------------------------------------------
Run order
----------------------------------------------------------------------------------------------------------- */
* Step 1: Posterior firm-effect inputs (shipped lifted from replication package)
di as text "🎃 Step 1: posterior firm-effect CSVs already lifted into ${qje_2022_replication_package}/dump/"

* Note. The released posterior-feature CSVs are already lifted from the QJE
* replication package. The full deconvolve.R script reads omitted
* covariates/federal_contractors.csv, so a Figure 9-only extraction stops
* after the lifted posterior-feature CSVs.

* Step 2: Recreate CPS-derived Figure 9 covariates (rr_misc baseline)
di as text "🎃 Step 2: rr_misc CPS covariate construction (writes to dump/covariates/)"

do "${qje_2022_replication_code}/wage_regressions.do"

local missing_covars = 0
foreach file in ///
    "covariates/sic_race_wage_gap.dta" ///
    "covariates/sic_gender_wage_gap.dta" ///
    "covariates/sic_shares.dta" ///
    "covariates/naics3_race_wage_gap.dta" ///
    "covariates/naics3_gender_wage_gap.dta" ///
    "covariates/naics3_shares.dta" {

    cap confirm file "${qje_2022_replication_dump}/`file'"
    if _rc {
        local missing_covars = 1
        di as error "🧌 Missing rr_misc CPS-derived covariate: ${qje_2022_replication_dump}/`file'"
    }
}

if `missing_covars' {
    di as error "🪦 Cannot proceed until rr_misc CPS-derived covariate inputs are reconstructed"
    exit 601
}

* Step 3: Extract the firm-level industry shell used by Figure 9
di as text "🎃 Step 3: extract firm-level NAICS shell from qje_data.dta"

do "${qje_2022_replication_code}/create_figure_9_industry_shell.do"

cap confirm file "${qje_2022_replication_dump}/figure9_industry_shell.dta"
if _rc {
    di as error "🪦 Missing Figure 9 firm industry shell: ${qje_2022_replication_dump}/figure9_industry_shell.dta"
    exit 601
}

* Step 4: rr_misc Figure 9 regressions and figure export
di as text "🎃 Step 4: rr_misc posterior-gap regressions and CPS-only Figure 9 PDFs"

do "${qje_2022_replication_code}/create_figure_9_cps.do"

foreach file in ///
    "figure9_cps_white.pdf" ///
    "figure9_cps_male.pdf" {

    cap confirm file "${qje_2022_replication_figures}/`file'"
    if _rc {
        di as error "🪦 Missing rr_misc CPS Figure 9 output: ${qje_2022_replication_figures}/`file'"
        exit 601
    }
}

* Step 5: Recreate CPS-derived Figure 9 covariates (new-cleaning approach)
di as text "🎃 Step 5: new-cleaning CPS covariate construction (writes to dump/covariates_new_cleaning/)"

do "${qje_2022_replication_code}/wage_regressions_new_cleaning_approach.do"

local missing_new_covars = 0
foreach file in ///
    "covariates_new_cleaning/sic_race_wage_gap.dta" ///
    "covariates_new_cleaning/sic_gender_wage_gap.dta" ///
    "covariates_new_cleaning/sic_shares.dta" ///
    "covariates_new_cleaning/naics3_race_wage_gap.dta" ///
    "covariates_new_cleaning/naics3_gender_wage_gap.dta" ///
    "covariates_new_cleaning/naics3_shares.dta" {

    cap confirm file "${qje_2022_replication_dump}/`file'"
    if _rc {
        local missing_new_covars = 1
        di as error "🧌 Missing new-cleaning CPS-derived covariate: ${qje_2022_replication_dump}/`file'"
    }
}

if `missing_new_covars' {
    di as error "🪦 Cannot proceed until new-cleaning CPS-derived covariate inputs are reconstructed"
    exit 601
}

* Step 6: New-cleaning Figure 9 regressions and figure export
di as text "🎃 Step 6: new-cleaning posterior-gap regressions and CPS-only Figure 9 PDFs"

do "${qje_2022_replication_code}/create_figure_9_cps_new_cleaning.do"

foreach file in ///
    "figure9_cps_new_cleaning_white.pdf" ///
    "figure9_cps_new_cleaning_male.pdf" {

    cap confirm file "${qje_2022_replication_figures}/`file'"
    if _rc {
        di as error "🪦 Missing new-cleaning CPS Figure 9 output: ${qje_2022_replication_figures}/`file'"
        exit 601
    }
}

* Step 7: PDF-parse comparison of paper Figure 9 vs. both rebuilds
di as text "🎃 Step 7: parse paper Figure 9 PDFs and compare against numeric coefficients from both rebuilds"

shell "${python_venv_installation}" "${qje_2022_replication_code}/compare_cps_points_to_paper.py" ///
    "${qje_2022_replication_package}" ///
    "${qje_2022_replication_dump}" ///
    "${qje_2022_replication_tables}"

cap confirm file "${qje_2022_replication_tables}/figure9_cps_pdf_point_comparison.csv"
if _rc {
    di as error "🪦 Missing Figure 9 comparison output: ${qje_2022_replication_tables}/figure9_cps_pdf_point_comparison.csv"
    exit 601
}

* Step 8: Render every figure PDF to PNG for GitHub issue embedding
di as text "🎃 Step 8: render figure PDFs to PNG at figures/png/"

shell "${python_venv_installation}" "${qje_2022_replication_code}/render_figure_pdfs_to_png.py" ///
    "${qje_2022_replication_figures}"
