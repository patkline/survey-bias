/* -----------------------------------------------------------------------------------------------------------
Purpose: Metafile for the QJE 2022 (Kline-Rose-Walters) Figure 9 replication.
Reproduces the CPS-derived covariates of Figure 9 (Panel A non-Black-Black,
Panel B male-female) under the rr_misc cleaning baseline, the proposed
production-style new-cleaning approach, and robustness variants, then validates
against the paper's shipped Figure 9 PDF points.

Created: Nico Rotundo 2026-04-24

XX: The firm-level NAICS shell comes from Dropbox Survey
    consolidated_code/external/qje_data.dta because the lifted replication
    data.dta omits naics/naics3/sic_code.
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Figure 9 cps subset
----------------------------------------------------------------------------------------------------------- */
* Step 1: Posterior firm-effect inputs (shipped lifted from replication package)
di as text "🎃 Step 1: posterior firm-effect CSVs already lifted into ${qje_2022_replication_package}/dump/"

* Step 2: Recreate CPS-derived Figure 9 covariates (rr_misc baseline)
do "${qje_2022_replication_code}/wage_regressions.do"

* Step 3: Extract the firm-level industry shell used by Figure 9
do "${qje_2022_replication_code}/create_figure_9_industry_shell.do"

* Step 4: rr_misc Figure 9 regressions and figure export
do "${qje_2022_replication_code}/create_figure_9_cps.do"

* Step 5: Recreate CPS-derived Figure 9 covariates (new-cleaning approach)
do "${qje_2022_replication_code}/wage_regressions_new_cleaning_approach.do"

* Step 6: New-cleaning Figure 9 regressions and figure export
do "${qje_2022_replication_code}/create_figure_9_cps_new_cleaning.do"

* Step 7: Recreate CPS-derived Figure 9 covariates for robustness variants
do "${qje_2022_replication_code}/wage_regressions_robustness_variants.do"

* Step 8: Robustness Figure 9 regressions and figure export
do "${qje_2022_replication_code}/create_figure_9_cps_robustness_variants.do"

* Step 9: PDF-parse of paper Figure 9 
shell "${python_venv_installation}" "${qje_2022_replication_code}/parse_figure_9_pdf_points.py"

* Step 10: Compare CPS-derived covariates to paper Figure 9 points
do "${qje_2022_replication_code}/compare_cps_points_to_paper.do"

* Step 11: Render every figure PDF to PNG for GitHub issue embedding
shell "${python_venv_installation}" "${qje_2022_replication_code}/render_figure_pdfs_to_png.py" ///
    "${qje_2022_replication_figures}"

/* -----------------------------------------------------------------------------------------------------------
Full figure 9
----------------------------------------------------------------------------------------------------------- */