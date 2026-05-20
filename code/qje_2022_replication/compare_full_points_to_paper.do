/* -----------------------------------------------------------------------------------------------------------
Purpose: Compare paper Figure 9 points to the full Figure 9 rebuild

Created: Nico Rotundo 2026-05-20

Reads from: qje_2022_replication_data_and_outputs/data/dump/figure9_paper_pdf_points.dta
           qje_2022_replication_data_and_outputs/data/dump/figure9_full_rebuild_coefficients.dta
Writes to: qje_2022_replication_data_and_outputs/outputs/tables/figure9_full_pdf_point_comparison.csv

XX: EEO-1 rows are approximate because the original
    covariates/black_employment_shares_3dig.csv input is unrecovered.
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Load parsed paper Figure 9 points
----------------------------------------------------------------------------------------------------------- */
* Load parsed paper coefficients and confidence intervals from the paper PDFs
use "${qje_2022_replication_dump}/figure9_paper_pdf_points.dta", clear

* Confirm parsed paper points contain both paper panels, all Figure 9 rows, and both estimate types
gisid gap characteristic estimate_type
assert _N == 44

tempfile paper_points
save `paper_points', replace

/* -----------------------------------------------------------------------------------------------------------
Load full Figure 9 rebuild coefficients
----------------------------------------------------------------------------------------------------------- */
* Load full rebuild coefficients saved by create_figure_9_full.do
use "${qje_2022_replication_dump}/figure9_full_rebuild_coefficients.dta", clear

* Rename rebuild columns so paper and rebuild estimates can coexist
rename coefficient full_coefficient
rename se full_se

* Confirm the rebuild has one row per characteristic, panel, and estimate type
gisid gap characteristic estimate_type
assert _N == 44

/* -----------------------------------------------------------------------------------------------------------
Merge paper points onto rebuild coefficients
----------------------------------------------------------------------------------------------------------- */
* Merge parsed paper coefficients and confidence intervals onto each rebuild coefficient row
merge 1:1 gap characteristic estimate_type using `paper_points'
assert _merge == 3
drop _merge

* Confirm the comparison dataset has one row per full Figure 9 characteristic, panel, and estimate type
gisid gap characteristic estimate_type
assert _N == 44

/* -----------------------------------------------------------------------------------------------------------
Create paper-vs-rebuild comparison table
----------------------------------------------------------------------------------------------------------- */
* Label whether each row comes from an exact or approximate rebuilt input
gen input_recovery_status = "cps_recovered"
replace input_recovery_status = "eeo1_puf_approximation" if inlist(characteristic, ///
    "share_black_ind", "mgmt_dif_black_ind", "share_female_ind", "mgmt_dif_female_ind")
replace input_recovery_status = "census_recovered" if inlist(characteristic, "top4share")
assert ~mi(input_recovery_status)

* Compute coefficient differences
gen double difference_full_vs_paper = full_coefficient - paper_coefficient
assert ~mi(difference_full_vs_paper)

* Compute 95 percent confidence intervals for each rebuild coefficient
gen double full_ci_lower = full_coefficient - 1.96 * full_se
gen double full_ci_upper = full_coefficient + 1.96 * full_se
assert ~mi(full_ci_lower)
assert ~mi(full_ci_upper)

* Flag estimates whose 95 percent confidence interval excludes zero
gen byte paper_reject_zero_5pct = paper_ci_lower > 0 | paper_ci_upper < 0
gen byte full_reject_zero_5pct = full_ci_lower > 0 | full_ci_upper < 0
assert ~mi(paper_reject_zero_5pct)
assert ~mi(full_reject_zero_5pct)

* Flag whether the rebuild point estimate lies inside the paper's plotted confidence interval
gen byte full_inside_paper_ci = inrange(full_coefficient, paper_ci_lower, paper_ci_upper)
assert ~mi(full_inside_paper_ci)

* Create row-order variables that match the paper Figure 9 ordering
gen gap_order = 1 if inlist(gap, "white")
replace gap_order = 2 if inlist(gap, "male")
assert ~mi(gap_order)

gen characteristic_order = 1 if inlist(characteristic, "wage_level_race")
replace characteristic_order = 2 if inlist(characteristic, "wage_gap_race")
replace characteristic_order = 3 if inlist(characteristic, "share_black_ind")
replace characteristic_order = 4 if inlist(characteristic, "mgmt_dif_black_ind")
replace characteristic_order = 5 if inlist(characteristic, "col_share_gap_race")
replace characteristic_order = 6 if inlist(characteristic, "wage_level_gender")
replace characteristic_order = 7 if inlist(characteristic, "wage_gap_gender")
replace characteristic_order = 8 if inlist(characteristic, "share_female_ind")
replace characteristic_order = 9 if inlist(characteristic, "mgmt_dif_female_ind")
replace characteristic_order = 10 if inlist(characteristic, "col_share_gap_gender")
replace characteristic_order = 11 if inlist(characteristic, "top4share")
assert ~mi(characteristic_order)

gen estimate_type_order = 1 if inlist(estimate_type, "posterior_mean")
replace estimate_type_order = 2 if inlist(estimate_type, "linear_shrinkage")
assert ~mi(estimate_type_order)

* Sort rows into Figure 9 order
sort gap_order characteristic_order estimate_type_order
drop gap_order characteristic_order estimate_type_order

* Order comparison columns
order gap characteristic estimate_type input_recovery_status paper_coefficient full_coefficient ///
    difference_full_vs_paper paper_se full_se paper_ci_lower paper_ci_upper ///
    full_ci_lower full_ci_upper paper_reject_zero_5pct full_reject_zero_5pct ///
    full_inside_paper_ci

* Format numeric columns with enough precision for the CSV comparison
format paper_coefficient full_coefficient difference_full_vs_paper paper_se full_se ///
    paper_ci_lower paper_ci_upper full_ci_lower full_ci_upper %24.17g

* Export the paper-vs-rebuild comparison table
export delimited using "${qje_2022_replication_tables}/figure9_full_pdf_point_comparison.csv", replace
