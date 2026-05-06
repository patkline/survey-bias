/* -----------------------------------------------------------------------------------------------------------
Purpose: Compare paper Figure 9 CPS-derived points to rr_misc, corrected-cleaning,
and robustness CPS rebuilds

Created: Nico Rotundo 2026-05-05
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Load parsed paper Figure 9 points
----------------------------------------------------------------------------------------------------------- */
* Load parsed paper coefficients and confidence intervals from the paper PDFs
use "${qje_2022_replication_dump}/figure9_paper_pdf_points.dta", clear

* Confirm parsed paper points are unique at the comparison row level
gisid gap characteristic estimate_type

* Confirm parsed paper points contain both paper panels, all Figure 9 rows, and both estimate types
assert _N == 44

/* -----------------------------------------------------------------------------------------------------------
Keep CPS-derived Figure 9 rows
----------------------------------------------------------------------------------------------------------- */
* Keep the Figure 9 rows produced by the CPS wage and college-share rebuilds
keep if inlist(characteristic, "wage_level_race", "wage_gap_race", "col_share_gap_race", ///
    "wage_level_gender", "wage_gap_gender", "col_share_gap_gender")

* Confirm the comparison shell has one row per CPS characteristic, panel, and estimate type
gisid gap characteristic estimate_type
assert _N == 24

* Save parsed paper CPS rows so the script can load the rebuild coefficient datasets
tempfile paper_cps_points
save `paper_cps_points', replace

/* -----------------------------------------------------------------------------------------------------------
Load CPS rebuild coefficients
----------------------------------------------------------------------------------------------------------- */
* Load rr_misc rebuild coefficients saved by create_figure_9_cps.do
use "${qje_2022_replication_dump}/figure9_cps_rebuild_coefficients.dta", clear

* Rename rebuild columns so paper and rebuild estimates can coexist
rename coefficient cps_coefficient
rename se cps_se

* Label these rows as the rr_misc CPS rebuild
gen rebuild_type = "rr_misc"
assert ~mi(rebuild_type)
gisid gap characteristic estimate_type
assert _N == 24

* Save the rr_misc rebuild coefficients before loading the corrected-cleaning rebuild
tempfile rr_misc_cps_coefficients
save `rr_misc_cps_coefficients', replace

* Load corrected-cleaning rebuild coefficients saved by create_figure_9_cps_new_cleaning.do
use "${qje_2022_replication_dump}/figure9_cps_new_cleaning_rebuild_coefficients.dta", clear

* Rename rebuild columns so paper and rebuild estimates can coexist
rename coefficient cps_coefficient
rename se cps_se

* Label these rows as the corrected-cleaning CPS rebuild
gen rebuild_type = "new_cleaning"
assert ~mi(rebuild_type)
gisid gap characteristic estimate_type
assert _N == 24

* Append the rr_misc rebuild rows below the corrected-cleaning rebuild rows
append using `rr_misc_cps_coefficients'

* Confirm each rebuild has one coefficient for each CPS characteristic, panel, and estimate type
gisid rebuild_type gap characteristic estimate_type
assert _N == 48

/* -----------------------------------------------------------------------------------------------------------
Merge paper points onto rebuild coefficients
----------------------------------------------------------------------------------------------------------- */
* Merge parsed paper coefficients and confidence intervals onto each rebuild coefficient row
merge m:1 gap characteristic estimate_type using `paper_cps_points'
assert _merge == 3
drop _merge

* Confirm the comparison dataset has one row per rebuild, CPS characteristic, panel, and estimate type
gisid rebuild_type gap characteristic estimate_type
assert _N == 48

/* -----------------------------------------------------------------------------------------------------------
Create paper-vs-rebuild comparison table
----------------------------------------------------------------------------------------------------------- */
* Keep rr_misc rebuild rows as the base comparison table
keep if inlist(rebuild_type, "rr_misc")

* Keep paper fields and rr_misc coefficient fields for the final table
keep gap characteristic estimate_type paper_coefficient paper_ci_lower paper_ci_upper paper_se ///
    cps_coefficient cps_se
gisid gap characteristic estimate_type
assert _N == 24

* Save the rr_misc comparison rows before loading corrected-cleaning rows
tempfile rr_misc_comparison
save `rr_misc_comparison', replace

* Load corrected-cleaning rebuild coefficients
use "${qje_2022_replication_dump}/figure9_cps_new_cleaning_rebuild_coefficients.dta", clear

* Rename corrected-cleaning coefficient to the old comparison-table column name
rename coefficient cps_new_cleaning_coefficient
rename se cps_new_cleaning_se

* Keep the corrected-cleaning coefficient for each CPS characteristic, panel, and estimate type
keep gap characteristic estimate_type cps_new_cleaning_coefficient cps_new_cleaning_se
gisid gap characteristic estimate_type
assert _N == 24

* Merge corrected-cleaning coefficients onto the rr_misc comparison rows
merge 1:1 gap characteristic estimate_type using `rr_misc_comparison'
assert _merge == 3
drop _merge

* Save comparison rows before loading the hourwage robustness coefficients
tempfile comparison_with_new_cleaning
save `comparison_with_new_cleaning', replace

* Load hourwage robustness rebuild coefficients
use "${qje_2022_replication_dump}/figure9_cps_hourwage_rebuild_coefficients.dta", clear

* Rename hourwage robustness coefficients to comparison-table column names
rename coefficient cps_hourwage_coefficient
rename se cps_hourwage_se

* Keep the hourwage robustness coefficient for each CPS characteristic, panel, and estimate type
keep gap characteristic estimate_type cps_hourwage_coefficient cps_hourwage_se
gisid gap characteristic estimate_type
assert _N == 24

* Merge hourwage robustness coefficients onto the comparison rows
merge 1:1 gap characteristic estimate_type using `comparison_with_new_cleaning'
assert _merge == 3
drop _merge

* Save comparison rows before loading the allocation-flag robustness coefficients
tempfile comparison_with_hourwage
save `comparison_with_hourwage', replace

* Load allocation-flag robustness rebuild coefficients
use "${qje_2022_replication_dump}/figure9_cps_ignore_allocation_flags_rebuild_coefficients.dta", clear

* Rename allocation-flag robustness coefficients to Stata-safe comparison-table column names
rename coefficient cps_no_alloc_flags_coefficient
rename se cps_no_alloc_flags_se

* Keep the allocation-flag robustness coefficient for each CPS characteristic, panel, and estimate type
keep gap characteristic estimate_type cps_no_alloc_flags_coefficient cps_no_alloc_flags_se
gisid gap characteristic estimate_type
assert _N == 24

* Merge allocation-flag robustness coefficients onto the comparison rows
merge 1:1 gap characteristic estimate_type using `comparison_with_hourwage'
assert _merge == 3
drop _merge

* Compute coefficient differences reported in the old comparison CSV
gen double difference_cps_vs_paper = cps_coefficient - paper_coefficient
gen double difference_new_cleaning_vs_paper = cps_new_cleaning_coefficient - paper_coefficient
gen double difference_new_cleaning_vs_cps = cps_new_cleaning_coefficient - cps_coefficient
gen double diff_hourwage_vs_paper = cps_hourwage_coefficient - paper_coefficient
gen double diff_hourwage_vs_new_cleaning = cps_hourwage_coefficient - cps_new_cleaning_coefficient
gen double diff_no_alloc_vs_paper = cps_no_alloc_flags_coefficient - paper_coefficient
gen double diff_no_alloc_vs_new_cleaning = cps_no_alloc_flags_coefficient - cps_new_cleaning_coefficient
assert ~mi(difference_cps_vs_paper)
assert ~mi(difference_new_cleaning_vs_paper)
assert ~mi(difference_new_cleaning_vs_cps)
assert ~mi(diff_hourwage_vs_paper)
assert ~mi(diff_hourwage_vs_new_cleaning)
assert ~mi(diff_no_alloc_vs_paper)
assert ~mi(diff_no_alloc_vs_new_cleaning)

* Compute 95 percent confidence intervals for each rebuild coefficient
gen double cps_ci_lower = cps_coefficient - 1.96 * cps_se
gen double cps_ci_upper = cps_coefficient + 1.96 * cps_se
gen double cps_new_cleaning_ci_lower = cps_new_cleaning_coefficient - 1.96 * cps_new_cleaning_se
gen double cps_new_cleaning_ci_upper = cps_new_cleaning_coefficient + 1.96 * cps_new_cleaning_se
gen double cps_hourwage_ci_lower = cps_hourwage_coefficient - 1.96 * cps_hourwage_se
gen double cps_hourwage_ci_upper = cps_hourwage_coefficient + 1.96 * cps_hourwage_se
gen double cps_no_alloc_flags_ci_lower = cps_no_alloc_flags_coefficient - 1.96 * cps_no_alloc_flags_se
gen double cps_no_alloc_flags_ci_upper = cps_no_alloc_flags_coefficient + 1.96 * cps_no_alloc_flags_se
assert ~mi(cps_ci_lower)
assert ~mi(cps_ci_upper)
assert ~mi(cps_new_cleaning_ci_lower)
assert ~mi(cps_new_cleaning_ci_upper)
assert ~mi(cps_hourwage_ci_lower)
assert ~mi(cps_hourwage_ci_upper)
assert ~mi(cps_no_alloc_flags_ci_lower)
assert ~mi(cps_no_alloc_flags_ci_upper)

* Flag estimates whose 95 percent confidence interval excludes zero
gen byte paper_reject_zero_5pct = paper_ci_lower > 0 | paper_ci_upper < 0
gen byte cps_reject_zero_5pct = cps_ci_lower > 0 | cps_ci_upper < 0
gen byte new_cleaning_reject_zero_5pct = cps_new_cleaning_ci_lower > 0 | cps_new_cleaning_ci_upper < 0
gen byte hourwage_reject_zero_5pct = cps_hourwage_ci_lower > 0 | cps_hourwage_ci_upper < 0
gen byte no_alloc_flags_reject_zero_5pct = cps_no_alloc_flags_ci_lower > 0 | cps_no_alloc_flags_ci_upper < 0
assert ~mi(paper_reject_zero_5pct)
assert ~mi(cps_reject_zero_5pct)
assert ~mi(new_cleaning_reject_zero_5pct)
assert ~mi(hourwage_reject_zero_5pct)
assert ~mi(no_alloc_flags_reject_zero_5pct)

* Flag whether each rebuild point estimate lies inside the paper's plotted confidence interval
gen byte cps_inside_paper_ci = inrange(cps_coefficient, paper_ci_lower, paper_ci_upper)
gen byte new_cleaning_inside_paper_ci = inrange(cps_new_cleaning_coefficient, paper_ci_lower, paper_ci_upper)
gen byte hourwage_inside_paper_ci = inrange(cps_hourwage_coefficient, paper_ci_lower, paper_ci_upper)
gen byte no_alloc_flags_inside_paper_ci = inrange(cps_no_alloc_flags_coefficient, paper_ci_lower, paper_ci_upper)
assert ~mi(cps_inside_paper_ci)
assert ~mi(new_cleaning_inside_paper_ci)
assert ~mi(hourwage_inside_paper_ci)
assert ~mi(no_alloc_flags_inside_paper_ci)

* Create row-order variables that match the old Python comparison output
gen gap_order = 1 if inlist(gap, "white")
replace gap_order = 2 if inlist(gap, "male")
assert ~mi(gap_order)

gen characteristic_order = 1 if inlist(characteristic, "wage_level_race")
replace characteristic_order = 2 if inlist(characteristic, "wage_gap_race")
replace characteristic_order = 3 if inlist(characteristic, "col_share_gap_race")
replace characteristic_order = 4 if inlist(characteristic, "wage_level_gender")
replace characteristic_order = 5 if inlist(characteristic, "wage_gap_gender")
replace characteristic_order = 6 if inlist(characteristic, "col_share_gap_gender")
assert ~mi(characteristic_order)

gen estimate_type_order = 1 if inlist(estimate_type, "posterior_mean")
replace estimate_type_order = 2 if inlist(estimate_type, "linear_shrinkage")
assert ~mi(estimate_type_order)

* Sort rows into the old Python comparison output order
sort gap_order characteristic_order estimate_type_order
drop gap_order characteristic_order estimate_type_order

* Order old comparison columns first, followed by standard error and inference columns
order gap characteristic estimate_type paper_coefficient cps_coefficient cps_new_cleaning_coefficient ///
    cps_hourwage_coefficient cps_no_alloc_flags_coefficient ///
    difference_cps_vs_paper difference_new_cleaning_vs_paper difference_new_cleaning_vs_cps ///
    diff_hourwage_vs_paper diff_hourwage_vs_new_cleaning ///
    diff_no_alloc_vs_paper diff_no_alloc_vs_new_cleaning ///
    paper_se cps_se cps_new_cleaning_se cps_hourwage_se cps_no_alloc_flags_se ///
    paper_ci_lower paper_ci_upper cps_ci_lower cps_ci_upper ///
    cps_new_cleaning_ci_lower cps_new_cleaning_ci_upper ///
    cps_hourwage_ci_lower cps_hourwage_ci_upper ///
    cps_no_alloc_flags_ci_lower cps_no_alloc_flags_ci_upper ///
    paper_reject_zero_5pct cps_reject_zero_5pct new_cleaning_reject_zero_5pct ///
    hourwage_reject_zero_5pct no_alloc_flags_reject_zero_5pct ///
    cps_inside_paper_ci new_cleaning_inside_paper_ci ///
    hourwage_inside_paper_ci no_alloc_flags_inside_paper_ci

* Format numeric columns with enough precision for the CSV comparison
format paper_coefficient cps_coefficient cps_new_cleaning_coefficient ///
    cps_hourwage_coefficient cps_no_alloc_flags_coefficient ///
    difference_cps_vs_paper difference_new_cleaning_vs_paper difference_new_cleaning_vs_cps ///
    diff_hourwage_vs_paper diff_hourwage_vs_new_cleaning ///
    diff_no_alloc_vs_paper diff_no_alloc_vs_new_cleaning ///
    paper_se cps_se cps_new_cleaning_se cps_hourwage_se cps_no_alloc_flags_se ///
    paper_ci_lower paper_ci_upper cps_ci_lower cps_ci_upper ///
    cps_new_cleaning_ci_lower cps_new_cleaning_ci_upper ///
    cps_hourwage_ci_lower cps_hourwage_ci_upper ///
    cps_no_alloc_flags_ci_lower cps_no_alloc_flags_ci_upper %24.17g

* Export the paper-vs-rebuild comparison table
export delimited using "${qje_2022_replication_tables}/figure9_cps_pdf_point_comparison.csv", replace
