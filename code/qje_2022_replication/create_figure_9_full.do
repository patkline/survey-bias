/* -----------------------------------------------------------------------------------------------------------
Purpose: Recreate the full QJE Figure 9 using recovered CPS, EEO-1, and
Census concentration industry covariates

Created: Nico Rotundo 2026-05-20

Reads from: qje_2022_replication_data_and_outputs/data/dump/figure9_industry_shell.dta
           qje_2022_replication_data_and_outputs/data/dump/figure9_full_eeo1_industry_covariates.dta
           qje_2022_replication_data_and_outputs/data/dump/figure9_full_concentration_naics.dta
Writes to: qje_2022_replication_data_and_outputs/data/dump/figure9_full_rebuild_coefficients.dta
           qje_2022_replication_data_and_outputs/outputs/figures/figure9_full_white.pdf
           qje_2022_replication_data_and_outputs/outputs/figures/figure9_full_male.pdf

XX: The EEO-1 employment-share rows use the explicit public-data approximation
    created by create_figure_9_full_industry_covariates.do.
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Build NAICS3-level CPS covariate files
----------------------------------------------------------------------------------------------------------- */
* Build adjusted wage gap covariates
use "${qje_2022_replication_dump}/covariates/naics3_race_wage_gap.dta", clear
rename gap_coef wage_gap_race
rename gap_se wage_gap_race_se
rename ind_coef wage_level_race
rename ind_se wage_level_race_se
replace wage_gap_race = -wage_gap_race

preserve
use "${qje_2022_replication_dump}/covariates/naics3_gender_wage_gap.dta", clear
tempfile gender_wagegap
save `gender_wagegap', replace
restore

merge 1:1 naics3 using `gender_wagegap'
assert _merge == 3
drop _merge
rename gap_coef wage_gap_gender
rename gap_se wage_gap_gender_se
rename ind_coef wage_level_gender
rename ind_se wage_level_gender_se
replace wage_gap_gender = -wage_gap_gender

tempfile wagegap_naics
save `wagegap_naics', replace

* Build college-share gap covariates
use "${qje_2022_replication_dump}/covariates/naics3_shares.dta", clear
rename college_share_gap_race col_share_gap_race
rename college_share_gap_gender col_share_gap_gender
keep naics3 emp_gap_race emp_gap_gender col_share_gap_race col_share_gap_gender
foreach var of varlist emp_gap_race emp_gap_gender col_share_gap_race col_share_gap_gender {
    replace `var' = -`var'
}

tempfile shares_naics
save `shares_naics', replace

/* -----------------------------------------------------------------------------------------------------------
Run Figure 9 regressions
----------------------------------------------------------------------------------------------------------- */
* Create a temporary Stata dataset that accumulates full Figure 9 regression coefficients
tempfile full_coefficients
local regsave_action replace

* Loop over race and gender posterior gaps
foreach touse in white male {

    * Load firm industry shell
    use "${qje_2022_replication_dump}/figure9_industry_shell.dta", clear
    keep firm_id sic_code sic_combined naics naics3
    duplicates drop
    gisid firm_id

    * Merge EEO-1, concentration, and CPS-derived industry covariates
    merge m:1 naics3 using "${qje_2022_replication_dump}/figure9_full_eeo1_industry_covariates.dta", keep(3 1)
    assert inlist(_merge, 1, 3)
    drop _merge
    merge m:1 naics using "${qje_2022_replication_dump}/figure9_full_concentration_naics.dta", keep(3 1)
    assert inlist(_merge, 1, 3)
    drop _merge
    merge m:1 naics3 using `wagegap_naics', keep(3 1)
    assert inlist(_merge, 1, 3)
    drop _merge
    merge m:1 naics3 using `shares_naics', keep(3 1)
    assert inlist(_merge, 1, 3)
    drop _merge

    * Import posterior firm-level gap estimates
    preserve
    import delimited "${qje_2022_replication_package}/dump/`touse'_posterior_features.csv", clear varn(1)
    keep firm_id post_mean_beta normal_mean_beta
    tempfile postmeans
    save `postmeans', replace
    restore

    * Merge posterior firm-level gap estimates
    merge 1:1 firm_id using `postmeans'
    assert _merge == 3
    drop _merge

    * Define the original Figure 9 row order
    local figure9_order wage_level_race wage_gap_race share_black_ind mgmt_dif_black_ind col_share_gap_race ///
        wage_level_gender wage_gap_gender share_female_ind mgmt_dif_female_ind col_share_gap_gender top4share

    * Standardize covariates within the firm-level analysis sample
    foreach var of local figure9_order {

        qui su `var'
        assert r(N) > 0
        assert r(sd) > 0
        replace `var' = (`var' - r(mean)) / r(sd) if ~mi(`var')
    }

    * Label Figure 9 covariates
    label var wage_level_race "White adj wage"
    label var wage_gap_race "White - Black adj wage"
    label var share_black_ind "% ind Black"
    label var mgmt_dif_black_ind "% mgmt - % ind Black"
    label var col_share_gap_race "White - Black col share"
    label var wage_level_gender "Male adj wage"
    label var wage_gap_gender "Male - female adj wage"
    label var share_female_ind "% ind female"
    label var mgmt_dif_female_ind "% mgmt - % ind female"
    label var col_share_gap_gender "Male - female col share"
    label var top4share "Top 4 sales share"

    * Define graph subtitle
    if "`touse'" == "white" {
        local gap = "white-Black"
    }
    else if "`touse'" == "male" {
        local gap = "male-female"
    }

    * Run posterior-gap regressions
    eststo clear
    foreach var of local figure9_order {

        qui eststo: reg `var' post_mean_beta, robust
        estimates store ef_`var'
        regsave post_mean_beta using `full_coefficients', ///
            addlabel(gap, "`touse'", characteristic, "`var'", estimate_type, "posterior_mean") ///
            double `regsave_action'
        local regsave_action append

        qui eststo: reg `var' normal_mean_beta, robust
        estimates store fe_`var'
        regsave normal_mean_beta using `full_coefficients', ///
            addlabel(gap, "`touse'", characteristic, "`var'", estimate_type, "linear_shrinkage") ///
            double append
    }

    * Explicitly list estimates so coefplot cannot reorder wildcard matches
    local posterior_estimates
    local shrinkage_estimates
    foreach var of local figure9_order {
        local posterior_estimates "`posterior_estimates' ef_`var'"
        local shrinkage_estimates "`shrinkage_estimates' fe_`var'"
    }

    * Export full Figure 9 panel
    coefplot (`posterior_estimates', label("Posterior mean")) ///
        (`shrinkage_estimates', label("Linear shrinkage")), ///
        aseq ///
        swapnames ///
        eqrename(^ef_(.*)$ = \1 ^fe_(.*)$ = \1, regex) ///
        drop(_cons) ///
        xline(0) ///
        graphregion(color(white)) ///
        legend(region(color(white))) ///
        xtitle("Regression coefficient on `gap' gap") ///
        headings(wage_level_race = "{bf:Race characteristics}" ///
            wage_level_gender = "{bf:Gender characteristics}" ///
            top4share = "{bf:Concentration}")

    graph export "${qje_2022_replication_figures}/figure9_full_`touse'.pdf", replace
}

/* -----------------------------------------------------------------------------------------------------------
Save coefficient comparison dataset
----------------------------------------------------------------------------------------------------------- */
* Load the accumulated coefficient results saved by regsave
use `full_coefficients', clear

* Rename regsave columns to the comparison-script variable names
rename coef coefficient
rename stderr se

* Keep one coefficient and standard error for each full Figure 9 rebuild estimate
keep gap characteristic estimate_type coefficient se
gisid gap characteristic estimate_type
assert _N == 44

* Save the full Figure 9 rebuild coefficients
sort gap characteristic estimate_type
compress
save "${qje_2022_replication_dump}/figure9_full_rebuild_coefficients.dta", replace
