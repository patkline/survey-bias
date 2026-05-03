/* -----------------------------------------------------------------------------------------------------------
Purpose: Recreate the CPS-derived pieces of QJE Figure 9 using NAICS3-level
wage and college-share covariates from wage_regressions.do

Created: Nico Rotundo 2026-04-24
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

merge 1:1 naics3 using `gender_wagegap', nogen
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
* Open coefficient CSV for numeric comparison against paper Figure 9
file open coef_file using "${qje_2022_replication_dump}/figure9_cps_rebuild_coefficients.csv", write replace
file write coef_file "gap,characteristic,estimate_type,coefficient,se" _n

* Loop over race and gender posterior gaps
foreach touse in white male {

    * Load firm industry shell
    use "${qje_2022_replication_dump}/figure9_industry_shell.dta", clear
    keep firm_id sic_code sic_combined naics naics3
    duplicates drop
    gisid firm_id

    * Merge CPS-derived industry covariates
    merge m:1 naics3 using `wagegap_naics', keep(3 1) nogen
    merge m:1 naics3 using `shares_naics', keep(3 1) nogen

    * Import posterior firm-level gap estimates
    preserve
    import delimited "${qje_2022_replication_package}/dump/`touse'_posterior_features.csv", clear varn(1)
    keep firm_id post_mean_beta normal_mean_beta
    tempfile postmeans
    save `postmeans', replace
    restore

    * Merge posterior firm-level gap estimates
    merge 1:1 firm_id using `postmeans', keep(3) nogen

    * Standardize covariates within the firm-level analysis sample
    foreach var of varlist wage_level_race wage_gap_race col_share_gap_race ///
        wage_level_gender wage_gap_gender col_share_gap_gender {

        qui su `var', d
        replace `var' = (`var' - r(mean)) / r(sd)
    }

    * Label CPS-derived covariates
    label var wage_level_race "White adj wage"
    label var wage_gap_race "White - Black adj wage"
    label var col_share_gap_race "White - Black col share"
    label var wage_level_gender "Male adj wage"
    label var wage_gap_gender "Male - female adj wage"
    label var col_share_gap_gender "Male - female col share"

    * Define graph subtitle
    if "`touse'" == "white" {
        local gap = "white-Black"
    }
    else if "`touse'" == "male" {
        local gap = "male-female"
    }

    * Run posterior-gap regressions
    eststo clear
    foreach var of varlist wage_level_race wage_gap_race col_share_gap_race ///
        wage_level_gender wage_gap_gender col_share_gap_gender {

        qui eststo: reg `var' post_mean_beta, robust
        estimates store ef_`var'
        file write coef_file "`touse',`var',posterior_mean," ///
            %24.17g (_b[post_mean_beta]) "," %24.17g (_se[post_mean_beta]) _n

        qui eststo: reg `var' normal_mean_beta, robust
        estimates store fe_`var'
        file write coef_file "`touse',`var',linear_shrinkage," ///
            %24.17g (_b[normal_mean_beta]) "," %24.17g (_se[normal_mean_beta]) _n
    }

    * Export CPS-only Figure 9 panel
    coefplot (ef_*, label("Posterior mean")) ///
        (fe_*, label("Linear shrinkage")), ///
        aseq ///
        swapnames ///
        eqrename(^ef_(.*)$ = \1 ^fe_(.*)$ = \1, regex) ///
        drop(_cons) ///
        xline(0) ///
        graphregion(color(white)) ///
        legend(region(color(white))) ///
        xtitle("Regression coefficient on `gap' gap") ///
        headings(wage_level_race = "{bf:Race characteristics}" ///
            wage_level_gender = "{bf:Gender characteristics}")

    graph export "${qje_2022_replication_figures}/figure9_cps_`touse'.pdf", replace
}

* Close coefficient CSV
file close coef_file
