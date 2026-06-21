/* ------------------------------------------------------------------------------
Purpose: Scatter industry-level race and gender employment 
shares computed from EEO-1 vs Revelio

Created: Nico Rotundo 2026-06-21
------------------------------------------------------------------------------*/
* Run globals
do "${github}/survey-bias/code/globals.do"

/* ------------------------------------------------------------------------------
Import survey microdata and save a firm --> industry-group
crosswalk from the survey microdata
------------------------------------------------------------------------------*/
* Import the long survey microdata
import delimited "${processed}/long_survey_final.csv", asdouble clear

* Check uniqueness on response id x firm id
gisid responseid firm_id

* Keep the firm id + its industry code and industry-group name
keep firm_id aer_naics2 aer_naics2_name

* Check industry code and industry-group name is constant within firm
bysort firm_id (aer_naics2): assert aer_naics2[1] == aer_naics2[_N]
bysort firm_id (aer_naics2_name): assert aer_naics2_name[1] == aer_naics2_name[_N]

* Collapse to one row per firm
bysort firm_id: keep if _n == 1

* Should not be missing industry name 
assert ~mi(aer_naics2_name)

* Save the firm crosswalk
tempfile firm_industry_crosswalk
save `firm_industry_crosswalk'

/* ------------------------------------------------------------------------------
Compute industry-level demographic-group employment shares
from the firm-level Revelio data 
------------------------------------------------------------------------------*/
* Import the Revelio firm-level measures
import delimited "${processed}/revelio_firm_measures.csv", asdouble clear

* Check uniqueness on firm id 
gisid firm_id 

* Keep the firm id, industry bin, the two demographic shares, and the number of users for each firm
keep firm_id aer_naics2 black_share female_share weighted_n_ethnicity weighted_n_gender weighted_n_users

* Check that number of users, the number of users whose race is identified, and the number of users whose gender is identified for are all the same
assert weighted_n_users == weighted_n_ethnicity
assert weighted_n_users == weighted_n_gender

* Drop identified ethnicity and gender counts 
drop weighted_n_ethnicity weighted_n_gender 

* Check number of users is > 0 
assert weighted_n_users > 0 

* Keep firms with non-missing shares and a positive total-employment weight
keep if ~mi(black_share) & ~mi(female_share) & ~mi(weighted_n_users) 

* Merge the industry-group name onto each firm
merge m:1 firm_id using `firm_industry_crosswalk', update assert(2 3) keep(3) nogen

* Aggregate firm shares to industry-group shares, weighted by total employment
collapse (mean) revelio_black_share = black_share revelio_female_share = female_share [aw = weighted_n_users], by(aer_naics2_name)

* One row per industry group 
gisid aer_naics2_name

* Shares between 0 and 1 
assert inrange(revelio_black_share, 0, 1) & inrange(revelio_female_share, 0, 1)

* Save the Revelio industry-group shares
tempfile revelio_industry_shares
save `revelio_industry_shares'

/* ------------------------------------------------------------------------------
Merge Revelio data onto eeoc data
------------------------------------------------------------------------------*/
* Import the EEO-1 industry-group employment shares
import delimited "${dump}/industry_emp_share_by_demographic_eeo1.csv", asdouble clear

* Rename merge key to match the Revelio data 
rename sic_two_digit_bin_title_aer aer_naics2_name

* Merge the Revelio industry-group shares onto the EEO-1 industry groups
merge 1:1 aer_naics2_name using `revelio_industry_shares', assert(3) nogen

/* ------------------------------------------------------------------------------
Scatter EEO-1 vs Revelio industry-group shares, one figure per 
demographic group
------------------------------------------------------------------------------*/
* Loop over the two demographic groups
foreach group in black female {

    * Store local for group label
    local group_label = proper("`group'")

    * Loop over the different eeoc share measures 
    foreach job_type in all_jobs mid_off_manager front_line {
        
        * Store local for job type label
        local job_type_label = subinstr(proper("`job_type'"), "_", " ", .)
        
        * Correlate eeoc and revelio measures 
        corr share_emp_`group'_`job_type' revelio_`group'_share 

        * Store correlation
        local correlation_coef = r(rho)
        
        di `correlation_coef'

        * Generate level difference between two measures 
        gen abs_diff_levels = abs(share_emp_`group'_`job_type' - revelio_`group'_share)

        * Check distribution 
        quietly su abs_diff_levels, d

        * Store median and 95th percentile level difference 
        local median_abs_diff_level = r(p50)
        local p95_abs_diff_level = r(p95)

        * Regress eeoc and revelio measures 
        quietly reg share_emp_`group'_`job_type' revelio_`group'_share, robust
        di _b[revelio_`group'_share]

        * Format the correlation and level differences for the legend annotations
        local correlation_text : display %4.2f `correlation_coef'
        local median_text      : display %5.3f `median_abs_diff_level'
        local p95_text         : display %5.3f `p95_abs_diff_level'

        * Scatter Revelio against EEO-1 with a 45-degree line and industry labels
        twoway ///
            (scatter revelio_`group'_share share_emp_`group'_`job_type', msize(small) mlabel(aer_naics2_name) mlabsize(tiny)) ///
            (function y = x, range(share_emp_`group'_`job_type') lcolor(gs9) lpattern(dash)) ///
            (scatter revelio_`group'_share share_emp_`group'_`job_type' if 0, msymbol(none)) ///
            (scatter revelio_`group'_share share_emp_`group'_`job_type' if 0, msymbol(none)) ///
            , ///
            xtitle("EEO-1 `group_label' Employment Share, `job_type_label'") ///
            ytitle("Revelio `group_label' Employment Share") ///
            legend(order(2 "45{char 176} line" ///
                         3 "Correlation = `correlation_text'" ///
                         4 "|{&Delta} in Levels|: Median = `median_text', 95th percentile = `p95_text'") ///
                symx(7) size(*0.5) colgap(8) col(1) pos(11) ring(0) region(fcolor(none))) ///
            graphregion(color(white)) plotregion(color(white))

        * Export the figure to the scratch figures folder
        graph export "${code}/scratch_nico/figures/scatter_eeo1_vs_revelio_industry_shares_eeo1_share_emp_`group'_`job_type'_revelio_`group'_share.png", replace

        * Drop level difference
        drop abs_diff_levels
    }
}