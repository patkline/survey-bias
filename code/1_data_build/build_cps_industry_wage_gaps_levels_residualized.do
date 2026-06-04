/* -----------------------------------------------------------------------------------------------------------
Purpose: Clean IPUMS CPS ORG microdata (2022-2026) and aggregate to the
industry (2-digit SIC) x race x sex x age_bin level with employment and hourly wage 

Created: Nico Rotundo 2026-04-21

----------------------------------------------------------------------------------------------------------- */
* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Load CPS extract
----------------------------------------------------------------------------------------------------------- */
* Define a tempfile to store the decompressed CPS extract
tempfile cps_dta

* Decompress gzipped .dta and assign to tempfile since use does not read .gz natively
shell gunzip -kc "${external}/cps_extract_2022_2026.dta.gz" > "`cps_dta'"

* Load CPS extract with necessary variables 
use cpsidp year month earnwt age sex race wkstat classwkr ind1990 educ ///
    earnweek2 uhrsworkorg eligorg qearnwee quhrsworkorg ///
    using "`cps_dta'", clear

* Format cpsidp 
format %16.0f cpsidp

* Assert uniqueness on person-month identifiers
gisid cpsidp year month

* Order identifiers first
order cpsidp year month

* Sort by identifiers
gsort cpsidp year month

* Assert expected year coverage
su year
assert `r(min)' == 2022
assert `r(max)' == 2026

/* -----------------------------------------------------------------------------------------------------------
Filter to ORG earnings universe + validate race/sex codes and generate indicators 
----------------------------------------------------------------------------------------------------------- */
* Keep respondents in the ORG earnings universe --- those in rotation months 4/8 + employed as a wage/salary worker
keep if eligorg == 1

* Keep private sector only  --- classwkr 22 = private, 23 = private nonprofit
* Note. EEO-1 covers private firms only, so doing this upstream of CPS-vs-EEO-1 makes sense 
* Also matches old Stata (wage_regressions.do L169)
keep if inlist(classwkr, 22, 23)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to private sector only: " %8.0fc `r(N_drop)'

* Check RACE codes are IPUMS 2022+ harmonized scheme
assert inlist(race, 100, 200, 300, 651, 652, 700) | inrange(race, 801, 819) | inlist(race, 820, 830)

* Indicator for black vs non-black
gen str black_indicator = cond(race == 200, "Black", "Not Black")

* Encode black indicator 
encode black_indicator, gen(black)

* Drop black indicator 
drop black_indicator

/* -----------------------------------------------------------------------------------------------------------
Merge on ind1990 --> 2-digit SIC bin crosswalk to map to our industry codes in the paper
and save tempfile for org sample
----------------------------------------------------------------------------------------------------------- */
* Merge, keeping only matched observations since we won't be able to assign a sic bin to those without an ind1990 code in the crosswalk 
* _merge ==1 <--> ind1990 codes not in crosswalk (i.e., we do not have firms in these industries);  58,344 observations (13.63% of sample)
* _merge ==2 <--> ind1990 codes in crosswalk that do not appear in this sample
merge m:1 ind1990 using "${dump}/ind1990_sic_bin_aer_crosswalk.dta", keep(3) nogen

* Encode sic bin title
encode sic_two_digit_bin_title_aer, gen(temp)

* Drop string version of the sic bin title variable 
drop sic_two_digit_bin_title_aer

* Rename encoded sic bin title
rename temp sic_two_digit_bin_title_aer

* Tempfile
tempfile cps_org_sample
save `cps_org_sample'

/* -----------------------------------------------------------------------------------------------------------
Filter to wage-sample --- those usually full time, hourly workers, age 20-60, 
non-allocated/imputed variables --- and generate age bins 
----------------------------------------------------------------------------------------------------------- */
* Import ORG sample
use `cps_org_sample', clear

* Keep "usually full-time" workers who worked 35+ hours in reference week (wkstat == 11) 
keep if inlist(wkstat, 11)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by usually full time: " %8.0fc `r(N_drop)'

* Keep hourly workers only --- uhrsworkorg == {998, 999} <--> {don't know, NIU<--> salaried}
assert ~mi(uhrsworkorg) & ~inlist(uhrsworkorg, 998)
keep if ~inlist(uhrsworkorg, 999)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to hourly workers: " %8.0fc `r(N_drop)'

* Keep respondents aged 20-60
keep if inrange(age, 20, 60)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to ages 20-60: " %8.0fc `r(N_drop)'

* Drop allocated/imputed earnings and hours observations 
keep if inlist(qearnwee, 0) & inlist(quhrsworkorg, 0)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to non-allocated earnings and hours: " %8.0fc `r(N_drop)'

* Drop extraneous variables
drop eligorg quhrsworkorg wkstat classwkr

/* -----------------------------------------------------------------------------------------------------------
Generate education level variable 
----------------------------------------------------------------------------------------------------------- */
* Assert education level is not NIU
assert ~inlist(educ, 0, 1) & ~mi(educ)

** Generate education level variable using codes in cps 
* Less than high school diploma
gen education_level = 1 if inrange(educ, 2, 72)

* High school diploma
replace education_level = 2 if inlist(educ, 73)

* Some college, no degree
replace education_level = 3 if inrange(educ, 80, 90)

* Associate's degree
replace education_level = 4 if inrange(educ, 91, 109)

* Bachelor's degree 
replace education_level = 5 if inrange(educ, 110, 122)

* Post-graduate degree
replace education_level = 6 if inrange(educ, 123, 125)

* Should not be missing education level 
assert ~mi(education_level)

* Define labels 
label define education_level_label ///
    1 "Less than high school diploma" ///
    2 "High school diploma" ///
    3 "Some college, no degree" ///
    4 "Associate's degree" ///
    5 "Bachelor's degree" ///
    6 "Post-graduate degree"

* Label values of education level 
label values education_level education_level_label

* Crosstab to check 
tab educ education_level

* Drop original education variable
drop educ

/* -----------------------------------------------------------------------------------------------------------
Compute hourly wage and save tempfile for wage sample
----------------------------------------------------------------------------------------------------------- */
* Drop rows with anomalous / NIU earnweek2 (0 = data anomaly; 999999.99 = NIU)
* Note. Old Stata (L161) recoded earnweek=9999.99 to missing, while we drop directly + use earnweek2 since it's a new harmonized rounding scheme the CPS implemented 
drop if inlist(earnweek2, 0, 999999.99)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to valid weekly earnings: " %8.0fc `r(N_drop)'

* Assert usual hours worked is not {zero, don't know or not in universe} 
* Note. Old Stata (L162) used uhrsworkorg=. if inlist(998, 999)
assert ~inlist(uhrsworkorg, 0, 998, 999)

* Compute hourly wage for hourly workers 
* Note. Same formula as old Stata (L176) except earnweek is now earnweek2
gen double ln_hourly_wage = ln(earnweek2 / uhrsworkorg)
assert ~mi(ln_hourly_wage)

* Drop wage and hour components
drop earnweek2 uhrsworkorg

* Tempfile 
tempfile cps_wage_sample
save `cps_wage_sample'

/* -----------------------------------------------------------------------------------------------------------
Generate residualized industry-specific {black vs non-black, female vs male} wage gaps and levels by
partialing out year, education, and demographic composition
----------------------------------------------------------------------------------------------------------- */
foreach demographic_cut in black sex {
    * Show cut 
    di "`demographic_cut'"

    * Set locals per demographic cut 
    if inlist("`demographic_cut'", "black") {
        * Local base category
        local base_category 2

        * Local for other category
        local not_base_category 1

        * Local for other demographic cut 
        local other_demographic_cut "sex"
    }

    * Set locals per demographic cut 
    if inlist("`demographic_cut'", "sex") {
        * Local base category
        local base_category 1

        * Local for other category
        local not_base_category 2

        * Local for other demographic cut 
        local other_demographic_cut "black"
    }

    * Import wage sample
    use `cps_wage_sample', clear

    * Run wage-adjustment regression for given gap
    reg ln_hourly_wage ib`base_category'.`demographic_cut'##i.sic_two_digit_bin_title_aer i.year c.age##c.age##c.age##c.age i.`other_demographic_cut' i.education_level [pw=earnwt], robust

    * Check all observations are in estimation sample 
    assert e(sample) == 1

    * Set tempfile to store regression results for `demographic_cut' wage gap, with variables for 
    tempname `demographic_cut'_gap_dataset
    tempfile `demographic_cut'_gap_residualized
    postfile ``demographic_cut'_gap_dataset' ///
        str40(sic_two_digit_bin_aer sic_two_digit_bin_title_aer) ///
        double(wage_gap_`demographic_cut'_coef wage_gap_`demographic_cut'_se) ///
        double(wage_level_`demographic_cut'_coef) double(wage_level_`demographic_cut'_se) ///
        using ``demographic_cut'_gap_residualized'

    * Store each industry bin as a local
    levelsof sic_two_digit_bin_title_aer, local(sic_bins)

    * Loop over industry bins, storing residualized {wage gap, wage level} for each industry 
    foreach sic_bin of local sic_bins {
        * Store local with bin title 
        local sic_bin_title: label (sic_two_digit_bin_title_aer) `sic_bin'

        * Store value range for this industry bin
        qui levelsof sic_two_digit_bin_aer if sic_two_digit_bin_title_aer == `sic_bin', local(sic_bin_range) clean

        * Display given industry bin being processed
        display "🎃 Industry bin: `sic_bin_range', `sic_bin_title'"

        * Store the `demographic_cut' - Not `demographic_cut' adjusted log wage gap in this bin
        lincom _b[`not_base_category'.`demographic_cut'] + _b[`not_base_category'.`demographic_cut'#`sic_bin'.sic_two_digit_bin_title_aer]

        * Store coefficient and estimate 
        local gap_coef = r(estimate)
        local gap_se = r(se)

        * Store Not-`demographic_cut' adjusted log wage level in this bin 
        lincom _b[_cons] + _b[`sic_bin'.sic_two_digit_bin_title_aer]

        post ``demographic_cut'_gap_dataset' /// 
        ("`sic_bin_range'") ("`sic_bin_title'") /// 
        (`gap_coef') (`gap_se') (r(estimate)) (r(se))
    }

* Close post file 
postclose ``demographic_cut'_gap_dataset'

}

/* -----------------------------------------------------------------------------------------------------------
Export
----------------------------------------------------------------------------------------------------------- */ 
* Import race dataset with residualized wage gaps and levels
use `black_gap_residualized', clear

* Merge on sex dataset with residualized wage gaps and levels
merge 1:1 sic_two_digit_bin_aer using `sex_gap_residualized', assert(3) nogen

* Export as csv to use in the EIV regressions 
export delimited using "${dump}/cps_industry_wage_gaps_levels_residualized.csv", replace