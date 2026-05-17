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
use cpsidp year month earnwt age sex race wkstat classwkr ind1990 ///
    earnweek2 uhrsworkorg uhrswork1 eligorg qearnwee quhrsworkorg quhrswork1 ///
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

* Store count of total org sample
count
local total_org_sample = r(N)

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

* Tempfile
tempfile cps_org_sample
save `cps_org_sample'

/* -----------------------------------------------------------------------------------------------------------
Filter to wage-sample --- those usually full time, hourly workers, age 20-60, 
non-allocated/imputed variables --- and generate age bins 
----------------------------------------------------------------------------------------------------------- */
* Import ORG sample
use `cps_org_sample', clear

* Keep "usually full-time" workers 
* Note. Old Stata (wage_regressions.do L166) used wkstat == 11 only --- broader codes still classify as "usually FT", so keeping those here 
keep if inlist(wkstat, 11, 12, 13, 21)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by usually full time: " %8.0fc `r(N_drop)'

* Keep hourly workers only --- uhrsworkorg == 999 <--> NIU <--> salaried)
* Note. Functionally same as old Stata (L162) which recoded uhrsworkorg=999 to missing, such that wage denominator for salaried was missing
assert ~mi(uhrsworkorg)
keep if uhrsworkorg != 999

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to hourly workers: " %8.0fc `r(N_drop)'

* Keep respondents aged 20-60
keep if inrange(age, 20, 60)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to ages 20-60: " %8.0fc `r(N_drop)'

* Drop allocated/imputed earnings and hours observations 
* Note. Old Stata (L167-168) used `qearnwee != 0 & quhrsworkorg != 0` i.e., retained only allocated rows
keep if inlist(qearnwee, 0) & inlist(quhrsworkorg, 0)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to non-allocated earnings and hours: " %8.0fc `r(N_drop)'

* Drop filter columns no longer needed
drop eligorg quhrsworkorg quhrswork1 wkstat classwkr uhrswork1

/* -----------------------------------------------------------------------------------------------------------
Compute hourly wage
----------------------------------------------------------------------------------------------------------- */
* Check weekly earnings are non-allocated/imputed 
assert qearnwee == 0

* Drop rows with anomalous / NIU earnweek2 (0 = data anomaly; 999999.99 = NIU)
* Note. Old Stata (L161) recoded earnweek=9999.99 to missing, while we drop directly + use earnweek2 since it's a new harmonized rounding scheme the CPS implemented 
drop if inlist(earnweek2, 0, 999999.99)

* Display number of observations dropped by this filter
display "🎃 Number of observations dropped by restricting to valid weekly earnings: " %8.0fc `r(N_drop)'

* Assert usual hours worked is not zero, don't know or not in universe 
* Note. Old Stata (L162) used uhrsworkorg=. if inlist(998, 999)
assert ~inlist(uhrsworkorg, 0, 998, 999)

* Compute hourly wage for hourly workers 
* Note. Same formula as old Stata (L176) except earnweek is now earnweek2
gen double hourly_wage = earnweek2 / uhrsworkorg
assert ~mi(hourly_wage)

* Drop raw wage components
drop earnweek2 uhrsworkorg

/* -----------------------------------------------------------------------------------------------------------
Generate age bins
----------------------------------------------------------------------------------------------------------- */
* Generate age bins 
gen str age_bin = ""
replace age_bin = "20_29" if inrange(age, 20, 29)
replace age_bin = "30_39" if inrange(age, 30, 39)
replace age_bin = "40_49" if inrange(age, 40, 49)
replace age_bin = "50_60" if inrange(age, 50, 60)

* Assert no missing age bins 
assert ~mi(age_bin)

* Drop raw age column
drop age

/* -----------------------------------------------------------------------------------------------------------
Merge ind1990 → 2-digit SIC crosswalk
----------------------------------------------------------------------------------------------------------- */
* Merge ind1990 → sic_87_two_digit; keep master + matched 
* _merge == 2 are ind1990 codes in the crosswalk that do not appear in the sample 
merge m:1 ind1990 using "${dump}/ind1990_crosswalks.dta", keep(1 3) keepusing(sic_87_two_digit) nogen

* Drop rows with missing sic_87_two_digit
drop if mi(sic_87_two_digit)

* Display number of observations dropped
display "🎃 Number of observations dropped by missing sic_87_two_digit: " %8.0fc `r(N_drop)'

/* -----------------------------------------------------------------------------------------------------------
Aggregate to industry × race × sex × age_bin and merge broader cps employment pooling 
over age 
----------------------------------------------------------------------------------------------------------- */
* Collapse to (sic_87_two_digit, black_indicator, sex, age_bin): weighted mean wage + weighted count + unweighted obs count
gcollapse (mean)   avg_hourly_wage_cps = hourly_wage   ///
          (rawsum) count_avg_hourly_wage_cps = earnwt        ///
          (count)  n_obs_cps = earnwt        ///
          [pw=earnwt], ///
    by(sic_87_two_digit black_indicator sex age_bin)

* Order columns: keys first, then wage-sample metrics, then broader (EEO-1-comparable) metrics
order sic_87_two_digit black_indicator sex age_bin ///
      avg_hourly_wage_cps count_avg_hourly_wage_cps n_obs_cps

/* -----------------------------------------------------------------------------------------------------------
Export
----------------------------------------------------------------------------------------------------------- */
* Sort by keys
gsort sic_87_two_digit black_indicator sex age_bin

* Assert final uniqueness
gisid sic_87_two_digit black_indicator sex age_bin

* Report final cell count
count
display "🎃 Final industry × race × sex × age_bin cells: " %8.0fc `r(N)'

* Compress and save
compress
save "${dump}/industry_emp_wage_by_demographic_cps.dta", replace


/* -----------------------------------------------------------------------------------------------------------
Build broader CPS employment aggregate for EEO-1 comparison
----------------------------------------------------------------------------------------------------------- */
* Import org sample
use `cps_org_sample', clear

* Merge ind1990 --> sic_87_two_digit
* Drops _merge == 2 i.e., ind1990 values from crosswalk not in this subsample
merge m:1 ind1990 using "${dump}/ind1990_crosswalks.dta", keep(1 3) keepusing(sic_87_two_digit) nogen

* Drop those with missing sic_87_two_digit since they cannot be merged onto EEO-1 data --- .6% of the sample 
drop if mi(sic_87_two_digit)

* Save broader sample for reuse across the two year-window collapses
tempfile broader_sample
save `broader_sample'

* Collapse to industry x race x gender level, pooling all 5 years
gcollapse (rawsum) emp_cps_eeo1_comparable = earnwt (count) n_obs_cps_eeo1_comparable = earnwt, ///
    by(sic_87_two_digit black_indicator sex)
tempfile broader_aggregate_pool
save `broader_aggregate_pool'

* Collapse to industry x race x gender level, keeping just 2023
use if inlist(year, 2023) using `broader_sample', clear
gcollapse (rawsum) emp_cps_eeo1_comparable_2023 = earnwt (count) n_obs_cps_eeo1_comparable_2023 = earnwt, ///
    by(sic_87_two_digit black_indicator sex)

* Merge 2023-only onto 5-year pool 
merge 1:1 sic_87_two_digit black_indicator sex using `broader_aggregate_pool', assert(2 3) nogen

* Save broader aggregate for merge onto wage-sample aggregate downstream
tempfile broader_cps_emp_eeo1_comp
save `broader_cps_emp_eeo1_comp'