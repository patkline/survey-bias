/* -----------------------------------------------------------------------------------------------------------
Purpose: Clean EEO-1 2023 Public Use File (PUF) and aggregate to the
naics3 industry x race x sex level with employment counts

Created: Nico Rotundo 2026-04-24
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Load EEO-1 data and subset to national NAICS3 rows
----------------------------------------------------------------------------------------------------------- */
* Import EEO-1 PUF
import excel "${external}/EEO1_2023_PUF.xlsx", firstrow case(lower) clear

* Keep national NAICS3 subtotals by keeping observations where region/division/state/CBSA/county all missing
keep if mi(region) & mi(division) & mi(state) & mi(cbsa) & mi(county)

* Drop extraneous variables 
drop region division state cbsa county

/* -----------------------------------------------------------------------------------------------------------
Confirm construction of the rows where naics3 is missing (think these are just totals 
across all industries nationally and within naics2) and drop them
----------------------------------------------------------------------------------------------------------- */
** Confirm first row with missing naics2 and naics3 is pooling across all industries nationally
* Assert first row has missing naics2 and naics3
assert mi(naics2[1]) & mi(naics3[1])

* Assert third row has non-missing naics2 and non-missing naics3
assert ~mi(naics2[3]) & ~mi(naics3[3])

* Generate total of national level employment for rows with non-missing naics3
egen double total_emp = total(total10) if ~mi(naics3)

* Store national-level total employment 
local total_emp_aggregated = total_emp[3]

* Store what I think is the national-level row's total employment 
local national_level_employment = total10[1]

* Assert value is the same 
assert `total_emp_aggregated' == `national_level_employment' 

* Drop rows with missing naics2 and naics3
drop if mi(naics2) & mi(naics3)

* Drop extraneous variables
drop total_emp 

** Confirm rows with non-missing naics is pooling across all industries nationally 
* Sort by naics2 and naics3
gsort naics2 naics3

* Assert last row within a naics2 has missing naics3
by naics2 (naics3): assert mi(naics3[_N])

* Generate total employment within each naics2 by summing across naics3
by naics2: egen double total_emp = total(total10) if ~mi(naics3)

* For each naics2, assert that the row with missing naics3 has total employment equal to the sum of total employment across naics3 rows within that naics2
by naics2: assert total_emp == total10[_N] if _n != _N

* Drop rows with missing naics3
drop if mi(naics3)

* Check uniqueness on naics3 
gisid naics3

/* -----------------------------------------------------------------------------------------------------------
Keep nescessary variables, rename to be more descriptive, convert employment counts 
to numeric, and compute employment for black + non-black workers
----------------------------------------------------------------------------------------------------------- */
* Keep only naics3 keys + 14 race-sex-total columns aggregated across jobs 
keep naics3 naics3_name ///
     mt10 whm10 blkm10 hispm10 asianm10 aianm10 nhopim10 tomrm10 ///
     ft10 whf10 blkf10 hispf10 asianf10 aianf10 nhopif10 tomrf10

* Cast the 14 race-sex total columns to numeric
foreach variable of varlist mt10 whm10 blkm10 hispm10 asianm10 aianm10 nhopim10 tomrm10 ///
                     ft10 whf10 blkf10 hispf10 asianf10 aianf10 nhopif10 tomrf10 {
    destring `variable', replace
    assert ~mi(`variable')
}

* Rename variables to be more descriptive 
rename (mt10 ft10) (total_male_emp total_female_emp)
rename (whm10 whf10) (white_male_emp white_female_emp)
rename (blkm10 blkf10) (black_male_emp black_female_emp)
rename (hispm10 hispf10) (hispanic_male_emp hispanic_female_emp)
rename (asianm10 asianf10) (asian_male_emp asian_female_emp)
rename (aianm10 aianf10) (aian_male_emp aian_female_emp)
rename (nhopim10 nhopif10) (hawaiian_pi_male_emp hawaiian_pi_female_emp)
rename (tomrm10 tomrf10) (two_plus_race_male_emp two_plus_race_female_emp)

* Compute non-black employment and drop race-specific employment variables
foreach gender in male female {
  egen double non_black_`gender'_emp = rowtotal(white_`gender'_emp hispanic_`gender'_emp asian_`gender'_emp aian_`gender'_emp hawaiian_pi_`gender'_emp two_plus_race_`gender'_emp)

  drop white_`gender'_emp hispanic_`gender'_emp asian_`gender'_emp aian_`gender'_emp hawaiian_pi_`gender'_emp two_plus_race_`gender'_emp
}

* Check this is the same as total employment minus black employment for each gender and then drop total employment variables
foreach gender in male female {
  * Assertion 
  assert non_black_`gender'_emp == total_`gender'_emp - black_`gender'_emp
  
  * Drop 
  drop total_`gender'_emp
}

/* -----------------------------------------------------------------------------------------------------------
Reshape long by race and gender 
----------------------------------------------------------------------------------------------------------- */
* Rename variables for reshape convenience
rename *_emp emp_*

* Reshape long by gender 
reshape long emp_black_ emp_non_black_, i(naics3) j(sex) string
rename *_ *

* Reshape long by race 
reshape long emp_, i(naics3 sex) j(black_indicator) string
rename *_ *

* Rename race strings to match CPS coding 
replace black_indicator = "Black" if black_indicator == "black"
replace black_indicator = "Not Black" if black_indicator == "non_black"

* Rename employment variable 
rename emp employment_eeo1

/* -----------------------------------------------------------------------------------------------------------
Aggregate naics3 to sic
----------------------------------------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------------------------------------
Export
----------------------------------------------------------------------------------------------------------- */
* Assert expected cell count: 88 naics3 x 2 race x 2 sex = 352
assert _N == 88 * 2 * 2

* Check uniqueness
gisid naics3 black_indicator sex

* Order variables
order naics3 naics3_name black_indicator sex

* Compress and save
compress
save "${dump}/industry_emp_by_demographic_eeo1.dta", replace