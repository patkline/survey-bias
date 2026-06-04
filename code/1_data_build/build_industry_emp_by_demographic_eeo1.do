/* -----------------------------------------------------------------------------------------------------------
Purpose: Clean EEO-1 2023 Public Use File (PUF) employment counts by race, gender, and 
job, and crosswalk from 2022 naics to the aggregated sic buckets in this paper's data 

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
Keep nescessary variables, rename to be more descriptive, and convert employment counts 
to numeric
----------------------------------------------------------------------------------------------------------- */
* Keep only naics3 keys + (1) total, (2) black, (3) female employment counts for (i) across all jobs, (ii) senior off and managers, (iii) mid off and managers, and (iv) potential ``front line'' employees
keep naics3 naics3_name ///
  total10 blkt10 ft10 ///
  total1 blkt1 ft1   ///
  total1_2 blkt1_2 ft1_2 ///
  total4 blkt4 ft4 ///
  total5 blkt5 ft5 ///
  total6 blkt6 ft6 ///
  total8 blkt8 ft8 ///
  total9 blkt9 ft9

* Cast the 14 race-sex total columns to numeric 
ds naics3 naics3_name total10, not 
foreach variable in `r(varlist)' {
  * Destring --- only non-numeric strings are *, so fine to force (these are small cell supression from eeo1) 
  destring `variable', replace force 
}

* Rename variables to be more descriptive 
rename (total10 blkt10 ft10) (emp_pooled_all_jobs emp_black_all_jobs emp_female_all_jobs)
rename (total1 blkt1 ft1) (emp_pooled_senior_off_manager emp_black_senior_off_manager emp_female_senior_off_manager) 
rename (total1_2 blkt1_2 ft1_2) (emp_pooled_mid_off_manager emp_black_mid_off_manager emp_female_mid_off_manager)
rename (total4 blkt4 ft4) (emp_pooled_sales emp_black_sales emp_female_sales)
rename (total5 blkt5 ft5) (emp_pooled_clerk emp_black_clerk emp_female_clerk)
rename (total6 blkt6 ft6) (emp_pooled_craft emp_black_craft emp_female_craft)
rename (total8 blkt8 ft8) (emp_pooled_laborer emp_black_laborer emp_female_laborer)
rename (total9 blkt9 ft9) (emp_pooled_service emp_black_service emp_female_service)

* Rename naics variables to be consistent with conventions from elsewhere in our codebase
rename (naics3 naics3_name) (naics_2022_three_digit naics_2022_three_digit_title)

/* -----------------------------------------------------------------------------------------------------------
Merge on 2022 NAICS 3-digit to SIC bin crosswalk and collapse to aer sic bins 
----------------------------------------------------------------------------------------------------------- */
* Merge aer sic bins on
* Only keeping matches since other 2022 naics 3 digit codes (3 in total) are useless, and so are the aer sic bins that do not match to a 2022 naics 3 digit code in the ee01 (2 total) 
merge 1:1 naics_2022_three_digit using "${dump}/naics_2022_three_digit_sic_bin_aer_crosswalk.dta", keep(3) nogen

* Collapse to aer two-digit sic bins by summing employment counts across naics3 codes within each sic bin
ds naics_2022_three_digit naics_2022_three_digit_title sic_two_digit_bin_aer sic_two_digit_bin_title_aer, not v(32)
collapse (sum) `r(varlist)', by(sic_two_digit_bin_aer sic_two_digit_bin_title_aer)

/* -----------------------------------------------------------------------------------------------------------
Generate employment shares by demographic group x job type within each aer two-digit 
sic bin
----------------------------------------------------------------------------------------------------------- */
* Loop over demographic groups 
foreach demographic_group in black female {
  foreach job_type in all_jobs mid_off_manager {
    * Generate variable for share of {black, female} employment among {all jobs, mid_off_manager} within each two-digit sic bin
    gen share_emp_`demographic_group'_`job_type' = emp_`demographic_group'_`job_type' / emp_pooled_`job_type'
  }

  * Try version focused on front-line workers (sales, clerks, craft, laborer, service)
  gen share_emp_`demographic_group'_front_line = ///
  (emp_`demographic_group'_sales + emp_`demographic_group'_clerk + emp_`demographic_group'_craft + emp_`demographic_group'_laborer + emp_`demographic_group'_service) / (emp_pooled_sales + emp_pooled_clerk + emp_pooled_craft + emp_pooled_laborer + emp_pooled_service)
}

* Keep only employment share variables and sic bins 
keep sic_two_digit_bin_aer sic_two_digit_bin_title_aer share_emp*

/* -----------------------------------------------------------------------------------------------------------
Export
----------------------------------------------------------------------------------------------------------- */
* Assert expected cell count: 1 row per aer two-digit sic bin (19 total)
assert _N == 19

* Check uniqueness
gisid sic_two_digit_bin_aer

* Compress and save
compress
save "${dump}/industry_emp_by_demographic_eeo1.dta", replace