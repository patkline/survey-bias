/* ------------------------------------------------------------------------------
Purpose: Comparing Github output files against Dropbox files to 
make sure things are the same 

Created: Nico Rotundo 2026-01-23
------------------------------------------------------------------------------*/

/* ------------------------------------------------------------------------------
Long_survey
------------------------------------------------------------------------------*/
* Import Dropbox file
import delimited "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code/processed/long_survey.csv", clear

* Rename variables 
rename * *_old 

* Generate id variable for merge
gen id = _n

* Save as tempfile
tempfile dropbox_file
save `dropbox_file'

* Import Github file 
import delimited "/Users/nicorotundo/Documents/GitHub/survey-bias/data/processed/long_survey.csv", clear 

* Generate id variable for merge
gen id = _n

* Merge dropbox file
merge 1:1 id using `dropbox_file', update assert(3)

* Store variable names 
ds id *_old _merge, not 

* Check for differences
foreach var in `r(varlist)' {
di "`var'"
assert `var' == `var'_old
}

/* ------------------------------------------------------------------------------
Long_survey_final
------------------------------------------------------------------------------*/
* Import Dropbox file
import delimited "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code/processed/long_survey_final.csv", clear

* Rename variables 
rename * *_old 

* Generate id variable for merge
gen id = _n

* Save as tempfile
tempfile dropbox_file
save `dropbox_file'

* Import Github file 
import delimited "/Users/nicorotundo/Documents/GitHub/survey-bias/data/processed/long_survey_final.csv", clear 

* Generate id variable for merge
gen id = _n

* Merge dropbox file
merge 1:1 id using `dropbox_file', update assert(3)

* Store variable names 
ds id *_old _merge, not 

* Check for differences
foreach var in `r(varlist)' {
di "`var'"
assert `var' == `var'_old
}

/* ------------------------------------------------------------------------------
Industry map 
------------------------------------------------------------------------------*/
* Import Dropbox file
import excel "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code/processed/industry_map.xlsx", clear

* Rename variables 
rename * *_old 

* Generate id variable for merge
gen id = _n

* Save as tempfile
tempfile dropbox_file
save `dropbox_file'

* Import Github file 
import excel "/Users/nicorotundo/Documents/GitHub/survey-bias/data/processed/industry_map.xlsx", clear 

* Generate id variable for merge
gen id = _n

* Merge dropbox file
merge 1:1 id using `dropbox_file', update assert(3)

* Store variable names 
ds id *_old _merge, not 

* Check for differences
foreach var in `r(varlist)' {
di "`var'"
assert `var' == `var'_old
}

/* ------------------------------------------------------------------------------
Plackett_Luca_Full_Sample.xlsx
------------------------------------------------------------------------------*/
* Import Dropbox file
import excel "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code/excel/Plackett_Luce_Full_Sample.xlsx", clear

* Rename variables 
rename * *_old 

* Generate id variable for merge
gen id = _n

* Save as tempfile
tempfile dropbox_file
save `dropbox_file'

* Import Github file 
import excel "/Users/nicorotundo/Documents/GitHub/survey-bias/output/excel/Plackett_Luce_Full_Sample.xlsx", clear 

* Generate id variable for merge
gen id = _n

* Merge dropbox file
merge 1:1 id using `dropbox_file', update assert(3)

set excelxlsxlargefile off