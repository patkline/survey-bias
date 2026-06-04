/* -----------------------------------------------------------------------------------------------------------
Purpose: Build a crosswalk from ind1990 industry codes to 2-digit SIC bins from aer paper

Created: Nico Rotundo 2026-04-21
----------------------------------------------------------------------------------------------------------- */
* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Collapse sic87 to 2-digit SIC and build ind1990ddx --> sic_87_two_digit crosswalk
----------------------------------------------------------------------------------------------------------- */
* Load SIC87 --> ind1990ddx crosswalk (Autor/Dorn/Hanson 2019)
use "${external}/cw_sic87_ind1990ddx.dta", clear

* Assert uniqueness on sic87
gisid sic87

* Order unique variable first 
order sic87 

* Collapse sic87 to 2-digit SIC
gen sic_87_two_digit = floor(sic87 / 100)

* Pick a single 2-digit SIC for 4 ind1990ddx values where multiple SIC87 codes map in after the floor
* Note. Resolutions from wage_regressions.do L126-129
replace sic_87_two_digit = 15 if ind1990ddx == 60
replace sic_87_two_digit = 62 if ind1990ddx == 710
replace sic_87_two_digit = 63 if ind1990ddx == 711
replace sic_87_two_digit = 78 if ind1990ddx == 800

* Sort on ind1990ddx and sic_87_two_digit 
gsort ind1990ddx sic_87_two_digit

* Check that each ind1990ddx maps to only one sic_87_two_digit
bys ind1990ddx: assert sic_87_two_digit[1] == sic_87_two_digit[_N]

* Keep first observation in each (sic_87_two_digit, ind1990ddx) group
by ind1990ddx sic_87_two_digit: keep if _n == 1

* Drop sic87
drop sic87

* Assert uniqueness on ind1990ddx
gisid ind1990ddx

* Order unique variable first
order ind1990ddx

* Save crosswalk as tempfile
tempfile ind1990ddx_to_sic
save `ind1990ddx_to_sic'

/* -----------------------------------------------------------------------------------------------------------
Build ind1990 --> 2-digit SIC crosswalk via merge on ind1990ddx
----------------------------------------------------------------------------------------------------------- */
* Load ind1990 --> ind1990ddx crosswalk (Autor/Dorn/Hanson 2019)
use "${external}/cw_ind1990_ind1990ddx.dta", clear

* Assert uniqueness on ind1990
gisid ind1990

* Merge 2-digit SIC onto ind1990 via ind1990ddx
* _merge == 2 <--> case where sic_87_two_digit code has no ind1990 mapping via ind1990ddx; dropping since we won't be able to assign a sic_87_two_digit code to the ind1990 code in the cps data downstream
merge m:1 ind1990ddx using `ind1990ddx_to_sic', nogen keep(1 3)

* Assert ind1990 is still unique after merge
gisid ind1990

* Drop ind1990ddx
drop ind1990ddx

* Drop observations missing sic_87_two_digit since we won't be able to assign a sic_87_two_digit code to the ind1990 code in the cps data downstream
drop if mi(sic_87_two_digit)

* Rename sic_87_two_digit to match name elsewhere in our code 
rename sic_87_two_digit sic_two_digit

/* -----------------------------------------------------------------------------------------------------------
Merge on two-digit sic bin titles
----------------------------------------------------------------------------------------------------------- */
* Merge crosswalk of two-digit SIC codes to two-digit SIC bins from aer paper
* Keeping _merge == 3 since only these will be useful for mapping the cps ind1990 codes to the aer two-digit sic bins downstream
merge m:1 sic_two_digit using "${dump}/sic_two_digit_to_aer_bin_crosswalk.dta", keep(3) nogen

* Keep necessary variables 
keep ind1990 sic_two_digit_bin_aer sic_two_digit_bin_title_aer

* Assert ind1990 is the unique identifier 
gisid ind1990

* Sort by ind1990
gsort ind1990

* Compress and save output
compress
save "${dump}/ind1990_sic_bin_aer_crosswalk.dta", replace