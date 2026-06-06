/* -----------------------------------------------------------------------------------------------------------
Purpose: Build crosswalk from 2022 NAICS 3-digit codes to two-digit SIC bins from aer 
paper, making adjustments to expand the bins based on the additional representation of 
sic codes from the expanded sample of firms in this paper

Created: Nico Rotundo 2026-05-30
----------------------------------------------------------------------------------------------------------- */
* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Construct a crosswalk from two-digit sic codes to their associated bins and bin titles from
the aer paper, making adjustments to expand the bins based on the additional representation 
of sic codes from the expanded sample of firms in this paper 
----------------------------------------------------------------------------------------------------------- */
* Import two-digit sic code names 
import excel "${external}/Two-and-Four-Digit-SIC-with-Descriptions.xlsx", firstrow case(lower) sheet("SIC 2 DIGIT") clear

* Rename variables 
rename (twodigitsiccodes twodigitsiccodedescriptions) (sic_two_digit sic_two_digit_title)

* Destring two-digit sic codes 
destring sic_two_digit, replace

* Tempfile 
tempfile sic_two_digit_titles
save `sic_two_digit_titles'

* Import sic bin titles from aer paper
import delimited "${external}/formatted_sic_names.csv", clear

* Rename variables
rename (sic_combined sic_name) (lowest_sic_two_digit sic_two_digit_bin_title)

* Check uniqueness on lowest_sic_two_digit
gisid lowest_sic_two_digit

* Tempfile 
tempfile sic_bin_titles
save `sic_bin_titles'

* Import sic bins from aer paper 
import delimited "${external}/sic_updates.csv", clear

* Rename variables 
rename (new_sic new_sic_numeric) (sic_two_digit_bin lowest_sic_two_digit)

* Drop extraneous variables
drop firm_id 

* Keep one-row-per-bin 
gsort sic_two_digit_bin
by sic_two_digit_bin: keep if _n == 1

* Merge on bin titles
merge 1:1 lowest_sic_two_digit using `sic_bin_titles', assert(3) nogen

* Drop merge key since we are reclassifying bins 
drop lowest_sic_two_digit

** Manual bin expansions from this paper since we have new firms with new sic codes that fall outside of the original aer paper bins 
* Expanding manufacturing bin to include 36 (electronic, electrical equipment, and components, except computer equipment), 37 (transportation equipment), 38 (measurement/analysis/control instruments; photo/med/opt/gds; watches/clocks), and 39 (miscellaneous manufacturing industries)
replace sic_two_digit_bin = "24-39" if inlist(sic_two_digit_bin, "24-35") 

* Expanding freight/transport bin to include 40 (railroad transportation) and 41 (local, suburban, and interurban highway passenger transportation)
replace sic_two_digit_bin = "40-47" if inlist(sic_two_digit_bin, "42-47")

* Expanding bank/securities bin to include 60 (depository institutions)
replace sic_two_digit_bin = "60-64" if inlist(sic_two_digit_bin, "61-64")

* Generate lowest 2-digit sic code in each 2-digit bin from string bin variable 
g lowest_sic_two_digit = substr(sic_two_digit_bin, 1, 2)

* Generate highest 2-digit sic code in each 2-digit bin
g highest_sic_two_digit = substr(sic_two_digit_bin, -2, 2)

* Destring lowest and highest 2-digit sic codes
destring lowest_sic_two_digit highest_sic_two_digit, replace

* Expand 
expand highest_sic_two_digit - lowest_sic_two_digit + 1

* Generate 2-digit sic code for each row by adding the expansion index to the lowest 2-digit code
gsort sic_two_digit_bin
by sic_two_digit_bin: gen sic_two_digit = lowest_sic_two_digit + _n - 1

* Keep necessary variables 
keep sic_two_digit sic_two_digit_bin sic_two_digit_bin_title

* Rename
rename (sic_two_digit_bin sic_two_digit_bin_title) (sic_two_digit_bin_aer sic_two_digit_bin_title_aer)

*Merge on sic two-digit titles
merge 1:1 sic_two_digit using `sic_two_digit_titles', keep(1 3) nogen

* Order
order sic_two_digit_title sic_two_digit sic_two_digit_bin_aer sic_two_digit_bin_title_aer

* Should be one-row-per-2017-two-digit-sic-code
gisid sic_two_digit

* Export 
compress
save "${dump}/sic_two_digit_to_aer_bin_crosswalk.dta", replace

/* -----------------------------------------------------------------------------------------------------------
Construct a two datasets: (i) with 2017 NAICS 3-digit codes and their descriptions and 
(ii) with 2017 NAICS 2-digit codes and their descriptions
----------------------------------------------------------------------------------------------------------- */
* Import 2017 NAICS three-digit codes and descriptions
import excel "${external}/2017-NAICS-2-3-4-5-6-Digit-Codes-Listed-Numerically.xlsx", firstrow case(lower) sheet("3 Digit NAICS") clear

* Rename variables 
rename (threedigitnaicscodes naicstitleusa) (naics_2017_three_digit naics_2017_three_digit_title)

* Keep necessary variables 
keep naics_2017_three_digit naics_2017_three_digit_title

* Should be one-row-per-2017-three-digit-naics-code
gisid naics_2017_three_digit

* Tempfile 
tempfile naics_2017_three_digit_title
save `naics_2017_three_digit_title'

* Import 2017 NAICS two-digit codes and descriptions
import excel "${external}/2017-NAICS-2-3-4-5-6-Digit-Codes-Listed-Numerically.xlsx", firstrow case(lower) sheet("2 Digit NAICS") clear

* Rename variables 
rename (twodigitnaicscodes naicstitleusa) (naics_2017_two_digit_bin naics_2017_two_digit_bin_title)

* Keep necessary variables 
keep naics_2017_two_digit_bin naics_2017_two_digit_bin_title

* Generate lowest 2-digit NAICS code in each 2-digit bin
g lowest_naics_2017_two_digit = substr(naics_2017_two_digit_bin, 1, 2)

* Generate highest 2-digit NAICS code in each 2-digit bin
g highest_naics_2017_two_digit = substr(naics_2017_two_digit_bin, -2, 2)

*Destring lowest and highest 2-digit NAICS codes
destring lowest_naics_2017_two_digit highest_naics_2017_two_digit, replace

* Expand 
expand highest_naics_2017_two_digit - lowest_naics_2017_two_digit + 1

* Generate 2-digit NAICS code for each row by adding the expansion index to the lowest 2-digit code
gsort naics_2017_two_digit_bin
by naics_2017_two_digit_bin: gen naics_2017_two_digit = lowest_naics_2017_two_digit + _n - 1

* Keep necessary variables 
keep naics_2017_two_digit naics_2017_two_digit_bin_title

* Rename
rename naics_2017_two_digit_bin_title naics_2017_two_digit_title

* Order
order naics_2017_two_digit naics_2017_two_digit_title

* Should be one-row-per-2017-two-digit-naics-code
gisid naics_2017_two_digit

* Tempfile 
tempfile naics_2017_two_digit_title
save `naics_2017_two_digit_title'

/* -----------------------------------------------------------------------------------------------------------
Construct a 2022 3-digit NAICS --> 2017 3-digit NAICS crosswalk
----------------------------------------------------------------------------------------------------------- */
* Import 2022 NAICS to 2017 NAICS crosswalk
import excel "${external}/2022_to_2017_NAICS.xlsx", firstrow case(lower) cellrange(A3:D1153) allstring clear

* Confirm set of variables in the import file 
confirm variable naicscode naicstitle c naicstitleandspecificp

* Rename variables 
rename (naicscode naicstitle c naicstitleandspecificp) (naics_2022_six_digit naics_2022_six_digit_description naics_2017_six_digit naics_2017_six_digit_description)

* Convert naics codes to 3 digit and destring --- no leadings 0s so substr is fine 
foreach year in 2017 2022 {
    * Generate 3-digit NAICS by taking first 3 chars of 6-digit code
    gen naics_`year'_three_digit = substr(naics_`year'_six_digit, 1, 3)

    * Destring 3-digit NAICS
    destring naics_`year'_three_digit, replace
}

* Drop extraneous variables
drop naics_2022_six_digit naics_2017_six_digit *description

* Order variables
order naics_2022_three_digit naics_2017_three_digit

* Keep only first observation of each 2022-2017 naics three-digit pair 
gsort naics_2022_three_digit naics_2017_three_digit 
by naics_2022_three_digit naics_2017_three_digit: keep if _n == 1

* For each 2022 code, generate count of how many 2017 codes it maps to i.e., number of one-to-many instances
gsort naics_2022_three_digit
by naics_2022_three_digit: gen count_2017 = _N

* Check one-to-many instances i.e., where a 2022 code maps onto multiple 2017 codes 
unique(naics_2022_three_digit) if count_2017 > 1
tab naics_2022_three_digit if count_2017 > 1

** Manual resolutions for 2022 codes that map to multiple sic aggregated bins downstream

* Case where a NAICS 2022 code maps to NAICS 2017 == 454 (Nonstore Retailers) + other NAICS 2017 codes
* NAICS in 2022 eliminated subsector 454, reassigning its establishments to store-based retail subsectors by product sold rather than by sales method (e.g., storefront/internet/direct)
* Where a 2022 code matches both 454 and another code, the 454 link is just the e-commerce piece from 2017 folded in, so keep the oher code in 2017 and drop 454 
drop if naics_2017_three_digit == 454 & count_2017 > 1

* Case where a NAICS 2022 code maps to NAICS 2017 == 519 (Other Information Services) + other NAICS 2017 codes
* NAICS in 2022 eliminated subsector 519, removing the "internet as delivery method" distinction and folding its pieces into 513 Publishing (with 511) and 516 Broadcasting & Content Providers (with 515)
* Where a 2022 code matches both 519 and another code, the 519 link is just the e-commerce piece from 2017 folded in, so keep the oher code in 2017 and drop 519
drop if naics_2017_three_digit == 519 & count_2017 > 1

* Case where NAICS 2022 == 455 (General Merchandise Retailers) --> NAICS 2017 = {452 (General Merchandise Stores), 453 (Miscellaneous Store Retailers)} --- keeping NAICS 2017 = 452 (General Merchandise Stores) based on name  
drop if naics_2017_three_digit == 453 & naics_2022_three_digit == 455 & count_2017 > 1

** XXNR: do not have any bite since these map to the saame sic bins downstream regardless --- just deduplicating (probably a more elegant way to do this)
* Case where NAICS 2022 == 459 (Sporting Goods, Hobby, Musical Instrument, Book, and Miscellaneous Retailers) --> NAICS 2017 = {451 (Sporting Goods, Hobby, Musical Instrument, and Book Stores), 453 (Miscellaneous Store Retailers)} --- keeping NAICS 2017 = 451 (Sporting Goods, Hobby, Musical Instrument, and Book Stores) based on name
drop if naics_2017_three_digit == 453 & naics_2022_three_digit == 459 & count_2017 > 1

* Case where NAICS 2022 == 449 (Furniture, Home Furnishings, Electronics, and Appliance Retailers) --> NAICS 2017 = {442 (Furniture and Home Furnishings Stores), 443 (Electronic and Appliance Stores)} --- keeping NAICS 2017 = 442 (Furniture and Home Furnishings Stores) arbitrarily
drop if naics_2017_three_digit == 443 & naics_2022_three_digit == 449 & count_2017 > 1

* Drop count variable given cases are resolved 
drop count_2017

* Tempfile 
tempfile naics_2022_to_2017
save `naics_2022_to_2017'

/* -----------------------------------------------------------------------------------------------------------
Construct a either m:1 or 1:1 2017 3-digit NAICS --> two-digit SIC bin code crosswalk
----------------------------------------------------------------------------------------------------------- */
* Import 2017 NAICS to SIC crosswalk
import excel "${external}/2017-NAICS-to-SIC-Crosswalk.xlsx", firstrow case(lower) cellrange(A2:D2272) clear

* Rename variables 
rename (naics naicsdescription sic sicdescription) (naics_2017_six_digit naics_2017_six_digit_description sic_four_digit sic_four_digit_description)

* Recast sic to numeric --- only non-numeric string is "AUX", so seems fine to force 
destring sic_four_digit, replace force

* Convert naics to 3 digit 
gen naics_2017_three_digit = floor(naics_2017_six_digit/1000)

* Convert sic to two digit 
gen sic_two_digit = floor(sic_four_digit/100)

* Generate two-digit naics code 
gen naics_2017_two_digit = floor(naics_2017_three_digit / 10)

* Drop extraneous variables
drop naics_2017_six_digit sic_four_digit *_description

* Store count of every unique naics3-sic2 pair 
//gsort naics_2017_three_digit sic_two_digit
//sby naics_2017_three_digit sic_two_digit: gen count = _N

* Keep only first observation of each naics3-sic2 pair
gsort naics_2017_three_digit sic_two_digit
by naics_2017_three_digit sic_two_digit: keep if _n == 1

* Drop observation missing a two-digit sic code 
drop if missing(sic_two_digit)

* Merge on three-digit naics titles  
merge m:1 naics_2017_three_digit using `naics_2017_three_digit_title', assert(3) nogen

* Merge on two-digit naics titles
merge m:1 naics_2017_two_digit using `naics_2017_two_digit_title', assert(3) nogen

* Merge on two-digit sic bin titles, keeping only observations where the sic code has a bin match
* _merge == 1 <--> case where sic code has no bin match (i.e., no firm in our sample is apart of this industry)
* _merge == 2 <--> sic codes that do not actually exist in the sic code universe (exist as an integer here since we are using ranges)
merge m:1 sic_two_digit using "${dump}/sic_two_digit_to_aer_bin_crosswalk.dta", keep(3) nogen

* Define sic division based on two-digit sic code (from online reference: https://www.osha.gov/data/sic-manual)
gen sic_division = "Agriculture, Forestry, And Fishing" if inlist(sic_two_digit, 1, 2, 7, 8, 9)
replace sic_division = "Mining" if inlist(sic_two_digit, 10, 12, 13, 14)
replace sic_division = "Construction" if inlist(sic_two_digit, 15, 16, 17)
replace sic_division = "Manufacturing" if inrange(sic_two_digit, 20, 39)
replace sic_division = "Transportation, Communications, Electric, Gas, And Sanitary Services" if inrange(sic_two_digit, 40, 49)
replace sic_division = "Wholesale Trade" if inrange(sic_two_digit, 50, 51)
replace sic_division = "Retail Trade" if inrange(sic_two_digit, 52, 59)
replace sic_division = "Finance, Insurance, And Real Estate" if inrange(sic_two_digit, 60, 67)
replace sic_division = "Services" if inrange(sic_two_digit, 70, 89)
replace sic_division = "Public Administration" if inrange(sic_two_digit, 90, 97)

* Keep necessary variables 
keep naics_2017_two_digit  naics_2017_two_digit_title naics_2017_three_digit naics_2017_three_digit_title sic_division sic_two_digit_bin_aer sic_two_digit_bin_title_aer 

* Order variables 
order naics_2017_two_digit  naics_2017_two_digit_title naics_2017_three_digit naics_2017_three_digit_title sic_division sic_two_digit_bin_aer sic_two_digit_bin_title_aer 

* Keep one observation per 2017 three-digit naics x two-digit sic bin pair
gsort naics_2017_three_digit sic_two_digit_bin_aer
by naics_2017_three_digit sic_two_digit_bin_aer: keep if _n == 1

* For each 2017 naics code, generate count of how many two-digit sic codes it maps to i.e., number of one-to-many instances
gsort naics_2017_three_digit
by naics_2017_three_digit: gen count_sic_bin_mappings = _N

* Check one-to-many instances i.e., where a 2017 naics code maps onto multiple sic codes 
unique(naics_2017_three_digit) if count_sic_bin_mappings > 1
tab naics_2017_three_digit if count_sic_bin_mappings > 1

**** All manual resolutions are assigned to one of the sic_two_digit_bin_title_aer bins

** Manual resolutions for NAICS 2017 codes into the "Food products" aer bin
* NAICS 2017 three digit == {311 (Food Manufacturing), 312 (Beverage and Tobacco Product Manufacturing)} --- assign to "Food products" aer bin 
drop if inlist(naics_2017_three_digit, 311, 312) & sic_two_digit_bin_title_aer != "Food products"

** Manual resolutions for NAICS 2017 codes into the "Manufacturing" aer bin --- these share the quality that two-digit NAICS code is 31 (Manufacturing) 
* NAICS 2017 two digit == 31 (Manufacturing) --- assign to "Manufacturing" aer bin
assert naics_2017_two_digit_title == "Manufacturing" if count_sic_bin_mappings > 1 & inrange(naics_2017_three_digit, 313, 339)
drop if inrange(naics_2017_three_digit, 313, 339) & sic_two_digit_bin_title_aer != "Manufacturing"

** Manual resolution for NAICS 2017 three-digit codes into the "Freight / transport" aer bin
* NAICS 2017 three digit == {481 (Air Transportation), 485 (Transit and Ground Passenger Transportation), 491 (Postal Service), 486 (Pipeline Transportation), 488 (Support Activities for Transportation)} --- assign to "Freight / transport" aer bin
drop if inlist(naics_2017_three_digit, 481, 485, 491, 486, 488) & sic_two_digit_bin_title_aer != "Freight / transport"

** Manual resolution for NAICS 2017 three-digit codes into the "Communications" aer bin
* NAICS 2017 three digit == 517 (Telecommunications) --- assign to "Communications" aer bin
drop if naics_2017_three_digit == 517 & sic_two_digit_bin_title_aer != "Communications"

** Manual resolution for NAICS 2017 three-digit codes into the "Wholesale trade" aer bin
* NAICS 2017 three digit == 425 (Wholesale Electronic Markets and Agents and Brokers) --- assign to "Wholesale Trade" aer bin
drop if naics_2017_three_digit == 425 & sic_two_digit_bin_title_aer != "Wholesale trade"

** Manual resolution for NAICS 2017 three-digit codes into the "Building materials" aer bin
* NAICS 2017 three digit == 444 (Building Material and Garden Equipment and Supplies Dealers) --- assign to "Building materials" aer bin
drop if naics_2017_three_digit == 444 & sic_two_digit_bin_title_aer != "Building materials"

** Manual resolution for NAICS 2017 three-digit codes into the "General merchandise" aer bin
* NAICS 2017 three digit == 452 (General Merchandise Stores) --- assign to "General merchandise" aer bin
drop if naics_2017_three_digit == 452 & sic_two_digit_bin_title_aer != "General merchandise"

** Manual resolution for NAICS 2017 three-digit codes into the "Food stores" aer bin
* NAICS 2017 three digit == 445 (Food and Beverage Stores) --- assign to "Food stores" aer bin
drop if naics_2017_three_digit == 445 & sic_two_digit_bin_title_aer != "Food stores"

** Manual resolution for NAICS 2017 three-digit codes into the "Auto dealers / services / parts" aer bin
* NAICS 2017 three digit == {441 (Motor Vehicle & Parts Dealers), 447 (Gasoline Stations)} --- assign to "Auto dealers / services / parts" aer bin
drop if inlist(naics_2017_three_digit, 441, 447) & sic_two_digit_bin_title_aer != "Auto dealers / services / parts"

** Manual resolution for NAICS 2017 three-digit codes into the "Apparel stores" aer bin
* NAICS 2017 three digit == 448 (Clothing and Clothing Accessories Stores) --- assign to "Apparel stores" aer bin
drop if naics_2017_three_digit == 448 & sic_two_digit_bin_title_aer != "Apparel stores"

** Manual resolution for NAICS 2017 three-digit codes into the "Home furnishing stores" aer bin
*  NAICS 2017 three digit == {442 (Furniture and Home Furnishings Stores), 443 (Electronics and Appliance Stores)} --- assign to "Home furnishing stores" aer bin
drop if inlist(naics_2017_three_digit, 442, 443) & sic_two_digit_bin_title_aer != "Home furnishing stores"

** Manual resolution for NAICS 2017 three-digit codes into the "Eating / drinking" aer bin
* NAICS 2017 three digit == 722 (Food Services and Drinking Places) --- assign to "Eating / drinking" aer bin
drop if naics_2017_three_digit == 722 & sic_two_digit_bin_title_aer != "Eating / drinking"

** Manual resolution for NAICS 2017 three-digit codes into the "Other retail" aer bin
* NAICS 2017 three digit == {446 (Health and Personal Care Stores), 451 (Sporting Goods, Hobby, Musical Instrument, and Book Stores), 453 (Miscellaneous Store Retailers), 454 (Nonstore Retailers)} --- assign to "Other retail" aer bin
drop if inlist(naics_2017_three_digit, 446, 451, 453, 454) & sic_two_digit_bin_title_aer != "Other retail"

** Manual resolution for NAICS 2017 three-digit codes into the "Banks / securities" aer bin
* NAICS 2017 three digit == {522 (Credit Intermediation and Related Activities), 523 (Security and Commodity Contracts Intermediation and Related Activities), 525 (Funds, Trusts, and Other Financial Vehicles )} --- assign to "Banks / securities" aer bin
drop if inlist(naics_2017_three_digit, 522, 523, 525) & sic_two_digit_bin_title_aer != "Banks / securities"

** Manual resolution for NAICS 2017 three-digit codes into the "Accommodation / real estate" aer bin 
* NAICS 2017 three digit == 531 (Real Estate) --- assign to "Accommodation / real estate" aer bin
drop if naics_2017_three_digit == 531 & sic_two_digit_bin_title_aer != "Accommodation / real estate"

** Manual resolution for NAICS 2017 three-digit codes into the "Personal / business services" aer bin
* NAICS 2017 three digit == {812 (Personal and Laundry Services), 238 (Specialty Trade Contractors), 511 (Publishing Industries (Except Internet)), 512 (Motion Picture and Sound Recording Industries), 519 (Other Information Services), 532 (Rental and Leasing Services), 541 (Professional, Scientific, and Technical Services), 561 (Administrative and Support Services), 562 (Waste Management and Remediation Services), 611 (Educational Services), 711 (Performing Arts, Spectator Sports, and Similar Activities)} --- assign to "Personal / business services" aer bin
drop if inlist(naics_2017_three_digit, 812, 238, 511, 512, 519, 532, 541, 561, 562, 611, 711) & sic_two_digit_bin_title_aer != "Personal / business services"

** Manual resolution for NAICS 2017 three-digit codes into the "Repair services" aer bin
* NAICS 2017 three digit == 811 (Repair and Maintenance) --- assign to "Repair services" aer bin
drop if naics_2017_three_digit == 811 & sic_two_digit_bin_title_aer != "Repair services"

** Manual resolution for NAICS 2017 three-digit codes into the "Health and engineering services" aer bin
* NAICS 2017 three digit == {621 (Ambulatory Health Care Services), 237 (Heavy and Civil Engineering Services), 813 (Religious, Grantmaking, Civic, Professional, and Similar Organizations)} --- assign to "Health and engineering services" aer bin
drop if inlist(naics_2017_three_digit, 621, 237, 813) & sic_two_digit_bin_title_aer != "Health and engineering services"

* Check uniqueness on 2017 three-digit NAICS code
gisid naics_2017_three_digit

* Keep necessary variables
keep naics_2017_three_digit naics_2017_three_digit_title sic_two_digit_bin_aer sic_two_digit_bin_title_aer

* Tempfile
tempfile naics_2017_to_sic_bin
save `naics_2017_to_sic_bin'

/* -----------------------------------------------------------------------------------------------------------
Output final 2022 3-digit NAICS --> two-digit SIC bin code crosswalk
----------------------------------------------------------------------------------------------------------- */
* Import 2022 NAICS to 2017 NAICS crosswalk
use `naics_2022_to_2017', clear

* Merge on 2017 NAICS to SIC bin crosswalk, keeping only matches 
* _merge == 1 <--> 2017 naics code is not mapped to a sic bin (i.e., no firm in our sample is apart of this industry)
* _merge == 2 <--> sic bin code not represented in the 2017 naics universe; fine to drop since we will not be able to map the relevant NAICS codes in the PUF to these bins 
merge m:1 naics_2017_three_digit using `naics_2017_to_sic_bin', keep(3) nogen

* Keep necessary variables
keep naics_2022_three_digit sic_two_digit_bin_aer sic_two_digit_bin_title_aer

* Check uniqueness on 2022 three-digit NAICS code
gisid naics_2022_three_digit

* Export 
compress
save "${dump}/naics_2022_three_digit_sic_bin_aer_crosswalk.dta", replace