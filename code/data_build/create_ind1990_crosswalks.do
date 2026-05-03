/* -----------------------------------------------------------------------------------------------------------
Purpose: Build ind1990 (time-invariant industry code in CPS)--> (SIC category, NAICS3) 
crosswalks from external reference files (Autor/Dorn/Hanson + Census Bureau) and 
output a single combined crosswalk for constructing industry x demographic level wage 
and employment information

Created: Nico Rotundo 2026-04-21

----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Build ind1990 --> NAICS3 crosswalk from Census Bureau data 
* Note. Ported from wage_regressions.do L17-30
----------------------------------------------------------------------------------------------------------- */
* Load Census Bureau crosswalk — 13 cols A-M; data rows 26-307 (282 rows); header rows 1-25 excluded
import excel "${external}/industry-crosswalk-90-00-02-07-12.xls", sheet("1990-2012") cellrange(A25:M307)  firstrow clear 

* Rename the 4 columns we need 
rename (Census Census2000 Census2000CategoryTitle K) (ind1990 ind2000 ind2000_title naics2012)

* Keep just these 4 columns
keep ind1990 ind2000 ind2000_title naics2012

* Drop rows missing ind1990 or marked "New" (Census 2000 codes without a 1990 equivalent) since we will not be able to map to these from CPS ind1990
drop if missing(ind1990) | strpos(ind1990, "New") > 0

* Destring ind1990, stripping annotation characters (* p + - /) 
destring ind1990, ignore("*" "p" "+" "-" "/") replace

* Assert ind1990 is non-missing post-destring
assert ~mi(ind1990)

* Drop ind1990 == 992 ("Last worked 5+ years ago or never worked" — not a real industry)
drop if ind1990 == 992

**** Extract naics3 from naics2012 column
* Note. Ported from wage_regressions.do L33-50

** Case 1: clean numeric naics2012 --> naics3 = first 3 chars
gen naics3 = real(substr(naics2012, 1, 3)) if ~mi(real(naics2012))

** Case 2: multi-code naics2012 --> naics3 only if all sub-codes share the same first 3 chars
* Start from a cleaning copy of naics2012
clonevar naicstemp = naics2012

* Strip noise words from the cleaning copy
foreach noise_word in "exc" "pt" "Pts" "Part of" "and" {
    replace naicstemp = subinstr(naicstemp, "`noise_word'", "", .)
}

* Replace separators (. , - *) with spaces, then trim whitespace
foreach separator in "." "," "-" "*" {
    replace naicstemp = subinstr(naicstemp, "`separator'", " ", .)
}
replace naicstemp = trim(naicstemp)

* Tokenize naicstemp on whitespace into naicstemp1, naicstemp2, ...
split naicstemp

* Assert at most 6 sub-tokens (matches wage_regressions.do L46 forval bound; fails loudly if any row has 7+)
cap confirm variable naicstemp7
assert _rc != 0

* First 3 chars of first sub-code
gen first_three = substr(naicstemp1, 1, 3)

* Mark rows where any sub-code 2..6 has a different first 3 chars
gen all_match = 1
forval i = 2/6 {
    cap confirm variable naicstemp`i'
    if !_rc replace all_match = 0 if naicstemp`i' != "" & substr(naicstemp`i', 1, 3) != first_three
}

* Fill naics3 for multi-code rows where all sub-codes share the first 3 chars
replace naics3 = real(first_three) if mi(naics3) & all_match == 1

* Drop helper columns
drop first_three all_match naicstemp*

** Case 3: naics2012 missing + ind2000 = "Old" --> recover naics3 by looking up the 2000 code parsed from ind2000_title
* Parse 3-digit 2000 Census code from ind2000_title (format "(now part of XXX)", chars 14-16)
gen ind2000_from_title = real(substr(ind2000_title, 14, 3)) if mi(naics2012)

* Destring ind2000 in place, treating "Old" as missing (leaves numeric 2000 codes intact)
destring ind2000, ignore("Old") replace

* For each parsed title-code, assign naics3 from the row with matching ind2000 (only if exactly one match)
levelsof ind2000_from_title, local(ind2000_codes)
foreach ind2000_code of local ind2000_codes {
    sum naics3 if ind2000 == `ind2000_code'
    if r(N) == 1 {
        replace naics3 = r(mean) if ind2000_from_title == `ind2000_code'
    }
}

* Keep necessary variables 
keep ind1990 naics3

**** Manual resolutions for ind1990 codes whose naics2012 column is ambiguous or multi-NAICS3
* Note. Ported from wage_regressions.do L62-95

** Group 1: ind1990 corresponds to multiple naics2012 within the same obs (ambiguous 3-digit prefixes)
replace naics3 = 313 if ind1990 == 132     // Knitting mills --> Textile Mills
replace naics3 = 331 if ind1990 == 301     // Metal industries, n.s. --> Primary Metal Mfg
replace naics3 = 339 if ind1990 == 392     // Mfg industries, n.s. --> Misc Mfg
replace naics3 = 453 if ind1990 == 691     // Retail trade, n.s. --> Misc Store Retailers
replace naics3 = 522 if ind1990 == 700     // Banking --> Credit Intermediation
replace naics3 = 523 if ind1990 == 710     // Security brokerage --> Securities
replace naics3 = 561 if ind1990 == 741     // Business svc, n.e.c. --> Admin/Support
replace naics3 = 922 if ind1990 == 910     // Justice/public order --> Justice
replace naics3 = 924 if ind1990 == 930     // Admin env quality --> Environmental Programs
replace naics3 = 926 if ind1990 == 931     // Admin econ programs --> Economic Programs
replace naics3 = .   if ind1990 == 992     // "Last worked 1984 or earlier" --> missing

** Group 2: ind1990 mapped to multiple 3-digit naics in different rows (dedup-level duplicates)
replace naics3 = 711 if ind1990 == 810     // Misc entertainment --> Performing Arts
replace naics3 = 518 if ind1990 == 732     // Computer/data processing --> Data Processing
replace naics3 = 453 if ind1990 == 682     // Misc retail stores --> Misc Store Retailers
replace naics3 = 451 if ind1990 == 652     // Book/stationery --> Sporting Goods/Book/Music
replace naics3 = 562 if ind1990 == 471     // Sanitary svc --> Waste Management
replace naics3 = 488 if ind1990 == 432     // Transport svc --> Support for Transport
replace naics3 = 484 if ind1990 == 410     // Trucking --> Truck Transport
replace naics3 = 485 if ind1990 == 401     // Bus service --> Transit/Ground Passenger
replace naics3 = 335 if ind1990 == 342     // Elec machinery, n.e.c. --> Elec Equip Mfg
replace naics3 = 511 if ind1990 == 172     // Printing/publishing --> Publishing

** Group 3: ind1990 mapped to 2-digit NAICS (upgraded to 3-digit per researcher choice)
replace naics3 = 221 if ind1990 == 472     // Utilities, n.s. --> Utilities
replace naics3 = 236 if ind1990 == 60      // Construction --> Construction of Buildings
replace naics3 = 423 if ind1990 == 571     // Wholesale trade, n.s. --> Durable Wholesalers

** Group 4: no courier/messengers category in 1990 NAICS; warehousing re-mapped
replace naics3 = 492 if ind1990 == 411     // Warehousing --> Courier/Messengers

**** Dedup + save
* Sort by ind1990 and naics3
gsort ind1990 naics3

* Assert all rows within an ind1990 share the same naics3 (post-manual-resolution invariant)
bys ind1990: assert naics3[1] == naics3[_N]

* Keep one row per (ind1990, naics3) pair
by ind1990 naics3: keep if _n == 1

* Assert uniqueness on ind1990
gisid ind1990

* Order PK first
order ind1990

* Keep just the two crosswalk columns and label naics3
keep ind1990 naics3
label variable naics3 "NAICS 3-digit 2012 codes"

* Save as tempfile
tempfile ind1990_to_naics3
save `ind1990_to_naics3'

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

/* -----------------------------------------------------------------------------------------------------------
Merge naics3 onto ind1990 --> sic_87_two_digit crosswalk, derive naics2, and 
export 
----------------------------------------------------------------------------------------------------------- */
* Merge naics3 from census crosswalk onto working data 
merge 1:1 ind1990 using `ind1990_to_naics3', nogen

* Assert no row has both sic and naics3 missing (would be useless downstream)
assert ~(mi(sic_87_two_digit) & mi(naics3))

* Derive 2-digit NAICS (sector) from naics3
gen naics2 = floor(naics3 / 10)

* Label substantive variables (naics3 already labeled in Section 6)
label variable sic_87_two_digit "2-digit SIC 1987"
label variable naics2 "NAICS 2-digit 2012 (sector)"

* Assert ind1990 is unique
gisid ind1990

* Order variables and sort by ind1990
order ind1990 sic_87_two_digit naics3 naics2
gsort ind1990

* Compress and save permanent output
compress
save "${dump}/ind1990_crosswalks.dta", replace
