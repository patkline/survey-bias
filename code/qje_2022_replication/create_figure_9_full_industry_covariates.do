/* -----------------------------------------------------------------------------------------------------------
Purpose: Recreate the non-CPS industry covariates for the full QJE Figure 9
from public EEO-1 and Census concentration data

Created: Nico Rotundo 2026-05-20

Reads from: qje_2022_replication_data_and_outputs/data/raw/EEO1_2018_PUF.xlsx
           qje_2022_replication_data_and_outputs/data/raw/EC1700SIZECONCEN.dat
Writes to: qje_2022_replication_data_and_outputs/data/dump/figure9_full_eeo1_industry_covariates.dta
           qje_2022_replication_data_and_outputs/data/dump/figure9_full_concentration_naics.dta

XX: The original Figure 9 input covariates/black_employment_shares_3dig.csv is
    not recoverable from the replication package or the Survey Dropbox mirror.
    The EEO-1 rows created here are an explicit public-data approximation using
    the 2018 EEO-1 PUF from the EEOC.
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Build EEO-1 industry covariates
----------------------------------------------------------------------------------------------------------- */
* Import public EEO-1 PUF
import excel using "${qje_2022_replication_raw}/EEO1_2018_PUF.xlsx", ///
    sheet("Data") firstrow case(lower) allstring clear

* Keep national NAICS3 rows
keep if inlist(nation, "United States")
keep if mi(region) & mi(division) & mi(state) & mi(cbsa) & mi(county)
keep if ~mi(naics3)

* Destring employment-count columns
destring naics3 total10 blkt10 ft10 total1_2 blkt1_2 ft1_2, ignore("*") replace
gisid naics3

* Construct industry and management employment-share variables
gen double share_black_ind = blkt10 / total10
gen double share_female_ind = ft10 / total10
gen double share_black_mgmt_ind = blkt1_2 / total1_2
gen double share_female_mgmt_ind = ft1_2 / total1_2
gen double mgmt_dif_black_ind = share_black_mgmt_ind - share_black_ind
gen double mgmt_dif_female_ind = share_female_mgmt_ind - share_female_ind

assert inrange(share_black_ind, 0, 1)
assert inrange(share_female_ind, 0, 1)
assert inrange(share_black_mgmt_ind, 0, 1)
assert inrange(share_female_mgmt_ind, 0, 1)
assert inrange(mgmt_dif_black_ind, -1, 1)
assert inrange(mgmt_dif_female_ind, -1, 1)

* Label EEO-1 covariates
label var naics3 "Three-digit NAICS code"
label var share_black_ind "Industry Black employment share"
label var share_female_ind "Industry female employment share"
label var share_black_mgmt_ind "Industry Black first/mid management share"
label var share_female_mgmt_ind "Industry female first/mid management share"
label var mgmt_dif_black_ind "Black management share minus industry employment share"
label var mgmt_dif_female_ind "Female management share minus industry employment share"

* Save EEO-1 industry covariates
keep naics3 share_black_ind share_black_mgmt_ind mgmt_dif_black_ind ///
    share_female_ind share_female_mgmt_ind mgmt_dif_female_ind
order naics3 share_black_ind share_black_mgmt_ind mgmt_dif_black_ind ///
    share_female_ind share_female_mgmt_ind mgmt_dif_female_ind
sort naics3
compress
save "${qje_2022_replication_dump}/figure9_full_eeo1_industry_covariates.dta", replace
export delimited using "${qje_2022_replication_dump}/figure9_full_eeo1_industry_covariates.csv", replace

/* -----------------------------------------------------------------------------------------------------------
Build Census concentration covariates
----------------------------------------------------------------------------------------------------------- */
* Import public Census concentration file
import delimited "${qje_2022_replication_raw}/EC1700SIZECONCEN.dat", ///
    delimiters("|") varnames(1) case(lower) clear

* Keep top-four firm sales concentration
keep if inlist(concenfi, 604)
rename val_pct top4share
rename naics2017 naics
keep naics top4share
destring naics top4share, force replace
drop if mi(naics) | mi(top4share)
gisid naics

* Label concentration covariates
label var naics "NAICS code"
label var top4share "Top four firm sales share"

* Save concentration covariates
order naics top4share
sort naics
compress
save "${qje_2022_replication_dump}/figure9_full_concentration_naics.dta", replace
export delimited using "${qje_2022_replication_dump}/figure9_full_concentration_naics.csv", replace
