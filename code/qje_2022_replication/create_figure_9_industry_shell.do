/* -----------------------------------------------------------------------------------------------------------
Purpose: Extract the firm-level industry shell needed for the CPS-derived rows
of QJE Figure 9

Created: Nico Rotundo 2026-04-24

Reads from: Dropbox Survey consolidated_code/external/qje_data.dta
Writes to: code/qje_2022_replication/dump/figure9_industry_shell.dta
----------------------------------------------------------------------------------------------------------- */

* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Extract firm-level industry shell
----------------------------------------------------------------------------------------------------------- */
* Load original QJE data with full industry keys
use "${qje_2022_replication_data}/qje_data.dta", clear
keep firm_id sic_code sic_combined naics naics3
duplicates drop
gisid firm_id

* Save firm industry shell
order firm_id sic_code sic_combined naics naics3
sort firm_id
compress
save "${qje_2022_replication_dump}/figure9_industry_shell.dta", replace
export delimited using "${qje_2022_replication_dump}/figure9_industry_shell.csv", replace
