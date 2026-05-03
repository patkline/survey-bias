#delimit cr

* Run globals
do "${github}/survey-bias/code/globals.do"


#d; 

* Set file paths ;
global rawdir "${qje_2022_replication_data}" ;
global wrkdir "${qje_2022_replication_dump}/covariates_new_cleaning" ;

**********************************
***** 1. LOAD AND CLEAN
********************************** ;
#d;

**** 1.1 Cross walks ;

** ind1990 to naics2012 ;
import excel using  "$rawdir/industry-crosswalk-90-00-02-07-12.xls", clear cellra(A26:M307) ; 
  ren A ind1990 ;
  ren K naics2012 ; 
  ren C ind2000 ; 
  ren D ind2000_title ; 
  keep ind* naics2012 ;

  * clean up ind1990 ;
    drop if strpos(ind1990, "New") > 0 ;
    /* * = 1990 major group change to a different NAICS/2000 sector 
       p = part of a 1990 category (1990 category split into 2 or more major parts)
       - (minus) = component moved from 1990 industry to different 2000 category 
       + (plus) = component added to 1990 industry to make new 2000 category  */
    destring ind1990, ignore("*" "p" "+" "-" "+/-") replace ; 

  * clean up naics2012 ;
      gen naicstemp = naics2012 ; 
    /* clean conversions */
    gen naics3 = real(substr(naics2012, 1, 3)) if real(naics2012) != . ; 
    /* ind1990 corresponds to multiple naics2012, but same 3-digit naics  */
      * split into variables naicstempX for each subcode ;
      foreach s in "exc" "pt" "Pts" "Part of" "and" { ; replace naicstemp = subinstr(naicstemp, "`s'", "", .) ; } ; 
      foreach s in "." "," "-" "*" { ; replace naicstemp = subinstr(naicstemp, "`s'", " ", .) ; } ; 
      replace naicstemp = trim(naicstemp) ;
      split naicstemp ;
      * extract 3-digit code from first ;
      gen temp = substr(naicstemp1, 1, 3) ; 
      * tag obs where subsequent 3-digit codes don't match first ;
      gen same = 1 ;
      forval i = 2/6 { ;
        replace same = 0 if naicstemp`i' != "" & substr(naicstemp`i', 1, 3) !=  temp ; 
      } ; 
      replace naics3 = real(temp) if same == 1 ;
      drop temp same naicstemp* ;

    /* missing NAICS because census changed */
    gen ind2000_temp = real(substr(ind2000_title, 14, 3)) if missing(naics2012) ; /* get 2000 census code */
    destring ind2000, ignore("Old") replace ; 
    levelsof ind2000_temp, local(levels) ; 
    foreach i of local levels { ; 
      sum naics3 if ind2000 == `i' ;
      replace naics3 = `r(mean)' if ind2000_temp == `i' & `r(N)' == 1 ;
    } ;


    /* ind1990 corresponds to multiple 3-digit naics  (within same obs) */
    gen miss = missing(naics3) ;
    replace naics3 = 313 if ind1990 == 132 ;/* Knitting mills -> Textile Mills*/
    replace naics3 = 331 if ind1990 == 301 ;/* Metal industries, n.s. -> Primary Metal Manufacturing*/
    replace naics3 = 339 if ind1990 == 392 ;/* Manufacturing industries, n.s. -> Miscellaneous Manufacturing*/
    replace naics3 = 453 if ind1990 == 691 ;/* Retail trade, n.s. -> Miscellaneous Store Retailers */
    replace naics3 = 522 if ind1990 == 700 ;/* Banking -> Credit Intermediation and Related Activities */
    replace naics3 = 523 if ind1990 == 710 ;/* Security, commodity brokerage, and investment companies -> Securities, Commodity Contracts, and Other Financial Investment and Related Activities */
    replace naics3 = 561 if ind1990 == 741 ;/* Business services, n.e.c. -> Administrative and Support Services Administrative and Support Services  */
    replace naics3 = 922 if ind1990 == 910 ;/* Justice, public order, and safety -> Justice, Public Order, and Safety Activities*/
    replace naics3 = 924 if ind1990 == 930 ;/* Administration of environmental quality and housing programs -> Administration of Environmental Quality Programs */
    replace naics3 = 926 if ind1990 == 931 ;/* Administration of economic programs   ->  Administration of Economic Programs */
    replace naics3 = . if ind1990 == 992 ;/*  Last worked 1984 or earlier */

    /* ind1990 mapped to multiple 3-digit naics  (duplicates) */
    replace naics3 = 711 if ind1990 == 810 ;/* Miscellaneous entertainment and recreation services   -> Performing Arts, Spectator Sports and Related Industries */
    replace naics3 = 518 if ind1990 == 732 ;/* Computer and data processing services   ->  Data Processing, Hosting, and Related Services */
    replace naics3 = 453 if ind1990 == 682 ;/* Miscellaneous retail stores   -> Miscellaneous Store Retailers */
    replace naics3 = 451 if ind1990 == 652 ;/* Book and stationery stores  -> Sporting Goods, Hobby, Book and Music Stores*/
    replace naics3 = 562 if ind1990 == 471 ;/* Sanitary services  -> Waste Management and Remediation Services */
    replace naics3 = 488 if ind1990 == 432 ;/* Services incidental to transportation  -> Support Activities for Transportation*/
    replace naics3 = 484 if ind1990 == 410 ;/* Trucking service -> Truck Transportation */
    replace naics3 = 485 if ind1990 == 401 ;/* Bus service and urban transit -> Transit and Ground Passenger Transportation */
    replace naics3 = 335 if ind1990 == 342 ;/* Electrical machinery, equipment, and supplies, n.e.c.   -> Electrical Equipment, Appliance and Component Manufacturing */
    replace naics3 = 511 if ind1990 == 172 ;/* Printing, publishing, and allied industries, except newspapers -> Publishing Industries */


  /* mapped to 2-digit codes */
  replace naics3 = 221 if ind1990 == 472 ; /* Utilities, n.s. - > Utilities */
  replace naics3 = 236 if ind1990 == 60 ; /* Construction -> Construction of Buildings */
  replace naics3 = 423 if ind1990 == 571 ; /* Wholesale trade, n.s. ->  Merchant Wholesalers, Durable Goods */

  /* other issue, no courier and messengers in 1990 */
  replace naics3 = 492 if ind1990 == 411 ; /* warehousing and storage -> courier and messengers  */


  bys ind1990 naics3: keep if _n == 1 ;
  /* note: naics 3-digit 2012 codes same as 2017, see below */
  lab var naics3 "NAICS 3-digit 2012/2017 codes" ;
  keep ind1990 naics3 ; 

save "${wrkdir}/cw_ind1990_naics2012_3.dta", replace ; 

/*
* ind2012 to 3-digit NAICS 2017 ; 

  import excel using  "$rawdir/2017_to_2012_NAICS.xlsx", clear cellrange(A4) ; 
  gen naics2017_3 = substr(string(A), 1, 3) ;
  gen naics2012_3 = substr(string(C), 1, 3) ;
  bys naics2017_3 naics2012_3: keep if _n == 1 ;

  keep naics201?_3 ; 
  destring _all, replace ; 
  count if naics2017_3 != naics2012_3 ; /* all the same */
*/

/* naics3 = 423: Merchant Wholesalers, Durable Goods 
should correspond to durable goods in census ind1990 = 500-532*/


use "$rawdir/cw_sic87_ind1990ddx.dta", clear ; 
  * collapse to 2-digit ;
  gen sic = floor(sic87/100) ; 
  * resolve duplicates ;
  replace sic = 15 if ind1990ddx == 60 ;
  replace sic = 62 if ind1990ddx == 710 ;
  replace sic = 63 if ind1990ddx == 711 ;
  replace sic = 78 if ind1990ddx == 800 ;
  bys sic ind1990ddx: keep if _n == 1 ;  

  merge 1:m ind1990ddx using "$rawdir/cw_ind1990_ind1990ddx.dta", nogen ;
  drop if missing(ind1990) ;
  
save "${wrkdir}/cw_in1990_sic.dta", replace ; 

* CPS ; 
#d;
use "${qje_2022_replication_data}/cps_00086.dta", clear ;

  * education ;
  gen edlevel = 1 if inrange(educ,2, 72) ;
  replace edlevel = 2 if inrange(educ, 73, 73) ;
  replace edlevel = 3 if inrange(educ, 80, 90) ;
  replace edlevel = 4 if inrange(educ, 91, 109) ;
  replace edlevel = 5 if inrange(educ, 110, 122) ;
  replace edlevel = 6 if inrange(educ, 123, 125) ;
  label define edlevelL 1 "Below HS" 2 "HS grad" 3 "College dropout" 4 "Associate's degree" 5 "Bachelor's degree" 6 "Post-graduate degree" ;
  label val edlevel edlevelL ; 

  * college dummy ;
  gen college = inrange(educ, 110, 125) if inrange(educ, 2, 125) ;

  * black ;
  gen black = race == 200 if inrange(race, 100, 830) ;

  * female ;
  gen female = sex==2 ;

  * Recode missings ;
  replace earnweek = . if earnweek == 9999.99 ;
  replace uhrsworkorg = . if inlist(uhrsworkorg, 998, 999) ; 

  * Restrict data ;
  keep if inrange(age, 20, 60) ;
  keep if wkstat == 11 ; /* full-time */
  keep if qearnwee != 0 ; /* allocated earnings */
  keep if quhrsworkorg != 0  ; /* allocated hours */
  keep if inlist(classwkr, 22, 23) ; 
  
  * Merge in SIC codes  ; 
  merge m:1 ind1990 using "${wrkdir}/cw_in1990_sic.dta", nogen; 
  merge m:1 ind1990 using "${wrkdir}/cw_ind1990_naics2012_3.dta", nogen ;

  * New variables ;
  gen hr_wage = earnweek/uhrsworkorg ; 
  gen ln_hr_wage = ln(earnweek/uhrsworkorg) ;
  gen age4 = age^4 ; 

tempfile temp ;
save `temp' ; 

**********************************
***** ANALYSIS
********************************** ;
foreach ind in "sic" "naics3" { ; 
** Employment and college share gaps by industry ;
  * By race ;
  use `temp', clear ; 

  bys `ind' black: egen emp = mean(earnwt) ; 
  bys `ind' black: egen college_share = wtmean(college), weight(hwtfinl) ; 
  gen n = 1 ;
  bys `ind' black: egen obs = total(n) ;

  bys `ind' black: keep if _n == 1 ;

  bys black: egen tot_emp = total(emp) ;
  replace emp = emp/tot_emp ; 


  gen black_s = "_black" if black == 1 ;
  replace black_s = "_white" if black == 0 ; 
  drop if missing(black) ;

  keep `ind' black_s emp college_share obs ; 
  reshape wide emp college_share obs, i(`ind') j(black_s) string ; 

  gen emp_gap_race = emp_black - emp_white ;
  gen college_share_gap_race = college_share_black - college_share_white ;

  tempfile race ;
  save  `race' ; 

  * By gender ;
  use `temp', clear ; 
  
  bys `ind' female: egen emp = mean(earnwt) ; 
  bys `ind' female: egen college_share = wtmean(college), weight(hwtfinl) ; 
  gen n = 1 ;
  bys `ind' female: egen obs = total(n) ;

  bys `ind' female: keep if _n == 1 ;
  gen female_s = "_men" if female == 0 ;
  replace female_s = "_women" if female == 1 ; 
  drop if missing(female) ;

  keep `ind' female_s emp college_share obs ; 
  reshape wide emp college_share obs, i(`ind') j(female_s) string ; 


  gen emp_gap_gender = emp_women - emp_men ;
  gen college_share_gap_gender = college_share_women - college_share_men ;

  merge 1:1 `ind' using `race', nogen ; 

save "$wrkdir/`ind'_shares", replace ; 

** Wage gaps ; 

use `temp', clear ; 
* By race ; 

  * Version 1 ; 
  reg ln_hr_wage i.black##i.`ind' i.year [pw=earnwt], robust ; 

  * fill matrix with values of `ind', point estimates, and se ;
  levelsof `ind' if e(sample) == 1, local(levels) ; 
  cap mat drop estrace1 ;
  foreach s of local levels { ;
    qui lincom _b[1.black] +  _b[1.black#`s'.`ind'] ; 
    mat temp = (`s', `r(estimate)', `r(se)') ; 
	qui lincom _b[_cons] +  _b[`s'.`ind'] ; 
	mat temp2 = (`r(estimate)', `r(se)') ; 
    mat estrace1 = nullmat(estrace1) \ (temp, temp2) ; 
  } ;

  mat colnames estrace1 = `ind' gap_coef gap_se ind_coef ind_se ; 
  #d;



  * Version 2 ; 
  reg ln_hr_wage i.black##i.`ind' i.year age4 i.female i.edlevel [pw=earnwt], robust ; 

  * fill matrix with values of `ind', point estimates, and se ;
  levelsof `ind' if e(sample) == 1, local(levels) ; 
  cap mat drop estrace2 ;
  foreach s of local levels { ;
    qui lincom _b[1.black] +  _b[1.black#`s'.`ind'] ; 
    mat temp = (`s', `r(estimate)', `r(se)') ; 
	qui lincom _b[_cons] +  _b[`s'.`ind'] ; 
	mat temp2 = (`r(estimate)', `r(se)') ; 
    mat estrace2 = nullmat(estrace2) \ (temp, temp2) ; 
  } ;

  mat colnames estrace2 = `ind' gap_coef gap_se ind_coef ind_se ; 

* By gender ; 

  * Version 1 ; 
  reg ln_hr_wage i.female##i.`ind' i.year [pw=earnwt], robust ; 

  * fill matrix with values of `ind', point estimates, and se ;
  levelsof `ind' if e(sample) == 1, local(levels) ; 
  cap mat drop estgender1 ;
  foreach s of local levels { ;
    qui lincom _b[1.female] +  _b[1.female#`s'.`ind'] ; 
    mat temp = (`s', `r(estimate)', `r(se)') ;
	qui lincom _b[_cons] +  _b[`s'.`ind'] ; 
	mat temp2 = (`r(estimate)', `r(se)') ; 
    mat estgender1 = nullmat(estgender1) \ (temp, temp2) ; 
  } ;

  mat colnames estgender1 = `ind' gap_coef gap_se ind_coef ind_se ; 


  * Version 2 ; 
  reg ln_hr_wage i.female##i.`ind' i.year age4 i.black i.edlevel [pw=earnwt], robust ; 

  * fill matrix with values of `ind', point estimates, and se ;
  levelsof `ind' if e(sample) == 1, local(levels) ; 
  cap mat drop estgender2 ;
  foreach s of local levels { ;
    qui lincom _b[1.female] +  _b[1.female#`s'.`ind'] ; 
    mat temp = (`s', `r(estimate)', `r(se)') ; 
	qui lincom _b[_cons] +  _b[`s'.`ind'] ; 
	mat temp2 = (`r(estimate)', `r(se)') ; 
    mat estgender2 = nullmat(estgender2) \ (temp, temp2) ; 
  } ;

  mat colnames estgender2 = `ind' gap_coef gap_se ind_coef ind_se ; 


* Save datasets ;
#d ; 
clear ;
svmat estrace1, names(col) ;
save "${wrkdir}/`ind'_race_wage_gap_basic", replace ; 

clear ;
svmat estrace2, names(col) ;
save "${wrkdir}/`ind'_race_wage_gap", replace ; 

clear ;
svmat estgender1, names(col) ;
save "${wrkdir}/`ind'_gender_wage_gap_basic", replace ; 

clear ;
svmat estgender2, names(col) ;
save "${wrkdir}/`ind'_gender_wage_gap", replace ; 

} ; 


