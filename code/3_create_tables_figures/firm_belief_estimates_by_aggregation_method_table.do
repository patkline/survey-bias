/* -----------------------------------------------------------------------------------------------------------
Purpose: Appendix table of firm-level {pooled_favor_white, pooled_favor_male, conduct_favor_younger}
x {ols, borda} x {raw, empirical-Bayes-shrunk} estimates + each firm's industry
group, sorted by the race Borda EB estimate

Created: Nico Rotundo 2026-06-22
----------------------------------------------------------------------------------------------------------- */
* Run globals
do "${github}/survey-bias/code/globals.do"

/* -----------------------------------------------------------------------------------------------------------
Construct firm-industry bin crosswalk from microdata 
----------------------------------------------------------------------------------------------------------- */
* Import the long survey microdata
import delimited "${processed}/long_survey_final.csv", asdouble clear

* Uniquely identified by response x firm
gisid responseid firm_id

* Keep the firm identifier and its industry code and group name
keep firm_id aer_naics2 aer_naics2_name

* Check industry code constant within firm
gsort firm_id aer_naics2
by firm_id: assert aer_naics2[1] == aer_naics2[_N]

* Check industry group name constant within firm
gsort firm_id aer_naics2_name
by firm_id: assert aer_naics2_name[1] == aer_naics2_name[_N]

* Collapse to one row per firm
by firm_id: keep if _n == 1

* Should be unique on firm 
gisid firm_id

* Should not be missing industry bin name 
assert ~mi(aer_naics2_name)

* Save the firm -> industry crosswalk
tempfile firm_industry_crosswalk
save `firm_industry_crosswalk'

/* -----------------------------------------------------------------------------------------------------------
Load the firm-level raw and EB-shrunk belief estimates from the Coefficients sheet
----------------------------------------------------------------------------------------------------------- */
* Import the coefficient estimates
import parquet "${output}/intermediate/Full_Sample/Coefficients.parquet", clear

* Uniquely identified by subset x model x outcome x entity type x entity
gisid subset model outcome entity_type entity_id

* Keep the full-sample, firm-level rows for the two aggregation models and three beliefs
keep if inlist(subset, "all") & inlist(entity_type, "Firm") & inlist(model, "OLS", "Borda") & inlist(outcome, "pooled_favor_white", "pooled_favor_male", "conduct_favor_younger")

* Keep the firm identifier, model, belief, and the raw and EB-shrunk point estimates
keep entity_id entity model outcome estimate rse eb

* Order variables
order entity_id entity model outcome estimate rse eb

* Uniquely identified by firm x model x belief
gisid entity_id model outcome

/* -----------------------------------------------------------------------------------------------------------
Reshape to one row per firm, one column per belief x model
----------------------------------------------------------------------------------------------------------- */
* Combine belief and aggregation model into one column suffix (leading underscore so reshaped names read <value>_<belief>_<model>)
gen outcome_model = "_" + outcome + "_" + lower(model)

* Drop the now-redundant separate belief and model columns
drop outcome model

* Each firm has all six belief x model rows
gsort entity_id
by entity_id: assert _N == 6

* Rename estimate for character length considerations 
rename estimate raw

* Reshape wide to one row per firm, one column per belief x model for the raw estimate, its robust se, and the EB-shrunk estimate
reshape wide raw rse eb, i(entity_id entity) j(outcome_model) string

* Unique on firm 
gisid entity_id

* Should be 164 firms in the full sample
assert _N == 164

/* -----------------------------------------------------------------------------------------------------------
Merge each firm's industry group name from the survey microdata crosswalk
----------------------------------------------------------------------------------------------------------- */
* Rename "entity" to firm
rename (entity_id entity) (firm_id firm_name)

* Merge the industry group name onto each firm
merge 1:1 firm_id using `firm_industry_crosswalk', assert(3) keepusing(aer_naics2_name) nogen

* Rename industry group variable 
rename aer_naics2_name industry_group

* Order variables 
order firm_id firm_name industry_group ///
    raw_pooled_favor_white_ols rse_pooled_favor_white_ols eb_pooled_favor_white_ols ///
    raw_pooled_favor_white_borda rse_pooled_favor_white_borda eb_pooled_favor_white_borda ///
    raw_pooled_favor_male_ols rse_pooled_favor_male_ols eb_pooled_favor_male_ols ///
    raw_pooled_favor_male_borda rse_pooled_favor_male_borda eb_pooled_favor_male_borda ///
    raw_conduct_favor_younger_ols rse_conduct_favor_younger_ols eb_conduct_favor_younger_ols ///
    raw_conduct_favor_younger_borda rse_conduct_favor_younger_borda eb_conduct_favor_younger_borda ///

/* -----------------------------------------------------------------------------------------------------------
Sort firms by the race Borda EB estimate, as in Figure 7
----------------------------------------------------------------------------------------------------------- */
* Sort descending so the firms most believed to favor white applicants appear first
gsort -eb_pooled_favor_white_borda

/* -----------------------------------------------------------------------------------------------------------
Format the estimates and rename firms and industries for LaTex
----------------------------------------------------------------------------------------------------------- */
* Three-decimal display format for every estimate
global fmt %4.3f

* Remove characters from firm and industry names that LaTex does not like 
foreach name_variable of varlist firm_name industry_group {
    foreach special_character in "&" "%" "#" "_" {
        replace `name_variable' = subinstr(`name_variable', "`special_character'", char(92) + "`special_character'", .)
    }
    assert ~mi(`name_variable')
}

* Define local for the six belief x aggregation-model columns, in table order
local belief_models pooled_favor_white_ols pooled_favor_white_borda pooled_favor_male_ols pooled_favor_male_borda conduct_favor_younger_ols conduct_favor_younger_borda

/* -----------------------------------------------------------------------------------------------------------
Write the appendix table header
----------------------------------------------------------------------------------------------------------- */
* Open the .tex file
cap texdoc close
texdoc init "${tables}/firm_belief_estimates_by_aggregation_method_table.tex", replace force

* Open the longtable; the caption and label text are defined as macros in the paper so they can be edited without regenerating this file
tex \begin{longtable}{ll *{12}{c}}
tex \caption{\firmBeliefTableCaption}\label{\firmBeliefTableLabel}\\
tex \toprule

* Belief-group header, with the underlying belief variable named beneath each group
tex & & \multicolumn{4}{c}{Race} & \multicolumn{4}{c}{Gender} & \multicolumn{4}{c}{Age} \\
tex & & \multicolumn{4}{c}{\texttt{pooled\_favor\_white}} & \multicolumn{4}{c}{\texttt{pooled\_favor\_male}} & \multicolumn{4}{c}{\texttt{conduct\_favor\_younger}} \\
tex \cmidrule(lr){3-6} \cmidrule(lr){7-10} \cmidrule(lr){11-14}

* Aggregation-model header within each belief block
tex & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} \\
tex \cmidrule(lr){3-4} \cmidrule(lr){5-6} \cmidrule(lr){7-8} \cmidrule(lr){9-10} \cmidrule(lr){11-12} \cmidrule(lr){13-14}

* Raw / EB header, plus the firm and industry-group titles
tex Firm & Industry Group & Raw & EB & Raw & EB & Raw & EB & Raw & EB & Raw & EB & Raw & EB \\
tex \midrule
tex \endhead

* Loop over firms in the sorted order
forvalues firm = 1/`=_N' {

    * One row per firm: firm, industry group, then for each belief x model the raw estimate with its robust standard error in parentheses, and the EB estimate beside it
    local estimate_row "`=firm_name[`firm']' & `=industry_group[`firm']'"
    foreach belief_model in `belief_models' {
        local estimate_row "`estimate_row' & `: display ${fmt} raw_`belief_model'[`firm']' (`: display ${fmt} rse_`belief_model'[`firm']') & `: display ${fmt} eb_`belief_model'[`firm']'"
    }
    tex `estimate_row' \\
}

* Bottom rule and close the longtable
tex \bottomrule
tex \end{longtable}

* Close the .tex file
texdoc close
