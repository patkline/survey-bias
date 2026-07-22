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

* Should be 6515 respondents x 5 firm slots
assert _N == 32575

* Count distinct respondents
gdistinct responseid

* Should be 6515 distinct respondents
assert r(ndistinct) == 6515

* Sort by firm
gsort firm_id

* Count each firm's respondents i.e., those with the firm in their choice set
by firm_id: gen n_choice_set_respondents = _N

* Keep the firm identifier, its industry code and group name, and its choice-set respondent count
keep firm_id aer_naics2 aer_naics2_name n_choice_set_respondents

* Sort by firm and industry code
gsort firm_id aer_naics2

* Check industry code constant within firm
by firm_id: assert aer_naics2[1] == aer_naics2[_N]

* Sort by firm and industry group name
gsort firm_id aer_naics2_name

* Check industry group name constant within firm
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
Compute each firm's don't-know share per displayed belief measure from the summary-stats microdata
----------------------------------------------------------------------------------------------------------- */
* Import the summary-stats survey microdata; the -1 don't-know/prefer-not-to-answer codes are preserved in this export
import delimited "${processed}/long_survey_final_summary_stats.csv", asdouble clear

* Uniquely identified by response x firm
gisid responseid firm_id

* Should be 6515 respondents x 5 firm slots
assert _N == 32575

* Count distinct respondents
gdistinct responseid

* Should be 6515 distinct respondents
assert r(ndistinct) == 6515

* Firm slots numbered 1-5
assert inlist(option_number, 1, 2, 3, 4, 5)

* Response-level arm questions behind each displayed belief measure; the favor composites drop the first-listed arm's -1s, so the don't-know codes must come from the arm variables
local parts_pooled_favor_white firmcont_white firmcont_black conduct_black
local parts_pooled_favor_male firmcont_male firmcont_female conduct_female conduct_male
local parts_conduct_favor_younger conduct_older conduct_younger

* R writes missing cells as the literal text NA, so the arm columns import as string; recode to numeric missing
foreach component in firmcont_white firmcont_black conduct_black firmcont_male firmcont_female conduct_female conduct_male conduct_older conduct_younger {

    * Recode the NA text to empty i.e., numeric missing after destring
    replace `component' = "" if inlist(`component', "NA")

    * Convert to numeric
    destring `component', replace
}

* Loop over the three displayed belief measures
foreach belief in pooled_favor_white pooled_favor_male conduct_favor_younger {

    * Count the response's non-missing component arm answers
    egen byte n_parts_`belief' = rownonmiss(`parts_`belief'')

    * Arms are mutually exclusive, so each respondent answered at most one arm question per measure
    assert inlist(n_parts_`belief', 0, 1)

    * Drop the component-count helper
    drop n_parts_`belief'
}

* Loop over the three displayed belief measures
foreach belief in pooled_favor_white pooled_favor_male conduct_favor_younger {

    * Initialize the asked-a-component-question flag
    gen byte asked_`belief' = 0

    * Initialize the answered-don't-know flag
    gen byte dk_`belief' = 0

    * Loop over the measure's component arm questions
    foreach component of local parts_`belief' {

        * Component ratings take only the 1-5 scale values, the -1 don't-know code, or missing when not asked
        assert inlist(`component', -1, 1, 2, 3, 4, 5) | mi(`component')

        * Mark the response as asked if the component carries any answer code
        replace asked_`belief' = 1 if inlist(`component', -1, 1, 2, 3, 4, 5)

        * Mark the response as don't know iff the answer is -1
        replace dk_`belief' = 1 if inlist(`component', -1)
    }
}

* Sum asked and don't-know responses within firm
gcollapse (sum) asked_* dk_*, by(firm_id)

* Every firm has asked responses for every measure
foreach belief in pooled_favor_white pooled_favor_male conduct_favor_younger {
    assert asked_`belief' > 0
}

* Share of asked responses answered don't know, per measure
foreach belief in pooled_favor_white pooled_favor_male conduct_favor_younger {

    * Divide don't-know responses by asked responses
    gen double dk_share_`belief' = dk_`belief' / asked_`belief'

    * Shares bounded between 0 and 1
    assert inrange(dk_share_`belief', 0, 1)
}

* Keep the firm identifier and its don't-know shares
keep firm_id dk_share_*

* Save the firm -> don't-know share file
tempfile firm_dk_shares
save `firm_dk_shares'

/* -----------------------------------------------------------------------------------------------------------
Load the firm-level raw and EB-shrunk belief estimates from the Coefficients sheet
----------------------------------------------------------------------------------------------------------- */
* Import the coefficient estimates
import parquet "${output}/intermediate/Full_Sample/Coefficients.parquet", clear

* Uniquely identified by subset x model x outcome x entity type x entity
gisid subset model outcome entity_type entity_id

* Keep the full-sample, firm-level rows for the two raw-scale (not recentered) aggregation models and three beliefs
keep if inlist(subset, "all") & inlist(entity_type, "Firm") & inlist(model, "OLS_not_recentered", "Borda_not_recentered") & inlist(outcome, "pooled_favor_white", "pooled_favor_male", "conduct_favor_younger")

* Shorten the model names so the reshaped column suffixes keep the ols/borda names
replace model = "OLS" if inlist(model, "OLS_not_recentered")
replace model = "Borda" if inlist(model, "Borda_not_recentered")

* Model takes only the shortened names
assert inlist(model, "OLS", "Borda")

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

* Sort by firm
gsort entity_id

* Each firm has all six belief x model rows
by entity_id: assert _N == 6

* Rename estimate to raw so the reshaped names fit Stata's 32-character variable-name limit
rename estimate raw

* Reshape wide to one row per firm, one column per belief x model for the raw estimate, its robust se, and the EB-shrunk estimate
reshape wide raw rse eb, i(entity_id entity) j(outcome_model) string

* Unique on firm 
gisid entity_id

* Should be 164 firms in the full sample
assert _N == 164

* Every printed estimate cell is non-missing
foreach belief in pooled_favor_white pooled_favor_male conduct_favor_younger {
    foreach model in ols borda {
        assert ~mi(raw_`belief'_`model') & ~mi(rse_`belief'_`model') & ~mi(eb_`belief'_`model')
    }
}

/* -----------------------------------------------------------------------------------------------------------
Merge each firm's industry group name from the survey microdata crosswalk
----------------------------------------------------------------------------------------------------------- */
* Rename the entity columns to firm_id and firm_name
rename (entity_id entity) (firm_id firm_name)

* Merge the industry group name and choice-set respondent count onto each firm
merge 1:1 firm_id using `firm_industry_crosswalk', assert(3) keepusing(aer_naics2_name n_choice_set_respondents) nogen

* Merge the don't-know shares onto each firm
merge 1:1 firm_id using `firm_dk_shares', assert(3) nogen

* Rename industry group variable 
rename aer_naics2_name industry_group

* Order variables
order firm_id firm_name industry_group n_choice_set_respondents ///
    raw_pooled_favor_white_ols rse_pooled_favor_white_ols eb_pooled_favor_white_ols ///
    raw_pooled_favor_white_borda rse_pooled_favor_white_borda eb_pooled_favor_white_borda ///
    raw_pooled_favor_male_ols rse_pooled_favor_male_ols eb_pooled_favor_male_ols ///
    raw_pooled_favor_male_borda rse_pooled_favor_male_borda eb_pooled_favor_male_borda ///
    raw_conduct_favor_younger_ols rse_conduct_favor_younger_ols eb_conduct_favor_younger_ols ///
    raw_conduct_favor_younger_borda rse_conduct_favor_younger_borda eb_conduct_favor_younger_borda

/* -----------------------------------------------------------------------------------------------------------
Sort firms by the race Borda EB estimate, as in Figure 7
----------------------------------------------------------------------------------------------------------- */
* Sort descending so the firms most believed to favor white applicants appear first
gsort -eb_pooled_favor_white_borda

/* -----------------------------------------------------------------------------------------------------------
Format the estimates and rename firms and industries for LaTeX
----------------------------------------------------------------------------------------------------------- */
* Three-decimal display format for every estimate
global fmt %4.3f

* Escape LaTeX special characters in the firm and industry names
foreach name_variable of varlist firm_name industry_group {

    * Loop over the LaTeX special characters
    foreach special_character in "&" "%" "#" "_" {

        * Prefix the character with a backslash
        replace `name_variable' = subinstr(`name_variable', "`special_character'", char(92) + "`special_character'", .)
    }

    * Name never missing after escaping
    assert ~mi(`name_variable')
}

* Define local for the three belief blocks, in table order
local beliefs pooled_favor_white pooled_favor_male conduct_favor_younger

/* -----------------------------------------------------------------------------------------------------------
Write the appendix table header
----------------------------------------------------------------------------------------------------------- */
* Close any dangling texdoc handle from a prior failed run
cap texdoc close

* Open the .tex file
texdoc init "${tables}/firm_belief_estimates_by_aggregation_method_table.tex", replace force

* Open the longtable; the caption and label text are defined as macros in the paper so they can be edited without regenerating this file
tex \begin{longtable}{ll c *{15}{c}}
tex \caption{\firmBeliefTableCaption}\label{\firmBeliefTableLabel}\\
tex \toprule

* Belief-group header, with the measure's display label from the other exhibits beneath each group
tex & & & \multicolumn{5}{c}{Race} & \multicolumn{5}{c}{Gender} & \multicolumn{5}{c}{Age} \\
tex & & & \multicolumn{5}{c}{Discrimination Black (Pooled)} & \multicolumn{5}{c}{Discrimination Female (Pooled)} & \multicolumn{5}{c}{Discrimination Older (Conduct)} \\
tex \cmidrule(lr){4-8} \cmidrule(lr){9-13} \cmidrule(lr){14-18}

* Aggregation-model header within each belief block; the trailing blank column in each block is its don't-know share
tex & & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & \\
tex \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){9-10} \cmidrule(lr){11-12} \cmidrule(lr){14-15} \cmidrule(lr){16-17}

* Raw / EB / don't-know-share header, plus the firm, industry-group, and choice-set-count titles
tex Firm & Industry Group & \shortstack{Respondents\\with Firm in\\Choice Set} & Raw & EB & Raw & EB & \shortstack{DK\\Share} & Raw & EB & Raw & EB & \shortstack{DK\\Share} & Raw & EB & Raw & EB & \shortstack{DK\\Share} \\
tex \midrule
tex \endfirsthead

* Continuation-page header: same rows without the caption and label, so multipage output does not repeat the table number
tex \toprule
tex & & & \multicolumn{5}{c}{Race} & \multicolumn{5}{c}{Gender} & \multicolumn{5}{c}{Age} \\
tex & & & \multicolumn{5}{c}{Discrimination Black (Pooled)} & \multicolumn{5}{c}{Discrimination Female (Pooled)} & \multicolumn{5}{c}{Discrimination Older (Conduct)} \\
tex \cmidrule(lr){4-8} \cmidrule(lr){9-13} \cmidrule(lr){14-18}
tex & & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & & \multicolumn{2}{c}{Likert} & \multicolumn{2}{c}{Borda} & \\
tex \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){9-10} \cmidrule(lr){11-12} \cmidrule(lr){14-15} \cmidrule(lr){16-17}
tex Firm & Industry Group & \shortstack{Respondents\\with Firm in\\Choice Set} & Raw & EB & Raw & EB & \shortstack{DK\\Share} & Raw & EB & Raw & EB & \shortstack{DK\\Share} & Raw & EB & Raw & EB & \shortstack{DK\\Share} \\
tex \midrule
tex \endhead

* Loop over firms in the sorted order
forvalues firm = 1/`=_N' {

    * Start the row with the firm name, industry group, and choice-set respondent count
    local estimate_row "`=firm_name[`firm']' & `=industry_group[`firm']' & `=n_choice_set_respondents[`firm']'"

    * Loop over the three belief blocks in table order
    foreach belief in `beliefs' {

        * Append the raw estimate with its robust standard error in parentheses and the EB estimate, per aggregation model
        foreach model in ols borda {
            local estimate_row "`estimate_row' & `: display ${fmt} raw_`belief'_`model'[`firm']' (`: display ${fmt} rse_`belief'_`model'[`firm']') & `: display ${fmt} eb_`belief'_`model'[`firm']'"
        }

        * Append the block's don't-know share
        local estimate_row "`estimate_row' & `: display ${fmt} dk_share_`belief'[`firm']'"
    }

    * Write the firm's table row
    tex `estimate_row' \\
}

* Bottom rule and close the longtable
tex \bottomrule
tex \end{longtable}

* Close the .tex file
texdoc close
