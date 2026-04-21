/* ---------------------------------------------------------------------------------------------
Purpose: Define Stata globals (paths) and settings for the survey-bias project

Created: Nico Rotundo 2026-04-20
----------------------------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------------------------
Stata settings
----------------------------------------------------------------------------------------------*/
* Disable pagination of output
set more off

* Disable variable-name abbreviation
set varabbrev off

/* ---------------------------------------------------------------------------------------------
User detection and Dropbox root
----------------------------------------------------------------------------------------------*/
* Detect current system user
local user = lower("`c(username)'")

* Map system user --> Dropbox consolidated_code root
if "`user'" == "nicorotundo" {
    global dropbox_survey_bias_root "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code"
}
else if "`user'" == "monicahea" {
    global dropbox_survey_bias_root "/Users/monicahea/Dropbox/Survey/consolidated_code"
}
else if "`user'" == "jordancammarota" {
    global dropbox_survey_bias_root "/Users/jordancammarota/Dropbox/consolidated_code"
}
else {
    di as error "🧌 No Dropbox path configured for user: `user'"
    exit 198
}

/* ---------------------------------------------------------------------------------------------
GitHub root
----------------------------------------------------------------------------------------------*/
* Map system user --> GitHub survey-bias repo root (mirrors Dropbox mapping above)
if "`user'" == "nicorotundo" {
    global git_survey_bias_root "/Users/nicorotundo/GitHub/survey-bias"
}
//else if "`user'" == "monicahea" {
//    * XX NR: confirm GitHub path with Monica
//    global git_survey_bias_root "/Users/monicahea/GitHub/survey-bias"
//}
//else if "`user'" == "jordancammarota" {
//    * XX NR: confirm GitHub path with Jordan
//    global git_survey_bias_root "/Users/jordancammarota/GitHub/survey-bias"
//}
else {
    di as error "🧌 No GitHub path configured for user: `user'"
    exit 198
}

/* ---------------------------------------------------------------------------------------------
Data and output storage location switch
----------------------------------------------------------------------------------------------*/
* Select storage location for data and output --- options are "github" and "dropbox"
global data_and_output_storage_location "dropbox"

* Set data and output paths based on storage switch
if "${data_and_output_storage_location}" == "github" {
    global data "${git_survey_bias_root}/data"
    global output "${git_survey_bias_root}/output"
}
else if "${data_and_output_storage_location}" == "dropbox" {
    global db_data_output_mirror "${dropbox_survey_bias_root}/github_data_and_output_mirrors"
    global data "${db_data_output_mirror}/data"
    global output "${db_data_output_mirror}/output"
}
else {
    di as error "🧌 Invalid value for data_and_output_storage_location. Must be 'github' or 'dropbox'"
    exit 198
}

/* ---------------------------------------------------------------------------------------------
Code paths
----------------------------------------------------------------------------------------------*/
* GitHub code root and subdirectories
global code "${git_survey_bias_root}/code"
global build "${code}/data_build"
global analysis "${code}/analysis"
global create_tables_figures "${code}/create_tables_figures"
global helper_functions "${code}/helper_functions"

/* ---------------------------------------------------------------------------------------------
Data paths
----------------------------------------------------------------------------------------------*/
* Data subdirectories (data root set above by storage switch)
global raw "${data}/raw"
global processed "${data}/processed"
global external "${data}/external"
global dump "${data}/dump"

/* ---------------------------------------------------------------------------------------------
Output paths
----------------------------------------------------------------------------------------------*/
* Output subdirectories (output root set above by storage switch)
global excel "${output}/excel"
global figures "${output}/figures"
global tables "${output}/tables"

/* ---------------------------------------------------------------------------------------------
Required user-written packages
----------------------------------------------------------------------------------------------*/
* Check if gtools is installed (cap qui to suppress install-path print on success)
cap qui which gtools

* Install from SSC if missing (provides gcollapse, gisid, and other performance-oriented commands)
if _rc {
    di as text "🎃 Installing gtools from SSC"
    ssc install gtools
}
