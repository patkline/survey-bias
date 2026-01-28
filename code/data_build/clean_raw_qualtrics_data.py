# ------------------------------------------------------------------------------
# Purpose: Clean raw Qualtrics data
#
# Created: Evan Rose 
# Edited: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Import packages and path globals
# ------------------------------------------------------------------------------

# Import packages
import numpy as np
import pandas as pd
import re
import sys
from pathlib import Path

# Add code directory to Python path to import globals module
sys.path.insert(0, str(Path(__file__).parent.parent))

# Import path globals
from globals import raw, processed, external, dump

#XXremove these once I replace the paths below 
#project_dir = "~/Documents/consolidated_code_server"
#path_to_raw = project_dir + "/raw"
#path_to_data = project_dir + "/data"
#path_to_processed =  project_dir + "/processed"
#path_to_external =  project_dir + "/external"
#path_to_dump =  project_dir + "/dump"

# ------------------------------------------------------------------------------
# Adjust python settings
# ------------------------------------------------------------------------------

#warnings.simplefilter("ignore") XX

# Show up to 200 columns when printing dataframes
pd.options.display.max_columns = 200

# Show up to 1000 rows when printing dataframes
pd.options.display.max_rows = 1000

# Show up to 200 columns when using df.info()
pd.set_option('max_info_columns', 200)

# Don't wrap dataframe display across multiple lines (contradicted by next line) XX
#pd.set_option('expand_frame_repr', False)

# Do wrap dataframe display across multiple lines (overrides previous line)
pd.set_option('expand_frame_repr', True)

# Allow up to 1000 characters per column when displaying
pd.set_option('max_colwidth',1000)

# No width limit for display
pd.set_option('display.width',None)

# Format floating point numbers to 3 decimal places
pd.set_option('display.float_format', lambda x: '%.3f' % x)

# ------------------------------------------------------------------------------
# Import and clean raw survey data
# ------------------------------------------------------------------------------

# Load the probability samples
df = pd.read_csv(raw / 'prob/RR_Qualtrics_September 5, 2023_10.16.csv')

# The first rows contains the actual question, save it in a separate object
questions = df.iloc[0]

# Confirm that the first two rows are not actual observations
assert df.iloc[0]['Status'] != 'IP Address', "First row should be metadata, not an observation"
assert df.iloc[1]['Status'] != 'IP Address', "Second row should be metadata, not an observation"

# Confirm that all rows aside from the first two are actual observations
assert (df.iloc[2:]['Status'] == 'IP Address').all(), "All rows aside from the first two should be observations"

# Remove the first two rows
df = df.iloc[2:]

# Add the "sample" column with value 1 for the "prob" sample
df['sample'] = 1

# Load the convenience sample
df2 = pd.read_csv(raw / 'conv/RR_Qualtrics_October 5, 2023_15.10.csv')

# Confirm that the first three rows are not actual observations
assert df2.iloc[0]['Status'] != 'IP Address', "First row should be metadata, not an observation"
assert df2.iloc[1]['Status'] != 'IP Address', "Second row should be metadata, not an observation"
assert df2.iloc[2]['Status'] != 'IP Address', "Third row should be metadata, not an observation"

# Confirm that all rows aside from the first three are actual observations
assert (df2.iloc[3:]['Status'] == 'IP Address').all(), "All rows aside from the first three should be observations"

# Remove the first three rows (not actual observations)
df2 = df2.iloc[3:]

# Add the "sample" column with value 0 for the "conv" sample
df2['sample'] = 0

# ------------------------------------------------------------------------------
# Merge and append raw survey datasets together
# ------------------------------------------------------------------------------

# Append the convenience sample to the probability sample
df = pd.concat([df, df2])

# Store missing demographic information in a Python object 
dem = pd.read_csv(raw / 'RR data append.csv')

# Merge the appended missing demographic info to the main dataframe on response ID
df = df.merge(dem, on='ResponseId', how='outer', validate='1:1', indicator=True, suffixes=("", "_missdem"))

# Check that there are no ResponseIds in dem that are not in df
assert df._merge.value_counts()['right_only'] == 0

## Replace values of demographic variables in df dataframe with those in dem where available
# Loop through variables in dem dataframe 
for col in dem.columns:
    
    # Skip ResponseId column
    if col == 'ResponseId':
        continue

    # For age observations that exist in both datasets, replace value in df dataframe with those in dem dataframe, and drop the dem age variable 
    elif col == 'Q110':
        df.loc[df._merge == 'both','Q110'] = df.loc[df._merge == 'both','Q110_missdem']
        df.drop(columns='Q110_missdem', inplace=True)
    
    # For all other variables in the dem dataframe and for all observations that exist in both datasets, replace value in df dataframe with those in dem dataframe, and drop the dem variables
    
    # XXfor cases where both datasets have non-missing values, should check whether we want to replace the original value in the df dataframe with the value in the dem dataframe, since there are cases where they are different
    else:
        #XX assert (df.loc[df['*Q111'].notna() & df['Q111'].notna(), '*Q111'] == df.loc[df['*Q111'].notna() & df['Q111'].notna(), 'Q111']).all()
        df.loc[df._merge == 'both',f'*{col}'] = df.loc[df._merge == 'both', f'{col}']
        df.drop(columns =col, inplace=True)

# Drop the merge indicator column
df.drop(columns ='_merge', inplace = True)

# Store new survey round data in a Python object
df_app = pd.read_csv(raw / 'RR_Qualtrics_February 5, 2024_11.05.csv')

# Confirm that the first two rows are not actual observations
assert df_app.iloc[0]['Status'] != 'IP Address', "First row should be metadata, not an observation"
assert df_app.iloc[1]['Status'] != 'IP Address', "Second row should be metadata, not an observation"

# Remove observations where status variable is "Response Type", "{"ImportId":"status"}", "Survey Preview", or "Spam"
# XXCheck that I should be doing this --- checked from observation that this removes the 10 obsesrvations, but this code is a bit too ad-hoc 
df_app = df_app.loc[~df_app['Status'].isin(['Response Type', '{"ImportId":"status"}', 'Survey Preview', 'Spam'])]

# Remove the first two rows --- XXthis strikes me as just having been copied from the code that cleans the original df dataframe, but I think the new data has a different structure such that the index-based removal is not appropriate
#df_app = df_app.iloc[2:]

# Confirm that all remaining rows are actual observations
assert (df_app.iloc[2:]['Status'] == 'IP Address').all(), "All rows aside from the first two should be observations"

# Define "sample" variable with value 1 for the "prob" sample and 0 for the "conv" sample
df_app['sample'] = df_app['S'].apply(lambda x: 1 if x == "prob" else 0)

# Check how many ResponseIds in df_app are also in df
# 9189 responses exist in both datasets; 1071 are new responses in df_app
print(df_app.ResponseId.isin(df.ResponseId).value_counts())

# Check how many responses in df are also in df_app
# 9189 responses exist in both datasets
print(df.ResponseId.isin(df_app.ResponseId).value_counts())

#Note. Preceding two commands indicate that the new survey round is a superset of the prior one 

# Define new dataframe containing only new responses in df_app that were recorded before the chronological last date in the original df dataframe 
# XXWhat does excluded mean here?
# XXShould we be doing anything with these excluded observations? Looks like this variable is not used at all after this point
excluded = df_app.loc[~df_app.ResponseId.isin(df.ResponseId) 
    & (df_app.StartDate <= df.StartDate.max())]

# Keep only observations in the df_app dataframe that were recorded after the last date in the original df dataframe
#XXonly 44 observations get kept here; is that what we want?
df_app = df_app.loc[df_app.StartDate > df.StartDate.max()]
  
# Append the 44 new observations in df_app to the original df dataframe
df = pd.concat([df, df_app], ignore_index=True)

# Check that the dataset is unique on ResponseId
assert df.ResponseId.nunique() == df.shape[0]

# Remove preview answers
#XXdo not need this anymore since I removed preview answers above
#df = df.loc[df.DistributionChannel != 'preview']

# Check that distribution channel is never preview 
assert (df.DistributionChannel != 'preview').all()

# ------------------------------------------------------------------------------
# Rename and recode variables
# XXstopped checking carefully here --- should go back and do so
# ------------------------------------------------------------------------------

# Rename question columns
df.rename(columns={
            'race': 'race_metadata',
            '*Q109': 'gender', 
            'Q110': 'age',
            '*Q111': 'zipcode',
            '*Q112': 'hispanic',
            '*Q113': 'race',
            '*Q114': 'income', 
            '*Q115': 'married',
            '*Q116': 'educ',
            '*Q117': 'empstat',
            'Q108': 'looking_job',
            'Q78': 'party_affil',
            'Q121': 'confidence_race_names',
            'Q122': 'confidence_gend_names',
            'Q123': 'confidence_gend_conduct',
            'Q124': 'confidence_race_conduct',
            'Q250': 'confidence_age_conduct',
            'experience': 'any_entry_lev_exp',
            'Q76': 'information_source',
            'Q75': 'feared_discrim',
            'Q120': 'attention_check',
                     }, inplace = True)

for var, name in [
            ('Q88','name_contact'),
            ('Q210','conduct_female'),
            ('Q127','conduct_male'),
            ('Q248','conduct_older'),
            ('Q131','conduct_younger'),
            ('Q192','discretion')]:
    for k in range(1,6):
        df.rename(columns={f"{var}_{k}":f"{name}_{k}"}, inplace=True)

for var in ['firm','name']:
    for k in range(1,6):
        df.rename(columns={f"{var}{k}":f"{var}_{k}"}, inplace=True)

# Map observations with anonymous distribution channel to convenience sample 
df.loc[df.DistributionChannel == 'anonymous', 'sample_type'] = 'conv'

# Map observations with gl distribution channel to prob sample
df.loc[df.DistributionChannel == 'gl', 'sample_type'] = 'prob'

# Check that there are no missing values in sample_type
assert df.sample_type.isnull().sum() == 0

# Check that there are no missing values in the sample variable
assert df['sample'].isnull().sum() == 0

# ------------------------------------------------------------------------------
# Generate long version of conduct and name questions, then 
# merge together  
# ------------------------------------------------------------------------------

###  Generate long version of conduct questions
# Define list of the 18 firm evaluation/conduct questions
tokeep = [  'firm',
            'FirmSelective',
            'FirmDesire',
            'FirmContRace_wfirst0',
            'FirmContRace_wfirst1',
            'FirmHireRace_wfirst0',
            'FirmHireRace_wfirst1',
            'FirmContGend_mfirst0',
            'FirmContGend_mfirst1',
            'FirmHireGend_mfirst0',
            'FirmHireGend_mfirst1',
            'conduct_black',
            'conduct_white',
            'conduct_female',
            'conduct_male',
            'conduct_older',
            'conduct_younger',
            'discretion',
            ]

# Create dataframe with only the unique response id + the sample + 18 base firm variables x 5 firms = 96 total firm variables 
dflong = df[['ResponseId','sample'] +
    [c + f"_{k}" for c in tokeep for k in range(1,6)]].copy()

# Reshape dataframe to long format, with one row per respondent-firm number combination
dflong = pd.wide_to_long(dflong, tokeep, 
            i='ResponseId', j='option_number', suffix=r'_([0-5])').reset_index()

# Convert option_number from string to integer
dflong['option_number'] = dflong.option_number.apply(lambda x: int(x[1:]))

###  Generate long version of name questions
# Define list of the 4 firm evaluation/conduct questions
tokeep = [  'name',
            'name_contact',
            'NameRace_wfirst0',
            'NameRace_wfirst1'
            ]

# Create dataframe with only the unique response id + 4 base name variables x 5 names = 20 total name variables 
nameslong = df[['ResponseId'] +
    [c + f"_{k}" for c in tokeep for k in range(1,6)]].copy()

# Reshape dataframe to long format, with one row per respondent-name number combination
nameslong = pd.wide_to_long(nameslong, tokeep, 
            i='ResponseId', j='option_number', suffix=r'_([0-5])').reset_index()

# Convert option_number from string to integer
nameslong['option_number'] = nameslong.option_number.apply(lambda x: int(x[1:]))

# Merge long conduct and name dataframes together
dflong = dflong.merge(nameslong, how='left',
        on=['ResponseId','option_number'], validate='1:1')

# ------------------------------------------------------------------------------
# Recode responses in the long dataframe 
# ------------------------------------------------------------------------------

# Recode responses (small number is always more likely)
replace_dict = {    'Very likely': 1,
                    'Somewhat likely': 2,
                    'Neither likely nor unlikely': 3,
                    'Somewhat unlikely': 4,
                    'Very unlikely': 5,

                    'Much more likely': 1,
                    'Somewhat more likely': 2,
                    'Equally likely\nto contact\nboth': 3,
                    'Equally likely\nto hire\nboth': 3,
                    'Somewhat less likely': 4,
                    'Much less likely': 5,

                    '1\nMost likely': 1, 
                    '2\n ': 2, 
                    '3\n ': 3,
                    '4\n ': 4, 
                    '5\nLeast likely\n': 5,

                    'Equally likely': 3,

                    "Don't know/ prefer not to answer": -1,
                }
for var in ['FirmSelective','FirmDesire',
            'FirmContRace_wfirst0','FirmContRace_wfirst1',
            'FirmHireRace_wfirst0','FirmHireRace_wfirst1',
            'FirmContGend_mfirst0','FirmContGend_mfirst1',
            'FirmHireGend_mfirst0','FirmHireGend_mfirst1',
            'conduct_white','conduct_black',
            'conduct_female','conduct_male',
            'conduct_older','conduct_younger',
            'discretion',
            'name_contact',
            'NameRace_wfirst0','NameRace_wfirst1',
             ]:
    dflong[var] = dflong[var].replace(replace_dict)
    dflong[var] = pd.to_numeric(dflong[var])
    print(dflong[var].value_counts())

# ------------------------------------------------------------------
# Flip FirmSelective (only 1..5; preserve NA and -1)
# ------------------------------------------------------------------
mask = dflong['FirmSelective'].isin([1,2,3,4,5])
dflong.loc[mask, 'FirmSelective'] = 6 - dflong.loc[mask, 'FirmSelective']


# ------------------------------------------------------------------
# White / Black favor variables
# ------------------------------------------------------------------
dflong['FirmCont_favor_white'] = np.where(
    dflong['FirmContRace_wfirst1'].isin([1,2,3,4,5]),
    dflong['FirmContRace_wfirst1'],
    np.where(
        dflong['FirmContRace_wfirst0'].isin([1,2,3,4,5]),
        6 - dflong['FirmContRace_wfirst0'],
        dflong['FirmContRace_wfirst0']  # preserves -1 or NA
    )
)

dflong['FirmHire_favor_white'] = np.where(
    dflong['FirmHireRace_wfirst1'].isin([1,2,3,4,5]),
    dflong['FirmHireRace_wfirst1'],
    np.where(
        dflong['FirmHireRace_wfirst0'].isin([1,2,3,4,5]),
        6 - dflong['FirmHireRace_wfirst0'],
        dflong['FirmHireRace_wfirst0']
    )
)


# ------------------------------------------------------------------
# Male / Female favor variables
# ------------------------------------------------------------------
dflong['FirmCont_favor_male'] = np.where(
    dflong['FirmContGend_mfirst1'].isin([1,2,3,4,5]),
    dflong['FirmContGend_mfirst1'],
    np.where(
        dflong['FirmContGend_mfirst0'].isin([1,2,3,4,5]),
        6 - dflong['FirmContGend_mfirst0'],
        dflong['FirmContGend_mfirst0']
    )
)

dflong['FirmHire_favor_male'] = np.where(
    dflong['FirmHireGend_mfirst1'].isin([1,2,3,4,5]),
    dflong['FirmHireGend_mfirst1'],
    np.where(
        dflong['FirmHireGend_mfirst0'].isin([1,2,3,4,5]),
        6 - dflong['FirmHireGend_mfirst0'],
        dflong['FirmHireGend_mfirst0']
    )
)


# ------------------------------------------------------------------
# Conduct variables
# ------------------------------------------------------------------
dflong['conduct_favor_white'] = np.where(
    dflong['conduct_black'].isin([1,2,3,4,5]),
    dflong['conduct_black'],
    np.where(
        dflong['conduct_white'].isin([1,2,3,4,5]),
        6 - dflong['conduct_white'],
        dflong['conduct_white']
    )
)

dflong['conduct_favor_male'] = np.where(
    dflong['conduct_female'].isin([1,2,3,4,5]),
    dflong['conduct_female'],
    np.where(
        dflong['conduct_male'].isin([1,2,3,4,5]),
        6 - dflong['conduct_male'],
        dflong['conduct_male']
    )
)

dflong['conduct_favor_younger'] = np.where(
    dflong['conduct_older'].isin([1,2,3,4,5]),
    dflong['conduct_older'],
    np.where(
        dflong['conduct_younger'].isin([1,2,3,4,5]),
        6 - dflong['conduct_younger'],
        dflong['conduct_younger']
    )
)


# ------------------------------------------------------------------
# Name race variable
# ------------------------------------------------------------------
dflong['name_likely_white'] = np.where(
    dflong['NameRace_wfirst1'].isin([1,2,3,4,5]),
    dflong['NameRace_wfirst1'],
    np.where(
        dflong['NameRace_wfirst0'].isin([1,2,3,4,5]),
        6 - dflong['NameRace_wfirst0'],
        dflong['NameRace_wfirst0']
    )
)

# ------------------------------------------------------------------------------
# Clean and merge experimental datasets together, and merge 
# onto long data 
# ------------------------------------------------------------------------------

# Store various experimental estimates in Python objects
exp_ev = pd.read_csv(external / 'theta_estimates_wjobs_v7.csv')
exp_ev_g = pd.read_csv(external / 'theta_estimates_wjobs_v7gender.csv')
exp_ev_40 = pd.read_csv(external / 'theta_estimates_wjobs_v7over40.csv')
exp_ev_ranking = pd.read_csv(external / 'theta_estimates_wjobs_v7_ranking.csv')
exp_cb_central = pd.read_csv(external / 'centralization.csv')

#XX looks like this is created from a dofile? if so, just making a note to include this further upsteam in the /data_build/metafile.R and edit the data/export filepaths 
exp_cb_central_se = pd.read_csv(external / 'centralization_w_se.csv')

# Rename variables in the exp_ev_g dataframe 
exp_ev_g['dif_gender'] = exp_ev_g.dif
exp_ev_g['log_dif_gender'] = exp_ev_g.log_dif
exp_ev_g['dif_se_gender'] = exp_ev_g.dif_se
exp_ev_g['log_dif_se_gender'] = exp_ev_g.log_dif_se

# Rename variables in the exp_ev_40 dataframe
exp_ev_40['dif_age'] = exp_ev_40.dif
exp_ev_40['log_dif_age'] = exp_ev_40.log_dif
exp_ev_40['dif_se_age'] = exp_ev_40.dif_se
exp_ev_40['log_dif_se_age'] = exp_ev_40.log_dif_se

# Rename variables in the exp_cb_central dataframe
exp_cb_central['cb_central_full'] = exp_cb_central.cb_central
exp_cb_central_se['cb_central_full_se'] = exp_cb_central_se.cb_central_se

# Merge secondary dataframes onto the main exp_ev dataframe
exp_ev = exp_ev.merge(exp_ev_g[['firm_id', 'dif_gender', 'dif_se_gender', 'log_dif_gender', 'log_dif_se_gender']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_ev_40[['firm_id', 'dif_age', 'dif_se_age', 'log_dif_age', 'log_dif_se_age']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_ev_ranking[['firm_id', 'groups_lambda0.25']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_cb_central[['firm_id','cb_central_full']], how = 'outer', validate = '1:1')
exp_ev = exp_ev.merge(exp_cb_central_se[['firm_id','cb_central_full_se']], how = 'outer', validate = '1:1')

# Store firm names in a Python object
firms = pd.read_csv(external / 'formatted_firm_names.csv')

# Merge firm names onto exp_ev dataframe 
exp_ev = exp_ev.merge(firms, how = 'left', validate = '1:1')

# Standardize firm codes 
exp_ev.rename(columns = {'firm_code': 'firm'}, inplace = True)

# Keep necessary variables
exp_ev = exp_ev[['dif', 'dif_gender', 'log_dif', 'log_dif_gender', 'groups_lambda0.25',
                 'dif_se', 'dif_se_gender', 'log_dif_se', 'log_dif_se_gender'
                 , 'dif_age', 'dif_se_age', 'log_dif_age', 'log_dif_se_age', 'firm', 'firm_id','cb_central_full', 'cb_central_full_se']]

# Define dictionary to normalize firm names between datasets
replace_firms = {
    'State Farm': 'State Farm Insurance Cos.',
    "Dick's": "Dick's Sporting Goods",
    'J.B. Hunt': 'J.B. Hunt Transport Services',
    'Walgreens': 'Walgreens Boots Alliance',
    'Marriott': 'Marriott International',
    'Honeywell': 'Honeywell International',
    'Hilton': 'Hilton Worldwide Holdings',
    'Publix': 'Publix Super Markets',
    'Dr Pepper': 'Dr Pepper Snapple Group',
    'Ascena (Ann Taylor / Loft)': 'Ascena Retail Group (Loft stores, Lane Bryant, Ann Taylor)',
    'Cardinal Health':'Cardinal Health (Outcomes Incorporated, CareFusion)',
    'Charter / Spectrum':'Charter Communications (Spectrum)',
    'Sears': 'Sears Holdings (Kmart, Sears)',
    'US Bank': 'U.S. Bancorp (US Bank)',
    "Kroger": 'Kroger (Harris Teeter, Fred Meyer, Ralphs, King Soopers)',
    'XPO Logistics': 'XPO Logistics (Con-way)',
    'Sherwin-Williams': 'Sherwin-Williams (Valspar, Minwax)',
    "Kohl's": "Kohl's (Kohl's Department Stores)",
    "UnitedHealth": "UnitedHealth Group (UnitedHealthcare, Optum)",
    "TJX": "TJX (TJ Maxx, Marshalls, HomeGoods)",
    "Avis-Budget": "Avis Budget Group (Zipcar, Avis Car Rental, Budget Car Rental)",
    "ATandT": "AT&T",
    "Aramark": "Aramark (AmeriPride Services, AIM Services, Good Uncle)",
    'DISH': "DISH Network (Sling TV, Boost Mobile)",
    'Lab Corp': 'Laboratory Corp. of America (Labcorp)',
    'AECOM': 'AECOM (Tishman Realty & Construction, Hunt Construction)',
    'Hertz': 'Hertz Global Holdings (Hertz)',
    "O'Reilly Automotive": "O'Reilly Automotive (O'Reilly Auto Parts)",
    "Genuine Parts": 'Genuine Parts (Automotive Parts Group, Motion Industries)',
    'US Foods': 'US Foods Holding',
    'Walmart': "Walmart (Walmart, Sam's Club)",
    'CBRE': 'CBRE Group (Trammell Crow Company)',
    'Goodyear': 'Goodyear Tire & Rubber (Dunlop Tires, Cooper Tires)',
    'Jones Financial': 'Jones Financial (Edward Jones)',
    'Mondelez': 'Mondelez International (Oreo, Cadbury, Milka)',
    'VFC (North Face / Vans)': 'VF (Vans, The North Face, Supreme)',
    'Stanley Black & Decker': 'Stanley Black & Decker (DeWalt, Lenox, Porter Cable)',
    'Jones Lang LaSalle': 'Jones Lang LaSalle (JLL, HFF, Corrigo)',
    'Universal Health': 'Universal Health Services (Ardent Health Services, Horizon Health Services, Pavilion Foundation)',
    'Estee Lauder': 'Estee Lauder (Michael Kors, AVEDA, MAC)',
    'LKQ (automotive)': 'LKQ (Keystone, Warn Industries)',
    'Ascena (Ann Taylor / Loft)': 'Ascena Retail Group (Loft stores, Lane Bryant, Ann Taylor)',
    'Dean Foods': "Dean Foods (Friendly's)",
    'Builders FirstSource': 'Builders FirstSource (ProBuild, Building Materials Holding)',
    'Performance Food Group': 'Performance Food Group (Vistar, PFG Customized)',
    'UGI': 'UGI (AmeriGas, FLAGA)',
    'Bank of America': 'Bank of America Corp.',
    'BB&T': 'BB&T Corp. (Truist Financial, SunTrust Banks)'
}

# Apply firm name standardization
exp_ev.firm.replace(replace_firms, inplace = True)

# Check that all firms in the long dataframe are in the experimental estimates dataframe
firm_check = dflong[['firm']].drop_duplicates().merge(exp_ev, how = 'outer', validate = '1:1', indicator = True)

# Save the firm check dataframe for review
firm_check.to_csv(dump / 'check_firms1.csv', index = False)

# Confirm that there are no firms in dflong that are not in exp_ev
# assert firm_check._merge.value_counts()['right_only'] == 0

# Merge experimental estimates onto long dataframe
dflong = dflong.merge(exp_ev, how = 'left', validate = 'm:1')

# Confirm that the merge worked correctly by checking number of unique firms with non-missing dif and log_dif values
# assert dflong.loc[dflong.log_dif.notnull(), ['firm','log_dif']].drop_duplicates().shape[0] == 97

# ------------------------------------------------------------------------------
# Clean demographic variables and those from other survey 
# questions
# ------------------------------------------------------------------------------

## Recode race
# Define new race variable set to "Other" by default
df['race_recode'] = 'Other'

# Replace values for race_recode with white if original race variable is "White" 
df.loc[df.race == 'White', 'race_recode'] = 'White'

# Replace values for race_recode with Black if original race variable is "Black or African American"
df.loc[df.race == 'Black or African American', 'race_recode'] = 'Black'

# Convert all characters in the race variable to only letters and spaces 
# df['race'] = df['race'].astype(str).apply(lambda x: re.sub(r'[^a-zA-Z]', ' ', x)).str.strip()
# Convert all characters in the race variable to only letters and spaces (vectorized, preserves NaN)
df['race'] = df['race'].astype('object') \
                     .str.replace(r'[^a-zA-Z]', ' ', regex=True) \
                     .str.strip()
# Convert literal 'nan' strings (if any) to real NA
df.loc[df['race'].str.lower() == 'nan', 'race'] = np.nan

## Education
# Convert all characters in the educ variable to only letters, numbers, and spaces
df['educ'] = df['educ'].astype(str).apply(lambda x: re.sub(r'[^a-zA-Z0-9]', ' ', x)).str.strip()
# df['educ'] = df['educ'].astype('object') \
#                       .str.replace(r'[^a-zA-Z0-9]', ' ', regex=True) \
#                       .str.strip()

# Recode education values for consistency
df['educ'].replace({'Some college  no degree': 'Some college, no degree',
                    'High school graduate   high school diploma or the equivalent  GED': 'High school diploma',
                     'Master s degree': 'Master degree',
                     'Bachelor s degree': 'Bachelor degree',
                     '1st  2nd  3rd  or 4th grade': '4th grade or below',
                     '9th grade': 'Some years of high school',
                     '10th grade': 'Some years of high school',
                     '11th grade': 'Some years of high school',
                     '12th grade no diploma': 'Some years of high school'}, inplace = True)

# Replace "nan" strings with missing values
df.loc[df.educ == "nan", 'educ'] = ""
# df.loc[df['educ'].str.lower() == 'nan', 'educ'] = ""

## Employment

# Convert all characters in the empstat variable to only letters, underscores, numbers, and spaces
df['empstat'] = df['empstat'].astype(str).apply(lambda x: re.sub(r'[^\w\s]','',x)).str.strip()
# df['empstat'] = df['empstat'].astype('object') \
#                           .str.replace(r'[^\w\s]', '', regex=True) \
#                           .str.strip()

# Replace "nan" strings with missing values
df.loc[df.empstat == "nan", 'empstat'] = ""
# df.loc[df['empstat'].str.lower() == 'nan', 'empstat'] = ""

# Convert age variable to numeric
df['age'] = pd.to_numeric(df.age, errors = 'coerce')

# Convert zipcode variable to numeric
df['zipcode'] = pd.to_numeric(df.zipcode, errors = 'coerce')

# Create a score for the attention check
def attention_check(x, i):
    return x[f'attentionFirm{i}'] in [x['firm_1'], x['firm_2'], x['firm_3'], x['firm_4'], x['firm_5']]

# Calculate attention check results for each of the first three firms
df['attentionFirm1_check'] = df.apply(lambda x: attention_check(x, i = 1), axis = 1)
df['attentionFirm2_check'] = df.apply(lambda x: attention_check(x, i = 2), axis = 1)
df['attentionFirm3_check'] = df.apply(lambda x: attention_check(x, i = 3), axis = 1)

# Define a function to calculate the overall attention score
def count_attention(x):
    # Check first if the answer is not missing
    if pd.isnull(x['attention_check']):
        return np.nan
    else:
        tot_right = 0
        for k in range(1,4):
            in_firm_list = attention_check(x, i = k)
            checked = "${e://Field/attention" + "Firm{}".format(k) + "}" in x['attention_check']
            if in_firm_list & checked:
                tot_right += 1
            if (not in_firm_list) & (not checked):
                tot_right += 1

        return tot_right/3

# Calculate overall attention score
df['attention_score'] = df.apply(lambda x: count_attention(x), axis=1)

# Convert response duration to numeric
df['response_duration'] = pd.to_numeric(df['Duration (in seconds)'])

# Clean firm names in long dataframe by removing any text in parentheses
dflong['firm_clean'] = dflong['firm'].astype(str).apply(lambda x: re.sub(r'\([^)]*\)', '', x)).str.strip()

# ------------------------------------------------------------------------------
# Merge long dataframe with main df dataframe
# ------------------------------------------------------------------------------
# Merge
dflong = dflong.merge(
            df[
                ['ResponseId','race','race_recode','gender','age',
                'educ','zipcode','married',
                'empstat','looking_job',
                'party_affil',
                'confidence_race_names','confidence_gend_names',
                'confidence_race_conduct','confidence_gend_conduct','confidence_age_conduct',
                'any_entry_lev_exp',
                'information_source',
                'feared_discrim',
                'attention_score',
                'response_duration','income','hispanic','race_metadata'
                ]
            ], how='left', on='ResponseId', validate='m:1')

# Restrict to respondents who passed attention check
dflong = dflong.loc[dflong.attention_score == 1]

# Sort 
dflong = dflong.sort_values(['ResponseId','option_number'])

# Export cleaned long dataframe to Stata .dta file 
dflong.to_stata(processed / 'long_survey.dta')

# Export cleaned long dataframe to .csv file 
dflong.to_csv(processed / 'long_survey.csv', index=False)
