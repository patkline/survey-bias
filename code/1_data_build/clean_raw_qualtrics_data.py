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
from globals import raw, processed, external

# ------------------------------------------------------------------------------
# Import and clean raw survey data
# ------------------------------------------------------------------------------

# Load the probability samples
df = pd.read_csv(raw / 'prob/RR_Qualtrics_September 5, 2023_10.16.csv')

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
    
    # Treat the demographic append as authoritative for matched responses.
    else:
        df.loc[df._merge == 'both',f'*{col}'] = df.loc[df._merge == 'both', f'{col}']
        df.drop(columns =col, inplace=True)

# Drop the merge indicator column
df.drop(columns ='_merge', inplace = True)

# Store new survey round data in a Python object
df_app = pd.read_csv(raw / 'RR_Qualtrics_February 5, 2024_11.05.csv')

# Confirm that the first two rows are not actual observations
assert df_app.iloc[0]['Status'] != 'IP Address', "First row should be metadata, not an observation"
assert df_app.iloc[1]['Status'] != 'IP Address', "Second row should be metadata, not an observation"

# Remove metadata, preview, and spam rows from the appended export.
df_app = df_app.loc[~df_app['Status'].isin(['Response Type', '{"ImportId":"status"}', 'Survey Preview', 'Spam'])]

# Confirm that all remaining rows are actual observations
assert (df_app['Status'] == 'IP Address').all(), "All remaining rows should be observations"

# Define "sample" variable with value 1 for the "prob" sample and 0 for the "conv" sample
df_app['sample'] = df_app['S'].apply(lambda x: 1 if x == "prob" else 0)

# Keep appended observations recorded after the last date in the original export.
df_app = df_app.loc[df_app.StartDate > df.StartDate.max()]
  
# Append the 44 new observations in df_app to the original df dataframe
df = pd.concat([df, df_app], ignore_index=True)

# Check that the dataset is unique on ResponseId
assert df.ResponseId.nunique() == df.shape[0]

# Check that distribution channel is never preview 
assert (df.DistributionChannel != 'preview').all()

# ------------------------------------------------------------------------------
# Rename and recode variables
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
            ('Q210','conduct_female'),
            ('Q127','conduct_male'),
            ('Q248','conduct_older'),
            ('Q131','conduct_younger'),
            ('Q192','discretion')]:
    for k in range(1,6):
        df.rename(columns={f"{var}_{k}":f"{name}_{k}"}, inplace=True)

for k in range(1,6):
    df.rename(columns={f"firm{k}":f"firm_{k}"}, inplace=True)

# Check that there are no missing values in the sample variable
assert df['sample'].isnull().sum() == 0

# ------------------------------------------------------------------------------
# Generate long version of conduct questions
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
             ]:
    dflong[var] = dflong[var].replace(replace_dict)
    dflong[var] = pd.to_numeric(dflong[var])
    invalid_values = set(dflong[var].dropna().unique()) - {-1, 1, 2, 3, 4, 5}
    assert not invalid_values, f"Unexpected recoded values in {var}: {invalid_values}"

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
dflong['conduct_favor_white'] = dflong['conduct_black']

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


# ------------------------------------------------------------------------------
# Clean and merge experimental datasets together, and merge 
# onto long data 
# ------------------------------------------------------------------------------

# Load the race and gender log contact gaps used by the retained EIV tables.
race_audit_estimates = pd.read_csv(external / 'theta_estimates_wjobs_v7.csv')[
    ['firm_id', 'njobs', 'log_dif']
]
gender_audit_estimates = (
    pd.read_csv(external / 'theta_estimates_wjobs_v7gender.csv')[['firm_id', 'log_dif']]
    .rename(columns={'log_dif': 'log_dif_gender'})
)

# Use the complete firm-name key as the base so firms without audit estimates
# retain their firm IDs.
firms = pd.read_csv(external / 'formatted_firm_names.csv')
exp_ev = firms.merge(race_audit_estimates, on='firm_id', how='left', validate='1:1')
exp_ev = exp_ev.merge(gender_audit_estimates, on='firm_id', how='left', validate='1:1')

# Standardize firm codes 
exp_ev.rename(columns = {'firm_code': 'firm'}, inplace = True)

# Keep necessary variables
exp_ev = exp_ev[['log_dif', 'log_dif_gender', 'firm', 'firm_id', 'njobs']]

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

# Merge experimental estimates onto long dataframe
dflong = dflong.merge(exp_ev, how = 'left', validate = 'm:1')

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

# Convert all characters in the race variable to only letters and spaces (vectorized, preserves NaN)
df['race'] = df['race'].astype('object') \
                     .str.replace(r'[^a-zA-Z]', ' ', regex=True) \
                     .str.strip()
# Convert literal 'nan' strings (if any) to real NA
df.loc[df['race'].str.lower() == 'nan', 'race'] = np.nan

## Education
# Convert all characters in the educ variable to only letters, numbers, and spaces
df['educ'] = df['educ'].astype('object') \
                       .str.replace(r'[^a-zA-Z0-9]', ' ', regex=True) \
                       .str.strip()

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
df.loc[df['educ'].str.lower() == 'nan', 'educ'] = ""

## Employment

# Convert all characters in the empstat variable to only letters, underscores, numbers, and spaces
df['empstat'] = df['empstat'].astype('object') \
                           .str.replace(r'[^\w\s]', '', regex=True) \
                           .str.strip()

# Replace "nan" strings with missing values
df.loc[df['empstat'].str.lower() == 'nan', 'empstat'] = ""

# Convert age variable to numeric
df['age'] = pd.to_numeric(df.age, errors = 'coerce')

# Convert zipcode variable to numeric
df['zipcode'] = pd.to_numeric(df.zipcode, errors = 'coerce')

# Create a score for the attention check
def attention_check(x, i):
    return x[f'attentionFirm{i}'] in [x['firm_1'], x['firm_2'], x['firm_3'], x['firm_4'], x['firm_5']]

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

# Export cleaned long dataframe to .csv file 
dflong.to_csv(processed / 'long_survey.csv', index=False)
