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
from globals import raw, cleaned_survey_data, external, dump

#XXremove these once I replace the paths below 
#project_dir = "/Users/evanrose/Library/CloudStorage/Dropbox/GSI-GSR-Reader/Audit/Survey/consolidated_code"
#project_dir = "/Users/jordancammarota/Dropbox/consolidated_code"
#path_to_raw = project_dir + "/raw"
#path_to_data = project_dir + "/data"
#path_to_processed =  project_dir + "/processed"
#path_to_external =  project_dir + "/external"
#path_to_dump =  project_dir + "/dump"

# ------------------------------------------------------------------------------
# Adjust python settings
# ------------------------------------------------------------------------------

#warnings.simplefilter("ignore")

# Show up to 200 columns when printing dataframes
pd.options.display.max_columns = 200

# Show up to 1000 rows when printing dataframes
pd.options.display.max_rows = 1000

# Show up to 200 columns when using df.info()
pd.set_option('max_info_columns', 200)

# Don't wrap dataframe display across multiple lines (contradicted by next line)
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
# Import and merge raw survey data
# ------------------------------------------------------------------------------

# Load the probability samples
df = pd.read_csv(raw / 'prob/RR_Qualtrics_September 5, 2023_10.16.csv')

# the first rows contains the actual question, save it in a separate variable
questions = df.iloc[0]

print("here")
quit() 

# Remove the first two rows (not actual observations)
df = df.iloc[2:]

# Add the "sample" column with value 1 for the "prob" sample
df['sample'] = 1

# Load the convenience sample
df2 = pd.read_csv(path_to_raw + '/conv/RR_Qualtrics_October 5, 2023_15.10.csv')
df2 = df2.iloc[2:]

# Add the "sample" column with value 1 for the "prob" sample
df2['sample'] = 0

df = pd.concat([df, df2])

# Add the appended missing demographic info
dem = pd.read_csv(path_to_raw + "/RR data append.csv")
df = df.merge(dem, on='ResponseId', how='outer', validate='1:1', indicator=True, suffixes=("", "_missdem"))
assert df._merge.value_counts()['right_only'] == 0

# Now replace missing values
for col in dem.columns:
    if col == 'ResponseId':
        continue
    elif col == 'Q110':
        df.loc[df._merge == 'both','Q110'] = df.loc[df._merge == 'both','Q110_missdem']
        df.drop(columns='Q110_missdem', inplace=True)
    else:
        df.loc[df._merge == 'both',f'*{col}'] = df.loc[df._merge == 'both', f'{col}']
        df.drop(columns =col, inplace=True)
df.drop(columns ='_merge', inplace = True)

# Add newer answers
df_app = pd.read_csv(path_to_raw + "/RR_Qualtrics_February 5, 2024_11.05.csv")
df_app = df_app.iloc[2:]
df_app['sample'] = df_app['S'].apply(lambda x: 1 if x == "prob" else 0)
print(df_app.ResponseId.isin(df.ResponseId).value_counts())
print(df.ResponseId.isin(df_app.ResponseId).value_counts())

# Excluded
excluded = df_app.loc[~df_app.ResponseId.isin(df.ResponseId) 
    & (df_app.StartDate <= df.StartDate.max())]

# Filter them
df_app = df_app.loc[df_app.StartDate > df.StartDate.max()]
  
# Then add them
df = pd.concat([df, df_app], ignore_index=True)
assert df.ResponseId.nunique() == df.shape[0]

# Remove preview answers
df = df.loc[df.DistributionChannel != 'preview']


### 2) RENAME AND RECODE VARIABLES
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

# Sample type
df.loc[df.DistributionChannel == 'anonymous', 'sample_type'] = 'conv'
df.loc[df.DistributionChannel == 'gl', 'sample_type'] = 'prob'
assert df.sample_type.isnull().sum() == 0


### 3) Generate long version of name and conduct questions, then combine
# Firm questions
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
dflong = df[['ResponseId','sample'] +
    [c + f"_{k}" for c in tokeep for k in range(1,6)]].copy()
dflong = pd.wide_to_long(dflong, tokeep, 
            i='ResponseId', j='option_number', suffix=r'_([0-5])').reset_index()
dflong['option_number'] = dflong.option_number.apply(lambda x: int(x[1:]))

# Name questions
tokeep = [  'name',
            'name_contact',
            'NameRace_wfirst0',
            'NameRace_wfirst1'
            ]
nameslong = df[['ResponseId'] +
    [c + f"_{k}" for c in tokeep for k in range(1,6)]].copy()
nameslong = pd.wide_to_long(nameslong, tokeep, 
            i='ResponseId', j='option_number', suffix=r'_([0-5])').reset_index()
nameslong['option_number'] = nameslong.option_number.apply(lambda x: int(x[1:]))


# Combine
dflong = dflong.merge(nameslong, how='left',
        on=['ResponseId','option_number'], validate='1:1')

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

                    "Don't know/ prefer not to answer": np.NaN,
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

# Flip FirmSelective
dflong['FirmSelective'] = 6 - dflong.FirmSelective

# Generate versions comebining white/black and male/female first versions
dflong['FirmCont_favor_white'] = dflong.FirmContRace_wfirst1.where(
                            dflong.FirmContRace_wfirst1.notnull(), 6-dflong.FirmContRace_wfirst0)
dflong['FirmHire_favor_white'] = dflong.FirmHireRace_wfirst1.where(
                            dflong.FirmHireRace_wfirst1.notnull(), 6-dflong.FirmHireRace_wfirst0)

dflong['FirmCont_favor_male'] = dflong.FirmContGend_mfirst1.where(
                            dflong.FirmContGend_mfirst1.notnull(), 6-dflong.FirmContGend_mfirst0)
dflong['FirmHire_favor_male'] = dflong.FirmHireGend_mfirst1.where(
                            dflong.FirmHireGend_mfirst1.notnull(), 6-dflong.FirmHireGend_mfirst0)

dflong['conduct_favor_white'] = dflong.conduct_black.where(
                            dflong.conduct_black.notnull(), 6-dflong.conduct_white)
dflong['conduct_favor_male'] = dflong.conduct_female.where(
                            dflong.conduct_female.notnull(), 6-dflong.conduct_male)
dflong['conduct_favor_younger'] = dflong.conduct_older.where(
                            dflong.conduct_older.notnull(), 6-dflong.conduct_younger)

dflong['name_likely_white'] = dflong.NameRace_wfirst1.where(
                            dflong.NameRace_wfirst1.notnull(), 6-dflong.NameRace_wfirst0)


### 4) Add experimental data
# Get the contact gaps from report cards
exp_ev = pd.read_csv(path_to_external + "/theta_estimates_wjobs_v7.csv")
exp_ev_g = pd.read_csv(path_to_external + "/theta_estimates_wjobs_v7gender.csv")
exp_ev_40 = pd.read_csv(path_to_external + "/theta_estimates_wjobs_v7over40.csv")
exp_ev_ranking = pd.read_csv(path_to_external + "/theta_estimates_wjobs_v7_ranking.csv")
exp_cb_central = pd.read_csv(path_to_external + "/centralization.csv")
exp_ev_g['dif_gender'] = exp_ev_g.dif
exp_ev_g['log_dif_gender'] = exp_ev_g.log_dif
exp_ev_g['dif_se_gender'] = exp_ev_g.dif_se
exp_ev_g['log_dif_se_gender'] = exp_ev_g.log_dif_se
exp_ev_40['dif_age'] = exp_ev_40.dif
exp_ev_40['log_dif_age'] = exp_ev_40.log_dif
exp_ev_40['dif_se_age'] = exp_ev_40.dif_se
exp_ev_40['log_dif_se_age'] = exp_ev_40.log_dif_se
exp_cb_central['cb_central_full'] = exp_cb_central.cb_central
exp_ev = exp_ev.merge(exp_ev_g[['firm_id', 'dif_gender', 'dif_se_gender', 'log_dif_gender', 'log_dif_se_gender']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_ev_40[['firm_id', 'dif_age', 'dif_se_age', 'log_dif_age', 'log_dif_se_age']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_ev_ranking[['firm_id', 'groups_lambda0.25']], how = 'left', validate = '1:1')
exp_ev = exp_ev.merge(exp_cb_central[['firm_id','cb_central_full']], how = 'outer', validate = '1:1')

firms = pd.read_csv(path_to_external + "/formatted_firm_names.csv")
exp_ev = exp_ev.merge(firms, how = 'left', validate = '1:1')
exp_ev.rename(columns = {'firm_code': 'firm'}, inplace = True)

# Subset
exp_ev = exp_ev[['dif', 'dif_gender', 'log_dif', 'log_dif_gender', 'groups_lambda0.25',
                 'dif_se', 'dif_se_gender', 'log_dif_se', 'log_dif_se_gender'
                 , 'dif_age', 'dif_se_age', 'log_dif_age', 'log_dif_se_age', 'firm', 'firm_id','cb_central', 'cb_central_full']]

# Merge onto long data
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

exp_ev.firm.replace(replace_firms, inplace = True)

# Merge
firm_check = dflong[['firm']].drop_duplicates().merge(exp_ev, how = 'outer', validate = '1:1', indicator = True)
firm_check.to_csv(path_to_dump + "/check_firms1.csv", index = False)
assert firm_check._merge.value_counts()['right_only'] == 0
dflong = dflong.merge(exp_ev, how = 'left', validate = 'm:1')
assert dflong.loc[dflong.log_dif.notnull(), ['firm','log_dif']].drop_duplicates().shape[0] == 97


### 5) Add demographic and other questions from the survey
# Recode race
df['race_recode'] = 'Other'
df.loc[df.race == 'White', 'race_recode'] = 'White'
df.loc[df.race == 'Black or African American', 'race_recode'] = 'Black'
df['race'] = df['race'].astype(str).apply(lambda x: re.sub(r'[^a-zA-Z]', ' ', x)).str.strip()

# Education
df['educ'] = df['educ'].astype(str).apply(lambda x: re.sub(r'[^a-zA-Z0-9]', ' ', x)).str.strip()
df['educ'].replace({'Some college  no degree': 'Some college, no degree',
                    'High school graduate   high school diploma or the equivalent  GED': 'High school diploma',
                     'Master s degree': 'Master degree',
                     'Bachelor s degree': 'Bachelor degree',
                     '1st  2nd  3rd  or 4th grade': '4th grade or below',
                     '9th grade': 'Some years of high school',
                     '10th grade': 'Some years of high school',
                     '11th grade': 'Some years of high school',
                     '12th grade no diploma': 'Some years of high school'}, inplace = True)
df.loc[df.educ == "nan", 'educ'] = ""

# Employment
df['empstat'] = df['empstat'].astype(str).apply(lambda x: re.sub(r'[^\w\s]','',x)).str.strip()
df.loc[df.empstat == "nan", 'empstat'] = ""

# Convert some vars to numeric
df['age'] = pd.to_numeric(df.age, errors = 'coerce')
df['zipcode'] = pd.to_numeric(df.zipcode, errors = 'coerce')

# Create a score for the attention check
def attention_check(x, i):
    return x[f'attentionFirm{i}'] in [x['firm_1'], x['firm_2'], x['firm_3'], x['firm_4'], x['firm_5']]

df['attentionFirm1_check'] = df.apply(lambda x: attention_check(x, i = 1), axis = 1)
df['attentionFirm2_check'] = df.apply(lambda x: attention_check(x, i = 2), axis = 1)
df['attentionFirm3_check'] = df.apply(lambda x: attention_check(x, i = 3), axis = 1)

def count_attention(x):
    # Check first if the answer is not missing
    if pd.isnull(x['attention_check']):
        return np.NaN
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

df['attention_score'] = df.apply(lambda x: count_attention(x), axis=1)

# Response times
df['response_duration'] = pd.to_numeric(df['Duration (in seconds)'])

# "Clean" firm name
dflong['firm_clean'] = dflong['firm'].astype(str).apply(lambda x: re.sub(r'\([^)]*\)', '', x)).str.strip()


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


# Keep only those who pass attention check
dflong = dflong.loc[dflong.attention_score == 1]


### 6) Save
dflong = dflong.sort_values(['ResponseId','option_number'])
dflong.to_stata(path_to_processed + "/long_survey.dta")
dflong.to_csv(path_to_processed + "/long_survey.csv", index=False)
