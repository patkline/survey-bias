#This script was used to pull the Revelio data from WRDS JupyterHub
#The output is revelio_company_race_gender_sentiment_2023_weighted_by_rcid.csv

import wrds
import pandas as pd

conn = wrds.Connection()

sql = r"""
WITH target_rcids(rcid) AS (
    VALUES
        ('1328385'),
        ('1181876'),
        ('1285359'),
        ('96737402'),
        ('498179'),
        ('502241'),
        ('591387'),
        ('576405'),
        ('409938'),
        ('22253513'),
        ('513937'),
        ('264792'),
        ('597372'),
        ('1278377'),
        ('22142414'),
        ('1133445'),
        ('608403'),
        ('597965'),
        ('1314252'),
        ('1072323'),
        ('1106413'),
        ('1146370'),
        ('494205'),
        ('471501'),
        ('1190765'),
        ('184611'),
        ('1492897'),
        ('377578'),
        ('218'),
        ('860320'),
        ('180932'),
        ('22142761'),
        ('239923'),
        ('267301'),
        ('604422'),
        ('736220'),
        ('7375'),
        ('912348'),
        ('449435'),
        ('1494801'),
        ('21023141'),
        ('1013753'),
        ('1141065'),
        ('22142476'),
        ('1070638'),
        ('1029450'),
        ('1058616'),
        ('554028'),
        ('1037681'),
        ('1092415'),
        ('864132'),
        ('22178039'),
        ('1122757'),
        ('1400574'),
        ('89998'),
        ('806872'),
        ('47061'),
        ('771437'),
        ('1006102'),
        ('1483487'),
        ('1109560'),
        ('97840'),
        ('1289236'),
        ('20924380'),
        ('786030'),
        ('1047983'),
        ('196290'),
        ('1302930'),
        ('442731'),
        ('380780'),
        ('350953'),
        ('1474903'),
        ('1037635'),
        ('22142390'),
        ('775676'),
        ('491397'),
        ('306545'),
        ('367297'),
        ('351560'),
        ('22142783'),
        ('851738'),
        ('279267'),
        ('111681'),
        ('1232095'),
        ('1359692'),
        ('22214941'),
        ('420392'),
        ('324175'),
        ('22142648'),
        ('20921805'),
        ('739347'),
        ('762166'),
        ('1128013'),
        ('172733'),
        ('21019929'),
        ('489711'),
        ('826245'),
        ('1112079'),
        ('1485785'),
        ('291535'),
        ('614617'),
        ('1017034'),
        ('647627'),
        ('703073'),
        ('1035037'),
        ('606871'),
        ('915304'),
        ('1042797'),
        ('1398143'),
        ('21007446'),
        ('600328'),
        ('1417832'),
        ('254822'),
        ('961524'),
        ('1000539'),
        ('513460'),
        ('745033'),
        ('21042065'),
        ('464717'),
        ('22255773'),
        ('1320707'),
        ('20921455'),
        ('903518'),
        ('1033568'),
        ('22142913'),
        ('268767'),
        ('1489790'),
        ('820133'),
        ('777168'),
        ('766823'),
        ('472640'),
        ('642928'),
        ('233402'),
        ('8053793'),
        ('828625'),
        ('1191500'),
        ('110490'),
        ('22141982'),
        ('1311350'),
        ('8002708'),
        ('1010588'),
        ('508665'),
        ('538792'),
        ('1353141'),
        ('543448'),
        ('471430'),
        ('280930'),
        ('393528')
),

company_names AS (
    /*
       The supplied IDs are company-level RCIDs, not ultimate-parent RCIDs.
       So this lookup is by company_mapping.rcid directly.
    */
    SELECT
        cm.rcid::text AS rcid,
        MIN(cm.company::text) AS mapping_company
    FROM revelio_common.company_mapping AS cm
    INNER JOIN target_rcids AS t
        ON cm.rcid::text = t.rcid
    WHERE cm.company IS NOT NULL
    GROUP BY
        cm.rcid::text
),

positions_2023 AS (
    /*
       Pull only positions whose own company RCID is in your target RCID list.
       No subsidiary/parent expansion is done here.
    */
    SELECT
        t.rcid,
        p.rcid::text AS position_rcid,
        p.user_id::text AS user_id,
        p.position_id,
        p.startdate,
        p.enddate,
        p.seniority,
        p.weight::double precision AS position_weight
    FROM revelio_individual.individual_positions AS p
    INNER JOIN target_rcids AS t
        ON p.rcid::text = t.rcid
    WHERE DATE '2023-12-31' >= COALESCE(p.startdate, '-infinity'::date)
      AND DATE '2023-01-01' <= COALESCE(p.enddate,   'infinity'::date)
      AND p.seniority = 1

      -- Optional: uncomment if you only want U.S. positions
      -- AND p.country = 'United States'
),

position_summary AS (
    SELECT
        rcid,
        COUNT(*) AS n_position_rows_2023,
        COUNT(DISTINCT position_rcid) AS n_position_rcids_2023,
        COUNT(DISTINCT user_id) AS n_users_from_positions_2023
    FROM positions_2023
    GROUP BY
        rcid
),

company_workers_2023 AS (
    /*
       Collapse to one row per company RCID x user_id.

       This avoids double-counting a worker who has multiple relevant
       position rows at the same company RCID in 2023.
    */
    SELECT
        rcid,
        user_id,
        AVG(position_weight) FILTER (WHERE position_weight IS NOT NULL) AS worker_weight,
        COUNT(DISTINCT position_id) AS n_positions_2023
    FROM positions_2023
    GROUP BY
        rcid,
        user_id
),

matched_workers AS (
    SELECT
        cw.rcid,
        cw.user_id,
        cw.n_positions_2023,

        /*
           If weight is missing, fall back to 1 for that worker.
           The output includes n_missing_weight so you can check whether this matters.
        */
        COALESCE(cw.worker_weight, 1.0) AS w,
        CASE WHEN cw.worker_weight IS NULL THEN 1 ELSE 0 END AS missing_weight,

        u.f_prob::double precision AS f_prob,
        u.m_prob::double precision AS m_prob,

        u.white_prob::double precision AS white_prob,
        u.black_prob::double precision AS black_prob,
        u.api_prob::double precision AS api_prob,
        u.hispanic_prob::double precision AS hispanic_prob,
        u.native_prob::double precision AS native_prob,
        u.multiple_prob::double precision AS multiple_prob,

        u.sex_predicted,
        u.ethnicity_predicted,

        CASE
            WHEN u.f_prob IS NOT NULL
             AND u.m_prob IS NOT NULL
            THEN 1 ELSE 0
        END AS has_gender_probs,

        CASE
            WHEN u.white_prob IS NOT NULL
             AND u.black_prob IS NOT NULL
             AND u.api_prob IS NOT NULL
             AND u.hispanic_prob IS NOT NULL
             AND u.native_prob IS NOT NULL
             AND u.multiple_prob IS NOT NULL
            THEN 1 ELSE 0
        END AS has_ethnicity_probs

    FROM company_workers_2023 AS cw
    INNER JOIN revelio_individual.individual_user AS u
        ON u.user_id::text = cw.user_id
),

demographic_shares AS (
    SELECT
        rcid,

        COUNT(*) AS n_unique_users_matched_to_user_file,
        SUM(w) AS weighted_n_users,

        SUM(missing_weight) AS n_missing_weight,
        SUM(CASE WHEN has_gender_probs = 1 THEN 1 ELSE 0 END) AS n_users_with_gender_probs,
        SUM(CASE WHEN has_ethnicity_probs = 1 THEN 1 ELSE 0 END) AS n_users_with_ethnicity_probs,

        SUM(w) FILTER (WHERE has_gender_probs = 1) AS weighted_n_gender,
        SUM(w) FILTER (WHERE has_ethnicity_probs = 1) AS weighted_n_ethnicity,

        SUM(w * f_prob) FILTER (WHERE has_gender_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_gender_probs = 1), 0) AS female_share,

        SUM(w * m_prob) FILTER (WHERE has_gender_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_gender_probs = 1), 0) AS male_share,

        SUM(w * white_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS white_share,

        SUM(w * black_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS black_share,

        SUM(w * api_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS api_share,

        SUM(w * hispanic_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS hispanic_share,

        SUM(w * native_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS native_share,

        SUM(w * multiple_prob) FILTER (WHERE has_ethnicity_probs = 1)
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS multiple_share

    FROM matched_workers
    GROUP BY
        rcid
),

sentiment_scores_dedup AS (
    /*
       One sentiment row per target RCID. If there are duplicate rows for
       an RCID, keep the one with the largest num_reviews.
    */
    SELECT DISTINCT ON (ss.rcid::text)
        ss.rcid::text AS rcid,
        ss.company::text AS sentiment_company,
        ss.num_reviews::double precision AS sentiment_num_reviews,

        ss.management_sentiment::double precision AS management_sentiment,
        ss.innovative_technology_sentiment::double precision AS innovative_technology_sentiment,
        ss.work_life_balance_sentiment::double precision AS work_life_balance_sentiment,
        ss.mentorship_sentiment::double precision AS mentorship_sentiment,
        ss.career_advancement_sentiment::double precision AS career_advancement_sentiment,
        ss.div_and_inclusion_sentiment::double precision AS div_and_inclusion_sentiment,
        ss.coworkers_sentiment::double precision AS coworkers_sentiment,
        ss.compensation_sentiment::double precision AS compensation_sentiment,
        ss.culture_sentiment::double precision AS culture_sentiment,
        ss.co_and_division_size_sentiment::double precision AS co_and_division_size_sentiment,
        ss.perks_and_benefits_sentiment::double precision AS perks_and_benefits_sentiment,
        ss.onboarding_sentiment::double precision AS onboarding_sentiment,
        ss.remote_work_sentiment::double precision AS remote_work_sentiment

    FROM revelio_sentiment.sentiment_scores AS ss
    INNER JOIN target_rcids AS t
        ON ss.rcid::text = t.rcid
    ORDER BY
        ss.rcid::text,
        ss.num_reviews DESC NULLS LAST
)

SELECT
    t.rcid,

    COALESCE(cn.mapping_company, sd.sentiment_company) AS company,
    COALESCE(cn.mapping_company, sd.sentiment_company) AS company_name,
    CASE
        WHEN cn.mapping_company IS NOT NULL THEN 'company_mapping'
        WHEN sd.sentiment_company IS NOT NULL THEN 'sentiment_scores'
        ELSE 'no_name_found'
    END AS company_name_source,

    CASE
        WHEN d.rcid IS NOT NULL THEN 1
        ELSE 0
    END AS has_2023_worker_match,

    ps.n_position_rows_2023,
    ps.n_position_rcids_2023,
    ps.n_users_from_positions_2023,

    d.n_unique_users_matched_to_user_file,
    d.weighted_n_users,
    d.n_missing_weight,
    d.n_users_with_gender_probs,
    d.n_users_with_ethnicity_probs,
    d.weighted_n_gender,
    d.weighted_n_ethnicity,

    d.female_share,
    d.male_share,
    d.white_share,
    d.black_share,
    d.api_share,
    d.hispanic_share,
    d.native_share,
    d.multiple_share,

    CASE
        WHEN sd.rcid IS NOT NULL THEN 'direct_rcid'
        ELSE 'no_sentiment_match'
    END AS sentiment_match_type,

    sd.sentiment_company,
    sd.sentiment_num_reviews,

    sd.management_sentiment,
    sd.innovative_technology_sentiment,
    sd.work_life_balance_sentiment,
    sd.mentorship_sentiment,
    sd.career_advancement_sentiment,
    sd.div_and_inclusion_sentiment,
    sd.coworkers_sentiment,
    sd.compensation_sentiment,
    sd.culture_sentiment,
    sd.co_and_division_size_sentiment,
    sd.perks_and_benefits_sentiment,
    sd.onboarding_sentiment,
    sd.remote_work_sentiment

FROM target_rcids AS t
LEFT JOIN company_names AS cn
    ON t.rcid = cn.rcid
LEFT JOIN position_summary AS ps
    ON t.rcid = ps.rcid
LEFT JOIN demographic_shares AS d
    ON t.rcid = d.rcid
LEFT JOIN sentiment_scores_dedup AS sd
    ON t.rcid = sd.rcid
ORDER BY
    company NULLS LAST,
    t.rcid;

"""

df = conn.raw_sql(sql)

out_file = "revelio_company_race_gender_sentiment_2023_weighted_by_rcid.csv"
df.to_csv(out_file, index=False)

print(f"Saved {len(df):,} rows to {out_file}")
print("Rows with no company name:", int(df["company"].isna().sum()))
print("Rows with no 2023 worker match:", int((df["has_2023_worker_match"] == 0).sum()))
print("Rows with no sentiment match:", int((df["sentiment_match_type"] == "no_sentiment_match").sum()))

df.head()
