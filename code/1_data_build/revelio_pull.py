r"""
Pull Revelio 2023 workforce race/gender shares plus salary-by-race/gender,
adding manual parent-RCID inheritance for selected subsidiary/brand firms.

Designed to run locally using the WRDS Python package.

Local setup, macOS/Linux:
    cd /path/to/survey-bias
    source .venv/bin/activate
    python -m pip install -U pip wheel wrds pandas
    python code/1_data_build/revelio_pull.py

Local setup, Windows PowerShell:
    cd \path\to\survey-bias
    .\.venv\Scripts\activate
    python -m pip install -U pip wheel wrds pandas
    python code/1_data_build/revelio_pull.py

Username options:
    * Set wrds_usernames_by_user in code/globals.py.
    * Or set WRDS_USERNAME in your shell.
    * Or pass your username directly:
      python code/1_data_build/revelio_pull.py --wrds-username your_wrds_username

First local run:
    * You should be prompted for your WRDS password unless a local pgpass file
      already exists and is readable by PostgreSQL.
    * WRDS/your institution may also trigger Duo/MFA.
    * If prompted to create a pgpass file, saying yes lets future runs avoid
      retyping your password. Do not put your password in this script.

Default output:
    data/external/revelio_company_race_gender_salary_2023_with_parent_subsidiaries.csv
    or the Dropbox mirror equivalent, depending on code/globals.py.

Notes:
    * Direct firms use their own RCID.
    * Manual subsidiary rows use the parent RCID as workforce_source_rcid.
    * Aggregate company-level sentiment_scores is intentionally not pulled.
    * The D&I block uses revelio_sentiment.sentiment_individual_reviews, not
      revelio_sentiment.sentiment_scores. Delete that CTE/join if you want no
      sentiment/review tables at all.
"""

import argparse
import sys
from pathlib import Path

# Add code directory to Python path to import globals module
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

# Import path globals
from globals import (
    ensure_python_packages,
    external,
    wrds_username as default_wrds_username,
)


DEFAULT_OUTPUT_FILENAME = (
    "revelio_company_race_gender_salary_2023_with_parent_subsidiaries.csv"
)
DEFAULT_OUTPUT_PATH = external / DEFAULT_OUTPUT_FILENAME


# These are the 148 RCIDs from the prior 148-row output. Replace with your
# original RCID list if you prefer a different order.
target_rcids = [
    "22142476",
    "22141982",
    "1289236",
    "614617",
    "464717",
    "1328385",
    "8053793",
    "766823",
    "1359692",
    "21007446",
    "172733",
    "1232095",
    "280930",
    "97840",
    "736220",
    "1010588",
    "771437",
    "233402",
    "393528",
    "377578",
    "264792",
    "597372",
    "471501",
    "380780",
    "1483487",
    "110490",
    "21042065",
    "1047983",
    "218",
    "442731",
    "1141065",
    "1400574",
    "267301",
    "1492897",
    "786030",
    "324175",
    "471430",
    "1128013",
    "1017034",
    "1474903",
    "597965",
    "21023141",
    "703073",
    "606871",
    "111681",
    "502241",
    "1353141",
    "647627",
    "22253513",
    "1494801",
    "912348",
    "806872",
    "1314252",
    "860320",
    "1285359",
    "1320707",
    "47061",
    "1489790",
    "828625",
    "1398143",
    "279267",
    "762166",
    "604422",
    "21019929",
    "196290",
    "20924380",
    "508665",
    "89998",
    "1311350",
    "22142390",
    "1000539",
    "1146370",
    "1042797",
    "1033568",
    "820133",
    "543448",
    "498179",
    "513937",
    "489711",
    "96737402",
    "367297",
    "268767",
    "20921805",
    "777168",
    "642928",
    "7375",
    "826245",
    "864132",
    "350953",
    "239923",
    "1485785",
    "1037681",
    "775676",
    "1072323",
    "851738",
    "291535",
    "472640",
    "1190765",
    "22255773",
    "1112079",
    "915304",
    "1109560",
    "22178039",
    "513460",
    "608403",
    "538792",
    "420392",
    "576405",
    "554028",
    "254822",
    "1278377",
    "591387",
    "180932",
    "1037635",
    "1106413",
    "739347",
    "409938",
    "1029450",
    "22142913",
    "961524",
    "1058616",
    "351560",
    "494205",
    "22142414",
    "22142648",
    "306545",
    "745033",
    "1006102",
    "600328",
    "184611",
    "1035037",
    "22214941",
    "1070638",
    "22142761",
    "1013753",
    "1417832",
    "449435",
    "8002708",
    "20921455",
    "1092415",
    "1122757",
    "1133445",
    "22142783",
    "903518",
    "1191500",
    "1181876",
    "1302930",
    "491397"
]


# These add 11 rows and assign each subsidiary/brand the parent company's
# workforce demographic and salary-by-group metrics.
# Tuple fields: analysis_firm_key, company_name, parent_company, parent_rcid
inherited_firms = [
    ("subsidiary:kfc", "KFC", "Yum! Brands", "701451"),
    ("subsidiary:taco_bell", "Taco Bell", "Yum! Brands", "701451"),
    ("subsidiary:pizza_hut", "Pizza Hut", "Yum! Brands", "701451"),

    ("subsidiary:longhorn_steakhouse", "Longhorn Steakhouse", "Darden Restaurants", "22142879"),
    ("subsidiary:olive_garden", "Olive Garden", "Darden Restaurants", "22142879"),

    ("subsidiary:pilot_flying_j", "Pilot Flying J", "Berkshire Hathaway", "383823"),
    ("subsidiary:geico", "Geico", "Berkshire Hathaway", "383823"),
    ("subsidiary:mclane_company", "McLane Company", "Berkshire Hathaway", "383823"),
    ("subsidiary:bnsf_railway", "BNSF Railway", "Berkshire Hathaway", "383823"),

    ("subsidiary:pratt_whitney", "Pratt & Whitney", "RTX", "396968"),
    ("subsidiary:collins_aerospace", "Collins Aerospace", "RTX", "396968"),
]


def sql_quote(value):
    """Return a SQL literal for controlled string values used in VALUES CTEs."""
    if value is None:
        return "NULL"
    return "'" + str(value).replace("'", "''") + "'"


target_rcids_values = ",\n        ".join(
    f"({sql_quote(rcid)})" for rcid in target_rcids
)

inherited_firms_values = ",\n        ".join(
    "("
    + ", ".join(sql_quote(x) for x in row)
    + ")"
    for row in inherited_firms
)


sql = f"""
WITH target_rcids(rcid) AS (
    VALUES
        {target_rcids_values}
),

inherited_firms(
    analysis_firm_key,
    company_name,
    parent_company,
    workforce_source_rcid
) AS (
    VALUES
        {inherited_firms_values}
),

/*
   source_rcids are the RCIDs actually used to pull workforce data.

   Direct firms: source RCID = their own company RCID.
   Inherited subsidiary/brand rows: source RCID = parent company RCID.
*/
source_rcids(rcid) AS (
    SELECT rcid FROM target_rcids
    UNION
    SELECT workforce_source_rcid FROM inherited_firms
),

company_names AS (
    SELECT
        cm.rcid::text AS rcid,
        MIN(cm.company::text) AS mapping_company
    FROM revelio_common.company_mapping AS cm
    INNER JOIN source_rcids AS s
        ON cm.rcid = s.rcid::bigint
    WHERE cm.company IS NOT NULL
    GROUP BY
        cm.rcid::text
),

direct_analysis_firms AS (
    SELECT
        'rcid:' || t.rcid AS analysis_firm_key,
        t.rcid AS requested_rcid,
        t.rcid AS workforce_source_rcid,
        COALESCE(cn.mapping_company, t.rcid) AS company,
        COALESCE(cn.mapping_company, t.rcid) AS company_name,
        CASE
            WHEN cn.mapping_company IS NOT NULL THEN 'company_mapping'
            ELSE 'no_name_found'
        END AS company_name_source,
        NULL::text AS parent_company,
        'direct_rcid' AS workforce_match_type
    FROM target_rcids AS t
    LEFT JOIN company_names AS cn
        ON t.rcid = cn.rcid
),

inherited_analysis_firms AS (
    SELECT
        i.analysis_firm_key,
        NULL::text AS requested_rcid,
        i.workforce_source_rcid,
        i.company_name AS company,
        i.company_name AS company_name,
        'manual_subsidiary_parent_map' AS company_name_source,
        i.parent_company,
        'parent_rcid_inherited' AS workforce_match_type
    FROM inherited_firms AS i
),

analysis_firms AS (
    SELECT * FROM direct_analysis_firms
    UNION ALL
    SELECT * FROM inherited_analysis_firms
),

positions_2023 AS (
    /*
       Pull 2023 seniority-1 positions from the RCID that supplies workforce data.
       For direct rows this is the firm RCID. For inherited subsidiary rows this
       is the parent RCID.
    */
    SELECT
        s.rcid AS workforce_source_rcid,
        p.rcid::text AS position_rcid,
        p.user_id::text AS user_id,
        p.position_id,
        p.startdate,
        p.enddate,
        p.seniority,
        p.weight::double precision AS position_weight,

        /*
           Uses the salary variable from revelio_individual.individual_positions.
           Nonpositive values are treated as missing.
        */
        CASE
            WHEN p.salary IS NULL THEN NULL
            WHEN p.salary::double precision <= 0 THEN NULL
            ELSE p.salary::double precision
        END AS position_salary

    FROM revelio_individual.individual_positions AS p
    INNER JOIN source_rcids AS s
        ON p.rcid = s.rcid::bigint
    WHERE DATE '2023-12-31' >= COALESCE(p.startdate, '-infinity'::date)
      AND DATE '2023-01-01' <= COALESCE(p.enddate,   'infinity'::date)
      AND p.seniority = 1

      -- Optional: uncomment if you only want U.S. positions
      -- AND p.country = 'United States'
),

position_summary AS (
    SELECT
        workforce_source_rcid,
        COUNT(*) AS n_position_rows_2023,
        COUNT(DISTINCT position_rcid) AS n_position_rcids_2023,
        COUNT(DISTINCT user_id) AS n_users_from_positions_2023,
        SUM(CASE WHEN position_salary IS NOT NULL THEN 1 ELSE 0 END)
            AS n_position_rows_with_salary_2023
    FROM positions_2023
    GROUP BY
        workforce_source_rcid
),

company_workers_2023 AS (
    /*
       Collapse to one row per source RCID x user_id.
       This avoids double-counting a worker with multiple relevant positions at
       the same RCID in 2023.

       worker_salary is the worker-level mean of nonmissing positive salary
       across their relevant 2023 position rows at that RCID.
    */
    SELECT
        workforce_source_rcid,
        user_id,
        AVG(position_weight) FILTER (WHERE position_weight IS NOT NULL) AS worker_weight,
        AVG(position_salary) FILTER (WHERE position_salary IS NOT NULL) AS worker_salary,
        COUNT(DISTINCT position_id) AS n_positions_2023,
        COUNT(position_salary) AS n_salary_position_rows_2023
    FROM positions_2023
    GROUP BY
        workforce_source_rcid,
        user_id
),

matched_workers AS (
    SELECT
        cw.workforce_source_rcid,
        cw.user_id,
        cw.n_positions_2023,
        cw.n_salary_position_rows_2023,

        /*
           If weight is missing, fall back to 1 for that worker.
           Output includes n_missing_weight for diagnostics.
        */
        COALESCE(cw.worker_weight, 1.0) AS w,
        CASE WHEN cw.worker_weight IS NULL THEN 1 ELSE 0 END AS missing_weight,

        cw.worker_salary,

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

workforce_metrics AS (
    SELECT
        workforce_source_rcid,

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
            / NULLIF(SUM(w) FILTER (WHERE has_ethnicity_probs = 1), 0) AS multiple_share,

        /*
           Salary diagnostics and probability-weighted conditional salary means.

           Example:
             avg_salary_female =
               SUM(worker_weight * salary * Pr(female))
               / SUM(worker_weight * Pr(female))
        */
        SUM(CASE WHEN worker_salary IS NOT NULL THEN 1 ELSE 0 END) AS n_users_with_salary,
        SUM(w) FILTER (WHERE worker_salary IS NOT NULL) AS weighted_n_salary,

        AVG(worker_salary) FILTER (WHERE worker_salary IS NOT NULL) AS unweighted_avg_salary_all,

        SUM(w * worker_salary) FILTER (WHERE worker_salary IS NOT NULL)
            / NULLIF(SUM(w) FILTER (WHERE worker_salary IS NOT NULL), 0) AS avg_salary_all,

        SUM(w * f_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_gender_probs = 1
        ) AS weighted_salary_denominator_female,

        SUM(w * m_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_gender_probs = 1
        ) AS weighted_salary_denominator_male,

        SUM(w * worker_salary * f_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_gender_probs = 1
        ) / NULLIF(
            SUM(w * f_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_gender_probs = 1
            ),
            0
        ) AS avg_salary_female,

        SUM(w * worker_salary * m_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_gender_probs = 1
        ) / NULLIF(
            SUM(w * m_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_gender_probs = 1
            ),
            0
        ) AS avg_salary_male,

        SUM(w * white_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_white,

        SUM(w * black_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_black,

        SUM(w * api_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_api,

        SUM(w * hispanic_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_hispanic,

        SUM(w * native_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_native,

        SUM(w * multiple_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) AS weighted_salary_denominator_multiple,

        SUM(w * worker_salary * white_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * white_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_white,

        SUM(w * worker_salary * black_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * black_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_black,

        SUM(w * worker_salary * api_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * api_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_api,

        SUM(w * worker_salary * hispanic_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * hispanic_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_hispanic,

        SUM(w * worker_salary * native_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * native_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_native,

        SUM(w * worker_salary * multiple_prob) FILTER (
            WHERE worker_salary IS NOT NULL
              AND has_ethnicity_probs = 1
        ) / NULLIF(
            SUM(w * multiple_prob) FILTER (
                WHERE worker_salary IS NOT NULL
                  AND has_ethnicity_probs = 1
            ),
            0
        ) AS avg_salary_multiple

    FROM matched_workers
    GROUP BY
        workforce_source_rcid
),

individual_diversity_inclusion_ratings AS (
    /*
       This is not revelio_sentiment.sentiment_scores. It keeps your prior
       individual-review D&I rating measure, now keyed on workforce_source_rcid
       so inherited rows get the parent company's 2023 D&I review average.
    */
    SELECT
        sir.rcid::text AS workforce_source_rcid,
        COUNT(*) AS n_di_rating_reviews_current_ft_seniority1_2023,
        AVG(sir.rating_diversity_and_inclusion::double precision)
            AS avg_di_rating_current_ft_seniority1_2023
    FROM revelio_sentiment.sentiment_individual_reviews AS sir
    INNER JOIN source_rcids AS s
        ON sir.rcid = s.rcid::bigint
    WHERE sir.review_date BETWEEN DATE '2023-01-01' AND DATE '2023-12-31'
      AND TRIM(sir.seniority::text) = '1'
      AND (
          LOWER(TRIM(sir.reviewer_current_job::text)) IN (
              'true',
              't',
              '1',
              'yes',
              'y'
          )
          OR POSITION('current' IN LOWER(TRIM(sir.reviewer_current_job::text))) > 0
      )
      AND (
          LOWER(TRIM(sir.reviewer_employment_status::text)) IN (
              'regular',
              'ft'
          )
          OR (
              POSITION('full' IN LOWER(TRIM(sir.reviewer_employment_status::text))) > 0
              AND POSITION('time' IN LOWER(TRIM(sir.reviewer_employment_status::text))) > 0
          )
      )
      AND sir.rating_diversity_and_inclusion::double precision BETWEEN 1 AND 5
    GROUP BY
        sir.rcid::text
)

SELECT
    af.analysis_firm_key,
    af.requested_rcid,
    af.workforce_source_rcid,
    source_cn.mapping_company AS workforce_source_company,

    af.company,
    af.company_name,
    af.company_name_source,
    af.parent_company,
    af.workforce_match_type,

    CASE
        WHEN wm.workforce_source_rcid IS NOT NULL THEN 1
        ELSE 0
    END AS has_2023_worker_match,

    ps.n_position_rows_2023,
    ps.n_position_rcids_2023,
    ps.n_users_from_positions_2023,
    ps.n_position_rows_with_salary_2023,

    wm.n_unique_users_matched_to_user_file,
    wm.weighted_n_users,
    wm.n_missing_weight,
    wm.n_users_with_gender_probs,
    wm.n_users_with_ethnicity_probs,
    wm.weighted_n_gender,
    wm.weighted_n_ethnicity,

    wm.female_share,
    wm.male_share,
    wm.white_share,
    wm.black_share,
    wm.api_share,
    wm.hispanic_share,
    wm.native_share,
    wm.multiple_share,

    wm.n_users_with_salary,
    wm.weighted_n_salary,
    wm.unweighted_avg_salary_all,
    wm.avg_salary_all,

    wm.weighted_salary_denominator_female,
    wm.weighted_salary_denominator_male,
    wm.avg_salary_female,
    wm.avg_salary_male,

    wm.weighted_salary_denominator_white,
    wm.weighted_salary_denominator_black,
    wm.weighted_salary_denominator_api,
    wm.weighted_salary_denominator_hispanic,
    wm.weighted_salary_denominator_native,
    wm.weighted_salary_denominator_multiple,

    wm.avg_salary_white,
    wm.avg_salary_black,
    wm.avg_salary_api,
    wm.avg_salary_hispanic,
    wm.avg_salary_native,
    wm.avg_salary_multiple,

    idir.n_di_rating_reviews_current_ft_seniority1_2023,
    idir.avg_di_rating_current_ft_seniority1_2023

FROM analysis_firms AS af
LEFT JOIN company_names AS source_cn
    ON af.workforce_source_rcid = source_cn.rcid
LEFT JOIN position_summary AS ps
    ON af.workforce_source_rcid = ps.workforce_source_rcid
LEFT JOIN workforce_metrics AS wm
    ON af.workforce_source_rcid = wm.workforce_source_rcid
LEFT JOIN individual_diversity_inclusion_ratings AS idir
    ON af.workforce_source_rcid = idir.workforce_source_rcid
ORDER BY
    af.company NULLS LAST,
    af.analysis_firm_key;
"""


full_target_rcids_cte_body = "VALUES\n        " + target_rcids_values
full_inherited_firms_cte_body = "VALUES\n        " + inherited_firms_values


def target_rcids_cte_body(rcids):
    if rcids:
        return "VALUES\n        " + ",\n        ".join(
            f"({sql_quote(rcid)})" for rcid in rcids
        )
    return "SELECT NULL::text AS rcid WHERE FALSE"


def inherited_firms_cte_body(rows):
    if rows:
        return "VALUES\n        " + ",\n        ".join(
            "(" + ", ".join(sql_quote(x) for x in row) + ")"
            for row in rows
        )
    return (
        "SELECT NULL::text AS analysis_firm_key,\n"
        "        NULL::text AS company_name,\n"
        "        NULL::text AS parent_company,\n"
        "        NULL::text AS workforce_source_rcid\n"
        "    WHERE FALSE"
    )


def render_sql(rcids=None, inherited_rows=None):
    if rcids is None:
        rcids = target_rcids
    if inherited_rows is None:
        inherited_rows = inherited_firms

    return (
        sql.replace(full_target_rcids_cte_body, target_rcids_cte_body(rcids), 1)
        .replace(
            full_inherited_firms_cte_body,
            inherited_firms_cte_body(inherited_rows),
            1,
        )
    )


def chunked(values, chunk_size):
    for start in range(0, len(values), chunk_size):
        yield values[start:start + chunk_size]


def query_batches(batch_size):
    if batch_size is None or batch_size <= 0:
        return [("all", target_rcids, inherited_firms)]

    batches = [
        (f"direct_{batch_number:03d}", rcid_batch, [])
        for batch_number, rcid_batch in enumerate(chunked(target_rcids, batch_size), 1)
    ]

    if inherited_firms:
        batches.append(("inherited", [], inherited_firms))

    return batches


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        description=(
            "Pull Revelio 2023 race/gender shares and salary-by-group metrics "
            "from WRDS, with manual parent-RCID inheritance for selected "
            "subsidiary/brand firms."
        )
    )
    parser.add_argument(
        "--wrds-username",
        default=default_wrds_username,
        help=(
            "WRDS username. Defaults to WRDS_USERNAME if set; otherwise to "
            "wrds_username from code/globals.py. If both are missing, the wrds "
            "package will prompt and may default to your operating-system "
            "username."
        ),
    )
    parser.add_argument(
        "--output",
        default=None,
        help=(
            "CSV output path. Defaults to "
            f"{DEFAULT_OUTPUT_PATH}, using the data/external path from "
            "code/globals.py."
        ),
    )
    parser.add_argument(
        "--save-sql",
        default=None,
        help="Optional path to write the generated SQL before running it.",
    )
    parser.add_argument(
        "--sql-only",
        action="store_true",
        help="Write/show the SQL and exit without connecting to WRDS.",
    )
    parser.add_argument(
        "--create-pgpass-after-connect",
        action="store_true",
        help=(
            "After connecting, call conn.create_pgpass_file() and exit. This is "
            "optional; many wrds versions already offer to create pgpass during "
            "the first successful connection."
        ),
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=1,
        help=(
            "Number of direct RCIDs to query per WRDS batch. Default: 1. "
            "Use 0 to run the original single large query."
        ),
    )
    parser.add_argument(
        "--batch-dir",
        default=None,
        help=(
            "Directory for batch checkpoint CSVs. Defaults to a sibling "
            "directory next to the output CSV."
        ),
    )
    parser.add_argument(
        "--rerun-batches",
        action="store_true",
        help="Ignore existing batch checkpoint CSVs and rerun every batch.",
    )
    parser.add_argument(
        "--batch-retries",
        type=int,
        default=1,
        help=(
            "Number of retry attempts for a failed batch after the first try. "
            "Each retry opens a fresh WRDS connection. Default: 1."
        ),
    )
    return parser.parse_args(argv)


def import_wrds_or_exit():
    try:
        import wrds  # type: ignore
    except ModuleNotFoundError:
        print("WRDS Python package not found; installing it into this Python environment...")
        try:
            ensure_python_packages(["wrds"])
            import wrds  # type: ignore
        except Exception as exc:
            print(
                "Could not import or install the WRDS Python package. "
                "You can install it manually with:\n\n"
                f"    {sys.executable} -m pip install --upgrade pip setuptools wheel\n"
                f"    {sys.executable} -m pip install wrds\n",
                file=sys.stderr,
            )
            print(f"Install/import error: {exc}", file=sys.stderr)
            raise SystemExit(1)
    return wrds


def import_pandas_or_exit():
    try:
        import pandas as pd  # type: ignore
    except ModuleNotFoundError:
        print("pandas not found; installing it into this Python environment...")
        try:
            ensure_python_packages(["pandas"])
            import pandas as pd  # type: ignore
        except Exception as exc:
            print(
                "Could not import or install pandas. You can install it manually with:\n\n"
                f"    {sys.executable} -m pip install pandas\n",
                file=sys.stderr,
            )
            print(f"Install/import error: {exc}", file=sys.stderr)
            raise SystemExit(1)
    return pd


def make_connection(wrds_username=None):
    """
    Open a WRDS connection.

    Password handling is delegated to the wrds package/PostgreSQL:
      * If a valid pgpass file exists, it can use that saved password.
      * Otherwise, it should prompt interactively for your WRDS password.
      * This script intentionally does not accept or store a password.
    """
    wrds = import_wrds_or_exit()

    kwargs = {}
    if wrds_username:
        kwargs["wrds_username"] = wrds_username

    try:
        return wrds.Connection(**kwargs)
    except KeyboardInterrupt:
        print("\nConnection cancelled by user.", file=sys.stderr)
        raise SystemExit(130)
    except Exception as exc:
        print("\nWRDS connection failed.", file=sys.stderr)
        print(f"Error type: {type(exc).__name__}", file=sys.stderr)
        print(f"Error message: {exc}", file=sys.stderr)
        print(
            "\nThings to check:\n"
            "  1. Your WRDS username is correct. You can pass it with "
            "--wrds-username or set WRDS_USERNAME.\n"
            "  2. You can log in to WRDS in a browser with the same credentials.\n"
            "  3. Your terminal can accept interactive password input and Duo/MFA.\n"
            "  4. If you use pgpass, the file permissions/location are correct.\n",
            file=sys.stderr,
        )
        raise SystemExit(1)


def close_connection(conn):
    try:
        conn.close()
    except Exception:
        pass


def run_sql_with_fresh_connection(sql_text, wrds_username):
    conn = make_connection(wrds_username)
    try:
        return conn.raw_sql(sql_text)
    finally:
        close_connection(conn)


def run_revelio_query(args, out_file):
    pd = import_pandas_or_exit()
    batches = query_batches(args.batch_size)

    if len(batches) == 1 and batches[0][0] == "all":
        print("Running Revelio query as one full WRDS query...")
        return run_sql_with_fresh_connection(render_sql(), args.wrds_username)

    batch_dir = (
        Path(args.batch_dir).expanduser()
        if args.batch_dir
        else out_file.parent / f"{out_file.stem}_batches_size{args.batch_size}"
    )
    batch_dir.mkdir(parents=True, exist_ok=True)

    print(
        f"Running Revelio query in {len(batches)} batches "
        f"with checkpoints in {batch_dir}"
    )

    frames = []
    for batch_index, (batch_label, batch_rcids, batch_inherited) in enumerate(batches, 1):
        batch_path = batch_dir / f"{batch_index:03d}_{batch_label}.csv"

        if batch_path.exists() and not args.rerun_batches:
            print(f"Using existing batch {batch_index}/{len(batches)}: {batch_path}")
            frames.append(
                pd.read_csv(
                    batch_path,
                    dtype={
                        "analysis_firm_key": "string",
                        "requested_rcid": "string",
                        "workforce_source_rcid": "string",
                    },
                )
            )
            continue

        print(
            f"Running batch {batch_index}/{len(batches)} ({batch_label}): "
            f"{len(batch_rcids)} direct RCIDs, {len(batch_inherited)} inherited rows"
        )
        batch_sql = render_sql(batch_rcids, batch_inherited)
        batch_df = None
        for attempt in range(1, args.batch_retries + 2):
            try:
                if attempt > 1:
                    print(
                        f"Retrying batch {batch_index}/{len(batches)} "
                        f"({batch_label}), attempt {attempt}"
                    )
                batch_df = run_sql_with_fresh_connection(
                    batch_sql,
                    args.wrds_username,
                )
                break
            except Exception as exc:
                print(
                    f"\nWRDS query batch failed on attempt {attempt}: {batch_label}",
                    file=sys.stderr,
                )
                print(f"Error type: {type(exc).__name__}", file=sys.stderr)
                print(f"Error message: {exc}", file=sys.stderr)
                if attempt > args.batch_retries:
                    print(f"Completed batch CSVs remain in: {batch_dir}", file=sys.stderr)
                    print(
                        "Rerun the same command to resume from completed batches, "
                        "or pass --rerun-batches to ignore checkpoints.",
                        file=sys.stderr,
                    )
                    raise

        if batch_df is None:
            raise RuntimeError(f"Batch did not return data: {batch_label}")

        batch_df.to_csv(batch_path, index=False)
        print(f"Saved batch {batch_index}/{len(batches)} to {batch_path}")
        frames.append(batch_df)

    df = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    if "analysis_firm_key" in df.columns:
        df = df.drop_duplicates(subset=["analysis_firm_key"], keep="last")
    if {"company", "analysis_firm_key"}.issubset(df.columns):
        df = (
            df.sort_values(["company", "analysis_firm_key"], na_position="last")
            .reset_index(drop=True)
        )

    return df


def write_sql_if_requested(save_sql_path):
    if not save_sql_path:
        return
    sql_path = Path(save_sql_path).expanduser()
    if sql_path.parent and str(sql_path.parent) != ".":
        sql_path.parent.mkdir(parents=True, exist_ok=True)
    sql_path.write_text(sql, encoding="utf-8")
    print(f"Wrote SQL to {sql_path}")


def main(argv=None):
    args = parse_args(argv)
    out_file = Path(args.output).expanduser() if args.output else DEFAULT_OUTPUT_PATH

    write_sql_if_requested(args.save_sql)
    if args.sql_only:
        if not args.save_sql:
            print(sql)
        return 0

    print("Connecting to WRDS...")
    if args.wrds_username:
        print(f"Using WRDS username: {args.wrds_username}")
    else:
        print(
            "No --wrds-username or WRDS_USERNAME supplied; "
            "the wrds package will prompt for username/password."
        )
    print("This script does not store or accept a password on the command line.")
    print(f"Output path: {out_file}")

    if args.create_pgpass_after_connect:
        conn = make_connection(args.wrds_username)
        try:
            if not hasattr(conn, "create_pgpass_file"):
                print("This wrds.Connection object does not expose create_pgpass_file().")
                return 1
            conn.create_pgpass_file()
            print("pgpass setup/update step finished. Exiting without running the query.")
            return 0
        finally:
            close_connection(conn)

    df = run_revelio_query(args, out_file)

    if out_file.parent and str(out_file.parent) != ".":
        out_file.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out_file, index=False)

    print(f"Saved {len(df):,} rows to {out_file}")
    print("Direct RCID rows:", int((df["workforce_match_type"] == "direct_rcid").sum()))
    print(
        "Rows using parent-inherited workforce metrics:",
        int((df["workforce_match_type"] == "parent_rcid_inherited").sum())
    )
    print("Rows with no company name:", int(df["company"].isna().sum()))
    print("Rows with no 2023 worker match:", int((df["has_2023_worker_match"] == 0).sum()))
    print("Rows with no salary average:", int(df["avg_salary_all"].isna().sum()))
    print(
        "Rows with no 2023 current full-time seniority-1 D&I rating:",
        int(df["avg_di_rating_current_ft_seniority1_2023"].isna().sum())
    )

    print(df.head())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
