# ------------------------------------------------------------------------------
# Purpose: EIV tables using Revelio workforce composition / sentiment outcomes
#
# This mirrors the OLS/Borda EIV tables, but replaces the experimental contact-gap
# left-hand side with external Revelio firm-level measures.
# ------------------------------------------------------------------------------

source("code/globals.R")
source(file.path(analysis, "eivreg.R"))
source(file.path(analysis, "eiv_functions.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

revelio_file <- "revelio_company_race_gender_sentiment_2023_weighted_by_rcid.csv"

resolve_project_file <- function(primary_path, github_relative_path, label) {
  if (file.exists(primary_path)) return(primary_path)

  fallback_path <- file.path(git_survey_bias_root, github_relative_path)
  if (file.exists(fallback_path)) return(fallback_path)

  stop(label, " not found at either ", primary_path, " or ", fallback_path)
}

revelio_path <- resolve_project_file(
  file.path(external, revelio_file),
  file.path("data", "external", revelio_file),
  "Revelio composition file"
)

survey_path <- resolve_project_file(
  file.path(processed, "long_survey_final.csv"),
  file.path("data", "processed", "long_survey_final.csv"),
  "Analysis survey file"
)

# Set these when testing locally, e.g.
# REVELIO_EIV_TABLES_DIR=output/tables REVELIO_EIV_WRITE_SHEETS=false
tables_out <- Sys.getenv("REVELIO_EIV_TABLES_DIR", unset = tables)
write_eiv_sheets <- tolower(Sys.getenv("REVELIO_EIV_WRITE_SHEETS", unset = "true")) %in%
  c("1", "true", "yes", "y")

default_filemap <- tibble::tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination",
             "40 Years or Older",
             "Less than 40 Years Old"),
  subdir = c("Full_Sample",
             "Subset_Black",
             "Subset_White",
             "Subset_Female",
             "Subset_Male",
             "Subset_Looking",
             "Subset_Not_Looking",
             "Subset_Feared_Discrimination_1",
             "Subset_Feared_Discrimination_0",
             "Subset_Age_gte40",
             "Subset_Age_lt40")
)

normalize_company_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)

  drop_words <- c(
    "inc", "incorporated", "corp", "corporation", "co", "company",
    "companies", "cos", "group", "holdings", "holding", "plc",
    "llc", "ltd", "limited", "the"
  )

  for (word in drop_words) {
    x <- gsub(paste0("\\b", word, "\\b"), " ", x)
  }

  trimws(gsub("\\s+", " ", x))
}

company_aliases <- c(
  "adp" = "automatic data processing",
  "carrier" = "carrier global",
  "costco" = "costco wholesale",
  "disney" = "walt disney",
  "dowdupont" = "dow",
  "dr pepper snapple" = "keurig dr pepper",
  "estee lauder" = "estee lauder",
  "general electric" = "ge aerospace",
  "ibm" = "international business machines",
  "icahn enterprises" = "icahn enterprises lp",
  "jpmorgan chase" = "jpmorgan chase and",
  "laboratory of america" = "labcorp",
  "merck" = "merck and",
  "otis" = "otis worldwide",
  "sears" = "sears roebuck and",
  "synnex" = "td synnex",
  "united continental" = "united airlines",
  "ups" = "united parcel service",
  "verizon" = "verizon communications",
  "victoria s secret" = "victoria s secret and",
  "wells fargo" = "wells fargo and",
  "xpo logistics" = "xpo"
)

apply_company_aliases <- function(key) {
  out <- key
  matched <- key %in% names(company_aliases)
  out[matched] <- unname(company_aliases[key[matched]])
  out
}

revelio <- readr::read_csv(revelio_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    revelio_key_company = apply_company_aliases(normalize_company_name(.data$company)),
    revelio_key_company_name = apply_company_aliases(normalize_company_name(.data$company_name))
  )

revelio_lookup <- dplyr::bind_rows(
  revelio %>% dplyr::mutate(revelio_key = .data$revelio_key_company),
  revelio %>% dplyr::mutate(revelio_key = .data$revelio_key_company_name)
) %>%
  dplyr::filter(!is.na(.data$revelio_key), nzchar(.data$revelio_key)) %>%
  dplyr::select(
    revelio_key,
    rcid,
    revelio_company = company,
    female_share,
    black_share,
    div_and_inclusion_sentiment
  ) %>%
  dplyr::distinct()

dupe_lookup <- revelio_lookup %>%
  dplyr::distinct(.data$revelio_key, .data$rcid) %>%
  dplyr::count(.data$revelio_key, name = "n_rcid") %>%
  dplyr::filter(.data$n_rcid > 1)

if (nrow(dupe_lookup) > 0) {
  stop("Duplicate normalized Revelio lookup keys: ", paste(dupe_lookup$revelio_key, collapse = ", "))
}

survey_industry_map <- readr::read_csv(survey_path, show_col_types = FALSE) %>%
  dplyr::transmute(
    entity_id = as.integer(.data$firm_id),
    aer_naics2 = suppressWarnings(as.integer(.data$aer_naics2))
  ) %>%
  dplyr::distinct()

make_revelio_firm_map <- function(coef_firm_wide) {
  firm_names <- coef_firm_wide %>%
    dplyr::distinct(entity_id, entity) %>%
    dplyr::mutate(
      survey_key = apply_company_aliases(normalize_company_name(.data$entity))
    )

  out <- firm_names %>%
    dplyr::left_join(
      revelio_lookup,
      by = c("survey_key" = "revelio_key")
    )

  unmatched <- out %>%
    dplyr::filter(is.na(.data$rcid)) %>%
    dplyr::arrange(.data$entity)

  if (nrow(unmatched) > 0) {
    message(
      "Revelio unmatched firms (kept as missing LHS): ",
      paste(unmatched$entity, collapse = "; ")
    )
  }

  out %>%
    dplyr::select(
      entity_id,
      female_share,
      black_share,
      div_and_inclusion_sentiment
    )
}

to_wide_by_outcome <- function(coef_long) {
  coef_long %>%
    dplyr::filter(.data$subset == "subset97",
                  .data$entity_type == "Firm",
                  .data$model %in% c("OLS", "Borda")) %>%
    dplyr::select(model, entity_id, entity, outcome, estimate, njobs) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = c(model, entity_id, entity, njobs),
      names_from = outcome,
      values_from = estimate
    )
}

rhs_outcomes <- c(
  "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male",
  "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"
)

regs_revelio <- list(
  list(lhs = "female_share", rhs = c("FirmCont_favor_male")),
  list(lhs = "female_share", rhs = c("conduct_favor_male")),
  list(lhs = "female_share", rhs = c("pooled_favor_male")),
  list(lhs = "black_share", rhs = c("FirmCont_favor_white")),
  list(lhs = "black_share", rhs = c("conduct_favor_white")),
  list(lhs = "black_share", rhs = c("pooled_favor_white")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("FirmCont_favor_male")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("conduct_favor_male")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("pooled_favor_male")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("FirmCont_favor_white")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("conduct_favor_white")),
  list(lhs = "div_and_inclusion_sentiment", rhs = c("pooled_favor_white"))
)

run_revelio_eiv_for_subdir <- function(subdir) {
  dir_path <- file.path(intermediate, subdir)
  if (!dir.exists(dir_path)) {
    warning("Skipping missing intermediate directory: ", dir_path)
    return(tibble::tibble())
  }

  coef_long <- read_parquet_sheet(dir_path, "Coefficients")
  variance_df <- read_parquet_sheet(dir_path, "variance")
  covariance_df <- read_parquet_sheet(dir_path, "covariance")

  coef_firm_wide <- to_wide_by_outcome(coef_long) %>%
    dplyr::left_join(survey_industry_map, by = "entity_id") %>%
    dplyr::left_join(make_revelio_firm_map(.), by = "entity_id")

  models_to_run <- c("OLS", "Borda")
  noise_mats <- setNames(vector("list", length(models_to_run)), models_to_run)

  for (model in models_to_run) {
    noise_mats[[model]] <- build_noise_matrix(
      variance_df = variance_df,
      covariance_df = covariance_df,
      outcomes = rhs_outcomes,
      subset_value = "subset97",
      model_value = model
    )
  }

  eiv_df <- run_eiv_suite(
    regs = regs_revelio,
    coef_df_wide = coef_firm_wide,
    noise_mats_97 = noise_mats,
    models = models_to_run,
    id_col = "entity_id",
    model_col = "model",
    fe_col = "aer_naics2",
    weights_col = "njobs",
    use_fe = TRUE
  )

  if (isTRUE(write_eiv_sheets)) {
    write_parquet_sheet(dir_path, "EIV_revelio_firm", eiv_df)
  }

  eiv_df
}

pull_est_from_df <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num) {
  if (!nrow(eiv_df)) return("NA (NA)")

  eiv_df$coef <- suppressWarnings(as.numeric(eiv_df$coef))

  out <- eiv_df %>%
    dplyr::filter(
      .data$model == model_filter,
      .data$lhs == lhs_var,
      .data$rhs == rhs_var,
      .data$coef == coef_num
    )

  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est[1]))
  se <- suppressWarnings(as.numeric(out$sample_se[1]))

  paste0(fmt3(est), " (", fmt3(se), ")")
}

build_panel_df <- function(eiv_by_subdir, model_filter, lhs, rhs_contact,
                           rhs_conduct, rhs_pooled,
                           filemap = default_filemap) {
  filemap %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `(1) Contact` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                       lhs, rhs_contact, model_filter, 1L),
      `(2) Contact (Industry FE)` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                                     lhs, rhs_contact, model_filter, 2L),
      `(3) Conduct` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                       lhs, rhs_conduct, model_filter, 1L),
      `(4) Conduct (Industry FE)` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                                     lhs, rhs_conduct, model_filter, 2L),
      `(5) Pooled` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                      lhs, rhs_pooled, model_filter, 1L),
      `(6) Pooled (Industry FE)` = pull_est_from_df(eiv_by_subdir[[.data$subdir]] %||% tibble::tibble(),
                                                    lhs, rhs_pooled, model_filter, 2L)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-subdir)
}

write_two_panel_table <- function(eiv_by_subdir, lhs, rhs_contact, rhs_conduct,
                                  rhs_pooled, out_file) {
  df_likert <- build_panel_df(
    eiv_by_subdir = eiv_by_subdir,
    model_filter = "OLS",
    lhs = lhs,
    rhs_contact = rhs_contact,
    rhs_conduct = rhs_conduct,
    rhs_pooled = rhs_pooled
  )

  df_borda <- build_panel_df(
    eiv_by_subdir = eiv_by_subdir,
    model_filter = "Borda",
    lhs = lhs,
    rhs_contact = rhs_contact,
    rhs_conduct = rhs_conduct,
    rhs_pooled = rhs_pooled
  )

  common_cols <- colnames(df_likert)
  combined_df <- dplyr::bind_rows(df_likert[, common_cols], df_borda[, common_cols])

  n_likert <- nrow(df_likert)
  n_borda <- nrow(df_borda)

  tex_code <- knitr::kable(
    combined_df,
    format = "latex",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(combined_df) - 1)),
    col.names = common_cols,
    linesep = "",
    escape = FALSE
  ) %>%
    kableExtra::pack_rows("Panel A: Likert", 1, n_likert) %>%
    kableExtra::pack_rows("Panel B: Borda", n_likert + 1, n_likert + n_borda)

  out_tex <- file.path(tables_out, out_file)
  write_lines_checked(tex_code, out_tex, label = "Revelio EIV LaTeX table")
  message("Saved: ", out_tex)
}

message("Reading Revelio measures from: ", revelio_path)
message("Reading EIV inputs from: ", intermediate)
message("Writing tables to: ", tables_out)
message("Write EIV_revelio_firm parquet sheets: ", write_eiv_sheets)

eiv_by_subdir <- setNames(
  lapply(default_filemap$subdir, run_revelio_eiv_for_subdir),
  default_filemap$subdir
)

write_two_panel_table(
  eiv_by_subdir = eiv_by_subdir,
  lhs = "female_share",
  rhs_contact = "FirmCont_favor_male",
  rhs_conduct = "conduct_favor_male",
  rhs_pooled = "pooled_favor_male",
  out_file = "EIV_revelio_female_share_gender_ols_borda.tex"
)

write_two_panel_table(
  eiv_by_subdir = eiv_by_subdir,
  lhs = "black_share",
  rhs_contact = "FirmCont_favor_white",
  rhs_conduct = "conduct_favor_white",
  rhs_pooled = "pooled_favor_white",
  out_file = "EIV_revelio_black_share_race_ols_borda.tex"
)

write_two_panel_table(
  eiv_by_subdir = eiv_by_subdir,
  lhs = "div_and_inclusion_sentiment",
  rhs_contact = "FirmCont_favor_male",
  rhs_conduct = "conduct_favor_male",
  rhs_pooled = "pooled_favor_male",
  out_file = "EIV_revelio_div_and_inclusion_sentiment_gender_ols_borda.tex"
)

write_two_panel_table(
  eiv_by_subdir = eiv_by_subdir,
  lhs = "div_and_inclusion_sentiment",
  rhs_contact = "FirmCont_favor_white",
  rhs_conduct = "conduct_favor_white",
  rhs_pooled = "pooled_favor_white",
  out_file = "EIV_revelio_div_and_inclusion_sentiment_race_ols_borda.tex"
)

message("Revelio EIV composition tables complete.")
