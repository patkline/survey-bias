# ------------------------------------------------------------------------------
# One-stop summary runner
# - Reads long_survey_final.csv
# - Sources summary_* scripts
# - Runs: ratings plots, yes/no plots, summary tables, duration hist
# - Adds two-way bar charts (Gender×Race, Looking×Race)
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

## Source helper functions

# plot_rating_1to5_restrict()
source(file.path(helper_functions, "summary_figures_ratings.R"))

# plot_yes_no_pna_both()
source(file.path(helper_functions, "summary_figures_yes_no.R"))

# make_summary_table() 
source(file.path(helper_functions, "summary_statistics_new.R"))

# make_response_duration_hist()
source(file.path(helper_functions, "response_duration_hist.R")) 

# two_way_bar()
source(file.path(helper_functions, "summary_two_way_bar.R"))

# make_confidence_table()
source(file.path(helper_functions, "summary_figures_confidence.R"))

# make_information_source_hist()
source(file.path(helper_functions, "summary_figure_info_source.R"))

#if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
#if (!dir.exists(figures))  dir.create(figures,  recursive = TRUE)

# --- Source helper scripts ---------------------------------------
#src <- function(fn) {
#  p <- file.path(code, fn)
#  if (file.exists(p)) {
#    message("Sourcing: ", p)
#    source(p)
#  } else stop("Required file not found: ", p)
#}
# Expect these files to define the functions shown in your snippets


# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

# Assert data file exists
stopifnot(file.exists(file.path(processed, "long_survey_final_summary_stats.csv")))

# Load data 
data <- read.csv(file.path(processed, "long_survey_final_summary_stats.csv"), stringsAsFactors = FALSE)

# Check data is unique on ResponseId x firm
stopifnot(nrow(data) == length(unique(paste(data$ResponseId, data$option_number))))

# ------------------------------------------------------------------------------
# xx
# ------------------------------------------------------------------------------
# --- 1) Ratings (1–5) plots --------------------------------------
stopifnot(exists("plot_rating_1to5_restrict"))

out_png <- function(name) file.path(figures, paste0(name, ".png"))

rating_calls <- list(
  list(var = "FirmCont_white",  out = "FirmCont_favor_white_share"),
  list(var = "FirmCont_black",  out = "FirmCont_favor_black_share"),
  list(var = "FirmHire_white",  out = "FirmHire_favor_white_share"),
  list(var = "FirmHire_black",  out = "FirmHire_favor_black_share"),
  list(var = "FirmCont_male",   out = "FirmCont_favor_male_share"),
  list(var = "FirmCont_female", out = "FirmCont_favor_female_share"),
  list(var = "FirmHire_male",   out = "FirmHire_favor_male_share"),
  list(var = "FirmHire_female", out = "FirmHire_favor_female_share"),
  list(var = "conduct_white",   out = "conduct_white_share"),
  list(var = "conduct_black",   out = "conduct_black_share"),
  list(var = "conduct_female",  out = "conduct_female_share"),
  list(var = "conduct_male",    out = "conduct_male_share"),
  list(var = "conduct_older",   out = "conduct_older_share"),
  list(var = "conduct_younger", out = "conduct_younger_share")
)

for (rc in rating_calls) {
  if (rc$var %in% names(data)) {
    message("Plotting ratings for: ", rc$var)
    
    # conduct_* → "level" labels; everything else → "gap" labels
    label_version <- if (startsWith(rc$var, "conduct_")) "level" else "gap"
    
    plot_rating_1to5_restrict(
      df = data,
      var = rc$var,
      id_var = "ResponseId",
      label_version = label_version,
      outfile = out_png(rc$out),
      ymax = 60
    )
  } else {
    message("Skipping missing rating var: ", rc$var)
  }
}

# ------------------------------------------------------------------------------
# xx
# ------------------------------------------------------------------------------
# --- 2) Yes / No / Prefer-not plots --------------------------------
stopifnot(exists("plot_yes_no_pna_both"))

if ("any_entry_lev_exp" %in% names(data)) {
  plot_yes_no_pna_both(
    df = data,
    var = "any_entry_lev_exp",
    outfile_base = file.path(figures, "any_entry_lev_exp")
  )
} else message("Missing var: any_entry_lev_exp (skipping)")

if ("feared_discrim" %in% names(data)) {
  plot_yes_no_pna_both(
    df = data,
    var = "feared_discrim",
    outfile_base = file.path(figures, "feared_discrim")
  )
} else message("Missing var: feared_discrim (skipping)")

# --- 3) Summary tables --------------------------------------------
stopifnot(exists("make_summary_table"))
temp_data <- data %>% mutate(gender = ifelse(gender == 1, "Female", "Male"))

# (a) Probability vs Convenience
if ("sample" %in% names(data)) {
  make_summary_table(
    df         = temp_data,
    sample_var = "sample",
    label0     = "Convenience",
    label1     = "Probability",
    outfile    = file.path(figures, "summary_prob_conv.tex")
  )
} else {
  message("Missing 'sample' column for prob/conv table (skipping)")
}

# (b) Names vs Conduct split (recreate 'sample' as in your code)
if ("conduct_white" %in% names(data)) {
  alt_data <- temp_data %>%
    dplyr::select(-dplyr::any_of("sample")) %>%
    dplyr::mutate(sample = ifelse(is.na(conduct_white), 1, 0))
  make_summary_table(
    df         = alt_data,
    sample_var = "sample",
    label0     = "Conduct",
    label1     = "Names",
    outfile    = file.path(figures, "summary_cond_names.tex")
  )
} else {
  message("Missing 'conduct_white' for Names vs Conduct table (skipping)")
}

# --- 4) Response duration histogram --------------------------------
stopifnot(exists("make_response_duration_hist"))
make_response_duration_hist(
  data    = data,
  outfile = file.path(figures, "response_duration_hist.png")
)

# --- 5) Two-way bar charts (NEW) -----------------------------------
stopifnot(exists("two_way_bar"))

# Clean labels for plotting
plot_data <- data %>%
  mutate(
    gender      = ifelse(gender == 1, "Female", "Male"),
    looking_job = ifelse(looking_job == 1, "Looking for Job", "Not Looking")
  )

# (a) Split by Race within Gender
two_way_bar(
  plot_data,
  x = gender,
  group = race_recode,
  y = fear,                          # 0/1 indicator
  percent = TRUE,
  x_lab = "Gender",
  y_lab = "Share of Respondents (%)",
  legend_title = "Race",
  title = "",
  group_order = c("Black", "White", "Other"),
  outfile = file.path(figures, "twoway_gender_by_race_fear.png")
)

two_way_bar(
  plot_data,
  x = looking_job,
  group = race_recode,
  y = fear,                          # 0/1 indicator
  percent = TRUE,
  x_lab = "Looking for Job",
  y_lab = "Share of Respondents (%)",
  legend_title = "Race",
  title = "",
  group_order = c("Black", "White", "Other"),
  outfile = file.path(figures, "twoway_looking_by_race_fear.png")
)


res <- make_confidence_table(
  df      = data,
  id_var  = "ResponseId",
  out_tex = file.path(paste0(figures, "/confidence_conf_table.tex"))
)

# --- 6) Information source histogram (conduct arm only) ----------
make_information_source_hist(
  df      = data,
  id_var  = "ResponseId",
  outfile = file.path(figures, "information_source_hist.png")
)




message("✅ Summary bundle complete. Outputs in: ", normalizePath(figures))
