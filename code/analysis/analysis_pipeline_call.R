# 
# ## 0) parse args -------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript run_wrapper.R <subset_var> <subset_value> <output_filename>")
}

subset_var      <- args[[1]]                   # still a string
subset_value    <- as.integer(args[[2]])       # coerce to integer
if (is.na(subset_value)) {
  stop("`<subset_value>` must be an integer")
}
output_filename <- args[[3]]                   # still a string

## ------------------------------------------------------------------
## Use Posit Package Manager – Ubuntu 24.04 (noble) binary channel
## ------------------------------------------------------------------
## 1. Point to the Posit mirror (but don’t force binary)
# options(repos = c(PPM = "https://packagemanager.posit.co/cran/latest"))
# 
# ## 2. Install-if-missing
need <- c("dplyr","tibble","tidyr",
          "sandwich","prefmod","readxl","openxlsx","ggplot2","writexl","PlackettLuce")
for (pkg in need) {
  library(pkg, character.only = TRUE)
}

# 
# user_lib <- Sys.getenv("R_LIBS_USER")
# if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
# 
# for (pkg in need) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     install.packages(pkg, lib = user_lib, dependencies = TRUE)
#   }
#   library(pkg, character.only = TRUE)
# }
# 
# # absolute path to the file you downloaded or scp’d to the cluster
# pkgfile <- "~/Documents/consolidated_code_server/code/PlackettLuce_0.4.3.tar.gz"
# 
# # pick a personal library (create if missing)
# user_lib <- Sys.getenv("R_LIBS_USER")
# if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
# 
# # install, letting R build from source
# install.packages(pkgfile,
#                  repos        = NULL,     # <-- local file, not CRAN
#                  type         = "source",
#                  lib          = user_lib,
#                  dependencies = TRUE)     # pull any missing R-level deps


## Read in Data
dir <- "~/Documents/consolidated_code_server/processed/"
excel_dir <- "~/Documents/consolidated_code_server/excel/"
file_path <- file.path(dir, "long_survey_final.csv")
source("~/Documents/consolidated_code_server/code/analysis_pipeline_v2.R")
source("~/Documents/consolidated_code_server/code/1_preprocessing_v3.R")
source("~/Documents/consolidated_code_server/code/pm_calc.R")

# Create Necessary Variables
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Define outcomes and relevant columns
survey_vars <- c("FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
                 "FirmCont_favor_male", "FirmHire_favor_male", "conduct_favor_male",
                 "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire",
                 "conduct_black","pooled_favor_white","pooled_favor_male")
experimental_vars <- c("dif", "log_dif", "dif_gender", "log_dif_gender", "dif_age", "log_dif_age", "cb_central_full")
respondent_col <- "ResponseId"
firm_col <- "firm"

outcome_types <- list(
  "FirmCont_favor_white" = "Non-Experimental",
  "FirmHire_favor_white" = "Non-Experimental",
  "conduct_favor_white" = "Non-Experimental",
  "FirmCont_favor_male" = "Non-Experimental",
  "FirmHire_favor_male" = "Non-Experimental",
  "conduct_favor_male" = "Non-Experimental",
  "conduct_black" = "Non-Experimental",
  "conduct_white" = "Non-Experimental",
  "conduct_male" = "Non-Experimental",
  "conduct_female" = "Non-Experimental",
  "FirmCont_black" = "Non-Experimental",
  "FirmCont_white" = "Non-Experimental",
  "FirmCont_male" = "Non-Experimental",
  "FirmCont_female" = "Non-Experimental",
  "FirmHire_white" = "Non-Experimental",
  "FirmHire_black" = "Non-Experimental",
  "FirmHire_male" = "Non-Experimental",
  "FirmHire_female" = "Non-Experimental",
  "conduct_favor_younger" = "Non-Experimental",
  "discretion" = "Non-Experimental",
  "FirmSelective" = "Non-Experimental",
  "FirmDesire" = "Non-Experimental",
  "pooled_favor_white" = "Non-Experimental",
  "pooled_favor_male" = "Non-Experimental",
  "dif" = "Experimental",
  "log_dif" = "Experimental",
  "dif_gender" = "Experimental",
  "log_dif_gender" = "Experimental",
  "dif_age" = "Experimental",
  "log_dif_age" = "Experimental",
  "equal_weighted_favor_white" = "Non-Experimental",
  "equal_weighted_favor_male" = "Non-Experimental"
)


# Convert sentinel -1 to NA for all outcome columns
# (handles both numeric -1 and character "-1")
all_outcomes <- intersect(
  unique(c(survey_vars, names(outcome_types))),   # everything you might analyze
  names(data)                                     # actually present in the file
)

data <- data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(all_outcomes),
      ~ dplyr::na_if(suppressWarnings(as.numeric(.)), -1)
    )
  )

data <- data %>%
  mutate(educ = educ_0_1,
         age = age_gt40)

firms97 <- data %>% filter(!is.na(dif)) %>% select(firm_id) %>% distinct() %>% pull(firm_id)

# subset_var <- NULL
# subset_value <- NULL
# output_filename <- "Plackett_Luce_Full_Sample"


# # Function Call Female
system.time({
  run_analysis_pipeline(data, respondent_col, firm_col, survey_vars, experimental_vars,
                        outcome_types, subset_var = subset_var, subset_value = subset_value,
                        firms97 = firms97,
                        output_filename = output_filename,
                        industry_map_path = "~/Documents/consolidated_code_server/processed/industry_map.xlsx",
                        generate_wide = FALSE,
                        process_outcomes = FALSE, 
                        run_bootstrap = FALSE, 
                        run_bs_eiv = TRUE, 
                        eiv_summary = TRUE,
                        pairwise_process = FALSE,
                        borda_score = FALSE,
                        borda_bs_w = FALSE,
                        run_borda_eiv = TRUE,
                        borda_eiv_summary = TRUE,
                        pairwise_process_borda = FALSE,
                        sum_signal_noise = FALSE,
                        sim_pl_to_borda = FALSE,
                        exact_pl_to_borda = FALSE,
                        diagnostic = FALSE,
                        B = 1)
})


