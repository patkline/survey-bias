# ------------------------------------------------------------------------------
# Purpose: Define R globals, installs and loads required packages 
# for R, initialize Python virtual environment, and install 
# required packages for Python
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Define user-specific Dropbox root path
# ------------------------------------------------------------------------------
# Map system user -> Dropbox consolidated_code root
dropbox_roots_by_user <- c(
  nicorotundo    = "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code",
  monicahea      = "/Users/monicahea/Dropbox/Survey/consolidated_code",
  jordancammarota = "/Users/jordancammarota/Dropbox/consolidated_code"
)

# Get current system user
user <- tolower(Sys.info()[["user"]])

# Assign Dropbox root for current user
dropbox_survey_bias_root <- unname(dropbox_roots_by_user[user])

# ------------------------------------------------------------------------------
# Define project paths (no need to modify these)
# ------------------------------------------------------------------------------
# Path to GitHub root directory
git_survey_bias_root <- here::here()

# Tell Git to use this repo's auto-run scripts in `.githooks` when switching branches or merging
suppressWarnings(system2(
  "git",
  args = c("-C", git_survey_bias_root, "config", "--local", "core.hooksPath", ".githooks"),
  stdout = FALSE,
  stderr = FALSE
))

# Select storage location for data and output --- options are "github" and "dropbox"
data_and_output_storage_location <- "dropbox" #"github"

# If switch is set to Github, set data and output paths to github paths
if (data_and_output_storage_location == "github") {

  # Configure Git LFS for GitHub mode (normal behavior with automatic downloads on checkout)
  suppressWarnings(system2(
      "git",
      args = c("-C", git_survey_bias_root, "lfs", "install", "--local", "--skip-repo"),
      stdout = FALSE,
      stderr = FALSE
    ))

  # Data and output paths within GitHub repository
  data <- file.path(git_survey_bias_root, "data")
  output <- file.path(git_survey_bias_root, "output")

# If switch is set to dropbox, set data and output paths to Dropbox mirror
} else if (data_and_output_storage_location == "dropbox") {
  
  # Configure Git LFS for Dropbox mode to prevent automatic downloads of large files on checkout (since data is accessed via Dropbox, not GitHub)
  suppressWarnings(system2(
      "git",
      args = c("-C", git_survey_bias_root, "lfs", "install", "--local", "--skip-smudge", "--skip-repo"),
      stdout = FALSE,
      stderr = FALSE
    ))

  # Path to the data and output mirror on Dropbox
  db_survey_bias_data_and_output_mirror <- file.path(dropbox_survey_bias_root, "github_data_and_output_mirrors")

  # Throw error if Dropbox path is not configured for current user
    if (is.na(dropbox_survey_bias_root) || !nzchar(dropbox_survey_bias_root)) {
    stop("🧌 No Dropbox path configured for user: ", user)
  }

  # Data and output paths on Dropbox mirror
  data <- file.path(db_survey_bias_data_and_output_mirror, "data")
  output <- file.path(db_survey_bias_data_and_output_mirror, "output")

} else {
  stop("🧌 Invalid value for `data_and_output_storage_location`. Must be 'github' or 'dropbox'")
}

# GitHub code paths
code <- file.path(git_survey_bias_root, "code")
build <- file.path(code, "1_data_build")
analysis <- file.path(code, "2_analysis")
create_tables_figures <- file.path(code, "3_create_tables_figures")
helper_functions <- file.path(code, "helper_functions")

# Github data paths
raw <- file.path(data, "raw")
processed <- file.path(data, "processed")
external <- file.path(data, "external")
dump <- file.path(data, "dump")

# Github output paths
excel <- file.path(output, "excel")
intermediate <- file.path(output, "intermediate")
figures <- file.path(output, "figures")
tables <- file.path(output, "tables")

# Define path to Python virtual environment
python_venv_directory <- file.path(git_survey_bias_root, ".venv")
python_venv_installation <- file.path(python_venv_directory, "bin", "python")

# ------------------------------------------------------------------------------
# Install and load required packages for R 
#
# Note. All packages are locally installed within the virtual 
# environments in the project directory and are only installed 
# when needed
# ------------------------------------------------------------------------------

# Define list of required R packages
required_r_packages <- c(
  "ggplot2", 
  "readxl",
  # project infrastructure
  "here",
  "renv",
  
  # core packages (added)
  "arrow",
  "dplyr",
  "igraph",
  "openxlsx",
  "parallel",
  "PlackettLuce",
  "prefmod",
  "readxl",
  "sandwich",
  "tibble",
  "tidyr",
  "writexl",
  "xtable",
  "scales",
  "rlang",
  "stringr",
  "tibble",
  "kableExtra",
  "knitr", 
  "magick", 
  "ggpattern",
  "ragg",
  "rlang",
  "ordinal",
  "readr"
)

# Identify missing R packages
missing_r_packages <- required_r_packages[!sapply(required_r_packages, requireNamespace, quietly = TRUE)]

# Install R packages iff missing
if (length(missing_r_packages) > 0) {
message("🎃 Installing missing R packages: ", paste(missing_r_packages, collapse = ", "))
  renv::install(missing_r_packages)
}

# Load R packages
invisible(lapply(required_r_packages, library, character.only = TRUE, quietly = TRUE))

# ------------------------------------------------------------------------------
# Create Python virtual environment if it doesn't exist
#
# Note. This mirrors renv's behavior --- automatically creates a 
#local Python environment on first use so new users can just 
# open the project and everything works
# ------------------------------------------------------------------------------

# Create virtual environment if it doesn't exist
if (!file.exists(python_venv_installation)) {
  message("🎃 Python virtual environment not found --- creating .venv")
  
  # Store string for python command 
  # Try python3 first (macOS/Linux), then fall back to python (Windows)
  python_cmd <- if (Sys.which("python3") != "") "python3" else "python"
  
  # Create virtual environment
  system2(python_cmd, 
        args = c("-m", "venv", python_venv_directory), 
        stdout = TRUE, 
        stderr = TRUE)
  
  if (!file.exists(python_venv_installation)) {
    stop("Failed to create Python virtual environment. Ensure Python 3 is installed on your system.")
  } else {
    message("🎃 Python virtual environment created successfully")
  }
}

# ------------------------------------------------------------------------------
# Install and load required packages for Python 
#
# Note. All packages are locally installed within the virtual 
# environments in the project directory are are only installs 
# when needed
# ------------------------------------------------------------------------------

# Define list of required Python packages
required_python_packages <- c(
  "numpy",
  "pandas",
  "openpyxl"
)

# Initialize vector to hold missing Python packages
missing_python_packages <- character(0)

# Loop through required Python packages and check if installed
for (package in required_python_packages) {
    # System call to check if package can be imported
    check <- system2(python_venv_installation, 
                     args = c("-c", shQuote(sprintf("import %s", package))), 
                     stdout = FALSE, 
                     stderr = FALSE)
    
    # If check fails, package is missing and add to list of missing packages 
    if (check != 0) {
      missing_python_packages <- c(missing_python_packages, package)
    }
  }
  
# Install Python packages iff missing
if (length(missing_python_packages) > 0) {
    message("🎃 Installing missing Python packages: ", paste(missing_python_packages, collapse = ", "))
    system2(python_venv_installation, 
        args = c("-m", "pip", "install", paste(missing_python_packages, collapse = " ")))
}

# ------------------------------------------------------------------------------
# Edit other R settings 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Source custom functions (if any)
# ------------------------------------------------------------------------------

# Parquet "sheet" helpers (read_parquet_sheet, write_parquet_sheet, ...)
# Sourced here so both the analysis pipeline and table/figure scripts can use
# them without having to load the full analysis/load_all.R
source(file.path(helper_functions, "sheet_functions.R"))
