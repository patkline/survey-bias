# ------------------------------------------------------------------------------
# Purpose: Define R globals, installs and loads required packages 
# for R, initialize Python virtual environment, and install 
# required packages for Python
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Define project paths (no need to modify these)
# ------------------------------------------------------------------------------

# Path to GitHub root directory
git_survey_bias_root <- here::here()

# GitHub code paths
code <- file.path(git_survey_bias_root, "code")
build <- file.path(code, "data_build")
analysis <- file.path(code, "analysis")
create_tables_figures <- file.path(code, "create_tables_figures")

# Github data paths
data <- file.path(git_survey_bias_root, "data")
raw <- file.path(data, "raw")
processed <- file.path(data, "processed")
external <- file.path(data, "external")
dump <- file.path(data, "dump")

# Dropbox data paths
#db_survey_bias <- file.path(dropbox, "Survey Bias")
#data <- file.path(db_survey_bias, "data")
#raw <- file.path(data, "raw")
#processed <- file.path(data, "processed")
#scratch <- file.path(data, "scratch")

# Dropbox results paths
#results <- file.path(db_survey_bias, "results")
#results_figures <- file.path(results, "figures")
#results_tables <- file.path(results, "tables")
#results_scratch <- file.path(results, "scratch")

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
  "ggplot2"
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
  "pandas"
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
