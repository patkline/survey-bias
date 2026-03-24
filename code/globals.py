# ------------------------------------------------------------------------------
# Purpose: Define Python globals for survey bias project in a 
# module to be imported elsewhere
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------
# Import library to interact with the operating system
import os

# Import library to get current system user
import getpass

# Import path management library
from pathlib import Path

# ------------------------------------------------------------------------------
# Define user-specific Dropbox root path
# ------------------------------------------------------------------------------
# User -> Dropbox root (same entries as globals.R)
dropbox_roots_by_user = {
    "nicorotundo": "/Users/nicorotundo/Opportunity Insights Dropbox/Nico Rotundo/Survey/consolidated_code",
    "monicahea": "/Users/monicahea/Dropbox/Survey/consolidated_code",
    "jordancammarota": "/Users/jordancammarota/Dropbox/consolidated_code",
}

# Get current system user
user = getpass.getuser().lower()

# Assign Dropbox root for current user
dropbox_survey_bias_root = dropbox_roots_by_user.get(user)

# ------------------------------------------------------------------------------
# Auto-detect project root (similar to R's here::here())
# ------------------------------------------------------------------------------
# Start from the directory containing this file (code/)
_current = Path(__file__).resolve().parent

# Walk up the directory tree looking for project markers
while _current != _current.parent:
    # Once we find the folder containing .git --- survey-bias --- set it as the root 
    if (_current / ".git").exists():
        git_survey_bias_root = _current
        break
    _current = _current.parent
else:
    # If we couldn't find root, raise an error
    raise FileNotFoundError("Could not find project root (survey-bias directory)")

# ------------------------------------------------------------------------------
# Define project paths (no need to modify these)
# ------------------------------------------------------------------------------
# Select storage location for data and output --- options are "github" and "dropbox"
data_and_output_storage_location = "dropbox"  # "github"

# If switch is set to github, set data and output paths to github paths
if data_and_output_storage_location == "github":
    
    # Data and output paths within GitHub repository
    data = git_survey_bias_root / "data"
    output = git_survey_bias_root / "output"

# If switch is set to dropbox, set data and output paths to Dropbox mirror
elif data_and_output_storage_location == "dropbox":
    # Throw error if Dropbox path is not configured for current user
    if not dropbox_survey_bias_root:
        raise RuntimeError(f"🧌 No Dropbox path configured for user: {user}")

    # Path to the data and output mirror on Dropbox
    db_survey_bias_data_and_output_mirror = Path(dropbox_survey_bias_root) / "github_data_and_output_mirrors"
    
    # Data and output paths on Dropbox mirror
    data = db_survey_bias_data_and_output_mirror / "data"
    output = db_survey_bias_data_and_output_mirror / "output"

else:
    raise ValueError("🧌 Invalid value for `data_and_output_storage_location`. Must be 'github' or 'dropbox'")

# GitHub code paths
code = git_survey_bias_root / "code"
build = code / "data_build"
analysis = code / "analysis"
create_tables_figures = code / "create_tables_figures"

# Data paths
raw = data / "raw"
processed = data / "processed"
external = data / "external"
dump = data / "dump"

# Output paths
excel = output / "excel"
figures = output / "figures"
tables = output / "tables"

# ------------------------------------------------------------------------------
# Edit other python settings 
# ------------------------------------------------------------------------------
