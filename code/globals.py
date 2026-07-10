# ------------------------------------------------------------------------------
# Purpose: Define Python globals for survey bias project in a 
# module to be imported elsewhere
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------
# Import library to interact with the operating system
import os

# Import library to import modules dynamically
import importlib

# Import library to get current system user
import getpass

# Import library to install missing Python packages
import subprocess

# Import library for current Python executable path
import sys

# Import path management library
from pathlib import Path

# ------------------------------------------------------------------------------
# Define user-specific Dropbox root path
# ------------------------------------------------------------------------------
# User -> Dropbox root (same entries as globals.R)
dropbox_roots_by_user = {
    "nicorotundo": "/Users/nicorotundo/Library/CloudStorage/Dropbox/Survey/consolidated_code",
    "monicahea": "/Users/monicahea/Dropbox/Survey/consolidated_code",
    "jordancammarota": "/Users/jordancammarota/Dropbox/consolidated_code",
    "anh-huynguyen": "/Users/anh-huynguyen/Dropbox/Survey/consolidated_code"
}

# Get current system user
user = getpass.getuser().lower()

# Assign Dropbox root for current user
dropbox_survey_bias_root = dropbox_roots_by_user.get(user)

# Branch-specific Dropbox data/output sandbox. This folder lives inside the
# canonical mirror so runs on this branch cannot overwrite existing outputs.
dropbox_data_output_mirror_relative_path = Path(
    "github_data_and_output_mirrors"
) / "github_data_and_output_mirrors_loosen_sample_filters"

# ------------------------------------------------------------------------------
# Define optional user-specific WRDS username
# ------------------------------------------------------------------------------
# Put only WRDS usernames here, never passwords. Leave as None to make WRDS prompt
# or use the WRDS_USERNAME environment variable / --wrds-username CLI flag.
wrds_usernames_by_user = {
    "nicorotundo": None,
    "monicahea": None,
    "jordancammarota": None,
    "anh-huynguyen": "anhhuynguyen",
}

# WRDS_USERNAME environment variable overrides the user-specific default above
wrds_username = os.environ.get("WRDS_USERNAME") or wrds_usernames_by_user.get(user)

# ------------------------------------------------------------------------------
# Define required Python packages
# ------------------------------------------------------------------------------
# Package name -> import name. Keep this in sync with required_python_packages
# in globals.R.
required_python_packages = {
    "numpy": "numpy",
    "pandas": "pandas",
    "openpyxl": "openpyxl",
    "wrds": "wrds",
}


def ensure_python_packages(package_names=None):
    """Install missing project Python packages into the active interpreter."""
    if package_names is None:
        package_names = required_python_packages.keys()

    missing_packages = []
    for package_name in package_names:
        import_name = required_python_packages[package_name]
        try:
            importlib.import_module(import_name)
        except ModuleNotFoundError:
            missing_packages.append(package_name)

    if not missing_packages:
        return

    subprocess.check_call(
        [
            sys.executable,
            "-m",
            "pip",
            "install",
            "--upgrade",
            "pip",
            "setuptools",
            "wheel",
        ]
    )
    subprocess.check_call(
        [sys.executable, "-m", "pip", "install", *missing_packages]
    )

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
    qje_2022_data_and_outputs = git_survey_bias_root / "qje_2022_replication_data_and_outputs"

# If switch is set to dropbox, set data and output paths to Dropbox mirror
elif data_and_output_storage_location == "dropbox":
    # Throw error if Dropbox path is not configured for current user
    if not dropbox_survey_bias_root:
        raise RuntimeError(f"🧌 No Dropbox path configured for user: {user}")

    # Path to the data and output mirror on Dropbox
    db_survey_bias_data_and_output_mirror = (
        Path(dropbox_survey_bias_root) / dropbox_data_output_mirror_relative_path
    )
    
    # Data and output paths on Dropbox mirror
    data = db_survey_bias_data_and_output_mirror / "data"
    output = db_survey_bias_data_and_output_mirror / "output"
    qje_2022_data_and_outputs = db_survey_bias_data_and_output_mirror / "qje_2022_replication_data_and_outputs"

else:
    raise ValueError("🧌 Invalid value for `data_and_output_storage_location`. Must be 'github' or 'dropbox'")

# GitHub code paths
code = git_survey_bias_root / "code"
build = code / "1_data_build"
analysis = code / "2_analysis"
create_tables_figures = code / "3_create_tables_figures"
helper_functions = code / "helper_functions"

# Data paths
raw = data / "raw"
processed = data / "processed"
external = data / "external"
dump = data / "dump"

# Output paths
excel = output / "excel"
intermediate = output / "intermediate"
figures = output / "figures"
tables = output / "tables"

# QJE 2022 replication paths
qje_2022_replication_code = code / "qje_2022_replication"
qje_2022_replication_data = qje_2022_data_and_outputs / "data"
qje_2022_replication_outputs = qje_2022_data_and_outputs / "outputs"
qje_2022_replication_raw = qje_2022_replication_data / "raw"
qje_2022_replication_package = qje_2022_replication_raw / "qje_2022_full_replication_package"
qje_2022_replication_figures = qje_2022_replication_outputs / "figures"
qje_2022_replication_tables = qje_2022_replication_outputs / "tables"
qje_2022_replication_dump = qje_2022_replication_data / "dump"

# ------------------------------------------------------------------------------
# Edit other python settings 
# ------------------------------------------------------------------------------
