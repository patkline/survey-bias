# ------------------------------------------------------------------------------
# Purpose: Define Python globals for survey bias project in a 
# module to be imported elsewhere
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

# Import path management library
from pathlib import Path

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

# GitHub code paths
code = git_survey_bias_root / "code"
build = code / "data_build"
analysis = code / "analysis"
create_tables_figures = code / "create_tables_figures"

# GitHub data paths
data = git_survey_bias_root / "data"
raw = data / "raw"
processed = data / "processed"
external = data / "external"
dump = data / "dump"

# ------------------------------------------------------------------------------
# Edit other python settings 
# ------------------------------------------------------------------------------

