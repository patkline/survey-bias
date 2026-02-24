# ------------------------------------------------------------------------------
# Purpose: README file for survey bias project
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

Repository for the code used to version-control the data build, analysis, and results for the survey bias project.

# Getting started 

## Prerequisites
- R 
- Python 3
- Git LFS

## Setup instructions

### If you haven't cloned the repository yet,
1. Install Git LFS at https://git-lfs.com/ 

2. Clone the repository via GitHub desktop interface or by typing `git clone https://github.com/pat-kline/survey-bias.git` in your terminal within whatever directory you want to store the project in on your machine

### If you already cloned the repository,
1. Install Git LFS at https://git-lfs.com/

2. Download the actual data files by running `git lfs pull` in the `/survey-bias` directory in your terminal

### Final setup (all users),
1. Open an R terminal in VScode in the project directory and the `.Rprofile` will automatically,
   - Activate `renv` for package management
   - Set the working directory
   - Install necessary R packages

2. Confirm the following message displayed in your terminal "🎃 .Rprofile was successfully sourced" 

3. With the above, everything else should be automatically set up when you run any code file (i.e., Python virtual environment, Python packages, etc...)

## For code you write, 
1. For R scripts, make sure to run `source("code/globals.R")` at the very top of your script to load global variables and packages

# Comparing results across code changes

To do a clean before/after check of **all results outputs**, use the rerun+compare driver. Baseline is defined as the **git-tracked** contents of `output/` (what a fresh clone would see).

From the project root:

- `Rscript code/tools/results_rerun_compare.R --run-name my_test`

This will:
1. Prompt to revert `output/` to the git baseline if it differs (type `YES` to proceed).
2. Rerun results via `code/create_tables_figures/metafile.R`.
3. Write a compact run bundle under `output/runs/` containing:
   - `changes.csv` (file-level + cell-level diffs for `.tex` and `.xlsx`)
   - `comparison.tex` (+ `comparison.pdf` if `pdflatex` is installed) showing old vs new for changed tables/figures (figures are side-by-side; `.tex` tables are included as old/new snippets)

Notes:
- Baseline is defined as the git-tracked contents of output/tables, output/figures, output/excel.
- `--skip-rerun` compares your current output/ against the baseline. If output/ is dirty, the tool will prompt to temporarily reset output/ to capture the baseline snapshot and then restore your outputs.

If there are zero changes, the tool prints a message and deletes the run folder.

## For any issues, reach out to me at `nrotundo@berkeley.edu`

# Documentation on codebase

1. Infrastructure code folders/files 
   
   1. `/.Rprofile` --- File that auto-executes upon R startup (analogous to Stata profile.do file); does the following,
      1. Activates renv (which manages R package versions for the project)
      2. Checks if necessary R packages are installed (and installs them if not)
         1. `here` --- Allows for the relative file paths in the codebase to function without setting manually 
         2. `jsonlite, rlang` --- Necessary for renv to function properly
      3. Sets working to the project root directory (i.e., /survey-bias) and checks that this was successful (since all file paths in the codebase are relative to this root directory)
   
   2. `/code/globals.R` --- File that should be placed at the top of all R scripts in the codebase (XXcould put this in .Rprofile instead, but then any changes to globals.R would require restarting R to take effect); does the following, 
      1. Defines R global variables used across the codebase (i.e., file paths currently, but could include e.g., constants, formatting, etc... later on) 
         1. Filepaths to various folders in the project (e.g., data, code, results, etc...)
            1. Any changes to filepaths in `globals.R` should be mirrored in `globals.py`
      2. Checks installation (and auto-installs if missing) required packages for R into the `renv` folder and loads them 
      3. Initializes Python virtual environment in the `.venv` folder (if not already created)
         1. `.venv` is analagous to R's `renv` 
         2. However, unlike `renv`, Python virtual environments should not be saved to version control (and I think is automatically not)
         3. Thus, it has to be initialized on each user's machine when they first run the codebase, and the code does this automatically
      4. Checks installation (and auto-installs if missing) required packages for Python into the `.venv` virtual environment
      
   3. `/code/globals.py` --- File that defines Python global variables used across the codebase
      1. Compared to `globals.R`, you do not run this file, but instead import the specific filepaths you need into your Python scripts
      2. Any changes to filepaths in `globals.R` should be mirrored here (XXaiming to automate this in the future, perhaps using a JSON/YAML file as the single source of truth for filepaths that gets fed into `globals.R` and `globals.py`)
      
2. Masterfiles 
   1. `/code/metafile.` --- File that will eventually execute the entire project top-to-bottom (and is useful generally for documenting the project workflow)
      1. Currently executes sub-metafiles for each major step of the project, which are housed in subdirectories of `/code/`; currently --- data build, analysis, results
      2. If an additional major step is added to the project that does not fit into these existing categories, a new subdirectory and sub-metafile should be created for it
   
   2. `/code/XX/metafile.R` --- File that executes all R scripts in a given subdirectory in the correct order
      1. Whenever a new code file is created, it should be added to the appropriate sub-metafile in the correct order

3. External data files (i.e., not generated by the codebase here) --- stored in the `/data/external` folder 
   1. `2019_Business_Academic_QCQ.txt.gz` --- RefUSA data used for the `A Discrimination Report Card` AER industry code classifications

4. Archive files in `/data/archive`
   1. `industry_map.xlsx` is a deprecated file that we used to draw our industry codes, `aer_naics2`, from
      1. However, it was unclear where this file and these industry codes came from, so added a file in `data_build` that draws the industry codes directly from the RefUSA data
      2. Added this file to the archive folder for comparison 
      3. See `https://github.com/patkline/survey-bias/issues/48` for more details on what we did here 