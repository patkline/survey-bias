<!------------------------------------------------------------------------------
Purpose: README file for survey bias project

Created: Nico Rotundo 2026-01-06
# ----------------------------------------------------------------------------->

# Getting started 

## Setup instructions

### Prerequisites
- R 
- Python 3

### If you haven't cloned the repository yet,
1. Install Git LFS at https://git-lfs.com/ 

2. Clone the repository via GitHub desktop interface or by typing `git clone https://github.com/pat-kline/survey-bias.git` in your terminal within whatever directory you want to store the project in on your machine

### If you already cloned the repository,
1. Install Git LFS at https://git-lfs.com/

2. Download the actual data files by running `git lfs pull` in the `/survey-bias` directory in your terminal

### Final setup (all users),
1. Add your Dropbox folder path to `globals.R` and `globals.py`
   1. Run `whoami` in your terminal to get your username
   1. In `globals.R`,
      1. Under the `Define user-specific Dropbox root path` section, add an analagous set of lines for your username and Dropbox path as the existing ones for Nico, Monica, and Jordan (i.e., `"your_username" =  "your_dropbox_path"`)
   2. In `globals.py`, 
      1. Do the same as in `globals.R`
      
2. Open an R terminal in VScode in the project directory and the `.Rprofile` will automatically,
   - Activate `renv` for package management
   - Set the working directory
   - Install necessary R packages

3. Confirm the following message displayed in your terminal "🎃 .Rprofile was successfully sourced" 

4. With the above, everything else should be automatically set up when you run any code file (i.e., Python virtual environment, Python packages, etc...)

## Important general notes 

### I. Data and output storage system
This project stores data using Git LFS --- however, we have a backup storage plan in place in the (rare) case that we run out of Git LFS storage or bandwidth, 

   1. *To set the system, just set the value of the `data_and_output_storage_location` variable in `globals.R` and `globals.py` to the intended value (i.e., usually `github`; but `dropbox` if space constrained on Git LFS)*
   2. *If switching from Github to Dropbox, one person needs to do the following,*
      
      i. Make sure your local `data/` and `/output` folders are exactly the versions you want to mirror in Dropbox --- if needed, run `git lfs pull` to get the latest versions of files tracked in LFS

      ii. Copy the `data/`, `output/`, and `qje_2022_replication_data_and_outputs/` folders to `/Survey/consolidated_code/github_data_and_output_mirrors`

      iii. Switch the `data_and_output_storage_location` variable in `globals.R` and `globals.py` to `dropbox`

      iv. Source `globals.R` in your R terminal

   3. *If switching from Dropbox to Github, one person needs to do the following,*
      
      i. Make sure the Dropbox `/Survey/consolidated_code/github_data_and_output_mirrors` folder contains the correct `data/`, `output/`, and `qje_2022_replication_data_and_outputs/` folders

      ii. Copy the `data/`, `output/`, and `qje_2022_replication_data_and_outputs/` folders from `/Survey/consolidated_code/github_data_and_output_mirrors` to the corresponding local folders in the repository

      iii. Review the set of changes and double-check that the local `data/` and `/output` folders are exactly how you want them to be in the repository (since these will be what gets pushed to Github and tracked in Git LFS)

      iv. Commit and push the changes to Github (which will also push the relevant LFS objects)

      v. Switch the `data_and_output_storage_location` variable in `globals.R` and `globals.py` to `github`

      vi. Source `globals.R` in your R terminal

   4. After doing so, the backround processes for (i) syncing data and outputs to the intended storage location and (ii) zeroing LFS bandwidth use (in the case of switching to Dropbox) will be automatically set up

### II. Sourcing globals at the top of scripts

- *For R scripts, run `source("code/globals.R")` at the top of your script to load global variables and packages*
- *For Stata scripts, run `do "${github}/survey-bias/code/globals.do"` at the top of your script. This requires `${github}` to be defined in your personal `profile.do` (pointing at your local GitHub root, e.g. `global github "/Users/<username>/GitHub"`). Place `profile.do` in Stata's personal ado directory --- run `display c(sysdir_personal)` in Stata to find the path.*

### III. Comparing results across code changes
Use `code/tools/results_rerun_compare.R` from the project root to compare outputs before vs after code changes

#### Option definitions and prompt notes
1. `--run-name` (optional): this option defines a suffix added to the run subfolder name under `output/results_build_runs/` (or Dropbox mirror output path when in Dropbox mode)
   - If not provided, no run-name suffix is added
   
2. `--baseline` (optional): this command sets what branch's `/output` folder to use as a baseline for comparison
   - Defaults to using the `/output` files in `/origin/main` (i.e., the `/output` files you see if you go to Github and look at `/main`) as the baseline files you are comparing against 
   - Can also be set to `origin-current`, which will use the `/output` files in `/origin/<current_branch>` (i.e., the `/output` files you see if you go to Github and look at `/<current_branch>`) as the baseline files you are comparing against 
   - We can eventually add more baseline options if they become relevant
  <!-- - Can also be set to `current`, which uses your current local `/output` folder at runtime as the baseline snapshot -->

3. `--skip-rerun` (optional flag): do not run the `code/create_tables_figures/metafile.R` file
   - Compares current outputs against baseline snapshot i.e., what you see in `/output` currently is what you compare against baseline

4. `--with-xlsx-cell-diffs` (optional flag): compute cell-level diffs for changed `.xlsx` files
   - Default behavior is off (only file-level diffs are written for Excel files)
   - Turn this on when you specifically need cell-level spreadsheet change details (you most likely will not)

5. *Reasons the script may abort before doing anything,*
   - The script first checks whether the local tracking ref is synced with `origin/main` or `origin/<current_branch>` (depending on baseline mode)
   - The script then checks whether baseline LFS objects for `/output/{tables,figures,excel}` are already in local LFS cache
     - In github mode, if either check fails, the script prompts before running `git fetch` and/or `git lfs fetch` (and reports estimated LFS download size when needed)
     - In dropbox mode, if either check fails, the script aborts, since downloading files from Github is not an option with the space constraints 

6. *Local output note.* The script compares against whatever is currently in your local `/output` folder on the “new” side of the comparison
   - If your local `/output` folder contains stale files from earlier runs, manually edited files, or extra untracked files, those may appear in the comparison
     - In github storage mode (output inside repository), the script warns when this is the case and continues
     - In dropbox storage mode (output outside repository), the script cannot warn you using Git since the Dropbox `/output` folder is outside the repository

#### Common use cases and their respective commands (where <> denotes things to fill in)
1. You want to rerun the entire set of code that creates outputs (i.e., `code/create_tables_figures/metafile.R`) and compare against `origin/main` as the baseline,
   - `Rscript code/tools/results_rerun_compare.R --run-name <my_test>`
2. You want to rerun the entire set of code that creates outputs and compare against your current branch on GitHub (`origin/<current_branch>`) as the baseline,
   - `Rscript code/tools/results_rerun_compare.R --run-name <my_test> --baseline origin-current`
3. You want to compare your local `/output` folder as-is (e.g., you have already generated a subset of new results) against `origin/main` as the baseline,
   - `Rscript code/tools/results_rerun_compare.R --run-name <my_test> --skip-rerun`
4. You want to compare your local `/output` folder as-is (e.g., you have already generated a subset of new results) against `origin/<current_branch>` as the baseline,
   - `Rscript code/tools/results_rerun_compare.R --run-name <my_test> --baseline origin-current --skip-rerun`
<!-- 5. You have already generated a subset of new results in the `/output` folder and want to compare against your local current branch's `/output` folder + unpushed commits as the baseline,
   - `Rscript code/tools/results_rerun_compare.R --run-name <my_test> --baseline current --skip-rerun` -->
<!--6. You want cell-level diffs for changed Excel outputs in any of the above workflows,
   - add `--with-xlsx-cell-diffs` to the command-->
   
#### How the script works 
1. Creates a run subfolder under `output/results_build_runs/` (or Dropbox mirror output path when in Dropbox mode i.e., `/Survey/consolidated_code/github_data_and_output_mirrors`)
2. Sets baseline source paths based on `--baseline` option
   - `main` -> `origin/main`
   - `origin-current` -> `origin/<current_branch>`
3. Runs the following preflight checks before copying the `/output` files from the baseline you chose, 
   - Your local copy of the baseline branch matches what is currently on GitHub
   - Required baseline LFS objects for `output/{tables,figures,excel}` are available locally
   - In github mode, the script can prompt to run `git fetch` / `git lfs fetch`; in dropbox mode, preflight failures abort
4. Creates `old_full/{tables,figures,excel}` by copying baseline output files into the run folder
5. Creates `new_full/{tables,figures,excel}` only when `--skip-rerun` is set, by copying current local output into the run folder; otherwise, runs `code/create_tables_figures/metafile.R` and treats the active output root as the "new" side.
6. Compares `old_full` vs new side
   - If there are zero differences, deletes the run folder and exits
   - Otherwise writes file-level diffs to `changes.csv` and continues
7. Copies changed files into run-level `old/` and `new/`, writes `comparison.tex`, and generates `comparison.pdf`
8. Appends `.xlsx` cell-level diffs to `changes.csv` only when `--with-xlsx-cell-diffs` is set
9.  Writes metadata (`meta.json`) describing baseline mode/ref and run counts

---
# For any issues on the above or below, reach out to me at `nrotundo@berkeley.edu`

## Todo
1. We should get package management working properly for both R and Python (i.e., `renv` for R and virtual environment for Python) so that we can ensure reproducibility across time

2. Separately, need to figure out the EML integration and adjust the codebase accordingly

## Documentation on codebase

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

3. QJE 2022 Figure 9 replication
   1. `/code/qje_2022_replication` --- Folder that contains self-contained code for reproducing exhibits in Kline-Rose-Walters (2022)
      1. Includes the Stata metafile, original rr_misc cleaning baseline, corrected-cleaning variant, figure construction scripts, and comparison scripts
      2. Large inputs and generated outputs live in `qje_2022_replication_data_and_outputs/`, routed through the same GitHub-vs-Dropbox storage switch as `data/` and `output/`
      3. The full paper replication package lives at `qje_2022_replication_data_and_outputs/qje_2022_full_replication_package/` for provenance checks and line diffs against the authors' original code

4. Git LFS hook policy (fail-closed)
   1. This repo uses fail-closed hooks in `.githooks/` for `post-checkout`, `post-merge`, and `pre-push`
   2. If a hook cannot parse/validate `data_and_output_storage_location` in `code/globals.R` or cannot apply the expected LFS mode, the Git operation is blocked
   3. Hook changes are repo-local (`git lfs install --local --skip-repo ...` and `git config --local ...`), so this does not modify global Git behavior on a machine 
   
5. External data files (i.e., not generated by the codebase here) --- stored in the `/data/external` folder 
   1. `2019_Business_Academic_QCQ.txt.gz` --- RefUSA data used for the `A Discrimination Report Card` AER industry code classifications

6. Archive files in `/data/archive`
   1. `industry_map.xlsx` is a deprecated file that we used to draw our industry codes, `aer_naics2`, from
      1. However, it was unclear where this file and these industry codes came from, so added a file in `data_build` that draws the industry codes directly from the RefUSA data
      2. Added this file to the archive folder for comparison 
      3. See `https://github.com/patkline/survey-bias/issues/48` for more details on what we did here 
