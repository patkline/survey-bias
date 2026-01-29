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
1. Install Git LFS at https://git-lfs.com/ (step 1 only)

2. Clone the repository using `git clone https://github.com/[username]/survey-bias.git`

### If you already cloned the repository:
1. Install Git LFS at https://git-lfs.com/ (step 1 only)

2. Download the actual data files by running `git lfs pull` in the `/survey-bias` directory 

### Final setup (all users):
1. Open an R terminal in VScode in the project directory and the `.Rprofile` will automatically,
   - Activate `renv` for package management
   - Set the working directory
   - Install necessary R packages

2. Confirm the following message displayed in your terminal "🎃 .Rprofile was successfully sourced" 

3. With the above, everything else should be automatically set up when you run any code file (i.e., Python virtual environment, Python packages, etc...)

## For any issues, reach out to me at `nrotundo@berkeley.edu`

# Todo
1. Build out structure of survey bias project directory (i.e., code, data, etc... folders) and get basic infrastructure working (i.e., automated setup of background things, such that anyone should be able to just clone the directory and just run any given file; package management for R and Python; globals for filepaths; metafiles for project execution that run the project from top-to-bottom and organize code order)
   1. I think all that is left here is to make sure that the pakage management is functioning properly --- so it preserves package versions across time 
   2. perhaps worthwhile to use Github LFS instead of dropbox for data
      1. from my understanding it stores the data on some server and just saves a small version of the data file with binary pointers to these data (so like a filepath to the data stored on the server)
         1.  when using EML, this seems like it would be useful to avoid having to download the entire data folder locally and managing the dropbox-eml sync in addition to the github-eml sync
       2.  also, it'll be a good excuse to refine how we manage and store data, while keeping the dropbox archive as a backup  

2.  for the metafiles, need to figure out if `source` can submit bash scripts, or if it is running things interactively 

3. I went through `clean_raw_qualtrics_data.py` (i.e., `code/Evan/1_clean_data.py` in the Dropbox) and noted various questions i had with `XX` comments --- need to cntrl-f for `XX` and address those questions at some point 

4. Now I think we just need to move over the remaining project code from the Dropbox to here, and change filepaths 
   1. Need to figure out the best approach to porting the Dropbox codebase here efficiently --- ideally would go file-by-file, adding necessary data to the Github data folder as we go, but this might be too time-consuming
      1. Alternatively, could just upload the entire codebase and clean up as we go, but hesitate to do this, as then we'll be at square one re organizing the codebase
      2. Perhaps just upload the necessary files for what is currently in the draft, but nothing else? and then worst case if we need something else later we can add it then 

5. For all intermediate datasets (i.e., not raw data), should probably check against the dropbox versions to ensure they match exactly

6. Separately, need to figure out the EML integration and adjust the codebase accordingly

# Temporary notes on codebase (will finalize)

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

# Miscellaneous temporary notes 

1. Right now, I have both the data and results stored in the GitHub repository both for simplicity and to ensure I am not overwriting data and results for the draft
   1. However, if we want to store these in Dropbox, it should be the case that the only changes will be to the filepaths in `globals.R` and `globals.py` 
      1. This may require people to manually set Dropbox filepaths on their machines in `globals.R` and `globals.py` (unless we can find a way to automate this based on e.g., OS username or something else unique to each machine)
2. Key features of this codebase 
   1. Auto-setup of necessary infrastructure for new users 
   2. Ability to execute any code file interactively without using the metafile
   3. XX