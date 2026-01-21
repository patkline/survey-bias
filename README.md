# ------------------------------------------------------------------------------
# Purpose: README file for survey bias project
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

Repository for the code used to version-control the data build, analysis, and results for the survey bias project.

# Todo
1. I went through `clean_raw_qualtrics_data.py` (i.e., `code/Evan/1_clean_data.py` in the Dropbox) and noted various questions i had with `XX` comments --- need to cntrl-f for `XX` and address those questions at some point 
   1. For all intermediate datasets (i.e., not raw data), should probably check against the dropbox versions to ensure they match exactly
2. Need to figure out the best approach to porting the Dropbox codebase here efficiently --- ideally would go file-by-file, adding necessary data to the Github data folder as we go, but this might be too time-consuming
   1. Alternatively, could just upload the entire codebase and clean up as we go, but hesitate to do this, as then we'll be at square one re organizing the codebase 

# Temporary notes on codebase (will finalize)

1. Infrastructure code folders/files 
   
   1. `/.Rprofile` --- File that auto-executes upon R startup (analogous to Stata profile.do file); does the following,
      1. Activates renv (which manages R package versions for the project)
      2. Checks if essential R packages are installed (and installs them if not)
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