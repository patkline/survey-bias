# ------------------------------------------------------------------------------
# Purpose: Build a firm-by-SIC crosswalk from RefUSA and 
# write industry_map.xlsx
#
# Created: Nico Rotundo 2026-02-23 
# ------------------------------------------------------------------------------

# Source project globals so required packages are loaded.
source("code/globals.R")

# ------------------------------------------------------------------------------
# Import and prepare long survey data for crosswalk construction
# ------------------------------------------------------------------------------

# Import long_survey.csv
long_survey <- read.csv(file.path(processed, "long_survey.csv"))

# Confirm ResponseId and option_number uniquely identifies observations i.e., unique values of response id x unique values of option number = total rows
stopifnot(nrow(long_survey) == dplyr::n_distinct(long_survey$ResponseId, long_survey$option_number))

# Keep necessary variables i.e., firm_clean variable 
long_survey <- long_survey %>% dplyr::select(firm_clean)

# Trim leading and trailing whitespace from firm_clean values
long_survey$firm_clean <- trimws(long_survey$firm_clean)

# Confirm firm_clean is never missing i.e., confirm that all rows have non-NA and non-blank firm_clean values
stopifnot(all(!is.na(long_survey$firm_clean) & long_survey$firm_clean != ""))

# Keep one observation per unique firm_clean value to recover list of unique firms in our data, and keep only the firm_clean variable 
long_survey <- long_survey %>% dplyr::distinct(firm_clean)

# Should be 165 unique firms uniquely identified off firm_clean 
stopifnot(nrow(long_survey) == 165L | n_distinct(long_survey$firm_clean) == 165L)

# ------------------------------------------------------------------------------
# Import and assess structure of RefUSA data interactively
# ------------------------------------------------------------------------------
# Read in first 10000 rows of RefUSA to browse interactively 
refusa_first_10000_rows <- readr::read_csv(  
# Filepath
file = file.path(external, "2019_Business_Academic_QCQ.txt.gz"),

# Number of rows
n_max = 10000,

# Suprress column type printout
show_col_types = FALSE
)

# View data variables 
View(refusa_first_10000_rows)
e
# View sic variables 
#XXI think we want to be using "Primary SIC Code" for our matching, but should confirm with the data documentation 
#View(refusa_first_10000_rows %>% select(matches("(?i)sic")))

# ------------------------------------------------------------------------------
# Confirm that ABI variable is non-missing and uniquely identifies 
# rows in RefUSA
#
# 1. Load in RefUSA in chunks, and for each chunk, 
#   i. Check whether there are NA or blank values in the ABI 
#     column, and throw an error if so
#   ii. Check whether there are within-chunk duplicate values in 
#     the ABI column, and throw an error if so 
#   iii. Check whether there are across-chunk duplicate values in 
#     the ABI column by comparing to a global set of seen ABI values, 
#       and throw an error if so
#        b. if not, add ABI values from this chunk to a global set of seen ABI values for cross-chunk duplicate checks #        and proceed 
# ------------------------------------------------------------------------------
# XXCommenting out fow now --- uncomment out at end of file construction
if (FALSE) {
# Initialize hashed storage environment/dictionary that adds seen ABI values as keys
refusa_abi_seen <- new.env(hash = TRUE, parent = emptyenv())

# Read RefUSA ABI column in chunks for uniqueness assertion
readr::read_csv_chunked(
# Filepath for the RefUSA gzipped text file
file = file.path(external, "2019_Business_Academic_QCQ.txt.gz"),

# Use callback that enforces within-chunk and across-chunk uniqueness of ABI values 
callback = readr::DataFrameCallback$new(function(chunk, pos) {
  # Trim leading and trailing whitespace in ABI values in current chunk
  chunk$ABI <- trimws(chunk$ABI)

  # Fail immediately if any ABI in this chunk is missing (NA) or blank
  if (any(is.na(chunk$ABI) | chunk$ABI == "")) {
    stop(sprintf("🪦 Found missing ABI values in chunk at position %s", pos))
  }

  # Fail immediately if there are within-chunk duplicate ABI values
  if (any(duplicated(chunk$ABI))) {
    stop(sprintf("🪦 Found duplicate ABI values within chunk at position %s", pos))
  }

  # For each ABI value in this chunk, check whether it already exists in the global seen set and store results in a logical vector
  exists_vec <- vapply(chunk$ABI, exists, logical(1), envir = refusa_abi_seen, inherits = FALSE)
  
  # Check whether any of these ABI values are already present in the global seen set by seeing if any values in the logical vector are TRUE
  if (any(exists_vec)) {
    stop(sprintf("🪦 Found duplicate ABI across chunks at position %s", pos))
  }

  # Add the unique ABI keys from this chunk to the global seen set
  for (k in chunk$ABI) assign(k, TRUE, envir = refusa_abi_seen)

  # Return invisibly from callback
  invisible(NULL)
}),

# Set chunk size for throughput and memory balance
chunk_size = 250000,

# Restrict import to ABI column only for uniqueness checks
col_types = readr::cols_only(
  # Read ABI as character to preserve formatting
  ABI = readr::col_character()
),

# Disable progress bar for cleaner logs
progress = FALSE
)
}
# ------------------------------------------------------------------------------
# Build firm-level matching keys from long_survey firm_clean values
# 1. Build exact keys by uppercasing firm_clean values
#
# 2. Build normalized keys by (i) replacing ampersands with "AND", 
# (ii) removing non-alphanumeric characters except spaces, 
# (iii) collapsing repeated spaces to one space, and (iv) removing 
# trailing corporate suffixes from firm_clean values
#
# 3. Build compact keys by removing all whitespace from normalized keys
# ------------------------------------------------------------------------------
# Build exact and normalized keys, starting with making firm names uppercase
long_survey <- long_survey %>% dplyr::mutate(
  firm_clean_uppercase = toupper(firm_clean),
  firm_clean_normalized = firm_clean_uppercase
)

# Replace ampersands in normalized keys i.e., treat & as AND token
long_survey$firm_clean_normalized <- gsub("&", " AND ", long_survey$firm_clean_normalized, fixed = TRUE)

# Remove non-alphanumeric characters except spaces from normalized keys
long_survey$firm_clean_normalized <- gsub("[^A-Z0-9 ]+", " ", long_survey$firm_clean_normalized)

# Collapse repeated spaces to one space in normalized keys
long_survey$firm_clean_normalized <- gsub("\\s+", " ", long_survey$firm_clean_normalized)

# Define a regex pattern for trailing corporate suffixes
suffix_pattern <- "(\\s+(INC|INCORPORATED|CO|COMPANY|CORP|CORPORATION|LLC|LTD|PLC|HOLDINGS|HOLDING|GROUP|COS|THE))+$" 

# Remove trailing corporate suffix tokens from normalized keys.
long_survey$firm_clean_normalized <- gsub(suffix_pattern, "", long_survey$firm_clean_normalized)

# Trim leading and trailing whitespace in normalized keys after suffix removal
long_survey$firm_clean_normalized <- trimws(long_survey$firm_clean_normalized)

# Create compact keys by removing all whitespace from the normalized key
long_survey$firm_clean_normalized_compact <- gsub(" ", "", long_survey$firm_clean_normalized, fixed = TRUE)

# Assert that match keys are (i) non-missing and non-blank and (ii) unique.
for (variable in c("firm_clean_uppercase", "firm_clean_normalized", "firm_clean_normalized_compact")) {
  # Assert non-missing and non-blank values in given variable 
  stopifnot(all(!is.na(long_survey[[variable]]) & long_survey[[variable]] != ""))

  # Assert unique values in given variable
  stopifnot(nrow(long_survey) == n_distinct(long_survey[[variable]]))
}

#View(long_survey)

# ------------------------------------------------------------------------------
# Construct maps from match keys to firm_clean for each match 
# key type, and initialize hashed SIC count maps for each firm
# ------------------------------------------------------------------------------
## Loop over each match key, defining mathematical function mapping match_key values --> firm_clean values 
for (match_key in c("firm_clean_uppercase", "firm_clean_normalized", "firm_clean_normalized_compact")) {
  # Create vector storing range of match_key values we are mapping to (i.e., realized codomain of the given mapping function)
  match_key_to_firm_clean_map <- long_survey$firm_clean

  # Assign mapping from match_key values to firm_clean values
  names(match_key_to_firm_clean_map) <- long_survey[[match_key]]

  # Store lookup object using name pattern [[match_key]]_to_firm_clean_map
  assign(paste0(match_key, "_to_firm_clean_map"), match_key_to_firm_clean_map)
}

# Create list with one element per firm_clean value
# Each element will store one firm's hash map from raw Primary SIC Code -> count observed in RefUSA
primary_sic_code_count_list_by_firm_clean <- setNames(vector("list", length = nrow(long_survey)), long_survey$firm_clean)

# For each firm_clean value, create an empty hashed environment and place it in that firm's element in the primary_sic_code_count_list_by_firm_clean list
# In each environment within a given element of the primary_sic_code_count_list_by_firm_clean list, keys are raw Primary SIC Code values and values are integer counts such that we have a mapping from Primary SIC Code --> count for each firm_clean value
for (firm in long_survey$firm_clean) {
  primary_sic_code_count_list_by_firm_clean[[firm]] <- new.env(hash = TRUE, parent = emptyenv())
}

# Initialize per-firm counters for number of RefUSA rows matched by each key type
firm_clean_match_count_exact_name <- setNames(integer(nrow(long_survey)), long_survey$firm_clean)
firm_clean_match_count_normalized_name <- setNames(integer(nrow(long_survey)), long_survey$firm_clean)
firm_clean_match_count_normalized_compact_name <- setNames(integer(nrow(long_survey)), long_survey$firm_clean)

# ------------------------------------------------------------------------------
# Scan RefUSA in chunks and accumulate firm-by-SIC counts
# ------------------------------------------------------------------------------
# Define chunk callback that matches RefUSA company names to firm_clean values and updates raw Primary SIC Code counts by firm
callback <- readr::DataFrameCallback$new(function(chunk, pos) {
  
  ## Build firm-level matching keys for RefUSA chunk using same transformations as for long_survey keys
  # Build exact RefUSA match keys by uppercasing and trimming company names in RefUSA chunk
  refusa_company_match_key_uppercase <- toupper(trimws(chunk$Company))

  # Build normalized RefUSA match keys by applying the same steps used for long_survey normalization
  refusa_company_match_key_normalized <- refusa_company_match_key_uppercase
  refusa_company_match_key_normalized <- gsub("&", " AND ", refusa_company_match_key_normalized, fixed = TRUE)
  refusa_company_match_key_normalized <- gsub("[^A-Z0-9 ]+", " ", refusa_company_match_key_normalized)
  refusa_company_match_key_normalized <- gsub("\\s+", " ", refusa_company_match_key_normalized)
  refusa_company_match_key_normalized <- gsub(suffix_pattern, "", refusa_company_match_key_normalized)
  refusa_company_match_key_normalized <- trimws(refusa_company_match_key_normalized)

  # Build compact RefUSA match keys by removing spaces from normalized keys.
  refusa_company_match_key_normalized_compact <- gsub(" ", "", refusa_company_match_key_normalized, fixed = TRUE)

  ## Keep raw Primary SIC code values from RefUSA chunk for counting later and identify rows with valid match keys and valid classified raw Primary SIC codes for counting later
  # Keep RefUSA primary SIC code values
  primary_sic_code_refusa <- trimws(chunk[["Primary SIC Code"]])

  # Mark rows where exact match key is non-missing/non-blank and Primary SIC is present/classified (drop 9999* codes)
  row_is_valid_for_matching_and_sic_count <- !is.na(refusa_company_match_key_uppercase) &
    refusa_company_match_key_uppercase != "" &
    !is.na(primary_sic_code_refusa) &
    primary_sic_code_refusa != "" &
    !grepl("^9999", primary_sic_code_refusa)

  # Return immediately when no valid rows are present in this chunk
  if (!any(row_is_valid_for_matching_and_sic_count)) return(invisible(NULL))

  # Keep only valid company keys
  refusa_company_match_key_uppercase <- refusa_company_match_key_uppercase[row_is_valid_for_matching_and_sic_count]

  # Keep only valid normalized company keys
  refusa_company_match_key_normalized <- refusa_company_match_key_normalized[row_is_valid_for_matching_and_sic_count]

  # Keep only valid compact company keys
  refusa_company_match_key_normalized_compact <- refusa_company_match_key_normalized_compact[row_is_valid_for_matching_and_sic_count]

  # Keep only valid raw SIC codes
  primary_sic_code_refusa <- primary_sic_code_refusa[row_is_valid_for_matching_and_sic_count]

  ## Match RefUSA rows to firm_clean values using exact keys with normalized and compact key fallbacks
  # Attempt exact matching from company key to firm
  matched_firm_clean <- unname(firm_clean_uppercase_to_firm_clean_map[refusa_company_match_key_uppercase])

  # Mark rows matched using exact key lookup
  row_matched_using_exact_name <- !is.na(matched_firm_clean)

  # Identify rows still unmatched after exact matching
  row_needs_normalized_key_fallback <- is.na(matched_firm_clean)

  # Initialize logical vector marking rows matched using normalized fallback lookup
  row_matched_using_normalized_name <- rep(FALSE, length(matched_firm_clean))

  # Apply normalized-key matching to rows still unmatched after exact matching
  if (any(row_needs_normalized_key_fallback)) {
    # Lookup normalized-key matches for currently unmatched rows
    normalized_fallback_matches <- unname(firm_clean_normalized_to_firm_clean_map[refusa_company_match_key_normalized[row_needs_normalized_key_fallback]])

    # Mark rows that matched via normalized fallback lookup
    row_matched_using_normalized_name[row_needs_normalized_key_fallback] <- !is.na(normalized_fallback_matches)

    # Fill matches using normalized lookup
    matched_firm_clean[row_needs_normalized_key_fallback] <- normalized_fallback_matches
  }

  # Identify rows still unmatched after normalized matching
  row_needs_compact_key_fallback <- is.na(matched_firm_clean)

  # Initialize logical vector marking rows matched using normalized-compact fallback lookup
  row_matched_using_normalized_compact_name <- rep(FALSE, length(matched_firm_clean))

  # Apply compact-key matching to rows still unmatched
  if (any(row_needs_compact_key_fallback)) {
    # Lookup compact-key matches for currently unmatched rows
    normalized_compact_fallback_matches <- unname(firm_clean_normalized_compact_to_firm_clean_map[refusa_company_match_key_normalized_compact[row_needs_compact_key_fallback]])

    # Mark rows that matched via compact fallback lookup
    row_matched_using_normalized_compact_name[row_needs_compact_key_fallback] <- !is.na(normalized_compact_fallback_matches)

    # Fill matches using compact lookup.
    matched_firm_clean[row_needs_compact_key_fallback] <- normalized_compact_fallback_matches
  }

  ## For matched rows in this chunk, keep firm_clean values and raw Primary SIC codes
  # Identify rows that successfully matched to a firm
  row_successfully_matched_to_firm_clean <- !is.na(matched_firm_clean)

  # Return immediately when no rows matched in this chunk
  if (!any(row_successfully_matched_to_firm_clean)) return(invisible(NULL))

  # Count exact-name matches by firm in this chunk and add to global counters
  if (any(row_matched_using_exact_name)) {
    chunk_exact_name_match_count_by_firm <- table(matched_firm_clean[row_matched_using_exact_name])
    firm_clean_match_count_exact_name[names(chunk_exact_name_match_count_by_firm)] <<- firm_clean_match_count_exact_name[names(chunk_exact_name_match_count_by_firm)] + as.integer(chunk_exact_name_match_count_by_firm)
  }

  # Count normalized-name matches by firm in this chunk and add to global counters
  if (any(row_matched_using_normalized_name)) {
    chunk_normalized_name_match_count_by_firm <- table(matched_firm_clean[row_matched_using_normalized_name])
    firm_clean_match_count_normalized_name[names(chunk_normalized_name_match_count_by_firm)] <<- firm_clean_match_count_normalized_name[names(chunk_normalized_name_match_count_by_firm)] + as.integer(chunk_normalized_name_match_count_by_firm)
  }

  # Count normalized-compact-name matches by firm in this chunk and add to global counters
  if (any(row_matched_using_normalized_compact_name)) {
    chunk_normalized_compact_name_match_count_by_firm <- table(matched_firm_clean[row_matched_using_normalized_compact_name])
    firm_clean_match_count_normalized_compact_name[names(chunk_normalized_compact_name_match_count_by_firm)] <<- firm_clean_match_count_normalized_compact_name[names(chunk_normalized_compact_name_match_count_by_firm)] + as.integer(chunk_normalized_compact_name_match_count_by_firm)
  }

  ## Build firm-by-SIC count table for this chunk and update global SIC count maps by firm with counts from this chunk
  # Build firm-by-raw-SIC frequency table for this chunk, subsetting to matched rows
  chunk_firm_by_primary_sic_count_table <- as.data.frame(
    table(
      matched_firm_clean[row_successfully_matched_to_firm_clean],
      primary_sic_code_refusa[row_successfully_matched_to_firm_clean]
    ),
    stringsAsFactors = FALSE
  )

  # Name firm and SIC columns explicitly in chunk-level table.
  names(chunk_firm_by_primary_sic_count_table)[1:2] <- c("firm_clean", "primary_sic_code_refusa")

  # Keep only rows with positive counts
  chunk_firm_by_primary_sic_count_table <- chunk_firm_by_primary_sic_count_table[chunk_firm_by_primary_sic_count_table$Freq > 0, , drop = FALSE]

  # Add chunk frequencies into each firm's hashed SIC count map
  for (chunk_row_idx in seq_len(nrow(chunk_firm_by_primary_sic_count_table))) {
    # Read firm identifier for this chunk-level firm-by-SIC cell 
    firm_clean_i <- as.character(chunk_firm_by_primary_sic_count_table$firm_clean[chunk_row_idx])
    
    # Read raw SIC code for this chunk-level firm-by-SIC cell 
    primary_sic_code_refusa_i <- as.character(chunk_firm_by_primary_sic_count_table$primary_sic_code_refusa[chunk_row_idx])
    
    # Read count to add for this firm-by-SIC cell from this chunk
    count_i <- as.integer(chunk_firm_by_primary_sic_count_table$Freq[chunk_row_idx])

    # Read existing count for this raw SIC code, defaulting to zero when this raw SIC code has not been observed before for this firm
    existing_count_i <- if (exists(primary_sic_code_refusa_i, envir = primary_sic_code_count_list_by_firm_clean[[firm_clean_i]], inherits = FALSE)) {
      get(primary_sic_code_refusa_i, envir = primary_sic_code_count_list_by_firm_clean[[firm_clean_i]], inherits = FALSE)
    } else {
      0L
    }

    # Update firm-specific raw SIC count
    assign(primary_sic_code_refusa_i, existing_count_i + count_i, envir = primary_sic_code_count_list_by_firm_clean[[firm_clean_i]])
  }

  # Return invisibly from callback
  invisible(NULL)
})

# Read RefUSA in chunks and update SIC counts through callback
readr::read_csv_chunked(
  # Read RefUSA file path 
  file = file.path(external, "2019_Business_Academic_QCQ.txt.gz"),

  # Use callback that updates SIC counts per chunk
  callback = callback,

  # Set chunk size
  chunk_size = 250000,

  # Restrict read to required columns only
  col_types = readr::cols_only(
    # Read Company column as character
    Company = readr::col_character(),

    # Read Primary SIC Code column as character
    `Primary SIC Code` = readr::col_character()
  ),

  # Disable progress bar for cleaner logs.
  progress = FALSE
)

# Now have primary_sic_code_count_list_by_firm_clean i.e., a list with one element per firm_clean value, where each element is a hash map with Primary SIC Code --> count mappings for that firm_clean value based on RefUSA matches and counts across all chunks

# Build vector of firm-level match key type labels in long_survey order using key-type precedence exact -> normalized -> normalized compact
refusa_firm_match_key_type <- ifelse(
  firm_clean_match_count_exact_name > 0L,
  "exact_name",
  ifelse(
    firm_clean_match_count_normalized_name > 0L,
    "normalized_name",
    ifelse(
      firm_clean_match_count_normalized_compact_name > 0L,
      "normalized_compact_name",
      NA_character_
    )
  )
)

# ------------------------------------------------------------------------------
# Convert hashed firm-level SIC count maps into one dataframe 
# with one row per firm_clean x raw Primary SIC Code value
# ------------------------------------------------------------------------------
# Initialize empty dataframe that will store one row per firm_clean x raw Primary SIC Code value
primary_sic_code_refusa_count_by_firm <- data.frame(
  # Initialize column to store firm_clean values
  firm_clean = character(0),

  # Initialize column to store raw Primary SIC Code values
  primary_sic_code_refusa = character(0),

  # Initialize column to store counts for each firm_clean x raw Primary SIC Code value
  primary_sic_code_count = integer(0),

  # Keep character columns as character vectors
  stringsAsFactors = FALSE
)

# Loop over firm_clean values in the firm-level SIC count map list and convert each firm's hashed SIC count map into a temporary dataframe with one row per raw Primary SIC Code value observed for that firm_clean value, and then append each temporary dataframe into the global firm-by-SIC count dataframe
for (firm_clean_i in names(primary_sic_code_count_list_by_firm_clean)) {
  # Read all raw Primary SIC Code values observed for this firm_clean value
  observed_primary_sic_code_refusa_values <- ls(primary_sic_code_count_list_by_firm_clean[[firm_clean_i]], all.names = TRUE)

  # Skip this firm_clean value when no raw Primary SIC Code values were observed
  if (length(observed_primary_sic_code_refusa_values) == 0L) next

  # Append this firm_clean value's observed SIC values and counts directly into the global firm-by-SIC count dataframe
  primary_sic_code_refusa_count_by_firm <- rbind(
    primary_sic_code_refusa_count_by_firm,
    data.frame(
      # Store firm_clean value in each row
      firm_clean = rep(firm_clean_i, length(observed_primary_sic_code_refusa_values)),

      # Store observed raw Primary SIC Code values
      primary_sic_code_refusa = observed_primary_sic_code_refusa_values,

      # Store observed counts from firm-specific hash map
      primary_sic_code_count = as.integer(vapply(
        observed_primary_sic_code_refusa_values,
        get,
        integer(1),
        envir = primary_sic_code_count_list_by_firm_clean[[firm_clean_i]],
        inherits = FALSE
      )),

      # Keep character columns as character vectors
      stringsAsFactors = FALSE
    )
  )
}

# Assert RefUSA primary SIC values are numeric strings.
stopifnot(all(grepl("^[0-9]+$", primary_sic_code_refusa_count_by_firm$primary_sic_code_refusa)))

# Browse firm-by-SIC count dataframe 
View(primary_sic_code_refusa_count_by_firm)

# ------------------------------------------------------------------------------
# Collapse firm-by-SIC count table to one modal raw Primary SIC 
# Code value per firm_clean value
# ------------------------------------------------------------------------------
# Count number of firms where ties in modal SIC code are actually being broken
primary_sic_code_refusa_modal_tie_count <- primary_sic_code_refusa_count_by_firm %>%
  dplyr::group_by(firm_clean) %>%
  dplyr::summarise(
    number_of_modal_sic_candidates = sum(primary_sic_code_count == max(primary_sic_code_count)),
    .groups = "drop"
  ) %>%
  dplyr::summarise(
    primary_sic_code_refusa_modal_tie_count = sum(number_of_modal_sic_candidates > 1L)
  ) %>%
  dplyr::pull(primary_sic_code_refusa_modal_tie_count)

# Report number of firm_clean values where tie-breaking is required for modal SIC code selection
message(sprintf(
  "Modal primary_sic_code_refusa tie-break applied for %s firm_clean values.",
  primary_sic_code_refusa_modal_tie_count
))

# Collapse to one-row-per-firm_clean value dataset, storing modal RefUSA primary SIC code per firm_clean value
# R analogue of Stata `collapse (mode) primary_sic_code_refusa, by(firm_clean)`
# Break ties in modal SIC code by taking the smallest numeric SIC value
primary_sic_code_refusa_modal_by_firm <- primary_sic_code_refusa_count_by_firm %>%
  
  # Group by firm_clean to prepare for collapsing to one row per firm_clean value
  dplyr::group_by(firm_clean) %>%
  
  # Summarize to one row per firm_clean value by taking modal raw Primary SIC Code value with tie-breaking rules described above
  dplyr::summarise(
    primary_sic_code_refusa_modal = {
      
      # Keep SIC candidates tied at the maximum observed count for this firm_clean value
      primary_sic_code_refusa_modal_candidates <- primary_sic_code_refusa[primary_sic_code_count == max(primary_sic_code_count)]

      # Break ties by taking the smallest numeric SIC value among candidates when there are ties
      primary_sic_code_refusa_modal_candidates[which.min(as.integer(primary_sic_code_refusa_modal_candidates))]
    },
    .groups = "drop"
  )

# Merge onto long_survey firm list to keep long_survey row order and keep NA for unmatched firms
primary_sic_code_refusa_modal <- long_survey %>%
  dplyr::select(firm_clean) %>%
  
  # Left join to keep all firm_clean values in long_survey and align modal SIC codes to long_survey row order, with NA modal SIC codes for unmatched firms
  dplyr::left_join(primary_sic_code_refusa_modal_by_firm, by = "firm_clean")

# Build vector of modal raw Primary SIC Code values aligned to long_survey row order
primary_sic_code_refusa_modal <- primary_sic_code_refusa_modal$primary_sic_code_refusa_modal

# Parse two-digit SIC from modal raw RefUSA SIC values
primary_sic_code_refusa_modal_two_digit <- as.integer(substr(primary_sic_code_refusa_modal, 1, 2))

# Keep only valid two-digit SIC values in 1..99
primary_sic_code_refusa_modal_two_digit[is.na(primary_sic_code_refusa_modal_two_digit) | primary_sic_code_refusa_modal_two_digit < 1 | primary_sic_code_refusa_modal_two_digit > 99] <- NA_integer_

# Aggregate SIC bins to match required AER-style bucket rules.
primary_sic_code_refusa_modal_two_digit_aer_aggregation <- dplyr::case_when(
  # Keep missing SIC values as missing
  is.na(primary_sic_code_refusa_modal_two_digit) ~ NA_character_,
  
  # Bucket SIC 24-35 to "24-35"
  primary_sic_code_refusa_modal_two_digit >= 24 & primary_sic_code_refusa_modal_two_digit <= 35 ~ "24-35",
  
  # Bucket SIC 42-47 to "42-47"
  primary_sic_code_refusa_modal_two_digit >= 42 & primary_sic_code_refusa_modal_two_digit <= 47 ~ "42-47",
  
  # Bucket SIC 50-51 to "50-51"
  primary_sic_code_refusa_modal_two_digit >= 50 & primary_sic_code_refusa_modal_two_digit <= 51 ~ "50-51",
  
  # Bucket SIC 61-64 to "61-64"
  primary_sic_code_refusa_modal_two_digit >= 61 & primary_sic_code_refusa_modal_two_digit <= 64 ~ "61-64",
  
  # Bucket SIC 65-70 to "65-70"
  primary_sic_code_refusa_modal_two_digit >= 65 & primary_sic_code_refusa_modal_two_digit <= 70 ~ "65-70",
  
  # Bucket SIC 72-73 to "72-73"
  primary_sic_code_refusa_modal_two_digit >= 72 & primary_sic_code_refusa_modal_two_digit <= 73 ~ "72-73",
  
  # Bucket SIC 75-76 to "75-76"
  primary_sic_code_refusa_modal_two_digit >= 75 & primary_sic_code_refusa_modal_two_digit <= 76 ~ "75-76",
  
  # Bucket SIC 80-87 to "80-87"
  primary_sic_code_refusa_modal_two_digit >= 80 & primary_sic_code_refusa_modal_two_digit <= 87 ~ "80-87",

  # Keep all other SIC codes unchanged as strings
  TRUE ~ as.character(primary_sic_code_refusa_modal_two_digit)
)

# ------------------------------------------------------------------------------
# Assemble final crosswalk output and write 
# /dump/firm_industry_crosswalk_refusa.csv
# ------------------------------------------------------------------------------
# Build final industry_map directly from long_survey, dropping extraneous variables and adding modal sic variables constructed above 
industry_map <- long_survey %>%
  dplyr::transmute(
    firm_clean = firm_clean,
    refusa_firm_match_key_type = refusa_firm_match_key_type,
    primary_sic_code_refusa_modal = primary_sic_code_refusa_modal,
    primary_sic_code_refusa_modal_two_digit = as.numeric(primary_sic_code_refusa_modal_two_digit),
    primary_sic_code_refusa_modal_two_digit_aer_aggregation = as.character(primary_sic_code_refusa_modal_two_digit_aer_aggregation)
  )

# Check that industry_map is unique on firm_clean
stopifnot(dplyr::n_distinct(industry_map$firm_clean) == nrow(industry_map))

# Write updated crosswalk to dump/firm_industry_crosswalk_refusa.csv
readr::write_csv(industry_map, file.path(dump, "firm_industry_crosswalk_refusa.csv"))

# Report output write path and match coverage
message(sprintf(
  "🎃 Wrote updated industry map to data/dump/firm_industry_crosswalk_refusa.csv with %s/%s matched firms.",
  sum(!is.na(industry_map$primary_sic_code_refusa_modal_two_digit)),
  nrow(industry_map)
))

e 
#XXuncomment out abi uniqueness check and also print how many ties we had to break at the relevant part of the above code
