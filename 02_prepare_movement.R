#################################################################
# Bachelor Thesis
# Skript: 02_prepare_movement.R
# AUthor: Dorin Bez
# Created: 13.07.2026
# Purpose: Import, clean and agreegate ELAN movement data
# Operator and last date of work: Dorin Bez, 16.07.2026
################################################################

###############################################################
# 1 load packages
##############################################################

library(tidyverse)
library(readxl)
library(janitor)

###############################################################
# 2 Import raw ELAN data
###############################################################

# Import the combined ELAN export containing all coded annotations.
# Column names are not imported because the Excel file does not contain a usable header row.
elan_raw <- read_excel(
  "data_raw/ELAN_coding.xlsx",
  col_names = FALSE
)

# Assign meaningful variable names manually.
# The second column is empty in the original ELAN export and is retained
# temporarily so that the remaining columns keep their correct positions.
names(elan_raw) <- c(
  "tier",
  "empty_column",
  "begin_time",
  "end_time",
  "annotation",
  "filename"
)

###############################################################
# 3 Inspect raw ELAN data
###############################################################

# Inspect the imported raw ELAN dataset before cleaning.
# This step verifies that all columns were imported correctly and that the dataset has the expected structure.

# Display structure of the dataset
glimpse(elan_raw)
# Check column names
names(elan_raw)
# Open the dataset for manual inspection
# View(elan_raw)

#############################################################
# 4 Clean raw ELAN data
############################################################

# The raw ELAN export contains one empty column.
# This column is removed because it does not contain any information and is not needed for further processing.
elan_clean <- elan_raw %>%
  select(-empty_column)

# Verify the structure after removing the empty column.
glimpse(elan_clean)
# Inspect the cleaned dataset manually.
# View(elan_clean)

###############################################################
# 5 Calculate annotation duration
###############################################################

# duration_ms:
# Time difference between annotation start and end.

# duration_sec:
# Same duration expressed in seconds.
# This variable is used in all subsequent analyses.

# Convert annotation duration from milliseconds to seconds
elan_clean <- elan_clean %>%
  mutate(
    # Calculate the duration of every movement annotation.
    # ELAN stores time values in milliseconds.
    # Therefore, the duration is first calculated in milliseconds and then converted to seconds.
    duration_ms = end_time - begin_time,
    duration_sec = duration_ms / 1000
  )

# Inspect the distribution of annotation durations to identify implausible values or outliers.
summary(elan_clean$duration_sec)

# Check the number of annotations in each ELAN tier.
# This confirms that all expected annotation layers were imported successfully.
table(elan_clean$tier)

##############################################################
# 6 Verify imported ELAN files
#############################################################

# Count the number of unique ELAN files that were imported.
# Expected:
# - 25 regular recordings
# - 2 separately coded beginning recordings
# = 27 unique ELAN files in total.
n_distinct(elan_clean$filename)

# Count the separately coded beginning recordings.
# These files contain only the first part of two recordings and were coded separately in ELAN.
elan_clean %>%
  distinct(filename) %>%
  filter(str_detect(str_to_lower(filename), "beginning")) %>%
  summarise(n_beginning_files = n())

# Count the regular recording files.
# These are all recordings except the separately coded beginnings.
elan_clean %>%
  distinct(filename) %>%
  filter(!str_detect(str_to_lower(filename), "beginning")) %>%
  summarise(n_regular_files = n())

##############################################################
# 7 Identify separately coded beginnings
##############################################################

# Two recordings were coded in two separate ELAN files:
# one file contains the beginning of the recording,
# the second file contains the remainder.
#
# To analyse these recordings as one continuous trial,
# the beginning file is linked to its corresponding main file.

# Create a lookup table that links each separately coded beginning file to its corresponding main recording.
beginning_lookup <- tibble(
  beginning_filename = c(
    "K008_K007_none_JA_dp1_early_JA_14.05.25_top.eaf_beginning.eaf",
    "K008_K007_none_NC_dp1_late_NC_21.05.25_side_right.eaf_Beginning.eaf"
  ),
  main_filename = c(
    "K008_K007_none_JA_dp1_early_JA_14.05.25_side_right.ELAN.eaf",
    "K008_K007_none_NC_dp1_late_NC_21.05.25_side_right.ELAN.eaf"
  )
)

# Join the lookup table to the ELAN dataset.
# Only the two beginning recordings receive a corresponding main filename; all other recordings remain unchanged.
elan_clean <- elan_clean %>%
  left_join(
    beginning_lookup,
    by = c("filename" = "beginning_filename")
  ) %>%
  mutate(
    # TRUE only for the two separately coded beginning files.
    # FALSE for all regular recordings.
    separately_coded_beginning = !is.na(main_filename),
    
    # Preserve the original ELAN filename.
    # This information is retained for traceability.
    source_filename = filename,
    
    # Create a common recording identifier.
    #
    # Beginning files receive the filename of their corresponding
    # main recording.
    #
    # Regular recordings keep their original filename.
    #
    # This allows beginning and main recordings to be analysed
    # as one continuous recording later in the pipeline.
    analysis_filename = coalesce(main_filename, filename)
  )

# Verify that exactly two recordings were identified
# as separately coded beginnings.
#
# Expected result:
# FALSE = 25
# TRUE  = 2
elan_clean %>%
  distinct(filename, analysis_filename, separately_coded_beginning) %>%
  count(separately_coded_beginning)

# Display the filename mapping for manual inspection.
# Expected result:
# Two rows showing beginning_filename -> analysis_filename.
elan_clean %>%
  filter(separately_coded_beginning) %>%
  distinct(filename, analysis_filename) %>%
  View()

###############################################################
# 8 Extract ID1 and ID2 child assignments
###############################################################

# Extract the child identifiers assigned to ID1 and ID2 from each ELAN recording.
#
# These assignments are stored separately in the ELAN file and will later be attached to every movement annotation.
id_lookup <- elan_clean %>%
  # Keep only the ELAN tiers containing the child IDs.
  filter(tier %in% c("ID1", "ID2")) %>%
  # Keep only the variables needed to build the lookup table.
  select(filename, tier, annotation) %>%
  # Remove duplicate rows.
  # Each recording should contain only one ID1 and one ID2 assignment.
  distinct() %>%
  # Convert the long-format ID assignments into one row per recording.
  #
  # Example:
  # filename | tier | annotation
  #
  # becomes
  #
  # filename | child_ID1 | child_ID2
  pivot_wider(
    names_from = tier,
    values_from = annotation,
    names_prefix = "child_"
  )

# Verify that every recording has both child assignments.
#
# Expected result:
# missing_ID1 = 0
# missing_ID2 = 0
id_lookup %>%
  summarise(
    missing_ID1 = sum(is.na(child_ID1)),
    missing_ID2 = sum(is.na(child_ID2))
  )

# Verify that each recording contains exactly one ID1 and one ID2 assignment.
#
# Expected result:
# 0 rows returned
elan_clean %>%
  filter(tier %in% c("ID1", "ID2")) %>%
  count(filename, tier) %>%
  filter(n != 1)

###############################################################
# 9 Add child ID assignments to the ELAN dataset
###############################################################

elan_clean <- elan_clean %>%
  # Merge the child ID lookup table with the ELAN dataset.
  #
  # Every annotation now receives the corresponding child_ID1 and child_ID2 belonging to its recording.
  # Match the lookup table using the recording filename.
  left_join(id_lookup, by = "filename")

# Verify that every annotation received both child assignments.
#
# Expected result:
# missing_ID1 = 0
# missing_ID2 = 0
elan_clean %>%
  summarise(
    missing_ID1 = sum(is.na(child_ID1)),
    missing_ID2 = sum(is.na(child_ID2))
  )

###############################################################
# 10 Create the movement dataset
###############################################################

# Keep only movement annotations (ID1 and ID2 tiers).
#
# Each movement annotation is assigned to the corresponding child
# based on the movement tier (Movement_level_ID1 or Movement_level_ID2).
#
# The result is an annotation-level dataset in which every movement belongs to exactly one child.
movement_data <- elan_clean %>%
  # Remove all non-movement tiers.
  #
  # Only movement annotations coded for child ID1 and child ID2
  # are retained for further analysis.
  filter(tier %in% c("Movement_level_ID1", "Movement_level_ID2")) %>%
  # Assign the correct child ID to every movement annotation.
  #
  # Annotations from Movement_level_ID1 receive child_ID1,
  # annotations from Movement_level_ID2 receive child_ID2.
  #
  # This creates one common variable (child_id) that identifies the child performing each movement.
  mutate(
    child_id = case_when(
      tier == "Movement_level_ID1" ~ child_ID1,
      tier == "Movement_level_ID2" ~ child_ID2,
      TRUE ~ NA_character_
    )
  )

# Verify that every movement annotation was assigned to the correct child.
#
# This quality check lists, for each recording, which child ID belongs to each movement tier.
movement_data %>%
  distinct(filename, child_id, tier) %>%
  arrange(filename, child_id)

############################################################
# Correct known typo before extracting filename metadata
############################################################

# In this recording, the second interruption condition was
# accidentally written as "e" instead of "a".

movement_data <- movement_data %>%
  mutate(
    filename = str_replace(
      filename,
      fixed("K006_K012_dp2_early_JA_e_early_JA_30.04.25"),
      "K006_K012_dp2_early_JA_a_early_JA_30.04.25"
    )
  )

###############################################################
# 11 Extract metadata from ELAN filenames
###############################################################

# The ELAN filenames contain all experimental metadata, including child IDs, interruption conditions, timing,
# box conditions and recording date.
#
# These values are extracted once and stored as variables for later analyses.
movement_data <- movement_data %>%
  # Extract all experimental information encoded in the filename.
  #
  # Some filenames contain only one interruption condition or
  # missing timing information. 
  # Therefore, optional regex groups are used so that all filenames can still be parsed correctly.
  extract(
    filename,
    into = c(
      "file_child_1",
      "file_child_2",
      "interruption_condition_1",
      "timing_1",
      "box_condition_1",
      "interruption_condition_2",
      "timing_2",
      "box_condition_2",
      "date"
    ),
    regex = paste0(
      "(?i)^",
      "(K\\d+)_(K\\d+)_",
      "(a|dp1|dp2|none)",
      "(?:_(early|late))?_",
      "(JA|NC)",
      "(?:_",
      "(a|dp1|dp2|none)",
      "(?:_(early|late))?_",
      "(JA|NC)",
      ")?_",
      "(\\d{2}\\.\\d{2}\\.\\d{2})_.*$"
    ),
    remove = FALSE
  ) %>%
  mutate(
    # Standardise extracted values.
    #
    # Convert text to a consistent format (upper/lower case) so that later comparisons and filtering work reliably.
    interruption_condition_1 = str_to_lower(interruption_condition_1),
    interruption_condition_2 = str_to_lower(interruption_condition_2),
    timing_1 = str_to_lower(timing_1),
    timing_2 = str_to_lower(timing_2),
    box_condition_1 = str_to_upper(box_condition_1),
    box_condition_2 = str_to_upper(box_condition_2)
  )

# Correct known typos in recording filenames.
movement_data <- movement_data %>%
  mutate(
    date = case_when(
      date == "07.08.25" ~ "07.05.25",
      date == "15.06.25" ~ "25.06.25",
      TRUE ~ date
    )
  )

# Verify that the filename was parsed successfully.
#
# No child IDs or recording dates should be missing after extracting the metadata.
movement_data %>%
  distinct(filename, file_child_1, file_child_2, date) %>%
  filter(
    is.na(file_child_1) |
      is.na(file_child_2) |
      is.na(date)
  )

###############################################################
# 12 Define fixed participant pairs
###############################################################

# Define the fixed participant pairs used throughout the study.
#
# This lookup table assigns the original participant roles (participant_1 and participant_2) to each child pair.
#
# An order-independent pair identifier (pair_key) is created so that
# participant pairs can be recognised consistently regardless of the order of child IDs in the ELAN filename.
participant_pairs <- tibble(
  participant_1 = c("K011", "K006", "K002", "K014", "K007", "K010", "K004"),
  participant_2 = c("K005", "K012", "K008", "K009", "K008", "K013", "K003")
) %>%
  mutate(
    # Create a unique pair identifier so that participant pairs can
    # be matched regardless of the order of the child IDs in the
    # filename (e.g. K005_K011 equals K011_K005).
    pair_key = map2_chr(
      participant_1,
      participant_2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  )

# Inspect the lookup table used for all participant pair assignments.
View(participant_pairs)

###############################################################
# 13 link participant pairs to movement data
###############################################################

# Match each movement annotation to its corresponding participant pair.
#
# A pair_key is created from the two child IDs extracted from the
# filename and used to join the participant lookup table.
#
# This assigns the original participant roles
# (participant_1 and participant_2) to every movement annotation.
movement_data <- movement_data %>%
  mutate(
    pair_key = map2_chr(
      file_child_1,
      file_child_2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  left_join(
    participant_pairs,
    by = "pair_key"
  )

# Quality check:
# Every movement annotation should now have a participant_1 and participant_2 assignment.
movement_data %>%
  distinct(filename, participant_1, participant_2) %>%
  filter(
    is.na(participant_1) |
      is.na(participant_2)
  )

###############################################################
# 14 Create trial intervals from interruption annotations
###############################################################

# Retain one row per interruption interval.
# Each interruption interval represents one experimental trial.
# Trial numbers are assigned separately within each recording.

trial_intervals <- elan_clean %>%
  filter(tier == "Interruption_condition") %>%
  transmute(
    filename,
    trial_start = begin_time,
    trial_end = end_time,
    interruption_condition = str_to_lower(annotation)
  ) %>%
  group_by(filename) %>%
  arrange(trial_start, .by_group = TRUE) %>%
  mutate(
    trial = row_number()
  ) %>%
  ungroup()

# Check how many trials were detected per recording.
trial_intervals %>%
  count(filename, name = "n_trials") %>%
  count(n_trials)

# Inspect extracted trial intervals.
View(trial_intervals)

# Quality check:
# Regular recordings are expected to contain two trials.
# Files returned here require manual inspection.
trial_intervals %>%
  count(filename, name = "n_trials") %>%
  filter(n_trials != 2)

# Inspect recordings with only one detected trial.
# These correspond to recordings with a single interruption interval.
trial_intervals %>%
  count(filename, name = "n_trials") %>%
  filter(n_trials == 1)

###########################################################
# 15 Assign each movement annotation to the corresponding trial
###########################################################

# A movement annotation is assigned to a trial if it overlaps
# with the interruption condition interval defining that trial.
# This accounts for minor coding inaccuracies where the first
# movement may start slightly before the trial begins or the
# last movement may end slightly after the trial ends.
movement_data$trial <- NA_integer_

for(i in seq_len(nrow(trial_intervals))){
  
  idx <- movement_data$filename == trial_intervals$filename[i] &
    movement_data$end_time >= trial_intervals$trial_start[i] &
    movement_data$begin_time <= trial_intervals$trial_end[i]
  
  movement_data$trial[idx] <- trial_intervals$trial[i]
}

# Quality check:
# Count movement annotations assigned to each trial.
movement_data %>%
  count(trial)

movement_data %>%
  distinct(
    filename,
    trial,
    interruption_condition_1,
    interruption_condition_2
  ) %>%
  arrange(filename, trial) %>%
  
  # Inspect trial assignments together with interruption conditions.
  View()

############################################################
# 15b Correct known single-trial recordings
############################################################

# In these five recordings, only trial 2 was coded.
# Because only one interruption interval was detected,
# it was provisionally numbered as trial 1.
# The trial number is corrected to trial 2 before assigning
# the corresponding interruption condition.

movement_data <- movement_data %>%
  mutate(
    trial = case_when(
      child_id == "K006" &
        str_detect(analysis_filename, "30\\.04\\.25") ~ 2L,
      
      child_id == "K005" &
        str_detect(analysis_filename, "07\\.08\\.25") ~ 2L,
      
      child_id == "K009" &
        str_detect(analysis_filename, "08\\.05\\.25") ~ 2L,
      
      child_id == "K002" &
        str_detect(analysis_filename, "13\\.05\\.25") ~ 2L,
      
      child_id == "K009" &
        str_detect(analysis_filename, "14\\.05\\.25") ~ 2L,
      
      TRUE ~ trial
    )
  )

###########################################################
# 16 Assign the interruption condition to each trial
###########################################################

# The filename contains the interruption conditions for trial 1 and trial 2 in two separate variables.
#
# For recordings containing both trials, the condition corresponding to the assigned trial number is selected.
#
# Important:
# For recordings containing only one detected interruption interval,
# the interval was provisionally labelled as trial 1 in the previous step. 
# Therefore, the true experimental trial must be verified separately before this assignment can be considered final.
movement_data <- movement_data %>%
  mutate(
    interruption_condition = case_when(
      trial == 1 ~ interruption_condition_1,
      trial == 2 ~ interruption_condition_2,
      TRUE ~ NA_character_
    )
  )

# Quality check:
# Inspect the distribution of interruption conditions by assigned trial.
movement_data %>%
  count(trial, interruption_condition)

###############################################################
# 17 Assign separately coded beginnings to trial 1
###############################################################

# The two separately coded beginning files contain only the missing
# beginning of trial 1.
#
# Their original ELAN time coordinates are retained, but the annotations
# are assigned to trial 1 of the corresponding main recording.
#
# The interruption condition of trial 1 is also assigned explicitly,
# because these beginning annotations were not matched to a complete
# interruption interval in the previous steps.
movement_data <- movement_data %>%
  mutate(
    trial = if_else(
      separately_coded_beginning,
      1L,
      trial
    ),
    
    interruption_condition = if_else(
      separately_coded_beginning,
      interruption_condition_1,
      interruption_condition
    )
  )

# Quality check:
# All separately coded beginning annotations should belong to trial 1
# and should have the interruption condition of trial 1.
movement_data %>%
  filter(separately_coded_beginning) %>%
  distinct(
    source_filename,
    analysis_filename,
    trial,
    interruption_condition
  ) %>%
  View()

###############################################################
# 18 Import verified child-trial assignments
###############################################################

# The coding overview records which child was coded in trial 1
# and trial 2 of each recording.
#
# This information is used to correct cases in which only one trial
# was coded for a child and the chronological trial assignment is
# therefore not sufficient.

# Import the manually checked coding overview.
# The columns for trial 1 and trial 2 indicate which child was coded in each experimental trial.
coding_overview <- read_excel(
  "data_raw/ELAN_coding_overview.xlsx",
  sheet = "Tabelle1"
)

# Keep only the filename and the two coding columns.
# Empty filename rows are removed.
coding_overview <- coding_overview %>%
  rename(
    filename_overview = Name,
    trial_1_coded = `1 trail`,
    trial_2_coded = `2 trail`
  ) %>%
  filter(!is.na(filename_overview))

# Optional manual inspection of the cleaned coding overview.
coding_overview %>%
  select(
    filename_overview,
    trial_1_coded,
    trial_2_coded
  ) %>%
  View()

# Convert the trial columns from wide to long format.
# Each row then represents one recording and one experimental trial.
child_trial_lookup <- coding_overview %>%
  select(
    filename_overview,
    trial_1_coded,
    trial_2_coded
  ) %>%
  pivot_longer(
    cols = c(trial_1_coded, trial_2_coded),
    names_to = "trial_column",
    values_to = "coded_children"
  ) %>%
  mutate(
    # Translate the original column name into the experimental trial number.
    true_trial = case_when(
      trial_column == "trial_1_coded" ~ 1L,
      trial_column == "trial_2_coded" ~ 2L,
      TRUE ~ NA_integer_
    ),
    # Extract all child IDs such as K003 or K012 from the coding entry.
    child_id = str_extract_all(
      coded_children,
      "K\\d{3}"
    )
  ) %>%
  unnest(child_id) %>%
  # Entries such as "x" contain no child ID and are removed here.
  filter(!is.na(child_id)) %>%
  # Retain one unique row per recording, child and trial.
  select(
    filename_overview,
    child_id,
    true_trial
  ) %>%
  distinct()

# Inspect the resulting verified child-trial lookup table.
View(child_trial_lookup)

###############################################################
# 19 Filter movement data to verified coded child-trial combinations
###############################################################

# The trial assignment in movement_data was determined from the interruption time intervals.
#
# The coding overview is now used only to verify whether a given child  was actually coded in that trial.
#
# Movement annotations are retained only if the combination of
# recording, child and trial occurs in the coding overview.

verified_child_trials <- child_trial_lookup %>%
  rename(
    analysis_filename = filename_overview,
    trial = true_trial
  ) %>%
  distinct(
    analysis_filename,
    child_id,
    trial
  )

View(verified_child_trials)

movement_data_before_filter <- movement_data

###############################################################
# 20 Replace provisional trial numbers with verified assignments
###############################################################

# The initial trial variable was assigned chronologically within each
# recording. If only one trial was coded, it was therefore labelled as
# trial 1 even when it actually represented experimental trial 2.
#
# The coding overview is used here to translate the chronological trial
# number into the verified experimental trial number.


# Determine the chronological position of each child within
# every recording based on the current movement annotations.
#
# Example:
# Recording A
# K003 -> first observed child  -> trial_position = 1
# K004 -> second observed child -> trial_position = 2
observed_child_trials <- movement_data %>%
  distinct(
    analysis_filename,
    child_id,
    trial
  ) %>%
  arrange(
    analysis_filename,
    child_id,
    trial
  ) %>%
  group_by(
    analysis_filename,
    child_id
  ) %>%
  mutate(
    trial_position = row_number()
  ) %>%
  ungroup()

# Create the same chronological positions for the verified child-trial assignments imported from the coding overview.
# This allows both tables to be matched by position.
verified_trial_positions <- verified_child_trials %>%
  arrange(
    analysis_filename,
    child_id,
    trial
  ) %>%
  group_by(
    analysis_filename,
    child_id
  ) %>%
  mutate(
    trial_position = row_number(),
    verified_trial = trial
  ) %>%
  ungroup() %>%
  select(
    analysis_filename,
    child_id,
    trial_position,
    verified_trial
  )

# Match the observed child positions to the verified trial positions.
#
# After this join, each child receives the correct experimental
# trial number from the coding overview.
trial_correction_lookup <- observed_child_trials %>%
  left_join(
    verified_trial_positions,
    by = c(
      "analysis_filename",
      "child_id",
      "trial_position"
    )
  ) %>%
  select(
    analysis_filename,
    child_id,
    trial,
    verified_trial
  )

# Apply the verified trial numbers to every movement annotation.
#
# If a verified trial number exists, it replaces the provisional chronological trial assignment.
#
# coalesce() keeps the verified trial if available and otherwise retains the original trial number.
movement_data <- movement_data %>%
  left_join(
    trial_correction_lookup,
    by = c(
      "analysis_filename",
      "child_id",
      "trial"
    )
  ) %>%
  mutate(
    trial = coalesce(verified_trial, trial)
  ) %>%
  select(-verified_trial)

nrow(movement_data)

# Quality check
# Verify that every child-trial combination in movement_data is represented in the verified coding overview.
#
# Expected result:
# An empty table (0 rows).
#
# Any remaining rows indicate child-trial combinations that were
# not successfully matched to the verified coding overview.
movement_data %>%
  distinct(
    analysis_filename,
    child_id,
    trial
  ) %>%
  anti_join(
    verified_child_trials,
    by = c(
      "analysis_filename",
      "child_id",
      "trial"
    )
  ) %>%
  View()

############################################################
# 21 Define emotion child and removed child
############################################################

# In deliberate interruption trials:
# dp1 = participant 1 experiences the interruption,
#       participant 2 is removed.
# dp2 = participant 2 experiences the interruption,
#       participant 1 is removed.
#
# In accidental ("a") and no interruption ("none") trials, no child is assigned as the emotion child.

movement_data <- movement_data %>%
  mutate(
    # Create two new variables:
    # emotion_child = participant experiencing the interruption.
    # removed_child = participant leaving the interaction.
    emotion_child = case_when(
      interruption_condition == "dp1" ~ participant_1,
      interruption_condition == "dp2" ~ participant_2,
      TRUE ~ NA_character_
    ),
    
    removed_child = case_when(
      interruption_condition == "dp1" ~ participant_2,
      interruption_condition == "dp2" ~ participant_1,
      TRUE ~ NA_character_
    )
  )

# Quality check:
# Inspect the newly assigned emotion and removed child variables.

movement_data %>%
  distinct(
    interruption_condition,
    participant_1,
    participant_2,
    emotion_child,
    removed_child
  ) %>%
  View()

############################################################
# 19 Identify the role of the coded child
############################################################

# Classify the coded child according to the roles defined above.
movement_data <- movement_data %>%
  mutate(
    coded_role = case_when(
      child_id == emotion_child ~ "emotion_child",
      child_id == removed_child ~ "removed_child",
      interruption_condition %in% c("a", "none") ~ "both_present",
      TRUE ~ "other"
    )
    )

# Quality check:
# Inspect the distribution of coded roles across interruption conditions.
movement_data %>%
  count(interruption_condition, coded_role)

###############################################################
# 23 Standardize movement annotations labels
###############################################################

# Some movement categories were entered with inconsistent labels in ELAN.
# Standardize these labels before classifying movements into low and high
# movement categories.

# - "hand_movement hand_movement" -> "hand_movement"
# - "others" -> "other"
movement_data <- movement_data %>%
  mutate(
    annotation = case_when(
      annotation == "hand_movement hand_movement" ~ "hand_movement",
      annotation == "others" ~ "other",
      TRUE ~ annotation
    )
  )

###############################################################
# 24 Classify movement into low and high activity
###############################################################

# Each original ELAN movement annotation is assigned to one of
# two broader analytical movement levels: "low" or "high".
#
# Low movement includes predominantly stationary behaviours
# with limited whole-body movement or little translocation.
#
# High movement includes observable whole-body activity,
# locomotion, active posture changes or clear bodily activation.
#
# The category "other" is provisionally assigned to low movement.
# The corresponding video segments can be checked separately.
#
# If an annotation does not match any category listed below,
# movement_level is set to NA. This allows unidentified categories
# to be detected in the following quality check.
movement_data <- movement_data %>%
  mutate(
    movement_level = case_when(
      
      annotation %in% c(
        "lying",
        "sitting",
        "standing",
        "hand_movement",
        "bending_leaning",
        "other"
      ) ~ "low",
      
      annotation %in% c(
        "crouching",
        "pushing_pulling",
        "walking",
        "crawling",
        "climbing",
        "jumping",
        "running"
      ) ~ "high",
      
      TRUE ~ NA_character_
    )
  )

# Quality check:
# Count the original movement annotations within each assigned movement level. 
# No annotation should have an NA movement level.
movement_data %>%
  count(annotation, movement_level)

###############################################################
# 25 Create trial-specific experimental variables
###############################################################

# Each ELAN filename contains the experimental conditions for
# both trials in separate variables:
#
# - timing_1 and box_condition_1 describe trial 1.
# - timing_2 and box_condition_2 describe trial 2.
#
# However, every movement annotation has already been assigned
# to either trial 1 or trial 2 through the variable "trial".
#
# This step therefore creates one trial-specific variable called
# "timing" and one trial-specific variable called "box_condition".
#
# For annotations belonging to trial 1, the values ending in "_1"
# are used. For annotations belonging to trial 2, the values
# ending in "_2" are used.
movement_data <- movement_data %>%
  mutate(
    timing = case_when(
      trial == 1 ~ timing_1,
      trial == 2 ~ timing_2,
      TRUE ~ NA_character_
    ),
    
    box_condition = case_when(
      trial == 1 ~ box_condition_1,
      trial == 2 ~ box_condition_2,
      TRUE ~ NA_character_
    )
  )

############################################################
# 26 Exclude unassigned healthy-trial annotations
############################################################

# On 2025-04-16, an additional healthy trial was recorded before
# the intended experimental trial.
# Following the supervisor's decision, only the experimental trial
# is retained for the analysis.
movement_data <- movement_data %>%
  filter(
    !(
      child_id == "K005" &
        grepl("K005_K011", analysis_filename) &
        is.na(trial) &
        is.na(interruption_condition)
    )
  )

###############################################################
# 27 Create trial-level movement summary
###############################################################

# Until this point, movement_data is on the annotation level:
# every row represents one individually coded movement episode.
#
# For the statistical analysis, the movement data must be
# aggregated to the trial level. The resulting dataset
# "trial_summary" contains one row per child and trial.
#
# The data are grouped by:
#
# - analysis_filename:
#   identifies the experimental recording and combines separately
#   coded beginning files with their corresponding main recording.
#
# - child_id:
#   identifies the child whose movement was coded.
#
# - trial:
#   distinguishes trial 1 from trial 2 within the recording.
#
# Within each child-trial combination, the experimental conditions
# are retained and movement frequency, duration and proportion
# are calculated.
trial_summary <- movement_data %>%
  group_by(
    analysis_filename,
    child_id,
    trial
  ) %>%
  summarise(
    # Retain the experimental information belonging to the trial.
    # Because these values are constant within each trial,
    # the first value can be used.
    interruption_condition = first(interruption_condition),
    timing = first(timing),
    box_condition = first(box_condition),
    date = first(date),
    emotion_child = first(emotion_child),
    removed_child = first(removed_child),
    coded_role = first(coded_role),
    # Frequency:
    # Number of separately coded low- or high-movement episodes
    # occurring during the respective trial.
    low_frequency = sum(movement_level == "low"),
    high_frequency = sum(movement_level == "high"),
    # Duration:
    # Sum of the duration in seconds of all annotations classified
    # as low or high movement within the respective trial.
    low_duration_sec =
      sum(duration_sec[movement_level == "low"], na.rm = TRUE),
    
    high_duration_sec =
      sum(duration_sec[movement_level == "high"], na.rm = TRUE),
    # Total coded duration:
    # Sum of the duration of all movement annotations in the trial,
    # irrespective of whether they were classified as low or high.
    total_duration_sec =
      sum(duration_sec, na.rm = TRUE),
    # Remove the grouping structure after creating the summary.
    .groups = "drop"
  ) %>%
  # Proportion:
  # The absolute low- and high-movement durations are divided by
  # the total coded duration of the trial.
  #
  # This standardises the movement measures for differences in
  # trial length. The two proportions should add up to approximately 1.
  mutate(
    low_proportion = low_duration_sec / total_duration_sec,
    high_proportion = high_duration_sec / total_duration_sec
  )

# Check the dimensions of the trial-level dataset.
# Expected result: 52 rows, representing 52 coded child-trials.
dim(trial_summary)

# Check whether each recording-child-trial combination occurs
# exactly once in trial_summary.
#
# Expected result: an empty table with 0 rows.
trial_summary %>%
  count(analysis_filename, child_id, trial) %>%
  filter(n != 1)

# Check whether low and high proportions add up to approximately 1.
# Minor numerical rounding differences are possible.
trial_summary %>%
  mutate(
    proportion_sum =
      low_proportion + high_proportion
  ) %>%
  summarise(
    minimum_sum = min(proportion_sum, na.rm = TRUE),
    maximum_sum = max(proportion_sum, na.rm = TRUE),
    missing_sum = sum(is.na(proportion_sum))
  )

###############################################################
# 28 Final quality checks
###############################################################

# The annotation-level dataset should not contain missing child IDs.
movement_data %>%
  summarise(
    missing_child = sum(is.na(child_id))
  )

# Every annotation should have a low or high movement classification.
movement_data %>%
  summarise(
    missing_movement_level = sum(is.na(movement_level))
  )

# The trial-level dataset should contain 52 rows.
dim(trial_summary)

# Each combination of recording, child and trial should occur once.
# Expected result: 0 rows.
trial_summary %>%
  count(analysis_filename, child_id, trial) %>%
  filter(n != 1)

# Low and high proportions should add up to approximately 1.
trial_summary %>%
  mutate(
    proportion_sum = low_proportion + high_proportion
  ) %>%
  summarise(
    minimum_sum = min(proportion_sum, na.rm = TRUE),
    maximum_sum = max(proportion_sum, na.rm = TRUE),
    missing_sum = sum(is.na(proportion_sum))
  )

###############################################################
# 29 Export final movement datasets
###############################################################

# Annotation-level dataset:
# One row represents one coded movement annotation.
write_csv(
  movement_data,
  "data_processed/movement_data_final.csv"
)

# Trial-level dataset:
# One row represents one child in one experimental trial.
write_csv(
  trial_summary,
  "data_processed/trial_summary.csv"
)

















