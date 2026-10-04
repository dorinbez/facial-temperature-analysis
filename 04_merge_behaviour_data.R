############################################################
# Bachelor Thesis
# Script: 04_merge_behaviour_data.R
# Author: Dorin Bez
# Created: 17.07.2026
# Purpose: Merge the prepared movement, friendship and emotion
#          datasets into a trial-level analysis dataset.
# Operator and last date of work: Dorin Bez, 20.07.2026
############################################################

############################################################
# 1 Load libraries
############################################################

library(tidyverse)

############################################################
# 2 Define file paths
############################################################

movement_file <- "data_processed/trial_summary.csv"
friendship_file <- "data_processed/friendship_clean.csv"
emotion_file <- "data_processed/emotion_clean.csv"

############################################################
# 3 Import prepared datasets
############################################################

# Import movement dataset (one row per child and trial).
movement_data <- read_csv(movement_file)

# Import friendship questionnaire.
friendship_data <- read_csv(friendship_file)

# Import emotion questionnaire.
emotion_data <- read_csv(emotion_file)

############################################################
# 4 Inspect imported datasets
############################################################

# Inspect the number of rows and columns in each dataset.
glimpse(movement_data)
glimpse(friendship_data)
glimpse(emotion_data)

# Inspect variable names and data types.
# View(movement_data)
# View(friendship_data)
# View(emotion_data)

############################################################
# 5 Prepare merge identifiers
############################################################

# Convert the movement recording date to a proper R date.
# This ensures that it has the same format as the date in emotion_data.
movement_data <- movement_data %>%
  mutate(
    date = as.Date(date, format = "%d.%m.%y")
  )

# Extract both child IDs from the recording filename.
# Each recording filename contains the IDs of the two children in the dyad.
movement_data <- movement_data %>%
  mutate(
    dyad_ids = str_extract_all(analysis_filename, "K\\d{3}")
  )

# Identify the partner as the child ID in the filename that differs
# from the child whose behaviour was coded in the current row.
movement_data <- movement_data %>%
  rowwise() %>%
  mutate(
    partner_id = {
      ids <- unique(dyad_ids)
      partners <- ids[ids != child_id]
      
      if (length(partners) == 1) {
        partners
      } else {
        NA_character_
      }
    }
  ) %>%
  ungroup() %>%
  select(-dyad_ids)

# Check whether a partner ID was identified for every movement row.
movement_data %>%
  summarise(
    missing_partner_id = sum(is.na(partner_id))
  )

# Inspect the resulting child-partner assignments.
movement_data %>%
  distinct(
    analysis_filename,
    child_id,
    partner_id
  ) %>%
  arrange(
    analysis_filename,
    child_id
  ) %>%
  View()

############################################################
# 6 Merge friendship questionnaire
############################################################

# Add the friendship ratings reported by each child about the partner
# with whom the child participated in the respective recording.
behaviour_data <- movement_data %>%
  left_join(
    friendship_data,
    by = c("child_id", "partner_id")
  )

# The merge should not change the number of movement rows.
dim(movement_data)
dim(behaviour_data)

# Count missing friendship questionnaire values after the merge.
behaviour_data %>%
  summarise(
    missing_friendship_rating = sum(is.na(friendship_rating)),
    missing_liking_rating = sum(is.na(liking_rating))
  )

# # Check for child-partner combinations without questionnaire data.
behaviour_data %>%
  filter(
    is.na(friendship_rating) |
      is.na(liking_rating)
  ) %>%
  distinct(
    child_id,
    partner_id
  ) %>%
  arrange(
    child_id,
    partner_id
  ) %>%
  View()

# Check whether any behavioural trial rows lack friendship questionnaire data.
behaviour_data %>%
  filter(
    is.na(friendship_rating) |
      is.na(liking_rating)
  ) %>%
  count(
    child_id,
    partner_id,
    name = "affected_trial_rows"
  )

# Verify that the missing combinations are absent from
# the original friendship questionnaire dataset.
anti_join(
  movement_data %>%
    distinct(child_id, partner_id),
  friendship_data %>%
    distinct(child_id, partner_id),
  by = c("child_id", "partner_id")
)

############################################################
# 7 Check trial identifiers before merging emotion data
############################################################

# Display all unique trial labels in the movement dataset.
sort(unique(movement_data$trial))

# Display all unique trial labels in the emotion questionnaire.
sort(unique(emotion_data$trial_1))
sort(unique(emotion_data$trial_2))

############################################################
# 8 Reshape emotion questionnaire to trial level
############################################################

# Convert the two trial-condition columns into long format.
# This creates one row per child, date and experimental trial.
emotion_trial_data <- emotion_data %>%
  pivot_longer(
    cols = c(trial_1, trial_2),
    names_to = "trial_position",
    values_to = "interruption_condition"
  ) %>%
  mutate(
    trial = case_when(
      trial_position == "trial_1" ~ 1,
      trial_position == "trial_2" ~ 2
    )
  ) %>%
  select(
    date,
    child_id,
    trial,
    interruption_condition,
    emotion_accidental,
    emotion_deliberate,
    emotion_none,
    comments
  )

# Inspect the reshaped emotion dataset.
dim(emotion_trial_data)
glimpse(emotion_trial_data)
View(emotion_trial_data)

# Remove rows that do not represent an actually completed trial.
# These rows were created because some questionnaire entries contain
# no second experimental trial.
emotion_trial_data <- emotion_trial_data %>%
  filter(!is.na(interruption_condition))

dim(emotion_trial_data)

# Check whether each date-child-trial-condition combination
# occurs only once in the reshaped questionnaire dataset.
emotion_trial_data %>%
  count(
    date,
    child_id,
    trial,
    interruption_condition
  ) %>%
  filter(n > 1)

# Display the number of retained questionnaire trials.
emotion_trial_data %>%
  count(trial)

############################################################
# 8 Merge movement and emotion questionnaire
############################################################

behaviour_data <- behaviour_data %>%
  left_join(
    emotion_trial_data,
    by = c(
      "date",
      "child_id",
      "trial",
      "interruption_condition"
    )
  )

# Check whether all movement trials received questionnaire data.
behaviour_data %>%
  summarise(
    missing_accidental = sum(is.na(emotion_accidental)),
    missing_deliberate = sum(is.na(emotion_deliberate)),
    missing_none = sum(is.na(emotion_none))
  )

# Quality check: display trials for which all emotion responses are missing.
behaviour_data %>%
  filter(is.na(emotion_accidental) &
           is.na(emotion_deliberate) &
           is.na(emotion_none)) %>%
  select(
    date,
    child_id,
    trial,
    interruption_condition
  )

############################################################
# Export final behavioural dataset
############################################################

write_csv(
  behaviour_data,
  "data_processed/behaviour_data_final.csv"
)


























