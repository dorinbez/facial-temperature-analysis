############################################################
# Bachelor Thesis
# Script: 05_merge_temperature_data.R
# Author: Dorin Bez
# Created: 17.07.2026
# Purpose: Import the prepared temperature dataset and merge it
#          with the prepared behavioural dataset for the final analysis.
# Operator and last date of work: Dorin Bez, 21.07.2026
############################################################


############################################################
# 1 Load libraries
############################################################

library(tidyverse)

############################################################
# 2 Define file paths
############################################################

behaviour_file <- "data_processed/behaviour_data_final.csv"

temperature_file <- "data_processed/temperature_data_final.csv"


############################################################
# 3 Import prepared datasets
############################################################

# Import the prepared behavioural dataset.
behaviour_data <- read_csv(
  behaviour_file,
  show_col_types = FALSE
)

# Import the prepared temperature dataset.
temperature_data <- read_csv(
  temperature_file,
  show_col_types = FALSE
)

############################################################
# 4 Inspect imported datasets
############################################################

# Inspect the behavioural dataset.
dim(behaviour_data)
glimpse(behaviour_data)
View(behaviour_data)

# Inspect the temperature dataset.
dim(temperature_data)
glimpse(temperature_data)
View(temperature_data)

############################################################
# 5 Check merge-key uniqueness
############################################################

# Check whether the proposed merge variables uniquely identify
# each row in the behavioural dataset.
behaviour_data %>%
  count(
    child_id,
    date,
    interruption_condition,
    timing,
    box_condition
  ) %>%
  filter(n > 1)

# Check whether the proposed merge variables uniquely identify
# each row in the temperature dataset.
temperature_data %>%
  count(
    child_id,
    date,
    interruption_condition,
    timing,
    box_condition
  ) %>%
  filter(n > 1)

############################################################
# 6 Convert temperature date variable
############################################################

temperature_data <- temperature_data %>%
  mutate(
    date = as.Date(
      date,
      tryFormats = c("%d.%m.%y", "%d.%m.%Y")
    )
  )

############################################################
# 7 Standardise interruption conditions for merging
############################################################

# The temperature dataset does not distinguish between dp1 and dp2.
# Therefore, both conditions are grouped as "dp" for the merge.

behaviour_data <- behaviour_data %>%
  mutate(
    interruption_group = case_when(
      interruption_condition %in% c("dp1", "dp2") ~ "dp",
      TRUE ~ interruption_condition
    )
  )

temperature_data <- temperature_data %>%
  mutate(
    interruption_group = interruption_condition
  )

############################################################
# 8 Merge behavioural and temperature datasets
############################################################

final_data <- behaviour_data %>%
  left_join(
    temperature_data %>%
      select(-interruption_condition),
    by = c(
      "child_id",
      "date",
      "interruption_group",
      "timing",
      "box_condition"
    )
  )

############################################################
# 9 Inspect merged dataset
############################################################

dim(behaviour_data)
dim(final_data)

############################################################
# 10 Final plausibility checks
############################################################

# Verify that the merge did not change the number of behavioural rows.
dim(behaviour_data)
dim(final_data)

# Check for duplicated behavioural trial rows after the merge.
final_data %>%
  count(
    child_id,
    date,
    trial,
    interruption_condition
  ) %>%
  filter(n > 1)

# Count missing values in the central variables.
final_data %>%
  summarise(
    missing_partner_id = sum(is.na(partner_id)),
    missing_friendship_rating = sum(is.na(friendship_rating)),
    missing_liking_rating = sum(is.na(liking_rating)),
    missing_emotion_accidental = sum(is.na(emotion_accidental)),
    missing_emotion_deliberate = sum(is.na(emotion_deliberate)),
    missing_emotion_none = sum(is.na(emotion_none)),
    missing_delta_max = sum(is.na(delta_max_temp)),
    missing_delta_min = sum(is.na(delta_min_temp)),
    missing_delta_av = sum(is.na(delta_av_temp))
  )
###########################################################
# 11 Remove varaibles not required for the final analysis
###########################################################

# These variables were retained during data preparation and merging, 
# but are excluded from the final analysis dataset.
final_data <- final_data %>%
  select(
    -timing,
    -session,
    -comments,
  )

############################################################
# 12 Export final analysis dataset
############################################################

# Export the final merged dataset for statistical analysis.
write_csv(
  final_data,
  "data_processed/final_analysis_data.csv"
)

# Verify that the file was created successfully.
file.exists("data_processed/final_analysis_data.csv")

