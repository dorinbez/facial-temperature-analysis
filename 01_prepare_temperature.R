#################################################################
# Bachelor Thesis
# Skript: 01_prepare_temperature.R
# AUthor: Dorin Bez
# Created: 10.07.2026
# Purpose: Import, clean and agreegate FLIR temperature data
# Operator and last date of work: Dorin Bez, 13.07.2026
################################################################

# install packages
#install.packages(c(
#  "tidyverse",
#  "readxl",
#  "janitor",
#  "lme4",
#  "lmerTest"
#))

#####################################################
# 1 load packages
#####################################################

library(tidyverse)
library(readxl)
library(janitor)
library(lme4)
library(lmerTest)

####################################################
# 2 import data
###################################################

flir_raw <- read_excel(
  "data_raw/Flir_thermal_coding.xlsx"
  )

###################################################
# 3 inspect data
###################################################

glimpse(flir_raw)
names(flir_raw)
View(flir_raw)
summary(flir_raw)

###################################################
# 4 clean data
###################################################

flir_clean <- flir_raw %>%
  mutate(
    # remove the file extension
    filename = str_remove(
      filename_thermalvideo, 
      "\\.csq$"
      ),
    # add missing box condition information (NC) to four filenames
    filename = case_when(
      filename == "K006_sess3_dp_late_17.04.25_pretest" ~
        "K006_sess3_dp_late_NC_17.04.25_pretest",
      filename == "K006_sess3_dp_late_17.04.25_posttest" ~
        "K006_sess3_dp_late_NC_17.04.25_posttest",
      filename == "K009_sess2_dp_late_17.04.25_pretest" ~
        "K009_sess2_dp_late_NC_17.04.25_pretest",
      filename == "K009_sess2_dp_late_17.04.25_posttest" ~
        "K009_sess2_dp_late_NC_17.04.25_posttest",
      TRUE ~ filename
    ),
    # Replace multiple underscores with a single underscore
    filename = str_replace_all(
      filename,
      "_+",
      "_"
      )
  )

###################################################
# 5 check filename structure
##################################################

flir_clean %>%
  mutate(
    n_parts = str_count(filename, "_") + 1
    ) %>%
  count(n_parts)

#################################################
# 6 Split filename into metadata
#################################################

flir_clean <- flir_clean %>%
  mutate(
    n_parts = str_count(filename, "_") + 1
  ) %>%
  separate(
    filename,
    into = c(
      "child_id",
      "session",
      "interruption_condition",
      "part4",
      "part5",
      "part6",
      "part7"
    ),
    sep = "_",
    fill = "right",
    remove = FALSE
  ) %>%
  mutate(
    # Timing is only available for filenames with 7 parts
    timing = if_else(n_parts == 7, part4, NA_character_),
    
    # Box condition is located at different positions depending on filename structure
    box_condition = if_else(n_parts == 7, part5, part4),
    
    # Extract recording date
    date = if_else(n_parts == 7, part6, part5),
    
    # Extract experimental phase
    phase_file = if_else(n_parts == 7, part7, part6)
  )

##################################################
# 8 Standardize interruption condition labels
##################################################

# convert all interruption condition labels to lowercase
flir_clean <- flir_clean %>%
  mutate(
    interruption_condition = str_to_lower(
      interruption_condition
    )
  )

###################################################
# 9 Check extracted metadata
###################################################

flir_clean %>%
  select(
    filename,
    child_id,
    session,
    interruption_condition,
    timing,
    box_condition,
    date,
    phase_file
  ) %>%
  distinct() %>%
  View()

#################################################
# 10 Remove temporary variables
#################################################

flir_clean <- flir_clean %>%
  select(
    -part4,
    -part5,
    -part6,
    -part7,
    -n_parts
  )

glimpse(flir_clean)

##################################################
# 11 Remove unused variables
#################################################

flir_clean <- flir_clean %>%
  select(
    -any_of(c(
    "filename_thermalvideo",
    "id_datapoint",
    "timepoints(sec)",
    "actual_video_time (±5 sec)",
    "comments",
    "coder",
    "date_coded",
    "...15",
    "...1"
  ))
)

####################################################
# 12 Remove redundant phase variable
####################################################

# the original 'phase' variable was identical to 'phase_file'
flir_clean <- flir_clean %>%
  select(-phase) %>%
  rename(
    phase = phase_file
  )

##################################################
# 13 Inspect cleaned FLIR data
#################################################

glimpse(flir_clean)

# Check missing values per variable 
colSums(is.na(flir_clean))

###################################################
# 14 Verify the number of measurements per thermal recording
#################################################

# Each pretest and posttest recording should contain 12 measurements
flir_clean %>%
  count(filename, phase) %>%
  filter(n != 12)

##################################################
# 15 Aggregate temperature measurements per thermal recording
##################################################

temperature_phase_summary <- flir_clean %>%
  # group all 12 temperature measurements belonging to one recording 
  # and calculate the mean maximum, minimum and average temperature
  # for each pretest and posttest recording separately
  group_by(
    child_id,
    session,
    interruption_condition,
    timing,
    box_condition,
    date,
    phase
  ) %>%
  summarise(
    mean_max_temp = mean(max_temp_NT, na.rm = TRUE),
    mean_min_temp = mean(min_temp_NT, na.rm = TRUE),
    mean_av_temp  = mean(av_temp_NT, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(temperature_phase_summary)
View(temperature_phase_summary)

####################################################
# 16 Reshape pretest and posttest values into separate columns
####################################################

temperature_summary <- temperature_phase_summary %>%
  # Convert the dataset from long to wide format 
  # so that pretest and posttest values are stored in separate columns
  # for each participant and session
  pivot_wider(
    names_from = phase,
    values_from = c(
      mean_max_temp,
      mean_min_temp,
      mean_av_temp
    )
  )

####################################################
# 17 Calculate temperature change scores
####################################################

temperature_summary <- temperature_summary %>%
  # Calculate temperature changes by subtracting
  # pretest values from posttest values
  # Positive values indicate an increase in temperature
  # Negative values indicate a decrease in temperature
  mutate(
    # Change in maximum temperature
    delta_max_temp = mean_max_temp_posttest - mean_max_temp_pretest,
    # Change in minimum temperature
    delta_min_temp = mean_min_temp_posttest - mean_min_temp_pretest,
    # Change in average temperature
    delta_av_temp  = mean_av_temp_posttest  - mean_av_temp_pretest
  )

glimpse(temperature_summary)
View(temperature_summary)

##################################################
# 18 Check final temperature dataset
#################################################

# Check the number of rows and variables
dim(temperature_summary)

# Check missing values
colSums(is.na(temperature_summary))

# Check whether each participant-session-condition combination occurs only once
temperature_summary %>%
  count(child_id, session, interruption_condition) %>%
  filter(n != 1)

# Inspect temperature change scores
summary(
  temperature_summary %>%
    select(delta_max_temp, delta_min_temp, delta_av_temp)
)

###############################################################
# 19 Export processed temperature dataset
###############################################################

write_csv(
  temperature_summary,
  "data_processed/temperature_data_final.csv"
)

file.exists("data_processed/temperature_data_final.csv")





