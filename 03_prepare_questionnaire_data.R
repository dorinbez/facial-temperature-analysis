#################################################################
# Bachelor Thesis
# Skript: 03_prepare_questionnaire_data.R
# AUthor: Dorin Bez
# Created: 17.07.2026
# Purpose: Import, clean and prepare friendship and emotion questionnaire data 
#          for subsequent merging and the statistical analysis
# Operator and last date of work: Dorin Bez, 20.07.2026
################################################################

###############################################################
# 1 Load libraries
###############################################################

library(tidyverse)
library(readxl)
library(janitor)

###############################################################
# 2 Define file paths
###############################################################

friendship_file <- "data_raw/Friendship.xlsx"

emotion_file <- "data_raw/Emotions.xlsx"

kids_info_file <- "data_raw/kids_info.xlsx"

###############################################################
# 3 Import friendship questionnaire
###############################################################

# Import the friendship questionnaire.
# Each row contains one child's ratings of a specific partner.
friendship_raw <- read_excel(
  friendship_file
)

# Inspect the imported column names and data structure.
names(friendship_raw)
glimpse(friendship_raw)

# Open the complete dataset for manual inspection.
# View(friendship_raw)

############################################################
# 4 Clean friendship questionnaire
############################################################

# Retain only identifiers and the two questionnaire ratings.
# Notes and unused Excel columns are excluded from the analysis dataset.
friendship_clean <- friendship_raw %>%
  select(
    child_id,
    partner_id,
    friendship_rating,
    liking_rating
  )

# Remove placeholder questionnaire entries without any friendship information.
# Text entries such as "NA" are first converted into genuine missing values.
# Rows without either rating do not represent completed questionnaires
# and are therefore excluded from the cleaned dataset.
friendship_clean <- friendship_raw %>%
  select(
    child_id,
    partner_id,
    friendship_rating,
    liking_rating
  ) %>%
  mutate(
    friendship_rating = na_if(friendship_rating, "NA"),
    liking_rating = na_if(liking_rating, "NA"),
    friendship_rating = as.numeric(friendship_rating),
    liking_rating = as.numeric(liking_rating)
  ) %>%
  filter(
    !(is.na(friendship_rating) & is.na(liking_rating))
  )

glimpse(friendship_clean)

View(friendship_clean)

###############################################################
# 5 Friendship questionnaire coding
###############################################################

# Friendship and liking ratings use the following four-point scale:
# 1 = not at all
# 2 = rather not
# 3 = rather yes
# 4 = definitely

# Ratings are kept as numeric values because they are used in the statistical analyses.

###############################################################
# 6 Check friendship questionnaire
###############################################################

# Check for missing child or partner IDs.
# Expected result: 0 missing IDs.
friendship_clean %>%
  summarise(
    missing_child_id = sum(is.na(child_id)),
    missing_partner_id = sum(is.na(partner_id))
  )

# Check whether all ratings are within the valid response range from 1 to 4.
# Expected result: 0 invalid ratings.
friendship_clean %>%
  summarise(
    invalid_friendship_ratings =
      sum(!friendship_rating %in% 1:4 | is.na(friendship_rating)),
    invalid_liking_ratings =
      sum(!liking_rating %in% 1:4 | is.na(liking_rating))
  )

# Check whether each directed child-partner combination occurs only once.
# Expected result: an empty table with 0 rows.
friendship_clean %>%
  count(child_id, partner_id) %>%
  filter(n > 1)

############################################################
# 7 Import participant information
############################################################

# Import participant-level information.
# Each row represents one included child.
kids_info_raw <- read_excel(
  kids_info_file
)

# Inspect the imported data.
names(kids_info_raw)
glimpse(kids_info_raw)

############################################################
# 8 Clean participant information
############################################################

kids_info_clean <- kids_info_raw %>%
  clean_names() %>%
  transmute(
    child_id = str_trim(as.character(child_id)),
    age = as.numeric(age),
    sex = str_to_lower(str_trim(as.character(sex))),
    nationality = str_trim(as.character(nationality))
  )

kids_info_clean

############################################################
# 9 Check participant information
############################################################

# Expected result: 8 rows and 4 columns.
dim(kids_info_clean)

# Each child ID should occur exactly once.
kids_info_clean %>%
  count(child_id, name = "n") %>%
  filter(n != 1)

# No central participant information should be missing.
kids_info_clean %>%
  summarise(
    missing_child_id = sum(is.na(child_id)),
    missing_age = sum(is.na(age)),
    missing_sex = sum(is.na(sex)),
    missing_nationality = sum(is.na(nationality))
  )

###############################################################
# 10 Import emotion questionnaire
###############################################################

# Import the emotion questionnaire.
# Each row contains one child's emotion responses after the experiment.
emotion_raw <- read_excel(
  emotion_file
)

# Inspect the imported column names and data structure.
names(emotion_raw)
glimpse(emotion_raw)

# Open the complete dataset for manual inspection.
# View(emotion_raw)

###############################################################
# 11 Clean emotion questionnaire
###############################################################

# Retain the questionnaire responses, child identifier, date, 
# trial conditions and notes required for later assignment.
emotion_clean <- emotion_raw %>%
  select(
    Date,
    ID,
    `Q-accidental`,
    `Q-delibarate`,
    `Q-none`,
    trial_1,
    trial_2,
    comments
  ) %>%
  rename(
    date = Date,
    child_id = ID,
    emotion_accidental = `Q-accidental`,
    emotion_deliberate = `Q-delibarate`,
    emotion_none = `Q-none`
  ) %>%
  mutate(
    date = case_when(
      # Convert Excel serial numbers such as 45805.
      str_detect(date, "^\\d{5}$") ~
        as.Date(as.numeric(date), origin = "1899-12-30"),
      
      # Convert dates written as 28.05.25 or 28.05.2025.
      TRUE ~ as.Date(
        date,
        tryFormats = c("%d.%m.%y", "%d.%m.%Y")
      )
    )
  ) %>%
  # Remove empty Excel rows without a child ID.
  filter(!is.na(child_id))

glimpse(emotion_clean)

View(emotion_clean)

###############################################################
# 12 Emotion questionnaire coding
###############################################################

# Emotion responses are stored as categorical text values:
# happy        = positive emotional response
# sad          = sadness
# disappointed = disappointment
# mad          = anger
#
# NA indicates that the question was not asked or that no valid response was available.

# Question meanings:
# emotion_accidental:
# Emotion reported in response to the sticker-box event.
# This question was also asked in recordings without an accidental
# interruption and therefore cannot always be assigned uniquely to an accidental trial.
#
# emotion_deliberate:
# Emotion reported after the partner deliberately left the game.
# This question is relevant primarily for the emotion child in
# deliberate-interruption trials. The removed child was generally not asked this question.
#
# emotion_none:
# Control question concerning the receipt of the stickers.
# Because it was asked more generally, it is not assigned uniquely to a specific no-interruption trial.

############################################################
# 13 Check emotion questionnaire
############################################################

# Check for missing child IDs.
# Expected result: 0 missing child IDs.
emotion_clean %>%
  summarise(
    missing_child_id = sum(is.na(child_id))
  )

# List all emotion values occurring across the three questions.
# Expected non-missing values: happy, sad, disappointed and mad.
emotion_clean %>%
  pivot_longer(
    cols = starts_with("emotion_"),
    names_to = "question",
    values_to = "emotion"
  ) %>%
  count(question, emotion, sort = TRUE)

# Check which experimental conditions are listed for trial 1 and trial 2.
# Expected non-missing trial codes: a, dp1, dp2 and none.
emotion_clean %>%
  summarise(
    trial_1_values = paste(sort(unique(trial_1)), collapse = ", "),
    trial_2_values = paste(sort(unique(trial_2)), collapse = ", ")
  )

# Check whether completely identical questionnaire rows occur more than once.
# Expected result: an empty table with 0 rows.
emotion_clean %>%
  count(
    date,
    child_id,
    emotion_accidental,
    emotion_deliberate,
    emotion_none,
    trial_1,
    trial_2,
    comments
  ) %>%
  filter(n > 1)

# Document missing trial assignments.
# A missing trial_2 is expected for children who contributed only one
# questionnaire-relevant trial. Missing trial_1 values are not expected.
# Expected result:
# missing_trial_1 = 0
# missing_trial_2 = 8
emotion_clean %>%
  summarise(
    missing_trial_1 = sum(is.na(trial_1)),
    missing_trial_2 = sum(is.na(trial_2))
  )

# Display observations without a second trial for manual verification.
# These cases are retained because the absence of trial 2 is expected.
emotion_clean %>%
  filter(is.na(trial_2)) %>%
  select(date, child_id, trial_1, trial_2, comments)

############################################################
# 14 Export cleaned questionnaire datasets
############################################################

# Export the cleaned friendship questionnaire.
# One row represents one child rating one specific partner.
write_csv(
  friendship_clean,
  "data_processed/friendship_clean.csv"
)

# Export the cleaned emotion questionnaire.
# One row represents one child's questionnaire responses for one recording date.
write_csv(
  emotion_clean,
  "data_processed/emotion_clean.csv"
)

write_csv(
  kids_info_clean,
  "data_processed/kids_info_clean.csv"
)

# Check whether both exported files were created successfully.
file.exists("data_processed/friendship_clean.csv")
file.exists("data_processed/emotion_clean.csv")
file.exists("data_processed/kids_info_clean.csv")
