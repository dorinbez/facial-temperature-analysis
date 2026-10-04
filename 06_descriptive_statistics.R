############################################################
# Bachelor Thesis
# Script: 06_ descriptive_statistics.R
# Author: Dorin Bez
# Created: 21.07.2026
# Purpose: Perform descriptive statistical analyses of the final analysis dataset.
# Operator and last date of work: Dorin Bez, 27.07.2026
############################################################

############################################################
# 1 Load libraries
############################################################

library(tidyverse)

############################################################
# 2 Define file paths
############################################################

final_file <- "data_processed/final_analysis_data.csv"
kids_info_file <- "data_processed/kids_info_clean.csv"

############################################################
# 3 Import final dataset
############################################################

final_data <- read_csv(
  final_file,
  show_col_types = FALSE
)

kids_info <- read_csv(
  kids_info_file,
  show_col_types = FALSE
)

############################################################
# 4 Inspect dataset
############################################################

dim(final_data)
glimpse(final_data)
View(final_data)
summary(final_data)

dim(kids_info)
glimpse(kids_info)
View(kids_info)

############################################################
# 5 Describe participant sample
############################################################

# Calculate descriptive statistics for the eight included children.
# Each child is represented once in kids_info, so participant-level
# variables are not counted repeatedly across trials.
participant_descriptives <- kids_info %>%
  summarise(
    # Number of included children.
    n_children = n(),
    
    # Mean, standard deviation, median, and range of age.
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  ) %>%
  mutate(
    # Round all descriptive age statistics to two decimal places.
    across(
      -n_children,
      ~ round(.x, 2)
    )
  )

# Count the number of children in each sex category
# and calculate the corresponding percentage.
sex_counts <- kids_info %>%
  count(
    sex,
    name = "n"
  ) %>%
  mutate(
    percentage = round(
      n / sum(n) * 100,
      2
    )
  )

sex_counts

# Count the number of children in each nationality category.
# Nationality is described only at the participant level.
nationality_counts <- kids_info %>%
  count(
    nationality,
    name = "n"
  ) %>%
  arrange(desc(n))

nationality_counts

# The final sample consisted of eight children aged four to five years
# (M = 4.75, SD = 0.46, median = 5).
# Two children were female (25%) and six were male (75%).

############################################################
# 6 Describe the structure of the final dataset
############################################################

# Summarise the overall structure of the analysis dataset.
# Each row represents one child in one experimental trial.
dataset_structure <- final_data %>%
  summarise(
    n_child_trial_observations = n(),
    n_children = n_distinct(child_id),
    n_recordings = n_distinct(analysis_filename),
    n_dates = n_distinct(date)
  )

dataset_structure

# Expected result:
# 52 child-trial observations from 8 children,
# 25 recordings, and 13 recording dates.

# Count the number of child-trial observations contributed
# by each included child.
observations_per_child <- final_data %>%
  count(
    child_id,
    name = "n_child_trial_observations"
  ) %>%
  arrange(child_id)

observations_per_child
# K008 contributed 10 observations; all other children contributed 6.

# Summarise the distribution of child-trial observations per child.
observations_per_child_summary <- observations_per_child %>%
  summarise(
    mean_observations = mean(n_child_trial_observations),
    sd_observations = sd(n_child_trial_observations),
    median_observations = median(n_child_trial_observations),
    min_observations = min(n_child_trial_observations),
    max_observations = max(n_child_trial_observations)
  ) %>%
  mutate(
    across(
      everything(),
      ~ round(.x, 2)
    )
  )

observations_per_child_summary

# Count child-trial observations by box condition. ???????????????????????????????
box_condition_counts <- final_data %>%
  count(
    box_condition,
    name = "n"
  ) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(box_condition)

box_condition_counts

# Count child-trial observations for the three analysis conditions. ???????????????????????????????
# The original dp1 and dp2 conditions are combined as dp.
interruption_group_counts <- final_data %>%
  count(
    interruption_group,
    name = "n"
  ) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(interruption_group)

interruption_group_counts

# Retain the original distinction between dp1 and dp2
# for data-quality checks and transparent documentation.
original_condition_counts <- final_data %>%
  count(
    interruption_condition,
    name = "n"
  ) %>%
  arrange(interruption_condition)

original_condition_counts

# Number of observations according to the coded child's experimental role.
role_counts <- final_data %>%
  count(
    interruption_condition,
    coded_role,
    name = "n"
  ) %>%
  arrange(interruption_condition, coded_role)

role_counts
# Expected result:
# a and none trials are classified as "both_present".
# dp1 and dp2 trials are classified as "emotion_child".

# Number of observations per child and interruption group
observations_per_child_condition_check <- final_data %>%
  count(
    child_id,
    interruption_group,
    name = "n_observations"
  ) %>%
  tidyr::complete(
    child_id,
    interruption_group,
    fill = list(n_observations = 0)
  ) %>%
  arrange(child_id, interruption_group)

observations_per_child_condition_check

# Wide table showing the distribution of conditions within each child
observations_per_child_condition_wide_check <-
  observations_per_child_condition_check %>%
  tidyr::pivot_wider(
    names_from = interruption_group,
    values_from = n_observations,
    values_fill = 0
  ) %>%
  mutate(
    total = none + a + dp
  ) %>%
  arrange(desc(total), child_id)

observations_per_child_condition_wide_check

# Results:
# The final dataset contained 52 child-trial observations
# from 8 children, based on 25 recordings collected across
# 13 recording dates.
#
# Children contributed between 6 and 10 observations
# (M = 6.50, SD = 1.41). Seven children contributed 6 observations each, 
# whereas one child (K008) contributed 10 observations.
#
# Consequently, the dataset was slightly unbalanced across
# interruption groups. The no interruption and accidental
# interruption groups each contained 18 observations,
# whereas the deliberate interruption group contained
# 16 observations. This imbalance resulted from the
# additional observations contributed by K008.
#
# The dataset contained 26 JA and 26 NC child-trial observations.
#
# At the child-trial level:
# a = 18, dp = 16, none = 18.
#
# The original deliberate-interruption conditions were:
# dp1 = 8 and dp2 = 8.
#


############################################################
# 7 Describe main movement variables by analysis condition
############################################################

# Calculate descriptive statistics for the primary movement outcome
# and the two supplementary high-movement measures.
# The original dp1 and dp2 conditions are combined as dp.

movement_descriptives <- final_data %>%
  group_by(interruption_group) %>%
  summarise(
    n = n(),
    
    # Primary outcome: proportion of the coded trial
    # classified as high-frequency movement.
    mean_high_proportion = mean(high_proportion, na.rm = TRUE),
    sd_high_proportion = sd(high_proportion, na.rm = TRUE),
    median_high_proportion = median(high_proportion, na.rm = TRUE),
    min_high_proportion = min(high_proportion, na.rm = TRUE),
    max_high_proportion = max(high_proportion, na.rm = TRUE),
    
    # Supplementary measure: number of high-movement episodes.
    mean_high_frequency = mean(high_frequency, na.rm = TRUE),
    sd_high_frequency = sd(high_frequency, na.rm = TRUE),
    median_high_frequency = median(high_frequency, na.rm = TRUE),
    min_high_frequency = min(high_frequency, na.rm = TRUE),
    max_high_frequency = max(high_frequency, na.rm = TRUE),
    
    # Supplementary measure: absolute duration of high movement.
    mean_high_duration = mean(high_duration_sec, na.rm = TRUE),
    sd_high_duration = sd(high_duration_sec, na.rm = TRUE),
    median_high_duration = median(high_duration_sec, na.rm = TRUE),
    min_high_duration = min(high_duration_sec, na.rm = TRUE),
    max_high_duration = max(high_duration_sec, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(interruption_group)

movement_descriptives

# Create a rounded version for easier presentation in tables.
# The original unrounded values remain stored in movement_descriptives.
movement_descriptives_rounded <- movement_descriptives %>%
  mutate(
    across(
      -c(interruption_group, n),
      ~ round(.x, 2)
    )
  )

movement_descriptives_rounded

View(movement_descriptives_rounded)

# Results:
# The mean proportion of high-frequency movement was highest
# in the deliberate-interruption condition (dp; M = 0.58, SD = 0.10),
# followed by the accidental-interruption condition
# (a; M = 0.55, SD = 0.14) and the no-interruption condition
# (none; M = 0.52, SD = 0.11).
#
# The mean frequency of high-movement episodes was similar across
# conditions: a = 28.56 (SD = 9.76), dp = 28.25 (SD = 4.93),
# and none = 27.00 (SD = 8.27).
#
# The mean duration of high-frequency movement was highest in the
# deliberate-interruption condition (dp; M = 205.23 s, SD = 42.83),
# compared with the no-interruption condition
# (none; M = 188.07 s, SD = 60.42) and the accidental-interruption
# condition (a; M = 184.24 s, SD = 49.79).
#
# These results are descriptive and do not indicate whether
# differences between conditions are statistically significant.


############################################################
# 8 Describe temperature variables by interruption condition
############################################################

# Calculate descriptive statistics for the temperature changes
# separately for each interruption condition.
temperature_descriptives <- final_data %>%
  group_by(interruption_group) %>%
  summarise(
    # Number of child-trial observations per condition.
    n = n(),
    
    # Mean and standard deviation of the maximum temperature change.
    mean_delta_max = mean(delta_max_temp, na.rm = TRUE),
    sd_delta_max = sd(delta_max_temp, na.rm = TRUE),
    
    # Mean and standard deviation of the minimum temperature change.
    mean_delta_min = mean(delta_min_temp, na.rm = TRUE),
    sd_delta_min = sd(delta_min_temp, na.rm = TRUE),
    
    # Mean and standard deviation of the average temperature change.
    mean_delta_av = mean(delta_av_temp, na.rm = TRUE),
    sd_delta_av = sd(delta_av_temp, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(interruption_group)

temperature_descriptives

# Create a rounded version for easier presentation in tables.
# The original values remain stored in temperature_descriptives.
temperature_descriptives_rounded <- temperature_descriptives %>%
  mutate(
    across(
      -c(interruption_group, n),
      ~ round(.x, 2)
    )
  )

temperature_descriptives_rounded

# Results:
# Mean temperature changes were negative in all three conditions,
# except for the maximum temperature change in the deliberate-
# interruption condition, which was close to zero.
#
# In the accidental-interruption condition, the mean changes were
# -0.20 °C for maximum temperature, -0.33 °C for minimum temperature,
# and -0.28 °C for average temperature.
#
# In the deliberate-interruption condition, the mean changes were
# 0.02 °C for maximum temperature, -0.14 °C for minimum temperature,
# and -0.16 °C for average temperature.
#
# In the no-interruption condition, the mean changes were
# -0.56 °C for maximum temperature, -0.65 °C for minimum temperature,
# and -0.69 °C for average temperature.
#
# These results are descriptive and do not indicate whether
# differences between conditions are statistically significant.


############################################################
# 9 Describe condition-specific emotion responses
############################################################

# Select the emotion response corresponding to the experimental
# condition of each child-trial observation.
# The original dp1 and dp2 conditions use the deliberate-emotion item.

final_data <- final_data %>%
  mutate(
    emotion_response = case_when(
      interruption_group == "a" ~ emotion_accidental,
      interruption_group == "dp" ~ emotion_deliberate,
      interruption_group == "none" ~ emotion_none,
      TRUE ~ NA_character_
    )
  )

# Count the relevant emotion responses within each analysis condition.
# Missing responses are retained in this table.

emotion_response_counts <- final_data %>%
  count(
    interruption_group,
    emotion_response,
    name = "n"
  ) %>%
  arrange(interruption_group, emotion_response)

emotion_response_counts

# Inspect child-trial observations with a missing
# condition-specific emotion response.
missing_emotion_responses <- final_data %>%
  filter(is.na(emotion_response)) %>%
  select(
    analysis_filename,
    child_id,
    partner_id,
    trial,
    interruption_group,
    interruption_condition,
    coded_role,
    emotion_child,
    removed_child,
    emotion_response
  )

missing_emotion_responses

# Calculate percentages using only valid emotion responses.
# Missing responses are excluded from the denominator.
emotion_response_percentages <- final_data %>%
  filter(!is.na(emotion_response)) %>%
  count(
    interruption_group,
    emotion_response,
    name = "n"
  ) %>%
  group_by(interruption_group) %>%
  mutate(
    valid_n = sum(n),
    percentage = round(n / valid_n * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(interruption_group, emotion_response)

emotion_response_percentages

# Results:
# In the accidental-interruption condition, disappointment was the
# most frequent response (38.9%), followed by happiness and sadness
# (22.2% each) and anger (16.7%).
#
# In the deliberate-interruption condition, happiness was the most
# frequent response (42.9%), followed by disappointment (35.7%),
# sadness (14.3%), and anger (7.1%).
#
# In the no-interruption condition, all children responded with
# happiness (100%).
#
# Two emotion responses were missing in the deliberate-interruption
# condition. Percentages were therefore calculated using the
# 14 valid responses in this condition.


############################################################
# 10 Describe friendship and liking ratings
############################################################

# Remove repeated questionnaire values originating from multiple
# child-trial observations of the same child-partner combination.
# Descriptive statistics are then calculated separately for each
# analysis condition.
friendship_liking_descriptives <- final_data %>%
  select(
    interruption_group,
    child_id,
    partner_id,
    friendship_rating,
    liking_rating
  ) %>%
  distinct() %>%
  group_by(interruption_group) %>%
  summarise(
    n_total = n(),
    
    n_friendship_valid = sum(!is.na(friendship_rating)),
    mean_friendship = mean(friendship_rating, na.rm = TRUE),
    sd_friendship = sd(friendship_rating, na.rm = TRUE),
    
    n_liking_valid = sum(!is.na(liking_rating)),
    mean_liking = mean(liking_rating, na.rm = TRUE),
    sd_liking = sd(liking_rating, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(interruption_group)

friendship_liking_descriptives

# Create a rounded version for presentation.
# The valid sample sizes remain unrounded.
friendship_liking_descriptives_rounded <-
  friendship_liking_descriptives %>%
  mutate(
    across(
      -c(
        interruption_group,
        n_total,
        n_friendship_valid,
        n_liking_valid
      ),
      ~ round(.x, 2)
    )
  )

friendship_liking_descriptives_rounded

# Results:
# After removing repeated questionnaire values across child-trial
# observations, nine distinct child-partner combinations were
# represented in each analysis condition.
#
# Valid friendship and liking ratings were available for eight
# combinations in the accidental-interruption condition, seven in
# the deliberate-interruption condition, and eight in the
# no-interruption condition.
#
# Mean friendship ratings were identical across conditions
# (M = 3.00). Mean liking ratings were also highly similar:
# a = 3.62 (SD = 1.06), dp = 3.57 (SD = 1.13),
# and none = 3.62 (SD = 1.06).
#
# Missing questionnaire values were retained as genuine missing
# values and were excluded only from calculations involving the
# respective rating.


############################################################
# 11 Overall descriptives for friendship and liking
############################################################

# Remove repeated questionnaire values originating from multiple
# child-trial observations of the same child-partner combination.
# The resulting statistics therefore describe the distinct
# questionnaire ratings rather than repeated trial-level copies.
friendship_liking_overall <- final_data %>%
  select(
    child_id,
    partner_id,
    friendship_rating,
    liking_rating
  ) %>%
  distinct() %>%
  summarise(
    n_total = n(),
    
    # Friendship ratings
    n_friendship_valid = sum(!is.na(friendship_rating)),
    mean_friendship = mean(friendship_rating, na.rm = TRUE),
    median_friendship = median(friendship_rating, na.rm = TRUE),
    sd_friendship = sd(friendship_rating, na.rm = TRUE),
    min_friendship = min(friendship_rating, na.rm = TRUE),
    max_friendship = max(friendship_rating, na.rm = TRUE),
    
    # Liking ratings
    n_liking_valid = sum(!is.na(liking_rating)),
    mean_liking = mean(liking_rating, na.rm = TRUE),
    median_liking = median(liking_rating, na.rm = TRUE),
    sd_liking = sd(liking_rating, na.rm = TRUE),
    min_liking = min(liking_rating, na.rm = TRUE),
    max_liking = max(liking_rating, na.rm = TRUE)
  ) %>%
  mutate(
    # Round descriptive values, but retain sample sizes as integers.
    across(
      -c(
        n_total,
        n_friendship_valid,
        n_liking_valid
      ),
      ~ round(.x, 2)
    )
  )

friendship_liking_overall
View(friendship_liking_overall)

# Results:
# Nine distinct child-partner combinations were represented.
# Valid friendship and liking ratings were available for eight
# combinations; one combination had missing questionnaire data.
#
# The overall friendship rating had a mean of 3.00
# (Median = 3.00, SD = 1.07, range = 1–4).
#
# The overall liking rating had a mean of 3.62
# (Median = 4.00, SD = 1.06, range = 1–4).
#
# Missing ratings were retained as genuine missing values and were
# excluded only from calculations involving the respective variable.


############################################################
# 12 Correlation between friendship and liking ratings
############################################################

# Remove repeated questionnaire values originating from multiple
# child-trial observations of the same child-partner combination.
# The correlation is calculated using only complete and distinct
# questionnaire pairs.
friendship_liking_correlation <- final_data %>%
  select(
    child_id,
    partner_id,
    friendship_rating,
    liking_rating
  ) %>%
  distinct() %>%
  filter(
    !is.na(friendship_rating),
    !is.na(liking_rating)
  ) %>%
  summarise(
    n_complete = n(),
    correlation = cor(
      friendship_rating,
      liking_rating,
      method = "spearman"
    )
  ) %>%
  mutate(
    correlation = round(correlation, 2)
  )

friendship_liking_correlation

# Results:
# Eight distinct child-partner combinations had complete friendship
# and liking ratings.
#
# Friendship and liking showed a moderate positive Spearman
# correlation (rho = 0.61, n = 8).
# The two ratings were therefore related, but not identical.


############################################################
# 13 Inspect distributions of continuous study variables
############################################################

# Select the primary and supplementary continuous study variables.
# The original dp1 and dp2 conditions are combined in
# interruption_group as the deliberate-interruption condition.
distribution_data <- final_data %>%
  select(
    interruption_group,
    high_proportion,
    high_frequency,
    high_duration_sec,
    delta_max_temp,
    delta_min_temp,
    delta_av_temp
  ) %>%
  pivot_longer(
    cols = -interruption_group,
    names_to = "variable",
    values_to = "value"
  )

# Display the overall distribution of each continuous variable.
# Each variable is shown on its own scale because the measures
# use different units.
ggplot(
  distribution_data,
  aes(x = value)
) +
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "grey"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free"
  ) +
  labs(
    title = "Distributions of continuous study variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

# Results:
# The movement variables showed broadly continuous distributions.
# High-movement duration was approximately symmetric, whereas
# high-movement frequency and proportion showed some larger values
# at the upper end of their distributions.
#
# The temperature-change variables were centred close to zero but
# contained several comparatively large negative and positive values.
# These observations are inspected individually in the subsequent
# extreme-value check and are not removed automatically.


############################################################
# 14 Boxplots by interruption condition
############################################################

# Compare the distributions of the continuous study variables
# across the three analysis conditions: accidental interruption,
# deliberate interruption, and no interruption.
ggplot(
  distribution_data,
  aes(
    x = interruption_group,
    y = value
  )
) +
  geom_boxplot() +
  facet_wrap(
    ~ variable,
    scales = "free_y"
  ) +
  labs(
    title = "Continuous variables by interruption condition",
    x = "Interruption condition",
    y = "Value"
  ) +
  theme_minimal()

# Results:
# The distributions of the continuous study variables overlapped
# substantially across the three interruption conditions.
#
# The deliberate-interruption condition showed a slightly higher
# median high-movement proportion, whereas the temperature-change
# variables were generally centred close to zero in all conditions.
#
# Several observations were displayed as potential outliers,
# particularly for the temperature-change variables. These values
# are inspected individually in the following section and are not
# removed solely because they appear outside the boxplot whiskers.


############################################################
# 15 Inspect potentially extreme values
############################################################

# Convert the selected continuous variables to long format while
# retaining the information required to identify each observation.
extreme_value_data <- final_data %>%
  select(
    analysis_filename,
    child_id,
    partner_id,
    trial,
    interruption_group,
    interruption_condition,
    coded_role,
    high_proportion,
    high_frequency,
    high_duration_sec,
    delta_max_temp,
    delta_min_temp,
    delta_av_temp
  ) %>%
  pivot_longer(
    cols = c(
      high_proportion,
      high_frequency,
      high_duration_sec,
      delta_max_temp,
      delta_min_temp,
      delta_av_temp
    ),
    names_to = "variable",
    values_to = "value"
  )

# Calculate the lower and upper boxplot thresholds for each variable.
# Potentially extreme values are defined using the 1.5 × IQR rule.
extreme_value_limits <- extreme_value_data %>%
  group_by(variable) %>%
  summarise(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    lower_limit = q1 - 1.5 * iqr,
    upper_limit = q3 + 1.5 * iqr,
    .groups = "drop"
  )

extreme_value_limits

# Identify observations outside the variable-specific IQR limits.
extreme_values <- extreme_value_data %>%
  left_join(
    extreme_value_limits,
    by = "variable"
  ) %>%
  filter(
    !is.na(value),
    value < lower_limit | value > upper_limit
  ) %>%
  mutate(
    direction = case_when(
      value < lower_limit ~ "below lower limit",
      value > upper_limit ~ "above upper limit"
    )
  ) %>%
  arrange(
    variable,
    value
  )

extreme_values

# Count the number of potentially extreme values per variable.
extreme_value_counts <- extreme_values %>%
  count(
    variable,
    direction,
    name = "n"
  ) %>%
  arrange(variable, direction)

extreme_value_counts

# Potentially extreme observations were identified using the
# 1.5 × IQR rule. These observations are retained unless inspection
# identifies a data-entry, coding, or measurement error.


# Count the number of unique child-trial observations containing
# at least one potentially extreme value.
extreme_observation_count <- extreme_values %>%
  distinct(
    analysis_filename,
    child_id,
    trial
  ) %>%
  summarise(
    n_extreme_observations = n()
  )

extreme_observation_count


# Summarise the potentially extreme variables for each affected
# child-trial observation.
extreme_values_per_observation <- extreme_values %>%
  group_by(
    analysis_filename,
    child_id,
    partner_id,
    trial,
    interruption_group
  ) %>%
  summarise(
    n_extreme_values = n(),
    affected_variables = paste(variable, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_extreme_values),
    child_id
  )

extreme_values_per_observation

# Results:
# The IQR-based screening identified 15 potentially extreme
# variable-level values across 8 unique child-trial observations.
#
# Most flagged values concerned the temperature-change variables.
# Two high-movement frequency values exceeded the corresponding
# upper IQR limit.
#
# No potentially extreme values were identified for high-movement
# duration or high-movement proportion.
#
# Potentially extreme values were flagged for individual inspection
# and were not excluded automatically.

# All temperature-change variables were recalculated for the
# eight child-trial observations containing potentially extreme values.
# The stored delta values were identical to the differences between
# posttest and pretest temperatures in all cases.
# No calculation errors were identified.
# Therefore, the flagged observations were retained for further analysis.

############################################################
# 16 Summarise missing values
############################################################

# Count missing values for the variables relevant to the analyses.
missing_values_summary <- final_data %>%
  summarise(
    n_observations = n(),
    
    missing_high_frequency = sum(is.na(high_frequency)),
    missing_high_duration = sum(is.na(high_duration_sec)),
    missing_high_proportion = sum(is.na(high_proportion)),
    
    missing_delta_max_temp = sum(is.na(delta_max_temp)),
    missing_delta_min_temp = sum(is.na(delta_min_temp)),
    missing_delta_av_temp = sum(is.na(delta_av_temp)),
    
    missing_emotion_response = sum(is.na(emotion_response)),
    
    missing_friendship_rating = sum(is.na(friendship_rating)),
    missing_liking_rating = sum(is.na(liking_rating))
  )

missing_values_summary %>%
  print(width = Inf)

# Results:
# No values were missing for the movement or temperature-change variables.
# Two condition-specific emotion responses were missing.
# Friendship and liking ratings were each missing in six child-trial observations.
# These six rows represent one unique child-partner combination without
# questionnaire data.
# Missing values were retained and excluded only from analyses requiring
# the respective variable.

############################################################
# 17 Inspect sampling effort and balance
############################################################

# Count the number of child-trial observations contributed
# by each child in each analysis condition.
observations_per_child_condition <- final_data %>%
  count(
    child_id,
    interruption_group,
    name = "n_observations"
  ) %>%
  complete(
    child_id,
    interruption_group = c("a", "dp", "none"),
    fill = list(n_observations = 0)
  ) %>%
  arrange(child_id, interruption_group)

observations_per_child_condition

# Display the number of observations per child and condition
# in a wide format for easier comparison.
observations_per_child_condition_wide <-
  observations_per_child_condition %>%
  pivot_wider(
    names_from = interruption_group,
    values_from = n_observations
  )

observations_per_child_condition_wide

# Results:
# Seven of the eight children contributed two observations to each
# interruption condition.
# Child K008 contributed additional observations because this child
# participated as a replacement for another child.
# Therefore, K008 contributed four observations to the accidental-interruption
# condition, two observations to the deliberate-interruption condition,
# and four observations to the no-interruption condition.
# The resulting imbalance was expected and did not indicate duplicate data.


############################################################
# 18 Summarise box condition
############################################################

# Count observations for each box condition within interruption groups.
box_condition_summary <- final_data %>%
  count(interruption_group, box_condition)

box_condition_summary

box_condition_summary_wide <- box_condition_summary %>%
  pivot_wider(
    names_from = box_condition,
    values_from = n,
    values_fill = 0
  )

box_condition_summary_wide

# Results:
# The box condition was perfectly balanced across interruption groups.
# The accidental-interruption and no-interruption conditions each contained
# nine JA and nine NC observations.
# The deliberate-interruption condition contained eight JA and eight NC
# observations.
# Therefore, no imbalance between box conditions was present.


############################################################
# 19 Correlations among continuous variables
############################################################

continuous_variables <- final_data %>%
  select(
    high_proportion,
    delta_max_temp,
    delta_min_temp,
    delta_av_temp,
    friendship_rating,
    liking_rating
  )

correlation_matrix <- cor(
  continuous_variables,
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(correlation_matrix, 2)

# Results:
# The three temperature-change variables were highly correlated
# (r = 0.97–0.98), indicating that they captured very similar information.
# Friendship and liking ratings were also strongly positively correlated
# (r = 0.76).
# High-movement proportion showed only weak correlations with the remaining
# variables (|r| ≤ 0.18).
# Therefore, no strong association between movement behaviour and temperature
# change was observed.


############################################################
# 20 Pairwise scatterplots
############################################################

# install.packages("GGally")

library(GGally)

pairplot_data <- final_data %>%
  select(
    high_proportion,
    delta_max_temp,
    delta_min_temp,
    delta_av_temp,
    friendship_rating,
    liking_rating
  )

GGally::ggpairs(pairplot_data)

# Results:
# Pairwise scatterplots confirmed the correlation analysis.
# The three temperature-change variables showed nearly identical linear
# relationships.
# Friendship and liking ratings displayed a moderate to strong positive
# association.
# High-movement proportion showed no clear linear relationship with
# temperature-change or social-rating variables.
# No additional unusual observations or non-linear patterns were identified.


############################################################
# 21 Assess the need for transformations
############################################################

# Histograms and Q-Q plots were inspected previously.
# No variable showed strong skewness or severe deviations from normality.
# Therefore, no transformation (e.g., log or square-root transformation)
# was considered necessary before further analyses.

library(e1071)

continuous_variables %>%
  summarise(across(everything(), skewness, na.rm = TRUE))

# Results:
# Visual inspection of histograms, Q-Q plots, pairwise scatterplots,
# and skewness statistics indicated no substantial deviations from
# normality or severe skewness for the continuous variables.
# Although the temperature-change variables showed slight negative
# skewness, the deviations were considered minor and did not justify
# data transformation.
# Therefore, no variable transformation was applied prior to modelling.































############################################################
#### Table 1: Descriptive statistics
############################################################

#-----------------------------------------------------------
# Create descriptive statistics
#-----------------------------------------------------------

descriptive_table_raw <- final_data %>%
  dplyr::group_by(
    box_condition,
    interruption_group
  ) %>%
  dplyr::summarise(
    
    n = dplyr::n(),
    
    max_mean = mean(
      delta_max_temp,
      na.rm = TRUE
    ),
    
    max_sd = sd(
      delta_max_temp,
      na.rm = TRUE
    ),
    
    min_mean = mean(
      delta_min_temp,
      na.rm = TRUE
    ),
    
    min_sd = sd(
      delta_min_temp,
      na.rm = TRUE
    ),
    
    av_mean = mean(
      delta_av_temp,
      na.rm = TRUE
    ),
    
    av_sd = sd(
      delta_av_temp,
      na.rm = TRUE
    ),
    
    movement_mean = mean(
      high_proportion,
      na.rm = TRUE
    ),
    
    movement_sd = sd(
      high_proportion,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


#-----------------------------------------------------------
# Set condition order and readable labels
#-----------------------------------------------------------

descriptive_table <- descriptive_table_raw %>%
  dplyr::mutate(
    
    box_condition = factor(
      box_condition,
      levels = c(
        "NC",
        "JA"
      ),
      labels = c(
        "Non-Collaborative",
        "Joint Action"
      )
    ),
    
    interruption_group = factor(
      interruption_group,
      levels = c(
        "none",
        "a",
        "dp"
      ),
      labels = c(
        "No interruption",
        "Accidental interruption",
        "Deliberate interruption"
      )
    )
  ) %>%
  
  dplyr::arrange(
    box_condition,
    interruption_group
  ) %>%
  
  dplyr::mutate(
    
    Maximum_temperature =
      sprintf(
        "%.2f (%.2f)",
        max_mean,
        max_sd
      ),
    
    Minimum_temperature =
      sprintf(
        "%.2f (%.2f)",
        min_mean,
        min_sd
      ),
    
    Average_temperature =
      sprintf(
        "%.2f (%.2f)",
        av_mean,
        av_sd
      ),
    
    High_movement =
      sprintf(
        "%.2f (%.2f)",
        movement_mean,
        movement_sd
      )
  ) %>%
  
  dplyr::select(
    Box_condition = box_condition,
    Interruption_condition = interruption_group,
    n,
    Maximum_temperature,
    Minimum_temperature,
    Average_temperature,
    High_movement
  )


# Inspect table data
descriptive_table

# install.packages(c("flextable", "officer"))
library(flextable)
library(officer)


############################################################
#### Format Table 1 with flextable
############################################################

# Create display version with separate group-heading rows
table1_display <- dplyr::bind_rows(
  
  tibble::tibble(
    Interruption_condition = "Non-Collaborative",
    n = "",
    Maximum_temperature = "",
    Minimum_temperature = "",
    Average_temperature = "",
    High_movement = ""
  ),
  
  descriptive_table %>%
    dplyr::filter(Box_condition == "Non-Collaborative") %>%
    dplyr::transmute(
      Interruption_condition,
      n = as.character(n),
      Maximum_temperature,
      Minimum_temperature,
      Average_temperature,
      High_movement
    ),
  
  tibble::tibble(
    Interruption_condition = "Joint Action",
    n = "",
    Maximum_temperature = "",
    Minimum_temperature = "",
    Average_temperature = "",
    High_movement = ""
  ),
  
  descriptive_table %>%
    dplyr::filter(Box_condition == "Joint Action") %>%
    dplyr::transmute(
      Interruption_condition,
      n = as.character(n),
      Maximum_temperature,
      Minimum_temperature,
      Average_temperature,
      High_movement
    )
)


############################################################
# Create flextable
############################################################

table_descriptive <- flextable::flextable(
  table1_display
)

# Column labels
table_descriptive <- flextable::set_header_labels(
  table_descriptive,
  Interruption_condition = "Interruption condition",
  n = "n",
  Maximum_temperature = "Δ maximum temperature (°C)",
  Minimum_temperature = "Δ minimum temperature (°C)",
  Average_temperature = "Δ average temperature (°C)",
  High_movement = "High movement proportion"
)


############################################################
# Basic formatting
############################################################

# Remove all default borders
table_descriptive <- flextable::border_remove(
  table_descriptive
)

# Font
table_descriptive <- flextable::font(
  table_descriptive,
  fontname = "Arial",
  part = "all"
)

table_descriptive <- flextable::fontsize(
  table_descriptive,
  size = 14,
  part = "all"
)

# Bold column headers
table_descriptive <- flextable::bold(
  table_descriptive,
  part = "header"
)

# Bold group-heading rows
table_descriptive <- flextable::bold(
  table_descriptive,
  i = c(1, 5),
  part = "body"
)

table_descriptive <- flextable::padding(
  table_descriptive,
  i = c(1, 5),
  padding.top = 6,
  padding.bottom = 4,
  part = "body"
)

# Align text
table_descriptive <- flextable::align(
  table_descriptive,
  j = "Interruption_condition",
  align = "left",
  part = "all"
)

table_descriptive <- flextable::align(
  table_descriptive,
  j = c(
    "n",
    "Maximum_temperature",
    "Minimum_temperature",
    "Average_temperature",
    "High_movement"
  ),
  align = "center",
  part = "all"
)


############################################################
# Borders
############################################################

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

# Top line
table_descriptive <- flextable::hline_top(
  table_descriptive,
  border = outer_border,
  part = "header"
)

# Line below header
table_descriptive <- flextable::hline_bottom(
  table_descriptive,
  border = outer_border,
  part = "header"
)

# Separator before Joint Action
table_descriptive <- flextable::border(
  table_descriptive,
  i = 5,
  border.top = inner_border,
  part = "body"
)

# Bottom line
table_descriptive <- flextable::hline_bottom(
  table_descriptive,
  border = outer_border,
  part = "body"
)


############################################################
# Column widths and spacing
############################################################

table_descriptive <- flextable::width(
  table_descriptive,
  j = "Interruption_condition",
  width = 2.15
)

table_descriptive <- flextable::width(
  table_descriptive,
  j = "n",
  width = 0.35
)

table_descriptive <- flextable::width(
  table_descriptive,
  j = c(
    "Maximum_temperature",
    "Minimum_temperature",
    "Average_temperature"
  ),
  width = 1.65
)

table_descriptive <- flextable::width(
  table_descriptive,
  j = "High_movement",
  width = 1.55
)

# Slightly compact row spacing
table_descriptive <- flextable::padding(
  table_descriptive,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

# Extra spacing for group headings
table_descriptive <- flextable::padding(
  table_descriptive,
  i = c(1, 5),
  padding.top = 7,
  padding.bottom = 5,
  part = "body"
)

# Prevent rows from splitting across pages
table_descriptive <- flextable::keep_with_next(
  table_descriptive,
  i = 1:nrow(table1_display),
  value = FALSE
)


############################################################
# Display
############################################################

table_descriptive


#-----------------------------------------------------------
# Export Table 1 as high-resolution PNG
#-----------------------------------------------------------

if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2")
}

dir.create("tables", showWarnings = FALSE)

flextable::save_as_image(
  x = table_descriptive,
  path = "tables/Table_1_descriptive_statistics.png",
  zoom = 3,
  expand = 10
)





