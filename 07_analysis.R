############################################################
# Bachelor Thesis
# Script: 07_analysis.R
# Author: Dorin Bez
# Created: 27.07.2026
# Purpose: Statistical analysis of movement and temperature responses
# Operator and last date of work: Dorin Bez, 10.08.2026
############################################################

############################################################
# 1 Load packages
############################################################

library(tidyverse)
library(janitor)
library(lme4)
library(lmerTest)
library(performance)
library(emmeans)
library(DHARMa)


############################################################
# 2 Import final analysis dataset
############################################################

final_data <- read_csv(
  "data_processed/final_analysis_data.csv",
  show_col_types = FALSE
)

############################################################
# 3 Prepare variables for statistical analysis
############################################################

# Convert categorical variables to factors.
# Set reference levels for the main experimental conditions.
analysis_data <- final_data %>%
  mutate(
    emotion_response = case_when(
      interruption_group == "a" ~ emotion_accidental,
      interruption_group == "dp" ~ emotion_deliberate,
      interruption_group == "none" ~ emotion_none,
      TRUE ~ NA_character_
    ),
    
    # Main experimental factors
    interruption_group = factor(
      interruption_group,
      levels = c("none", "a", "dp")
    ),
    
    box_condition = factor(
      box_condition,
      levels = c("NC", "JA")
    ),
    
    # Additional categorical variables
    interruption_condition = factor(interruption_condition),
    child_id = factor(child_id),
    partner_id = factor(partner_id),
    analysis_filename = factor(analysis_filename),
    coded_role = factor(coded_role),
    emotion_response = factor(emotion_response)
  )

#############################################################
# 4 Create session and trial identifiers
#############################################################

# Create recording sessions based on recording dates.
# For each child, unique recording dates are ordered
# chronologically and assigned consecutive session IDs.
# All observations from the same child and recording date
# belong to the same recording session.
analysis_data <- analysis_data %>%
  arrange(child_id, date) %>%
  group_by(child_id) %>%
  mutate(
    session_id = dense_rank(date)
  ) %>%
  ungroup()

# Create a unique session identifier for each child.
analysis_data <- analysis_data %>%
  mutate(
    child_session_id = paste0(child_id, "_S", session_id)
  )

analysis_data %>%
  select(
    child_id,
    date,
    session_id,
    child_session_id
  ) %>%
  arrange(child_id, date)

# Create a consecutive trial identifier for each child.
# Trials are numbered chronologically across all recording
# sessions within each child.
analysis_data <- analysis_data %>%
  arrange(child_id, date, trial) %>%
  group_by(child_id) %>%
  mutate(
    child_trial_id = row_number()
  ) %>%
  ungroup()

analysis_data %>%
  select(
    child_id,
    date,
    session_id,
    child_session_id,
    trial,
    child_trial_id
  ) %>%
  arrange(child_id, child_trial_id)

# Verify that sessions and consecutive trial identifiers
# were created correctly.
analysis_data %>%
  arrange(child_id, date, trial) %>%
  select(
    child_id,
    date,
    session_id,
    trial,
    child_trial_id
  )

##############################################################
# 5 Create dyad identifiers
##############################################################

# Create a unique identifier for each child pair.
# Child IDs are sorted alphabetically so that both members
# of a dyad receive the same identifier regardless of the
# order in which child_id and partner_id are stored.
analysis_data <- analysis_data %>%
  rowwise() %>%
  mutate(
    dyad_id = paste(sort(c(as.character(child_id),
                           as.character(partner_id))),
                    collapse = "_")
  ) %>%
  ungroup()

# Verify that dyad identifiers were created correctly.
analysis_data %>%
  select(child_id, partner_id, dyad_id) %>%
  distinct() %>%
  arrange(dyad_id)

############################################################
# 6 Inspect analysis dataset
############################################################

# Display the structure of the analysis dataset.
glimpse(analysis_data)

# Display the dimensions of the dataset.
dim(analysis_data)

# Display summary statistics for all variables.
summary(analysis_data)

# Count observations per interruption group.
analysis_data %>%
  count(interruption_group)

# Count observations per child and interruption group.
analysis_data %>%
  count(child_id, interruption_group)



# Set reference categories for categorical predictors
analysis_data <- analysis_data %>%
  mutate(
    interruption_group = relevel(factor(interruption_group), ref = "none"),
    box_condition = relevel(factor(box_condition), ref = "NC"),
    emotion_response = relevel(factor(emotion_response), ref = "happy")
  )



############################################################
### Create consecutive trial identifier within each dyad
############################################################

# Create one row for each unique dyad-date-trial combination
dyad_trial_lookup <- analysis_data %>%
  distinct(
    dyad_id,
    date,
    trial
  ) %>%
  arrange(
    dyad_id,
    date,
    trial
  ) %>%
  group_by(dyad_id) %>%
  mutate(
    dyad_trial_id = row_number()
  ) %>%
  ungroup()


# Add the consecutive dyad trial ID to the full dataset
analysis_data <- analysis_data %>%
  left_join(
    dyad_trial_lookup,
    by = c("dyad_id", "date", "trial")
  )


# Create unique identifier for each consecutive trial within each dyad
analysis_data <- analysis_data %>%
  mutate(
    dyad_trial_re_id = interaction(
      dyad_id,
      dyad_trial_id,
      drop = TRUE
    )
  )


analysis_data %>%
  select(
    child_id,
    partner_id,
    dyad_id,
    date,
    session_id,
    child_trial_id,
    trial,
    dyad_trial_id,
    dyad_trial_re_id
  ) %>%
  arrange(
    dyad_id,
    date,
    trial,
    child_id
  )






















###########################################################
# 7 Statistical analyses
##########################################################

##########################################################
## 7.1 Fit random-effect structure
#########################################################

# Compare alternative random-effects structures before
# fitting the final linear mixed-effects models.

# Model 1: 
# Random intercept for child
model_random_child <- lmer(
  delta_av_temp ~ 1 +
    (1 | child_id),
  data = analysis_data,
  REML = TRUE
)

summary(model_random_child)

performance::check_singularity(model_random_child)
# Result:
# The model converged successfully and showed no singular fit.
# Therefore, child_id was considered an appropriate random intercept.


# Model 2:
# Additional random intercept for dyad. 
model_random_child_dyad <- lmer(
  delta_av_temp ~ 1 +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = TRUE
)

summary(model_random_child_dyad)

performance::check_singularity(model_random_child_dyad)

# Compare Model 1 and Model 2. 
anova(model_random_child, model_random_child_dyad)

plot(DHARMa::simulateResiduals(model_random_child))
plot(DHARMa::simulateResiduals(model_random_child_dyad))

# Result:
# The model converged successfully and showed no singular fit.
# However, adding dyad_id did not significantly improve model fit
# according to the likelihood-ratio test.
# Therefore, subsequent analyses retain child_id as the
# only random intercept.


# Model 3: 
# Additional random intercept for recoding session.
model_random_child_session <- lmer(
  delta_av_temp ~ 1 +
    (1 | child_id) +
    (1 | child_session_id),
  data = analysis_data,
  REML = TRUE
)
plot(DHARMa::simulateResiduals(model_random_child_session))

summary(model_random_child_session)

performance::check_singularity(model_random_child_session)
testResiduals(model_random_child_session)
# Result:
# The model resulted in a singular fit, indicating that
# child_session_id did not explain additional variance.
# Therefore, this random effect was not retained.

# Model 4:
# Additional random intercept for partner ID.
model_random_partner <- lmer(
  delta_av_temp ~
    1 +
    (1 | child_id) +
    (1 | partner_id),
  data = analysis_data,
  REML = TRUE
)

anova(
  model_random_child,
  model_random_partner
)
plot(DHARMa::simulateResiduals(model_random_partner))
summary(model_random_partner)
# Result:
# Adding partner_id as an additional random effect did not
# significantly improve model fit (likelihood-ratio test,
# p = 0.152). Therefore, partner_id was not retained.






# Model 5:
# Additional random intercept for the shared consecutive dyad trial
model_random_child_dyad_trial <- lmer(
  delta_av_temp ~ 1 +
    (1 | child_id) +
    (1 | dyad_trial_re_id),
  data = analysis_data,
  REML = TRUE
)

# Inspect model
summary(model_random_child_dyad_trial)

# Check singularity
performance::check_singularity(
  model_random_child_dyad_trial
)

# Inspect variance components
lme4::VarCorr(
  model_random_child_dyad_trial
)

# Residual diagnostics
plot(
  DHARMa::simulateResiduals(
    model_random_child_dyad_trial
  )
)

# Compare with child-only random-intercept model
anova(
  model_random_child,
  model_random_child_dyad_trial
)

# Compare AIC
AIC(
  model_random_child,
  model_random_child_dyad_trial
)

# Result:
# The model resulted in a singular fit.
# The estimated variance of the dyad_trial_re_id random intercept was zero.
# Adding the shared consecutive dyad-trial random intercept did not improve
# model fit compared with the child-only random-intercept model
# (LRT chi-square = 0, df = 1, p = 1.000).
# The AIC also increased by approximately 2 points.
# Therefore, dyad_trial_re_id was not retained as an additional random effect.



# Conclusion:
# Four alternative random-effects structures were evaluated.
# Models including child_id + partner_id and child_id + dyad_id
# showed comparable model fit (ΔAIC < 2) and similar residual diagnostics.
# However, the model including child_id and dyad_id was selected because
# it was not singular, both random effects explained non-zero variance,
# and conditional R² could be estimated.
# Therefore, child_id and dyad_id were retained as random intercepts
# in all subsequent analyses.










############################################################
## Supplementary Table:
## Evaluation of alternative random-effects structures
############################################################

library(dplyr)
library(tibble)
library(lme4)
library(gt)


############################################################
# Helper function:
# Extract variance of a specific added random effect
############################################################

get_random_variance <- function(model, grouping_factor) {
  
  vc <- as.data.frame(
    lme4::VarCorr(model)
  )
  
  variance <- vc %>%
    dplyr::filter(
      grp == grouping_factor,
      is.na(var2)
    ) %>%
    dplyr::pull(vcov)
  
  if (length(variance) == 0) {
    return(NA_real_)
  }
  
  variance[1]
}


############################################################
# Helper function:
# Compare alternative model with child-only reference model
############################################################

extract_random_comparison <- function(
    model,
    structure_name,
    added_group,
    decision
) {
  
  # Models have identical fixed-effects structures,
  # therefore comparison is performed on the REML-fitted models
  comparison <- anova(
    model_random_child,
    model,
    refit = FALSE
  )
  
  tibble(
    Random_effects_structure = structure_name,
    
    AIC = AIC(model),
    
    Delta_AIC =
      AIC(model) -
      AIC(model_random_child),
    
    Added_variance =
      get_random_variance(
        model,
        added_group
      ),
    
    Chi_square =
      comparison$Chisq[2],
    
    df =
      comparison$Df[2],
    
    p_value =
      comparison$`Pr(>Chisq)`[2],
    
    Singular =
      ifelse(
        lme4::isSingular(model),
        "Yes",
        "No"
      ),
    
    Decision = decision
  )
}


############################################################
# Child-only reference model
############################################################

random_effect_reference <- tibble(
  
  Random_effects_structure =
    "Child",
  
  AIC =
    AIC(model_random_child),
  
  Delta_AIC =
    0,
  
  Added_variance =
    NA_real_,
  
  Chi_square =
    NA_real_,
  
  df =
    NA_real_,
  
  p_value =
    NA_real_,
  
  Singular =
    ifelse(
      lme4::isSingular(model_random_child),
      "Yes",
      "No"
    ),
  
  Decision =
    "Reference"
)


############################################################
# Alternative random-effects structures
############################################################

random_effect_results <- bind_rows(
  
  random_effect_reference,
  
  extract_random_comparison(
    model_random_child_dyad,
    "Child + dyad",
    "dyad_id",
    "Retained"
  ),
  
  extract_random_comparison(
    model_random_child_session,
    "Child + session",
    "child_session_id",
    "Not retained"
  ),
  
  extract_random_comparison(
    model_random_partner,
    "Child + partner",
    "partner_id",
    "Not retained"
  ),
  
  extract_random_comparison(
    model_random_child_dyad_trial,
    "Child + dyad-trial",
    "dyad_trial_re_id",
    "Not retained"
  )
)


############################################################
# Format table values
############################################################

random_effect_table_data <- random_effect_results %>%
  mutate(
    
    AIC = round(AIC, 2),
    Delta_AIC = round(Delta_AIC, 2),
    
    Added_variance_display = case_when(
      is.na(Added_variance) ~ "—",
      TRUE ~ sprintf("%.3f", Added_variance)
    ),
    
    Chi_square_display = case_when(
      is.na(Chi_square) ~ "—",
      TRUE ~ sprintf("%.2f", Chi_square)
    ),
    
    df_display = case_when(
      is.na(df) ~ "—",
      TRUE ~ as.character(df)
    ),
    
    p_display = case_when(
      is.na(p_value) ~ "—",
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )

############################################################
# Inspect raw table before formatting
############################################################

random_effect_table_data


############################################################
# Create formatted flextable
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

random_effect_display <- random_effect_table_data |>
  dplyr::select(
    Random_effects_structure,
    AIC,
    Delta_AIC,
    Added_variance_display,
    Chi_square_display,
    df_display,
    p_display,
    Singular,
    Decision
  ) |>
  dplyr::mutate(
    AIC = sprintf("%.2f", AIC),
    Delta_AIC = sprintf("%.2f", Delta_AIC)
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_random_effects <- flextable::flextable(
  random_effect_display
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_random_effects <- flextable::set_header_labels(
  table_random_effects,
  Random_effects_structure = "Random-effects structure",
  AIC = "AIC",
  Delta_AIC = "ΔAIC",
  Added_variance_display = "Added-effect variance",
  Chi_square_display = "χ²",
  df_display = "df",
  p_display = "p",
  Singular = "Singular",
  Decision = "Decision"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_random_effects <- flextable::border_remove(
  table_random_effects
)

table_random_effects <- flextable::font(
  table_random_effects,
  fontname = "Arial",
  part = "all"
)

table_random_effects <- flextable::fontsize(
  table_random_effects,
  size = 14,
  part = "all"
)

table_random_effects <- flextable::bold(
  table_random_effects,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Text entries left-aligned
table_random_effects <- flextable::align(
  table_random_effects,
  j = "Random_effects_structure",
  align = "left",
  part = "body"
)

table_random_effects <- flextable::align(
  table_random_effects,
  j = "Decision",
  align = "center",
  part = "body"
)

# Numerical/categorical result columns centered
table_random_effects <- flextable::align(
  table_random_effects,
  j = c(
    "AIC",
    "Delta_AIC",
    "Added_variance_display",
    "Chi_square_display",
    "df_display",
    "p_display",
    "Singular"
  ),
  align = "center",
  part = "body"
)

# All headers centered
table_random_effects <- flextable::align(
  table_random_effects,
  align = "center",
  part = "header"
)

table_random_effects <- flextable::valign(
  table_random_effects,
  valign = "center",
  part = "body"
)


#-----------------------------------------------------------
# Borders
#-----------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

# Top line
table_random_effects <- flextable::hline_top(
  table_random_effects,
  border = outer_border,
  part = "header"
)

# Line below header
table_random_effects <- flextable::hline_bottom(
  table_random_effects,
  border = outer_border,
  part = "header"
)

# Bottom line
table_random_effects <- flextable::hline_bottom(
  table_random_effects,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Random_effects_structure",
  width = 2.35
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "AIC",
  width = 0.90
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Delta_AIC",
  width = 0.90
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Added_variance_display",
  width = 1.80
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Chi_square_display",
  width = 0.70
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "df_display",
  width = 0.55
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "p_display",
  width = 0.80
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Singular",
  width = 0.95
)

table_random_effects <- flextable::width(
  table_random_effects,
  j = "Decision",
  width = 1.35
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_random_effects <- flextable::padding(
  table_random_effects,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_random_effects <- flextable::line_spacing(
  table_random_effects,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_random_effects


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

flextable::save_as_image(
  x = table_random_effects,
  path = "tables/Table_S10_random_effects.png",
  zoom = 3,
  expand = 10
)























#########################################################
## 7.2 Average temperature (Δ average temperature)
##########################################################

#########################################################
##### 7.2.1 model buliding 
##########################################################

# Set happy as the reference category for emotion response
analysis_data$emotion_response <- relevel(
  factor(analysis_data$emotion_response),
  ref = "happy"
)

# Main effect model
model_main_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Interaction model
model_interaction_av <- lmer(
  delta_av_temp ~
    interruption_group * box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_main_av, model_interaction_av)

drop1(model_interaction_av, test = "Chisq")

# Result:
# Adding the interaction between interruption_group and
# box_condition did not significantly improve model fit
# (likelihood-ratio test, p = .862).
# Therefore, the simpler main-effects model was retained
# for all subsequent analyses.


###############################################################
#### 7.2.2 Model diagnostics
###############################################################

# Simulate scaled residuals for model diagnostics.
simulation_output <- DHARMa::simulateResiduals(
  fittedModel = model_main_av,  
  n = 1000
)

plot(simulation_output)

# Test for uniformity of residuals.
DHARMa::testUniformity(simulation_output)

# Test for over-/underdispersion.
DHARMa::testDispersion(simulation_output)

# Test for outliers.
DHARMa::testOutliers(simulation_output)

# Results:
# DHARMa diagnostics indicated no evidence of model violations.
# Residuals were approximately uniformly distributed,
# no over- or underdispersion was detected,
# and no significant outliers were identified.
# Therefore, the model assumptions were considered satisfied.
summary(model_main_av)
#### summary(model_interaction_av)

#### anova(model_interaction_av, type = 3)
anova(model_main_av, type = 3)

###############################################################
#### 7.2.3 Multicollinearity
###############################################################

# Evaluate multicollinearity for the interaction model
#### performance::check_collinearity(model_interaction_av)

# Evaluate multicollinearity for the final model
performance::check_collinearity(model_main_av)

# Results:
# The interaction model showed increased VIF values for the
# interaction terms, which is expected because interaction
# terms are inherently correlated with their corresponding
# main effects.
#
# In contrast, the final main-effects model showed low
# multicollinearity (all VIF values < 5), indicating that
# predictor estimates were not substantially affected by
# collinearity.


###############################################################
#### 7.2.4 Explained variance
###############################################################

# Calculate marginal and conditional R² for the final main-effects model.
performance::r2(model_main_av)

# Results:
# The final main-effects model explained approximately 28% of the
# variance through the fixed effects alone (marginal R² = 0.275)
# and approximately 56% of the total variance when including the
# random effects (conditional R² = 0.555).


# Calculate marginal and conditional R² for the interaction model.
# performance::r2(model_interaction_av)

# Results:
# The interaction model explained virtually the same amount of
# variance as the main-effects model (marginal R² = 0.278,
# conditional R² = 0.556).
# Thus, including the interaction did not meaningfully improve
# the explanatory power of the model.

############################################################
#### 7.2.5 save average temperature models
###########################################################

saveRDS(model_main_av, "output/model_main_av.rds")
saveRDS(model_interaction_av, "output/model_interaction_av.rds")






#########################################################
## 7.3 Minimum temperature (Δ minimum temperature)
##########################################################

#########################################################
##### 7.3.1 model buliding 
##########################################################

# Main effect model
model_main_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Interaction model
model_interaction_min <- lmer(
  delta_min_temp ~
    interruption_group * box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_main_min, model_interaction_min)

drop1(model_interaction_min, test = "Chisq")

# Results:
# Adding the interaction between interruption_group and
# box_condition did not significantly improve model fit
# (likelihood-ratio test: χ² = 0.47, p = .790).
# The Type III test likewise indicated that the interaction
# was not significant (p = .788).
# Therefore, the simpler main-effects model was retained
# for all subsequent analyses.


###############################################################
#### 7.3.2 Model diagnostics
###############################################################

# Simulate scaled residuals for model diagnostics.
simulation_output <- DHARMa::simulateResiduals(
  fittedModel = model_main_min,  
  n = 1000
)

plot(simulation_output)

# Test for uniformity of residuals.
DHARMa::testUniformity(simulation_output)

# Test for over-/underdispersion.
DHARMa::testDispersion(simulation_output)

# Test for outliers.
DHARMa::testOutliers(simulation_output)

# Results:
# DHARMa diagnostics indicated no evidence of model violations.
# Residuals were approximately uniformly distributed.
# No over- or underdispersion was detected.
# Although one observation appeared as a potential outlier,
# the outlier test was not significant (p = .086).
# Therefore, the model assumptions were considered satisfied.

summary(model_main_min)
#### summary(model_interaction_av)

#### anova(model_interaction_av, type = 3)
anova(model_main_min, type = 3)

# Results:
# No fixed effect reached statistical significance.
# A trend was observed for box_condition
# (Type III ANOVA: p = .076),
# whereas interruption_group, high_proportion,
# friendship_rating, and emotion_response
# showed no evidence of an association with
# minimum facial temperature.

###############################################################
#### 7.3.3 Multicollinearity
###############################################################

# Evaluate multicollinearity for the interaction model
# performance::check_collinearity(model_interaction_min)

# Evaluate multicollinearity for the final model
performance::check_collinearity(model_main_min)

# Results:
# The final main-effects model showed low multicollinearity
# (all VIF values < 5), indicating that predictor estimates
# were not substantially affected by collinearity.


###############################################################
#### 7.3.4 Explained variance
###############################################################

# Calculate marginal and conditional R² for the final main-effects model.
performance::r2(model_main_min)

# Results:
# The final main-effects model explained approximately 25% of the
# variance through the fixed effects alone (marginal R² = 0.252)
# and approximately 51% of the total variance when including the
# random effects (conditional R² = 0.514).


# Calculate marginal and conditional R² for the interaction model.
# performance::r2(model_interaction_min)


############################################################
#### 7.3.5 save minimum temperature models
###########################################################

saveRDS(model_main_min, "output/model_main_min.rds")
saveRDS(model_interaction_min, "output/model_interaction_min.rds")






#########################################################
## 7.4 Maximum temperature (Δ maximum temperature)
##########################################################

#########################################################
##### 7.4.1 model buliding 
##########################################################

# Main effect model
model_main_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Interaction model
model_interaction_max <- lmer(
  delta_max_temp ~
    interruption_group * box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_main_max, model_interaction_max)

drop1(model_interaction_max, test = "Chisq")

# Results:
# Adding the interaction between interruption_group and
# box_condition did not significantly improve model fit
# (likelihood-ratio test, χ² = 0.097, p = .953).
# The Type III test likewise indicated that the interaction
# was not significant (p = .951).
# Therefore, the simpler main-effects model was retained
# for all subsequent analyses.


###############################################################
#### 7.4.2 Model diagnostics
###############################################################

# Simulate scaled residuals for model diagnostics.
simulation_output <- DHARMa::simulateResiduals(
  fittedModel = model_main_max,  
  n = 1000
)

plot(simulation_output)

# Test for uniformity of residuals.
DHARMa::testUniformity(simulation_output)

# Test for over-/underdispersion.
DHARMa::testDispersion(simulation_output)

# Test for outliers.
DHARMa::testOutliers(simulation_output)

# Results:
# DHARMa diagnostics indicated no evidence of model violations.
# Residuals were approximately uniformly distributed.
# No over- or underdispersion was detected.
# Although one observation appeared as a potential outlier,
# the outlier test was not significant (p = .086).
# Therefore, the model assumptions were considered satisfied.

summary(model_main_max)
#### summary(model_interaction_max)

#### anova(model_interaction_max, type = 3)
anova(model_main_max, type = 3)

# Results:
# No fixed effect reached statistical significance.
# A trend was observed for box_condition
# (Type III ANOVA: p = .060),
# whereas interruption_group, high_proportion,
# friendship_rating, and emotion_response
# showed no evidence of an association with
# maximum facial temperature.


###############################################################
#### 7.4.3 Multicollinearity
###############################################################

# Evaluate multicollinearity for the interaction model
# performance::check_collinearity(model_interaction_max)

# Evaluate multicollinearity for the final model
performance::check_collinearity(model_main_max)

# Results:
# The final main-effects model showed low multicollinearity
# (all VIF values < 5), indicating that predictor estimates
# were not substantially affected by collinearity.


###############################################################
#### 7.4.4 Explained variance
###############################################################

# Calculate marginal and conditional R² for the final main-effects model.
performance::r2(model_main_max)

# Results:
# The final main-effects model explained approximately 24% of the
# variance through the fixed effects alone (marginal R² = 0.235)
# and approximately 39% of the total variance when including the
# random effects (conditional R² = 0.387).


# Calculate marginal and conditional R² for the interaction model.
# performance::r2(model_interaction_max)


############################################################
#### 7.4.5 save maximum temperature models
###########################################################

saveRDS(model_main_max, "output/model_main_max.rds")
saveRDS(model_interaction_max, "output/model_interaction_max.rds")














##########################################################
# 7.5 Model reduction
#########################################################

###############################################################
#### 7.5.1 Exploratory model reduction – Maximum temperature
###############################################################

# Use identical complete-case data for all model comparisons
data_reduction_max <- analysis_data %>%
  tidyr::drop_na(
    delta_max_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    emotion_response,
    child_id,
    dyad_id
  )

nrow(data_reduction_max)

# Set the most frequent emotion category as reference
data_reduction_max$emotion_response <- relevel(
  factor(data_reduction_max$emotion_response),
  ref = "happy"
)

# Full model used as starting point for exploratory reduction
model_full_reduction_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_max,
  REML = FALSE
)

summary(model_full_reduction_max)
performance::check_singularity(model_full_reduction_max)

# Examine which fixed effect contributes least to model fit
drop1(
  model_full_reduction_max,
  test = "Chisq"
)

###############################################################
# Reduced model 1
# Remove interruption_group
###############################################################

# interruption_group showed the smallest contribution to model fit
# in the full model and was therefore removed first.

model_reduced1_max <- update(
  model_full_reduction_max,
  . ~ . - interruption_group
)

# Test whether removing interruption_group reduces model fit
anova(
  model_reduced1_max,
  model_full_reduction_max
)

# Compare AIC
AIC(
  model_reduced1_max,
  model_full_reduction_max
)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced1_max,
  test = "Chisq"
)

# Step 1:
# Removing interruption_group did not significantly reduce model fit
# (LRT p = .854) and decreased AIC from 160.03 to 156.34.
# Therefore, interruption_group was removed.
#
# After removing interruption_group, friendship_rating showed the
# smallest contribution to model fit and was tested for removal next.


###############################################################
# Reduced model 2
# Remove friendship_rating
###############################################################

model_reduced2_max <- update(
  model_reduced1_max,
  . ~ . - friendship_rating
)

# Test whether removing friendship_rating reduces model fit
anova(
  model_reduced2_max,
  model_reduced1_max
)

# Compare AIC
AIC(
  model_reduced2_max,
  model_reduced1_max
)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced2_max,
  test = "Chisq"
)

# Step 2:
# Removing friendship_rating did not significantly reduce model fit
# (LRT p = .678) and decreased AIC from 156.34 to 154.52.
# Therefore, friendship_rating was removed.
#
# After removing friendship_rating, high_proportion showed the
# smallest contribution to model fit and was tested for removal next.


###############################################################
# Reduced model 3
# Remove high_proportion
###############################################################

model_reduced3_max <- update(
  model_reduced2_max,
  . ~ . - high_proportion
)

# Test whether removing high_proportion reduces model fit
anova(
  model_reduced3_max,
  model_reduced2_max
)

# Compare AIC
AIC(
  model_reduced3_max,
  model_reduced2_max
)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced3_max,
  test = "Chisq"
)

# Check singularity
performance::check_singularity(model_reduced3_max)

# Step 3:
# Removing high_proportion did not significantly reduce model fit
# (LRT p = .361) and decreased AIC from 154.52 to 153.35.
# Therefore, high_proportion was removed.


###############################################################
# Reduced model 4
# Remove emotion_response
###############################################################

model_reduced4_max <- update(
  model_reduced3_max,
  . ~ . - emotion_response
)

# Test whether removing emotion_response reduces model fit
anova(
  model_reduced4_max,
  model_reduced3_max
)

# Compare AIC
AIC(
  model_reduced4_max,
  model_reduced3_max
)

# Examine remaining fixed effects
drop1(
  model_reduced4_max,
  test = "Chisq"
)

# Check singularity
performance::check_singularity(model_reduced4_max)

# Step 4:
# Removing emotion_response did not clearly support further model reduction.
# Model fit slightly worsened (LRT p = .087; AIC increased from 153.35 to 153.91)
# and the reduced model resulted in a singular fit.
# Therefore, emotion_response was retained and model_reduced3_max
# was selected as the final reduced model.


###############################################################
####  final Reduced Model Max
###############################################################

# Final reduced model
summary(model_reduced3_max)
performance::check_singularity(model_reduced3_max)


###############################################################
### Residual diagnostics - Final reduced model MAX
###############################################################

# Simulation-based residual diagnostics
set.seed(123)

simulation_reduced_max <- DHARMa::simulateResiduals(
  fittedModel = model_reduced3_max,
  n = 1000
)

plot(simulation_reduced_max)

# Test residual uniformity
DHARMa::testUniformity(simulation_reduced_max)

# Test dispersion
DHARMa::testDispersion(simulation_reduced_max)

# Test outliers
DHARMa::testOutliers(simulation_reduced_max)

# Residual diagnostics did not indicate relevant model violations.
# Uniformity (p = .525), dispersion (p = .742), and outlier tests (p = .086)
# were non-significant, and no systematic residual pattern was detected.


# Store final reduced model
model_reduced_max <- model_reduced3_max


###############################################################
### Compare full and final reduced model
###############################################################

# Refit full model using the identical complete-case dataset
model_full_reduction_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_max,
  REML = FALSE
)

# Compare full and final reduced model
anova(
  model_reduced_max,
  model_full_reduction_max
)

# Compare AIC
AIC(
  model_reduced_max,
  model_full_reduction_max
)

# Check singularity of the comparison full model
performance::check_singularity(model_full_reduction_max)

# Comparison of the full and final reduced model:
# The full model did not provide a significantly better fit than the reduced model
# (LRT: chi-square(4) = 1.32, p = .858).
# The reduced model also had a substantially lower AIC (153.35 vs. 160.03).
# Therefore, the more parsimonious reduced model was retained.


###############################################################
### Save reduced maximum temperature model
###############################################################

saveRDS(
  model_reduced_max,
  "output/model_reduced_max.rds"
)






###############################################################
### 7.5.2 Model reduction - Average temperature
###############################################################

# Create identical complete-case dataset for the entire reduction procedure
data_reduction_av <- analysis_data %>%
  tidyr::drop_na(
    delta_av_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    emotion_response,
    child_id,
    dyad_id
  )

# Set happy as reference category for emotion response
data_reduction_av$emotion_response <- relevel(
  data_reduction_av$emotion_response,
  ref = "happy"
)

# Check number of observations
nrow(data_reduction_av)


###############################################################
# Full model used as starting point for exploratory reduction
###############################################################

model_full_reduction_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_av,
  REML = FALSE
)

summary(model_full_reduction_av)

# Check singularity
performance::check_singularity(model_full_reduction_av)

# Examine which fixed effect contributes least to model fit
drop1(
  model_full_reduction_av,
  test = "Chisq"
)

###############################################################
# Reduced model 1 - Remove friendship_rating
###############################################################

model_reduced1_av <- update(
  model_full_reduction_av,
  . ~ . - friendship_rating
)

# Test whether removing friendship_rating decreases model fit
anova(
  model_reduced1_av,
  model_full_reduction_av
)

# Compare AIC
AIC(
  model_reduced1_av,
  model_full_reduction_av
)

# Check singularity
performance::check_singularity(model_reduced1_av)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced1_av,
  test = "Chisq"
)

# Step 1:
# Removing friendship_rating did not significantly reduce model fit
# (LRT p = .481), while AIC decreased from 166.96 to 165.46.
# The reduced model remained non-singular.
# Therefore, friendship_rating was removed from the model.
#
# Among the remaining predictors, interruption_group contributed least
# to model fit (drop1 p = .256) and was selected for the next reduction step.


###############################################################
# Reduced model 2 - Remove interruption_group
###############################################################

model_reduced2_av <- update(
  model_reduced1_av,
  . ~ . - interruption_group
)

# Test whether removing interruption_group decreases model fit
anova(
  model_reduced2_av,
  model_reduced1_av
)

# Compare AIC
AIC(
  model_reduced2_av,
  model_reduced1_av
)

# Check singularity
performance::check_singularity(model_reduced2_av)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced2_av,
  test = "Chisq"
)

# Step 2:
# Removing interruption_group did not significantly reduce model fit
# (LRT p = .370), while AIC decreased from 165.46 to 163.45.
# The reduced model remained non-singular.
# Therefore, interruption_group was removed from the model.
#
# Among the remaining predictors, high_proportion contributed least
# to model fit (drop1 p = .317) and was selected for the next reduction step.


###############################################################
# Reduced model 3 - Remove high_proportion
###############################################################

model_reduced3_av <- update(
  model_reduced2_av,
  . ~ . - high_proportion
)

# Test whether removing high_proportion decreases model fit
anova(
  model_reduced3_av,
  model_reduced2_av
)

# Compare AIC
AIC(
  model_reduced3_av,
  model_reduced2_av
)

# Check singularity
performance::check_singularity(model_reduced3_av)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced3_av,
  test = "Chisq"
)

# Step 3:
# Removing high_proportion did not significantly reduce model fit
# (LRT p = .331), while AIC decreased from 163.45 to 162.39.
# The reduced model remained non-singular.
# Therefore, high_proportion was removed from the model.
#
# Among the remaining predictors, box_condition contributed least
# to model fit (drop1 p = .150) and was selected for the next reduction step.


###############################################################
# Reduced model 4 - Remove box_condition
###############################################################

model_reduced4_av <- update(
  model_reduced3_av,
  . ~ . - box_condition
)

# Test whether removing box_condition decreases model fit
anova(
  model_reduced4_av,
  model_reduced3_av
)

# Compare AIC
AIC(
  model_reduced4_av,
  model_reduced3_av
)

# Check singularity
performance::check_singularity(model_reduced4_av)

# Examine remaining fixed effects
drop1(
  model_reduced4_av,
  test = "Chisq"
)

# Step 4:
# Removing box_condition did not significantly reduce model fit
# (LRT p = .145).
# AIC increased only minimally from 162.39 to 162.51,
# indicating essentially comparable model fit.
# The reduced model remained non-singular.
# Therefore, box_condition was removed in favour of the more parsimonious model.
#
# emotion_response was the only remaining fixed effect
# and showed a borderline contribution to model fit (drop1 p = .058).
# Its removal was therefore tested in one final reduction step.


###############################################################
# Reduced model 5 - Remove emotion_response
###############################################################

model_reduced5_av <- update(
  model_reduced4_av,
  . ~ . - emotion_response
)

# Test whether removing emotion_response decreases model fit
anova(
  model_reduced5_av,
  model_reduced4_av
)

# Compare AIC
AIC(
  model_reduced5_av,
  model_reduced4_av
)

# Check singularity
performance::check_singularity(model_reduced5_av)

summary(model_reduced5_av)

# Step 5:
# Removing emotion_response did not significantly reduce model fit
# (LRT p = .067).
# AIC increased only slightly from 162.51 to 163.67 (ΔAIC = 1.16),
# indicating comparable model support.
# The reduced model remained non-singular.
# Therefore, emotion_response was removed in favour of the more
# parsimonious model.

# model_reduced5_av was selected as the final reduced model.


###############################################################
# Final Reduced Model AV
###############################################################

model_reduced_av <- model_reduced5_av

summary(model_reduced_av)
performance::check_singularity(model_reduced_av)


###############################################################
# Residual diagnostics - Final reduced model AV
###############################################################

# Simulation-based residual diagnostics
set.seed(123)

simulation_reduced_av <- DHARMa::simulateResiduals(
  fittedModel = model_reduced_av,
  n = 1000
)

plot(simulation_reduced_av)

# Test residual uniformity
DHARMa::testUniformity(simulation_reduced_av)

# Test dispersion
DHARMa::testDispersion(simulation_reduced_av)

# Test outliers
DHARMa::testOutliers(simulation_reduced_av)

# Residual diagnostics did not indicate relevant model violations.
# Residual uniformity (p = .080), dispersion (p = .732),
# and the outlier test (p = .086) were all non-significant.
# Therefore, the final reduced AV model was considered adequate
# with respect to the assessed residual assumptions.


###############################################################
### Compare full and final reduced model - AV
###############################################################

# Refit the full model using the identical complete-case dataset
model_full_reduction_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_av,
  REML = FALSE
)

# Compare full and final reduced model using likelihood ratio test
anova(
  model_reduced_av,
  model_full_reduction_av
)

# Compare AIC
AIC(
  model_reduced_av,
  model_full_reduction_av
)

# Check singularity of the comparison full model
performance::check_singularity(model_full_reduction_av)

# Final model comparison:
# The full model did not provide a significantly better fit than the
# final reduced model (LRT: Chi-square(8) = 12.71, p = .122).
# In addition, the reduced model had a lower AIC
# (163.67 vs. 166.96; delta AIC = 3.29) and a substantially lower BIC.
# The full comparison model was non-singular.
# Therefore, the more parsimonious reduced model was retained as the
# final exploratory reduced model for average temperature.


###############################################################
### Save reduced average temperature model
###############################################################

saveRDS(
  model_reduced_av,
  "output/model_reduced_av.rds"
)






###############################################################
### 7.5.3 Model reduction - Minimum temperature
###############################################################

# Create identical complete-case dataset for the entire reduction procedure
data_reduction_min <- analysis_data %>%
  tidyr::drop_na(
    delta_min_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    emotion_response,
    child_id,
    dyad_id
  )

# Set happy as reference category for emotion_response
data_reduction_min$emotion_response <- relevel(
  data_reduction_min$emotion_response,
  ref = "happy"
)

# Check number of observations
nrow(data_reduction_min)

###############################################################
# Full model used as starting point for exploratory reduction
###############################################################

model_full_reduction_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_min,
  REML = FALSE
)

summary(model_full_reduction_min)

# Check singularity
performance::check_singularity(model_full_reduction_min)

# Examine which fixed effect contributes least to model fit
drop1(
  model_full_reduction_min,
  test = "Chisq"
)

# Step 1:
# In the full model, interruption_group contributed least to model fit
# according to the drop1 analysis (p = .512).
# Therefore, interruption_group was selected for the first reduction step.


###############################################################
# Reduced model 1 - Remove interruption_group
###############################################################

model_reduced1_min <- update(
  model_full_reduction_min,
  . ~ . - interruption_group
)

# Test whether removing interruption_group decreases model fit
anova(
  model_reduced1_min,
  model_full_reduction_min
)

# Compare AIC
AIC(
  model_reduced1_min,
  model_full_reduction_min
)

# Check singularity
performance::check_singularity(model_reduced1_min)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced1_min,
  test = "Chisq"
)

# Step 1:
# Removing interruption_group did not significantly reduce model fit
# (LRT p = .619), while AIC decreased from 166.94 to 163.90.
# The reduced model remained non-singular.
# Therefore, interruption_group was removed from the model.

# Among the remaining predictors, emotion_response contributed least
# to model fit (drop1 p = .445) and was selected for the next reduction step.


###############################################################
# Reduced model 2 - Remove emotion_response
###############################################################

model_reduced2_min <- update(
  model_reduced1_min,
  . ~ . - emotion_response
)

# Test whether removing emotion_response decreases model fit
anova(
  model_reduced2_min,
  model_reduced1_min
)

# Compare AIC
AIC(
  model_reduced2_min,
  model_reduced1_min
)

# Check singularity
performance::check_singularity(model_reduced2_min)

# Examine which remaining fixed effect contributes least
drop1(
  model_reduced2_min,
  test = "Chisq"
)

# Step 2:
# Removing emotion_response did not significantly reduce model fit
# (LRT p = .488), while AIC decreased from 163.90 to 160.34.
# The reduced model remained non-singular.
# Therefore, emotion_response was removed from the model.

# Among the remaining predictors, high_proportion contributed least
# to model fit (drop1 p = .143) and was selected for the next reduction step.


###############################################################
# Reduced model 3 - Remove high_proportion
###############################################################

model_reduced3_min <- update(
  model_reduced2_min,
  . ~ . - high_proportion
)

# Test whether removing high_proportion decreases model fit
anova(
  model_reduced3_min,
  model_reduced2_min
)

# Compare AIC
AIC(
  model_reduced3_min,
  model_reduced2_min
)

# Check singularity
performance::check_singularity(model_reduced3_min)

# Examine remaining fixed effects
drop1(
  model_reduced3_min,
  test = "Chisq"
)

# Step 3:
# Removing high_proportion did not significantly reduce model fit
# (LRT p = .146), but AIC slightly increased from 160.34 to 160.45.
# More importantly, the resulting model became singular.
# Therefore, further reduction was not supported and high_proportion
# was retained in the model.
#
# model_reduced2_min was selected as the final reduced model.


###############################################################
# Final Reduced Model MIN
###############################################################

model_reduced_min <- model_reduced2_min

summary(model_reduced_min)
performance::check_singularity(model_reduced_min)


###############################################################
# Residual diagnostics - Final reduced model MIN
###############################################################

# Simulation-based residual diagnostics
set.seed(123)

simulation_reduced_min <- DHARMa::simulateResiduals(
  fittedModel = model_reduced_min,
  n = 1000
)

plot(simulation_reduced_min)

# Test residual uniformity
DHARMa::testUniformity(simulation_reduced_min)

# Test dispersion
DHARMa::testDispersion(simulation_reduced_min)

# Test outliers
DHARMa::testOutliers(simulation_reduced_min)

# Residual diagnostics did not indicate relevant model violations.
# Residual uniformity (p = .293), dispersion (p = .630),
# and the outlier test (p = .086) were all non-significant.
# No systematic residual pattern was detected.
# Therefore, the final reduced MIN model was considered adequate
# with respect to the assessed residual assumptions.


###############################################################
### Compare full and final reduced model - MIN
###############################################################

# Refit the full model using the identical complete-case dataset
model_full_reduction_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_reduction_min,
  REML = FALSE
)

# Compare full and final reduced model using likelihood ratio test
anova(
  model_reduced_min,
  model_full_reduction_min
)

# Compare AIC
AIC(
  model_reduced_min,
  model_full_reduction_min
)

# Check singularity of the comparison full model
performance::check_singularity(model_full_reduction_min)

# Final model comparison:
# The full model did not provide a significantly better fit than the
# final reduced model (LRT: Chi-square(5) = 3.39, p = .640).
# In addition, the reduced model had a substantially lower AIC
# (160.34 vs. 166.94; delta AIC = 6.61).
# The full comparison model was non-singular.
# Therefore, the more parsimonious reduced model was retained as the
# final exploratory reduced model for minimum temperature.

###############################################################
### Save reduced minimum temperature model
###############################################################

saveRDS(
  model_reduced_min,
  "output/model_reduced_min.rds"
)
























































#########################################
#########################################
######################################### nach Meeting 08.04.2026 Mediation skript 08 oder diesen weg unten
########################################

######################################################
# Average temperature (Δ average temperature)
##########################################################

#########################################################
##### Base Model
##########################################################

# base model
model_base_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

summary(model_base_av)
performance::check_singularity(model_base_av)

# Results
# The base model was non-singular and included both child- and dyad-level random intercepts.
# None of the fixed effects reached significance, although high_proportion showed a positive trend.


#########################################################
##### Base interaction model Model
##########################################################
model_base_interaction_av <- lmer(
  delta_av_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_base_av, model_base_interaction_av)

AIC(model_base_av, model_base_interaction_av)

performance::check_singularity(model_base_interaction_av)

# Adding the interaction between interruption_group and box_condition
# did not improve model fit (LRT p = .883; AIC increased from 190.74 to 194.49).
# The interaction did not improve model fit according to the likelihood-ratio
# test and AIC. However, following the theoretical analysis plan, the
# interaction model was retained as an additional base-model specification
# alongside the additive base model.


###############################################################
### Save base average temperature model
###############################################################

saveRDS(
  model_base_av,
  "output/model_base_av.rds"
)


###############################################################
# Test whether friendship rating improves the model
###############################################################

data_friendship_av <- analysis_data %>%
  tidyr::drop_na(
    delta_av_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    child_id,
    dyad_id
  )


model_base_friendship_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_av,
  REML = FALSE
)

model_friendship_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_av,
  REML = FALSE
)

anova(
  model_base_friendship_av,
  model_friendship_av
)

AIC(
  model_base_friendship_av,
  model_friendship_av
)

summary(model_friendship_av)

performance::check_singularity(model_base_friendship_av)
performance::check_singularity(model_friendship_av)

# Adding friendship_rating slightly reduced the AIC, but did not significantly
# improve model fit (LRT p = .079). Friendship showed a negative trend (p = .062).


###############################################################
# Test whether emotion response improves the model
###############################################################

data_emotion_av <- analysis_data %>%
  tidyr::drop_na(
    delta_av_temp,
    interruption_group,
    box_condition,
    high_proportion,
    emotion_response,
    child_id,
    dyad_id
  )

model_base_emotion_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_av,
  REML = FALSE
)

# Set the most frequent emotion category as the reference
data_emotion_av$emotion_response <- relevel(
  data_emotion_av$emotion_response,
  ref = "happy"
)

# Refit the model after changing the reference category
model_emotion_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_av,
  REML = FALSE
)

summary(model_emotion_av)

anova(
  model_base_emotion_av,
  model_emotion_av
)

AIC(
  model_base_emotion_av,
  model_emotion_av
)

# Adding emotion_response significantly improved model fit
# (LRT p = .036; AIC decreased from 180.59 to 178.06).
# Temperature change was lower for happy responses than for the other emotion categories.


###############################################################
# Diagnostics for the selected average-temperature model
###############################################################

# Check for a singular random-effects structure
performance::check_singularity(model_emotion_av)

# Check multicollinearity among fixed effects
performance::check_collinearity(model_emotion_av)

# Calculate marginal and conditional R²
performance::r2_nakagawa(model_emotion_av)

# Calculate the ICC for each grouping factor
performance::icc(
  model_emotion_av,
  by_group = TRUE
)

# Visually inspect model assumptions
performance::check_model(model_emotion_av)

###############################################################
# Simulation-based residual diagnostics
###############################################################

set.seed(123)

simulation_emotion_av <- DHARMa::simulateResiduals(
  fittedModel = model_emotion_av,
  n = 1000
)

plot(simulation_emotion_av)

DHARMa::testUniformity(simulation_emotion_av)
DHARMa::testDispersion(simulation_emotion_av)
DHARMa::testOutliers(simulation_emotion_av)

# Results:
  # Model diagnostics indicated no major assumption violations.
  # Multicollinearity was low (1.01-2.56 < 3), and DHARMa tests showed no significant
  # deviations in residual uniformity, dispersion, or outlier frequency.
  # Fixed effects explained 23.3% of the variance, while the full model
  # including random effects explained 56.6%.


###############################################################
# Test whether movement improves the average-temperature model
###############################################################

# Model without movement
model_without_movement_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_av,
  REML = FALSE
)

# Model including movement
model_with_movement_av <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_av,
  REML = FALSE
)

# Compare both models
anova(
  model_without_movement_av,
  model_with_movement_av
)

AIC(
  model_without_movement_av,
  model_with_movement_av
)

summary(model_with_movement_av)

performance::check_singularity(model_without_movement_av)
performance::check_singularity(model_with_movement_av)

# Adding high_proportion did not significantly improve model fit
# (LRT p = .254; AIC increased slightly from 177.36 to 178.06).
# High_proportion was not significantly associated with average
# temperature change (p = .247), but was retained as a theoretically
# relevant covariate.








#########################################################
## Minimum temperature (Δ minimum temperature)
##########################################################

#########################################################
##### model buliding 
##########################################################

# base model
model_base_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

summary(model_base_min)
performance::check_singularity(model_base_min)

# Results
# The minimum-temperature base model was non-singular.
# None of the fixed effects reached significance, although high_proportion
# showed a positive trend.


#########################################################
##### Base interaction model Model
##########################################################

model_base_interaction_min <- lmer(
  delta_min_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_base_min, model_base_interaction_min)

AIC(model_base_min, model_base_interaction_min)

performance::check_singularity(model_base_interaction_min)

# Result:
# Adding the interruption_group × box_condition interaction did not improve
# model fit (LRT p = .862; AIC increased from 189.47 to 193.17).
# Therefore, the additive base model was retained.


###############################################################
### Save base average temperature model
###############################################################

saveRDS(
  model_base_min,
  "output/model_base_min.rds"
)


###############################################################
# Test whether friendship rating improves the model
###############################################################

data_friendship_min <- analysis_data %>%
  tidyr::drop_na(
    delta_min_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    child_id,
    dyad_id
  )


model_base_friendship_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_min,
  REML = FALSE
)

model_friendship_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_min,
  REML = FALSE
)

anova(
  model_base_friendship_min,
  model_friendship_min
)

AIC(
  model_base_friendship_min,
  model_friendship_min
)

summary(model_friendship_min)

performance::check_singularity(model_base_friendship_min)
performance::check_singularity(model_friendship_min)

# Adding friendship_rating slightly reduced the AIC, but did not significantly
# improve model fit (LRT p = .074). Friendship showed a negative trend (p = .061).


###############################################################
# Test whether emotion response improves the model
###############################################################

data_emotion_min <- analysis_data %>%
  tidyr::drop_na(
    delta_min_temp,
    interruption_group,
    box_condition,
    high_proportion,
    emotion_response,
    child_id,
    dyad_id
  )

model_base_emotion_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_min,
  REML = FALSE
)

# Set the most frequent emotion category as the reference
data_emotion_min$emotion_response <- relevel(
  data_emotion_min$emotion_response,
  ref = "happy"
)

# Refit the model after changing the reference category
model_emotion_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_min,
  REML = FALSE
)

summary(model_emotion_min)

anova(
  model_base_emotion_min,
  model_emotion_min
)

AIC(
  model_base_emotion_min,
  model_emotion_min
)


performance::check_singularity(model_base_emotion_min)
performance::check_singularity(model_emotion_min)

# Adding emotion_response did not significantly improve model fit
# (LRT p = .095; AIC decreased only slightly from 178.74 to 178.37).
# Therefore, emotion_response was not retained in the minimum-temperature model.

###############################################################
# Diagnostics for the selected minimum-temperature model
###############################################################

# Check singularity
performance::check_singularity(model_base_min)

# Check multicollinearity
performance::check_collinearity(model_base_min)

# Calculate marginal and conditional R²
performance::r2_nakagawa(model_base_min)

# Calculate ICC for each grouping factor
performance::icc(
  model_base_min,
  by_group = TRUE
)

# Visually inspect model assumptions
performance::check_model(model_base_min)

###############################################################
# Simulation-based residual diagnostics
###############################################################

set.seed(123)

simulation_base_min <- DHARMa::simulateResiduals(
  fittedModel = model_base_min,
  n = 1000
)

plot(simulation_base_min)

DHARMa::testUniformity(simulation_base_min)
DHARMa::testDispersion(simulation_base_min)
DHARMa::testOutliers(simulation_base_min)

###############################################################
# Residual diagnostics by predictor
###############################################################

# Extract the exact data used in the model
model_data_min <- model.frame(model_base_min)

# Residuals versus the continuous movement covariate
DHARMa::plotResiduals(
  simulation_base_min,
  form = model_data_min$high_proportion
)

# Residuals versus interruption group
DHARMa::plotResiduals(
  simulation_base_min,
  form = model_data_min$interruption_group
)

# Residuals versus box condition
DHARMa::plotResiduals(
  simulation_base_min,
  form = model_data_min$box_condition
)

# Check residuals across experimental condition combinations
condition_combination_min <- interaction(
  model_data_min$interruption_group,
  model_data_min$box_condition
)

DHARMa::plotResiduals(
  simulation_base_min,
  form = condition_combination_min
)

# Predictor-specific DHARMa checks showed no significant residual problems
# for high_proportion, interruption_group, or box_condition.
# Although the residual-versus-predicted plot indicated a quantile pattern,
# no clear predictor-specific source was identified, and the model was retained.

###############################################################
# Test whether movement improves the minimum-temperature model
###############################################################

model_without_movement_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

model_with_movement_min <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

anova(
  model_without_movement_min,
  model_with_movement_min
)

AIC(
  model_without_movement_min,
  model_with_movement_min
)

# Adding high_proportion did not significantly improve model fit,
# although there was a weak trend (LRT p = .094).
# AIC decreased slightly from 190.27 to 189.47.
# Movement was retained as a theoretically relevant covariate.







######################################################
# MAximum temperature (Δ maximum temperature)
##########################################################

#########################################################
##### Base Model
##########################################################

# base model
model_base_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id)+
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

summary(model_base_max)
performance::check_singularity(model_base_max)

# Results
# The maximum-temperature base model was non-singular.
# None of the fixed effects reached significance, although box_condition
# showed a positive trend.


#########################################################
##### Base interaction model Model
##########################################################

model_base_interaction_max <- lmer(
  delta_max_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# compare nested models
anova(model_base_max, model_base_interaction_max)

AIC(model_base_max, model_base_interaction_max)

performance::check_singularity(model_base_interaction_max)

# Adding the interruption_group × box_condition interaction did not improve
# model fit (LRT p = .988; AIC increased from 179.91 to 183.89).
# Therefore, the additive base model was retained.
# Residual checks across all combinations of interruption_group and
# box_condition showed no significant variance or distributional differences.
# Therefore, no condition-specific source of the quantile pattern was identified.


###############################################################
### Save base average temperature model
###############################################################

saveRDS(
  model_base_max,
  "output/model_base_max.rds"
)


###############################################################
# Test whether friendship rating improves the model
###############################################################

data_friendship_max <- analysis_data %>%
  tidyr::drop_na(
    delta_max_temp,
    interruption_group,
    box_condition,
    high_proportion,
    friendship_rating,
    child_id,
    dyad_id
  )


model_base_friendship_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_max,
  REML = FALSE
)

model_friendship_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    friendship_rating +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_friendship_max,
  REML = FALSE
)

anova(
  model_base_friendship_max,
  model_friendship_max
)

AIC(
  model_base_friendship_max,
  model_friendship_max
)

summary(model_friendship_max)

performance::check_singularity(model_base_friendship_max)
performance::check_singularity(model_friendship_max)

# Adding friendship_rating did not significantly improve model fit
# (LRT p = .148; AIC remained nearly unchanged).
# The extended model was singular, so friendship_rating was not retained.


###############################################################
# Test whether emotion response improves the model
###############################################################

data_emotion_max <- analysis_data %>%
  tidyr::drop_na(
    delta_max_temp,
    interruption_group,
    box_condition,
    high_proportion,
    emotion_response,
    child_id,
    dyad_id
  )

model_base_emotion_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_max,
  REML = FALSE
)

# Set the most frequent emotion category as the reference
data_emotion_max$emotion_response <- relevel(
  data_emotion_max$emotion_response,
  ref = "happy"
)

# Refit the model after changing the reference category
model_emotion_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    emotion_response +
    (1 | child_id) +
    (1 | dyad_id),
  data = data_emotion_max,
  REML = FALSE
)

summary(model_emotion_max)

anova(
  model_base_emotion_max,
  model_emotion_max
)

AIC(
  model_base_emotion_max,
  model_emotion_max
)

performance::check_singularity(model_base_emotion_max)
performance::check_singularity(model_emotion_max)

# Adding emotion_response did not significantly improve model fit
# (LRT p = .100; AIC decreased only slightly from 170.78 to 170.53).
# Therefore, emotion_response was not retained in the maximum-temperature model.

###############################################################
# Diagnostics for the selected maximum-temperature model
###############################################################

# Check singularity
performance::check_singularity(model_base_max)

# Check multicollinearity
performance::check_collinearity(model_base_max)

# Calculate marginal and conditional R²
performance::r2_nakagawa(model_base_max)

# Calculate ICC for each grouping factor
performance::icc(
  model_base_max,
  by_group = TRUE
)

# Visually inspect model assumptions
performance::check_model(model_base_max)

###############################################################
# Simulation-based residual diagnostics
###############################################################

set.seed(123)

simulation_base_max <- DHARMa::simulateResiduals(
  fittedModel = model_base_max,
  n = 1000
)

plot(simulation_base_max)

DHARMa::testUniformity(simulation_base_max)
DHARMa::testDispersion(simulation_base_max)
DHARMa::testOutliers(simulation_base_max)

###############################################################
# Residual diagnostics by predictor
###############################################################

# Extract the exact data used in the model
model_data_max <- model.frame(model_base_max)

# Residuals versus the movement covariate
DHARMa::plotResiduals(
  simulation_base_max,
  form = model_data_max$high_proportion
)

# Residuals versus interruption group
DHARMa::plotResiduals(
  simulation_base_max,
  form = model_data_max$interruption_group
)

# Residuals versus box condition
DHARMa::plotResiduals(
  simulation_base_max,
  form = model_data_max$box_condition
)

condition_combination_max <- interaction(
  model_data_max$interruption_group,
  model_data_max$box_condition
)

DHARMa::plotResiduals(
  simulation_base_max,
  form = condition_combination_max
)

# Predictor-specific DHARMa checks showed no significant residual problems
# for high_proportion, interruption_group, box_condition, or their condition combinations.
# Although the residual-versus-predicted plot indicated a quantile pattern,
# no clear source was identified, and the final maximum-temperature model was retained.


###############################################################
# Test whether movement improves the maximum-temperature model
###############################################################

model_without_movement_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

model_with_movement_max <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

anova(
  model_without_movement_max,
  model_with_movement_max
)

AIC(
  model_without_movement_max,
  model_with_movement_max
)

# Adding high_proportion did not significantly improve model fit
# (LRT p = .134; AIC decreased only slightly from 180.15 to 179.91).
# Movement was nevertheless retained as a theoretically relevant covariate.










# Overall tests for interruption_group in the selected additive models
anova(
  model_with_movement_av,
  type = 3,
  ddf = "Satterthwaite"
)

anova(
  model_with_movement_min,
  type = 3,
  ddf = "Satterthwaite"
)

anova(
  model_with_movement_max,
  type = 3,
  ddf = "Satterthwaite"
)


# Compare interruption-group estimates before and after controlling for movement
fixef(model_without_movement_av)[
  grepl("interruption_group", names(fixef(model_without_movement_av)))
]

fixef(model_with_movement_av)[
  grepl("interruption_group", names(fixef(model_with_movement_av)))
]

fixef(model_without_movement_min)[
  grepl("interruption_group", names(fixef(model_without_movement_min)))
]

fixef(model_with_movement_min)[
  grepl("interruption_group", names(fixef(model_with_movement_min)))
]

fixef(model_without_movement_max)[
  grepl("interruption_group", names(fixef(model_without_movement_max)))
]

fixef(model_with_movement_max)[
  grepl("interruption_group", names(fixef(model_with_movement_max)))
]

# Overall conclusion of the alternative approach to mediation:
# Interruption group was not significantly associated with average,
# minimum, or maximum temperature change.

# High_proportion did not show a significant overall association with
# temperature change, although a weak positive trend was observed for
# minimum temperature.

# Controlling for movement produced only limited numerical changes in the
# interruption-group estimates and did not alter the overall conclusions.

# Furthermore, no significant interruption_group × high_proportion
# interactions were found. Thus, there was no evidence that movement
# moderated the association between interruption condition and temperature.











############################################################
## Supplementary Table: Additional covariate analyses
############################################################

library(dplyr)
library(tibble)
library(gt)
library(lme4)

# Helper function
extract_covariate_comparison <- function(
    model_without,
    model_with,
    outcome,
    covariate
) {
  
  comparison <- anova(
    model_without,
    model_with
  )
  
  tibble(
    Covariate = covariate,
    Temperature_measure = outcome,
    AIC_without = AIC(model_without),
    AIC_with = AIC(model_with),
    Chi_square = comparison$Chisq[2],
    df = comparison$Df[2],
    p_value = comparison$`Pr(>Chisq)`[2],
    Singular = ifelse(
      lme4::isSingular(model_with),
      "Yes",
      "No"
    )
  )
}


############################################################
# Combine all covariate comparisons
############################################################

additional_covariate_results <- bind_rows(
  
  # Friendship rating
  extract_covariate_comparison(
    model_base_friendship_max,
    model_friendship_max,
    "Maximum",
    "Friendship rating"
  ),
  
  extract_covariate_comparison(
    model_base_friendship_min,
    model_friendship_min,
    "Minimum",
    "Friendship rating"
  ),
  
  extract_covariate_comparison(
    model_base_friendship_av,
    model_friendship_av,
    "Average",
    "Friendship rating"
  ),
  
  # Emotion response
  extract_covariate_comparison(
    model_base_emotion_max,
    model_emotion_max,
    "Maximum",
    "Emotion response"
  ),
  
  extract_covariate_comparison(
    model_base_emotion_min,
    model_emotion_min,
    "Minimum",
    "Emotion response"
  ),
  
  extract_covariate_comparison(
    model_base_emotion_av,
    model_emotion_av,
    "Average",
    "Emotion response"
  )
)


############################################################
# Format values
############################################################

additional_covariate_results <- additional_covariate_results %>%
  mutate(
    AIC_without = round(AIC_without, 2),
    AIC_with = round(AIC_with, 2),
    Chi_square = round(Chi_square, 2),
    
    p_display = case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )


############################################################
# Create formatted table
############################################################

############################################################
# Create formatted flextable
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

additional_covariate_display <- additional_covariate_results |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::case_when(
      Covariate == "Friendship rating" &
        Temperature_measure == "Maximum" ~
        "Δ maximum temperature",
      
      Covariate == "Friendship rating" &
        Temperature_measure == "Minimum" ~
        "Δ minimum temperature",
      
      Covariate == "Friendship rating" &
        Temperature_measure == "Average" ~
        "Δ average temperature",
      
      Covariate == "Emotion response" &
        Temperature_measure == "Maximum" ~
        "Δ maximum temperature",
      
      Covariate == "Emotion response" &
        Temperature_measure == "Minimum" ~
        "Δ minimum temperature",
      
      Covariate == "Emotion response" &
        Temperature_measure == "Average" ~
        "Δ average temperature",
      
      TRUE ~ Temperature_measure
    ),
    
    AIC_without_display = sprintf(
      "%.2f",
      AIC_without
    ),
    
    AIC_with_display = sprintf(
      "%.2f",
      AIC_with
    ),
    
    Chi_square_display = sprintf(
      "%.2f",
      Chi_square
    ),
    
    df_display = as.character(
      df
    ),
    
    p_numeric = p_value,
    
    p_display = dplyr::case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )


#-----------------------------------------------------------
# Add group-heading rows
#-----------------------------------------------------------

table_covariate_display <- additional_covariate_display |>
  dplyr::mutate(
    Covariate_display = dplyr::case_when(
      dplyr::row_number() == 1 ~ "Friendship rating",
      dplyr::row_number() == 4 ~ "Emotion response",
      TRUE ~ ""
    )
  ) |>
  dplyr::select(
    Covariate_display,
    Temperature_measure,
    AIC_without_display,
    AIC_with_display,
    Chi_square_display,
    df_display,
    Singular,
    p_display,
    p_numeric
  )

#-----------------------------------------------------------
# Identify significant rows
#-----------------------------------------------------------

significant_rows <- which(
  !is.na(table_covariate_display$p_numeric) &
    table_covariate_display$p_numeric < .05
)


#-----------------------------------------------------------
# Remove helper p column
#-----------------------------------------------------------

table_covariate_display_ft <- table_covariate_display |>
  dplyr::select(
    -p_numeric
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_additional_covariates <- flextable::flextable(
  table_covariate_display_ft
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_additional_covariates <- flextable::set_header_labels(
  table_additional_covariates,
  Covariate_display = "Covariate",
  Temperature_measure = "Temperature measure",
  AIC_without_display = "AIC without",
  AIC_with_display = "AIC with",
  Chi_square_display = "χ²",
  df_display = "df",
  Singular = "Singular",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_additional_covariates <- flextable::border_remove(
  table_additional_covariates
)

table_additional_covariates <- flextable::font(
  table_additional_covariates,
  fontname = "Arial",
  part = "all"
)

table_additional_covariates <- flextable::fontsize(
  table_additional_covariates,
  size = 14,
  part = "all"
)

table_additional_covariates <- flextable::bold(
  table_additional_covariates,
  part = "header"
)

# Group headings
table_additional_covariates <- flextable::bold(
  table_additional_covariates,
  i = c(1, 4),
  j = "Covariate_display",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Text columns left aligned
table_additional_covariates <- flextable::align(
  table_additional_covariates,
  j = c(
    "Covariate_display",
    "Temperature_measure"
  ),
  align = "left",
  part = "body"
)

# Result columns centered
table_additional_covariates <- flextable::align(
  table_additional_covariates,
  j = c(
    "AIC_without_display",
    "AIC_with_display",
    "Chi_square_display",
    "df_display",
    "Singular",
    "p_display"
  ),
  align = "center",
  part = "body"
)

# All headers centered
table_additional_covariates <- flextable::align(
  table_additional_covariates,
  align = "center",
  part = "header"
)


#-----------------------------------------------------------
# Borders
#-----------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

# Top line
table_additional_covariates <- flextable::hline_top(
  table_additional_covariates,
  border = outer_border,
  part = "header"
)

# Below header
table_additional_covariates <- flextable::hline_bottom(
  table_additional_covariates,
  border = outer_border,
  part = "header"
)

# Between covariate blocks
table_additional_covariates <- flextable::hline(
  table_additional_covariates,
  i = 3,
  border = inner_border,
  part = "body"
)

# Bottom
table_additional_covariates <- flextable::hline_bottom(
  table_additional_covariates,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "Covariate_display",
  width = 2.00
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "Temperature_measure",
  width = 2.60
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "AIC_without_display",
  width = 1.25
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "AIC_with_display",
  width = 1.15
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "Chi_square_display",
  width = 0.80
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "df_display",
  width = 0.60
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "Singular",
  width = 1.00
)

table_additional_covariates <- flextable::width(
  table_additional_covariates,
  j = "p_display",
  width = 0.80
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_additional_covariates <- flextable::padding(
  table_additional_covariates,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_additional_covariates <- flextable::line_spacing(
  table_additional_covariates,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant rows
#-----------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_additional_covariates <- flextable::bold(
    table_additional_covariates,
    i = significant_rows,
    part = "body"
  )
}


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_additional_covariates


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_additional_covariates,
  path = "tables/Table_S13_additional_covariates.png",
  zoom = 3,
  expand = 10
)




































##########################
##########################
########################## 11.08. Meeting interaction models machen
#########################
#########################

######################################################
# Average temperature (Δ average temperature)
##########################################################

#########################################################
##### Base interaction model Model
##########################################################
model_base_interaction_av <- lmer(
  delta_av_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


############################################################
### Diagnostics for base interaction model - average temperature
############################################################

# Check singularity
performance::check_singularity(model_base_interaction_av)

# Check multicollinearity
performance::check_collinearity(model_base_interaction_av)

# Calculate marginal and conditional R-squared
performance::r2_nakagawa(model_base_interaction_av)

# Calculate ICC for each grouping factor
performance::icc(
  model_base_interaction_av,
  by_group = TRUE
)

# Visually inspect model assumptions
performance::check_model(model_base_interaction_av)


############################################################
### Simulation-based residual diagnostics
############################################################

set.seed(123)

simulation_base_interaction_av <- DHARMa::simulateResiduals(
  fittedModel = model_base_interaction_av,
  n = 1000
)

plot(simulation_base_interaction_av)

DHARMa::testUniformity(simulation_base_interaction_av)
DHARMa::testDispersion(simulation_base_interaction_av)
DHARMa::testOutliers(simulation_base_interaction_av)

############################################################
### Residual diagnostics by predictor
############################################################

# Extract exact data used in the fitted model
model_data_interaction_av <- model.frame(model_base_interaction_av)

# Residuals versus movement
DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = model_data_interaction_av$high_proportion
)

# Residuals versus interruption group
DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = model_data_interaction_av$interruption_group
)

# Residuals versus box condition
DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = model_data_interaction_av$box_condition
)

# Residuals versus interruption × box combination
condition_combination_av <- interaction(
  model_data_interaction_av$interruption_group,
  model_data_interaction_av$box_condition,
  drop = TRUE
)

DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = condition_combination_av
)

# Residuals by child
DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = model_data_interaction_av$child_id
)

# Residuals by dyad
DHARMa::plotResiduals(
  simulation_base_interaction_av,
  form = model_data_interaction_av$dyad_id
)

# Results:
# The base interaction model was non-singular.

# Marginal R2 was 0.099 and conditional R2 was 0.303,
# indicating that the fixed effects explained approximately 9.9% of the
# variance, while fixed and random effects together explained approximately
# 30.3% of the variance.

# ICC estimates indicated variance attributable to both child_id (ICC = 0.129)
# and dyad_id (ICC = 0.098).

# Collinearity was elevated for the interruption_group × box_condition
# interaction (VIF = 8.24), although the adjusted VIF was substantially lower
# (adjusted VIF = 2.87).

# DHARMa tests showed no significant deviation from uniformity (p = .360),
# no evidence of over- or underdispersion (p = .542), and no outlier problem
# (p = 1).

# The residual-versus-predicted plot showed a significant adjusted quantile
# pattern. However, predictor-specific residual diagnostics did not identify
# significant problems for high_proportion, interruption_group, box_condition,
# their condition combinations, or the grouping factors.

# Therefore, no clear predictor-specific source of the residual pattern was
# identified and the interaction model was retained.


###############################################################
### Save base interaction average temperature model
###############################################################

saveRDS(
  model_base_interaction_av,
  "output/model_base_interaction_av.rds"
)


############################################################
#### Reduction of average-temperature interaction model
############################################################

# Starting model:
# delta_av_temp ~
#   interruption_group * box_condition +
#   high_proportion +
#   (1 | child_id) +
#   (1 | dyad_id)

############################################################
# 1. Starting interaction model
############################################################

model_reduction_start_av <- model_base_interaction_av

summary(model_reduction_start_av)


############################################################
# 2. Test removal of the interaction
############################################################

# Reduced candidate without the interaction
# Main effects remain in the model.

model_no_interaction_av <- update(
  model_reduction_start_av,
  . ~ . - interruption_group:box_condition
)

# Compare models using likelihood-ratio test
# Both models were fitted with REML = FALSE.

anova(
  model_no_interaction_av,
  model_reduction_start_av
)

# Compare information criteria

AIC(
  model_no_interaction_av,
  model_reduction_start_av
)

BIC(
  model_no_interaction_av,
  model_reduction_start_av
)

# Reduction results:
# Removing the interruption_group × box_condition interaction did not
# significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 0.250, p = .883).
# The model without the interaction also showed lower AIC
# (190.74 vs. 194.49) and BIC (206.35 vs. 214.00).
# Therefore, the interaction was removed and the reduction was
# continued from the additive base model.


############################################################
# Continue reduction from model without interaction
############################################################

model_reduction_step1_av <- model_no_interaction_av

# Test removal of each fixed-effect term
drop1(
  model_reduction_step1_av,
  test = "Chisq"
)


############################################################
# Reduction step 2:
# Test removal of interruption_group
############################################################

model_reduction_step2_av <- update(
  model_reduction_step1_av,
  . ~ . - interruption_group
)

#---------------------------------------------------------
# Compare reduced candidate with previous model
#---------------------------------------------------------

anova(
  model_reduction_step2_av,
  model_reduction_step1_av
)

AIC(
  model_reduction_step2_av,
  model_reduction_step1_av
)

BIC(
  model_reduction_step2_av,
  model_reduction_step1_av
)

#---------------------------------------------------------
# Check remaining fixed effects
#---------------------------------------------------------

drop1(
  model_reduction_step2_av,
  test = "Chisq"
)

#---------------------------------------------------------
# Check singularity
#---------------------------------------------------------

performance::check_singularity(
  model_reduction_step2_av
)

VarCorr(
  model_reduction_step2_av
)

# Reduction step 2 results:
# Removing interruption_group did not significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 0.744, p = .690).
# Model parsimony improved, with lower AIC
# (187.49 vs. 190.74) and BIC
# (199.19 vs. 206.35).
# The reduced candidate remained non-singular.
# Therefore, interruption_group was removed from the model.


############################################################
# Reduction step 3:
# Test removal of box_condition
############################################################

model_reduction_step3_av <- update(
  model_reduction_step2_av,
  . ~ . - box_condition
)

#---------------------------------------------------------
# Compare candidate with previous model
#---------------------------------------------------------

anova(
  model_reduction_step3_av,
  model_reduction_step2_av
)

AIC(
  model_reduction_step3_av,
  model_reduction_step2_av
)

BIC(
  model_reduction_step3_av,
  model_reduction_step2_av
)

#---------------------------------------------------------
# Check remaining fixed effect
#---------------------------------------------------------

drop1(
  model_reduction_step3_av,
  test = "Chisq"
)

#---------------------------------------------------------
# Check singularity and random effects
#---------------------------------------------------------

performance::check_singularity(
  model_reduction_step3_av
)

VarCorr(
  model_reduction_step3_av
)

# Reduction step 3 results:
# Removing box_condition did not significantly reduce model fit
# (likelihood-ratio test: chi-square(1) = 1.844, p = .175).
# AIC remained essentially unchanged but slightly favored the
# simpler model (187.33 vs. 187.49), while BIC also favored
# the reduced model (197.09 vs. 199.19).
# The reduced candidate remained non-singular.
# Therefore, box_condition was removed from the model.


############################################################
# Reduction step 4:
# Test removal of high_proportion
############################################################

model_reduction_step4_av <- update(
  model_reduction_step3_av,
  . ~ . - high_proportion
)

#---------------------------------------------------------
# Compare candidate with previous model
#---------------------------------------------------------

anova(
  model_reduction_step4_av,
  model_reduction_step3_av
)

AIC(
  model_reduction_step4_av,
  model_reduction_step3_av
)

BIC(
  model_reduction_step4_av,
  model_reduction_step3_av
)

#---------------------------------------------------------
# Check singularity and random effects
#---------------------------------------------------------

performance::check_singularity(
  model_reduction_step4_av
)

VarCorr(
  model_reduction_step4_av
)

# Reduction step 4 results:
# Removing high_proportion did not significantly reduce model fit
# according to the likelihood-ratio test
# (chi-square(1) = 3.153, p = .076).
# AIC slightly favored the model retaining high_proportion
# (187.33 vs. 188.48), whereas BIC slightly favored the
# simpler model without high_proportion
# (196.29 vs. 197.09).
# Under the backward-elimination criterion, high_proportion
# was therefore removed because its removal did not result
# in a significant loss of model fit.
# The final reduced model contains no fixed-effect predictors
# and retains only the random intercepts for child_id and dyad_id.


############################################################
#### Final reduced model - average temperature
############################################################

model_reduced_interaction_av <- model_reduction_step4_av


############################################################
# Display final reduced model
############################################################

summary(
  model_reduced_interaction_av
)


############################################################
# Final comparison:
# interaction model vs final reduced model
############################################################

anova(
  model_reduced_interaction_av,
  model_base_interaction_av
)

AIC(
  model_reduced_interaction_av,
  model_base_interaction_av
)

BIC(
  model_reduced_interaction_av,
  model_base_interaction_av
)


############################################################
# Final reduced-model diagnostics
############################################################

performance::check_singularity(
  model_reduced_interaction_av
)

VarCorr(
  model_reduced_interaction_av
)


############################################################
# Final reduction results - average temperature
############################################################

# The final reduced model contained no fixed-effect predictors
# and retained only random intercepts for child_id and dyad_id:
#
# delta_av_temp ~
#   (1 | child_id) +
#   (1 | dyad_id)
#
# Compared with the original interaction model, the final reduced
# model did not show a significant loss of model fit
# (likelihood-ratio test: chi-square(6) = 5.99, p = .424).
#
# The reduced model showed substantially lower AIC
# (188.48 vs. 194.49) and BIC
# (196.29 vs. 214.00), supporting the more parsimonious model.
#
# The final reduced model was not singular.
#
# Thus, none of the fixed-effect predictors were required to
# maintain model fit under the applied backward-reduction criterion.


############################################################
#### Save final reduced model - average temperature
############################################################

saveRDS(
  model_reduced_interaction_av,
  file = "output/model_reduced_interaction_av.rds"
)












######################################################
# Minimum temperature (Δ minimum temperature)
##########################################################

#########################################################
##### Base interaction model Model
##########################################################

model_base_interaction_min <- lmer(
  delta_min_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

############################################################
### Diagnostics for base interaction model - minimum temperature
############################################################

performance::check_singularity(model_base_interaction_min)
performance::check_collinearity(model_base_interaction_min)
performance::r2_nakagawa(model_base_interaction_min)

performance::icc(
  model_base_interaction_min,
  by_group = TRUE
)

performance::check_model(model_base_interaction_min)

set.seed(123)

simulation_base_interaction_min <- DHARMa::simulateResiduals(
  fittedModel = model_base_interaction_min,
  n = 1000
)

plot(simulation_base_interaction_min)

DHARMa::testUniformity(simulation_base_interaction_min)
DHARMa::testDispersion(simulation_base_interaction_min)
DHARMa::testOutliers(simulation_base_interaction_min)

model_data_interaction_min <- model.frame(model_base_interaction_min)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = model_data_interaction_min$high_proportion
)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = model_data_interaction_min$interruption_group
)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = model_data_interaction_min$box_condition
)

condition_combination_min <- interaction(
  model_data_interaction_min$interruption_group,
  model_data_interaction_min$box_condition,
  drop = TRUE
)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = condition_combination_min
)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = model_data_interaction_min$child_id
)

DHARMa::plotResiduals(
  simulation_base_interaction_min,
  form = model_data_interaction_min$dyad_id
)

# Results:
# The base interaction model for minimum temperature was non-singular.

# Collinearity was acceptable overall. The raw VIF for the
# interruption_group × box_condition interaction was elevated,
# whereas the adjusted VIF was 2.87.

# Marginal R2 was 0.102 and conditional R2 was 0.339.
# Thus, the fixed effects explained approximately 10.2% of the variance,
# while fixed and random effects together explained approximately 33.9%.

# ICC estimates were 0.132 for child_id and 0.132 for dyad_id.

# DHARMa tests showed no significant deviation from uniformity
# (p = .431), no significant dispersion problem (p = .534),
# and no significant outlier problem (p = 1.000).

# The residual-versus-predicted plot indicated a significant quantile pattern.
# However, predictor-specific residual diagnostics for high_proportion,
# interruption_group, box_condition, their condition combinations,
# and child_id showed no significant deviations.

# Therefore, no clear predictor-specific source of the residual pattern
# was identified, and the interaction model was retained.


###############################################################
### Save base interaction minimum temperature model
###############################################################

saveRDS(
  model_base_interaction_min,
  "output/model_base_interaction_min.rds"
)


############################################################
#### Reduction of minimum-temperature interaction model
############################################################

# Starting model:
# delta_min_temp ~
#   interruption_group * box_condition +
#   high_proportion +
#   (1 | child_id) +
#   (1 | dyad_id)

############################################################
# 1. Starting interaction model
############################################################

model_reduction_start_min <- model_base_interaction_min

summary(
  model_reduction_start_min
)


############################################################
# 2. Test removal of the interaction
############################################################

# Reduced candidate without the interaction
# Main effects remain in the model.

model_no_interaction_min <- update(
  model_reduction_start_min,
  . ~ . - interruption_group:box_condition
)

############################################################
# Compare models
############################################################

anova(
  model_no_interaction_min,
  model_reduction_start_min
)

AIC(
  model_no_interaction_min,
  model_reduction_start_min
)

BIC(
  model_no_interaction_min,
  model_reduction_start_min
)


############################################################
# 3. Continue reduction from model without interaction
############################################################

model_reduction_step1_min <- model_no_interaction_min

# Test which fixed-effect term is the weakest candidate
# for further removal.

drop1(
  model_reduction_step1_min,
  test = "Chisq"
)


############################################################
# Check candidate model
############################################################

performance::check_singularity(
  model_reduction_step1_min
)

VarCorr(
  model_reduction_step1_min
)

# Reduction step 1 results:
# Removing the interruption_group × box_condition interaction did not
# significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 0.297, p = .862).
# The model without the interaction showed lower AIC
# (189.47 vs. 193.17) and BIC
# (205.08 vs. 212.68).
# The reduced candidate remained non-singular.
# Therefore, the interaction was removed and model reduction
# was continued from the additive model.


############################################################
# Reduction step 2:
# Test removal of interruption_group
############################################################

model_reduction_step2_min <- update(
  model_reduction_step1_min,
  . ~ . - interruption_group
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step2_min,
  model_reduction_step1_min
)

AIC(
  model_reduction_step2_min,
  model_reduction_step1_min
)

BIC(
  model_reduction_step2_min,
  model_reduction_step1_min
)

############################################################
# Check remaining fixed effects
############################################################

drop1(
  model_reduction_step2_min,
  test = "Chisq"
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step2_min
)

VarCorr(
  model_reduction_step2_min
)

# Reduction step 2 results:
# Removing interruption_group did not significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 0.509, p = .775).
# The reduced model showed lower AIC
# (185.98 vs. 189.47) and BIC
# (197.69 vs. 205.08).
# The reduced candidate remained non-singular.
# Therefore, interruption_group was removed from the model.


############################################################
# Reduction step 3:
# Test removal of box_condition
############################################################

model_reduction_step3_min <- update(
  model_reduction_step2_min,
  . ~ . - box_condition
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step3_min,
  model_reduction_step2_min
)

AIC(
  model_reduction_step3_min,
  model_reduction_step2_min
)

BIC(
  model_reduction_step3_min,
  model_reduction_step2_min
)

############################################################
# Check remaining fixed effect
############################################################

drop1(
  model_reduction_step3_min,
  test = "Chisq"
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step3_min
)

VarCorr(
  model_reduction_step3_min
)

# Reduction step 3 results:
# Removing box_condition did not significantly reduce model fit
# (likelihood-ratio test: chi-square(1) = 2.597, p = .107).
# AIC slightly favored the model retaining box_condition
# (185.98 vs. 186.58), whereas BIC favored the simpler model
# without box_condition (196.33 vs. 197.69).
# Under the backward-elimination criterion, box_condition was
# therefore removed because its removal did not result in a
# significant loss of model fit.
# The reduced candidate remained non-singular.


############################################################
# Reduction step 4:
# Test removal of high_proportion
############################################################

model_reduction_step4_min <- update(
  model_reduction_step3_min,
  . ~ . - high_proportion
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step4_min,
  model_reduction_step3_min
)

AIC(
  model_reduction_step4_min,
  model_reduction_step3_min
)

BIC(
  model_reduction_step4_min,
  model_reduction_step3_min
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step4_min
)

VarCorr(
  model_reduction_step4_min
)

# Reduction step 4 results:
# Removing high_proportion did not significantly reduce model fit
# according to the likelihood-ratio test
# (chi-square(1) = 3.166, p = .075).
# AIC slightly favored the model retaining high_proportion
# (186.58 vs. 187.74), whereas BIC slightly favored the
# simpler model without high_proportion
# (195.55 vs. 196.33).
# Under the backward-elimination criterion, high_proportion
# was therefore removed because its removal did not result
# in a significant loss of model fit.
# The final reduced model contains no fixed-effect predictors
# and retains only the random intercepts for child_id and dyad_id.


############################################################
#### Final reduced model - minimum temperature
############################################################

model_reduced_interaction_min <- model_reduction_step4_min


############################################################
# Display final reduced model
############################################################

summary(
  model_reduced_interaction_min
)


############################################################
# Final comparison:
# interaction model vs final reduced model
############################################################

anova(
  model_reduced_interaction_min,
  model_base_interaction_min
)

AIC(
  model_reduced_interaction_min,
  model_base_interaction_min
)

BIC(
  model_reduced_interaction_min,
  model_base_interaction_min
)


############################################################
# Final reduced-model diagnostics
############################################################

performance::check_singularity(
  model_reduced_interaction_min
)

VarCorr(
  model_reduced_interaction_min
)


############################################################
# Final reduction results - minimum temperature
############################################################

# The final reduced model contained no fixed-effect predictors
# and retained only random intercepts for child_id and dyad_id:
#
# delta_min_temp ~
#   (1 | child_id) +
#   (1 | dyad_id)
#
# Compared with the original interaction model, the final reduced
# model did not show a significant loss of model fit
# (likelihood-ratio test: chi-square(6) = 6.569, p = .363).
#
# The final reduced model showed lower AIC
# (187.74 vs. 193.17) and substantially lower BIC
# (195.55 vs. 212.68), supporting the more parsimonious model.
#
# The final reduced model was not singular.
#
# Thus, none of the fixed-effect predictors was required to retain
# comparable model fit under the applied backward-elimination
# criterion.


############################################################
#### Save final reduced model - minimum temperature
############################################################

saveRDS(
  model_reduced_interaction_min,
  file = "output/model_reduced_interaction_min.rds"
)













######################################################
# Maximum temperature (Δ maximum temperature)
##########################################################

#########################################################
##### Base interaction model Model
##########################################################
model_base_interaction_max <- lmer(
  delta_max_temp ~
    interruption_group * box_condition +
    high_proportion +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


############################################################
### Diagnostics for base interaction model - maximum temperature
############################################################

performance::check_singularity(model_base_interaction_max)
performance::check_collinearity(model_base_interaction_max)
performance::r2_nakagawa(model_base_interaction_max)

performance::icc(
  model_base_interaction_max,
  by_group = TRUE
)

performance::check_model(model_base_interaction_max)

set.seed(123)

simulation_base_interaction_max <- DHARMa::simulateResiduals(
  fittedModel = model_base_interaction_max,
  n = 1000
)

plot(simulation_base_interaction_max)

DHARMa::testUniformity(simulation_base_interaction_max)
DHARMa::testDispersion(simulation_base_interaction_max)
DHARMa::testOutliers(simulation_base_interaction_max)

model_data_interaction_max <- model.frame(model_base_interaction_max)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = model_data_interaction_max$high_proportion
)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = model_data_interaction_max$interruption_group
)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = model_data_interaction_max$box_condition
)

condition_combination_max <- interaction(
  model_data_interaction_max$interruption_group,
  model_data_interaction_max$box_condition,
  drop = TRUE
)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = condition_combination_max
)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = model_data_interaction_max$child_id
)

DHARMa::plotResiduals(
  simulation_base_interaction_max,
  form = model_data_interaction_max$dyad_id
)

# Diagnostic conclusion for the base interaction model - maximum temperature:
# The model was non-singular.
# Global DHARMa tests indicated no significant deviation from uniformity,
# dispersion, or outlier expectations.
# Although the residual-versus-predicted plot indicated a significant
# quantile pattern, predictor-specific residual checks for high_proportion,
# interruption_group, box_condition, their condition combinations,
# child_id, and dyad_id showed no significant residual problems.
# The interaction term showed increased collinearity, but the adjusted VIF
# remained moderate. As the interaction was theoretically specified,
# the model was retained.


###############################################################
### Save base interaction maximum temperature model
###############################################################

saveRDS(
  model_base_interaction_max,
  "output/model_base_interaction_max.rds"
)


############################################################
#### Reduction of maximum-temperature interaction model
############################################################

############################################################
# 1. Starting interaction model
############################################################

model_reduction_start_max <- model_base_interaction_max

summary(
  model_reduction_start_max
)


############################################################
# 2. Test removal of the interaction
############################################################

model_no_interaction_max <- update(
  model_reduction_start_max,
  . ~ . - interruption_group:box_condition
)

############################################################
# Compare models
############################################################

anova(
  model_no_interaction_max,
  model_reduction_start_max
)

AIC(
  model_no_interaction_max,
  model_reduction_start_max
)

BIC(
  model_no_interaction_max,
  model_reduction_start_max
)


############################################################
# 3. Continue reduction from model without interaction
############################################################

model_reduction_step1_max <- model_no_interaction_max

drop1(
  model_reduction_step1_max,
  test = "Chisq"
)


############################################################
# Check model
############################################################

performance::check_singularity(
  model_reduction_step1_max
)

VarCorr(
  model_reduction_step1_max
)

# Reduction step 1 results:
# Removing the interruption_group × box_condition interaction did not
# significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 0.025, p = .988).
# The model without the interaction showed lower AIC
# (179.91 vs. 183.89) and BIC
# (195.52 vs. 203.40).
# The reduced candidate remained non-singular.
# Therefore, the interaction was removed and model reduction
# was continued from the additive model.


############################################################
# Reduction step 2:
# Test removal of interruption_group
############################################################

model_reduction_step2_max <- update(
  model_reduction_step1_max,
  . ~ . - interruption_group
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step2_max,
  model_reduction_step1_max
)

AIC(
  model_reduction_step2_max,
  model_reduction_step1_max
)

BIC(
  model_reduction_step2_max,
  model_reduction_step1_max
)

############################################################
# Check remaining fixed effects
############################################################

drop1(
  model_reduction_step2_max,
  test = "Chisq"
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step2_max
)

VarCorr(
  model_reduction_step2_max
)

# Reduction step 2 results:
# Removing interruption_group did not significantly reduce model fit
# (likelihood-ratio test: chi-square(2) = 1.058, p = .589).
# The reduced model showed lower AIC
# (176.97 vs. 179.91) and BIC
# (188.68 vs. 195.52).
# The reduced candidate remained non-singular.
# Therefore, interruption_group was removed from the model.


############################################################
# Reduction step 3:
# Test removal of box_condition
############################################################

model_reduction_step3_max <- update(
  model_reduction_step2_max,
  . ~ . - box_condition
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step3_max,
  model_reduction_step2_max
)

AIC(
  model_reduction_step3_max,
  model_reduction_step2_max
)

BIC(
  model_reduction_step3_max,
  model_reduction_step2_max
)

############################################################
# Check remaining fixed effect
############################################################

drop1(
  model_reduction_step3_max,
  test = "Chisq"
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step3_max
)

VarCorr(
  model_reduction_step3_max
)

# Reduction step 3 results:
# Removing box_condition did not significantly reduce model fit
# (likelihood-ratio test: chi-square(1) = 3.221, p = .073).
# AIC slightly favored the model retaining box_condition
# (176.97 vs. 178.19), whereas BIC slightly favored the
# simpler model without box_condition
# (187.95 vs. 188.68).
# Under the backward-elimination criterion, box_condition was
# therefore removed because its removal did not result in a
# significant loss of model fit.
# The reduced candidate remained non-singular.


############################################################
# Reduction step 4:
# Test removal of high_proportion
############################################################

model_reduction_step4_max <- update(
  model_reduction_step3_max,
  . ~ . - high_proportion
)

############################################################
# Compare candidate with previous model
############################################################

anova(
  model_reduction_step4_max,
  model_reduction_step3_max
)

AIC(
  model_reduction_step4_max,
  model_reduction_step3_max
)

BIC(
  model_reduction_step4_max,
  model_reduction_step3_max
)

############################################################
# Check singularity and random effects
############################################################

performance::check_singularity(
  model_reduction_step4_max
)

VarCorr(
  model_reduction_step4_max
)

# Reduction step 4 results:
# Removing high_proportion did not significantly reduce model fit
# according to the likelihood-ratio test
# (chi-square(1) = 2.760, p = .097).
# AIC slightly favored the model retaining high_proportion
# (178.19 vs. 178.95), whereas BIC slightly favored the
# simpler model without high_proportion
# (186.76 vs. 187.95).
# Under the backward-elimination criterion, high_proportion
# was therefore removed because its removal did not result
# in a significant loss of model fit.
# The final reduced model contains no fixed-effect predictors
# and retains only the random intercepts for child_id and dyad_id.


############################################################
#### Final reduced model - maximum temperature
############################################################

model_reduced_interaction_max <- model_reduction_step4_max


############################################################
# Display final reduced model
############################################################

summary(
  model_reduced_interaction_max
)


############################################################
# Final comparison:
# interaction model vs final reduced model
############################################################

anova(
  model_reduced_interaction_max,
  model_base_interaction_max
)

AIC(
  model_reduced_interaction_max,
  model_base_interaction_max
)

BIC(
  model_reduced_interaction_max,
  model_base_interaction_max
)


############################################################
# Final reduced-model diagnostics
############################################################

performance::check_singularity(
  model_reduced_interaction_max
)

VarCorr(
  model_reduced_interaction_max
)


############################################################
# Final reduction results - maximum temperature
############################################################

# The final reduced model contained no fixed-effect predictors
# and retained only random intercepts for child_id and dyad_id:
#
# delta_max_temp ~
#   (1 | child_id) +
#   (1 | dyad_id)
#
# Compared with the original interaction model, the final reduced
# model did not show a significant loss of model fit
# (likelihood-ratio test: chi-square(6) = 7.064, p = .315).
#
# The final reduced model showed lower AIC
# (178.95 vs. 183.89) and substantially lower BIC
# (186.76 vs. 203.40), supporting the more parsimonious model.
#
# The final reduced model was not singular.
#
# Thus, none of the fixed-effect predictors was required to retain
# comparable model fit under the applied backward-elimination
# criterion.


############################################################
#### Save final reduced model - maximum temperature
############################################################

saveRDS(
  model_reduced_interaction_max,
  file = "output/model_reduced_interaction_max.rds"
)










############################################################
#### Final validation: interaction vs reduced models
############################################################


############################################################
# Average temperature
############################################################

comparison_reduction_av <- anova(
  model_reduced_interaction_av,
  model_base_interaction_av
)

comparison_reduction_av

AIC(
  model_reduced_interaction_av,
  model_base_interaction_av
)

BIC(
  model_reduced_interaction_av,
  model_base_interaction_av
)

logLik(model_reduced_interaction_av)
logLik(model_base_interaction_av)


############################################################
# Minimum temperature
############################################################

comparison_reduction_min <- anova(
  model_reduced_interaction_min,
  model_base_interaction_min
)

comparison_reduction_min

AIC(
  model_reduced_interaction_min,
  model_base_interaction_min
)

BIC(
  model_reduced_interaction_min,
  model_base_interaction_min
)

logLik(model_reduced_interaction_min)
logLik(model_base_interaction_min)


############################################################
# Maximum temperature
############################################################

comparison_reduction_max <- anova(
  model_reduced_interaction_max,
  model_base_interaction_max
)

comparison_reduction_max

AIC(
  model_reduced_interaction_max,
  model_base_interaction_max
)

BIC(
  model_reduced_interaction_max,
  model_base_interaction_max
)

logLik(model_reduced_interaction_max)
logLik(model_base_interaction_max)

############################################################
# Final validation results: interaction vs reduced models
############################################################

# For all three temperature outcomes, the fully reduced models
# containing only random intercepts did not fit the data
# significantly worse than the corresponding interaction models.
#
# Average temperature:
# chi-square(6) = 5.99, p = .424.
# The reduced model showed lower AIC (188.48 vs. 194.49)
# and BIC (196.29 vs. 214.00).
#
# Minimum temperature:
# chi-square(6) = 6.57, p = .363.
# The reduced model showed lower AIC (187.74 vs. 193.17)
# and BIC (195.55 vs. 212.68).
#
# Maximum temperature:
# chi-square(6) = 7.06, p = .315.
# The reduced model showed lower AIC (178.95 vs. 183.89)
# and BIC (186.76 vs. 203.40).
#
# Thus, removing all fixed-effect predictors did not result in
# a significant loss of model fit for any of the three outcomes,
# while the information criteria consistently favored the
# more parsimonious reduced models.


############################################################
#### Table 5: Final model-reduction comparisons
############################################################

library(gt)


#-----------------------------------------------------------
# Store final likelihood-ratio comparisons
#-----------------------------------------------------------

comparison_reduction_av <- anova(
  model_reduced_interaction_av,
  model_base_interaction_av
)

comparison_reduction_min <- anova(
  model_reduced_interaction_min,
  model_base_interaction_min
)

comparison_reduction_max <- anova(
  model_reduced_interaction_max,
  model_base_interaction_max
)


#-----------------------------------------------------------
# Create summary data frame
#-----------------------------------------------------------

model_reduction_table <- tibble::tibble(
  
  Temperature_measure = c(
    "Maximum",
    "Minimum",
    "Average"
  ),
  
  Reduced_AIC = c(
    AIC(model_reduced_interaction_max),
    AIC(model_reduced_interaction_min),
    AIC(model_reduced_interaction_av)
  ),
  
  Interaction_AIC = c(
    AIC(model_base_interaction_max),
    AIC(model_base_interaction_min),
    AIC(model_base_interaction_av)
  ),
  
  Chi_square = c(
    comparison_reduction_max$Chisq[2],
    comparison_reduction_min$Chisq[2],
    comparison_reduction_av$Chisq[2]
  ),
  
  df = c(
    comparison_reduction_max$Df[2],
    comparison_reduction_min$Df[2],
    comparison_reduction_av$Df[2]
  ),
  
  p = c(
    comparison_reduction_max$`Pr(>Chisq)`[2],
    comparison_reduction_min$`Pr(>Chisq)`[2],
    comparison_reduction_av$`Pr(>Chisq)`[2]
  )
)

model_reduction_table


#-----------------------------------------------------------
# Create formatted Table 5
#-----------------------------------------------------------

table_model_reduction <- model_reduction_table %>%
  
  gt::gt() %>%
  
  gt::tab_header(
    title = "Model reduction",
    subtitle = "Comparison of primary interaction and final reduced models"
  ) %>%
  
  gt::cols_label(
    Temperature_measure = "Temperature measure",
    Reduced_AIC = "Reduced AIC",
    Interaction_AIC = "Interaction AIC",
    Chi_square = "χ²",
    df = "df",
    p = "p"
  ) %>%
  
  gt::fmt_number(
    columns = c(
      Reduced_AIC,
      Interaction_AIC,
      Chi_square
    ),
    decimals = 2
  ) %>%
  
  gt::fmt_integer(
    columns = df
  ) %>%
  
  gt::fmt_number(
    columns = p,
    decimals = 3
  ) %>%
  
  gt::cols_align(
    align = "left",
    columns = Temperature_measure
  ) %>%
  
  gt::cols_align(
    align = "center",
    columns = c(
      Reduced_AIC,
      Interaction_AIC,
      Chi_square,
      df,
      p
    )
  ) %>%
  
  gt::tab_source_note(
    source_note = paste0(
      "Note. Likelihood-ratio tests compare the final reduced model ",
      "containing only random intercepts for child and dyad with the ",
      "corresponding primary interaction model."
    )
  ) %>%
  
  gt::tab_options(
    table.font.size = 11,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 10,
    column_labels.font.weight = "bold",
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    data_row.padding = gt::px(5)
  )


table_model_reduction


dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

gt::gtsave(
  table_model_reduction,
  filename = "tables/05_model_reduction.docx"
)




###############################
################################
############################### MODERATION ANALYSIS
##############################
###############################






###############################################################
# Moderation analysis
###############################################################

###############################################################
# interruption x movement
###############################################################

###############################################################
# maximum temperature - interruption x movement
###############################################################

# Mean-center movement for easier interpretation
analysis_data <- analysis_data %>%
  mutate(
    high_proportion_c = high_proportion -
      mean(high_proportion, na.rm = TRUE)
  )

# Additive model with centered movement
model_with_movement_max_c <- lmer(
  delta_max_temp ~
    interruption_group +
    box_condition +
    high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Moderation model
model_moderation_max <- lmer(
  delta_max_temp ~
    interruption_group * high_proportion_c +
    box_condition +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Test whether the interaction improves model fit
anova(
  model_with_movement_max_c,
  model_moderation_max
)

AIC(
  model_with_movement_max_c,
  model_moderation_max
)

summary(model_moderation_max)

performance::check_singularity(model_moderation_max)

# Moderation results:
# Adding the interruption_group × high_proportion_c interaction did not
# significantly improve model fit
# (LRT p = .257; AIC increased from 179.91 to 181.20).
#
# Thus, there was no evidence that movement moderated the effect of
# interruption condition on maximum temperature change.
# The moderation model was non-singular







###############################################################
# minimum temperature - interruption x movement
###############################################################

# Mean-center movement for easier interpretation
analysis_data <- analysis_data %>%
  mutate(
    high_proportion_c = high_proportion -
      mean(high_proportion, na.rm = TRUE)
  )

# Additive model with centered movement
model_with_movement_min_c <- lmer(
  delta_min_temp ~
    interruption_group +
    box_condition +
    high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Moderation model
model_moderation_min <- lmer(
  delta_min_temp ~
    interruption_group * high_proportion_c +
    box_condition +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

# Test whether the interaction improves model fit
anova(
  model_with_movement_min_c,
  model_moderation_min
)

AIC(
  model_with_movement_min_c,
  model_moderation_min
)

summary(model_moderation_min)

performance::check_singularity(model_moderation_min)

# Moderation results:
# Adding the interruption_group × high_proportion_c interaction did not
# significantly improve model fit
# (LRT p = .384; AIC increased from 189.47 to 191.56).
#
# Thus, there was no evidence that movement moderated the effect of
# interruption condition on minimum temperature change.
# The moderation model was non-singular.






############################################################
## Moderation analysis: interruption group - average temperature
############################################################

#-----------------------------------------------------------
# Additive comparison model
#-----------------------------------------------------------

model_with_movement_av_c <- lmer(
  delta_av_temp ~
    interruption_group +
    box_condition +
    high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#-----------------------------------------------------------
# Moderation model
#-----------------------------------------------------------

model_moderation_av <- lmer(
  delta_av_temp ~
    interruption_group * high_proportion_c +
    box_condition +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#-----------------------------------------------------------
# Compare additive and moderation model
#-----------------------------------------------------------

anova(
  model_with_movement_av_c,
  model_moderation_av
)

AIC(
  model_with_movement_av_c,
  model_moderation_av
)


#-----------------------------------------------------------
# Inspect moderation model
#-----------------------------------------------------------

summary(model_moderation_av)

performance::check_singularity(
  model_moderation_av
)


# Moderation results:
# Adding the interruption_group × high_proportion_c interaction did not
# significantly improve model fit
# (LRT chi-square(2) = 2.25, p = .325;
# AIC increased from 190.74 to 192.49).
#
# Thus, there was no evidence that movement moderated the effect of
# interruption condition on average temperature change.
# The moderation model was non-singular.











###############################################################
# box condition x movement
###############################################################

############################################################
# box condition - average temperature
############################################################

model_moderation_box_av <- lmer(
  delta_av_temp ~
    box_condition * high_proportion_c +
    interruption_group +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

#-----------------------------------------------------------
# Additive comparison model using exactly the same data
#-----------------------------------------------------------

model_no_moderation_box_av <- update(
  model_moderation_box_av,
  . ~ . - box_condition:high_proportion_c
)

#-----------------------------------------------------------
# Check same number of observations
#-----------------------------------------------------------

nobs(model_no_moderation_box_av)
nobs(model_moderation_box_av)

#-----------------------------------------------------------
# Compare models
#-----------------------------------------------------------

anova(
  model_no_moderation_box_av,
  model_moderation_box_av
)

AIC(
  model_no_moderation_box_av,
  model_moderation_box_av
)

#-----------------------------------------------------------
# Inspect moderation model
#-----------------------------------------------------------

summary(model_moderation_box_av)

performance::check_singularity(
  model_moderation_box_av
)

# Moderation results:
# Adding the box_condition × high_proportion_c interaction significantly
# improved model fit
# (LRT chi-square(1) = 5.68, p = .017; AIC: 190.74 -> 187.06).
# The interaction was significant and negative
# (b = -7.05, SE = 2.83, p = .016).
#
# Thus, there was evidence that movement moderated the effect of
# box condition on average temperature change.
# The moderation model was non-singular.






############################################################
# box condition - minimum temperature
############################################################

model_moderation_box_min <- lmer(
  delta_min_temp ~
    box_condition * high_proportion_c +
    interruption_group +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)

anova(
  model_with_movement_min_c,
  model_moderation_box_min
)

AIC(
  model_with_movement_min_c,
  model_moderation_box_min
)

summary(model_moderation_box_min)

performance::check_singularity(
  model_moderation_box_min
)

# Moderation results:
# Adding the box_condition × high_proportion_c interaction significantly
# improved model fit
# (LRT chi-square(1) = 6.00, p = .014; AIC: 189.47 -> 185.47).
# The interaction was significant and negative
# (b = -7.07, SE = 2.77, p = .014).
#
# Thus, there was evidence that movement moderated the effect of
# box condition on minimum temperature change.
# The moderation model was non-singular.


############################################################
# box condition - maximum temperature
############################################################

# Test whether movement moderates the association between
# box condition and maximum temperature change

model_moderation_box_max <- lmer(
  delta_max_temp ~
    box_condition * high_proportion_c +
    interruption_group +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#-----------------------------------------------------------
# Compare additive and moderation model
#-----------------------------------------------------------

anova(
  model_with_movement_max_c,
  model_moderation_box_max
)

AIC(
  model_with_movement_max_c,
  model_moderation_box_max
)


#----------------------------------------------------------
# Inspect moderation model
#----------------------------------------------------------

summary(model_moderation_box_max)

performance::check_singularity(
  model_moderation_box_max
)

# Moderation results:
# Adding the box_condition × high_proportion_c interaction significantly
# improved model fit
# (LRT chi-square(1) = 4.93, p = .026; AIC: 179.91 -> 176.99).
# The interaction was significant and negative
# (b = -5.99, SE = 2.60, p = .026).
#
# Thus, there was evidence that movement moderated the effect of
# box condition on maximum temperature change.
# The moderation model was non-singular.












############################################################
# Three-way moderation analysis: box condition x interruption x movement
############################################################

############################################################
#  maximum temperature
############################################################

model_moderation_threeway_max <- lmer(
  delta_max_temp ~
    box_condition * interruption_group * high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#----------------------------------------------------------
# Comparison model without the three-way interaction
#----------------------------------------------------------

model_no_threeway_max <- update(
  model_moderation_threeway_max,
  . ~ . - box_condition:interruption_group:high_proportion_c
)


#----------------------------------------------------------
# Check same observations
#----------------------------------------------------------

nobs(model_no_threeway_max)
nobs(model_moderation_threeway_max)


#----------------------------------------------------------
# Test whether the three-way interaction improves model fit
#----------------------------------------------------------

anova(
  model_no_threeway_max,
  model_moderation_threeway_max
)

AIC(
  model_no_threeway_max,
  model_moderation_threeway_max
)


#----------------------------------------------------------
# Inspect model
#----------------------------------------------------------

summary(model_moderation_threeway_max)

performance::check_singularity(
  model_moderation_threeway_max
)


#----------------------------------------------------------
# Number of observations per experimental combination
#----------------------------------------------------------

table(
  analysis_data$box_condition,
  analysis_data$interruption_group
)


#----------------------------------------------------------
# Movement distribution within each experimental combination
#----------------------------------------------------------

analysis_data |>
  dplyr::group_by(
    box_condition,
    interruption_group
  ) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_high = mean(high_proportion, na.rm = TRUE),
    sd_high = sd(high_proportion, na.rm = TRUE),
    min_high = min(high_proportion, na.rm = TRUE),
    max_high = max(high_proportion, na.rm = TRUE),
    .groups = "drop"
  )


#----------------------------------------------------------
# Movement slopes for each Box × Interruption combination
#----------------------------------------------------------

trends_threeway_max <- emmeans::emtrends(
  model_moderation_threeway_max,
  ~ box_condition * interruption_group,
  var = "high_proportion_c"
)

trends_threeway_max


trends_box_by_interruption_max <- emmeans::emtrends(
  model_moderation_threeway_max,
  ~ box_condition | interruption_group,
  var = "high_proportion_c"
)

pairs(
  trends_box_by_interruption_max,
  adjust = "holm"
)


#----------------------------------------------------------
# Simple movement slopes using Satterthwaite df
#----------------------------------------------------------

trends_threeway_max <- emmeans::emtrends(
  model_moderation_threeway_max,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

trends_threeway_max


#----------------------------------------------------------
# Compare NC vs JA within each interruption group
#----------------------------------------------------------

trends_box_by_interruption_max <- emmeans::emtrends(
  model_moderation_threeway_max,
  ~ box_condition | interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

pairs(
  trends_box_by_interruption_max,
  adjust = "holm"
)

# Three-way moderation results:
# Adding the box_condition × interruption_group × high_proportion_c
# interaction significantly improved model fit
# (LRT chi-square(2) = 17.91, p < .001; AIC: 178.82 -> 164.91).
#
# Thus, there was evidence that movement moderated the
# Box condition × Interruption condition interaction on
# maximum temperature change.
# The three-way model was non-singular.










############################################################
## Three-way moderation analysis: minimum temperature
############################################################


#-----------------------------------------------------------
# Fit three-way moderation model
#-----------------------------------------------------------

model_moderation_threeway_min <- lmer(
  delta_min_temp ~
    box_condition * interruption_group * high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#-----------------------------------------------------------
# Comparison model without three-way interaction
#-----------------------------------------------------------

model_no_threeway_min <- update(
  model_moderation_threeway_min,
  . ~ . - box_condition:interruption_group:high_proportion_c
)


#-----------------------------------------------------------
# Check number of observations
#-----------------------------------------------------------

nobs(model_no_threeway_min)
nobs(model_moderation_threeway_min)


#-----------------------------------------------------------
# Test whether three-way interaction improves model fit
#-----------------------------------------------------------

anova(
  model_no_threeway_min,
  model_moderation_threeway_min
)

AIC(
  model_no_threeway_min,
  model_moderation_threeway_min
)


#-----------------------------------------------------------
# Inspect three-way moderation model
#-----------------------------------------------------------

summary(model_moderation_threeway_min)

performance::check_singularity(
  model_moderation_threeway_min
)


#-----------------------------------------------------------
# Estimate movement slopes for all experimental combinations
#-----------------------------------------------------------

trends_threeway_min <- emmeans::emtrends(
  model_moderation_threeway_min,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

trends_threeway_min


#-----------------------------------------------------------
# Compare NC and JA slopes within each interruption group
#-----------------------------------------------------------

trends_box_by_interruption_min <- emmeans::emtrends(
  model_moderation_threeway_min,
  ~ box_condition | interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

pairs(
  trends_box_by_interruption_min,
  adjust = "holm"
)


# Three-way moderation results:
# Adding the box_condition × interruption_group × high_proportion_c
# interaction significantly improved model fit
# (LRT chi-square(2) = 14.74, p < .001; AIC: 188.12 -> 177.38).
#
# Thus, there was evidence that movement moderated the
# Box condition × Interruption condition interaction on
# minimum temperature change.
# The three-way model was non-singular.










############################################################
## Three-way moderation analysis: average temperature
############################################################


#-----------------------------------------------------------
# Fit three-way moderation model
#-----------------------------------------------------------

model_moderation_threeway_av <- lmer(
  delta_av_temp ~
    box_condition * interruption_group * high_proportion_c +
    (1 | child_id) +
    (1 | dyad_id),
  data = analysis_data,
  REML = FALSE
)


#-----------------------------------------------------------
# Comparison model without three-way interaction
#-----------------------------------------------------------

model_no_threeway_av <- update(
  model_moderation_threeway_av,
  . ~ . - box_condition:interruption_group:high_proportion_c
)


#-----------------------------------------------------------
# Check number of observations
#-----------------------------------------------------------

nobs(model_no_threeway_av)
nobs(model_moderation_threeway_av)


#-----------------------------------------------------------
# Test whether three-way interaction improves model fit
#-----------------------------------------------------------

anova(
  model_no_threeway_av,
  model_moderation_threeway_av
)

AIC(
  model_no_threeway_av,
  model_moderation_threeway_av
)


#-----------------------------------------------------------
# Inspect three-way moderation model
#-----------------------------------------------------------

summary(model_moderation_threeway_av)

performance::check_singularity(
  model_moderation_threeway_av
)


#-----------------------------------------------------------
# Estimate movement slopes for all experimental combinations
#-----------------------------------------------------------

trends_threeway_av <- emmeans::emtrends(
  model_moderation_threeway_av,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

trends_threeway_av


#-----------------------------------------------------------
# Compare NC and JA slopes within each interruption group
#-----------------------------------------------------------

trends_box_by_interruption_av <- emmeans::emtrends(
  model_moderation_threeway_av,
  ~ box_condition | interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

pairs(
  trends_box_by_interruption_av,
  adjust = "holm"
)


# Three-way moderation results:
# Adding the box_condition × interruption_group × high_proportion_c
# interaction significantly improved model fit
# (LRT chi-square(2) = 12.77, p = .002; AIC: 189.46 -> 180.69).
#
# Thus, there was evidence that movement moderated the
# Box condition × Interruption condition interaction on
# average temperature change.
# The three-way model was non-singular.









############################################################
## Summary of moderation analyses
############################################################


#-----------------------------------------------------------
# Movement as moderator of Interruption condition
#-----------------------------------------------------------

# There was no evidence that movement moderated the effect of
# interruption condition on temperature change for any outcome:
#
# MAX: LRT chi-square(2) = 2.72, p = .257
# MIN: LRT chi-square(2) = 1.91, p = .384
# AV:  LRT chi-square(2) = 2.25, p = .325


#-----------------------------------------------------------
# Movement as moderator of Box condition
#-----------------------------------------------------------

# Movement significantly moderated the effect of box condition
# on all three temperature outcomes:
#
# MAX: LRT chi-square(1) = 4.93, p = .026
# MIN: LRT chi-square(1) = 6.00, p = .014
# AV:  LRT chi-square(1) = 5.68, p = .017
#
# Thus, the effect of box condition on facial temperature change
# depended on the level of movement.


#-----------------------------------------------------------
# Movement as moderator of Box × Interruption interaction
#-----------------------------------------------------------

# Movement significantly moderated the Box condition ×
# Interruption condition interaction for all three outcomes:
#
# MAX: LRT chi-square(2) = 17.91, p < .001
# MIN: LRT chi-square(2) = 14.74, p < .001
# AV:  LRT chi-square(2) = 12.77, p = .002
#
# Follow-up contrasts showed that the movement-related change
# in the NC–JA difference differed significantly between
# No interruption and Accidental interruption for all outcomes:
#
# MAX: b = 24.44, p < .001
# MIN: b = 25.20, p < .001
# AV:  b = 24.27, p < .001
#
# It also differed significantly between No interruption and
# Deliberate interruption:
#
# MAX: b = 21.51, p = .003
# MIN: b = 20.35, p = .009
# AV:  b = 18.32, p = .028
#
# In contrast, Accidental and Deliberate interruption did not
# differ significantly in this moderation effect:
#
# MAX: p = .617
# MIN: p = .454
# AV:  p = .380
#
# Overall, movement moderated the Box condition × Interruption
# condition interaction consistently across all three temperature
# outcomes. This moderation was primarily characterized by
# differences between the No-interruption condition and both
# interruption conditions.
#
# All moderation models were non-singular.






############################################################
## Moderation results tables
############################################################

library(dplyr)
library(tidyr)
library(tibble)
library(gt)
library(emmeans)


############################################################
## Moderation model-comparison table
############################################################


#-----------------------------------------------------------
# Helper function: extract likelihood-ratio comparison
#-----------------------------------------------------------

extract_moderation_comparison <- function(
    model_without,
    model_with,
    outcome_name,
    moderation_name
) {
  
  comparison <- stats::anova(
    model_without,
    model_with
  )
  
  tibble::tibble(
    
    Outcome =
      outcome_name,
    
    Moderation =
      moderation_name,
    
    AIC_without =
      stats::AIC(model_without),
    
    AIC_with =
      stats::AIC(model_with),
    
    Chi_square =
      comparison$Chisq[2],
    
    # Difference in number of estimated parameters
    df =
      attr(logLik(model_with), "df") -
      attr(logLik(model_without), "df"),
    
    p_value =
      comparison$`Pr(>Chisq)`[2]
  )
}


#-----------------------------------------------------------
# Create comparable models without the tested interaction
#-----------------------------------------------------------

# Interruption condition × movement
model_no_moderation_max <- update(
  model_moderation_max,
  . ~ . - interruption_group:high_proportion_c
)

model_no_moderation_min <- update(
  model_moderation_min,
  . ~ . - interruption_group:high_proportion_c
)

model_no_moderation_av <- update(
  model_moderation_av,
  . ~ . - interruption_group:high_proportion_c
)


# Box condition × movement
model_no_moderation_box_max <- update(
  model_moderation_box_max,
  . ~ . - box_condition:high_proportion_c
)

model_no_moderation_box_min <- update(
  model_moderation_box_min,
  . ~ . - box_condition:high_proportion_c
)

model_no_moderation_box_av <- update(
  model_moderation_box_av,
  . ~ . - box_condition:high_proportion_c
)


# Three-way interaction
model_no_threeway_max <- update(
  model_moderation_threeway_max,
  . ~ . - box_condition:interruption_group:high_proportion_c
)

model_no_threeway_min <- update(
  model_moderation_threeway_min,
  . ~ . - box_condition:interruption_group:high_proportion_c
)

model_no_threeway_av <- update(
  model_moderation_threeway_av,
  . ~ . - box_condition:interruption_group:high_proportion_c
)


#-----------------------------------------------------------
# Combine all nine moderation comparisons
#-----------------------------------------------------------

moderation_results_table_raw <- dplyr::bind_rows(
  
  # Interruption × movement
  extract_moderation_comparison(
    model_no_moderation_max,
    model_moderation_max,
    "Maximum",
    "Interruption × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_moderation_min,
    model_moderation_min,
    "Minimum",
    "Interruption × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_moderation_av,
    model_moderation_av,
    "Average",
    "Interruption × Movement"
  ),
  
  
  # Box × movement
  extract_moderation_comparison(
    model_no_moderation_box_max,
    model_moderation_box_max,
    "Maximum",
    "Box condition × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_moderation_box_min,
    model_moderation_box_min,
    "Minimum",
    "Box condition × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_moderation_box_av,
    model_moderation_box_av,
    "Average",
    "Box condition × Movement"
  ),
  
  
  # Box × interruption × movement
  extract_moderation_comparison(
    model_no_threeway_max,
    model_moderation_threeway_max,
    "Maximum",
    "Box × Interruption × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_threeway_min,
    model_moderation_threeway_min,
    "Minimum",
    "Box × Interruption × Movement"
  ),
  
  extract_moderation_comparison(
    model_no_threeway_av,
    model_moderation_threeway_av,
    "Average",
    "Box × Interruption × Movement"
  )
)


#-----------------------------------------------------------
# Prepare presentation version
#-----------------------------------------------------------

moderation_results_table <- moderation_results_table_raw |>
  dplyr::mutate(
    
    AIC_without =
      round(AIC_without, 2),
    
    AIC_with =
      round(AIC_with, 2),
    
    Chi_square =
      round(Chi_square, 2),
    
    p_display =
      dplyr::case_when(
        p_value < 0.001 ~ "< .001",
        TRUE ~ sprintf("%.3f", p_value)
      )
  )
############################################################
# Create formatted flextable
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare display table
#-----------------------------------------------------------

moderation_display <- moderation_results_table |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum" = "Δ maximum temperature",
      "Minimum" = "Δ minimum temperature",
      "Average" = "Δ average temperature"
    ),
    
    AIC_without_display = sprintf(
      "%.2f",
      AIC_without
    ),
    
    AIC_with_display = sprintf(
      "%.2f",
      AIC_with
    ),
    
    Chi_square_display = sprintf(
      "%.2f",
      Chi_square
    ),
    
    df_display = as.character(
      df
    ),
    
    p_numeric = p_value,
    
    p_display = dplyr::case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )


#-----------------------------------------------------------
# Show moderation heading only in first row of each block
#-----------------------------------------------------------

moderation_display <- moderation_display |>
  dplyr::mutate(
    Moderation_display = dplyr::case_when(
      dplyr::row_number() == 1 ~
        "Interruption × Movement",
      
      dplyr::row_number() == 4 ~
        "Box condition × Movement",
      
      dplyr::row_number() == 7 ~
        "Box condition × Interruption × Movement",
      
      TRUE ~ ""
    )
  ) |>
  dplyr::select(
    Moderation_display,
    Temperature_measure,
    AIC_without_display,
    AIC_with_display,
    Chi_square_display,
    df_display,
    p_display,
    p_numeric
  )


#-----------------------------------------------------------
# Identify significant rows
#-----------------------------------------------------------

significant_rows <- which(
  !is.na(moderation_display$p_numeric) &
    moderation_display$p_numeric < .05
)


#-----------------------------------------------------------
# Remove helper column
#-----------------------------------------------------------

moderation_display_ft <- moderation_display |>
  dplyr::select(
    -p_numeric
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_moderation_results <- flextable::flextable(
  moderation_display_ft
)

# Merge moderation labels across the three temperature rows
table_moderation_results <- flextable::merge_at(
  table_moderation_results,
  i = 1:3,
  j = "Moderation_display",
  part = "body"
)

table_moderation_results <- flextable::merge_at(
  table_moderation_results,
  i = 4:6,
  j = "Moderation_display",
  part = "body"
)

table_moderation_results <- flextable::merge_at(
  table_moderation_results,
  i = 7:9,
  j = "Moderation_display",
  part = "body"
)

#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_moderation_results <- flextable::set_header_labels(
  table_moderation_results,
  Moderation_display = "Moderation",
  Temperature_measure = "Temperature measure",
  AIC_without_display = "AIC without",
  AIC_with_display = "AIC with",
  Chi_square_display = "χ²",
  df_display = "df",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_moderation_results <- flextable::border_remove(
  table_moderation_results
)

table_moderation_results <- flextable::font(
  table_moderation_results,
  fontname = "Arial",
  part = "all"
)

table_moderation_results <- flextable::fontsize(
  table_moderation_results,
  size = 14,
  part = "all"
)

table_moderation_results <- flextable::bold(
  table_moderation_results,
  part = "header"
)

# Moderation block labels bold
table_moderation_results <- flextable::bold(
  table_moderation_results,
  i = c(1, 4, 7),
  j = "Moderation_display",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Text columns left-aligned
table_moderation_results <- flextable::align(
  table_moderation_results,
  j = c(
    "Moderation_display",
    "Temperature_measure"
  ),
  align = "left",
  part = "body"
)

# Numeric columns centered
table_moderation_results <- flextable::align(
  table_moderation_results,
  j = c(
    "AIC_without_display",
    "AIC_with_display",
    "Chi_square_display",
    "df_display",
    "p_display"
  ),
  align = "center",
  part = "body"
)

# Headers centered
table_moderation_results <- flextable::align(
  table_moderation_results,
  align = "center",
  part = "header"
)

table_moderation_results <- flextable::valign(
  table_moderation_results,
  j = "Moderation_display",
  valign = "top",
  part = "body"
)


#-----------------------------------------------------------
# Borders
#-----------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

# Top line
table_moderation_results <- flextable::hline_top(
  table_moderation_results,
  border = outer_border,
  part = "header"
)

# Below header
table_moderation_results <- flextable::hline_bottom(
  table_moderation_results,
  border = outer_border,
  part = "header"
)

# Between moderation blocks
table_moderation_results <- flextable::hline(
  table_moderation_results,
  i = c(3, 6),
  border = inner_border,
  part = "body"
)

# Bottom line
table_moderation_results <- flextable::hline_bottom(
  table_moderation_results,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "Moderation_display",
  width = 3.10
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "Temperature_measure",
  width = 2.60
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "AIC_without_display",
  width = 1.20
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "AIC_with_display",
  width = 1.10
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "Chi_square_display",
  width = 0.80
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "df_display",
  width = 0.60
)

table_moderation_results <- flextable::width(
  table_moderation_results,
  j = "p_display",
  width = 0.80
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_moderation_results <- flextable::padding(
  table_moderation_results,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_moderation_results <- flextable::line_spacing(
  table_moderation_results,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant rows
#-----------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_moderation_results <- flextable::bold(
    table_moderation_results,
    i = significant_rows,
    part = "body"
  )
}


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_moderation_results


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_moderation_results,
  path = "tables/Table_S14_moderation_comparisons.png",
  zoom = 3,
  expand = 10
)













############################################################
## Follow-up: Movement as moderator of Box × Interruption
## Average temperature
############################################################


#-----------------------------------------------------------
# Movement slopes for all Box × Interruption combinations
#-----------------------------------------------------------

movement_trends_av <- emmeans::emtrends(
  model_moderation_threeway_av,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

movement_trends_av


#-----------------------------------------------------------
# Test whether the Box × Interruption effect changes
# as movement increases
#-----------------------------------------------------------

movement_moderation_contrasts_av <- emmeans::contrast(
  movement_trends_av,
  interaction = c(
    "pairwise",
    "pairwise"
  ),
  adjust = "holm"
)


#-----------------------------------------------------------
# Display contrasts
#-----------------------------------------------------------

summary(
  movement_moderation_contrasts_av,
  infer = c(TRUE, TRUE)
)


############################################################
## Follow-up: Movement as moderator of Box × Interruption
## Minimum temperature
############################################################


#-----------------------------------------------------------
# Movement slopes for all Box × Interruption combinations
#-----------------------------------------------------------

movement_trends_min <- emmeans::emtrends(
  model_moderation_threeway_min,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

movement_trends_min


#-----------------------------------------------------------
# Test whether the Box × Interruption effect changes
# as movement increases
#-----------------------------------------------------------

movement_moderation_contrasts_min <- emmeans::contrast(
  movement_trends_min,
  interaction = c(
    "pairwise",
    "pairwise"
  ),
  adjust = "holm"
)


#-----------------------------------------------------------
# Display contrasts
#-----------------------------------------------------------

summary(
  movement_moderation_contrasts_min,
  infer = c(TRUE, TRUE)
)



############################################################
## Follow-up: Movement as moderator of Box × Interruption
## Maximum temperature
############################################################


#-----------------------------------------------------------
# Movement slopes for all Box × Interruption combinations
#-----------------------------------------------------------

movement_trends_max <- emmeans::emtrends(
  model_moderation_threeway_max,
  ~ box_condition * interruption_group,
  var = "high_proportion_c",
  lmer.df = "satterthwaite"
)

movement_trends_max


#-----------------------------------------------------------
# Test whether the Box × Interruption effect changes
# as movement increases
#-----------------------------------------------------------

movement_moderation_contrasts_max <- emmeans::contrast(
  movement_trends_max,
  interaction = c(
    "pairwise",
    "pairwise"
  ),
  adjust = "holm"
)


#-----------------------------------------------------------
# Display contrasts
#-----------------------------------------------------------

summary(
  movement_moderation_contrasts_max,
  infer = c(TRUE, TRUE)
)

############################################################
## Summary of three-way moderation follow-up
############################################################

# Follow-up contrasts were used to examine whether movement moderated
# the Box condition × Interruption condition interaction.
#
# Across all three temperature outcomes, the movement-related change
# in the NC–JA difference differed significantly between
# No interruption and Accidental interruption:
#
# MAX: b = 24.44, 95% CI [11.34, 37.50], p < .001
# MIN: b = 25.20, 95% CI [10.51, 39.90], p < .001
# AV:  b = 24.27, 95% CI [8.93, 39.60], p < .001
#
# The movement-related change in the NC–JA difference also differed
# significantly between No interruption and Deliberate interruption:
#
# MAX: b = 21.51, 95% CI [5.76, 37.30], p = .003
# MIN: b = 20.35, 95% CI [3.45, 37.30], p = .009
# AV:  b = 18.32, 95% CI [0.52, 36.10], p = .028
#
# In contrast, Accidental and Deliberate interruption did not differ
# significantly in how movement changed the NC–JA difference:
#
# MAX: b = -2.93, 95% CI [-17.40, 11.50], p = .617
# MIN: b = -4.84, 95% CI [-20.79, 11.10], p = .454
# AV:  b = -5.95, 95% CI [-22.68, 10.80], p = .380
#
# Thus, movement moderated the Box condition × Interruption condition
# interaction consistently across maximum, minimum, and average
# temperature change. The moderation was primarily characterized by
# differences between the No-interruption condition and both
# interruption conditions, whereas Accidental and Deliberate
# interruption did not differ significantly from each other.
#
# Confidence intervals were Bonferroni-adjusted and p-values were
# Holm-adjusted for the three follow-up contrasts within each outcome.











############################################################
## Follow-up tables:
## Movement as moderator of Box × Interruption interaction
############################################################


#-----------------------------------------------------------
# Helper function:
# extract movement slopes + 95% CI + NC–JA comparison
#-----------------------------------------------------------

extract_movement_slopes <- function(
    trends,
    box_contrasts,
    outcome_name
) {
  
  #---------------------------------------------------------
  # 1. Extract movement slopes
  #---------------------------------------------------------
  
  trends_df <- as.data.frame(
    summary(
      trends,
      infer = c(TRUE, TRUE)
    )
  )
  
  # Automatically identify movement trend column
  trend_column <- grep(
    "\\.trend$",
    names(trends_df),
    value = TRUE
  )
  
  slopes_long <- trends_df |>
    dplyr::transmute(
      
      Outcome = outcome_name,
      
      Interruption =
        dplyr::recode(
          interruption_group,
          "none" = "No interruption",
          "a" = "Accidental interruption",
          "dp" = "Deliberate interruption"
        ),
      
      Box = box_condition,
      
      Movement_slope =
        .data[[trend_column]],
      
      Lower =
        lower.CL,
      
      Upper =
        upper.CL
    )
  
  
  #---------------------------------------------------------
  # 2. Convert slopes to wide format
  #---------------------------------------------------------
  
  slopes_wide <- slopes_long |>
    tidyr::pivot_wider(
      names_from = Box,
      values_from = c(
        Movement_slope,
        Lower,
        Upper
      ),
      names_sep = "_"
    ) |>
    dplyr::rename(
      
      Slope_NC = Movement_slope_NC,
      Lower_NC = Lower_NC,
      Upper_NC = Upper_NC,
      
      Slope_JA = Movement_slope_JA,
      Lower_JA = Lower_JA,
      Upper_JA = Upper_JA
    )
  
  
  #---------------------------------------------------------
  # 3. Extract NC–JA contrasts within interruption condition
  #---------------------------------------------------------
  
  contrast_df <- as.data.frame(
    summary(
      box_contrasts,
      infer = c(TRUE, TRUE)
    )
  ) |>
    dplyr::transmute(
      
      Interruption =
        dplyr::recode(
          interruption_group,
          "none" = "No interruption",
          "a" = "Accidental interruption",
          "dp" = "Deliberate interruption"
        ),
      
      Difference_NC_JA = estimate,
      
      Difference_SE = SE,
      
      Difference_df = df,
      
      Difference_lower = lower.CL,
      
      Difference_upper = upper.CL,
      
      p_value = p.value
    )
  
  
  #---------------------------------------------------------
  # 4. Merge slopes and contrasts
  #---------------------------------------------------------
  
  slopes_wide |>
    dplyr::left_join(
      contrast_df,
      by = "Interruption"
    )
}


#-----------------------------------------------------------
# Create NC–JA contrasts for all three outcomes
#-----------------------------------------------------------

movement_contrasts_max <- pairs(
  emmeans::emtrends(
    model_moderation_threeway_max,
    ~ box_condition | interruption_group,
    var = "high_proportion_c",
    lmer.df = "satterthwaite"
  ),
  adjust = "holm"
)

movement_contrasts_min <- pairs(
  emmeans::emtrends(
    model_moderation_threeway_min,
    ~ box_condition | interruption_group,
    var = "high_proportion_c",
    lmer.df = "satterthwaite"
  ),
  adjust = "holm"
)

movement_contrasts_av <- pairs(
  emmeans::emtrends(
    model_moderation_threeway_av,
    ~ box_condition | interruption_group,
    var = "high_proportion_c",
    lmer.df = "satterthwaite"
  ),
  adjust = "holm"
)


#-----------------------------------------------------------
# Combine movement slopes for all three outcomes
#-----------------------------------------------------------

movement_slopes_table_raw <- dplyr::bind_rows(
  
  extract_movement_slopes(
    movement_trends_max,
    movement_contrasts_max,
    "Maximum"
  ),
  
  extract_movement_slopes(
    movement_trends_min,
    movement_contrasts_min,
    "Minimum"
  ),
  
  extract_movement_slopes(
    movement_trends_av,
    movement_contrasts_av,
    "Average"
  )
)


############################################################
# Prepare presentation version
############################################################

library(flextable)
library(officer)

movement_slopes_display <- movement_slopes_table_raw |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum" = "Δ maximum temperature",
      "Minimum" = "Δ minimum temperature",
      "Average" = "Δ average temperature"
    ),
    
    NC_slope_CI = sprintf(
      "%.2f [%.2f, %.2f]",
      Slope_NC,
      Lower_NC,
      Upper_NC
    ),
    
    JA_slope_CI = sprintf(
      "%.2f [%.2f, %.2f]",
      Slope_JA,
      Lower_JA,
      Upper_JA
    ),
    
    Difference_display = sprintf(
      "%.2f",
      Difference_NC_JA
    ),
    
    p_numeric = p_value,
    
    p_display = dplyr::case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    # Show temperature label only in first row of each block
    Temperature_display = dplyr::case_when(
      dplyr::row_number() == 1 ~ "Δ maximum temperature",
      dplyr::row_number() == 4 ~ "Δ minimum temperature",
      dplyr::row_number() == 7 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )


#-----------------------------------------------------------
# Identify significant rows
#-----------------------------------------------------------

significant_rows <- which(
  !is.na(movement_slopes_display$p_numeric) &
    movement_slopes_display$p_numeric < .05
)


#-----------------------------------------------------------
# Keep only display columns
#-----------------------------------------------------------

movement_slopes_display_ft <- movement_slopes_display |>
  dplyr::select(
    Temperature_display,
    Interruption,
    NC_slope_CI,
    JA_slope_CI,
    Difference_display,
    p_display
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_movement_slopes <- flextable::flextable(
  movement_slopes_display_ft
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_movement_slopes <- flextable::set_header_labels(
  table_movement_slopes,
  Temperature_display = "Temperature measure",
  Interruption = "Interruption condition",
  NC_slope_CI = "Non-Collaborative slope [95% CI]",
  JA_slope_CI = "Joint Action slope [95% CI]",
  Difference_display = "NC − JA difference",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_movement_slopes <- flextable::border_remove(
  table_movement_slopes
)

table_movement_slopes <- flextable::font(
  table_movement_slopes,
  fontname = "Arial",
  part = "all"
)

table_movement_slopes <- flextable::fontsize(
  table_movement_slopes,
  size = 14,
  part = "all"
)

table_movement_slopes <- flextable::bold(
  table_movement_slopes,
  part = "header"
)

table_movement_slopes <- flextable::line_spacing(
  table_movement_slopes,
  space = 1.25,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Text columns left-aligned
table_movement_slopes <- flextable::align(
  table_movement_slopes,
  j = c(
    "Temperature_display",
    "Interruption"
  ),
  align = "left",
  part = "body"
)

# Numeric/result columns centered
table_movement_slopes <- flextable::align(
  table_movement_slopes,
  j = c(
    "NC_slope_CI",
    "JA_slope_CI",
    "Difference_display",
    "p_display"
  ),
  align = "center",
  part = "body"
)

# Headers centered
table_movement_slopes <- flextable::align(
  table_movement_slopes,
  align = "center",
  part = "header"
)


#-----------------------------------------------------------
# Borders
#-----------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

table_movement_slopes <- flextable::hline_top(
  table_movement_slopes,
  border = outer_border,
  part = "header"
)

table_movement_slopes <- flextable::hline_bottom(
  table_movement_slopes,
  border = outer_border,
  part = "header"
)

# Separators between Maximum / Minimum / Average
table_movement_slopes <- flextable::hline(
  table_movement_slopes,
  i = c(3, 6),
  border = inner_border,
  part = "body"
)

table_movement_slopes <- flextable::hline_bottom(
  table_movement_slopes,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "Temperature_display",
  width = 2.60
)

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "Interruption",
  width = 2.35
)

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "NC_slope_CI",
  width = 2.30
)

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "JA_slope_CI",
  width = 2.15
)

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "Difference_display",
  width = 1.55
)

table_movement_slopes <- flextable::width(
  table_movement_slopes,
  j = "p_display",
  width = 0.85
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_movement_slopes <- flextable::padding(
  table_movement_slopes,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_movement_slopes <- flextable::line_spacing(
  table_movement_slopes,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant rows
#-----------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_movement_slopes <- flextable::bold(
    table_movement_slopes,
    i = significant_rows,
    part = "body"
  )
}


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_movement_slopes


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_movement_slopes,
  path = "tables/Table_S15_movement_slopes.png",
  zoom = 3,
  expand = 10
)
























############################################################
## Contrasts of NC − JA movement-slope differences
############################################################


#-----------------------------------------------------------
# Helper function: extract contrasts
#-----------------------------------------------------------

extract_movement_contrasts <- function(
    contrasts,
    outcome_name
) {
  
  as.data.frame(
    summary(
      contrasts,
      infer = c(TRUE, TRUE)
    )
  ) |>
    
    dplyr::transmute(
      
      Outcome =
        outcome_name,
      
      Comparison =
        dplyr::recode(
          interruption_group_pairwise,
          
          "none - a" =
            "No interruption vs Accidental interruption",
          
          "none - dp" =
            "No interruption vs Deliberate interruption",
          
          "a - dp" =
            "Accidental interrupton vs Deliberate interruption"
        ),
      
      Estimate =
        estimate,
      
      SE =
        SE,
      
      df =
        df,
      
      CI_low =
        lower.CL,
      
      CI_high =
        upper.CL,
      
      p_value =
        p.value
    )
}


#-----------------------------------------------------------
# Combine contrasts for all three outcomes
#-----------------------------------------------------------

movement_contrasts_table_raw <- dplyr::bind_rows(
  
  extract_movement_contrasts(
    movement_moderation_contrasts_max,
    "Maximum"
  ),
  
  extract_movement_contrasts(
    movement_moderation_contrasts_min,
    "Minimum"
  ),
  
  extract_movement_contrasts(
    movement_moderation_contrasts_av,
    "Average"
  )
)


############################################################
# Prepare presentation version
############################################################

library(flextable)
library(officer)

movement_contrasts_display <- movement_contrasts_table_raw |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum" = "Δ maximum temperature",
      "Minimum" = "Δ minimum temperature",
      "Average" = "Δ average temperature"
    ),
    
    Difference_display = sprintf(
      "%.2f",
      Estimate
    ),
    
    SE_display = sprintf(
      "%.2f",
      SE
    ),
    
    df_display = sprintf(
      "%.1f",
      df
    ),
    
    CI_display = sprintf(
      "[%.2f, %.2f]",
      CI_low,
      CI_high
    ),
    
    p_numeric = p_value,
    
    p_display = dplyr::case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    Temperature_display = dplyr::case_when(
      dplyr::row_number() == 1 ~ "Δ maximum temperature",
      dplyr::row_number() == 4 ~ "Δ minimum temperature",
      dplyr::row_number() == 7 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )


#-----------------------------------------------------------
# Identify significant rows
#-----------------------------------------------------------

significant_rows <- which(
  !is.na(movement_contrasts_display$p_numeric) &
    movement_contrasts_display$p_numeric < .05
)


#-----------------------------------------------------------
# Keep only display columns
#-----------------------------------------------------------

movement_contrasts_display_ft <- movement_contrasts_display |>
  dplyr::select(
    Temperature_display,
    Comparison,
    Difference_display,
    SE_display,
    df_display,
    CI_display,
    p_display
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_movement_contrasts <- flextable::flextable(
  movement_contrasts_display_ft
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_movement_contrasts <- flextable::set_header_labels(
  table_movement_contrasts,
  Temperature_display = "Temperature measure",
  Comparison = "Interruption comparison",
  Difference_display = "Difference",
  SE_display = "SE",
  df_display = "df",
  CI_display = "95% CI",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_movement_contrasts <- flextable::border_remove(
  table_movement_contrasts
)

table_movement_contrasts <- flextable::font(
  table_movement_contrasts,
  fontname = "Arial",
  part = "all"
)

table_movement_contrasts <- flextable::fontsize(
  table_movement_contrasts,
  size = 14,
  part = "all"
)

table_movement_contrasts <- flextable::bold(
  table_movement_contrasts,
  part = "header"
)

# Slightly more space in multi-line headers if needed
table_movement_contrasts <- flextable::line_spacing(
  table_movement_contrasts,
  space = 1.25,
  part = "header"
)

table_movement_contrasts <- flextable::padding(
  table_movement_contrasts,
  padding.top = 5,
  padding.bottom = 5,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

table_movement_contrasts <- flextable::align(
  table_movement_contrasts,
  j = c(
    "Temperature_display",
    "Comparison"
  ),
  align = "left",
  part = "body"
)

table_movement_contrasts <- flextable::align(
  table_movement_contrasts,
  j = c(
    "Difference_display",
    "SE_display",
    "df_display",
    "CI_display",
    "p_display"
  ),
  align = "center",
  part = "body"
)

table_movement_contrasts <- flextable::align(
  table_movement_contrasts,
  align = "center",
  part = "header"
)


#-----------------------------------------------------------
# Borders
#-----------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

table_movement_contrasts <- flextable::hline_top(
  table_movement_contrasts,
  border = outer_border,
  part = "header"
)

table_movement_contrasts <- flextable::hline_bottom(
  table_movement_contrasts,
  border = outer_border,
  part = "header"
)

table_movement_contrasts <- flextable::hline(
  table_movement_contrasts,
  i = c(3, 6),
  border = inner_border,
  part = "body"
)

table_movement_contrasts <- flextable::hline_bottom(
  table_movement_contrasts,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "Temperature_display",
  width = 2.60
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "Comparison",
  width = 4.20
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "Difference_display",
  width = 1.15
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "SE_display",
  width = 0.75
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "df_display",
  width = 0.75
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "CI_display",
  width = 1.65
)

table_movement_contrasts <- flextable::width(
  table_movement_contrasts,
  j = "p_display",
  width = 0.80
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_movement_contrasts <- flextable::padding(
  table_movement_contrasts,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_movement_contrasts <- flextable::line_spacing(
  table_movement_contrasts,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant rows
#-----------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_movement_contrasts <- flextable::bold(
    table_movement_contrasts,
    i = significant_rows,
    part = "body"
  )
}


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_movement_contrasts


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_movement_contrasts,
  path = "tables/Table_S16_movement_contrasts.png",
  zoom = 3,
  expand = 10
)

# Results / interpretation:
# Follow-up analyses of the three-way moderation showed a consistent
# pattern across maximum, minimum, and average temperature change.
#
# In the no-interruption condition, the positive association between
# high movement proportion and temperature change was substantially
# stronger in the Non-Collaborative (NC) condition than in the
# Joint Action (JA) condition. The corresponding NC-JA movement-slope
# differences were 25.53 for maximum, 26.43 for minimum, and 25.53
# for average temperature change.
#
# These NC-JA movement-slope differences were significantly larger
# in the no-interruption condition than in the accidental-interruption
# condition for maximum (difference = 24.44, p < .001), minimum
# (difference = 25.20, p < .001), and average temperature change
# (difference = 24.27, p < .001).
#
# Likewise, the NC-JA movement-slope differences were significantly
# larger in the no-interruption condition than in the deliberate-
# interruption condition for maximum (difference = 21.51, p = .003),
# minimum (difference = 20.35, p = .009), and average temperature
# change (difference = 18.32, p = .028).
#
# In contrast, the movement-related NC-JA differences did not differ
# significantly between accidental and deliberate interruption for
# maximum (p = .617), minimum (p = .454), or average temperature
# change (p = .380).
#
# Thus, the moderation pattern was primarily driven by the
# no-interruption condition: the relationship between movement and
# temperature differed strongly between NC and JA when no interruption
# occurred, whereas this NC-JA difference was substantially reduced
# under both accidental and deliberate interruption.









############################################################
## Save moderation tables
############################################################


#-----------------------------------------------------------
# Save moderation results table
#-----------------------------------------------------------

gt::gtsave(
  table_moderation_results,
  filename = "tables/06_moderation_results.docx"
)


#-----------------------------------------------------------
# Save movement slopes table
#-----------------------------------------------------------

gt::gtsave(
  table_movement_slopes,
  filename = "tables/07_movement_slopes.docx"
)


#-----------------------------------------------------------
# Save movement moderation contrast table
#-----------------------------------------------------------

gt::gtsave(
  table_movement_contrasts,
  filename = "tables/08_movement_moderation_contrasts.docx"
)





























############################################################
#### Leave-one-child-out sensitivity analysis
############################################################

#-----------------------------------------------------------
# Helper function
#-----------------------------------------------------------

run_loo_threeway <- function(
    data,
    outcome,
    outcome_label
) {
  
  children <- unique(
    data$child_id
  )
  
  
  loo_results <- lapply(
    children,
    function(child_to_remove) {
      
      #------------------------------------------------------
      # Remove one child
      #------------------------------------------------------
      
      dat_loo <- data %>%
        dplyr::filter(
          child_id != child_to_remove
        ) %>%
        droplevels()
      
      
      #------------------------------------------------------
      # Fit full three-way model
      #------------------------------------------------------
      
      full_formula <- stats::as.formula(
        paste0(
          outcome,
          " ~ ",
          "box_condition * ",
          "interruption_group * ",
          "high_proportion_c + ",
          "(1 | child_id) + ",
          "(1 | dyad_id)"
        )
      )
      
      model_threeway_loo <- lme4::lmer(
        full_formula,
        data = dat_loo,
        REML = FALSE
      )
      
      
      #------------------------------------------------------
      # Remove only the three-way interaction
      #------------------------------------------------------
      
      model_no_threeway_loo <- update(
        model_threeway_loo,
        . ~ . -
          box_condition:
          interruption_group:
          high_proportion_c
      )
      
      
      #------------------------------------------------------
      # Likelihood-ratio test
      #------------------------------------------------------
      
      logLik_without <- logLik(
        model_no_threeway_loo
      )
      
      logLik_with <- logLik(
        model_threeway_loo
      )
      
      chi_square <- 2 * (
        as.numeric(logLik_with) -
          as.numeric(logLik_without)
      )
      
      df_difference <-
        attr(logLik_with, "df") -
        attr(logLik_without, "df")
      
      p_value <- stats::pchisq(
        chi_square,
        df = df_difference,
        lower.tail = FALSE
      )
      
      
      #------------------------------------------------------
      # Store results
      #------------------------------------------------------
      
      data.frame(
        Outcome =
          outcome_label,
        
        omitted_child =
          as.character(child_to_remove),
        
        Chi_square =
          chi_square,
        
        df =
          df_difference,
        
        p_value =
          p_value,
        
        AIC_without_threeway =
          AIC(model_no_threeway_loo),
        
        AIC_with_threeway =
          AIC(model_threeway_loo),
        
        singular_without =
          lme4::isSingular(
            model_no_threeway_loo
          ),
        
        singular_threeway =
          lme4::isSingular(
            model_threeway_loo
          )
      )
    }
  )
  
  
  dplyr::bind_rows(
    loo_results
  )
}


#-----------------------------------------------------------
# Average temperature
#-----------------------------------------------------------

loo_threeway_av <- run_loo_threeway(
  data = analysis_data,
  outcome = "delta_av_temp",
  outcome_label = "Average"
)


#-----------------------------------------------------------
# Minimum temperature
#-----------------------------------------------------------

loo_threeway_min <- run_loo_threeway(
  data = analysis_data,
  outcome = "delta_min_temp",
  outcome_label = "Minimum"
)


#-----------------------------------------------------------
# Maximum temperature
#-----------------------------------------------------------

loo_threeway_max <- run_loo_threeway(
  data = analysis_data,
  outcome = "delta_max_temp",
  outcome_label = "Maximum"
)


#-----------------------------------------------------------
# Combine all three outcomes
#-----------------------------------------------------------

loo_threeway_all <- dplyr::bind_rows(
  loo_threeway_av,
  loo_threeway_min,
  loo_threeway_max
)


#-----------------------------------------------------------
# Create rounded display version
#-----------------------------------------------------------

loo_threeway_all_display <- loo_threeway_all %>%
  dplyr::mutate(
    
    Chi_square = round(
      Chi_square,
      3
    ),
    
    p_value = round(
      p_value,
      4
    ),
    
    AIC_without_threeway = round(
      AIC_without_threeway,
      2
    ),
    
    AIC_with_threeway = round(
      AIC_with_threeway,
      2
    )
  )


#-----------------------------------------------------------
# Display results
#-----------------------------------------------------------

tibble::as_tibble(
  loo_threeway_all_display
)

loo_threeway_all_display %>%
  dplyr::filter(Outcome == "Minimum")


loo_threeway_all_display %>%
  dplyr::filter(Outcome == "Maximum")



#-----------------------------------------------------------
# Save results
#-----------------------------------------------------------

dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)

utils::write.csv(
  loo_threeway_all_display,
  file = "output/loo_threeway_sensitivity.csv",
  row.names = FALSE
)


library(lme4)
library(lmerTest)
library(dplyr)
library(purrr)
library(tidyr)
library(gt)

# ------------------------------------------------------------
# Leave-one-child-out sensitivity analysis
# ------------------------------------------------------------

# Function for one temperature outcome
run_loocv_threeway <- function(data, outcome, outcome_label) {
  
  child_ids <- unique(data$child_id)
  
  map_dfr(child_ids, function(id) {
    
    # Remove one child
    dat_sub <- data %>%
      filter(child_id != id)
    
    # Full three-way model
    formula_full <- as.formula(
      paste0(
        outcome,
        " ~ box_condition * interruption_group * high_proportion_c + ",
        "(1 | child_id) + (1 | dyad_id)"
      )
    )
    
    model_full <- lmer(
      formula_full,
      data = dat_sub,
      REML = FALSE
    )
    
    # Same model without the three-way interaction
    model_no_threeway <- update(
      model_full,
      . ~ . - box_condition:interruption_group:high_proportion_c
    )
    
    # Likelihood-ratio comparison
    comparison <- anova(
      model_no_threeway,
      model_full
    )
    
    tibble(
      Outcome = outcome_label,
      Excluded_child = as.character(id),
      
      AIC_without = AIC(model_no_threeway),
      AIC_with = AIC(model_full),
      
      Chisq = comparison$Chisq[2],
      df = comparison$Df[2],
      p = comparison$`Pr(>Chisq)`[2],
      
      Singular = performance::check_singularity(model_full)
    )
  })
}


# ------------------------------------------------------------
# Run for all three temperature outcomes
# ------------------------------------------------------------

loocv_results <- bind_rows(
  
  run_loocv_threeway(
    analysis_data,
    outcome = "delta_max_temp",
    outcome_label = "Maximum"
  ),
  
  run_loocv_threeway(
    analysis_data,
    outcome = "delta_min_temp",
    outcome_label = "Minimum"
  ),
  
  run_loocv_threeway(
    analysis_data,
    outcome = "delta_av_temp",
    outcome_label = "Average"
  )
)


# View raw results
loocv_results

# ------------------------------------------------------------
# Prepare presentation version
# ------------------------------------------------------------

library(flextable)
library(officer)

loocv_display <- loocv_results %>%
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum" = "Δ maximum temperature",
      "Minimum" = "Δ minimum temperature",
      "Average" = "Δ average temperature"
    ),
    
    AIC_without_display = sprintf(
      "%.2f",
      AIC_without
    ),
    
    AIC_with_display = sprintf(
      "%.2f",
      AIC_with
    ),
    
    Chisq_display = sprintf(
      "%.2f",
      Chisq
    ),
    
    df_display = sprintf(
      "%.0f",
      df
    ),
    
    p_numeric = p,
    
    p_display = dplyr::case_when(
      p < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p)
    ),
    
    Singular_display = ifelse(
      Singular,
      "Yes",
      "No"
    ),
    
    Temperature_display = dplyr::case_when(
      dplyr::row_number() == 1 ~ "Δ maximum temperature",
      dplyr::row_number() == 9 ~ "Δ minimum temperature",
      dplyr::row_number() == 17 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )


# ------------------------------------------------------------
# Identify significant rows
# ------------------------------------------------------------

significant_rows <- which(
  !is.na(loocv_display$p_numeric) &
    loocv_display$p_numeric < .05
)


# ------------------------------------------------------------
# Keep only display columns
# ------------------------------------------------------------

loocv_display_ft <- loocv_display %>%
  dplyr::select(
    Temperature_display,
    Excluded_child,
    AIC_without_display,
    AIC_with_display,
    Chisq_display,
    df_display,
    p_display,
    Singular_display
  )


# ------------------------------------------------------------
# Create flextable
# ------------------------------------------------------------

table_loocv <- flextable::flextable(
  loocv_display_ft
)


# ------------------------------------------------------------
# Header labels
# ------------------------------------------------------------

table_loocv <- flextable::set_header_labels(
  table_loocv,
  Temperature_display = "Temperature measure",
  Excluded_child = "Excluded child",
  AIC_without_display = "AIC without",
  AIC_with_display = "AIC with",
  Chisq_display = "\u03C7\u00B2",
  df_display = "df",
  p_display = "p",
  Singular_display = "Singular"
)


# ------------------------------------------------------------
# Basic formatting
# ------------------------------------------------------------

table_loocv <- flextable::border_remove(
  table_loocv
)

table_loocv <- flextable::font(
  table_loocv,
  fontname = "Arial",
  part = "all"
)

table_loocv <- flextable::fontsize(
  table_loocv,
  size = 14,
  part = "all"
)

table_loocv <- flextable::bold(
  table_loocv,
  part = "header"
)

table_loocv <- flextable::line_spacing(
  table_loocv,
  space = 1.25,
  part = "header"
)

table_loocv <- flextable::padding(
  table_loocv,
  padding.top = 5,
  padding.bottom = 5,
  part = "header"
)


# ------------------------------------------------------------
# Alignment
# ------------------------------------------------------------

table_loocv <- flextable::align(
  table_loocv,
  j = "Temperature_display",
  align = "left",
  part = "body"
)

table_loocv <- flextable::align(
  table_loocv,
  j = "Excluded_child",
  align = "center",
  part = "body"
)

table_loocv <- flextable::align(
  table_loocv,
  j = c(
    "AIC_without_display",
    "AIC_with_display",
    "Chisq_display",
    "df_display",
    "p_display",
    "Singular_display"
  ),
  align = "center",
  part = "body"
)

table_loocv <- flextable::align(
  table_loocv,
  align = "center",
  part = "header"
)


# ------------------------------------------------------------
# Borders
# ------------------------------------------------------------

outer_border <- officer::fp_border(
  color = "black",
  width = 1.2
)

inner_border <- officer::fp_border(
  color = "black",
  width = 0.6
)

table_loocv <- flextable::hline_top(
  table_loocv,
  border = outer_border,
  part = "header"
)

table_loocv <- flextable::hline_bottom(
  table_loocv,
  border = outer_border,
  part = "header"
)

# Separators after Maximum and Minimum blocks
table_loocv <- flextable::hline(
  table_loocv,
  i = c(8, 16),
  border = inner_border,
  part = "body"
)

table_loocv <- flextable::hline_bottom(
  table_loocv,
  border = outer_border,
  part = "body"
)


# ------------------------------------------------------------
# Column widths
# ------------------------------------------------------------

table_loocv <- flextable::width(
  table_loocv,
  j = "Temperature_display",
  width = 2.65
)

table_loocv <- flextable::width(
  table_loocv,
  j = "Excluded_child",
  width = 1.45
)

table_loocv <- flextable::width(
  table_loocv,
  j = "AIC_without_display",
  width = 1.40
)

table_loocv <- flextable::width(
  table_loocv,
  j = "AIC_with_display",
  width = 1.30
)

table_loocv <- flextable::width(
  table_loocv,
  j = "Chisq_display",
  width = 0.95
)

table_loocv <- flextable::width(
  table_loocv,
  j = "df_display",
  width = 0.65
)

table_loocv <- flextable::width(
  table_loocv,
  j = "p_display",
  width = 0.90
)

table_loocv <- flextable::width(
  table_loocv,
  j = "Singular_display",
  width = 1.05
)


# ------------------------------------------------------------
# Spacing
# ------------------------------------------------------------

table_loocv <- flextable::padding(
  table_loocv,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_loocv <- flextable::line_spacing(
  table_loocv,
  space = 1.15,
  part = "body"
)


# ------------------------------------------------------------
# Bold significant rows
# ------------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_loocv <- flextable::bold(
    table_loocv,
    i = significant_rows,
    part = "body"
  )
}


# ------------------------------------------------------------
# Display table
# ------------------------------------------------------------

table_loocv


# ------------------------------------------------------------
# Export PNG
# ------------------------------------------------------------

flextable::save_as_image(
  x = table_loocv,
  path = "tables/Table_S17_leave_one_child_out.png",
  zoom = 3,
  expand = 10
)




















############################################################
#### Figure 2: Three-way moderation plot
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(viridisLite)


#-----------------------------------------------------------
# Labels
#-----------------------------------------------------------

interruption_labels <- c(
  "none" = "No interruption",
  "a"    = "Accidental interruption",
  "dp"   = "Deliberate interruption"
)

box_labels <- c(
  "NC" = "Non-Collaborative",
  "JA" = "Joint Action"
)


#-----------------------------------------------------------
# Mean used to center high movement proportion
#-----------------------------------------------------------

movement_mean <- mean(
  analysis_data$high_proportion,
  na.rm = TRUE
)


#-----------------------------------------------------------
# Determine observed movement ranges
# for every Box × Interruption combination
#-----------------------------------------------------------

movement_ranges <- analysis_data %>%
  dplyr::group_by(
    interruption_group,
    box_condition
  ) %>%
  dplyr::summarise(
    min_high = min(
      high_proportion,
      na.rm = TRUE
    ),
    max_high = max(
      high_proportion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


#-----------------------------------------------------------
# Determine common observed NC–JA range
# within each interruption condition
#-----------------------------------------------------------

movement_overlap <- movement_ranges %>%
  dplyr::group_by(
    interruption_group
  ) %>%
  dplyr::summarise(
    
    overlap_min = max(
      min_high
    ),
    
    overlap_max = min(
      max_high
    ),
    
    .groups = "drop"
  )


# Display ranges for documentation
movement_overlap


#-----------------------------------------------------------
# Helper function:
# Generate predictions only within observed overlap
#-----------------------------------------------------------

create_threeway_predictions <- function(
    model,
    outcome_label,
    data = analysis_data,
    n_points = 80
) {
  
  prediction_list <- lapply(
    seq_len(
      nrow(movement_overlap)
    ),
    function(i) {
      
      current_interruption <-
        as.character(
          movement_overlap$interruption_group[i]
        )
      
      
      # Original high_proportion values
      movement_original <- seq(
        from =
          movement_overlap$overlap_min[i],
        to =
          movement_overlap$overlap_max[i],
        length.out =
          n_points
      )
      
      
      # Convert to centered scale used by model
      movement_centered <-
        movement_original -
        movement_mean
      
      
      # Estimated marginal means across movement values
      emm <- emmeans::emmeans(
        model,
        specs =
          ~ box_condition *
          interruption_group *
          high_proportion_c,
        at = list(
          high_proportion_c =
            movement_centered,
          interruption_group =
            current_interruption
        ),
        lmer.df =
          "satterthwaite"
      )
      
      # Extract estimates and confidence intervals
      emm_df <- as.data.frame(
        summary(
          emm,
          infer = c(
            TRUE,
            FALSE
          )
        )
      )
      
      
      emm_df %>%
        dplyr::mutate(
          
          Outcome =
            outcome_label,
          
          interruption_group =
            current_interruption,
          
          # Return x-axis to original scale
          high_proportion =
            high_proportion_c +
            movement_mean
        )
    }
  )
  
  
  dplyr::bind_rows(
    prediction_list
  )
}


#-----------------------------------------------------------
# Predictions: Average temperature
#-----------------------------------------------------------

pred_threeway_av <-
  create_threeway_predictions(
    model =
      model_moderation_threeway_av,
    outcome_label =
      "Average Δ temperature"
  )


#-----------------------------------------------------------
# Predictions: Minimum temperature
#-----------------------------------------------------------

pred_threeway_min <-
  create_threeway_predictions(
    model =
      model_moderation_threeway_min,
    outcome_label =
      "Minimum Δ temperature"
  )


#-----------------------------------------------------------
# Predictions: Maximum temperature
#-----------------------------------------------------------

pred_threeway_max <-
  create_threeway_predictions(
    model =
      model_moderation_threeway_max,
    outcome_label =
      "Maximum Δ temperature"
  )


#-----------------------------------------------------------
# Combine predictions
#-----------------------------------------------------------

threeway_predictions <- dplyr::bind_rows(
  pred_threeway_av,
  pred_threeway_min,
  pred_threeway_max
)


#-----------------------------------------------------------
# Prepare raw observed data
#-----------------------------------------------------------

threeway_raw_data <- analysis_data %>%
  
  dplyr::select(
    high_proportion,
    box_condition,
    interruption_group,
    delta_av_temp,
    delta_min_temp,
    delta_max_temp
  ) %>%
  
  tidyr::pivot_longer(
    
    cols = c(
      delta_av_temp,
      delta_min_temp,
      delta_max_temp
    ),
    
    names_to =
      "Outcome",
    
    values_to =
      "temperature_change"
  ) %>%
  
  dplyr::mutate(
    
    Outcome = dplyr::recode(
      Outcome,
      
      "delta_av_temp" =
        "Average Δ temperature",
      
      "delta_min_temp" =
        "Minimum Δ temperature",
      
      "delta_max_temp" =
        "Maximum Δ temperature"
    )
  )


#-----------------------------------------------------------
# Set facet order
#-----------------------------------------------------------

outcome_order <- c(
  "Maximum Δ temperature",
  "Minimum Δ temperature",
  "Average Δ temperature"
)

interruption_order <- c(
  "none",
  "a",
  "dp"
)


threeway_predictions <-
  threeway_predictions %>%
  dplyr::mutate(
    
    Outcome = factor(
      Outcome,
      levels = outcome_order
    ),
    
    interruption_group = factor(
      interruption_group,
      levels = interruption_order
    )
  )


threeway_raw_data <-
  threeway_raw_data %>%
  dplyr::mutate(
    
    Outcome = factor(
      Outcome,
      levels = outcome_order
    ),
    
    interruption_group = factor(
      interruption_group,
      levels = interruption_order
    )
  )


#-----------------------------------------------------------
# Create Figure 2
#-----------------------------------------------------------

figure_2_threeway <- ggplot2::ggplot() +
  
  
  # Zero = no temperature change
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey55"
  ) +
  
  
  # Observed raw data
  ggplot2::geom_point(
    data =
      threeway_raw_data,
    
    ggplot2::aes(
      x =
        high_proportion,
      y =
        temperature_change,
      colour =
        box_condition,
      shape =
        box_condition
    ),
    
    size = 2,
    alpha = 0.55
  ) +
  
  
  # 95% confidence intervals
  ggplot2::geom_ribbon(
    data =
      threeway_predictions,
    
    ggplot2::aes(
      x =
        high_proportion,
      ymin =
        lower.CL,
      ymax =
        upper.CL,
      fill =
        box_condition,
      group =
        box_condition
    ),
    
    alpha = 0.13,
    colour = NA,
    show.legend = FALSE
  ) +
  
  
  # Model-predicted slopes
  ggplot2::geom_line(
    data =
      threeway_predictions,
    
    ggplot2::aes(
      x =
        high_proportion,
      y =
        emmean,
      colour =
        box_condition,
      linetype =
        box_condition,
      group =
        box_condition
    ),
    
    linewidth = 1.05
  ) +
  
  
  # 3 outcomes × 3 interruption conditions
  ggplot2::facet_grid(
    
    rows =
      ggplot2::vars(
        Outcome
      ),
    
    cols =
      ggplot2::vars(
        interruption_group
      ),
    
    labeller =
      ggplot2::labeller(
        interruption_group =
          interruption_labels
      )
  ) +
  
  
  # Colours
  ggplot2::scale_colour_viridis_d(
    option = "D",
    begin = 0.20,
    end = 0.80,
    labels = box_labels,
    name = "Box condition"
  ) +
  
  
  ggplot2::scale_fill_viridis_d(
    option = "D",
    begin = 0.20,
    end = 0.80,
    labels = box_labels,
    name = "Box condition"
  ) +
  
  
  # Additional visual distinction
  ggplot2::scale_linetype_manual(
    values = c(
      "NC" = "solid",
      "JA" = "dashed"
    ),
    labels = box_labels,
    name = "Box condition"
  ) +
  
  
  ggplot2::scale_shape_manual(
    values = c(
      "NC" = 16,
      "JA" = 17
    ),
    labels = box_labels,
    name = "Box condition"
  ) +
  
  
  # Axis labels
  ggplot2::labs(
    x =
      "High movement proportion",
    y =
      "Δ temperature (°C)"
  ) +
  
  
  # Theme
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  
  
  ggplot2::theme(
    
    axis.title =
      ggplot2::element_text(
        face = "bold",
        size = 11
      ),
    
    axis.text =
      ggplot2::element_text(
        colour = "black",
        size = 9.5
      ),
    
    strip.text =
      ggplot2::element_text(
        face = "bold",
        size = 10.5
      ),
    
    strip.background =
      ggplot2::element_rect(
        fill = "grey95",
        colour = NA
      ),
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    panel.grid.major.x =
      ggplot2::element_blank(),
    
    panel.grid.major.y =
      ggplot2::element_line(
        linewidth = 0.35,
        colour = "grey90"
      ),
    
    legend.position =
      "bottom",
    
    legend.title =
      ggplot2::element_text(
        face = "bold"
      ),
    
    panel.spacing =
      grid::unit(
        1.4,
        "lines"
      ),
    
    panel.border =
      ggplot2::element_rect(
        colour = "grey75",
        fill = NA,
        linewidth = 0.5
      )
  )


#-----------------------------------------------------------
# Display Figure 2
#-----------------------------------------------------------

figure_2_threeway


#-----------------------------------------------------------
# Save Figure 2
#-----------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)


ggplot2::ggsave(
  filename =
    "figures/figure_2_threeway_moderation.png",
  plot =
    figure_2_threeway,
  width =
    12,
  height =
    9,
  units =
    "in",
  dpi =
    300
)


# Optional vector version
ggplot2::ggsave(
  filename =
    "figures/figure_2_threeway_moderation.pdf",
  plot =
    figure_2_threeway,
  width =
    12,
  height =
    9,
  units =
    "in"
)




















































############################################################
#### Appendix: 4-panel DHARMa diagnostic figures
############################################################

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)


#-----------------------------------------------------------
# Helper function for 4-panel DHARMa diagnostics
#-----------------------------------------------------------

save_dharma_4panel <- function(simulation_object,
                               filename,
                               title) {
  
  png(
    filename = filename,
    width = 2600,
    height = 2200,
    res = 300
  )
  
  old_par <- par(
    mfrow = c(2, 2),
    mar = c(4.5, 4.5, 3.5, 1.5),
    oma = c(0, 0, 3, 0)
  )
  
  # 1. QQ plot / uniformity
  DHARMa::plotQQunif(
    simulation_object
  )
  
  # 2. Residuals vs. predicted
  DHARMa::plotResiduals(
    simulation_object
  )
  
  # 3. Dispersion test
  DHARMa::testDispersion(
    simulation_object,
    plot = TRUE
  )
  
  # 4. Outlier test
  DHARMa::testOutliers(
    simulation_object,
    plot = TRUE
  )
  
  # Overall figure title
  mtext(
    title,
    outer = TRUE,
    side = 3,
    line = 1,
    font = 2,
    cex = 1.3
  )
  
  par(old_par)
  dev.off()
}






############################################################
#### Average temperature
############################################################

save_dharma_4panel(
  simulation_object = simulation_base_interaction_av,
  filename = "figures/dharma_primary_average.png",
  title = "Primary interaction model – Average temperature change"
)


############################################################
#### Minimum temperature
############################################################

save_dharma_4panel(
  simulation_object = simulation_base_interaction_min,
  filename = "figures/dharma_primary_minimum.png",
  title = "Primary interaction model – Minimum temperature change"
)


############################################################
#### Maximum temperature
############################################################

save_dharma_4panel(
  simulation_object = simulation_base_interaction_max,
  filename = "figures/dharma_primary_maximum.png",
  title = "Primary interaction model – Maximum temperature change"
)
