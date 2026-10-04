###############################################################
# Bachelor Thesis
# Script: 08_model_inference.R
# Author: Dorin Bez
# Created: 10.08.2026
# Purpose: Model inference and robustness analyses for
#          maximum, average, and minimum facial temperature
###############################################################

###############################################################
### 1. Load packages
###############################################################

library(lme4)
library(lmerTest)
library(permutes)
library(emmeans)
library(performance)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)


###############################################################
### Reproducibility settings
###############################################################

# Seed based on the analysis date: 10 August 2026
SEED <- 20260810


###############################################################
### 2. Load final models
###############################################################

# Base models
model_base_max <- readRDS("output/model_base_max.rds")
model_base_av  <- readRDS("output/model_base_av.rds")
model_base_min <- readRDS("output/model_base_min.rds")

# Interaction models
model_base_interaction_max <- readRDS("output/model_base_interaction_max.rds")
model_base_interaction_av  <- readRDS("output/model_base_interaction_av.rds")
model_base_interaction_min <- readRDS("output/model_base_interaction_min.rds")


# Interaction models
model_reduced_interaction_max <- readRDS("output/model_reduced_interaction_max.rds")
model_reduced_interaction_av  <- readRDS("output/model_reduced_interaction_av.rds")
model_reduced_interaction_min <- readRDS("output/model_reduced_interaction_min.rds")


###############################################################
### 3. Check loaded models
###############################################################

summary(model_base_max)
summary(model_base_av)
summary(model_base_min)








###############################################################
# 4. Maximum temperature
###############################################################

###############################################################
### 4.3 Base model MAX
###############################################################

############################################################
# 4.3.1 Bootstrap
############################################################

# Number of parametric bootstrap samples
N_BOOT <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Perform parametric bootstrap for all fixed-effect coefficients
boot_base_max <- lme4::bootMer(
  x = model_base_max,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)


# ----------------------------------------------------------
# Bootstrap diagnostics
# ----------------------------------------------------------

# Check whether any bootstrap model fits failed
attr(boot_base_max, "bootFail")

# Display messages associated with failed bootstrap fits
attr(boot_base_max, "boot.fail.msgs")

# Check all warnings and messages generated during bootstrapping
attr(boot_base_max, "boot.all.msgs")


# Inspect variance components of the original fitted model
lme4::VarCorr(model_base_max)

# Check singularity of the original fitted model
performance::check_singularity(model_base_max)

# Bootstrap diagnostics
# The original base model was non-singular.
# All 2000 bootstrap refits were completed successfully.
# However, 1500 of 2000 bootstrap refits resulted in a singular fit,
# indicating instability in the estimation of the random-effect
# variance components across simulated datasets.
# One additional convergence warning occurred.
#
# Therefore, bootstrap confidence intervals are calculated below,
# but should be interpreted cautiously.


# ----------------------------------------------------------
# Calculate bootstrap confidence intervals
# ----------------------------------------------------------

# Calculate 95% percentile bootstrap confidence intervals
boot_ci_base_max <- t(
  apply(
    boot_base_max$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

# Add coefficient names
rownames(boot_ci_base_max) <- names(lme4::fixef(model_base_max))

# Display bootstrap confidence intervals
boot_ci_base_max

# Bootstrap results
# The 95% percentile bootstrap confidence intervals for all substantive
# fixed-effect coefficients included zero.
# Thus, none of the predictors showed a clearly non-zero effect based
# on the bootstrap confidence intervals.
#
# The confidence interval for box_condition was very close to excluding zero
# (-0.016 to 1.138), indicating a possible positive trend.
#
# The confidence interval for high_proportion was comparatively wide
# (-0.434 to 4.987), indicating substantial uncertainty around the
# estimated positive association with maximum facial temperature change.
#
# Results should be interpreted cautiously because 1500 of 2000
# bootstrap refits resulted in singular random-effects solutions.


############################################################
### 4.3.2 Permutation tests
############################################################

# Use exactly the observations included in the fitted base MAX model
data_perm_base_max <- model.frame(model_base_max)

# Check reference categories
levels(data_perm_base_max$interruption_group)
levels(data_perm_base_max$box_condition)

# Number of permutations
N_PERM <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Run coefficient-level permutation tests
perm_base_max <- permutes::perm.lmer(
  formula = formula(model_base_max),
  data = data_perm_base_max,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)

# Display permutation results
perm_base_max

# Permutation results
# Neither accidental nor deliberate interruption was significantly
# associated with maximum facial temperature change
# (accidental: p = .204; deliberate: p = .131).

# Box condition showed a significant positive effect
# (p = .032).

# High movement proportion also showed a significant positive effect
# based on the permutation test (p = .033).


############################################################
### 4.3.3 Estimated marginal means
############################################################

# ----------------------------------------------------------
# Estimated marginal means for interruption group
# ----------------------------------------------------------

emm_interruption_base_max <- emmeans::emmeans(
  model_base_max,
  specs = ~ interruption_group,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interruption_base_max,
  infer = c(TRUE, FALSE)
)

# Pairwise comparisons between interruption groups
pairs_interruption_base_max <- pairs(
  emm_interruption_base_max,
  adjust = "holm"
)

pairs_interruption_base_max

# Results:
# Estimated marginal means were similar across interruption conditions.
# None of the pairwise comparisons between interruption groups was significant
# after Holm correction (all p = 1.000).


# ----------------------------------------------------------
# Estimated marginal means for box condition
# ----------------------------------------------------------

emm_box_base_max <- emmeans::emmeans(
  model_base_max,
  specs = ~ box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_box_base_max,
  infer = c(TRUE, FALSE)
)

# Pairwise comparison between box conditions
pairs_box_base_max <- pairs(
  emm_box_base_max,
  adjust = "holm"
)

pairs_box_base_max

# Results:
# The estimated marginal mean was higher in the JA condition than in the NC condition.
# The pairwise comparison showed a positive trend but did not reach significance
# using the Satterthwaite-based test (NC - JA = -0.560, p = .073).

# ----------------------------------------------------------
# Estimated trend for high movement proportion
# ----------------------------------------------------------

trend_high_base_max <- emmeans::emtrends(
  model_base_max,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope and 95% confidence interval
summary(
  trend_high_base_max,
  infer = c(TRUE, TRUE)
)

# Results:
# High movement proportion showed a positive estimated association with
# maximum facial temperature change (slope = 2.26).
# However, the conventional 95% confidence interval included zero
# (95% CI [-0.54, 5.06]) and the Satterthwaite-based test was not significant
# (p = .111).


# Note:
# The p-values reported by emmeans/emtrends are based on conventional
# Satterthwaite approximations and are presented as supplementary model-based
# inference. Permutation p-values are used as the primary significance
# criterion for the fixed-effect coefficients.


############################################################
## 4.3.4 R-squared
############################################################

# Calculate marginal and conditional R-squared
# Marginal R2 represents variance explained by the fixed effects only.
# Conditional R2 represents variance explained by both fixed and random effects.
r2_base_max <- performance::r2_nakagawa(
  model_base_max
)

# Display R-squared results
r2_base_max

# Results:
# The fixed effects explained approximately 11.9% of the variance
# in maximum facial temperature change (marginal R2 = 0.119).
# Including the child- and dyad-level random effects increased the
# explained variance to approximately 27.0% (conditional R2 = 0.270).
# Thus, a substantial additional proportion of variance was associated
# with between-child and between-dyad differences.


############################################################
### 4.3.5 Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates from the fitted model
coefficient_base_max <- tibble::enframe(
  lme4::fixef(model_base_max),
  name = "term",
  value = "estimate"
)

# Display coefficient table
coefficient_base_max


# Convert bootstrap confidence intervals into a tidy data frame
boot_ci_df_base_max <- tibble::tibble(
  term = rownames(boot_ci_base_max),
  conf.low = boot_ci_base_max[, 1],
  conf.high = boot_ci_base_max[, 2]
)

# Display bootstrap confidence intervals
boot_ci_df_base_max


# Convert permutation results into a tidy data frame
perm_df_base_max <- as.data.frame(perm_base_max) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

# Display permutation results
perm_df_base_max


# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
forest_df_base_max <- coefficient_base_max |>
  dplyr::left_join(
    boot_ci_df_base_max,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_max,
    by = "term"
  )

# Display combined results
forest_df_base_max


############################################################
### 4.3.6 Forest plot
############################################################

# Prepare data for the forest plot
# Points represent the original model estimates.
# Horizontal lines represent the 95% bootstrap confidence intervals.
# Statistical significance is based on permutation p-values.
forest_plot_df_base_max <- forest_df_base_max |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    # Create readable labels for all fixed effects
    term_label = dplyr::case_when(
      term == "interruption_groupa" ~
        "Accidental interruption vs none",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none",
      
      term == "box_conditionJA" ~
        "JA vs NC",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      TRUE ~ term
    ),
    
    # Determine statistical significance from permutation p-values
    significant = !is.na(p_perm) & p_perm < 0.05,
    
    # Classify effects according to direction and significance
    significance_group = dplyr::case_when(
      significant & estimate < 0 ~ "Negative significant",
      significant & estimate > 0 ~ "Positive significant",
      TRUE ~ "Not significant"
    ),
    
    # Add an asterisk to statistically significant effects
    star = dplyr::if_else(significant, "*", "")
  )

# Display prepared forest plot data
forest_plot_df_base_max



# Define the order of predictors in the forest plot
term_order_base_max <- c(
  "Accidental interruption vs none",
  "Deliberate interruption vs none",
  "JA vs NC",
  "High movement proportion"
)

# Apply the predefined predictor order
forest_plot_df_base_max <- forest_plot_df_base_max |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_base_max)
    )
  )



# Define colours according to effect direction and significance
forest_pal_base_max <- c(
  "Negative significant" = "#3B4CC0",
  "Positive significant" = "#B40426",
  "Not significant" = "#D9A441"
)

# Create forest plot
p_forest_base_max <- ggplot2::ggplot(
  forest_plot_df_base_max,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  
  # Add vertical reference line representing no effect
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.6
  ) +
  
  # Add 95% bootstrap confidence intervals
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  
  # Add original fixed-effect estimates
  ggplot2::geom_point(
    size = 3.5
  ) +
  
  # Mark significant permutation-test results with an asterisk
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_max,
      significant
    ),
    ggplot2::aes(label = star),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Apply colours for significance groups
  ggplot2::scale_colour_manual(
    values = forest_pal_base_max
  ) +
  
  # Add plot labels
  ggplot2::labs(
    title = "Base model: maximum facial temperature change",
    subtitle = "95% bootstrap CIs; significance based on permutation tests",
    x = "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  # Apply a clean plotting theme
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  )

# Display forest plot
p_forest_base_max



# Save forest plot as high-resolution PNG
ggplot2::ggsave(
  filename = "figures/forest_plot_base_max.png",
  plot = p_forest_base_max,
  width = 10,
  height = 5.5,
  dpi = 300
)












############################################################
# 5. Average temperature
############################################################

############################################################
### 5.3 Base model AV
############################################################

############################################################
### 5.3.1 Bootstrap
############################################################

# Number of parametric bootstrap samples
N_BOOT <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Perform parametric bootstrap for all fixed-effect coefficients
boot_base_av <- lme4::bootMer(
  x = model_base_av,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)


# ----------------------------------------------------------
# Bootstrap diagnostics
# ----------------------------------------------------------

# Check whether any bootstrap model fits failed
attr(boot_base_av, "bootFail")

# Display messages associated with failed bootstrap fits
attr(boot_base_av, "boot.fail.msgs")

# Check all warnings and messages generated during bootstrapping
attr(boot_base_av, "boot.all.msgs")

# Inspect variance components of the original fitted model
lme4::VarCorr(model_base_av)

# Check singularity of the original fitted model
performance::check_singularity(model_base_av)

# Bootstrap diagnostics:
# The original base average-temperature model was non-singular.
# All 2000 bootstrap refits were completed successfully.
# However, 1389 bootstrap refits resulted in a singular fit,
# indicating instability in the estimation of the random-effect
# variance components across simulated datasets.
# Six additional convergence warnings occurred.


# ----------------------------------------------------------
# Calculate bootstrap confidence intervals
# ----------------------------------------------------------

# Calculate 95% percentile bootstrap confidence intervals
boot_ci_base_av <- t(
  apply(
    boot_base_av$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

# Add coefficient names
rownames(boot_ci_base_av) <- names(lme4::fixef(model_base_av))

# Display bootstrap confidence intervals
boot_ci_base_av

# Bootstrap results:
# All 95% percentile bootstrap confidence intervals for the
# fixed-effect predictors included zero.
# Therefore, none of the fixed-effect coefficients showed clear
# evidence of a non-zero effect based on the bootstrap confidence intervals.
# Interpretation should remain cautious because a substantial proportion
# of bootstrap refits resulted in singular random-effect solutions.


############################################################
### 5.3.2 Permutation tests
############################################################

# Use exactly the observations included in the fitted base AV model
data_perm_base_av <- model.frame(model_base_av)

# Check reference categories
levels(data_perm_base_av$interruption_group)
levels(data_perm_base_av$box_condition)

# Number of permutations
N_PERM <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Run coefficient-level permutation tests
perm_base_av <- permutes::perm.lmer(
  formula = formula(model_base_av),
  data = data_perm_base_av,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)

# Display permutation results
perm_base_av

# Results:
# Neither accidental nor deliberate interruption differed significantly
# from the reference condition based on the permutation tests
# (p = .187 and p = .308, respectively).
#
# Box condition did not reach statistical significance
# (p = .084).
#
# High movement proportion showed a significant positive association
# with average facial temperature change
# (permutation p = .018).
#
# Note that the corresponding 95% bootstrap confidence interval
# for high_proportion included zero, indicating uncertainty in the
# magnitude of the estimated effect.


############################################################
### 5.3.3 Estimated marginal means
############################################################

# Estimated marginal means for interruption group
emm_interruption_base_av <- emmeans::emmeans(
  model_base_av,
  specs = ~ interruption_group,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interruption_base_av,
  infer = c(TRUE, FALSE)
)

# Pairwise comparisons between interruption groups
pairs_interruption_base_av <- pairs(
  emm_interruption_base_av,
  adjust = "holm"
)

pairs_interruption_base_av


# Estimated marginal means for box condition
emm_box_base_av <- emmeans::emmeans(
  model_base_av,
  specs = ~ box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_box_base_av,
  infer = c(TRUE, FALSE)
)

# Pairwise comparison between box conditions
pairs_box_base_av <- pairs(
  emm_box_base_av,
  adjust = "holm"
)

pairs_box_base_av


# Estimate the slope of high_proportion while controlling
# for all other predictors in the base model
trend_high_base_av <- emmeans::emtrends(
  model_base_av,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope and 95% confidence interval
summary(
  trend_high_base_av,
  infer = c(TRUE, TRUE)
)

# Results:
# Estimated marginal means showed no statistically significant
# difference between the two box conditions.
# The estimated contrast between NC and JA was -0.458
# (p = .175).

# High movement proportion showed a positive estimated association
# with average facial temperature change (slope = 2.72).
# However, the conventional 95% confidence interval included zero
# (95% CI [-0.36, 5.81]) and the Satterthwaite-based test
# was not statistically significant (p = .082).

# Note:
# The emmeans/emtrends p-values are conventional Satterthwaite-based
# approximations and are treated as supplementary inference.
# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
### 5.3.4 R-squared
############################################################

# Calculate marginal and conditional R-squared
# Marginal R2 represents variance explained by the fixed effects only.
# Conditional R2 represents variance explained by both fixed and random effects.
r2_base_av <- performance::r2_nakagawa(
  model_base_av
)

# Display R-squared results
r2_base_av

# Results:
# The fixed effects explained approximately 9.5% of the variance
# in average facial temperature change (marginal R2 = .095).
#
# The combination of fixed and random effects explained approximately
# 30.0% of the variance (conditional R2 = .300).
#
# Thus, a substantial part of the explained variance was associated
# with the random-effects structure.


############################################################
### 5.3.5 Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates from the fitted model
coefficient_base_av <- tibble::enframe(
  lme4::fixef(model_base_av),
  name = "term",
  value = "estimate"
)

# Display coefficient table
coefficient_base_av


# Convert bootstrap confidence intervals into a tidy data frame
boot_ci_df_base_av <- tibble::tibble(
  term = rownames(boot_ci_base_av),
  conf.low = boot_ci_base_av[, 1],
  conf.high = boot_ci_base_av[, 2]
)

# Display bootstrap confidence intervals
boot_ci_df_base_av


# Convert permutation results into a tidy data frame
perm_df_base_av <- as.data.frame(perm_base_av) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

# Display permutation results
perm_df_base_av


# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
forest_df_base_av <- coefficient_base_av |>
  dplyr::left_join(
    boot_ci_df_base_av,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_av,
    by = "term"
  )

# Display combined results
forest_df_base_av



# Prepare data for the forest plot
# Points represent the original model estimates.
# Horizontal lines represent the 95% bootstrap confidence intervals.
# Statistical significance is based on permutation p-values.
forest_plot_df_base_av <- forest_df_base_av |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    # Create readable labels for all fixed effects
    term_label = dplyr::case_when(
      term == "interruption_groupa" ~
        "Accidental interruption vs none",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none",
      
      term == "box_conditionJA" ~
        "JA vs NC",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      TRUE ~ term
    ),
    
    # Determine statistical significance from permutation p-values
    significant = !is.na(p_perm) & p_perm < 0.05,
    
    # Classify effects according to direction and significance
    significance_group = dplyr::case_when(
      significant & estimate < 0 ~ "Negative significant",
      significant & estimate > 0 ~ "Positive significant",
      TRUE ~ "Not significant"
    ),
    
    # Add an asterisk to statistically significant effects
    star = dplyr::if_else(significant, "*", "")
  )

# Display prepared forest plot data
forest_plot_df_base_av



# Define the order of predictors in the forest plot
term_order_base_av <- c(
  "Accidental interruption vs none",
  "Deliberate interruption vs none",
  "JA vs NC",
  "High movement proportion"
)

# Apply the predefined predictor order
forest_plot_df_base_av <- forest_plot_df_base_av |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_base_av)
    )
  )

# Results:
# The original fixed-effect estimates, 95% bootstrap confidence intervals,
# and permutation p-values were successfully combined for visualization.

# Based on the permutation tests, high movement proportion was classified
# as a significant positive effect (estimate = 2.72, p = .018).

# Accidental interruption, deliberate interruption, and box condition
# were classified as not statistically significant.

# The 95% bootstrap confidence intervals included zero for all four
# fixed-effect coefficients, including high movement proportion.


############################################################
### 5.3.6 Forest plot
############################################################

# Create forest plot
p_forest_base_av <- ggplot2::ggplot(
  forest_plot_df_base_av,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  
  # Add vertical reference line at zero
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  # Add 95% bootstrap confidence intervals
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  
  # Add original fixed-effect estimates
  ggplot2::geom_point(
    size = 3.5
  ) +
  
  # Mark significant permutation-test results with an asterisk
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_av,
      significant
    ),
    ggplot2::aes(label = star),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Apply the same colours as in the previous forest plots
  ggplot2::scale_colour_manual(
    values = c(
      "Negative significant" = "#3B4CC0",
      "Positive significant" = "#B40426",
      "Not significant" = "#D9A441"
    )
  ) +
  
  # Add plot labels
  ggplot2::labs(
    title = "Base model: average facial temperature change",
    subtitle = "95% bootstrap CIs; significance based on permutation tests",
    x = "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  # Apply a clean plotting theme
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  )

# Display forest plot
p_forest_base_av



# Save forest plot as high-resolution PNG
ggplot2::ggsave(
  filename = "figures/forest_plot_base_av.png",
  plot = p_forest_base_av,
  width = 10,
  height = 5.5,
  dpi = 300
)








############################################################
# 6. Minimum temperature
############################################################


############################################################
### 6.3 Base model MIN
############################################################


############################################################
### 6.3.1 Bootstrap
############################################################

# Number of parametric bootstrap samples
N_BOOT <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Perform parametric bootstrap for all fixed-effect coefficients
boot_base_min <- lme4::bootMer(
  x = model_base_min,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)

# Check whether any bootstrap model fits failed
attr(boot_base_min, "bootFail")

# Display messages associated with failed bootstrap fits
attr(boot_base_min, "boot.fail.msgs")

# Check all warnings and messages generated during bootstrapping
attr(boot_base_min, "boot.all.msgs")


# ----------------------------------------------------------
# Bootstrap diagnostics
# ----------------------------------------------------------

# Inspect variance components of the original fitted model
lme4::VarCorr(model_base_min)

# Check singularity of the original fitted model
performance::check_singularity(model_base_min)


# ----------------------------------------------------------
# Calculate bootstrap confidence intervals
# ----------------------------------------------------------

# Calculate 95% percentile bootstrap confidence intervals
boot_ci_base_min <- t(
  apply(
    boot_base_min$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

# Add coefficient names
rownames(boot_ci_base_min) <- names(lme4::fixef(model_base_min))

# Display bootstrap confidence intervals
boot_ci_base_min

# Results:
# All 2000 bootstrap iterations were completed without failed model fits.

# The original fitted base model was non-singular.
# However, 1299 of the 2000 bootstrap refits resulted in singular
# random-effects solutions, indicating some instability in the
# estimation of the random-effects variance components.

# None of the fixed-effect bootstrap confidence intervals clearly
# excluded zero.

# Interpretation should therefore remain cautious.
# Permutation tests are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
### 6.3.2 Permutation tests
############################################################

# Use exactly the observations included in the fitted base MIN model
data_perm_base_min <- model.frame(model_base_min)

# Check reference categories
levels(data_perm_base_min$interruption_group)
levels(data_perm_base_min$box_condition)

# Number of permutations
N_PERM <- 2000L

# Set seed for reproducibility
set.seed(SEED)

# Run coefficient-level permutation tests
perm_base_min <- permutes::perm.lmer(
  formula = formula(model_base_min),
  data = data_perm_base_min,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)

# Display permutation results
perm_base_min

# Results:
# Neither accidental nor deliberate interruption showed a significant
# association with minimum facial temperature change
# (accidental: permutation beta = 0.252, p = .287;
# deliberate: permutation beta = 0.240, p = .338).

# Box condition showed a positive estimated effect but did not reach
# the predefined significance threshold
# (permutation beta = 0.533, p = .053).

# High movement proportion showed a significant positive association
# with minimum facial temperature change
# (permutation beta = 2.689, p = .019).

# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
### 6.3.3 Estimated marginal means
############################################################

# ----------------------------------------------------------
# Interruption group
# ----------------------------------------------------------

# Calculate adjusted means for each interruption condition
emm_interruption_base_min <- emmeans::emmeans(
  model_base_min,
  specs = ~ interruption_group,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interruption_base_min,
  infer = c(TRUE, FALSE)
)

# Pairwise comparisons between interruption groups
pairs_interruption_base_min <- pairs(
  emm_interruption_base_min,
  adjust = "holm"
)

pairs_interruption_base_min


# ----------------------------------------------------------
# Box condition
# ----------------------------------------------------------

# Calculate adjusted means for box condition
emm_box_base_min <- emmeans::emmeans(
  model_base_min,
  specs = ~ box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_box_base_min,
  infer = c(TRUE, FALSE)
)

# Pairwise comparison between box conditions
pairs_box_base_min <- pairs(
  emm_box_base_min,
  adjust = "holm"
)

pairs_box_base_min


# ----------------------------------------------------------
# High movement proportion
# ----------------------------------------------------------

# Estimate the slope of high_proportion while controlling
# for all other predictors in the base model
trend_high_base_min <- emmeans::emtrends(
  model_base_min,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope and 95% confidence interval
summary(
  trend_high_base_min,
  infer = c(TRUE, TRUE)
)

# Results:
# Pairwise comparisons between interruption conditions showed no
# significant differences after Holm correction (all p = 1.000).

# The adjusted difference between NC and JA was negative
# (NC - JA = -0.533), but the conventional Satterthwaite-based
# comparison was not significant (p = .108).

# High movement proportion showed a positive estimated slope
# (slope = 2.68, 95% CI [-0.357, 5.71]),
# but the conventional Satterthwaite-based test was not significant
# (p = .082).

# Note:
# The p-values reported by emmeans/emtrends are conventional
# Satterthwaite-based approximations and are used as supplementary inference.
# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
### 6.3.4 R-squared
############################################################

# Calculate marginal and conditional R-squared
# Marginal R2 represents variance explained by the fixed effects only.
# Conditional R2 represents variance explained by both fixed and random effects.
r2_base_min <- performance::r2_nakagawa(
  model_base_min
)

# Display R-squared results
r2_base_min

# Results:
# The fixed effects explained approximately 9.8% of the variance
# in minimum facial temperature change (marginal R2 = 0.098).

# The combination of fixed and random effects explained approximately
# 33.3% of the variance (conditional R2 = 0.333).

# The difference between marginal and conditional R2 indicates that
# additional variance was accounted for by the random-effects structure.


############################################################
### 6.3.5 Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates from the fitted model
coefficient_base_min <- tibble::enframe(
  lme4::fixef(model_base_min),
  name = "term",
  value = "estimate"
)

# Display coefficient table
coefficient_base_min


# Convert bootstrap confidence intervals into a tidy data frame
boot_ci_df_base_min <- tibble::tibble(
  term = rownames(boot_ci_base_min),
  conf.low = boot_ci_base_min[, 1],
  conf.high = boot_ci_base_min[, 2]
)

# Display bootstrap confidence intervals
boot_ci_df_base_min


# Convert permutation results into a tidy data frame
perm_df_base_min <- as.data.frame(perm_base_min) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

# Display permutation results
perm_df_base_min


# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
forest_df_base_min <- coefficient_base_min |>
  dplyr::left_join(
    boot_ci_df_base_min,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_min,
    by = "term"
  )

# Display combined results
forest_df_base_min


# Prepare data for the forest plot
# Points represent the original model estimates.
# Horizontal lines represent the 95% bootstrap confidence intervals.
# Statistical significance is based on permutation p-values.

forest_plot_df_base_min <- forest_df_base_min |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    # Create readable labels for all fixed effects
    term_label = dplyr::case_when(
      term == "interruption_groupa" ~
        "Accidental interruption vs none",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none",
      
      term == "box_conditionJA" ~
        "JA vs NC",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      TRUE ~ term
    ),
    
    # Determine statistical significance from permutation p-values
    significant = !is.na(p_perm) & p_perm < 0.05,
    
    # Classify effects according to direction and significance
    significance_group = dplyr::case_when(
      significant & estimate < 0 ~ "Negative significant",
      significant & estimate > 0 ~ "Positive significant",
      TRUE ~ "Not significant"
    ),
    
    # Add an asterisk to statistically significant effects
    star = dplyr::if_else(significant, "*", "")
  )

# Display prepared forest plot data
forest_plot_df_base_min


# Define the order of predictors in the forest plot
term_order_base_min <- c(
  "Accidental interruption vs none",
  "Deliberate interruption vs none",
  "JA vs NC",
  "High movement proportion"
)

# Apply the predefined predictor order
forest_plot_df_base_min <- forest_plot_df_base_min |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_base_min)
    )
  )


############################################################
### 6.3.6 Forest plot
############################################################

# Define colours according to effect direction and significance
forest_pal_base_min <- c(
  "Negative significant" = "#3B4CC0",
  "Positive significant" = "#B40426",
  "Not significant" = "#D9A441"
)

# Create forest plot
p_forest_base_min <- ggplot2::ggplot(
  forest_plot_df_base_min,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  
  # Add vertical reference line representing no effect
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.6
  ) +
  
  # Add 95% bootstrap confidence intervals
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  
  # Add original fixed-effect estimates
  ggplot2::geom_point(
    size = 3.5
  ) +
  
  # Mark significant permutation-test results with an asterisk
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_min,
      significant
    ),
    ggplot2::aes(label = star),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Apply colours
  ggplot2::scale_colour_manual(
    values = forest_pal_base_min
  ) +
  
  # Add plot labels
  ggplot2::labs(
    title = "Base model: minimum facial temperature change",
    subtitle = "95% bootstrap CIs; significance based on permutation tests",
    x = "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  # Apply clean plotting theme
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  )

# Display forest plot
p_forest_base_min

# Save forest plot as high-resolution PNG
ggplot2::ggsave(
  filename = "figures/forest_plot_base_min.png",
  plot = p_forest_base_min,
  width = 10,
  height = 5.5,
  dpi = 300
)


















#########################
#######################
####################### Interaction Models
#####################
###################


############################################################
# 8.1 Average temperature interaction model
############################################################

############################################################
#### 8.1.1 Bootstrapping interaction models
############################################################

set.seed(SEED)

boot_base_interaction_av <- lme4::bootMer(
  model_base_interaction_av,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)

# Check bootstrap convergence
attr(boot_base_interaction_av, "bootFail")
attr(boot_base_interaction_av, "boot.fail.msgs")
attr(boot_base_interaction_av, "boot.all.msgs")

# Calculate 95% percentile bootstrap confidence intervals
boot_ci_base_interaction_av <- t(
  apply(
    boot_base_interaction_av$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

boot_ci_base_interaction_av

# Bootstrap results:
# All bootstrap iterations completed successfully (bootFail = 0).
# The 95% bootstrap CIs for all predictors, including both
# interruption_group × box_condition interaction terms, included zero.
# Thus, the bootstrap provided no clear evidence for any fixed effect
# or interaction effect on average temperature.
# Some bootstrap resamples produced singular-fit warnings, but none failed.


############################################################
#### 8.1.2 Permutation test
############################################################

# Use exactly the observations included in the fitted
# average-temperature interaction model

data_perm_base_interaction_av <- model.frame(
  model_base_interaction_av
)

#----------------------------------------------------------
# Check reference categories
#----------------------------------------------------------

levels(
  data_perm_base_interaction_av$interruption_group
)

levels(
  data_perm_base_interaction_av$box_condition
)

#----------------------------------------------------------
# Number of permutations
#----------------------------------------------------------

N_PERM <- 2000L


#----------------------------------------------------------
# Set seed for reproducibility
#----------------------------------------------------------

set.seed(SEED)


#----------------------------------------------------------
# Run coefficient-level permutation tests
#----------------------------------------------------------

perm_base_interaction_av <- permutes::perm.lmer(
  formula = formula(model_base_interaction_av),
  data = data_perm_base_interaction_av,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)


#----------------------------------------------------------
# Display permutation results
#----------------------------------------------------------

perm_base_interaction_av

# Permutation results:
#
# Coefficient-level permutation tests showed no significant effect
# of accidental interruption compared with no interruption
# (beta = 0.495, p = .072) or deliberate interruption compared
# with no interruption (beta = 0.460, p = .095) within the
# reference box condition (NC).
#
# Within the no-interruption reference group, JA was associated
# with a significantly higher average facial temperature change
# than NC (beta = 0.685, p = .017).
#
# High movement proportion showed a significant positive association
# with average facial temperature change
# (beta = 2.635, p = .023).
#
# Neither interaction coefficient was significant
# (accidental interruption x JA: beta = -0.321, p = .364;
# deliberate interruption x JA: beta = -0.376, p = .318).
#
# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
#### 8.1.3 Estimated marginal means
############################################################

# Calculate estimated marginal means for all combinations of
# interruption group and box condition
emm_interaction_av <- emmeans::emmeans(
  model_base_interaction_av,
  specs = ~ interruption_group * box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interaction_av,
  infer = c(TRUE, FALSE)
)


#----------------------------------------------------------
# Simple effects of interruption group within each box condition
#----------------------------------------------------------

# Compare interruption groups separately within NC and JA
pairs_interruption_by_box_av <- emmeans::contrast(
  emm_interaction_av,
  method = "pairwise",
  by = "box_condition",
  adjust = "holm"
)

pairs_interruption_by_box_av


#----------------------------------------------------------
# Simple effects of box condition within each interruption group
#----------------------------------------------------------

# Compare NC and JA separately within each interruption group
pairs_box_by_interruption_av <- emmeans::contrast(
  emm_interaction_av,
  method = "pairwise",
  by = "interruption_group",
  adjust = "holm"
)

pairs_box_by_interruption_av


#----------------------------------------------------------
# Overall interaction test
#----------------------------------------------------------

# Conventional omnibus test of the interaction
# This is supplementary to the permutation-based interaction test
joint_interaction_av <- emmeans::joint_tests(
  model_base_interaction_av
)

joint_interaction_av


#----------------------------------------------------------
# High movement proportion
#----------------------------------------------------------

# Estimate the slope of high_proportion while controlling
# for interruption group, box condition, and their interaction
trend_high_interaction_av <- emmeans::emtrends(
  model_base_interaction_av,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope, 95% confidence interval, and p-value
summary(
  trend_high_interaction_av,
  infer = c(TRUE, TRUE)
)

# EMMs results:
# Estimated marginal means and simple-effect comparisons showed no
# significant differences between interruption groups within either
# box condition (all Holm-adjusted p = 1.000).
# Likewise, NC and JA did not differ significantly within any
# interruption group (all p >= .231).
# The omnibus interaction test was non-significant
# (F = 0.108, p = .897), consistent with the permutation result.
# The estimated high_proportion slope was positive but non-significant
# (b = 2.66, 95% CI [-0.52, 5.83], p = .100).


############################################################
#### 8.1.4 R-squared
############################################################

# Calculate marginal and conditional R-squared
r2_base_interaction_av <- performance::r2_nakagawa(
  model_base_interaction_av
)

r2_base_interaction_av

# R-squared results:
# The interaction model explained 9.9% of the variance through the
# fixed effects alone (marginal R² = .099).
# Including the random effects increased the explained variance to
# 30.3% (conditional R² = .303).


############################################################
#### 8.1.5 Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates
coefficient_base_interaction_av <- tibble::enframe(
  lme4::fixef(model_base_interaction_av),
  name = "term",
  value = "estimate"
)

coefficient_base_interaction_av


# Convert bootstrap confidence intervals into a tidy data frame
boot_ci_df_base_interaction_av <- tibble::tibble(
  term = rownames(boot_ci_base_interaction_av),
  conf.low = boot_ci_base_interaction_av[, 1],
  conf.high = boot_ci_base_interaction_av[, 2]
)

boot_ci_df_base_interaction_av


# Convert permutation results into a tidy data frame
perm_df_base_interaction_av <- as.data.frame(
  perm_base_interaction_av
) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

perm_df_base_interaction_av


# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
forest_df_base_interaction_av <-
  coefficient_base_interaction_av |>
  dplyr::left_join(
    boot_ci_df_base_interaction_av,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_interaction_av,
    by = "term"
  )

forest_df_base_interaction_av


############################################################
#### 8.1.6. Forest plot
############################################################

# Prepare data for the forest plot
# Points represent the original model estimates.
# Horizontal lines represent the 95% bootstrap confidence intervals.
# Statistical significance is based on permutation p-values.

forest_plot_df_base_interaction_av <-
  forest_df_base_interaction_av |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    # Create readable labels for all fixed effects
    term_label = dplyr::case_when(
      term == "interruption_groupa" ~
        "Accidental interruption vs none (NC)",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none (NC)",
      
      term == "box_conditionJA" ~
        "JA vs NC (no interruption)",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      term == "interruption_groupa:box_conditionJA" ~
        "Accidental interruption × JA",
      
      term == "interruption_groupdp:box_conditionJA" ~
        "Deliberate interruption × JA",
      
      TRUE ~ term
    ),
    
    # Determine statistical significance from permutation p-values
    significant = !is.na(p_perm) & p_perm < 0.05,
    
    # Classify effects according to direction and significance
    significance_group = dplyr::case_when(
      significant & estimate < 0 ~ "Negative significant",
      significant & estimate > 0 ~ "Positive significant",
      TRUE ~ "Not significant"
    ),
    
    # Add an asterisk to statistically significant effects
    star = dplyr::if_else(significant, "*", "")
  )

# Display prepared forest plot data
forest_plot_df_base_interaction_av


#----------------------------------------------------------
# Define predictor order
#----------------------------------------------------------

term_order_base_interaction_av <- c(
  "Accidental interruption vs none (NC)",
  "Deliberate interruption vs none (NC)",
  "JA vs NC (no interruption)",
  "High movement proportion",
  "Accidental interruption × JA",
  "Deliberate interruption × JA"
)

forest_plot_df_base_interaction_av <-
  forest_plot_df_base_interaction_av |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(term_order_base_interaction_av)
    )
  )


#----------------------------------------------------------
# Create forest plot
#----------------------------------------------------------

forest_pal_base_interaction_av <- c(
  "Negative significant" = "#3B4CC0",
  "Positive significant" = "#B40426",
  "Not significant" = "#D9A441"
)

p_forest_base_interaction_av <- ggplot2::ggplot(
  forest_plot_df_base_interaction_av,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.6
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    size = 3.5
  ) +
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_interaction_av,
      significant
    ),
    ggplot2::aes(label = star),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = forest_pal_base_interaction_av
  ) +
  ggplot2::labs(
    title = "Interaction model: average facial temperature change",
    subtitle = "95% bootstrap CIs; significance based on permutation tests",
    x = "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  )

# Display forest plot
p_forest_base_interaction_av

# Save forest plot as high-resolution PNG
ggplot2::ggsave(
  filename = "figures/forest_plot_base_interaction_av.png",
  plot = p_forest_base_interaction_av,
  width = 10,
  height = 6.5,
  dpi = 300
)








############################################################
#### 8.2 Minimum temperature
############################################################

############################################################
#### 8.2.1 Bootstrapping
############################################################

set.seed(SEED)

boot_base_interaction_min <- lme4::bootMer(
  model_base_interaction_min,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)

# Check bootstrap convergence
attr(boot_base_interaction_min, "bootFail")
attr(boot_base_interaction_min, "boot.fail.msgs")
attr(boot_base_interaction_min, "boot.all.msgs")

# Calculate 95% percentile bootstrap confidence intervals
boot_ci_base_interaction_min <- t(
  apply(
    boot_base_interaction_min$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

boot_ci_base_interaction_min

# Bootstrap results:
# All 2000 bootstrap iterations completed successfully (bootFail = 0).
# Singular fits occurred in 1267 of 2000 bootstrap samples.
# Two additional convergence warnings occurred.
# The 95% bootstrap CIs for all predictors, including both
# interruption_group × box_condition interaction terms, included zero.
# Thus, the bootstrap provided no clear evidence for fixed or
# interaction effects on minimum temperature.


############################################################
#### 8.2.2 Permutation test
############################################################

# Use exactly the observations included in the fitted
# minimum-temperature interaction model
data_perm_base_interaction_min <- model.frame(
  model_base_interaction_min
)

#----------------------------------------------------------
# Check reference categories
#----------------------------------------------------------

levels(
  data_perm_base_interaction_min$interruption_group
)

levels(
  data_perm_base_interaction_min$box_condition
)

#----------------------------------------------------------
# Number of permutations
#----------------------------------------------------------

N_PERM <- 2000L

#----------------------------------------------------------
# Set seed for reproducibility
#----------------------------------------------------------

set.seed(SEED)

#----------------------------------------------------------
# Run coefficient-level permutation tests
#----------------------------------------------------------

perm_base_interaction_min <- permutes::perm.lmer(
  formula = formula(model_base_interaction_min),
  data = data_perm_base_interaction_min,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)

#----------------------------------------------------------
# Display permutation results
#----------------------------------------------------------

perm_base_interaction_min

# Permutation results:
#
# Coefficient-level permutation tests showed no significant effect
# of accidental interruption compared with no interruption
# (beta = 0.462, p = .081) or deliberate interruption compared
# with no interruption (beta = 0.381, p = .145) within the
# reference box condition (NC).
#
# Within the no-interruption reference group, JA was associated
# with a significantly higher minimum facial temperature change
# than NC (beta = 0.762, p = .007).
#
# High movement proportion showed a significant positive association
# with minimum facial temperature change
# (beta = 2.686, p = .018).
#
# Neither interaction coefficient was significant
# (accidental interruption x JA: beta = -0.419, p = .228;
# deliberate interruption x JA: beta = -0.274, p = .455).
#
# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
#### 8.2.3 Estimated marginal means
############################################################

# Calculate estimated marginal means for all combinations of
# interruption group and box condition
emm_interaction_min <- emmeans::emmeans(
  model_base_interaction_min,
  specs = ~ interruption_group * box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interaction_min,
  infer = c(TRUE, FALSE)
)

#----------------------------------------------------------
# Simple effects of interruption group within each box condition
#----------------------------------------------------------

# Compare interruption groups separately within NC and JA
pairs_interruption_by_box_min <- emmeans::contrast(
  emm_interaction_min,
  method = "pairwise",
  by = "box_condition",
  adjust = "holm"
)

pairs_interruption_by_box_min

#----------------------------------------------------------
# Simple effects of box condition within each interruption group
#----------------------------------------------------------

# Compare NC and JA separately within each interruption group
pairs_box_by_interruption_min <- emmeans::contrast(
  emm_interaction_min,
  method = "pairwise",
  by = "interruption_group",
  adjust = "holm"
)

pairs_box_by_interruption_min

#----------------------------------------------------------
# Overall interaction test
#----------------------------------------------------------

# Conventional omnibus test of the interaction
# This is supplementary to the permutation-based interaction test
joint_interaction_min <- emmeans::joint_tests(
  model_base_interaction_min
)

joint_interaction_min

#----------------------------------------------------------
# High movement proportion
#----------------------------------------------------------

# Estimate the slope of high_proportion while controlling
# for interruption group, box condition, and their interaction
trend_high_interaction_min <- emmeans::emtrends(
  model_base_interaction_min,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope, 95% confidence interval, and p-value
summary(
  trend_high_interaction_min,
  infer = c(TRUE, TRUE)
)

# EMMs results:
#
# Estimated marginal means and simple-effect comparisons showed no
# significant differences between interruption groups within either
# box condition (all Holm-adjusted p = 1.000).
#
# Likewise, NC and JA did not differ significantly within any
# interruption group (all p >= .174).
#
# The omnibus interaction test was non-significant
# (F = 0.129, p = .879), consistent with the permutation result
# (permutation p = .8901).
#
# The estimated high_proportion slope was positive but non-significant
# (b = 2.71, 95% CI [-0.42, 5.83], p = .088).


############################################################
#### 8.2.4 R-squared
############################################################

# Calculate marginal and conditional R-squared
# for the base interaction model
r2_base_interaction_min <- performance::r2_nakagawa(
  model_base_interaction_min
)

# Display results
r2_base_interaction_min

# R-squared results:
#
# The fixed effects explained approximately 10.2% of the variance
# in delta_min_temp (marginal R2 = 0.102).
#
# Including the random effects increased the explained variance
# to approximately 33.9% (conditional R2 = 0.339).
#
# Thus, a substantial proportion of the explained variance was
# attributable to between-child and/or between-dyad differences.


############################################################
#### 8.2.5. Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates
coefficient_base_interaction_min <- tibble::enframe(
  lme4::fixef(model_base_interaction_min),
  name = "term",
  value = "estimate"
)

coefficient_base_interaction_min


#----------------------------------------------------------
# Convert bootstrap confidence intervals into a tidy data frame
#----------------------------------------------------------

boot_ci_df_base_interaction_min <- tibble::tibble(
  term = rownames(boot_ci_base_interaction_min),
  conf.low = boot_ci_base_interaction_min[, 1],
  conf.high = boot_ci_base_interaction_min[, 2]
)

boot_ci_df_base_interaction_min


#----------------------------------------------------------
# Convert permutation results into a tidy data frame
#----------------------------------------------------------

perm_df_base_interaction_min <- as.data.frame(
  perm_base_interaction_min
) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

perm_df_base_interaction_min


#----------------------------------------------------------
# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
#----------------------------------------------------------

forest_df_base_interaction_min <-
  coefficient_base_interaction_min |>
  dplyr::left_join(
    boot_ci_df_base_interaction_min,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_interaction_min,
    by = "term"
  )

forest_df_base_interaction_min


############################################################
#### 8.2.6 Forest plot
############################################################

# Prepare data for the forest plot
# Points represent the original model estimates.
# Horizontal lines represent the 95% bootstrap confidence intervals.
# Statistical significance is based on permutation p-values.

forest_plot_df_base_interaction_min <-
  forest_df_base_interaction_min |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    # Create readable labels for all fixed effects
    
    term_label = dplyr::case_when(
      
      term == "interruption_groupa" ~
        "Accidental interruption vs none (NC)",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none (NC)",
      
      term == "box_conditionJA" ~
        "JA vs NC (no interruption)",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      term == "interruption_groupa:box_conditionJA" ~
        "Accidental interruption × JA",
      
      term == "interruption_groupdp:box_conditionJA" ~
        "Deliberate interruption × JA",
      
      TRUE ~ term
    ),
    
    # Determine statistical significance from permutation p-values
    
    significant =
      !is.na(p_perm) &
      p_perm < 0.05,
    
    # Classify effects according to direction and significance
    
    significance_group = dplyr::case_when(
      
      significant & estimate < 0 ~
        "Negative significant",
      
      significant & estimate > 0 ~
        "Positive significant",
      
      TRUE ~
        "Not significant"
    ),
    
    # Add an asterisk to statistically significant effects
    
    star = dplyr::if_else(
      significant,
      "*",
      ""
    )
  )

# Display prepared data

forest_plot_df_base_interaction_min


#----------------------------------------------------------
# Define predictor order
#----------------------------------------------------------

term_order_base_interaction_min <- c(
  "Accidental interruption vs none (NC)",
  "Deliberate interruption vs none (NC)",
  "JA vs NC (no interruption)",
  "High movement proportion",
  "Accidental interruption × JA",
  "Deliberate interruption × JA"
)

forest_plot_df_base_interaction_min <-
  forest_plot_df_base_interaction_min |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(
        term_order_base_interaction_min
      )
    )
  )


#----------------------------------------------------------
# Define colours
#----------------------------------------------------------

forest_pal_base_interaction_min <- c(
  "Negative significant" = "#3B4CC0",
  "Positive significant" = "#B40426",
  "Not significant" = "#D9A441"
)


#----------------------------------------------------------
# Create forest plot
#----------------------------------------------------------

p_forest_base_interaction_min <- ggplot2::ggplot(
  forest_plot_df_base_interaction_min,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  
  # Reference line for no effect
  
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.6
  ) +
  
  # 95% bootstrap confidence intervals
  
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  
  # Original fixed-effect estimates
  
  ggplot2::geom_point(
    size = 3.5
  ) +
  
  # Asterisks for significant permutation-test results
  
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_interaction_min,
      significant
    ),
    ggplot2::aes(
      label = star
    ),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Apply colours
  
  ggplot2::scale_colour_manual(
    values = forest_pal_base_interaction_min
  ) +
  
  # Plot labels
  
  ggplot2::labs(
    title =
      "Interaction model: minimum facial temperature change",
    subtitle =
      "95% bootstrap CIs; significance based on permutation tests",
    x =
      "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  # Theme
  
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor =
      ggplot2::element_blank()
  )


#----------------------------------------------------------
# Display forest plot
#----------------------------------------------------------

p_forest_base_interaction_min


#----------------------------------------------------------
# Save forest plot
#----------------------------------------------------------

ggplot2::ggsave(
  filename =
    "figures/forest_plot_base_interaction_min.png",
  plot =
    p_forest_base_interaction_min,
  width = 10,
  height = 6.5,
  dpi = 300
)










############################################################
#### 8.3 MAXIMUM TEMPERATURE
############################################################

############################################################
#### 8.3.1. Parametric bootstrap
############################################################

# Parametric bootstrap for fixed-effect estimates
# of the base interaction model
set.seed(SEED)

boot_base_interaction_max <- lme4::bootMer(
  model_base_interaction_max,
  FUN = function(fitted_model) {
    lme4::fixef(fitted_model)
  },
  nsim = N_BOOT,
  type = "parametric",
  use.u = FALSE,
  parallel = "no"
)

#----------------------------------------------------------
# Check bootstrap convergence
#----------------------------------------------------------

attr(
  boot_base_interaction_max,
  "bootFail"
)

attr(
  boot_base_interaction_max,
  "boot.fail.msgs"
)

attr(
  boot_base_interaction_max,
  "boot.all.msgs"
)

#----------------------------------------------------------
# Check random-effects structure before bootstrap
#----------------------------------------------------------

lme4::VarCorr(
  model_base_interaction_max
)

performance::check_singularity(
  model_base_interaction_max
)


#----------------------------------------------------------
# Calculate percentile-based 95% bootstrap confidence intervals
#----------------------------------------------------------

boot_ci_base_interaction_max <- t(
  apply(
    boot_base_interaction_max$t,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

# Add coefficient names
rownames(boot_ci_base_interaction_max) <-
  names(
    lme4::fixef(
      model_base_interaction_max
    )
  )

colnames(boot_ci_base_interaction_max) <-
  c(
    "CI_2.5",
    "CI_97.5"
  )

# Display bootstrap confidence intervals
boot_ci_base_interaction_max

# Bootstrap results:
#
# The parametric bootstrap completed successfully with no failed
# simulations (bootFail = 0).
#
# The observed interaction model itself was not singular.
# However, 1457 of 2000 bootstrap refits (72.9%) resulted in
# singular fits.
#
# The 95% percentile bootstrap confidence intervals for all
# predictors and interaction terms included zero.
#
# Therefore, the bootstrap results provided no robust evidence
# for effects of interruption_group, box_condition,
# high_proportion, or the interruption_group × box_condition
# interaction on delta_max_temp.


############################################################
#### 8.3.2. Permutation tests
############################################################

# Use exactly the observations included in the fitted
# maximum-temperature interaction model
data_perm_base_interaction_max <- model.frame(
  model_base_interaction_max
)

#----------------------------------------------------------
# Check reference categories
#----------------------------------------------------------

levels(
  data_perm_base_interaction_max$interruption_group
)

levels(
  data_perm_base_interaction_max$box_condition
)

#----------------------------------------------------------
# Number of permutations
#----------------------------------------------------------

N_PERM <- 2000L

#----------------------------------------------------------
# Set seed for reproducibility
#----------------------------------------------------------

set.seed(SEED)

#----------------------------------------------------------
# Run coefficient-level permutation tests
#----------------------------------------------------------

perm_base_interaction_max <- permutes::perm.lmer(
  formula = formula(model_base_interaction_max),
  data = data_perm_base_interaction_max,
  nperm = N_PERM,
  type = "regression",
  progress = TRUE
)

#----------------------------------------------------------
# Display permutation results
#----------------------------------------------------------

perm_base_interaction_max

# Permutation results:
#
# Coefficient-level permutation tests showed no significant effect
# of accidental interruption compared with no interruption
# (beta = 0.267, p = .249) or deliberate interruption compared
# with no interruption (beta = 0.415, p = .102) within the
# reference box condition (NC).
#
# Within the no-interruption reference group, JA was associated
# with a significantly higher maximum facial temperature change
# than NC (beta = 0.559, p = .037).
#
# High movement proportion showed a significant positive association
# with maximum facial temperature change
# (beta = 2.174, p = .039).
#
# Neither interaction coefficient was significant
# (accidental interruption x JA: beta = 0.061, p = .860;
# deliberate interruption x JA: beta = -0.066, p = .844).
#
# Permutation p-values are used as the primary significance criterion
# for the fixed-effect coefficients.


############################################################
#### 8.3.3. Estimated marginal means
############################################################

# Calculate estimated marginal means for all combinations of
# interruption group and box condition
emm_interaction_max <- emmeans::emmeans(
  model_base_interaction_max,
  specs = ~ interruption_group * box_condition,
  lmer.df = "satterthwaite"
)

# Display estimated marginal means and 95% confidence intervals
summary(
  emm_interaction_max,
  infer = c(TRUE, FALSE)
)

#----------------------------------------------------------
# Simple effects of interruption group within each box condition
#----------------------------------------------------------

# Compare interruption groups separately within NC and JA
pairs_interruption_by_box_max <- emmeans::contrast(
  emm_interaction_max,
  method = "pairwise",
  by = "box_condition",
  adjust = "holm"
)

pairs_interruption_by_box_max

#----------------------------------------------------------
# Simple effects of box condition within each interruption group
#----------------------------------------------------------

# Compare NC and JA separately within each interruption group
pairs_box_by_interruption_max <- emmeans::contrast(
  emm_interaction_max,
  method = "pairwise",
  by = "interruption_group",
  adjust = "holm"
)

pairs_box_by_interruption_max

#----------------------------------------------------------
# Overall interaction test
#----------------------------------------------------------

# Conventional omnibus test of the interaction
# This is supplementary to the permutation-based interaction test
joint_interaction_max <- emmeans::joint_tests(
  model_base_interaction_max
)

joint_interaction_max

#----------------------------------------------------------
# High movement proportion
#----------------------------------------------------------

# Estimate the slope of high_proportion while controlling
# for interruption group, box condition, and their interaction
trend_high_interaction_max <- emmeans::emtrends(
  model_base_interaction_max,
  specs = ~ 1,
  var = "high_proportion",
  lmer.df = "satterthwaite"
)

# Display estimated slope, 95% confidence interval, and p-value
summary(
  trend_high_interaction_max,
  infer = c(TRUE, TRUE)
)

# EMMs results:
#
# Estimated marginal means and simple-effect comparisons showed no
# significant differences between interruption groups within either
# box condition (all Holm-adjusted p = 1.000).
#
# Likewise, NC and JA did not differ significantly within any
# interruption group (all p >= .243).
#
# The omnibus interaction test was non-significant
# (F = 0.011, p = .989), consistent with the permutation result
# (permutation p = .988).
#
# The omnibus main effects of interruption_group
# (F = 0.466, p = .630) and box_condition
# (F = 2.906, p = .095) were also non-significant.
#
# The estimated high_proportion slope was positive but non-significant
# (b = 2.20, 95% CI [-0.69, 5.09], p = .133).



############################################################
#### 8.3.4. R-squared
############################################################

# Calculate marginal and conditional R-squared
# for the base interaction model
r2_base_interaction_max <- performance::r2_nakagawa(
  model_base_interaction_max
)

# Display results
r2_base_interaction_max

# R-squared results:
#
# The fixed effects explained approximately 11.9% of the variance
# in delta_max_temp (marginal R2 = 0.119).
#
# Including the random effects increased the explained variance
# to approximately 26.9% (conditional R2 = 0.269).
#
# Thus, additional variance was explained by the random-effects
# structure beyond the fixed effects alone.


############################################################
#### 8.3.5. Prepare results for forest plot
############################################################

# Extract original fixed-effect estimates
coefficient_base_interaction_max <- tibble::enframe(
  lme4::fixef(model_base_interaction_max),
  name = "term",
  value = "estimate"
)

coefficient_base_interaction_max


#----------------------------------------------------------
# Convert bootstrap confidence intervals into a tidy data frame
#----------------------------------------------------------

boot_ci_df_base_interaction_max <- tibble::tibble(
  term = rownames(boot_ci_base_interaction_max),
  conf.low = boot_ci_base_interaction_max[, 1],
  conf.high = boot_ci_base_interaction_max[, 2]
)

boot_ci_df_base_interaction_max


#----------------------------------------------------------
# Convert permutation results into a tidy data frame
#----------------------------------------------------------

perm_df_base_interaction_max <- as.data.frame(
  perm_base_interaction_max
) |>
  dplyr::transmute(
    term = as.character(Factor),
    estimate_perm = beta,
    statistic_perm = t,
    p_perm = p
  )

perm_df_base_interaction_max


#----------------------------------------------------------
# Combine original estimates, bootstrap confidence intervals,
# and permutation p-values
#----------------------------------------------------------

forest_df_base_interaction_max <-
  coefficient_base_interaction_max |>
  dplyr::left_join(
    boot_ci_df_base_interaction_max,
    by = "term"
  ) |>
  dplyr::left_join(
    perm_df_base_interaction_max,
    by = "term"
  )

forest_df_base_interaction_max


############################################################
#### 8.3.6 Forest plot
############################################################

forest_plot_df_base_interaction_max <-
  forest_df_base_interaction_max |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    
    term_label = dplyr::case_when(
      term == "interruption_groupa" ~
        "Accidental interruption vs none (NC)",
      
      term == "interruption_groupdp" ~
        "Deliberate interruption vs none (NC)",
      
      term == "box_conditionJA" ~
        "JA vs NC (no interruption)",
      
      term == "high_proportion" ~
        "High movement proportion",
      
      term == "interruption_groupa:box_conditionJA" ~
        "Accidental interruption × JA",
      
      term == "interruption_groupdp:box_conditionJA" ~
        "Deliberate interruption × JA",
      
      TRUE ~ term
    ),
    
    significant =
      !is.na(p_perm) &
      p_perm < 0.05,
    
    significance_group = dplyr::case_when(
      significant & estimate < 0 ~
        "Negative significant",
      
      significant & estimate > 0 ~
        "Positive significant",
      
      TRUE ~
        "Not significant"
    ),
    
    star = dplyr::if_else(
      significant,
      "*",
      ""
    )
  )


#----------------------------------------------------------
# Predictor order
#----------------------------------------------------------

term_order_base_interaction_max <- c(
  "Accidental interruption vs none (NC)",
  "Deliberate interruption vs none (NC)",
  "JA vs NC (no interruption)",
  "High movement proportion",
  "Accidental interruption × JA",
  "Deliberate interruption × JA"
)

forest_plot_df_base_interaction_max <-
  forest_plot_df_base_interaction_max |>
  dplyr::mutate(
    term_label = factor(
      term_label,
      levels = rev(
        term_order_base_interaction_max
      )
    )
  )


#----------------------------------------------------------
# Colours
#----------------------------------------------------------

forest_pal_base_interaction_max <- c(
  "Negative significant" = "#3B4CC0",
  "Positive significant" = "#B40426",
  "Not significant" = "#D9A441"
)


#----------------------------------------------------------
# Create forest plot
#----------------------------------------------------------

p_forest_base_interaction_max <- ggplot2::ggplot(
  forest_plot_df_base_interaction_max,
  ggplot2::aes(
    x = estimate,
    y = term_label,
    colour = significance_group
  )
) +
  
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.6
  ) +
  
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = term_label,
      yend = term_label
    ),
    linewidth = 0.9
  ) +
  
  ggplot2::geom_point(
    size = 3.5
  ) +
  
  ggplot2::geom_text(
    data = dplyr::filter(
      forest_plot_df_base_interaction_max,
      significant
    ),
    ggplot2::aes(
      label = star
    ),
    nudge_y = 0.22,
    colour = "black",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  ggplot2::scale_colour_manual(
    values = forest_pal_base_interaction_max
  ) +
  
  ggplot2::labs(
    title =
      "Interaction model: maximum facial temperature change",
    subtitle =
      "95% bootstrap CIs; significance based on permutation tests",
    x =
      "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    ),
    axis.text.y = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "none",
    panel.grid.minor =
      ggplot2::element_blank()
  )


#----------------------------------------------------------
# Display plot
#----------------------------------------------------------

p_forest_base_interaction_max


#----------------------------------------------------------
# Save plot
#----------------------------------------------------------

ggplot2::ggsave(
  filename =
    "figures/forest_plot_base_interaction_max.png",
  plot =
    p_forest_base_interaction_max,
  width = 10,
  height = 6.5,
  dpi = 300
)















############################################################
## 8.4 Final combined forest plot
############################################################

library(dplyr)
library(forcats)
library(ggplot2)
library(viridisLite)
library(grid)


#-----------------------------------------------------------
# Predictor labels
#-----------------------------------------------------------

name_map <- c(
  "interruption_groupa" =
    "Accidental vs no interruption",
  
  "interruption_groupdp" =
    "Deliberate vs no interruption",
  
  "box_conditionJA" =
    "Joint Action vs Non-Collaborative",
  
  "high_proportion" =
    "High movement proportion",
  
  "interruption_groupa:box_conditionJA" =
    "Accidental interruption × Joint Action",
  
  "interruption_groupdp:box_conditionJA" =
    "Deliberate interruption × Joint Action"
)


#-----------------------------------------------------------
# Predictor order
#-----------------------------------------------------------

pred_order <- c(
  "Accidental vs no interruption",
  "Deliberate vs no interruption",
  "Joint Action vs Non-Collaborative",
  "High movement proportion",
  "Accidental interruption × Joint Action",
  "Deliberate interruption × Joint Action"
)


#-----------------------------------------------------------
# Combine primary interaction models
#-----------------------------------------------------------


plot_df <- dplyr::bind_rows(
  
  forest_df_base_interaction_max |>
    dplyr::mutate(
      outcome_label = "Maximum Δ temperature"
    ),
  
  forest_df_base_interaction_min |>
    dplyr::mutate(
      outcome_label = "Minimum Δ temperature"
    ),
  
  forest_df_base_interaction_av |>
    dplyr::mutate(
      outcome_label = "Average Δ temperature"
    )
)


#-----------------------------------------------------------
# Prepare predictors and significance groups
#-----------------------------------------------------------

plot_df <- plot_df |>
  
  # Remove intercept
  dplyr::filter(
    term != "(Intercept)"
  ) |>
  
  # Rename predictors
  dplyr::mutate(
    
    predictor_label =
      dplyr::recode(
        term,
        !!!name_map
      ),
    
    # Significance is based on permutation p-values
    signif =
      !is.na(p_perm) &
      p_perm < 0.05,
    
    # Colour according to direction and significance
    color_group =
      dplyr::case_when(
        
        signif &
          estimate < 0 ~ "neg_sig",
        
        signif &
          estimate > 0 ~ "pos_sig",
        
        TRUE ~ "nonsig"
      )
  )


#-----------------------------------------------------------
# Set predictor order
#-----------------------------------------------------------

plot_df <- plot_df |>
  dplyr::mutate(
    
    predictor_label = factor(
      predictor_label,
      levels = rev(pred_order)
    ),
    
    outcome_label = factor(
      outcome_label,
      levels = c(
        "Maximum Δ temperature",
        "Minimum Δ temperature",
        "Average Δ temperature"
      )
    )
  )


#-----------------------------------------------------------
# Magma colour palette
#-----------------------------------------------------------

pal <- c(
  "neg_sig" = "#3B4CC0",
  "pos_sig" = "#B40426",
  "nonsig" = "grey75"
)


#-----------------------------------------------------------
# Create combined forest plot
#-----------------------------------------------------------

final_forest_overview <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(
    x = estimate,
    y = predictor_label
  )
) +
  
  # Zero reference line
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  # Bootstrap confidence intervals
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = conf.low,
      xmax = conf.high,
      color = color_group
    ),
    height = 0.25,
    linewidth = 0.9
  ) +
  
  # Point estimates
  ggplot2::geom_point(
    ggplot2::aes(
      color = color_group
    ),
    size = 3.5
  ) +
  
  # Significance stars based on permutation p-values
  ggplot2::geom_text(
    data = dplyr::filter(
      plot_df,
      signif
    ),
    ggplot2::aes(
      label = "*"
    ),
    nudge_y = 0.28,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  
  # Separate panel for each temperature outcome
  ggplot2::facet_grid(
    outcome_label ~ .,
    scales = "free_x"
  ) +
  
  # Magma colours
  ggplot2::scale_color_manual(
    values = pal
  ) +
  
  # Labels
  ggplot2::labs(
    title = "Primary model estimates for facial temperature change",
    subtitle = "95% bootstrap CIs; significance based on permutation tests",
    x = "Estimate (95% bootstrap CI)",
    y = NULL
  ) +
  
  # Theme
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  
  ggplot2::theme(
    
    # Everything in bold
    text = ggplot2::element_text(
      face = "bold",
      color = "black"
    ),
    
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      color = "black",
      size = 16
    ),
    
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      color = "black",
      size = 11
    ),
    
    strip.text = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 11
    ),
    
    strip.background = ggplot2::element_rect(
      fill = "grey92",
      color = "grey60"
    ),
    
    axis.text.y = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    
    axis.text.x = ggplot2::element_text(
      face = "bold",
      color = "black"
    ),
    
    axis.title.x = ggplot2::element_text(
      face = "bold",
      color = "black"
    ),
    
    legend.position = "none",
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    panel.spacing.x =
      grid::unit(
        1.2,
        "cm"
      ),
    
    panel.spacing.y =
      grid::unit(
        0.8,
        "cm"
      )
  )


#-----------------------------------------------------------
# Display plot
#-----------------------------------------------------------

print(
  final_forest_overview
)


#-----------------------------------------------------------
# Save plot
#-----------------------------------------------------------

ggplot2::ggsave(
  filename =
    "figures/primary_model_forest_plot.png",
  
  plot =
    final_forest_overview,
  
  width = 10,
  height = 10,
  dpi = 300
)

#---------------------------------------------------------

# Forest plot summary:
#
# Across the interaction models for average, minimum, and maximum
# facial temperature change, coefficient-level permutation tests
# showed a consistent pattern.
#
# Neither accidental nor deliberate interruption showed a significant
# association with temperature change within the reference box condition
# (NC), and neither interaction coefficient was significant.
#
# In contrast, JA (relative to NC) within the no-interruption reference
# group and high movement proportion were significantly positively
# associated with temperature change across all three outcome models.
#
# Forest plots display the original fixed-effect estimates together with
# 95% bootstrap confidence intervals, while statistical significance is
# indicated on the basis of permutation p-values.












############################################################
#### 9. Predicted effects
############################################################

library(ggplot2)
library(dplyr)
library(ggeffects)
library(patchwork)
library(sjPlot)


############################################################
## Helper function: styled effect plot panel
############################################################


#-----------------------------------------------------------
# Create four-panel effect figure
#-----------------------------------------------------------

make_effect_boxplot_panel <- function(
    data,
    outcome,
    model,
    outcome_title,
    y_label
) {
  
  #---------------------------------------------------------
  # Labels
  #---------------------------------------------------------
  
  interruption_labels <- c(
    "none" = "No interruption",
    "a" = "Accidental interruption",
    "dp" = "Deliberate interruption"
  )
  
  box_labels <- c(
    "NC" = "Non-Collaborative",
    "JA" = "Joint Action"
  )
  
 
  #---------------------------------------------------------
  # Common theme
  #---------------------------------------------------------
  
  theme_effects <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      
      text = ggplot2::element_text(
        face = "bold"
      ),
      
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 13
      ),
      
      axis.title = ggplot2::element_text(
        face = "bold",
        size = 11
      ),
      
      axis.text = ggplot2::element_text(
        face = "bold",
        size = 10,
        colour = "black"
      ),
      
      legend.title = ggplot2::element_text(
        face = "bold",
        size = 10
      ),
      
      legend.text = ggplot2::element_text(
        face = "bold",
        size = 10
      ),
      
      panel.grid.minor = ggplot2::element_blank(),
      
      panel.grid.major.x = ggplot2::element_blank(),
      
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey90",
        linewidth = 0.4
      )
    )
  
  
  #---------------------------------------------------------
  # Interruption condition
  #---------------------------------------------------------
  
  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = interruption_group,
      y = .data[[outcome]],
      fill = interruption_group
    )
  ) +
    
    ggplot2::geom_boxplot(
      width = 0.55,
      alpha = 0.75,
      linewidth = 0.7,
      outlier.shape = NA,
      show.legend = FALSE
    ) +
    
    ggplot2::geom_jitter(
      width = 0.06,
      size = 1,
      alpha = 0.30,
      colour = "black",
      show.legend = FALSE
    ) +
    
    ggplot2::scale_fill_viridis_d(
      option = "D",
      begin = 0.15,
      end = 0.85,
      labels = interruption_labels
    ) +
    
    ggplot2::scale_x_discrete(
      labels = interruption_labels
    ) +
    
    ggplot2::labs(
      title = "Interruption condition",
      x = NULL,
      y = y_label
    ) +
    
    theme_effects +
    
    ggplot2::theme(
      legend.position = "none"
    )
  
  
  #---------------------------------------------------------
  # Box condition
  #---------------------------------------------------------
  
  p2 <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = box_condition,
      y = .data[[outcome]],
      fill = box_condition
    )
  ) +
    
    ggplot2::geom_boxplot(
      width = 0.55,
      alpha = 0.75,
      linewidth = 0.7,
      outlier.shape = NA,
      show.legend = FALSE
    ) +
    
    ggplot2::geom_jitter(
      width = 0.06,
      size = 1,
      alpha = 0.30,
      colour = "black",
      show.legend = FALSE
    ) +
    
    ggplot2::scale_fill_viridis_d(
      option = "D",
      begin = 0.20,
      end = 0.80,
      labels = box_labels
    ) +
    
    ggplot2::scale_x_discrete(
      labels = box_labels
    ) +
    
    ggplot2::labs(
      title = "Box condition",
      x = NULL,
      y = y_label
    ) +
    
    theme_effects +
    
    ggplot2::theme(
      legend.position = "none"
    )
  
  
  #---------------------------------------------------------
  # High movement proportion
  #---------------------------------------------------------
  
  movement_pred <- ggeffects::predict_response(
    model,
    terms = "high_proportion [all]"
  )
  
  viridis_line <- viridisLite::viridis(
    n = 5,
    option = "D"
  )[3]
  
  p3 <- ggplot2::ggplot(
    movement_pred,
    ggplot2::aes(
      x = x,
      y = predicted
    )
  ) +
    
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      fill = viridis_line,
      alpha = 0.20
    ) +
    
    ggplot2::geom_line(
      colour = viridis_line,
      linewidth = 1
    ) +
    
    ggplot2::labs(
      title = "High movement proportion",
      x = "High movement proportion",
      y = y_label
    ) +
    
    theme_effects +
    
    ggplot2::theme(
      legend.position = "none"
    )
  
  
  #---------------------------------------------------------
  # Interruption condition × Box condition
  #---------------------------------------------------------
  
  p4 <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = interruption_group,
      y = .data[[outcome]],
      fill = box_condition
    )
  ) +
    
    ggplot2::geom_boxplot(
      width = 0.58,
      alpha = 0.80,
      linewidth = 0.7,
      outlier.shape = NA,
      position = ggplot2::position_dodge(
        width = 0.70
      )
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(
        colour = box_condition
      ),
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.05,
        dodge.width = 0.70
      ),
      size = 0.9,
      alpha = 0.25,
      show.legend = FALSE
    ) +
    
    ggplot2::scale_fill_viridis_d(
      option = "D",
      begin = 0.20,
      end = 0.80,
      labels = box_labels
    ) +
    
    ggplot2::scale_colour_viridis_d(
      option = "D",
      begin = 0.20,
      end = 0.80,
      labels = box_labels
    ) +
    
    ggplot2::scale_x_discrete(
      labels = interruption_labels
    ) +
    
    ggplot2::labs(
      title = "Interruption condition × Box condition",
      x = NULL,
      y = y_label,
      fill = "Box condition"
    ) +
    
    theme_effects +
    
    ggplot2::theme(
      legend.position = "right"
      )
  

  #---------------------------------------------------------
  # Combine panels
  #---------------------------------------------------------
  
  combined_plot <- (
    p1 | p2
  ) / (
    p3 | p4
  ) +
    
    patchwork::plot_annotation(
      title = outcome_title,
      
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "bold",
          size = 17
        )
      )
    )
  
  combined_plot
}

############################################################
## Average temperature
############################################################


#-----------------------------------------------------------
# Create effect panel
#-----------------------------------------------------------

effects_av <- make_effect_boxplot_panel(
  data = analysis_data,
  outcome = "delta_av_temp",
  model = model_base_interaction_av,
  outcome_title = "Effects – Average temperature change",
  y_label = "Average temperature change"
)


############################################################
## Minimum temperature
############################################################


#-----------------------------------------------------------
# Create effect panel
#-----------------------------------------------------------

effects_min <- make_effect_boxplot_panel(
  data = analysis_data,
  outcome = "delta_min_temp",
  model = model_base_interaction_min,
  outcome_title = "Effects – Minimum temperature change",
  y_label = "Minimum temperature change"
)

effects_min


############################################################
## Maximum temperature
############################################################


#-----------------------------------------------------------
# Create effect panel
#-----------------------------------------------------------

effects_max <- make_effect_boxplot_panel(
  data = analysis_data,
  outcome = "delta_max_temp",
  model = model_base_interaction_max,
  outcome_title = "Effects – Maximum temperature change",
  y_label = "Maximum temperature change"
)

effects_max


############################################################
## Save effect plots
############################################################


#-----------------------------------------------------------
# Average temperature
#-----------------------------------------------------------

ggplot2::ggsave(
  filename = "figures/effects_average.png",
  plot = effects_av,
  width = 12,
  height = 9,
  dpi = 300
)


#-----------------------------------------------------------
# Minimum temperature
#-----------------------------------------------------------

ggplot2::ggsave(
  filename = "figures/effects_minimum.png",
  plot = effects_min,
  width = 12,
  height = 9,
  dpi = 300
)


#-----------------------------------------------------------
# Maximum temperature
#-----------------------------------------------------------

ggplot2::ggsave(
  filename = "figures/effects_maximum.png",
  plot = effects_max,
  width = 12,
  height = 9,
  dpi = 300
)



















############################################################
## 11. Results tables
############################################################


############################################################
## 11.1 Fixed-effects summary table
############################################################


#-----------------------------------------------------------
# Predictor labels
#-----------------------------------------------------------

term_labels_table <- c(
  "(Intercept)" =
    "Intercept",
  
  "interruption_groupa" =
    "Accidental interruption vs No interruption",
  
  "interruption_groupdp" =
    "Deliberate interruption vs No interruption",
  
  "box_conditionJA" =
    "Joint Action vs Non-Collaborative",
  
  "high_proportion" =
    "High movement proportion",
  
  "interruption_groupa:box_conditionJA" =
    "Accidental interruption × Joint Action",
  
  "interruption_groupdp:box_conditionJA" =
    "Deliberate interruption × Joint Action"
)


#-----------------------------------------------------------
# Helper function: extract fixed effects
#-----------------------------------------------------------

extract_fixed_effects <- function(model, model_name) {
  
  # Fixed-effect coefficient table
  coef_table <- as.data.frame(
    coef(summary(model))
  )
  
  coef_table$term <- rownames(coef_table)
  
  # Identify p-value column
  p_column <- grep(
    "^Pr\\(",
    names(coef_table),
    value = TRUE
  )
  
  if (length(p_column) == 1) {
    
    p_values <- coef_table[[p_column]]
    
  } else {
    
    p_values <- rep(
      NA_real_,
      nrow(coef_table)
    )
  }
  
  
  # 95% Wald confidence intervals
  ci_table <- confint(
    model,
    parm = "beta_",
    method = "Wald"
  )
  
  
  # Create result table
  tibble::tibble(
    Model = model_name,
    
    term = coef_table$term,
    
    Estimate =
      coef_table$Estimate,
    
    SE =
      coef_table$`Std. Error`,
    
    CI_low =
      ci_table[
        coef_table$term,
        1
      ],
    
    CI_high =
      ci_table[
        coef_table$term,
        2
      ],
    
    p_value =
      p_values
  )
}


#-----------------------------------------------------------
# Extract results from all six models
#-----------------------------------------------------------

fixed_effects_table_raw <- dplyr::bind_rows(
  
  extract_fixed_effects(
    model_base_interaction_max,
    "Maximum – Interaction"
  ),
  
  extract_fixed_effects(
    model_base_max,
    "Maximum – Base"
  ),
  
  extract_fixed_effects(
    model_base_interaction_min,
    "Minimum – Interaction"
  ),
  
  extract_fixed_effects(
    model_base_min,
    "Minimum – Base"
  ),
  
  extract_fixed_effects(
    model_base_interaction_av,
    "Average – Interaction"
  ),
  
  extract_fixed_effects(
    model_base_av,
    "Average – Base"
  )
)


#-----------------------------------------------------------
# Add readable predictor names
#-----------------------------------------------------------

fixed_effects_table_raw <- fixed_effects_table_raw |>
  dplyr::mutate(
    
    Predictor = dplyr::recode(
      term,
      !!!term_labels_table
    )
    
  ) |>
  dplyr::select(
    Model,
    Predictor,
    Estimate,
    SE,
    CI_low,
    CI_high,
    p_value
  )


#-----------------------------------------------------------
# Create presentation version
#-----------------------------------------------------------

fixed_effects_table <- fixed_effects_table_raw |>
  dplyr::mutate(
    
    Estimate = round(
      Estimate,
      3
    ),
    
    SE = round(
      SE,
      3
    ),
    
    CI_low = round(
      CI_low,
      3
    ),
    
    CI_high = round(
      CI_high,
      3
    ),
    
    p_value = dplyr::case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "< .001",
      TRUE ~ sprintf(
        "%.3f",
        p_value
      )
    )
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

fixed_effects_table







############################################################
## 11.2 Model-fit summary table
############################################################


#-----------------------------------------------------------
# Define models
#-----------------------------------------------------------

models_fit_table <- list(
  
  "Maximum – Interaction" =
    model_base_interaction_max,
  
  "Maximum – Additive" =
    model_base_max,
  
  "Minimum – Interaction" =
    model_base_interaction_min,
  
  "Minimum – Additive" =
    model_base_min,
  
  "Average – Interaction" =
    model_base_interaction_av,
  
  "Average – Additive" =
    model_base_av
)


#-----------------------------------------------------------
# Helper function: extract model-fit statistics
#-----------------------------------------------------------

extract_model_fit <- function(model, model_name) {
  
  r2_values <- performance::r2_nakagawa(
    model
  )
  
  tibble::tibble(
    
    Model =
      model_name,
    
    AIC =
      stats::AIC(model),
    
    BIC =
      stats::BIC(model),
    
    logLik =
      as.numeric(
        stats::logLik(model)
      ),
    
    R2_marginal =
      as.numeric(
        r2_values$R2_marginal
      ),
    
    R2_conditional =
      as.numeric(
        r2_values$R2_conditional
      ),
    
    N =
      stats::nobs(model),
    
    N_child =
      dplyr::n_distinct(
        model.frame(model)$child_id
      ),
    
    N_dyad =
      dplyr::n_distinct(
        model.frame(model)$dyad_id
      )
  )
}


#-----------------------------------------------------------
# Extract model-fit statistics
#-----------------------------------------------------------

model_fit_table_raw <- purrr::imap_dfr(
  models_fit_table,
  extract_model_fit
)


#-----------------------------------------------------------
# Create presentation version
#-----------------------------------------------------------

model_fit_table <- model_fit_table_raw |>
  dplyr::mutate(
    
    AIC = round(
      AIC,
      2
    ),
    
    BIC = round(
      BIC,
      2
    ),
    
    logLik = round(
      logLik,
      2
    ),
    
    R2_marginal = round(
      R2_marginal,
      3
    ),
    
    R2_conditional = round(
      R2_conditional,
      3
    )
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

model_fit_table


############################################################
## Formatted model-fit table
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

model_fit_display <- model_fit_table |>
  dplyr::mutate(
    Model = as.character(Model),
    
    AIC = sprintf("%.2f", AIC),
    BIC = sprintf("%.2f", BIC),
    logLik = sprintf("%.2f", logLik),
    
    R2_marginal = sprintf("%.3f", R2_marginal),
    R2_conditional = sprintf("%.3f", R2_conditional),
    
    N = as.character(N),
    N_child = as.character(N_child),
    N_dyad = as.character(N_dyad)
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_model_fit <- flextable::flextable(
  model_fit_display
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_model_fit <- flextable::set_header_labels(
  table_model_fit,
  Model = "Model",
  AIC = "AIC",
  BIC = "BIC",
  logLik = "logLik",
  R2_marginal = "Marginal R²",
  R2_conditional = "Conditional R²",
  N = "N",
  N_child = "Children",
  N_dyad = "Dyads"
)


table_model_fit <- flextable::align(
  table_model_fit,
  align = "center",
  part = "header"
)

table_model_fit <- flextable::bold(
  table_model_fit,
  part = "header"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_model_fit <- flextable::border_remove(
  table_model_fit
)

table_model_fit <- flextable::font(
  table_model_fit,
  fontname = "Arial",
  part = "all"
)

table_model_fit <- flextable::fontsize(
  table_model_fit,
  size = 14,
  part = "all"
)

table_model_fit <- flextable::bold(
  table_model_fit,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Model names left-aligned
table_model_fit <- flextable::align(
  table_model_fit,
  j = "Model",
  align = "left",
  part = "body"
)

# Numerical columns centered
table_model_fit <- flextable::align(
  table_model_fit,
  j = c(
    "AIC",
    "BIC",
    "logLik",
    "R2_marginal",
    "R2_conditional",
    "N",
    "N_child",
    "N_dyad"
  ),
  align = "center",
  part = "body"
)

# All headers centered
table_model_fit <- flextable::align(
  table_model_fit,
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
table_model_fit <- flextable::hline_top(
  table_model_fit,
  border = outer_border,
  part = "header"
)

# Line below header
table_model_fit <- flextable::hline_bottom(
  table_model_fit,
  border = outer_border,
  part = "header"
)

# Separators between Maximum, Minimum, Average blocks
table_model_fit <- flextable::hline(
  table_model_fit,
  i = c(2, 4),
  border = inner_border,
  part = "body"
)

# Bottom line
table_model_fit <- flextable::hline_bottom(
  table_model_fit,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_model_fit <- flextable::width(
  table_model_fit,
  j = "Model",
  width = 2.70
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "AIC",
  width = 0.90
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "BIC",
  width = 0.90
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "logLik",
  width = 1.00
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "R2_marginal",
  width = 1.25
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "R2_conditional",
  width = 1.60
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "N",
  width = 0.60
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "N_child",
  width = 0.90
)

table_model_fit <- flextable::width(
  table_model_fit,
  j = "N_dyad",
  width = 0.80
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_model_fit <- flextable::padding(
  table_model_fit,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_model_fit <- flextable::line_spacing(
  table_model_fit,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_model_fit


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_model_fit,
  path = "tables/Table_S11_model_fit.png",
  zoom = 3,
  expand = 10
)


















############################################################
## 11.3 Bootstrap and permutation summary table
############################################################


#-----------------------------------------------------------
# Combine robust inference results
#-----------------------------------------------------------

robust_results_table_raw <- dplyr::bind_rows(
  
  forest_df_base_interaction_max |>
    dplyr::mutate(
      Model = "Maximum temperature change"
    ),
  
  forest_df_base_interaction_min |>
    dplyr::mutate(
      Model = "Minimum temperature change"
    ),
  
  forest_df_base_interaction_av |>
    dplyr::mutate(
      Model = "Average temperature change"
    )
)


#-----------------------------------------------------------
# Add readable predictor labels
#-----------------------------------------------------------

robust_results_table_raw <- robust_results_table_raw |>
  dplyr::filter(
    term != "(Intercept)"
  ) |>
  dplyr::mutate(
    
    Predictor = dplyr::recode(
      term,
      !!!term_labels_table
    ),
    
    Significant =
      !is.na(p_perm) &
      p_perm < 0.05
    
  ) |>
  dplyr::select(
    Model,
    Predictor,
    Estimate = estimate,
    Bootstrap_CI_low = conf.low,
    Bootstrap_CI_high = conf.high,
    Permutation_p = p_perm,
    Significant
  )


#-----------------------------------------------------------
# Create presentation version
#-----------------------------------------------------------

robust_results_table <- robust_results_table_raw |>
  dplyr::mutate(
    
    Estimate = round(
      Estimate,
      3
    ),
    
    Bootstrap_CI_low = round(
      Bootstrap_CI_low,
      3
    ),
    
    Bootstrap_CI_high = round(
      Bootstrap_CI_high,
      3
    ),
    
    Permutation_p = dplyr::case_when(
      is.na(Permutation_p) ~ NA_character_,
      Permutation_p < 0.001 ~ "< .001",
      TRUE ~ sprintf(
        "%.3f",
        Permutation_p
      )
    ),
    
    Significant = dplyr::if_else(
      Significant,
      "Yes",
      "No"
    )
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

robust_results_table







############################################################
## 11.4 Estimated marginal means
############################################################

library(emmeans)


#-----------------------------------------------------------
# Helper function: extract EMMs
#-----------------------------------------------------------

extract_emms <- function(model, model_name) {
  
  emms <- emmeans::emmeans(
    model,
    ~ interruption_group * box_condition,
    lmer.df = "satterthwaite"
  )
  
  as.data.frame(
    summary(
      emms,
      infer = c(TRUE, TRUE)
    )
  ) |>
    dplyr::mutate(
      Model = model_name,
      
      Interruption = dplyr::recode(
        interruption_group,
        "none" = "No interruption",
        "a" = "Accidental interruption",
        "dp" = "Deliberate interruption"
      ),
      
      Box_condition = dplyr::recode(
        box_condition,
        "NC" = "Non-Collaborative",
        "JA" = "Joint Action"
      )
    ) |>
    dplyr::select(
      Model,
      Interruption,
      Box_condition,
      EMM = emmean,
      SE,
      df,
      CI_low = lower.CL,
      CI_high = upper.CL
    )
}


#-----------------------------------------------------------
# Extract EMMs for all three interaction models
#-----------------------------------------------------------

emm_table_raw <- dplyr::bind_rows(
  
  extract_emms(
    model_base_interaction_max,
    "Maximum"
  ),
  
  extract_emms(
    model_base_interaction_min,
    "Minimum"
  ),
  
  extract_emms(
    model_base_interaction_av,
    "Average"
  )
)


#-----------------------------------------------------------
# Create presentation version
#-----------------------------------------------------------

emm_table <- emm_table_raw |>
  dplyr::mutate(
    EMM = round(EMM, 3),
    SE = round(SE, 3),
    df = round(df, 1),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3)
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

emm_table








############################################################
## 11.5 Model-reduction summary
############################################################


#-----------------------------------------------------------
# Helper function: interaction vs reduced model
#-----------------------------------------------------------

extract_reduction_result <- function(
    reduced_model,
    interaction_model,
    outcome_name
) {
  
  comparison <- anova(
    reduced_model,
    interaction_model
  )
  
  tibble::tibble(
    Outcome = outcome_name,
    
    AIC_reduced =
      AIC(reduced_model),
    
    AIC_interaction =
      AIC(interaction_model),
    
    Chi_square =
      comparison$Chisq[2],
    
    df =
      comparison$Df[2],
    
    p_value =
      comparison$`Pr(>Chisq)`[2]
  )
}


#-----------------------------------------------------------
# Combine outcomes
#-----------------------------------------------------------

model_reduction_table <- dplyr::bind_rows(
  
  extract_reduction_result(
    model_reduced_interaction_max,
    model_base_interaction_max,
    "Maximum"
  ),
  
  extract_reduction_result(
    model_reduced_interaction_min,
    model_base_interaction_min,
    "Minimum"
  ),
  
  extract_reduction_result(
    model_reduced_interaction_av,
    model_base_interaction_av,
    "Average"
  )
) |>
  dplyr::mutate(
    
    AIC_reduced =
      round(AIC_reduced, 2),
    
    AIC_interaction =
      round(AIC_interaction, 2),
    
    Chi_square =
      round(Chi_square, 2),
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

model_reduction_table

############################################################
## 11.5.1 Formatted model-reduction table
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

model_reduction_display <- model_reduction_table |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum" = "Δ maximum temperature",
      "Minimum" = "Δ minimum temperature",
      "Average" = "Δ average temperature"
    ),
    
    Reduced_AIC = sprintf(
      "%.2f",
      AIC_reduced
    ),
    
    Interaction_AIC = sprintf(
      "%.2f",
      AIC_interaction
    ),
    
    Chi_square_display = sprintf(
      "%.2f",
      Chi_square
    ),
    
    df_display = as.character(
      df
    ),
    
    p_display = dplyr::case_when(
      as.numeric(p_value) < .001 ~ "< .001",
      TRUE ~ sprintf(
        "%.3f",
        as.numeric(p_value)
      )
    )
  ) |>
  
  dplyr::select(
    Temperature_measure,
    Reduced_AIC,
    Interaction_AIC,
    Chi_square_display,
    df_display,
    p_display
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_model_reduction <- flextable::flextable(
  model_reduction_display
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_model_reduction <- flextable::set_header_labels(
  table_model_reduction,
  Temperature_measure = "Temperature measure",
  Reduced_AIC = "Reduced AIC",
  Interaction_AIC = "Interaction AIC",
  Chi_square_display = "χ²",
  df_display = "df",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_model_reduction <- flextable::border_remove(
  table_model_reduction
)

table_model_reduction <- flextable::font(
  table_model_reduction,
  fontname = "Arial",
  part = "all"
)

table_model_reduction <- flextable::fontsize(
  table_model_reduction,
  size = 14,
  part = "all"
)

table_model_reduction <- flextable::bold(
  table_model_reduction,
  part = "header"
)

table_model_reduction <- flextable::bold(
  table_model_reduction,
  j = "Temperature_measure",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Temperature measure left-aligned
table_model_reduction <- flextable::align(
  table_model_reduction,
  j = "Temperature_measure",
  align = "left",
  part = "body"
)

# Numerical columns centered
table_model_reduction <- flextable::align(
  table_model_reduction,
  j = c(
    "Reduced_AIC",
    "Interaction_AIC",
    "Chi_square_display",
    "df_display",
    "p_display"
  ),
  align = "center",
  part = "body"
)

# Headers centered
table_model_reduction <- flextable::align(
  table_model_reduction,
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

table_model_reduction <- flextable::hline_top(
  table_model_reduction,
  border = outer_border,
  part = "header"
)

table_model_reduction <- flextable::hline_bottom(
  table_model_reduction,
  border = outer_border,
  part = "header"
)

table_model_reduction <- flextable::hline_bottom(
  table_model_reduction,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "Temperature_measure",
  width = 2.60
)

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "Reduced_AIC",
  width = 1.45
)

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "Interaction_AIC",
  width = 1.55
)

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "Chi_square_display",
  width = 0.80
)

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "df_display",
  width = 0.65
)

table_model_reduction <- flextable::width(
  table_model_reduction,
  j = "p_display",
  width = 0.80
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_model_reduction <- flextable::padding(
  table_model_reduction,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_model_reduction <- flextable::line_spacing(
  table_model_reduction,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_model_reduction


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_model_reduction,
  path = "tables/Table_S12_model_reduction.png",
  zoom = 3,
  expand = 10
)


















############################################################
## 11.6 Pairwise comparisons of interruption conditions
############################################################

#-----------------------------------------------------------
# Helper function: extract pairwise comparisons
#-----------------------------------------------------------

extract_pairwise_interruption <- function(
    pairwise_object,
    outcome_name
) {
  
  pairwise_df <- as.data.frame(
    summary(
      pairwise_object,
      infer = c(TRUE, TRUE),
      adjust = "holm"
    )
  )
  
  pairwise_df |>
    
    dplyr::mutate(
      
      Outcome = outcome_name,
      
      Interruption_comparison = dplyr::recode(
        contrast,
        "none - a" =
          "No interruption – Accidental interruption",
        "none - dp" =
          "No interruption – Deliberate interruption",
        "a - dp" =
          "Accidental interruption – Deliberate interruption"
      ),
      
      Box_condition = dplyr::recode(
        box_condition,
        "NC" = "Non-Collaborative",
        "JA" = "Joint Action"
      )
    ) |>
    
    dplyr::select(
      Outcome,
      Interruption_comparison,
      Box_condition,
      Estimate = estimate,
      SE,
      df,
      CI_low = lower.CL,
      CI_high = upper.CL,
      p_value = p.value
    )
}


#-----------------------------------------------------------
# Combine outcomes
#-----------------------------------------------------------

pairwise_interruption_table <- dplyr::bind_rows(
  
  extract_pairwise_interruption(
    pairs_interruption_by_box_max,
    "Maximum"
  ),
  
  extract_pairwise_interruption(
    pairs_interruption_by_box_min,
    "Minimum"
  ),
  
  extract_pairwise_interruption(
    pairs_interruption_by_box_av,
    "Average"
  )
)


#-----------------------------------------------------------
# Check table
#-----------------------------------------------------------

pairwise_interruption_table


############################################################
## 11.6.1 Formatted pairwise-comparison table
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

pairwise_interruption_display <- pairwise_interruption_table |>
  dplyr::mutate(
    
    # Keep outcome order: Maximum -> Minimum -> Average
    Outcome = factor(
      Outcome,
      levels = c(
        "Maximum",
        "Minimum",
        "Average"
      )
    )
    
  ) |>
  dplyr::arrange(
    Outcome
  ) |>
  dplyr::mutate(
    
    # Store numerical p-value for significance formatting
    p_numeric = p_value,
    
    # Round values for presentation
    Estimate_display = sprintf(
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
    
    p_display = dplyr::case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    # Show outcome only in first row of each block
    Outcome_display = dplyr::case_when(
      dplyr::row_number() == 1  ~ "Δ maximum temperature",
      dplyr::row_number() == 7  ~ "Δ minimum temperature",
      dplyr::row_number() == 13 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )


#-----------------------------------------------------------
# Identify significant rows
#-----------------------------------------------------------

significant_rows <- which(
  !is.na(pairwise_interruption_display$p_numeric) &
    pairwise_interruption_display$p_numeric < .05
)


#-----------------------------------------------------------
# Keep only columns shown in the table
#-----------------------------------------------------------

pairwise_interruption_display_ft <-
  pairwise_interruption_display |>
  dplyr::select(
    Outcome_display,
    Interruption_comparison,
    Box_condition,
    Estimate_display,
    SE_display,
    df_display,
    CI_display,
    p_display
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_pairwise <- flextable::flextable(
  pairwise_interruption_display_ft
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_pairwise <- flextable::set_header_labels(
  table_pairwise,
  Outcome_display = "Temperature measure",
  Interruption_comparison = "Interruption comparison",
  Box_condition = "Box condition",
  Estimate_display = "Estimate",
  SE_display = "SE",
  df_display = "df",
  CI_display = "95% CI",
  p_display = "p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_pairwise <- flextable::border_remove(
  table_pairwise
)

table_pairwise <- flextable::font(
  table_pairwise,
  fontname = "Arial",
  part = "all"
)

table_pairwise <- flextable::fontsize(
  table_pairwise,
  size = 14,
  part = "all"
)

table_pairwise <- flextable::bold(
  table_pairwise,
  part = "header"
)

# Bold Maximum / Minimum / Average
table_pairwise <- flextable::bold(
  table_pairwise,
  i = c(1, 7, 13),
  j = "Outcome_display",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Outcome and comparison entries left-aligned
table_pairwise <- flextable::align(
  table_pairwise,
  j = c(
    "Outcome_display",
    "Interruption_comparison"
  ),
  align = "left",
  part = "body"
)

# All remaining body columns centered
table_pairwise <- flextable::align(
  table_pairwise,
  j = c(
    "Box_condition",
    "Estimate_display",
    "SE_display",
    "df_display",
    "CI_display",
    "p_display"
  ),
  align = "center",
  part = "body"
)

# ALL headers centered
table_pairwise <- flextable::align(
  table_pairwise,
  align = "center",
  part = "header"
)

table_pairwise <- flextable::valign(
  table_pairwise,
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
table_pairwise <- flextable::hline_top(
  table_pairwise,
  border = outer_border,
  part = "header"
)

# Line below header
table_pairwise <- flextable::hline_bottom(
  table_pairwise,
  border = outer_border,
  part = "header"
)

# Lines between Maximum / Minimum / Average
table_pairwise <- flextable::hline(
  table_pairwise,
  i = c(6, 12),
  border = inner_border,
  part = "body"
)

# Bottom line
table_pairwise <- flextable::hline_bottom(
  table_pairwise,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_pairwise <- flextable::width(
  table_pairwise,
  j = "Outcome_display",
  width = 2.45
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "Interruption_comparison",
  width = 4.40
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "Box_condition",
  width = 1.85
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "Estimate_display",
  width = 0.95
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "SE_display",
  width = 0.70
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "df_display",
  width = 0.65
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "CI_display",
  width = 1.65
)

table_pairwise <- flextable::width(
  table_pairwise,
  j = "p_display",
  width = 0.75
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_pairwise <- flextable::padding(
  table_pairwise,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_pairwise <- flextable::line_spacing(
  table_pairwise,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant rows, if present
#-----------------------------------------------------------

if (length(significant_rows) > 0) {
  
  table_pairwise <- flextable::bold(
    table_pairwise,
    i = significant_rows,
    part = "body"
  )
}


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_pairwise


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

flextable::save_as_image(
  x = table_pairwise,
  path = "tables/Table_S9_pairwise_comparisons.png",
  zoom = 3,
  expand = 10
)




















############################################################
## 12. Formatted results tables
############################################################

# install.packages("gt")
library(gt)


############################################################
## 12.1 Formatted fixed-effects table
############################################################

#-----------------------------------------------------------
# Prepare table
#-----------------------------------------------------------

fixed_effects_table_formatted <- fixed_effects_table |>
  dplyr::mutate(
    Model = factor(
      Model,
      levels = c(
        "Maximum – Interaction",
        "Maximum – Base",
        "Minimum – Interaction",
        "Minimum – Base",
        "Average – Interaction",
        "Average – Base"
      )
    )
  )


#-----------------------------------------------------------
# Create formatted table
#-----------------------------------------------------------

table_fixed_effects <- fixed_effects_table_formatted |>
  gt::gt(
    groupname_col = "Model",
    rowname_col = "Predictor"
  ) |>
  
  gt::tab_header(
    title = gt::md("**Fixed effects of the linear mixed-effects models**"),
    subtitle = "Model-based estimates, standard errors, 95% confidence intervals, and p-values"
  ) |>
  
  gt::tab_spanner(
    label = "95% CI",
    columns = c(
      CI_low,
      CI_high
    )
  ) |>
  
  gt::cols_label(
    Estimate = "Estimate",
    SE = "SE",
    CI_low = "Lower",
    CI_high = "Upper",
    p_value = "p"
  ) |>
  
  gt::fmt_number(
    columns = c(
      Estimate,
      SE,
      CI_low,
      CI_high
    ),
    decimals = 3
  ) |>
  
  gt::cols_align(
    align = "left",
    columns = "Predictor"
  ) |>
  
  gt::cols_align(
    align = "center",
    columns = c(
      Estimate,
      SE,
      CI_low,
      CI_high,
      p_value
    )
  ) |>
  
  gt::tab_style(
    style = gt::cell_text(
      weight = "bold"
    ),
    locations = gt::cells_row_groups()
  ) |>
  
  gt::tab_style(
    style = gt::cell_fill(
      color = "grey95"
    ),
    locations = gt::cells_row_groups()
  ) |>
  
  gt::tab_options(
    table.font.size = 11,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 10,
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    data_row.padding = gt::px(5)
  )

table_fixed_effects


############################################################
## 12.2 Formatted model-fit table
############################################################

#-----------------------------------------------------------
# Create formatted table
#-----------------------------------------------------------

table_model_fit <- model_fit_table |>
  gt::gt(
    rowname_col = "Model"
  ) |>
  
  gt::tab_header(
    title = gt::md("**Model fit statistics**")
  ) |>
  
  gt::tab_spanner(
    label = "Nakagawa R²",
    columns = c(
      R2_marginal,
      R2_conditional
    )
  ) |>
  
  gt::cols_label(
    AIC = "AIC",
    BIC = "BIC",
    logLik = "logLik",
    R2_marginal = "Marginal",
    R2_conditional = "Conditional",
    N = "N",
    N_child = "Children",
    N_dyad = "Dyads"
  ) |>
  
  gt::fmt_number(
    columns = c(
      AIC,
      BIC,
      logLik
    ),
    decimals = 2
  ) |>
  
  gt::fmt_number(
    columns = c(
      R2_marginal,
      R2_conditional
    ),
    decimals = 3
  ) |>
  
  gt::cols_align(
    align = "center",
    columns = gt::everything()
  ) |>
  
  gt::tab_options(
    table.font.size = 11,
    heading.title.font.size = 14,
    column_labels.font.weight = "bold",
    stub.font.weight = "bold",
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    data_row.padding = gt::px(6)
  )

table_model_fit


############################################################
## 12.3 Formatted bootstrap and permutation table
############################################################

############################################################
## 12.3 Formatted bootstrap and permutation table
############################################################

library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare presentation table
#-----------------------------------------------------------

robust_table_formatted <- robust_results_table_raw |>
  dplyr::mutate(
    
    Predictor = dplyr::recode(
      Predictor,
      "Accidental interruption vs No interruption" =
        "Accidental interruption vs no interruption",
      "Deliberate interruption vs No interruption" =
        "Deliberate interruption vs no interruption"
    ),
    
    # Set outcome order
    Model = factor(
      Model,
      levels = c(
        "Maximum temperature change",
        "Minimum temperature change",
        "Average temperature change"
      )
    ),
    
    # Round estimates and bootstrap CIs to two decimals
    Estimate_display = sprintf(
      "%.2f",
      Estimate
    ),
    
    Bootstrap_CI_display = sprintf(
      "[%.2f, %.2f]",
      Bootstrap_CI_low,
      Bootstrap_CI_high
    ),
    
    # Permutation p-values to three decimals
    Permutation_p_display = dplyr::case_when(
      is.na(Permutation_p) ~ "",
      Permutation_p < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", Permutation_p)
    )
  ) |>
  
  dplyr::arrange(
    Model
  ) |>
  
  dplyr::select(
    Model,
    Predictor,
    Estimate_display,
    Bootstrap_CI_display,
    Permutation_p_display,
    Permutation_p
  )

#-----------------------------------------------------------
# Prepare table without extra group-heading rows
#-----------------------------------------------------------

table_primary_display <- robust_table_formatted |>
  dplyr::mutate(
    Model = as.character(Model),
    Model = dplyr::case_when(
      dplyr::row_number() == 1  ~ "Δ maximum temperature",
      dplyr::row_number() == 7  ~ "Δ minimum temperature",
      dplyr::row_number() == 13 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )

#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

# Save row numbers of significant results before removing helper column
significant_rows <- which(
  !is.na(table_primary_display$Permutation_p) &
    table_primary_display$Permutation_p < .05
)

# Remove helper column from displayed table
table_primary_display_ft <- table_primary_display |>
  dplyr::select(
    -Permutation_p
  )

# Create flextable
table_robust <- flextable::flextable(
  table_primary_display_ft
)

# Header labels
table_robust <- flextable::set_header_labels(
  table_robust,
  Model = "Outcome",
  Predictor = "Predictor",
  Estimate_display = "Estimate",
  Bootstrap_CI_display = "95% bootstrap CI",
  Permutation_p_display = "Permutation p"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_robust <- flextable::border_remove(
  table_robust
)

table_robust <- flextable::font(
  table_robust,
  fontname = "Arial",
  part = "all"
)

table_robust <- flextable::fontsize(
  table_robust,
  size = 14,
  part = "all"
)

table_robust <- flextable::bold(
  table_robust,
  part = "header"
)


#-----------------------------------------------------------
# Bold outcome headings
#-----------------------------------------------------------

table_robust <- flextable::bold(
  table_robust,
  i = c(1, 7, 13),
  j = "Model",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Text columns left-aligned
table_robust <- flextable::align(
  table_robust,
  j = c(
    "Model",
    "Predictor"
  ),
  align = "left",
  part = "all"
)

# Numeric columns centered
table_robust <- flextable::align(
  table_robust,
  j = c(
    "Estimate_display",
    "Bootstrap_CI_display",
    "Permutation_p_display"
  ),
  align = "center",
  part = "all"
)

# Headers centered
table_robust <- flextable::align(
  table_robust,
  align = "center",
  part = "header"
)

table_robust <- flextable::valign(
  table_robust,
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
table_robust <- flextable::hline_top(
  table_robust,
  border = outer_border,
  part = "header"
)

# Line below header
table_robust <- flextable::hline_bottom(
  table_robust,
  border = outer_border,
  part = "header"
)

# Separators before Minimum and Average
# Separators between outcome blocks
table_robust <- flextable::hline(
  table_robust,
  i = c(6, 12),
  border = inner_border,
  part = "body"
)

# Bottom line
table_robust <- flextable::hline_bottom(
  table_robust,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_robust <- flextable::width(
  table_robust,
  j = "Model",
  width = 2.70
)

table_robust <- flextable::width(
  table_robust,
  j = "Predictor",
  width = 3.70
)

table_robust <- flextable::width(
  table_robust,
  j = "Estimate_display",
  width = 1.00
)

table_robust <- flextable::width(
  table_robust,
  j = "Bootstrap_CI_display",
  width = 1.85
)

table_robust <- flextable::width(
  table_robust,
  j = "Permutation_p_display",
  width = 1.60
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_robust <- flextable::padding(
  table_robust,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_robust <- flextable::line_spacing(
  table_robust,
  space = 1.15,
  part = "body"
)


#-----------------------------------------------------------
# Bold significant results
#-----------------------------------------------------------

table_robust <- flextable::bold(
  table_robust,
  i = significant_rows,
  j = c(
    "Predictor",
    "Estimate_display",
    "Bootstrap_CI_display",
    "Permutation_p_display"
  ),
  part = "body"
)


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_robust


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

flextable::save_as_image(
  x = table_robust,
  path = "tables/Table_S8_primary_model_results.png",
  zoom = 3,
  expand = 10
)


############################################################
## 12.4 Formatted estimated marginal means table
############################################################

#-----------------------------------------------------------
# Create formatted table
#-----------------------------------------------------------

table_emm <- emm_table |>
  gt::gt(
    groupname_col = "Model"
  ) |>
  
  gt::tab_header(
    title = gt::md("**Estimated marginal means**"),
    subtitle = "Estimated marginal means for interruption and box-condition combinations"
  ) |>
  
  gt::tab_spanner(
    label = "95% CI",
    columns = c(
      CI_low,
      CI_high
    )
  ) |>
  
  gt::cols_label(
    Interruption = "Interruption condition",
    Box_condition = "Box condition",
    EMM = "EMM",
    SE = "SE",
    df = "df",
    CI_low = "Lower",
    CI_high = "Upper"
  ) |>
  
  gt::fmt_number(
    columns = c(
      EMM,
      SE,
      CI_low,
      CI_high
    ),
    decimals = 3
  ) |>
  
  gt::fmt_number(
    columns = df,
    decimals = 1
  ) |>
  
  gt::tab_style(
    style = gt::cell_text(
      weight = "bold"
    ),
    locations = gt::cells_row_groups()
  ) |>
  
  gt::tab_style(
    style = gt::cell_fill(
      color = "grey95"
    ),
    locations = gt::cells_row_groups()
  ) |>
  
  gt::tab_options(
    table.font.size = 11,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 10,
    column_labels.font.weight = "bold",
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    data_row.padding = gt::px(5)
  )

table_emm


############################################################
## 12.5 Formatted model-reduction table
############################################################

table_reduction <- model_reduction_table |>
  gt::gt(
    rowname_col = "Outcome"
  ) |>
  
  gt::tab_header(
    title = gt::md("**Model reduction**"),
    subtitle = "Comparison of interaction and reduced models"
  ) |>
  
  gt::cols_label(
    AIC_reduced = "Reduced AIC",
    AIC_interaction = "Interaction AIC",
    Chi_square = "χ²",
    p_value = "p"
  ) |>
  
  gt::fmt_number(
    columns = c(
      AIC_reduced,
      AIC_interaction,
      Chi_square
    ),
    decimals = 2
  ) |>
  
  gt::cols_align(
    align = "center",
    columns = gt::everything()
  ) |>
  
  gt::tab_options(
    table.font.size = 11,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 10,
    column_labels.font.weight = "bold",
    stub.font.weight = "bold",
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    data_row.padding = gt::px(6)
  )

table_reduction



############################################################
## 12.5 Formatted pairwise-comparisons table
############################################################

#-----------------------------------------------------------
# Create formatted table
#-----------------------------------------------------------

table_pairwise_interruption <- pairwise_interruption_table |>
  
  gt::gt(
    groupname_col = "Outcome"
  ) |>
  
  gt::tab_header(
    
    title = gt::md(
      "**Pairwise comparisons of interruption conditions**"
    ),
    
    subtitle =
      "Pairwise comparisons within each box condition with Holm-adjusted p-values"
  ) |>
  
  gt::tab_spanner(
    label = "95% CI",
    columns = c(
      CI_low,
      CI_high
    )
  ) |>
  
  gt::cols_label(
    
    Interruption_comparison =
      "Interruption comparison",
    
    Box_condition =
      "Box condition",
    
    Estimate =
      "Estimate",
    
    SE =
      "SE",
    
    df =
      "df",
    
    CI_low =
      "Lower",
    
    CI_high =
      "Upper",
    
    p_value =
      "p"
  ) |>
  
  gt::fmt_number(
    columns = c(
      Estimate,
      SE,
      CI_low,
      CI_high
    ),
    decimals = 3
  ) |>
  
  gt::fmt_number(
    columns = df,
    decimals = 1
  ) |>
  
  gt::fmt_number(
    columns = p_value,
    decimals = 3
  ) |>
  
  gt::sub_missing(
    missing_text = "—"
  ) |>
  
  gt::cols_align(
    align = "center",
    columns = c(
      Interruption_comparison,
      Box_condition
    )
  ) |>
  
  gt::cols_align(
    align = "right",
    columns = c(
      Estimate,
      SE,
      df,
      CI_low,
      CI_high,
      p_value
    )
  ) |>
  
  gt::tab_style(
    
    style = list(
      gt::cell_text(
        weight = "bold"
      ),
      gt::cell_fill(
        color = "#F2F2F2"
      )
    ),
    
    locations =
      gt::cells_row_groups()
  ) |>
  
  gt::tab_options(
    
    table.font.size = 11,
    
    heading.title.font.size = 14,
    
    heading.subtitle.font.size = 10,
    
    column_labels.font.weight = "bold",
    
    row_group.font.weight = "bold",
    
    table.border.top.width =
      gt::px(1),
    
    table.border.bottom.width =
      gt::px(1),
    
    data_row.padding =
      gt::px(5)
  )


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_pairwise_interruption









############################################################
## 12.6 Save formatted results tables
############################################################

#-----------------------------------------------------------
# Create output folder
#-----------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)


############################################################
## Save formatted tables as Word files
############################################################

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

gt::gtsave(
  table_fixed_effects,
  filename = "tables/01_fixed_effects.docx"
)

gt::gtsave(
  table_model_fit,
  filename = "tables/02_model_fit.docx"
)

gt::gtsave(
  table_robust,
  filename = "tables/03_bootstrap_permutation.docx"
)

gt::gtsave(
  table_emm,
  filename = "tables/04_estimated_marginal_means.docx"
)

gt::gtsave(
  table_reduction,
  filename = "tables/05_model_reduction.docx"
)

gt::gtsave(
  table_pairwise_interruption,
  filename = "tables/05_pairwise_comparisons.html"
)



































############################################################
## Simulation-based power and sensitivity analysis
############################################################

# Install once if necessary
# install.packages("simr")

library(simr)


#-----------------------------------------------------------
# Settings
#-----------------------------------------------------------

POWER_NSIM <- 1000
POWER_ALPHA <- 0.05
POWER_SEED <- 26082026









############################################################
## Maximum
############################################################

#-----------------------------------------------------------
# Current-design power: Box condition
# Maximum temperature
#-----------------------------------------------------------

set.seed(POWER_SEED)

power_box_max <- simr::powerSim(
  model_base_interaction_max,
  test = simr::fixed(
    "box_conditionJA",
    method = "t"
  ),
  nsim = POWER_NSIM,
  alpha = POWER_ALPHA
)

power_box_max


#-----------------------------------------------------------
# Current-design power: High movement proportion
# Maximum temperature
#-----------------------------------------------------------

set.seed(POWER_SEED)

power_movement_max <- simr::powerSim(
  model_base_interaction_max,
  test = simr::fixed(
    "high_proportion",
    method = "t"
  ),
  nsim = POWER_NSIM,
  alpha = POWER_ALPHA
)

power_movement_max


#-----------------------------------------------------------
# Current-design power: Interruption condition
# Maximum temperature
#-----------------------------------------------------------

set.seed(POWER_SEED)

power_interruption_max <- simr::powerSim(
  model_base_interaction_max,
  test = simr::fixed(
    "interruption_group",
    method = "anova"
  ),
  nsim = POWER_NSIM,
  alpha = POWER_ALPHA
)

power_interruption_max


#-----------------------------------------------------------
# Current-design power: Interruption × Box interaction
# Maximum temperature
#-----------------------------------------------------------

set.seed(POWER_SEED)

power_interaction_max <- simr::powerSim(
  model_base_interaction_max,
  test = simr::fixed(
    "interruption_group:box_condition",
    method = "anova"
  ),
  nsim = POWER_NSIM,
  alpha = POWER_ALPHA
)

power_interaction_max

#-----------------------------------------------------------
# Results / interpretation
#-----------------------------------------------------------

# Simulation-based power analyses based on the effect estimates
# from the fitted maximum-temperature interaction model indicated
# low statistical power for all examined effects.
#
# Estimated power was 23.8% for Box Condition, 37.3% for
# High Movement Proportion, 20.6% for Interruption Condition,
# and 8.4% for the Interruption Condition × Box Condition
# interaction.
#
# Thus, none of the examined effects reached the conventional
# target of 80% power. Power was highest for High Movement
# Proportion and lowest for the Interruption × Box interaction.
#
# Because these simulations use effect sizes estimated from the
# observed data, the results represent achieved power conditional
# on the fitted model estimates and should be interpreted cautiously.





############################################################
## Sensitivity analysis: Maximum temperature
############################################################

# Goal:
# Determine how much larger each observed effect would need to be
# for the current design to achieve approximately 80% power.

#-----------------------------------------------------------
# Helper function: estimate power for scaled effect
#-----------------------------------------------------------

estimate_power_for_scaled_effect <- function(
    model,
    coefficient_names,
    multiplier,
    test,
    nsim = 300,
    alpha = 0.05,
    seed = 26082026
) {
  
  # Copy model
  sim_model <- model
  
  # Original fixed effects
  original_fixef <- lme4::fixef(model)
  
  # Create modified coefficient vector
  new_fixef <- original_fixef
  
  new_fixef[coefficient_names] <-
    original_fixef[coefficient_names] * multiplier
  
  # Set modified fixed effects in simulation model
  simr::fixef(sim_model) <- new_fixef
  
  # Run power simulation
  set.seed(seed)
  
  result <- simr::powerSim(
    sim_model,
    test = test,
    nsim = nsim,
    alpha = alpha,
    progress = FALSE
  )
  
  # Extract estimated power
  power_value <- result$x / result$n
  
  data.frame(
    multiplier = multiplier,
    power = power_value
  )
}

#-----------------------------------------------------------
# Box condition: coarse search
#-----------------------------------------------------------

# First identify the approximate multiplier range in which
# power reaches 80%.

box_grid_max <- dplyr::bind_rows(
  
  lapply(
    seq(1, 5, by = 0.5),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = "box_conditionJA",
        multiplier = mult,
        test = simr::fixed(
          "box_conditionJA",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

box_grid_max


#-----------------------------------------------------------
# High movement proportion: coarse search
#-----------------------------------------------------------

# Identify the approximate multiplier needed for the observed
# movement effect to reach 80% power.

movement_grid_max <- dplyr::bind_rows(
  
  lapply(
    seq(1, 5, by = 0.5),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = "high_proportion",
        multiplier = mult,
        test = simr::fixed(
          "high_proportion",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

movement_grid_max

# Sensitivity results – Maximum temperature:
# For Box Condition, approximately 80% power was reached when the
# observed effect was increased to about 2.5 times its original size.
#
# For High Movement Proportion, approximately 80% power was reached
# when the observed effect was increased to about 2 times its original size.
#
# Thus, with the current design, substantially larger effects than those
# observed would be required to achieve conventional 80% power.


#-----------------------------------------------------------
# Interruption condition: coarse search
#-----------------------------------------------------------

# Interruption condition is represented by two coefficients.
# Both coefficients are therefore scaled by the same multiplier
# while preserving their relative magnitude.

interruption_terms_max <- c(
  "interruption_groupa",
  "interruption_groupdp"
)

interruption_grid_max <- dplyr::bind_rows(
  
  lapply(
    seq(1, 6, by = 0.5),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = interruption_terms_max,
        multiplier = mult,
        test = simr::fixed(
          "interruption_group",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

interruption_grid_max


#-----------------------------------------------------------
# Interruption × Box interaction: coarse search
#-----------------------------------------------------------

# The interaction is represented by two interaction coefficients.
# Both are scaled together to retain the observed interaction pattern.

interaction_terms_max <- names(
  lme4::fixef(model_base_interaction_max)
)[
  grepl(
    "box_conditionJA.*interruption_group|interruption_group.*box_conditionJA",
    names(lme4::fixef(model_base_interaction_max))
  )
]

interaction_terms_max


# Initial search range
interaction_grid_max <- dplyr::bind_rows(
  
  lapply(
    seq(1, 10, by = 1),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = interaction_terms_max,
        multiplier = mult,
        test = simr::fixed(
          "interruption_group:box_condition",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

interaction_grid_max


#-----------------------------------------------------------
# Interruption × Box interaction: extended coarse search
#-----------------------------------------------------------

# The first search did not reach 80% power.
# Therefore, extend the multiplier range until the 80% region
# is identified.
interaction_grid_max_2 <- dplyr::bind_rows(
  lapply(
    seq(10, 30, by = 2),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = interaction_terms_max,
        multiplier = mult,
        test = simr::fixed(
          "interruption_group:box_condition",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

interaction_grid_max_2




############################################################
## Fine search around the 80% power threshold
############################################################

#-----------------------------------------------------------
# Box condition: fine search
#-----------------------------------------------------------

# Coarse search indicated that 80% power is reached
# between approximately x2.0 and x2.5 of the observed effect.

box_grid_max_fine <- dplyr::bind_rows(
  lapply(
    seq(2.0, 2.5, by = 0.1),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = "box_conditionJA",
        multiplier = mult,
        test = simr::fixed(
          "box_conditionJA",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

box_grid_max_fine


#-----------------------------------------------------------
# High movement proportion: fine search
#-----------------------------------------------------------

# Coarse search indicated that 80% power is reached
# between approximately x1.5 and x2.0 of the observed effect.

movement_grid_max_fine <- dplyr::bind_rows(
  lapply(
    seq(1.5, 2.0, by = 0.1),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = "high_proportion",
        multiplier = mult,
        test = simr::fixed(
          "high_proportion",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

movement_grid_max_fine


#-----------------------------------------------------------
# Interruption condition: fine search
#-----------------------------------------------------------

# Coarse search indicated that 80% power is reached
# between approximately x2.5 and x3.0 of the observed effect.

interruption_grid_max_fine <- dplyr::bind_rows(
  lapply(
    seq(2.5, 3.0, by = 0.1),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = interruption_terms_max,
        multiplier = mult,
        test = simr::fixed(
          "interruption_group",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

interruption_grid_max_fine


#-----------------------------------------------------------
# Interruption × Box interaction: fine search
#-----------------------------------------------------------

# Extended coarse search indicated that 80% power is reached
# between approximately x18 and x20 of the observed interaction.

interaction_grid_max_fine <- dplyr::bind_rows(
  lapply(
    seq(18, 20, by = 0.25),
    function(mult) {
      
      estimate_power_for_scaled_effect(
        model = model_base_interaction_max,
        coefficient_names = interaction_terms_max,
        multiplier = mult,
        test = simr::fixed(
          "interruption_group:box_condition",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

interaction_grid_max_fine

#-----------------------------------------------------------
# Preliminary sensitivity results
#-----------------------------------------------------------

# Fine-grid sensitivity analyses indicated that approximately
# 80% power was reached when the observed effects were increased
# to the following magnitudes:
#
# Box Condition:
# approximately 2.5 times the observed effect
# (estimated power = 83.3%).
#
# High Movement Proportion:
# approximately 1.8 times the observed effect
# (estimated power = 80.0%).
#
# Interruption Condition:
# approximately 2.8 times the observed effect
# (estimated power = 81.7%).
#
# Interruption Condition × Box Condition:
# approximately 19.25 times the observed interaction effect
# (estimated power = 80.0%).
#
# These values are preliminary threshold estimates based on
# 300 simulations and were subsequently confirmed using
# 1000 simulations.


############################################################
## Final confirmation with 1000 simulations
## Maximum temperature
############################################################

#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

box_max_final <- estimate_power_for_scaled_effect(
  model = model_base_interaction_max,
  coefficient_names = "box_conditionJA",
  multiplier = 2.5,
  test = simr::fixed(
    "box_conditionJA",
    method = "t"
  ),
  nsim = 1000
)

box_max_final


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

movement_max_final <- estimate_power_for_scaled_effect(
  model = model_base_interaction_max,
  coefficient_names = "high_proportion",
  multiplier = 1.8,
  test = simr::fixed(
    "high_proportion",
    method = "t"
  ),
  nsim = 1000
)

movement_max_final


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

interruption_max_final <- estimate_power_for_scaled_effect(
  model = model_base_interaction_max,
  coefficient_names = interruption_terms_max,
  multiplier = 2.8,
  test = simr::fixed(
    "interruption_group",
    method = "anova"
  ),
  nsim = 1000
)

interruption_max_final


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

interaction_max_final <- estimate_power_for_scaled_effect(
  model = model_base_interaction_max,
  coefficient_names = interaction_terms_max,
  multiplier = 19.25,
  test = simr::fixed(
    "interruption_group:box_condition",
    method = "anova"
  ),
  nsim = 1000
)

interaction_max_final


#-----------------------------------------------------------
# Interruption × Box interaction:
# final confirmation at slightly larger effect
#-----------------------------------------------------------

interaction_max_final_2 <- estimate_power_for_scaled_effect(
  model = model_base_interaction_max,
  coefficient_names = interaction_terms_max,
  multiplier = 19.75,
  test = simr::fixed(
    "interruption_group:box_condition",
    method = "anova"
  ),
  nsim = 1000
)

interaction_max_final_2

#-----------------------------------------------------------
# Final sensitivity results
#-----------------------------------------------------------

# Final simulations with 1000 iterations confirmed that approximately
# 80% power was achieved when the observed effects were increased by
# the following factors:
#
# Box Condition:
# 2.5 × the observed effect (power = .818).
#
# High Movement Proportion:
# 1.8 × the observed effect (power = .808).
#
# Interruption Condition:
# 2.8 × the observed effect (power = .807).
#
# Interruption Condition × Box Condition:
# 19.75 × the observed interaction effect (power = .804).
#
# Thus, the current design was most sensitive to the movement effect,
# whereas substantially larger Box and Interruption effects would have
# been required to achieve 80% power.
#
# Sensitivity to the Interruption × Box interaction was particularly low:
# the interaction would have needed to be approximately 20 times larger
# than the observed interaction effect to achieve 80% power with the
# current design.






############################################################
## Sample-size analysis with balanced child-dyad structure
## Maximum temperature
############################################################

# Goal:
# Simulate larger samples while preserving the intended study design:
# - each child participates once
# - two children form one dyad
# - each child contributes all six Box × Interruption combinations
# - observed fixed effects and variance components are retained


#-----------------------------------------------------------
# Prepare one six-condition profile per observed child
#-----------------------------------------------------------

# Duplicate trials are collapsed so that each child contributes
# exactly one observation per experimental combination.

power_template_max <- analysis_data |>
  dplyr::group_by(
    child_id,
    interruption_group,
    box_condition
  ) |>
  dplyr::summarise(
    high_proportion = mean(
      high_proportion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


#-----------------------------------------------------------
# Check that every observed child has six conditions
#-----------------------------------------------------------

power_template_max |>
  dplyr::count(child_id)


#-----------------------------------------------------------
# Helper function: create balanced future design
#-----------------------------------------------------------

create_balanced_power_data <- function(
    template_data,
    n_children
) {
  
  # Two children per dyad -> N must be even
  if (n_children %% 2 != 0) {
    stop("n_children must be an even number.")
  }
  
  
  # Observed child profiles
  original_children <- unique(
    template_data$child_id
  )
  
  
  # Reuse observed movement profiles across simulated children
  profile_ids <- rep(
    original_children,
    length.out = n_children
  )
  
  
  simulated_data <- dplyr::bind_rows(
    
    lapply(
      seq_len(n_children),
      function(i) {
        
        template_data |>
          dplyr::filter(
            child_id == profile_ids[i]
          ) |>
          dplyr::mutate(
            
            # Unique simulated child ID
            child_id = sprintf(
              "child_%03d",
              i
            ),
            
            # Two children per simulated dyad:
            # 1-2 = dyad 1, 3-4 = dyad 2, etc.
            dyad_id = sprintf(
              "dyad_%03d",
              ceiling(i / 2)
            )
          )
      }
    )
  )
  
  
  # Preserve factor structure from original data
  simulated_data <- simulated_data |>
    dplyr::mutate(
      
      child_id = factor(child_id),
      
      dyad_id = factor(dyad_id),
      
      interruption_group = factor(
        interruption_group,
        levels = levels(
          analysis_data$interruption_group
        )
      ),
      
      box_condition = factor(
        box_condition,
        levels = levels(
          analysis_data$box_condition
        )
      ),
      
      # Placeholder response required for makeLmer()
      delta_max_temp = 0
    )
  
  
  simulated_data
}


#-----------------------------------------------------------
# Extract fixed effects from fitted MAX model
#-----------------------------------------------------------

fixef_max_power <- lme4::fixef(
  model_base_interaction_max
)

fixef_max_power


#-----------------------------------------------------------
# Extract complete random-effect structure
#-----------------------------------------------------------

varcorr_max_power <- lme4::VarCorr(
  model_base_interaction_max
)

varcorr_max_power


# Check random-effect order
names(
  lme4::VarCorr(
    model_base_interaction_max
  )
)


#-----------------------------------------------------------
# Extract residual standard deviation
#-----------------------------------------------------------

sigma_max_power <- sigma(
  model_base_interaction_max
)

sigma_max_power


#-----------------------------------------------------------
# Helper function: create simulation model for a given N
#-----------------------------------------------------------

make_power_model_max <- function(
    n_children
) {
  
  new_data <- create_balanced_power_data(
    template_data = power_template_max,
    n_children = n_children
  )
  
  
  simr::makeLmer(
    
    delta_max_temp ~
      interruption_group * box_condition +
      high_proportion +
      (1 | child_id) +
      (1 | dyad_id),
    
    # Fixed-effect estimates from fitted MAX model
    fixef = fixef_max_power,
    
    # Random-effect variances from fitted MAX model
    VarCorr = varcorr_max_power,
    
    # Residual SD from fitted MAX model
    sigma = sigma_max_power,
    
    data = new_data
  )
}


#-----------------------------------------------------------
# Test model construction with 16 children
#-----------------------------------------------------------

model_max_N16 <- make_power_model_max(
  n_children = 16
)


#-----------------------------------------------------------
# Inspect simulated model
#-----------------------------------------------------------

summary(
  model_max_N16
)


#-----------------------------------------------------------
# Check simulated sample structure
#-----------------------------------------------------------

# Expected: 16 children × 6 conditions = 96 rows
nrow(
  simr::getData(
    model_max_N16
  )
)


# Expected: 16 children
dplyr::n_distinct(
  simr::getData(
    model_max_N16
  )$child_id
)


# Expected: 8 dyads
dplyr::n_distinct(
  simr::getData(
    model_max_N16
  )$dyad_id
)




############################################################
## Balanced sample-size power analysis
## Maximum temperature
############################################################

# Goal:
# Estimate how many children are required to reach approximately
# 80% power while preserving the intended design of two children
# per dyad and six repeated conditions per child.


#-----------------------------------------------------------
# Helper function: power at a given number of children
#-----------------------------------------------------------

estimate_power_at_n_max <- function(
    n_children,
    test,
    nsim = 300,
    alpha = 0.05,
    seed = 26082026
) {
  
  sim_model <- make_power_model_max(
    n_children = n_children
  )
  
  set.seed(seed)
  
  result <- simr::powerSim(
    sim_model,
    test = test,
    nsim = nsim,
    alpha = alpha,
    progress = FALSE
  )
  
  data.frame(
    n_children = n_children,
    n_dyads = n_children / 2,
    power = result$x / result$n
  )
}


#-----------------------------------------------------------
# Box condition: coarse sample-size search
#-----------------------------------------------------------

sample_size_box_max <- dplyr::bind_rows(
  lapply(
    c(8, 12, 16, 20, 24, 30, 40, 50, 60, 80, 100),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "box_conditionJA",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_box_max


#-----------------------------------------------------------
# High movement proportion: coarse sample-size search
#-----------------------------------------------------------

sample_size_movement_max <- dplyr::bind_rows(
  lapply(
    c(8, 12, 16, 20, 24, 30, 40, 50, 60, 80, 100),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "high_proportion",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_movement_max


#-----------------------------------------------------------
# Interruption condition: coarse sample-size search
#-----------------------------------------------------------

sample_size_interruption_max <- dplyr::bind_rows(
  lapply(
    c(8, 12, 16, 20, 24, 30, 40, 50, 60, 80, 100),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "interruption_group",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_interruption_max


#-----------------------------------------------------------
# Interruption × Box interaction: coarse sample-size search
#-----------------------------------------------------------

sample_size_interaction_max <- dplyr::bind_rows(
  lapply(
    c(8, 12, 16, 20, 24, 30, 40, 50, 60, 80, 100),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "interruption_group:box_condition",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_interaction_max


############################################################
## Extended sample-size search:
## Interruption × Box interaction
############################################################

sample_size_interaction_max_2 <- dplyr::bind_rows(
  lapply(
    c(120, 150, 200, 250, 300, 400, 500),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "interruption_group:box_condition",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_interaction_max_2





############################################################
## Fine sample-size search: Box condition
############################################################

sample_size_box_max_fine <- dplyr::bind_rows(
  lapply(
    seq(50, 64, by = 2),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "box_conditionJA",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_box_max_fine


############################################################
## Fine sample-size search: High movement proportion
############################################################

sample_size_movement_max_fine <- dplyr::bind_rows(
  lapply(
    seq(24, 32, by = 2),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "high_proportion",
          method = "t"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_movement_max_fine


############################################################
## Fine sample-size search: Interruption condition
############################################################

sample_size_interruption_max_fine <- dplyr::bind_rows(
  lapply(
    seq(68, 82, by = 2),
    function(n) {
      
      estimate_power_at_n_max(
        n_children = n,
        test = simr::fixed(
          "interruption_group",
          method = "anova"
        ),
        nsim = 300
      )
    }
  )
)

sample_size_interruption_max_fine



############################################################
## Final sample-size confirmation with 1000 simulations
## Maximum temperature
############################################################


#-----------------------------------------------------------
# Box condition: N = 60
#-----------------------------------------------------------

box_sample_size_max_final <- estimate_power_at_n_max(
  n_children = 60,
  test = simr::fixed(
    "box_conditionJA",
    method = "t"
  ),
  nsim = 1000
)

box_sample_size_max_final


#-----------------------------------------------------------
# High movement proportion: N = 28
#-----------------------------------------------------------

movement_sample_size_max_final <- estimate_power_at_n_max(
  n_children = 28,
  test = simr::fixed(
    "high_proportion",
    method = "t"
  ),
  nsim = 1000
)

movement_sample_size_max_final


#-----------------------------------------------------------
# Interruption condition: N = 70
#-----------------------------------------------------------

interruption_sample_size_max_final <- estimate_power_at_n_max(
  n_children = 70,
  test = simr::fixed(
    "interruption_group",
    method = "anova"
  ),
  nsim = 1000
)

interruption_sample_size_max_final


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

# No practically feasible sample size within the examined range
# reached 80% power.
#
# Even at N = 500 children (250 dyads), estimated power was
# approximately 17%, reflecting the very small observed
# interaction effect.



#-----------------------------------------------------------
# Box condition: next sample-size candidate
#-----------------------------------------------------------

box_sample_size_max_final_2 <- estimate_power_at_n_max(
  n_children = 62,
  test = simr::fixed(
    "box_conditionJA",
    method = "t"
  ),
  nsim = 1000
)

box_sample_size_max_final_2


#-----------------------------------------------------------
# High movement proportion: next sample-size candidate
#-----------------------------------------------------------

movement_sample_size_max_final_2 <- estimate_power_at_n_max(
  n_children = 30,
  test = simr::fixed(
    "high_proportion",
    method = "t"
  ),
  nsim = 1000
)

movement_sample_size_max_final_2


#-----------------------------------------------------------
# Interruption condition: next sample-size candidate
#-----------------------------------------------------------

interruption_sample_size_max_final_2 <- estimate_power_at_n_max(
  n_children = 76,
  test = simr::fixed(
    "interruption_group",
    method = "anova"
  ),
  nsim = 1000
)

interruption_sample_size_max_final_2

#-----------------------------------------------------------
# Final sample-size results
#-----------------------------------------------------------

# Final simulations with 1000 iterations indicated that approximately
# 80% power would be achieved with:
#
# Box Condition:
# 62 children (31 dyads), power = .812.
#
# High Movement Proportion:
# 30 children (15 dyads), power = .820.
#
# Interruption Condition:
# 76 children (38 dyads), power = .806.
#
# For the Interruption Condition × Box Condition interaction,
# 80% power was not reached within the examined range.
# Even with 500 children (250 dyads), estimated power remained below 20%.
#
# Thus, the current design would require substantially larger samples
# to detect the observed Box and Interruption effects with adequate power,
# whereas the observed movement effect would require a smaller, but still
# considerably larger, sample than the current N = 8.
#
# The observed Interruption × Box interaction was extremely small, and
# therefore could not be detected with 80% power within a practically
# meaningful sample-size range.












############################################################
## Parallel setup
############################################################

library(parallel)

N_CORES <- 6

cl <- parallel::makeCluster(
  N_CORES
)

parallel::clusterEvalQ(
  cl,
  {
    library(lme4)
    library(lmerTest)
    library(simr)
    library(dplyr)
  }
)


############################################################
## Simulation-based power and sensitivity analysis
## Minimum temperature
## Parallel computation using 6 workers
############################################################

############################################################
## 1. Current power
############################################################

#-----------------------------------------------------------
# Define the four effects to be tested
#-----------------------------------------------------------

power_tests_min <- list(
  
  Box = list(
    term = "box_conditionJA",
    method = "t"
  ),
  
  Movement = list(
    term = "high_proportion",
    method = "t"
  ),
  
  Interruption = list(
    term = "interruption_group",
    method = "anova"
  ),
  
  Interaction = list(
    term = "interruption_group:box_condition",
    method = "anova"
  )
)


#-----------------------------------------------------------
# Export MIN model to parallel workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "model_base_interaction_min",
    "power_tests_min",
    "POWER_ALPHA",
    "POWER_SEED"
  ),
  envir = .GlobalEnv
)

parallel::clusterExport(
  cl,
  varlist = c(
    "model_base_interaction_min",
    "power_tests_min",
    "POWER_ALPHA",
    "POWER_SEED"
  ),
  envir = .GlobalEnv
)


#-----------------------------------------------------------
# Run the four power analyses in parallel
#-----------------------------------------------------------

power_min_list <- parallel::parLapply(
  cl,
  names(power_tests_min),
  function(effect_name) {
    
    spec <- power_tests_min[[effect_name]]
    
    set.seed(
      POWER_SEED +
        match(
          effect_name,
          names(power_tests_min)
        )
    )
    
    result <- simr::powerSim(
      model_base_interaction_min,
      test = simr::fixed(
        spec$term,
        method = spec$method
      ),
      nsim = 1000,
      alpha = POWER_ALPHA,
      progress = FALSE
    )
    
    ci <- stats::binom.test(
      result$x,
      result$n
    )$conf.int
    
    data.frame(
      Effect = effect_name,
      Power = result$x / result$n,
      CI_low = ci[1],
      CI_high = ci[2]
    )
  }
)


#-----------------------------------------------------------
# Combine current-power results
#-----------------------------------------------------------

power_min_current <- dplyr::bind_rows(
  power_min_list
)

power_min_current

#-----------------------------------------------------------
# Results / interpretation
#-----------------------------------------------------------

# Simulation-based power analyses for the minimum-temperature
# interaction model indicated low statistical power for all
# examined effects.
#
# Estimated power was 35.3% for Box Condition, 47.3% for
# High Movement Proportion, 12.8% for Interruption Condition,
# and 11.8% for the Interruption Condition × Box Condition
# interaction.
#
# Thus, none of the examined effects reached the conventional
# target of 80% power. Power was highest for High Movement
# Proportion and lowest for the interaction effect.
#
# Because these simulations are based on effect estimates from
# the fitted model, they should be interpreted as achieved power
# conditional on the observed model estimates.




############################################################
## Sensitivity analysis
## Minimum temperature
## Parallel computation
############################################################

#-----------------------------------------------------------
# Helper function:
# estimate power after scaling one or more coefficients
#-----------------------------------------------------------

estimate_power_for_scaled_effect_parallel <- function(
    model,
    coefficient_names,
    multiplier,
    test_term,
    test_method,
    nsim = 300,
    alpha = 0.05,
    seed = 26082026
) {
  
  sim_model <- model
  
  original_fixef <- lme4::fixef(model)
  
  new_fixef <- original_fixef
  
  new_fixef[coefficient_names] <-
    original_fixef[coefficient_names] * multiplier
  
  simr::fixef(sim_model) <- new_fixef
  
  set.seed(
    seed + round(multiplier * 100)
  )
  
  result <- simr::powerSim(
    sim_model,
    test = simr::fixed(
      test_term,
      method = test_method
    ),
    nsim = nsim,
    alpha = alpha,
    progress = FALSE
  )
  
  data.frame(
    multiplier = multiplier,
    power = result$x / result$n,
    N_errors = nrow(result$errors),
    N_warnings = nrow(result$warnings)
  )
}

#-----------------------------------------------------------
# Coefficient names for multi-parameter effects
#-----------------------------------------------------------

interruption_terms_min <- c(
  "interruption_groupa",
  "interruption_groupdp"
)

interaction_terms_min <- names(
  lme4::fixef(
    model_base_interaction_min
  )
)[
  grepl(
    "box_conditionJA.*interruption_group|interruption_group.*box_conditionJA",
    names(
      lme4::fixef(
        model_base_interaction_min
      )
    )
  )
]

interaction_terms_min


#-----------------------------------------------------------
# Export required objects and helper function to workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "model_base_interaction_min",
    "estimate_power_for_scaled_effect_parallel",
    "interruption_terms_min",
    "interaction_terms_min",
    "POWER_ALPHA",
    "POWER_SEED"
  ),
  envir = .GlobalEnv
)


#-----------------------------------------------------------
# Box condition: coarse sensitivity search
#-----------------------------------------------------------

box_multiplier_min <- seq(
  1,
  5,
  by = 0.5
)

box_grid_min_list <- parallel::parLapply(
  cl,
  box_multiplier_min,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = "box_conditionJA",
      multiplier = mult,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

box_grid_min <- dplyr::bind_rows(
  box_grid_min_list
)

box_grid_min


#-----------------------------------------------------------
# High movement proportion: coarse sensitivity search
#-----------------------------------------------------------

movement_multiplier_min <- seq(
  1,
  5,
  by = 0.5
)

movement_grid_min_list <- parallel::parLapply(
  cl,
  movement_multiplier_min,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = "high_proportion",
      multiplier = mult,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

movement_grid_min <- dplyr::bind_rows(
  movement_grid_min_list
)

movement_grid_min


#-----------------------------------------------------------
# Interruption condition: coarse sensitivity search
#-----------------------------------------------------------

interruption_multiplier_min <- seq(
  1,
  6,
  by = 0.5
)

interruption_grid_min_list <- parallel::parLapply(
  cl,
  interruption_multiplier_min,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = interruption_terms_min,
      multiplier = mult,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

interruption_grid_min <- dplyr::bind_rows(
  interruption_grid_min_list
)

interruption_grid_min


#-----------------------------------------------------------
# Interruption × Box interaction: coarse sensitivity search
#-----------------------------------------------------------

interaction_multiplier_min <- c(
  1, 2, 4, 6, 8, 10,
  12, 14, 16, 18, 20,
  24, 28, 32
)

interaction_grid_min_list <- parallel::parLapply(
  cl,
  interaction_multiplier_min,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = interaction_terms_min,
      multiplier = mult,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

interaction_grid_min <- dplyr::bind_rows(
  interaction_grid_min_list
)

interaction_grid_min




############################################################
## Fine sensitivity search
## Minimum temperature
############################################################

#-----------------------------------------------------------
# Box condition: fine search
#-----------------------------------------------------------

box_multiplier_min_fine <- seq(
  1.5,
  2.0,
  by = 0.1
)

box_grid_min_fine_list <- parallel::parLapply(
  cl,
  box_multiplier_min_fine,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = "box_conditionJA",
      multiplier = mult,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

box_grid_min_fine <- dplyr::bind_rows(
  box_grid_min_fine_list
)

box_grid_min_fine


#-----------------------------------------------------------
# High movement proportion: fine search
#-----------------------------------------------------------

movement_multiplier_min_fine <- seq(
  1.5,
  2.0,
  by = 0.1
)

movement_grid_min_fine_list <- parallel::parLapply(
  cl,
  movement_multiplier_min_fine,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = "high_proportion",
      multiplier = mult,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

movement_grid_min_fine <- dplyr::bind_rows(
  movement_grid_min_fine_list
)

movement_grid_min_fine


#-----------------------------------------------------------
# Interruption condition: fine search
#-----------------------------------------------------------

interruption_multiplier_min_fine <- seq(
  2.5,
  3.0,
  by = 0.1
)

interruption_grid_min_fine_list <- parallel::parLapply(
  cl,
  interruption_multiplier_min_fine,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = interruption_terms_min,
      multiplier = mult,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

interruption_grid_min_fine <- dplyr::bind_rows(
  interruption_grid_min_fine_list
)

interruption_grid_min_fine


#-----------------------------------------------------------
# Interruption × Box interaction: fine search
#-----------------------------------------------------------

interaction_multiplier_min_fine <- seq(
  4,
  6,
  by = 0.25
)

interaction_grid_min_fine_list <- parallel::parLapply(
  cl,
  interaction_multiplier_min_fine,
  function(mult) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_min,
      coefficient_names = interaction_terms_min,
      multiplier = mult,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

interaction_grid_min_fine <- dplyr::bind_rows(
  interaction_grid_min_fine_list
)

interaction_grid_min_fine

#-----------------------------------------------------------
# Preliminary sensitivity results
#-----------------------------------------------------------

# Fine-grid sensitivity analyses indicated that approximately
# 80% power was reached when the observed effects were increased
# to the following magnitudes:
#
# Box Condition:
# approximately 2.0 times the observed effect
# (estimated power = .823).
#
# High Movement Proportion:
# approximately 1.6 times the observed effect
# (estimated power = .823).
#
# Interruption Condition:
# approximately 2.9 times the observed effect
# (estimated power = .823).
#
# Interruption Condition × Box Condition:
# approximately 5.5 times the observed interaction effect
# (estimated power = .807).
#
# These values represent preliminary threshold estimates based
# on 300 simulations and were subsequently evaluated using
# 1000 simulations for final confirmation.


############################################################
## Final sensitivity confirmation with 1000 simulations
## Minimum temperature
############################################################

#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

box_min_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_min,
  coefficient_names = "box_conditionJA",
  multiplier = 2.0,
  test_term = "box_conditionJA",
  test_method = "t",
  nsim = 1000
)

box_min_final


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

movement_min_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_min,
  coefficient_names = "high_proportion",
  multiplier = 1.6,
  test_term = "high_proportion",
  test_method = "t",
  nsim = 1000
)

movement_min_final


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

interruption_min_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_min,
  coefficient_names = interruption_terms_min,
  multiplier = 2.9,
  test_term = "interruption_group",
  test_method = "anova",
  nsim = 1000
)

interruption_min_final


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

interaction_min_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_min,
  coefficient_names = interaction_terms_min,
  multiplier = 5.5,
  test_term = "interruption_group:box_condition",
  test_method = "anova",
  nsim = 1000
)

interaction_min_final


#-----------------------------------------------------------
# Final sensitivity results
#-----------------------------------------------------------

# Final simulations with 1000 iterations confirmed that
# approximately 80% power was achieved when the observed
# effects were increased by the following factors:
#
# Box Condition:
# 2.0 × the observed effect (power = .807).
#
# High Movement Proportion:
# 1.6 × the observed effect (power = .827).
#
# Interruption Condition:
# 2.9 × the observed effect (power = .829).
#
# Interruption Condition × Box Condition:
# 5.5 × the observed interaction effect (power = .820).
#
# Thus, the current design was most sensitive to the movement
# effect, followed by the Box Condition effect. Larger effects
# would have been required for Interruption Condition and,
# especially, for the Interruption × Box interaction to achieve
# conventional 80% power.


#-----------------------------------------------------------
# Interpretation note for main effects in the interaction model
#-----------------------------------------------------------

# The model contains the interaction:
# interruption_group × box_condition.
#
# Therefore, the main effects of Interruption Condition and
# Box Condition are conditional effects rather than general
# effects averaged across all levels of the other factor.
#
# Specifically:
# - the Interruption Condition main effect is interpreted at
#   the reference level of Box Condition;
# - the Box Condition main effect is interpreted at the
#   reference level of Interruption Condition;
# - the Interruption × Box interaction tests whether the effect
#   of one factor changes across levels of the other factor.
#
# Accordingly, power estimates for the Box and Interruption
# main effects should be interpreted conditionally, whereas
# the interaction power refers to the combined
# Interruption × Box interaction effect.




############################################################
## Sample-size analysis with balanced child-dyad structure
## Minimum temperature
## Parallel computation
############################################################


#-----------------------------------------------------------
# Create balanced template from observed data
#-----------------------------------------------------------

power_template_min <- analysis_data |>
  dplyr::group_by(
    child_id,
    interruption_group,
    box_condition
  ) |>
  dplyr::summarise(
    high_proportion = mean(
      high_proportion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Check:
# Expected = 6 observations per child
power_template_min |>
  dplyr::count(child_id)


#-----------------------------------------------------------
# Create balanced child-dyad data
#-----------------------------------------------------------

create_balanced_power_data_min <- function(
    template_data,
    n_children
) {
  
  # Two children per dyad -> N must be even
  if (n_children %% 2 != 0) {
    stop("n_children must be an even number.")
  }
  
  
  original_children <- unique(
    template_data$child_id
  )
  
  
  # Reuse observed movement profiles across simulated children
  profile_ids <- rep(
    original_children,
    length.out = n_children
  )
  
  
  simulated_data <- dplyr::bind_rows(
    
    lapply(
      seq_len(n_children),
      function(i) {
        
        template_data |>
          dplyr::filter(
            child_id == profile_ids[i]
          ) |>
          dplyr::mutate(
            
            child_id = sprintf(
              "child_%03d",
              i
            ),
            
            # Two children per dyad
            dyad_id = sprintf(
              "dyad_%03d",
              ceiling(i / 2)
            )
          )
      }
    )
  )
  
  
  simulated_data <- simulated_data |>
    dplyr::mutate(
      
      child_id = factor(child_id),
      
      dyad_id = factor(dyad_id),
      
      interruption_group = factor(
        interruption_group,
        levels = levels(
          analysis_data$interruption_group
        )
      ),
      
      box_condition = factor(
        box_condition,
        levels = levels(
          analysis_data$box_condition
        )
      ),
      
      # Placeholder response
      delta_min_temp = 0
    )
  
  
  simulated_data
}


#-----------------------------------------------------------
# Extract observed MIN model parameters
#-----------------------------------------------------------

fixef_min_power <- lme4::fixef(
  model_base_interaction_min
)

fixef_min_power


varcorr_min_power <- lme4::VarCorr(
  model_base_interaction_min
)

varcorr_min_power


sigma_min_power <- sigma(
  model_base_interaction_min
)

sigma_min_power


#-----------------------------------------------------------
# Construct MIN power model for specified sample size
#-----------------------------------------------------------

make_power_model_min <- function(
    n_children
) {
  
  new_data <- create_balanced_power_data_min(
    template_data = power_template_min,
    n_children = n_children
  )
  
  
  simr::makeLmer(
    
    delta_min_temp ~
      interruption_group * box_condition +
      high_proportion +
      (1 | child_id) +
      (1 | dyad_id),
    
    fixef = fixef_min_power,
    
    VarCorr = varcorr_min_power,
    
    sigma = sigma_min_power,
    
    data = new_data
  )
}


#-----------------------------------------------------------
# Validate simulated data structure
#-----------------------------------------------------------

model_min_N16 <- make_power_model_min(
  n_children = 16
)

summary(
  model_min_N16
)


# Expected: 16 × 6 = 96 observations
nrow(
  simr::getData(
    model_min_N16
  )
)


# Expected: 16 children
dplyr::n_distinct(
  simr::getData(
    model_min_N16
  )$child_id
)


# Expected: 8 dyads
dplyr::n_distinct(
  simr::getData(
    model_min_N16
  )$dyad_id
)


#-----------------------------------------------------------
# Estimate power at specified sample size
#-----------------------------------------------------------

estimate_power_at_n_min <- function(
    n_children,
    test_term,
    test_method,
    nsim = 300,
    alpha = 0.05,
    seed = 26082026
) {
  
  sim_model <- make_power_model_min(
    n_children = n_children
  )
  
  set.seed(
    seed + n_children
  )
  
  result <- simr::powerSim(
    sim_model,
    test = simr::fixed(
      test_term,
      method = test_method
    ),
    nsim = nsim,
    alpha = alpha,
    progress = FALSE
  )
  
  data.frame(
    n_children = n_children,
    n_dyads = n_children / 2,
    power = result$x / result$n,
    N_errors = nrow(result$errors),
    N_warnings = nrow(result$warnings)
  )
}


#-----------------------------------------------------------
# Export objects and functions to parallel workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "power_template_min",
    "create_balanced_power_data_min",
    "fixef_min_power",
    "varcorr_min_power",
    "sigma_min_power",
    "make_power_model_min",
    "estimate_power_at_n_min",
    "POWER_ALPHA",
    "POWER_SEED"
  ),
  envir = .GlobalEnv
)


#-----------------------------------------------------------
# Sample-size grid
#-----------------------------------------------------------

n_grid_min <- c(
  8, 12, 16, 20, 24,
  30, 40, 50, 60, 80, 100
)


#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

sample_size_box_min_list <- parallel::parLapply(
  cl,
  n_grid_min,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_box_min <- dplyr::bind_rows(
  sample_size_box_min_list
)

sample_size_box_min


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

sample_size_movement_min_list <- parallel::parLapply(
  cl,
  n_grid_min,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_movement_min <- dplyr::bind_rows(
  sample_size_movement_min_list
)

sample_size_movement_min


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

sample_size_interruption_min_list <- parallel::parLapply(
  cl,
  n_grid_min,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_min <- dplyr::bind_rows(
  sample_size_interruption_min_list
)

sample_size_interruption_min


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

sample_size_interaction_min_list <- parallel::parLapply(
  cl,
  n_grid_min,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_min <- dplyr::bind_rows(
  sample_size_interaction_min_list
)

sample_size_interaction_min




############################################################
## Fine and extended sample-size search
## Minimum temperature
############################################################

#-----------------------------------------------------------
# Box condition: fine search
#-----------------------------------------------------------

n_box_min_fine <- seq(
  30,
  40,
  by = 2
)

sample_size_box_min_fine_list <- parallel::parLapply(
  cl,
  n_box_min_fine,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_box_min_fine <- dplyr::bind_rows(
  sample_size_box_min_fine_list
)

sample_size_box_min_fine


#-----------------------------------------------------------
# High movement proportion: fine search
#-----------------------------------------------------------

n_movement_min_fine <- seq(
  20,
  28,
  by = 2
)

sample_size_movement_min_fine_list <- parallel::parLapply(
  cl,
  n_movement_min_fine,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_movement_min_fine <- dplyr::bind_rows(
  sample_size_movement_min_fine_list
)

sample_size_movement_min_fine


#-----------------------------------------------------------
# Interruption condition: extended coarse search
#-----------------------------------------------------------

# Power was still below 80% at N = 100.
# Therefore, extend the examined sample-size range.

n_interruption_min_extended <- c(
  120, 140, 160, 180, 200,
  220, 250, 300
)

sample_size_interruption_min_2_list <- parallel::parLapply(
  cl,
  n_interruption_min_extended,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_min_2 <- dplyr::bind_rows(
  sample_size_interruption_min_2_list
)

sample_size_interruption_min_2


#-----------------------------------------------------------
# Interruption × Box interaction: extended coarse search
#-----------------------------------------------------------

# Power was also well below 80% at N = 100.
# A broader sample-size range is therefore examined.

n_interaction_min_extended <- c(
  120, 150, 200, 250, 300,
  400, 500
)

sample_size_interaction_min_2_list <- parallel::parLapply(
  cl,
  n_interaction_min_extended,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_min_2 <- dplyr::bind_rows(
  sample_size_interaction_min_2_list
)

sample_size_interaction_min_2



############################################################
## Fine sample-size search
## Minimum temperature
############################################################

#-----------------------------------------------------------
# Interruption condition: fine search
#-----------------------------------------------------------

n_interruption_min_fine <- seq(
  160,
  180,
  by = 2
)

sample_size_interruption_min_fine_list <- parallel::parLapply(
  cl,
  n_interruption_min_fine,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_min_fine <- dplyr::bind_rows(
  sample_size_interruption_min_fine_list
)

sample_size_interruption_min_fine


#-----------------------------------------------------------
# Interruption × Box interaction: fine search
#-----------------------------------------------------------

n_interaction_min_fine <- seq(
  250,
  300,
  by = 10
)

sample_size_interaction_min_fine_list <- parallel::parLapply(
  cl,
  n_interaction_min_fine,
  function(n) {
    
    estimate_power_at_n_min(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_min_fine <- dplyr::bind_rows(
  sample_size_interaction_min_fine_list
)

sample_size_interaction_min_fine


############################################################
## Final sample-size confirmation with 1000 simulations
## Minimum temperature
############################################################


#-----------------------------------------------------------
# High movement proportion: N = 24
#-----------------------------------------------------------

movement_sample_size_min_final <- estimate_power_at_n_min(
  n_children = 24,
  test_term = "high_proportion",
  test_method = "t",
  nsim = 1000
)

movement_sample_size_min_final


#-----------------------------------------------------------
# Interruption condition: N = 166
#-----------------------------------------------------------

interruption_sample_size_min_final <- estimate_power_at_n_min(
  n_children = 166,
  test_term = "interruption_group",
  test_method = "anova",
  nsim = 1000
)

interruption_sample_size_min_final


#-----------------------------------------------------------
# Interruption × Box interaction: N = 260
#-----------------------------------------------------------

interaction_sample_size_min_final <- estimate_power_at_n_min(
  n_children = 260,
  test_term = "interruption_group:box_condition",
  test_method = "anova",
  nsim = 1000
)

interaction_sample_size_min_final


#-----------------------------------------------------------
# Box condition: N = 38
#-----------------------------------------------------------

box_sample_size_min_final <- estimate_power_at_n_min(
  n_children = 38,
  test_term = "box_conditionJA",
  test_method = "t",
  nsim = 1000
)

box_sample_size_min_final



#-----------------------------------------------------------
# Interruption × Box interaction: N = 300
#-----------------------------------------------------------

interaction_sample_size_min_final_300 <- estimate_power_at_n_min(
  n_children = 300,
  test_term = "interruption_group:box_condition",
  test_method = "anova",
  nsim = 1000
)

interaction_sample_size_min_final_300



#-----------------------------------------------------------
# Final sample-size results
#-----------------------------------------------------------

sample_size_results_min <- data.frame(
  Effect = c(
    "Box Condition",
    "Movement",
    "Interruption Condition",
    "Interruption × Box"
  ),
  N_children = c(
    38,
    24,
    166,
    300
  ),
  N_dyads = c(
    19,
    12,
    83,
    150
  ),
  Power = c(
    0.816,
    0.822,
    0.830,
    0.826
  )
)

sample_size_results_min

#-----------------------------------------------------------
# Results summary: Minimum temperature
#-----------------------------------------------------------

# At the observed sample size, simulation-based power was
# low for all tested fixed effects:
# - Box Condition: 35.3%
# - Movement: 47.3%
# - Interruption Condition: 12.8%
# - Interruption × Box interaction: 11.8%
#
# Sensitivity analyses indicated that approximately 80% power
# would be reached if the observed effects were:
# - Box Condition: 2.0 times larger
# - Movement: 1.6 times larger
# - Interruption Condition: 2.9 times larger
# - Interruption × Box interaction: 5.5 times larger
#
# Sample-size simulations based on the observed effect estimates
# and variance structure indicated that approximately:
# - 38 children (19 dyads) were required for Box Condition
#   (power = .816);
# - 24 children (12 dyads) were required for Movement
#   (power = .822);
# - 166 children (83 dyads) were required for Interruption Condition
#   (power = .830);
# - 300 children (150 dyads) were required for the
#   Interruption × Box interaction (power = .826).
#
# Note:
# These estimates are conditional on the fitted MIN interaction
# model and the assumed balanced design with two children per dyad
# and six observations per child.
#
# Because the model includes an Interruption × Box interaction,
# the Box Condition and Interruption Condition main effects are
# conditional effects and should not be interpreted as general
# effects averaged across all levels of the other factor.















############################################################
## Current simulation-based power
## Average temperature
## Parallel computation
############################################################

#-----------------------------------------------------------
# Define power tests
#-----------------------------------------------------------

power_tests_av <- list(
  
  Box = list(
    term = "box_conditionJA",
    method = "t"
  ),
  
  Movement = list(
    term = "high_proportion",
    method = "t"
  ),
  
  Interruption = list(
    term = "interruption_group",
    method = "anova"
  ),
  
  Interaction = list(
    term = "interruption_group:box_condition",
    method = "anova"
  )
)


#-----------------------------------------------------------
# Export required objects to parallel workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "model_base_interaction_av",
    "power_tests_av",
    "POWER_NSIM",
    "POWER_ALPHA",
    "POWER_SEED"
  ),
  envir = .GlobalEnv
)


#-----------------------------------------------------------
# Estimate current power in parallel
#-----------------------------------------------------------

power_av_current_list <- parallel::parLapply(
  cl,
  names(power_tests_av),
  function(effect_name) {
    
    test_info <- power_tests_av[[effect_name]]
    
    set.seed(
      POWER_SEED +
        match(
          effect_name,
          names(power_tests_av)
        )
    )
    
    result <- simr::powerSim(
      model_base_interaction_av,
      test = simr::fixed(
        test_info$term,
        method = test_info$method
      ),
      nsim = POWER_NSIM,
      alpha = POWER_ALPHA,
      progress = FALSE
    )
    
    
    ci <- stats::binom.test(
      result$x,
      result$n
    )$conf.int
    
    
    data.frame(
      Effect = effect_name,
      Power = result$x / result$n,
      CI_low = ci[1],
      CI_high = ci[2],
      N_errors = nrow(result$errors),
      N_warnings = nrow(result$warnings)
    )
  }
)


#-----------------------------------------------------------
# Combine results
#-----------------------------------------------------------

power_av_current <- dplyr::bind_rows(
  power_av_current_list
)

power_av_current


############################################################
## Sensitivity analysis
## Average temperature
## Parallel computation
############################################################

#-----------------------------------------------------------
# Define coefficient groups
#-----------------------------------------------------------

interruption_terms_av <- c(
  "interruption_groupa",
  "interruption_groupdp"
)

interaction_terms_av <- c(
  "interruption_groupa:box_conditionJA",
  "interruption_groupdp:box_conditionJA"
)


#-----------------------------------------------------------
# Export AV objects to parallel workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "model_base_interaction_av",
    "interruption_terms_av",
    "interaction_terms_av",
    "estimate_power_for_scaled_effect_parallel"
  ),
  envir = .GlobalEnv
)


############################################################
## Coarse sensitivity search
############################################################


#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

multipliers_box_av <- c(
  1, 1.5, 2, 2.5, 3
)

sensitivity_box_av_list <- parallel::parLapply(
  cl,
  multipliers_box_av,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = "box_conditionJA",
      multiplier = multiplier,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sensitivity_box_av <- dplyr::bind_rows(
  sensitivity_box_av_list
)

sensitivity_box_av


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

multipliers_movement_av <- c(
  1, 1.25, 1.5, 1.75, 2
)

sensitivity_movement_av_list <- parallel::parLapply(
  cl,
  multipliers_movement_av,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = "high_proportion",
      multiplier = multiplier,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sensitivity_movement_av <- dplyr::bind_rows(
  sensitivity_movement_av_list
)

sensitivity_movement_av


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

multipliers_interruption_av <- c(
  1, 1.5, 2, 2.5, 3, 4
)

sensitivity_interruption_av_list <- parallel::parLapply(
  cl,
  multipliers_interruption_av,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = interruption_terms_av,
      multiplier = multiplier,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sensitivity_interruption_av <- dplyr::bind_rows(
  sensitivity_interruption_av_list
)

sensitivity_interruption_av


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

multipliers_interaction_av <- c(
  1, 2, 3, 4, 5, 6, 8, 10
)

sensitivity_interaction_av_list <- parallel::parLapply(
  cl,
  multipliers_interaction_av,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = interaction_terms_av,
      multiplier = multiplier,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sensitivity_interaction_av <- dplyr::bind_rows(
  sensitivity_interaction_av_list
)

sensitivity_interaction_av




############################################################
## Fine sensitivity search
## Average temperature
############################################################

#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

multipliers_box_av_fine <- seq(
  2.0,
  2.5,
  by = 0.1
)

sensitivity_box_av_fine_list <- parallel::parLapply(
  cl,
  multipliers_box_av_fine,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = "box_conditionJA",
      multiplier = multiplier,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sensitivity_box_av_fine <- dplyr::bind_rows(
  sensitivity_box_av_fine_list
)

sensitivity_box_av_fine


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

multipliers_movement_av_fine <- seq(
  1.50,
  1.75,
  by = 0.05
)

sensitivity_movement_av_fine_list <- parallel::parLapply(
  cl,
  multipliers_movement_av_fine,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = "high_proportion",
      multiplier = multiplier,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sensitivity_movement_av_fine <- dplyr::bind_rows(
  sensitivity_movement_av_fine_list
)

sensitivity_movement_av_fine


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

multipliers_interruption_av_fine <- seq(
  2.5,
  3.0,
  by = 0.1
)

sensitivity_interruption_av_fine_list <- parallel::parLapply(
  cl,
  multipliers_interruption_av_fine,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = interruption_terms_av,
      multiplier = multiplier,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sensitivity_interruption_av_fine <- dplyr::bind_rows(
  sensitivity_interruption_av_fine_list
)

sensitivity_interruption_av_fine


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

multipliers_interaction_av_fine <- seq(
  6.0,
  8.0,
  by = 0.25
)

sensitivity_interaction_av_fine_list <- parallel::parLapply(
  cl,
  multipliers_interaction_av_fine,
  function(multiplier) {
    
    estimate_power_for_scaled_effect_parallel(
      model = model_base_interaction_av,
      coefficient_names = interaction_terms_av,
      multiplier = multiplier,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sensitivity_interaction_av_fine <- dplyr::bind_rows(
  sensitivity_interaction_av_fine_list
)

sensitivity_interaction_av_fine




############################################################
## Final sensitivity confirmation with 1000 simulations
## Average temperature
############################################################

#-----------------------------------------------------------
# Box condition: ×2.2
#-----------------------------------------------------------

sensitivity_box_av_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_av,
  coefficient_names = "box_conditionJA",
  multiplier = 2.2,
  test_term = "box_conditionJA",
  test_method = "t",
  nsim = 1000
)

sensitivity_box_av_final


#-----------------------------------------------------------
# High movement proportion: ×1.6
#-----------------------------------------------------------

sensitivity_movement_av_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_av,
  coefficient_names = "high_proportion",
  multiplier = 1.6,
  test_term = "high_proportion",
  test_method = "t",
  nsim = 1000
)

sensitivity_movement_av_final


#-----------------------------------------------------------
# Interruption condition: ×2.7
#-----------------------------------------------------------

sensitivity_interruption_av_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_av,
  coefficient_names = interruption_terms_av,
  multiplier = 2.7,
  test_term = "interruption_group",
  test_method = "anova",
  nsim = 1000
)

sensitivity_interruption_av_final


#-----------------------------------------------------------
# Interruption × Box interaction: ×6.25
#-----------------------------------------------------------

sensitivity_interaction_av_final <- estimate_power_for_scaled_effect_parallel(
  model = model_base_interaction_av,
  coefficient_names = interaction_terms_av,
  multiplier = 6.25,
  test_term = "interruption_group:box_condition",
  test_method = "anova",
  nsim = 1000
)

sensitivity_interaction_av_final




#-----------------------------------------------------------
# Final sensitivity results
#-----------------------------------------------------------

sensitivity_results_av <- data.frame(
  Effect = c(
    "Box Condition",
    "Movement",
    "Interruption Condition",
    "Interruption × Box"
  ),
  Multiplier = c(
    2.2,
    1.6,
    2.7,
    6.25
  ),
  Power = c(
    0.805,
    0.805,
    0.845,
    0.835
  )
)

sensitivity_results_av

#-----------------------------------------------------------
# Results summary: Average temperature
#-----------------------------------------------------------

# At the observed sample size, simulation-based power was
# low for all tested fixed effects:
# - Box Condition: 28.6%
# - Movement: 45.2%
# - Interruption Condition: 14.5%
# - Interruption × Box interaction: 11.3%
#
# Sensitivity analyses indicated that approximately 80% power
# would be reached if the observed effects were:
# - Box Condition: 2.2 times larger (power = .805)
# - Movement: 1.6 times larger (power = .805)
# - Interruption Condition: 2.7 times larger (power = .845)
# - Interruption × Box interaction: 6.25 times larger (power = .835)
#
# Note:
# These sensitivity estimates are conditional on the fitted
# AV interaction model and its observed variance structure.
#
# Because the model includes an Interruption × Box interaction,
# the Box Condition and Interruption Condition main effects are
# conditional effects and should not be interpreted as general
# effects averaged across all levels of the other factor.




############################################################
## Sample-size analysis with balanced child-dyad structure
## Average temperature
## Parallel computation
############################################################

#-----------------------------------------------------------
# Create balanced template from observed data
#-----------------------------------------------------------

power_template_av <- analysis_data |>
  dplyr::group_by(
    child_id,
    interruption_group,
    box_condition
  ) |>
  dplyr::summarise(
    high_proportion = mean(
      high_proportion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Check:
# Expected = 6 observations per child
power_template_av |>
  dplyr::count(child_id)


#-----------------------------------------------------------
# Create balanced child-dyad data
#-----------------------------------------------------------

create_balanced_power_data_av <- function(
    template_data,
    n_children
) {
  
  # Two children per dyad -> N must be even
  if (n_children %% 2 != 0) {
    stop("n_children must be an even number.")
  }
  
  
  original_children <- unique(
    template_data$child_id
  )
  
  
  # Reuse observed movement profiles across simulated children
  profile_ids <- rep(
    original_children,
    length.out = n_children
  )
  
  
  simulated_data <- dplyr::bind_rows(
    
    lapply(
      seq_len(n_children),
      function(i) {
        
        template_data |>
          dplyr::filter(
            child_id == profile_ids[i]
          ) |>
          dplyr::mutate(
            
            child_id = sprintf(
              "child_%03d",
              i
            ),
            
            # Two children per dyad
            dyad_id = sprintf(
              "dyad_%03d",
              ceiling(i / 2)
            )
          )
      }
    )
  )
  
  
  simulated_data <- simulated_data |>
    dplyr::mutate(
      
      child_id = factor(child_id),
      
      dyad_id = factor(dyad_id),
      
      interruption_group = factor(
        interruption_group,
        levels = levels(
          analysis_data$interruption_group
        )
      ),
      
      box_condition = factor(
        box_condition,
        levels = levels(
          analysis_data$box_condition
        )
      ),
      
      # Placeholder response
      delta_av_temp = 0
    )
  
  
  simulated_data
}


#-----------------------------------------------------------
# Extract observed AV model parameters
#-----------------------------------------------------------

fixef_av_power <- lme4::fixef(
  model_base_interaction_av
)

fixef_av_power


varcorr_av_power <- lme4::VarCorr(
  model_base_interaction_av
)

varcorr_av_power


sigma_av_power <- sigma(
  model_base_interaction_av
)

sigma_av_power


#-----------------------------------------------------------
# Construct AV power model for specified sample size
#-----------------------------------------------------------

make_power_model_av <- function(
    n_children
) {
  
  new_data <- create_balanced_power_data_av(
    template_data = power_template_av,
    n_children = n_children
  )
  
  
  simr::makeLmer(
    
    delta_av_temp ~
      interruption_group * box_condition +
      high_proportion +
      (1 | child_id) +
      (1 | dyad_id),
    
    fixef = fixef_av_power,
    
    VarCorr = varcorr_av_power,
    
    sigma = sigma_av_power,
    
    data = new_data
  )
}


#-----------------------------------------------------------
# Validate simulated data structure
#-----------------------------------------------------------

model_av_N16 <- make_power_model_av(
  n_children = 16
)

summary(
  model_av_N16
)


# Expected: 16 × 6 = 96 observations
nrow(
  simr::getData(
    model_av_N16
  )
)


# Expected: 16 children
dplyr::n_distinct(
  simr::getData(
    model_av_N16
  )$child_id
)


# Expected: 8 dyads
dplyr::n_distinct(
  simr::getData(
    model_av_N16
  )$dyad_id
)


#-----------------------------------------------------------
# Estimate power at specified sample size
#-----------------------------------------------------------

estimate_power_at_n_av <- function(
    n_children,
    test_term,
    test_method,
    nsim = 300,
    alpha = 0.05,
    seed = 26082026
) {
  
  sim_model <- make_power_model_av(
    n_children = n_children
  )
  
  
  set.seed(
    seed + n_children
  )
  
  
  result <- simr::powerSim(
    sim_model,
    test = simr::fixed(
      test_term,
      method = test_method
    ),
    nsim = nsim,
    alpha = alpha,
    progress = FALSE
  )
  
  
  data.frame(
    n_children = n_children,
    n_dyads = n_children / 2,
    power = result$x / result$n,
    N_errors = nrow(result$errors),
    N_warnings = nrow(result$warnings)
  )
}


#-----------------------------------------------------------
# Export objects and functions to parallel workers
#-----------------------------------------------------------

parallel::clusterExport(
  cl,
  varlist = c(
    "analysis_data",
    "power_template_av",
    "create_balanced_power_data_av",
    "fixef_av_power",
    "varcorr_av_power",
    "sigma_av_power",
    "make_power_model_av",
    "estimate_power_at_n_av"
  ),
  envir = .GlobalEnv
)


#-----------------------------------------------------------
# Sample-size grid
#-----------------------------------------------------------

n_grid_av <- c(
  8, 12, 16, 20, 24,
  30, 40, 50, 60, 80, 100
)


#-----------------------------------------------------------
# Box condition
#-----------------------------------------------------------

sample_size_box_av_list <- parallel::parLapply(
  cl,
  n_grid_av,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_box_av <- dplyr::bind_rows(
  sample_size_box_av_list
)

sample_size_box_av


#-----------------------------------------------------------
# High movement proportion
#-----------------------------------------------------------

sample_size_movement_av_list <- parallel::parLapply(
  cl,
  n_grid_av,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_movement_av <- dplyr::bind_rows(
  sample_size_movement_av_list
)

sample_size_movement_av


#-----------------------------------------------------------
# Interruption condition
#-----------------------------------------------------------

sample_size_interruption_av_list <- parallel::parLapply(
  cl,
  n_grid_av,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_av <- dplyr::bind_rows(
  sample_size_interruption_av_list
)

sample_size_interruption_av


#-----------------------------------------------------------
# Interruption × Box interaction
#-----------------------------------------------------------

sample_size_interaction_av_list <- parallel::parLapply(
  cl,
  n_grid_av,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_av <- dplyr::bind_rows(
  sample_size_interaction_av_list
)

sample_size_interaction_av




############################################################
## Fine and extended sample-size search
## Average temperature
############################################################

#-----------------------------------------------------------
# Box condition: fine search
#-----------------------------------------------------------

n_box_av_fine <- seq(
  40,
  50,
  by = 2
)

sample_size_box_av_fine_list <- parallel::parLapply(
  cl,
  n_box_av_fine,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "box_conditionJA",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_box_av_fine <- dplyr::bind_rows(
  sample_size_box_av_fine_list
)

sample_size_box_av_fine


#-----------------------------------------------------------
# High movement proportion: fine search
#-----------------------------------------------------------

n_movement_av_fine <- seq(
  24,
  30,
  by = 2
)

sample_size_movement_av_fine_list <- parallel::parLapply(
  cl,
  n_movement_av_fine,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "high_proportion",
      test_method = "t",
      nsim = 300
    )
  }
)

sample_size_movement_av_fine <- dplyr::bind_rows(
  sample_size_movement_av_fine_list
)

sample_size_movement_av_fine


#-----------------------------------------------------------
# Interruption condition: extended search
#-----------------------------------------------------------

n_interruption_av_extended <- c(
  110, 120, 130, 140, 150,
  160, 180, 200
)

sample_size_interruption_av_2_list <- parallel::parLapply(
  cl,
  n_interruption_av_extended,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_av_2 <- dplyr::bind_rows(
  sample_size_interruption_av_2_list
)

sample_size_interruption_av_2


#-----------------------------------------------------------
# Interruption × Box interaction: extended search
#-----------------------------------------------------------

n_interaction_av_extended <- c(
  120, 150, 200, 250, 300,
  350, 400, 500
)

sample_size_interaction_av_2_list <- parallel::parLapply(
  cl,
  n_interaction_av_extended,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_av_2 <- dplyr::bind_rows(
  sample_size_interaction_av_2_list
)

sample_size_interaction_av_2





############################################################
## Fine sample-size search
## Average temperature
############################################################

#-----------------------------------------------------------
# Interruption condition: fine search
#-----------------------------------------------------------

n_interruption_av_fine <- seq(
  110,
  120,
  by = 2
)

sample_size_interruption_av_fine_list <- parallel::parLapply(
  cl,
  n_interruption_av_fine,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interruption_av_fine <- dplyr::bind_rows(
  sample_size_interruption_av_fine_list
)

sample_size_interruption_av_fine


#-----------------------------------------------------------
# Interruption × Box interaction: fine search
#-----------------------------------------------------------

n_interaction_av_fine <- seq(
  300,
  350,
  by = 10
)

sample_size_interaction_av_fine_list <- parallel::parLapply(
  cl,
  n_interaction_av_fine,
  function(n) {
    
    estimate_power_at_n_av(
      n_children = n,
      test_term = "interruption_group:box_condition",
      test_method = "anova",
      nsim = 300
    )
  }
)

sample_size_interaction_av_fine <- dplyr::bind_rows(
  sample_size_interaction_av_fine_list
)

sample_size_interaction_av_fine




############################################################
## Final sample-size confirmation with 1000 simulations
## Average temperature
############################################################


#-----------------------------------------------------------
# Box condition: N = 50
#-----------------------------------------------------------

box_sample_size_av_final <- estimate_power_at_n_av(
  n_children = 50,
  test_term = "box_conditionJA",
  test_method = "t",
  nsim = 1000
)

box_sample_size_av_final


#-----------------------------------------------------------
# High movement proportion: N = 26
#-----------------------------------------------------------

movement_sample_size_av_final <- estimate_power_at_n_av(
  n_children = 26,
  test_term = "high_proportion",
  test_method = "t",
  nsim = 1000
)

movement_sample_size_av_final


#-----------------------------------------------------------
# Interruption condition: N = 120
#-----------------------------------------------------------

interruption_sample_size_av_final <- estimate_power_at_n_av(
  n_children = 120,
  test_term = "interruption_group",
  test_method = "anova",
  nsim = 1000
)

interruption_sample_size_av_final


#-----------------------------------------------------------
# Interruption × Box interaction: N = 350
#-----------------------------------------------------------

interaction_sample_size_av_final <- estimate_power_at_n_av(
  n_children = 350,
  test_term = "interruption_group:box_condition",
  test_method = "anova",
  nsim = 1000
)

interaction_sample_size_av_final



#-----------------------------------------------------------
# Final sample-size results
#-----------------------------------------------------------

sample_size_results_av <- data.frame(
  Effect = c(
    "Box Condition",
    "Movement",
    "Interruption Condition",
    "Interruption × Box"
  ),
  N_children = c(
    50,
    26,
    120,
    350
  ),
  N_dyads = c(
    25,
    13,
    60,
    175
  ),
  Power = c(
    0.834,
    0.849,
    0.828,
    0.824
  )
)

sample_size_results_av

#-----------------------------------------------------------
# Results summary: Average temperature
#-----------------------------------------------------------

# At the observed sample size, simulation-based power was
# low for all tested fixed effects:
# - Box Condition: 28.6%
# - Movement: 45.2%
# - Interruption Condition: 14.5%
# - Interruption × Box interaction: 11.3%
#
# Sensitivity analyses indicated that approximately 80% power
# would be reached if the observed effects were:
# - Box Condition: 2.2 times larger (power = .805)
# - Movement: 1.6 times larger (power = .805)
# - Interruption Condition: 2.7 times larger (power = .845)
# - Interruption × Box interaction: 6.25 times larger (power = .835)
#
# Sample-size simulations based on the observed effect estimates
# and variance structure indicated that approximately:
# - 50 children (25 dyads) were required for Box Condition
#   (power = .834);
# - 26 children (13 dyads) were required for Movement
#   (power = .849);
# - 120 children (60 dyads) were required for Interruption Condition
#   (power = .828);
# - 350 children (175 dyads) were required for the
#   Interruption × Box interaction (power = .824).
#
# Note:
# These estimates are conditional on the fitted AV interaction
# model and the assumed balanced design with two children per dyad
# and six observations per child.
#
# Because the model includes an Interruption × Box interaction,
# the Box Condition and Interruption Condition main effects are
# conditional effects and should not be interpreted as general
# effects averaged across all levels of the other factor.










############################################################
## Summary table: Simulation-based power analyses
############################################################

#-----------------------------------------------------------
# Combine AV, MIN, and MAX results
#-----------------------------------------------------------

power_summary_all <- data.frame(
  
  Outcome = c(
    rep("Average temperature", 4),
    rep("Minimum temperature", 4),
    rep("Maximum temperature", 4)
  ),
  
  Effect = rep(
    c(
      "Box Condition",
      "Movement",
      "Interruption Condition",
      "Interruption × Box"
    ),
    3
  ),
  
  Current_power = c(
    # Average
    0.286,
    0.452,
    0.145,
    0.113,
    
    # Minimum
    0.353,
    0.473,
    0.128,
    0.118,
    
    # Maximum
    0.238,
    0.373,
    0.206,
    0.084
  ),
  
  Sensitivity_multiplier = c(
    # Average
    2.20,
    1.60,
    2.70,
    6.25,
    
    # Minimum
    2.00,
    1.60,
    2.90,
    5.50,
    
    # Maximum
    2.50,
    1.80,
    2.80,
    19.75
  ),
  
  Sensitivity_power = c(
    # Average
    0.805,
    0.805,
    0.845,
    0.835,
    
    # Minimum
    0.807,
    0.827,
    0.829,
    0.820,
    
    # Maximum
    0.818,
    0.808,
    0.807,
    0.804
  ),
  
  Required_N_children = c(
    # Average
    "50",
    "26",
    "120",
    "350",
    
    # Minimum
    "38",
    "24",
    "166",
    "300",
    
    # Maximum
    "62",
    "30",
    "76",
    ">500"
  ),
  
  Required_N_dyads = c(
    # Average
    "25",
    "13",
    "60",
    "175",
    
    # Minimum
    "19",
    "12",
    "83",
    "150",
    
    # Maximum
    "31",
    "15",
    "38",
    ">250"
  ),
  
  Sample_size_power = c(
    # Average
    0.834,
    0.849,
    0.828,
    0.824,
    
    # Minimum
    0.816,
    0.822,
    0.830,
    0.826,
    
    # Maximum
    0.812,
    0.820,
    0.806,
    0.173
  )
)


#-----------------------------------------------------------
# Display summary table
#-----------------------------------------------------------

power_summary_all




############################################################
## Results table: Simulation-based power analyses
############################################################

#-----------------------------------------------------------
# Prepare publication table
#-----------------------------------------------------------

power_results_table <- data.frame(
  
  Outcome = c(
    rep("Average temperature", 4),
    rep("Minimum temperature", 4),
    rep("Maximum temperature", 4)
  ),
  
  Effect = rep(
    c(
      "Box Condition",
      "Movement",
      "Interruption Condition",
      "Interruption × Box"
    ),
    3
  ),
  
  Current_power = c(
    0.286, 0.452, 0.145, 0.113,
    0.353, 0.473, 0.128, 0.118,
    0.238, 0.373, 0.206, 0.084
  ),
  
  Sensitivity_multiplier = c(
    2.20, 1.60, 2.70, 6.25,
    2.00, 1.60, 2.90, 5.50,
    2.50, 1.80, 2.80, 19.75
  ),
  
  Required_N = c(
    "50", "26", "120", "350",
    "38", "24", "166", "300",
    "62", "30", "76", ">500"
  ),
  
  Dyads = c(
    "25", "13", "60", "175",
    "19", "12", "83", "150",
    "31", "15", "38", ">250"
  )
)

power_results_table



############################################################
## Results table: Simulation-based power analyses
############################################################

#-----------------------------------------------------------
# Prepare table data
#-----------------------------------------------------------

power_table_data <- data.frame(
  
  Outcome = c(
    rep("Maximum temperature", 4),
    rep("Minimum temperature", 4),
    rep("Average temperature", 4)
  ),
  
  Effect = rep(
    c(
      "Box condition",
      "High movement proportion",
      "Interruption condition",
      "Interruption × Box"
    ),
    3
  ),
  
  Current_power = c(
    # Maximum
    0.238, 0.373, 0.206, 0.084,
    
    # Minimum
    0.353, 0.473, 0.128, 0.118,
    
    # Average
    0.286, 0.452, 0.145, 0.113
  ),
  
  Sensitivity_multiplier = c(
    # Maximum
    2.50, 1.80, 2.80, 19.75,
    
    # Minimum
    2.00, 1.60, 2.90, 5.50,
    
    # Average
    2.20, 1.60, 2.70, 6.25
  ),
  
  Sensitivity_power = c(
    # Maximum
    0.818, 0.808, 0.807, 0.804,
    
    # Minimum
    0.807, 0.827, 0.829, 0.820,
    
    # Average
    0.805, 0.805, 0.845, 0.835
  ),
  
  Required_children = c(
    # Maximum
    "62", "30", "76", ">500",
    
    # Minimum
    "38", "24", "166", "300",
    
    # Average
    "50", "26", "120", "350"
  ),
  
  Required_dyads = c(
    # Maximum
    "31", "15", "38", ">250",
    
    # Minimum
    "19", "12", "83", "150",
    
    # Average
    "25", "13", "60", "175"
  ),
  
  Sample_size_power = c(
    # Maximum
    0.812, 0.820, 0.806, 0.173,
    
    # Minimum
    0.816, 0.822, 0.830, 0.826,
    
    # Average
    0.834, 0.849, 0.828, 0.824
  )
)

############################################################
# Prepare presentation version
############################################################

library(flextable)
library(officer)

power_table_display <- power_table_data |>
  dplyr::mutate(
    
    Temperature_measure = dplyr::recode(
      Outcome,
      "Maximum temperature" = "Δ maximum temperature",
      "Minimum temperature" = "Δ minimum temperature",
      "Average temperature" = "Δ average temperature"
    ),
    
    Current_power_display = sprintf(
      "%.3f",
      Current_power
    ),
    
    Sensitivity_multiplier_display = sprintf(
      "%.2f",
      Sensitivity_multiplier
    ),
    
    Sensitivity_power_display = sprintf(
      "%.3f",
      Sensitivity_power
    ),
    
    Sample_size_power_display = sprintf(
      "%.3f",
      Sample_size_power
    ),
    
    Temperature_display = dplyr::case_when(
      dplyr::row_number() == 1 ~ "Δ maximum temperature",
      dplyr::row_number() == 5 ~ "Δ minimum temperature",
      dplyr::row_number() == 9 ~ "Δ average temperature",
      TRUE ~ ""
    )
  )


#-----------------------------------------------------------
# Keep only display columns
#-----------------------------------------------------------

power_table_display_ft <- power_table_display |>
  dplyr::select(
    Temperature_display,
    Effect,
    Current_power_display,
    Sensitivity_multiplier_display,
    Sensitivity_power_display,
    Required_children,
    Required_dyads,
    Sample_size_power_display
  )


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_power <- flextable::flextable(
  power_table_display_ft
)


#-----------------------------------------------------------
# Two-level header
#-----------------------------------------------------------

table_power <- flextable::add_header_row(
  table_power,
  values = c(
    "",
    "",
    "Current power",
    "Sensitivity analysis",
    "Sample-size analysis"
  ),
  colwidths = c(
    1,
    1,
    1,
    2,
    3
  )
)

table_power <- flextable::set_header_labels(
  table_power,
  Temperature_display = "Temperature measure",
  Effect = "Effect",
  Current_power_display = "",
  Sensitivity_multiplier_display = "Multiplier",
  Sensitivity_power_display = "Power",
  Required_children = "Children",
  Required_dyads = "Dyads",
  Sample_size_power_display = "Power"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_power <- flextable::border_remove(
  table_power
)

table_power <- flextable::font(
  table_power,
  fontname = "Arial",
  part = "all"
)

table_power <- flextable::fontsize(
  table_power,
  size = 14,
  part = "all"
)

table_power <- flextable::bold(
  table_power,
  part = "header"
)

table_power <- flextable::align(
  table_power,
  align = "center",
  part = "header"
)

table_power <- flextable::line_spacing(
  table_power,
  space = 1.20,
  part = "header"
)

table_power <- flextable::padding(
  table_power,
  padding.top = 5,
  padding.bottom = 5,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

table_power <- flextable::align(
  table_power,
  j = c(
    "Temperature_display",
    "Effect"
  ),
  align = "left",
  part = "body"
)

table_power <- flextable::align(
  table_power,
  j = c(
    "Current_power_display",
    "Sensitivity_multiplier_display",
    "Sensitivity_power_display",
    "Required_children",
    "Required_dyads",
    "Sample_size_power_display"
  ),
  align = "center",
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

table_power <- flextable::hline_top(
  table_power,
  border = outer_border,
  part = "header"
)

table_power <- flextable::hline_bottom(
  table_power,
  border = outer_border,
  part = "header"
)

# Separators between temperature blocks
table_power <- flextable::hline(
  table_power,
  i = c(4, 8),
  border = inner_border,
  part = "body"
)

table_power <- flextable::hline_bottom(
  table_power,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_power <- flextable::width(
  table_power,
  j = "Temperature_display",
  width = 2.50
)

table_power <- flextable::width(
  table_power,
  j = "Effect",
  width = 2.65
)

table_power <- flextable::width(
  table_power,
  j = "Current_power_display",
  width = 1.55
)

table_power <- flextable::width(
  table_power,
  j = "Sensitivity_multiplier_display",
  width = 1.20
)

table_power <- flextable::width(
  table_power,
  j = "Sensitivity_power_display",
  width = 1.00
)

table_power <- flextable::width(
  table_power,
  j = "Required_children",
  width = 1.10
)

table_power <- flextable::width(
  table_power,
  j = "Required_dyads",
  width = 0.95
)

table_power <- flextable::width(
  table_power,
  j = "Sample_size_power_display",
  width = 1.00
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_power <- flextable::padding(
  table_power,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_power <- flextable::line_spacing(
  table_power,
  space = 1.15,
  part = "body"
)

table_power <- flextable::bold(
  table_power,
  i = c(1, 5, 9),
  j = "Temperature_display",
  bold = TRUE,
  part = "body"
)

#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_power


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_power,
  path = "tables/Table_S19_power_analysis.png",
  zoom = 3,
  expand = 10
)









packages <- c(
  "lme4",
  "lmerTest",
  "performance",
  "DHARMa",
  "permutes",
  "emmeans",
  "simr",
  "irr",
  "e1071"
)

data.frame(
  Package = packages,
  Version = sapply(
    packages,
    function(x) as.character(packageVersion(x))
  )
)






































############################################################
## Discussion summary table
############################################################

library(dplyr)
library(flextable)
library(officer)


#-----------------------------------------------------------
# Prepare table data
#-----------------------------------------------------------

discussion_summary_data <- data.frame(
  
  Prediction = c(
    "Temperature differs between interruption conditions",
    "Interruption effects depend on social context",
    "Movement affects temperature change",
    "Movement modifies the relationship between experimental context and temperature change"
  ),
  
  Analysis = c(
    "LMM + bootstrap CIs +\npermutation tests;\nemmeans\u00A0follow\u2011ups",
    "LMM + bootstrap CIs +\npermutation tests",
    "LMM + bootstrap CIs +\npermutation tests",
    "Moderation LMMs + likelihood\u2011ratio\u00A0tests"
  ),
  
  Effect_tested = c(
    "Interruption condition",
    "Interruption × Box condition",
    "High movement proportion",
    "Box × Movement; Box\u00A0×\u00A0Interruption\u00A0×\u00A0Movement"
  ),
  
  Conclusion = c(
    "Not supported",
    "Not supported",
    "Supported",
    "Partly supported"
  )
)


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_discussion_summary <- flextable::flextable(
  discussion_summary_data
)


#-----------------------------------------------------------
# Header labels
#-----------------------------------------------------------

table_discussion_summary <- flextable::set_header_labels(
  table_discussion_summary,
  Prediction = "Prediction",
  Analysis = "Analysis",
  Effect_tested = "Effect tested",
  Conclusion = "Conclusion"
)


#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

table_discussion_summary <- flextable::border_remove(
  table_discussion_summary
)

table_discussion_summary <- flextable::font(
  table_discussion_summary,
  fontname = "Arial",
  part = "all"
)

table_discussion_summary <- flextable::fontsize(
  table_discussion_summary,
  size = 14,
  part = "all"
)

table_discussion_summary <- flextable::bold(
  table_discussion_summary,
  part = "header"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

table_discussion_summary <- flextable::align(
  table_discussion_summary,
  j = "Prediction",
  align = "center",
  part = "body"
)

table_discussion_summary <- flextable::align(
  table_discussion_summary,
  j = c(
    "Analysis",
    "Effect_tested",
    "Conclusion"
  ),
  align = "center",
  part = "body"
)

table_discussion_summary <- flextable::align(
  table_discussion_summary,
  j = "Conclusion",
  align = "center",
  part = "body"
)

table_discussion_summary <- flextable::align(
  table_discussion_summary,
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

table_discussion_summary <- flextable::hline_top(
  table_discussion_summary,
  border = outer_border,
  part = "header"
)

table_discussion_summary <- flextable::hline_bottom(
  table_discussion_summary,
  border = outer_border,
  part = "header"
)

table_discussion_summary <- flextable::hline_bottom(
  table_discussion_summary,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_discussion_summary <- flextable::width(
  table_discussion_summary,
  j = "Prediction",
  width = 3.10
)

table_discussion_summary <- flextable::width(
  table_discussion_summary,
  j = "Analysis",
  width = 2.95
)

table_discussion_summary <- flextable::width(
  table_discussion_summary,
  j = "Effect_tested",
  width = 3.55
)

table_discussion_summary <- flextable::width(
  table_discussion_summary,
  j = "Conclusion",
  width = 1.45
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

table_discussion_summary <- flextable::padding(
  table_discussion_summary,
  padding.top = 8,
  padding.bottom = 8,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

table_discussion_summary <- flextable::padding(
  table_discussion_summary,
  padding.top = 5,
  padding.bottom = 5,
  padding.left = 3,
  padding.right = 3,
  part = "header"
)

table_discussion_summary <- flextable::line_spacing(
  table_discussion_summary,
  space = 1.15,
  part = "body"
)

table_discussion_summary <- flextable::line_spacing(
  table_discussion_summary,
  space = 1.20,
  part = "header"
)


#-----------------------------------------------------------
# Emphasize conclusions
#-----------------------------------------------------------

table_discussion_summary <- flextable::bold(
  table_discussion_summary,
  j = "Conclusion",
  part = "body"
)


#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_discussion_summary


#-----------------------------------------------------------
# Export PNG
#-----------------------------------------------------------

flextable::save_as_image(
  x = table_discussion_summary,
  path = "tables/Table_discussion_summary.png",
  zoom = 3,
  expand = 10
)
