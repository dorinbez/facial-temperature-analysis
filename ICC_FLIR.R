# Title: FLIR Interrater Reliability (ICC)
# Creator: Dorin Bez
# Creation date: 01.07.26
# Operator and last date of work: Dorin Bez, 01.07.26

# install packages 
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("irr")

# load packages
library(readxl)
library(dplyr)
library(irr)

setwd("Z:/Comparative Joint Action Project/Thesis Projects/Bachelors/Dorin/FLIR_ICC")

# read Excel files
rater1 <- read_excel("data/Rater1_Dorin_FLIR_12videos.xlsx")
rater2 <- read_excel("data/Rater2_Brooklyn_FLIR_12videos.xlsx")

head(rater1)
head(rater2)

########################
# ICC Average Temperature
########################

# keep only variables needed for the ICC
rater1_av <- rater1 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         av_temp_NT) %>%
  rename(av_temp_rater1 = av_temp_NT)

rater2_av <- rater2 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         av_temp_NT) %>%
  rename(av_temp_rater2 = av_temp_NT)

# Join Rater 1 and Rater 2 data
# Merge Rater 1 and 2 data using filename, phase, datapoint to align corresponding measurements. 
icc_av_data <- inner_join( 
  rater1_av,
  rater2_av, 
  by = c("filename_thermalvideo", "phase", "id_datapoint")
)

# convert decimal commas to decimal points and convert Rater 2 temperature values to numeric.
icc_av_data <- icc_av_data %>%
  mutate(av_temp_rater2 = as.numeric(gsub(",", ".", av_temp_rater2)))

# check joined data
head(icc_av_data)
nrow(icc_av_data)
View(icc_av_data)

# calculate ICC for average nose-tip temperature
icc_av_result <- icc(
  icc_av_data[, c("av_temp_rater1", "av_temp_rater2")],
  model = "twoway",
  type = "agreement",
  unit = "single"
)

# show ICC result
icc_av_result

# Interpretation:
# ICC values closer to 1 indicate higher agreement between Rater 1 and Rater 2.
# An ICC above 0.90 is generally considered excellent. 


########################
# ICC Minimum Temperature
########################

# keep only variables needed for the ICC
rater1_min <- rater1 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         min_temp_NT) %>%
  rename(min_temp_rater1 = min_temp_NT)

rater2_min <- rater2 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         min_temp_NT) %>%
  rename(min_temp_rater2 = min_temp_NT)

# Join Rater 1 and Rater 2 data
# Merge Rater 1 and 2 data using filename, phase, datapoint to align corresponding measurements. 
icc_min_data <- inner_join( 
  rater1_min,
  rater2_min, 
  by = c("filename_thermalvideo", "phase", "id_datapoint")
)

# convert decimal commas to decimal points and convert Rater 2 minimum temperature values to numeric.
icc_min_data <- icc_min_data %>%
  mutate(min_temp_rater2 = as.numeric(gsub(",", ".", min_temp_rater2)))

# check joined data
head(icc_min_data)
nrow(icc_min_data)
View(icc_min_data)

# calculate ICC for minimum nose-tip temperature
icc_min_result <- icc(
  icc_min_data[, c("min_temp_rater1", "min_temp_rater2")],
  model = "twoway",
  type = "agreement",
  unit = "single"
)

# show ICC result
icc_min_result


########################
# ICC Maximum Temperature
########################

# keep only variables needed for the ICC
rater1_max <- rater1 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         max_temp_NT) %>%
  rename(max_temp_rater1 = max_temp_NT)

rater2_max <- rater2 %>%
  select(filename_thermalvideo,
         phase,
         id_datapoint,
         max_temp_NT) %>%
  rename(max_temp_rater2 = max_temp_NT)

# Join Rater 1 and Rater 2 data
# Merge Rater 1 and 2 data using filename, phase, datapoint to align corresponding measurements. 
icc_max_data <- inner_join( 
  rater1_max,
  rater2_max, 
  by = c("filename_thermalvideo", "phase", "id_datapoint")
)

# convert decimal commas to decimal points and convert Rater 2 maximum temperature values to numeric.
icc_max_data <- icc_max_data %>%
  mutate(max_temp_rater2 = as.numeric(gsub(",", ".", max_temp_rater2)))

# check joined data
head(icc_max_data)
nrow(icc_max_data)
View(icc_max_data)

# calculate ICC for maximum nose-tip temperature
icc_max_result <- icc(
  icc_max_data[, c("max_temp_rater1", "max_temp_rater2")],
  model = "twoway",
  type = "agreement",
  unit = "single"
)

# show ICC result
icc_max_result




























############################################################
#### Table S4: Inter-rater reliability
############################################################

library(dplyr)
library(tibble)
library(flextable)
library(officer)

#-----------------------------------------------------------
# Prepare table data
#-----------------------------------------------------------

irr_table_data <- tibble::tibble(
  
  Coding = c(
    "Thermal coding",
    "",
    "",
    "Movement coding",
    "",
    "",
    ""
  ),
  
  Measure = c(
    "Maximum temperature",
    "Minimum temperature",
    "Average temperature",
    "Individual movement categories",
    "Individual movement categories",
    "High/Low movement classification",
    "High/Low movement classification"
  ),
  
  Comparison = c(
    "",
    "",
    "",
    "Including no matches",
    "Excluding no matches",
    "Including no matches",
    "Excluding no matches"
  ),
  
  `ICC(A,1)` = c(
    "0.981",
    "0.997",
    "0.995",
    "",
    "",
    "",
    ""
  ),
  
  `95% CI` = c(
    "[0.973, 0.987]",
    "[0.995, 0.998]",
    "[0.991, 0.997]",
    "",
    "",
    "",
    ""
  ),
  
  `Raw agreement` = c(
    "",
    "",
    "",
    "0.65",
    "0.84",
    "0.70",
    "0.92"
  ),
  
  `κ` = c(
    "",
    "",
    "",
    "0.61",
    "0.82",
    "0.49",
    "0.81"
  ),
  
  `κmax` = c(
    "",
    "",
    "",
    "0.88",
    "0.92",
    "0.88",
    "0.97"
  )
)


#-----------------------------------------------------------
# Create flextable
#-----------------------------------------------------------

table_irr <- flextable::flextable(
  irr_table_data
)


# Merge the Thermal coding label across the three thermal rows
table_irr <- flextable::merge_at(
  table_irr,
  i = 1:3,
  j = "Coding",
  part = "body"
)

#-----------------------------------------------------------
# Basic formatting
#-----------------------------------------------------------

# Remove default borders
table_irr <- flextable::border_remove(
  table_irr
)

# Font
table_irr <- flextable::font(
  table_irr,
  fontname = "Arial",
  part = "all"
)

table_irr <- flextable::fontsize(
  table_irr,
  size = 14,
  part = "all"
)

# Bold column headers
table_irr <- flextable::bold(
  table_irr,
  part = "header"
)

# Bold group labels
table_irr <- flextable::bold(
  table_irr,
  i = c(1, 4),
  j = "Coding",
  part = "body"
)


#-----------------------------------------------------------
# Alignment
#-----------------------------------------------------------

# Left-align descriptive text columns
table_irr <- flextable::align(
  table_irr,
  j = c(
    "Coding",
    "Measure",
    "Comparison"
  ),
  align = "left",
  part = "all"
)

# Center-align numerical columns
table_irr <- flextable::align(
  table_irr,
  j = c(
    "ICC(A,1)",
    "95% CI",
    "Raw agreement",
    "κ",
    "κmax"
  ),
  align = "center",
  part = "all"
)

# Center header labels
table_irr <- flextable::align(
  table_irr,
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
table_irr <- flextable::hline_top(
  table_irr,
  border = outer_border,
  part = "header"
)

# Line below header
table_irr <- flextable::hline_bottom(
  table_irr,
  border = outer_border,
  part = "header"
)

# Separator before Movement coding
table_irr <- flextable::border(
  table_irr,
  i = 4,
  border.top = inner_border,
  part = "body"
)

# Bottom line
table_irr <- flextable::hline_bottom(
  table_irr,
  border = outer_border,
  part = "body"
)


#-----------------------------------------------------------
# Column widths
#-----------------------------------------------------------

table_irr <- flextable::width(
  table_irr,
  j = "Coding",
  width = 1.25
)

table_irr <- flextable::width(
  table_irr,
  j = "Measure",
  width = 2.15
)

table_irr <- flextable::width(
  table_irr,
  j = "Comparison",
  width = 1.65
)

table_irr <- flextable::width(
  table_irr,
  j = "ICC(A,1)",
  width = 0.85
)

table_irr <- flextable::width(
  table_irr,
  j = "95% CI",
  width = 1.35
)

table_irr <- flextable::width(
  table_irr,
  j = "Raw agreement",
  width = 1.10
)

table_irr <- flextable::width(
  table_irr,
  j = "κ",
  width = 0.55
)

table_irr <- flextable::width(
  table_irr,
  j = "κmax",
  width = 0.65
)


#-----------------------------------------------------------
# Spacing
#-----------------------------------------------------------

# General spacing
table_irr <- flextable::padding(
  table_irr,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

# More space between wrapped text lines
table_irr <- flextable::line_spacing(
  table_irr,
  space = 1.15,
  part = "body"
)

# Extra spacing for group starts
table_irr <- flextable::padding(
  table_irr,
  i = c(1, 4),
  padding.top = 6,
  padding.bottom = 4,
  part = "body"
)

# Align body text at the top of each cell
table_irr <- flextable::valign(
  table_irr,
  valign = "top",
  part = "body"
)

# Keep the merged Thermal coding cell aligned at the top
table_irr <- flextable::valign(
  table_irr,
  i = 1,
  j = "Coding",
  valign = "top",
  part = "body"
)

#-----------------------------------------------------------
# Display table
#-----------------------------------------------------------

table_irr


#-----------------------------------------------------------
# Export as high-resolution PNG
#-----------------------------------------------------------

dir.create(
  "tables",
  showWarnings = FALSE,
  recursive = TRUE
)

flextable::save_as_image(
  x = table_irr,
  path = "tables/Table_S4_interrater_reliability.png",
  zoom = 3,
  expand = 10
)

