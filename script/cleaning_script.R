# Libraries ---------------------------
library("tidyverse")
library("here")
library("readxl")
library("data.table")
library("naniar")

# Functions ---------------------------
source("script/functions_script.R")

# Load data ---------------------------
all_sheets <- readxl::excel_sheets(path = here::here("data/raw/raw_data.xlsx"))


# I only need the following sheets: "All," "ELL," "SWD," "Ethnicity," "Gender,"
# "Poverty," and "Ever ELL".

needed_sheets <- all_sheets[all_sheets %in% c(
  "All", "ELL", "SWD", "Ethnicity",
  "Gender", "Poverty", "Ever ELL"
)]

hs_data <- lapply(
  needed_sheets,
  read_excel_sheets,
  here::here("data/raw/raw_data.xlsx")
)

# Rename Columns ---------------------------

# Each sheet has the same column names in the same order
old_column_names <- c(
  "...1", "DBN", "School Name", "Category",
  "Cohort Year", "Cohort", "# Total Cohort",
  "# Grads", "% Grads", "# Total Regents",
  "% Total Regents of Cohort",
  "% Total Regents of Grads",
  "# Advanced Regents",
  "% Advanced Regents of Cohort",
  "% Advanced Regents of Grads",
  "# Regents without Advanced",
  "% Regents without Advanced of Cohort",
  "% Regents without Advanced of Grads",
  "# Local", "% Local of Cohort",
  "% Local of Grads", "# Still Enrolled",
  "% Still Enrolled", "# Dropout",
  "% Dropout"
)

new_column_names <- c(
  "id", "dbn", "school_name", "cohort_group",
  "cohort_start", "cohort_type", "group_size",
  "group_grad_total", "group_grad_rate",
  "group_regents_total", "group_regents_grad_rate",
  "percent_grad_regents",
  "group_adv_regents_total", "group_adv_regents_grad_rate",
  "percent_grads_adv",
  "group_nonadv_regents_total", "group_nonadv_regents_grad_rate",
  "percent_grads_nonadv",
  "group_local_total", "group_local_grad_rate",
  "percent_grads_local",
  "group_still_enrolled_total", "percent_cohort_still_enrolled",
  "group_dropout_total", "percent_cohort_dropout"
)

hs_data <- lapply(hs_data, setnames, old_column_names, new_column_names)

# Data Merging ---------------------------

# Merge all sheets into one

combined_data <- hs_data %>% reduce(full_join)

# The "id" column is a string of characters pasted together from the other columns, we do not need it

combined_data <- subset(combined_data, select = -id)

# Data Wrangling ---------------------------
# The data is currently in wide format with several columns.
# It will be easier for analysis to have it in long format.

# reorder columns first

combined_data <- combined_data[, c(
  "dbn", "school_name", "cohort_start", "cohort_type",
  "cohort_group", "group_size", "group_grad_total",
  "group_grad_rate", "group_regents_total",
  "group_regents_grad_rate", "percent_grad_regents",
  "group_adv_regents_total", "group_adv_regents_grad_rate",
  "percent_grads_adv", "group_nonadv_regents_total",
  "group_nonadv_regents_grad_rate", "percent_grads_nonadv",
  "group_local_total", "group_local_grad_rate", "percent_grads_local",
  "group_still_enrolled_total", "percent_cohort_still_enrolled",
  "group_dropout_total", "percent_cohort_dropout"
)]

#temporarily changing 'group size' to be a character
combined_data$group_size <-as.character(combined_data$group_size)

combined_data <- combined_data %>% pivot_longer(!c(dbn, school_name, cohort_start,
                                                   cohort_type, cohort_group),
                                                    names_to="group_attribute", 
                                                     values_to= "value" )

# Replacing Missing Values with NA ---------------------------


nrow(combined_data[combined_data$value=="s",])

# Over 2 million rows do not have a numeric value for the group attribute

combined_data <- combined_data %>%
  replace_with_na(replace = list(value = "s"))

combined_data$value <-as.numeric(combined_data$value)

nrow(combined_data[is.na(combined_data$value),])

#switching to NAs successful

# Converting Column Types ---------------------------
unique(combined_data$dbn)
unique(combined_data$school_name)
unique(combined_data$cohort_type)
unique(combined_data$cohort_group)

#first switching school names to lowercase for readability
combined_data$school_name <-tolower(combined_data$school_name)

#The above columns can be changed to factors

columns_to_factor <-c("dbn", "school_name", "cohort_type", "cohort_group")

combined_data[columns_to_factor] <-lapply(combined_data[columns_to_factor], factor)

sapply(combined_data,class)

