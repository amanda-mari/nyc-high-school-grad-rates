# Libraries ---------------------------
library("tidyverse")
library("here")
library("readxl")
library("data.table")

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
  "...1","DBN","School Name", "Category",
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
  "id", "dbn","school_name", "demo_group",
  "cohort_start", "cohort_type", "demo_group_size",
  "demo_grad_total", "demo_grad_rate",
  "demo_regents_total", "demo_regents_grad_rate",
  "percent_grad_regents",
  "demo_adv_regents_total", "demo_adv_regents_grad_rate",
  "percent_grads_adv",
  "demo_nonadv_regents_total", "demo_nonadv_regents_grad_rate",
  "percent_grads_nonadv",
  "demo_local_total", "demo_local_grad_rate",
  "percent_grads_local",
  "demo_still_enrolled_total", "percent_cohort_still_enrolled",
  "demo_dropout_total", "percent_cohort_dropout"
)

hs_data <- lapply(hs_data, setnames, old_column_names, new_column_names)

# Data Merging ---------------------------

# Merge all sheets into one

combined_data <- hs_data %>% reduce(full_join)

#The "id" column is a string of characters pasted together from the other columns, we do not need it

combined_data <-subset(combined_data,select=-id)

# Data Wrangling ---------------------------
# The data is currently in wide format with several columns.
# It will be easier for analysis to have it in wide format.



