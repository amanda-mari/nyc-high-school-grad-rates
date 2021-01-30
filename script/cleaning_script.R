# Libraries ---------------------------
library("tidyverse")
library("here")
library("readxl")
library("data.table")
library("naniar")
library("pdftools")
library("purrr")



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

# The "id" column is a string of characters pasted together from the other 
# columns, we do not need it

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

# temporarily changing 'group size' to be a character
combined_data$group_size <- as.character(combined_data$group_size)

combined_data <- combined_data %>% pivot_longer(!c(
  dbn, school_name, cohort_start,
  cohort_type, cohort_group
),
names_to = "group_attribute",
values_to = "value"
)

# Replacing Missing Values with NA ---------------------------


nrow(combined_data[combined_data$value == "s", ])

# Over 2 million rows do not have a numeric value for the group attribute

combined_data <- combined_data %>%
  replace_with_na(replace = list(value = "s"))

combined_data$value <- as.numeric(combined_data$value)

nrow(combined_data[is.na(combined_data$value), ])

# switching to NAs successful

# Converting Column Types ---------------------------
unique(combined_data$dbn)
unique(combined_data$school_name)
unique(combined_data$cohort_type)
unique(combined_data$cohort_group)

# first switching school names to lowercase for readability
combined_data$school_name <- tolower(combined_data$school_name)

# The above columns can be changed to factors

columns_to_factor <- c("dbn", "school_name", "cohort_type", "cohort_group")

combined_data[columns_to_factor] <- lapply(combined_data[columns_to_factor],
                                           factor)

sapply(combined_data, class)

# Obtaining School's Addresses (PDF Scraping) --------------------------
school_dbn <- as.character(unique(combined_data$dbn))

# 553 schools to find addresses for

pull_hs_dir_pdf <-list.files(path=here::here("data/raw/"),
                                        pattern="^nyc_hs_directory_.*?\\.pdf$",
                                        full.names = TRUE)

read_in_pdfs <-list()

# read all pdfs into a list and name each element of the list as the year of the high school 
# directory (which is within the file name)
for (i in seq_len(length(pull_hs_dir_pdf))) {
  read_in_pdfs[[i]] <-pdftools::pdf_data(pull_hs_dir_pdf[[i]])
  names(read_in_pdfs)[[i]] <-paste0("directory_",str_extract(pull_hs_dir_pdf[[i]], "\\d{4}"))
}

# get a column for the year vector
for (i in seq_along(read_in_pdfs)) {
  for (j in seq_along(read_in_pdfs[[i]])) {
    read_in_pdfs[[i]][[j]]$year <-names(read_in_pdfs)[[i]]
  }
}

# only want the year to repeat once in this vector
for (i in seq_along(read_in_pdfs)) {
  for (j in seq_along(read_in_pdfs[[i]])){
    for (k in 1:nrow(read_in_pdfs[[i]][[j]])) {
             if (k==1) {
             read_in_pdfs[[i]][[j]][k, "year"] <-paste(names(read_in_pdfs)[[i]])}
             else {
             read_in_pdfs[[i]][[j]][k, "year"] <-NA
               }
         }
  }
}
# remove the primary list division (by directory year)
read_in_pdfs <- read_in_pdfs %>% flatten()

#initialize lists to pull out information we need: directory years, school dbns and addresses
# using for loops
dir_text <- list()
dir_year <-list()
dir_string <- list()
dir_location <- list()


for (i in seq_along(read_in_pdfs)) {
  dir_text[[i]]<- read_in_pdfs[[i]]$text
  dir_year[[i]] <-read_in_pdfs[[i]]$year
  dir_string[[i]]<- paste(dir_text[[i]], collapse = " ")
}

dir_all_text <-as.data.frame(unlist(dir_string))
dir_year <-as.data.frame(unlist(dir_year))
dir_year$element_number <-seq_len(nrow(dir_year))
colnames(dir_year)[1] <-"directory_year"
dir_year <-dir_year[!is.na(dir_year$directory_year),]

table(dir_year$directory_year) #this is correct, 508 pages in 2011 directory and 677 in 2016

dir_info <-cbind(dir_year,dir_all_text)
colnames(dir_info)[3] <-"pdf_text"


#clean the page text based on how the directory is styled using regex
dir_info <-dir_info %>%
  filter(directory_year=="directory_2016") %>%
  str_extract(pdf_text, "(?=DBN).*?(\\d{5})")

