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
all_sheets <- readxl::excel_sheets(path = here::here("data/raw/raw_graduation_data.xlsx"))


# I only need the following sheets: "All," "ELL," "SWD," "Ethnicity," "Gender,"
# "Poverty," and "Ever ELL".

needed_sheets <- all_sheets[all_sheets %in% c(
  "All", "ELL", "SWD", "Ethnicity",
  "Gender", "Poverty", "Ever ELL"
)]

school_graduation_data <- lapply(
  needed_sheets,
  read_excel_sheets,
  here::here("data/raw/raw_graduation_data.xlsx")
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

school_graduation_data <- lapply(school_graduation_data, setnames,
                                 old_column_names, new_column_names)

# Data Merging ---------------------------

# Merge all sheets into one

school_graduation_data <- school_graduation_data %>% reduce(full_join)

# The "id" column is a string of characters pasted together from the other
# columns, we do not need it

school_graduation_data <- subset(school_graduation_data, select = -id)

# Data Wrangling ---------------------------
# The data is currently in wide format with several columns.
# It will be easier for analysis to have it in long format.

# reorder columns first

school_graduation_data <- school_graduation_data[, c(
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
school_graduation_data$group_size <- as.character(school_graduation_data$group_size)

school_graduation_data <- school_graduation_data %>% pivot_longer(!c(
  dbn, school_name, cohort_start,
  cohort_type, cohort_group
),
names_to = "group_attribute",
values_to = "value"
)

# Replacing Missing Values with NA ---------------------------


nrow(school_graduation_data[school_graduation_data$value == "s", ])

# Over 2 million rows do not have a numeric value for the group attribute

school_graduation_data <- school_graduation_data %>%
  replace_with_na(replace = list(value = "s"))

school_graduation_data$value <- as.numeric(school_graduation_data$value)

nrow(school_graduation_data[is.na(school_graduation_data$value), ])

# switching to NAs successful

# Converting Column Types ---------------------------
unique(school_graduation_data$dbn)
unique(school_graduation_data$school_name)
unique(school_graduation_data$cohort_type)
unique(school_graduation_data$cohort_group)

# first switching school names to lowercase for readability
school_graduation_data$school_name <- tolower(school_graduation_data$school_name)

# The above columns can be changed to factors

columns_to_factor <- c("dbn", "school_name", "cohort_type", "cohort_group")

school_graduation_data[columns_to_factor] <- lapply(
  school_graduation_data[columns_to_factor],
  factor
)

sapply(school_graduation_data, class)

# Obtaining School's Addresses (PDF Scraping) --------------------------
school_dbn <- as.character(unique(school_graduation_data$dbn))

# 553 schools to find addresses for

school_directory_pdfs <- list.files(
  path = here::here("data/raw/"),
  pattern = "^nyc_hs_directory_.*?\\.pdf$",
  full.names = TRUE
)


school_directory_data <- lapply(school_directory_pdfs, read_pdf_data)

for (i in seq_len(length(school_directory_pdfs))) {
  names(school_directory_data)[[i]] <- 
    paste0("directory_", str_extract(school_directory_pdfs[[i]], "\\d{4}"))
}


# remove blank pages
school_directory_data <- lapply(school_directory_data, remove_blank_pages)



# make a column for the year vector and only have the year appear once in 
#the first row of each tibble
for (i in seq_along(school_directory_data)) {
  for (j in seq_along(school_directory_data[[i]])) {
    for (k in seq_len(nrow(school_directory_data[[i]][[j]]))) {
      if (k == 1) {
        school_directory_data[[i]][[j]][k, "year"] <- 
          paste(str_extract(names(school_directory_data)[[i]], "\\d{4}"))
      }
      else {
        school_directory_data[[i]][[j]][k, "year"] <- NA
      }
    }
  }
}

# remove the primary list division (by directory year)
school_directory_data <- school_directory_data %>% flatten()

# initialize lists to pull out information we need: 
# directory years, school dbns and addresses using for loops
school_directory_text <- list()
school_directory_year <- list()




for (i in seq_along(school_directory_data)) {
  school_directory_text[[i]] <- school_directory_data[[i]]$text
  school_directory_year[[i]] <- school_directory_data[[i]]$year
  school_directory_text[[i]] <- paste(school_directory_text[[i]], collapse = " ")
}

school_directory_text <- as.data.frame(unlist(school_directory_text))
school_directory_year <- as.data.frame(unlist(school_directory_year))
school_directory_year$element_number <- seq_len(nrow(school_directory_year))
colnames(school_directory_year)[1] <- "year"
school_directory_year <-school_directory_year[!is.na(school_directory_year$year), ]

table(school_directory_year$year) 
# this is correct, 597 non-blank pages in 2008, 508 in 2011, and 676 in 2016

school_directory_data <- cbind(school_directory_year, school_directory_text)
colnames(school_directory_data)[3] <- "pdf_text"
school_directory_data <-subset(school_directory_data, select =c("year", "pdf_text"))

# Cleaning School's Addresses --------------------------
# The two manin components we want from the directory are each school's DBN and address.

# The DBN is the district borough number of each school. It is 6 characters long. The DBN starts
# with a 2 number combination for the district, then any of the following letters 'K' 'X' 'Q' 'M' 'R', 
# depending on the school's borough. Finally, the DBN ends with a 2 digit number for the school


# Each year of the directory is organized differently and not every page will have a dbn
# and/or an address on it. Therefore, we will clean the pdf text by year using regex


# 2008

address_directory_2008 <-
  str_extract(school_directory_data[school_directory_data$year==2008,"pdf_text"],"(?=Address:).*?(\\d{5})")
dbn_directory_2008 <-
  str_extract(school_directory_data[school_directory_data$year==2008,"pdf_text"],"[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")
school_directory_2008 <-as.data.frame(cbind(dbn_directory_2008, address_directory_2008))
school_directory_2008$year <-rep(2008, nrow(school_directory_2008))
school_directory_2008 <- school_directory_2008[!is.na(school_directory_2008$address_directory_2008), ]







