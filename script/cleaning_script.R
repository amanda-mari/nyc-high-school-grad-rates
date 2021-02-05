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
  "Gender", "Poverty", "Ever ELL")]

nyc_graduation_data <- lapply(
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

nyc_graduation_data <- lapply(nyc_graduation_data, setnames,
                                 old_column_names, new_column_names)

# Data Merging ---------------------------

# Merge all sheets into one

nyc_graduation_data <- nyc_graduation_data %>% reduce(full_join)

# The "id" column is a string of characters pasted together from the other
# columns, we do not need it

nyc_graduation_data <- subset(nyc_graduation_data, select = -id)

# Data Wrangling ---------------------------
# The data is currently in wide format with several columns.
# It will be easier for analysis to have it in long format.

# reorder columns first

nyc_graduation_data <- nyc_graduation_data[, c(
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
nyc_graduation_data$group_size <- as.character(nyc_graduation_data$group_size)

nyc_graduation_data <- nyc_graduation_data %>% pivot_longer(!c(
  dbn, school_name, cohort_start,
  cohort_type, cohort_group
),
names_to = "group_attribute",
values_to = "value"
)

# Replacing Missing Values with NA ---------------------------


nrow(nyc_graduation_data[nyc_graduation_data$value == "s", ])

# Over 2 million rows do not have a numeric value for the group attribute

nyc_graduation_data <- nyc_graduation_data %>%
  replace_with_na(replace = list(value = "s"))

nyc_graduation_data$value <- as.numeric(nyc_graduation_data$value)

nrow(nyc_graduation_data[is.na(nyc_graduation_data$value), ])

# switching to NAs successful

# Converting Column Types ---------------------------
unique(nyc_graduation_data$dbn)
unique(nyc_graduation_data$school_name)
unique(nyc_graduation_data$cohort_type)
unique(nyc_graduation_data$cohort_group)

# first switching school names to lowercase for readability
nyc_graduation_data$school_name <- tolower(nyc_graduation_data$school_name)

# The above columns can be changed to factors

columns_to_factor <- c("dbn", "school_name", "cohort_type", "cohort_group")

nyc_graduation_data[columns_to_factor] <- lapply(
  nyc_graduation_data[columns_to_factor],
  factor
)

sapply(nyc_graduation_data, class)

# Obtaining School's Addresses (PDF Scraping) --------------------------
school_dbn <- as.character(unique(nyc_graduation_data$dbn))

# 553 schools to find addresses for

nyc_directory_pdfs <- list.files(
  path = here::here("data/raw/"),
  pattern = "^nyc_hs_directory_.*?\\.pdf$",
  full.names = TRUE
)


nyc_directory_data <- lapply(nyc_directory_pdfs, read_pdf_data)

for (i in seq_len(length(nyc_directory_pdfs))) {
  names(nyc_directory_data)[[i]] <- 
    paste0("directory_", str_extract(nyc_directory_pdfs[[i]], "\\d{4}"))
}


# remove blank pages
nyc_directory_data <- lapply(nyc_directory_data, remove_blank_pages)



# make a column for the year vector and only have the year appear once in 
#the first row of each tibble
for (i in seq_along(nyc_directory_data)) {
  for (j in seq_along(nyc_directory_data[[i]])) {
    for (k in seq_len(nrow(nyc_directory_data[[i]][[j]]))) {
      if (k == 1) {
        nyc_directory_data[[i]][[j]][k, "year"] <- 
          paste(str_extract(names(nyc_directory_data)[[i]], "\\d{4}"))
      }
      else {
        nyc_directory_data[[i]][[j]][k, "year"] <- NA
      }
    }
  }
}

# remove the primary list division (by directory year)
nyc_directory_data <- nyc_directory_data %>% flatten()

# initialize lists to pull out information we need: 
# directory years, school dbns and addresses using for loops
nyc_directory_text <- list()
nyc_directory_year <- list()




for (i in seq_along(nyc_directory_data)) {
  nyc_directory_text[[i]] <- nyc_directory_data[[i]]$text
  nyc_directory_year[[i]] <- nyc_directory_data[[i]]$year
  nyc_directory_text[[i]] <- paste(nyc_directory_text[[i]], collapse = " ")
}

nyc_directory_text <- as.data.frame(unlist(nyc_directory_text))
nyc_directory_year <- as.data.frame(unlist(nyc_directory_year))
nyc_directory_year$element_number <- seq_len(nrow(nyc_directory_year))
colnames(nyc_directory_year)[1] <- "year"
nyc_directory_year <-nyc_directory_year[!is.na(nyc_directory_year$year), ]

table(nyc_directory_year$year) 
# this is correct, 597 non-blank pages in 2008, 508 in 2011, and 676 in 2016

nyc_directory_data <- cbind(nyc_directory_year, nyc_directory_text)
colnames(nyc_directory_data)[3] <- "pdf_text"
nyc_directory_data <-subset(nyc_directory_data, select =c("year", "pdf_text"))

# Cleaning School's Addresses --------------------------
# The two main components we want from the directory are each school's DBN and address.

# The DBN is the district borough number of each school. It is 6 characters long. The DBN starts
# with a 2 number combination for the district, then any of the following letters 'K' 'X' 'Q' 'M' 'R', 
# depending on the school's borough. Finally, the DBN ends with a 2 digit number for the school


# Each year of the directory is organized differently and not every page will have a dbn
# and/or an address on it. Therefore, we will clean the pdf text by year using regex




# Cleaning School's Addresses for 2008 --------------------------

nyc_directory_address_2008 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2008,"pdf_text"],"(?<=Address: ).*?(\\d{5})")
#some addresses still have the educational campus in them. They also have a special character


nyc_directory_dbn_2008 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2008,"pdf_text"],"[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")
nyc_directory_2008 <-as.data.frame(cbind(nyc_directory_dbn_2008, nyc_directory_address_2008))
nyc_directory_2008$year <-rep(2008, nrow(nyc_directory_2008))



colnames(nyc_directory_2008) <-c("dbn","address","year")
#nyc_directory_2008 <- nyc_directory_2008[!is.na(nyc_directory_2008$nyc_directory_address_2008), ]

# Cleaning School's Addresses for 2011 --------------------------

nyc_directory_address_2011 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2011,"pdf_text"],"(?=Address:).*?(\\d{5})")
# there is a lot of text between the second to last part of the address (i.e. road, avenue, street) and the city & zip code
# According to the NYC DOE, the letter in the middle of the DBN indicates which borough the school is in, so I will
# only extract the beginning of the address and the zipcode, and later on use the DBN to get the borough


#using common address terms like street and road to begin subsetting the string

nyc_directory_address_start_2011 <-
  str_extract(nyc_directory_address_2011,"(?<=Address: ).*?(Street|Road|Boulevard|Avenue|Place|Parkway|Drive)")

nyc_directory_address_end_2011 <-
  str_extract(nyc_directory_address_2011,"(?=, NY).*?(\\d{5})")


nyc_directory_dbn_2011 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2011,"pdf_text"],"(?<=DBN )[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")

nyc_directory_2011 <-as.data.frame(cbind(nyc_directory_dbn_2011, nyc_directory_address_start_2011, nyc_directory_address_end_2011))
nyc_directory_2011$year <-rep(2011, nrow(nyc_directory_2011))



nyc_directory_2011 <-nyc_directory_2011 %>% mutate(borough=case_when(grepl("X",nyc_directory_dbn_2011) ~ "Bronx",
                                                grepl("K",nyc_directory_dbn_2011) ~ "Brooklyn",
                                                grepl("Q",nyc_directory_dbn_2011) ~ "Queens",
                                                grepl("M",nyc_directory_dbn_2011) ~ "Manhattan",
                                                grepl("R",nyc_directory_dbn_2011) ~ "Staten Island"
                              ))

nyc_directory_2011$address <-paste(nyc_directory_2011$nyc_directory_address_start_2011, nyc_directory_2011$borough,
                                   nyc_directory_2011$nyc_directory_address_end_2011, sep = " ")

nyc_directory_2011 <-subset(nyc_directory_2011, 
                            select = -c(nyc_directory_address_start_2011, nyc_directory_address_end_2011, borough))

nyc_directory_2011 <-nyc_directory_2011[,c(1,3,2)]

colnames(nyc_directory_2011) <-c("dbn","address","year")
# Cleaning School's Addresses for 2016 --------------------------
nyc_directory_address_2016 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2016,"pdf_text"],"(?<=Address: ).*?(\\d{5})")

nyc_directory_dbn_2016 <-
  str_extract(nyc_directory_data[nyc_directory_data$year==2016,"pdf_text"],"(?<=DBN )[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")

nyc_directory_2016 <-as.data.frame(cbind(nyc_directory_dbn_2016, nyc_directory_address_2016))
nyc_directory_2016$year <-rep(2016, nrow(nyc_directory_2016))

colnames(nyc_directory_2016) <-c("dbn","address","year")





# Combining the Directory Addresses --------------------------
# I will be combining the directories back together using rbind, and if there are duplicate  DBNs, I will use the 
# cleanest address for the school

nyc_directory_data <-rbind(nyc_directory_2008, nyc_directory_2011, nyc_directory_2016)
nyc_directory_data <-nyc_directory_data[order(nyc_directory_data$dbn),]
nyc_directory_data <-nyc_directory_data[!is.na(nyc_directory_data$address),]

#remove the rows from 2011 that have addresses that start with NA i.e NA Bronx,NY

nyc_directory_data <-nyc_directory_data[!grepl("NA", nyc_directory_data$address), ]


#count the number of times a dbn appears, if it is more than once keep the most recent address for it
# since 2016 seems to be the cleanest.


  