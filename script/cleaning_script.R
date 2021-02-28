# Libraries ---------------------------
library("tidyverse")
library("here")
library("readxl")
library("data.table")
library("naniar")
library("pdftools")
library("rvest")
library("tmaptools")
library("openxlsx")

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

hs_grad_data <- lapply(
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

hs_grad_data <- lapply(
  hs_grad_data, setnames,
  old_column_names, new_column_names
)

# Data Merging ---------------------------

# Merge all sheets into one

hs_grad_data <- hs_grad_data %>% reduce(full_join)

# The "id" column is a string of characters pasted together from the other
# columns, we do not need it

hs_grad_data <- subset(hs_grad_data, select = -id)

# Data Wrangling ---------------------------
# The data is currently in wide format with several columns.
# We want it in long format

# reorder columns first

hs_grad_data <- hs_grad_data[, c(
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
hs_grad_data$group_size <- as.character(hs_grad_data$group_size)

hs_grad_data <- hs_grad_data %>% pivot_longer(!c(
  dbn, school_name, cohort_start,
  cohort_type, cohort_group
),
names_to = "group_attribute",
values_to = "value"
)

# Replacing Missing Values with NA ---------------------------


nrow(hs_grad_data[hs_grad_data$value == "s", ])

# Over 2 million rows do not have a numeric value for the group attribute

hs_grad_data <- hs_grad_data %>%
  replace_with_na(replace = list(value = "s"))

hs_grad_data$value <- as.numeric(hs_grad_data$value)

nrow(hs_grad_data[is.na(hs_grad_data$value), ])

# switching to NAs successful

# Converting Column Types ---------------------------
unique(hs_grad_data$dbn)
unique(hs_grad_data$school_name)
unique(hs_grad_data$cohort_type)
unique(hs_grad_data$cohort_group)

# first switching school names to lowercase for readability
hs_grad_data$school_name <- tolower(hs_grad_data$school_name)

# The above columns can be changed to factors

columns_to_factor <- c("dbn", "school_name", "cohort_type", "cohort_group")

hs_grad_data[columns_to_factor] <- lapply(
  hs_grad_data[columns_to_factor],
  factor
)

sapply(hs_grad_data, class)

# Obtaining School's Addresses (PDF Scraping) --------------------------
school_dbn <- as.character(unique(hs_grad_data$dbn))

# 553 schools to find addresses for

hs_dir_pdfs <- list.files(
  path = here::here("data/raw/"),
  pattern = "^nyc_hs_directory_.*?\\.pdf$",
  full.names = TRUE
)


hs_dir_data <- lapply(hs_dir_pdfs, read_pdf_data)

for (i in seq_len(length(hs_dir_pdfs))) {
  names(hs_dir_data)[[i]] <-
    paste0("directory_", str_extract(hs_dir_pdfs[[i]], "\\d{4}"))
}


# remove blank pages
hs_dir_data <- lapply(hs_dir_data, remove_blank_pages)



# make a column for the year vector and only have the year appear once in
# the first row of each tibble
for (i in seq_along(hs_dir_data)) {
  for (j in seq_along(hs_dir_data[[i]])) {
    for (k in seq_len(nrow(hs_dir_data[[i]][[j]]))) {
      if (k == 1) {
        hs_dir_data[[i]][[j]][k, "year"] <-
          paste(str_extract(names(hs_dir_data)[[i]], "\\d{4}"))
      }
      else {
        hs_dir_data[[i]][[j]][k, "year"] <- NA
      }
    }
  }
}

# remove the primary list division (by directory year)
hs_dir_data <- hs_dir_data %>% flatten()

# initialize lists to pull out information we need:
# directory years, school dbns, and addresses by using for loops
hs_dir_text <- list()
hs_dir_year <- list()




for (i in seq_along(hs_dir_data)) {
  hs_dir_text[[i]] <- hs_dir_data[[i]]$text
  hs_dir_year[[i]] <- hs_dir_data[[i]]$year
  hs_dir_text[[i]] <- paste(hs_dir_text[[i]], collapse = " ")
}

hs_dir_text <- as.data.frame(unlist(hs_dir_text))
hs_dir_year <- as.data.frame(unlist(hs_dir_year))
hs_dir_year$element_number <- seq_len(nrow(hs_dir_year))
colnames(hs_dir_year)[1] <- "year"
hs_dir_year <- hs_dir_year[!is.na(hs_dir_year$year), ]

table(hs_dir_year$year)
# this is correct, 597 non-blank pages in 2008, 508 in 2011, and 676 in 2016

hs_dir_data <- cbind(hs_dir_year, hs_dir_text)
colnames(hs_dir_data)[3] <- "pdf_text"
hs_dir_data <- subset(hs_dir_data, select = c("year", "pdf_text"))

# Cleaning School's Addresses --------------------------
# The two main components we want from the directory are each school's DBN and address.

# The DBN is the district borough number of each school. It is 6 characters long. The DBN starts
# with a 2 number combination for the district, then any of the following letters
# 'K' 'X' 'Q' 'M' 'R', depending on the school's borough.
# Finally, the DBN ends with a 2 digit number for the school


# Each year of the directory is organized differently and not every page will have a dbn
# and/or an address on it. Therefore, we will clean the pdf text by year using regex

# It is okay if every address is not perfectly cleaned, since we will standardize the addresses
# later on



# Cleaning School's Addresses for 2008 --------------------------

hs_dir_address_2008 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2008, "pdf_text"], "(?<=Address: ).*?(\\d{5})")
# some addresses still have the educational campus in them. They also have a special character

hs_dir_address_2008 <-
  str_extract(hs_dir_address_2008, "(?:[0-9]){0,3}.*")
hs_dir_address_2008 <- gsub("\\a", "", hs_dir_address_2008)

hs_dir_address_2008 <-
  str_extract(hs_dir_address_2008, "(?:[0-9]){0,3}.*")

hs_dir_address_2008 <- gsub(".*Campus|.*Center", "", hs_dir_address_2008)

hs_dir_address_2008 <- gsub("\\s{2}", "", hs_dir_address_2008, perl = TRUE)

hs_dir_address_2008 <- gsub("(\\s\\d{1,4})\\s{1,2}(\\d+)(?!\\D)", "", hs_dir_address_2008, perl = TRUE)

hs_dir_address_2008 <- trimws(hs_dir_address_2008)


hs_dir_dbn_2008 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2008, "pdf_text"], "[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")


hs_dir_2008 <- as.data.frame(cbind(hs_dir_dbn_2008, hs_dir_address_2008))


hs_dir_2008$year <- rep(2008, nrow(hs_dir_2008))



colnames(hs_dir_2008) <- c("dbn", "address", "year")


# Cleaning School's Addresses for 2011 --------------------------

hs_dir_address_2011 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2011, "pdf_text"], "(?=Address:).*?(\\d{5})")
# there is a lot of text between the second to last part of the address (i.e. road, avenue, street)
# and the city & zip code. According to the NYC DOE, the letter in the middle of the DBN
# indicates which borough the school is in, so I will only extract the beginning of
# the address and the zipcode, and later on use the DBN to get the borough


# using common address terms like street and road to begin subsetting the string

hs_dir_address_start_2011 <-
  str_extract(hs_dir_address_2011, "(?<=Address: ).*?(Street|Road|Boulevard|Avenue|Place|Parkway|Drive)")

hs_dir_address_end_2011 <-
  str_extract(hs_dir_address_2011, "(?=, NY).*?(\\d{5})")


hs_dir_dbn_2011 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2011, "pdf_text"], "(?<=DBN )[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")

hs_dir_2011 <- as.data.frame(cbind(hs_dir_dbn_2011, hs_dir_address_start_2011, hs_dir_address_end_2011))
hs_dir_2011$year <- rep(2011, nrow(hs_dir_2011))



hs_dir_2011 <- hs_dir_2011 %>% mutate(borough = case_when(
  grepl("X", hs_dir_dbn_2011) ~ "Bronx",
  grepl("K", hs_dir_dbn_2011) ~ "Brooklyn",
  grepl("Q", hs_dir_dbn_2011) ~ "Queens",
  grepl("M", hs_dir_dbn_2011) ~ "Manhattan",
  grepl("R", hs_dir_dbn_2011) ~ "Staten Island"
))

hs_dir_2011$address <- paste(hs_dir_2011$hs_dir_address_start_2011, hs_dir_2011$borough,
  hs_dir_2011$hs_dir_address_end_2011,
  sep = " "
)

hs_dir_2011 <- subset(hs_dir_2011,
  select = -c(hs_dir_address_start_2011, hs_dir_address_end_2011, borough)
)

hs_dir_2011 <- hs_dir_2011[, c(1, 3, 2)]

colnames(hs_dir_2011) <- c("dbn", "address", "year")
# Cleaning School's Addresses for 2016 --------------------------
hs_dir_address_2016 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2016, "pdf_text"], "(?<=Address: ).*?(\\d{5})")

hs_dir_dbn_2016 <-
  str_extract(hs_dir_data[hs_dir_data$year == 2016, "pdf_text"], "(?<=DBN )[0-9]{2}[K|X|M|Q|R]{1}[0-9]{2,3}")

hs_dir_2016 <- as.data.frame(cbind(hs_dir_dbn_2016, hs_dir_address_2016))
hs_dir_2016$year <- rep(2016, nrow(hs_dir_2016))

colnames(hs_dir_2016) <- c("dbn", "address", "year")

# There is one school left with an address at the end of the string rather than after the word "address"
# The address starts after the word 'Counseling.' for school with DBN "23K514"

hs_dir_2016$address <- gsub(".*Counseling.", "", hs_dir_2016$address)


hs_dir_2016[hs_dir_2016$dbn == "23K514", ]


# Combining the Directory Addresses --------------------------
# I will be combining the directories back together using rbind, and if there are duplicate  DBNs,
# I will use the most recent address for the school

hs_dir_data <- rbind(hs_dir_2008, hs_dir_2011, hs_dir_2016)
hs_dir_data <- hs_dir_data[order(hs_dir_data$dbn), ]
hs_dir_data <- hs_dir_data[!is.na(hs_dir_data$address), ]

# remove the rows from 2011 that have addresses that start with NA i.e NA Bronx,NY

hs_dir_data <- hs_dir_data[!grepl("NA", hs_dir_data$address), ]


# group the directory data by dbn and keep only the most recent year's address

hs_dir_data <- hs_dir_data %>%
  group_by(dbn) %>%
  slice(which.max(year))


# remove the rows with no DBN, since "123 Ci ty La ne Bronx, NY 99999" was used as an example
# in the 2016 directory to show how to find info in the directory; it is not a real school

hs_dir_data <- hs_dir_data[which(!is.na(hs_dir_data$dbn)), ]

hs_dir_data$address <- trimws(hs_dir_data$address)


school_dbn <- as.data.frame(school_dbn)
colnames(school_dbn)[1] <- "dbn"
address_info <- merge(school_dbn, hs_dir_data, by = "dbn", all.x = TRUE)



# Finding the Remaining Schools' Addresses with Web Scraping --------------------------

addresses_to_find <- address_info[is.na(address_info$address), ]

# 49 schools without addresses still, I will use web scraping to find them


addresses_to_find <- unique(hs_grad_data[hs_grad_data$dbn %in% addresses_to_find$dbn, c("dbn", "school_name")])
addresses_to_find[] <- lapply(addresses_to_find, as.character)

search_urls <- list()
read_search_urls <- list()
returned_addresses <- list()
addresses_as_character <- list()


for (i in seq_along(addresses_to_find$dbn)) {
  print(paste0("Find the address for DBN:", addresses_to_find$dbn[i]))
  search_urls[[i]] <- URLencode(paste0("https://insideschools.org/school/", addresses_to_find$dbn[i]))
  tryCatch(
    {
      read_search_urls[[i]] <- read_html(search_urls[[i]])
      Sys.sleep(2)
    },
    error = function(e) {
      NULL
    }
  )
}



for (i in seq_along(addresses_to_find$dbn)) {
  if (is.null(read_search_urls[[i]])) {
    returned_addresses[[i]] <- NA
    addresses_as_character[[i]] <- NA
    names(addresses_as_character)[[i]] <- paste0(addresses_to_find$dbn[i])
  }
  else {
    returned_addresses[[i]] <- html_nodes(read_search_urls[[i]], ".location-address") %>% html_text()
    addresses_as_character[[i]] <- as.character(returned_addresses[[i]][[1]])
    addresses_as_character[[i]] <- gsub("\n", " ", addresses_as_character[[i]])
    addresses_as_character[[i]] <- trimws(addresses_as_character[[i]])
    names(addresses_as_character)[[i]] <- paste0(addresses_to_find$dbn[i])
    Sys.sleep(3)
  }
}


new_addresses_found <- as.data.frame(unlist(addresses_as_character))
new_addresses_found$dbn <- rownames(new_addresses_found)
colnames(new_addresses_found)[1] <- "address"



new_addresses_found$address <- gsub("  ", " ", new_addresses_found$address)

original_endings <- c(
  " NY", " Bronx", " Brooklyn", " Manhattan", " Queens",
  " Staten Island", " Springfield Gardens", " New York"
)
new_endings <- c(
  ", NY, ", ", Bronx ", ", Brooklyn ", ", Manhattan ", ", Queens ",
  ", Staten Island ", ", Springfield Gardens ", ", New York "
)

for (i in seq_along(original_endings)) {
  new_addresses_found$address <- gsub(original_endings[[i]], new_endings[[i]], new_addresses_found$address)
}

new_addresses_found$address <- gsub("  ", " ", new_addresses_found$address)


new_addresses_found <- new_addresses_found[, c(2, 1)]


# Manually Inputting 4 Schools' Addresses --------------------------

new_addresses_found[is.na(new_addresses_found$address), ]

# After PDF scraping and web scraping, there are still 4 schools without addresses.
# It appears that these schools were closed, with some being broken down into smaller schools.
# I will input addresses manually based on information from google searches

# retrieving these schools names from our data

addresses_to_find[addresses_to_find$dbn %in% c("03M490", "07X470", "10X410", "15K460"), ]

new_addresses_found[new_addresses_found$dbn == "03M490", "address"] <- "122 Amsterdam Ave, New York, NY 10023"
new_addresses_found[new_addresses_found$dbn == "07X470", "address"] <- "701 Saint Ann's Avenue, Bronx, NY 10455"
new_addresses_found[new_addresses_found$dbn == "10X410", "address"] <- "240 East 172nd Street, Bronx, NY 10457"
new_addresses_found[new_addresses_found$dbn == "15K460", "address"] <- "237 Seventh Avenue, Brooklyn, NY 11215"


# Stacking all Schools' Addresses --------------------------

all_addresses <- merge(new_addresses_found, address_info, by = c("dbn", "address"), all = TRUE)
all_addresses <- all_addresses[!is.na(all_addresses$address), ]
all_addresses <- all_addresses[order(all_addresses$address), ]
all_addresses$address <- tolower(all_addresses$address)

# Address Standardization --------------------------

# using the letter in the DBN to identify each school's borough

all_addresses <- all_addresses %>% mutate(borough = case_when(
  grepl("X", all_addresses$dbn) ~ "Bronx",
  grepl("K", all_addresses$dbn) ~ "Brooklyn",
  grepl("Q", all_addresses$dbn) ~ "Queens",
  grepl("M", all_addresses$dbn) ~ "Manhattan",
  grepl("R", all_addresses$dbn) ~ "Staten Island"
))

all_addresses$zip_code <- str_extract(all_addresses$address, "[0-9]{5}")

# match everything before first comma

all_addresses$address_part_1 <- str_extract(all_addresses$address, "^(.+?),")

all_addresses$address_part_1 <- gsub(",", "", all_addresses$address_part_1)

all_addresses$address_part_1 <- trimws(all_addresses$address_part_1)

# remove remaining borough and neighborhood names from first part of each address
remove_at_string_end_only <- c(
  "bronx", "queens", "manhattan", "brooklyn", "staten island", "long island city",
  "south richmond hill", "richmond hill", "flushing", "bellerose",
  "forest hills", "fresh meadows", "far rockaway", "new york", "elmhurst",
  "south bronx", "cambria heights", "hollis", "springfield gardens", "south ozone park",
  "rockaway park", "ozone park", "oakland gardens", "astoria", "brookl yn",
  "ozone park", "bayside", "ma nhattan", "ridgewood", "saint albans", "n corona",
  "jamaica", "queens village"
)

remove_at_string_end_only <- paste0("\\b", remove_at_string_end_only, "$", collapse = "|")


all_addresses$address_part_1 <- gsub(remove_at_string_end_only, "", all_addresses$address_part_1, perl = TRUE)


all_addresses$address_part_1 <- trimws(all_addresses$address_part_1)



all_addresses$clean_address <- paste0(all_addresses$address_part_1, ", ", all_addresses$borough,
  ", NY", ", ", all_addresses$zip_code,
  sep = " "
)

# manually fixing a few addresses

address_unstandardized <- list(
  "109-89 204 street, Queens, NY, 11412 ",
  "123 west 43th street, Manhattan, NY, 10036 ",
  "240 ea s t 172 street, Bronx, NY, 10457 ",
  "26 broa dway, Manhattan, NY, 10004",
  "26 broa dway, Manhattan, NY, 10004 ",
  "27huntington street, Brooklyn, NY, 11231 ",
  "350 coney is land avenue, Brooklyn, NY, 11218 ",
  "317 east 67 street, Manhattan, NY, 10065 ",
  "333 east 151 street, Bronx, NY, 10451 ",
  "360 east 145 street, Bronx, NY, 10454 ",
  "75 west 205 street, Bronx, NY, 10468 ",
  "8-21 bay 25 street, Queens, NY, 11691 ",
  "89-30 114 street, Queens, NY, 11418 ",
  "1010 rev. j. a. polite avenue, Bronx, NY, 10459 ",
  "1010 rev. polite avenue, Bronx, NY, 10459 ",
  "1180 rev. j.a. polite ave., Bronx, NY, 10459 ",
  "99 terrace view avenue, Bronx, NY, 10463 ",
  "2040 antin pl, Bronx, NY, 10462 ",
  "145 west 84 street, Manhattan, NY, 10024 "
)

address_standardized <- list(
  "109-89 204th street,Queens, NY, 11412",
  "123 west 43rd street, Manhattan, NY, 10036",
  "240 east 172nd street, Bronx, NY, 10457",
  "26 broadway, Manhattan, NY, 10004",
  "26 broadway, Manhattan, NY, 10004",
  "27 huntington street, Brooklyn, NY, 11231",
  "350 coney island avenue, Brooklyn, NY, 11218",
  "317 east 67th street, Manhattan, NY, 10065",
  "333 east 151st street, Bronx, NY, 10451",
  "360 east 145th street, Bronx, NY, 10454",
  "75 west 205th street, Bronx, NY, 10468",
  "8-21 bay 25th street, Queens, NY, 11691",
  "89-30 114th street, Queens, NY, 11418",
  "1010 Reverend James A. Polite avenue, Bronx, NY, 10459",
  "1010 Reverend James A. Polite avenue, Bronx, NY, 10459",
  "1180 Reverend James A. Polite ave., Bronx, NY, 10459",
  "99 Terrace View Avenue, New York, New York, 10463",
  "2040 Mercy College Place, Bronx, NY, 10462",
  "145 west 84th street, Manhattan, NY, 10024"
)

for (i in seq_along(address_unstandardized)) {
  all_addresses[all_addresses$clean_address == address_unstandardized[i], "clean_address"] <- address_standardized[i]
}




all_addresses$clean_address <- trimws(all_addresses$clean_address)
# Geocoding the Addresses --------------------------

# Many schools are located in the same building. To shorten our geocoding request,
# we will only use the unique addresses and later on connect the lat/long to each school.


unique_addresses <- unique(all_addresses$clean_address)

length(unique_addresses)

# 301 addresses to find the lat/long coordinates for

all_lat_lon <- geocode_OSM(unique_addresses, keep.unfound = TRUE)




# Merging the Spatial Coordinates with the Graduation Data --------------------------

# first we need to merge "all_addresses" with "all_lat_lon" to get the lat/long for each DBN

spatial_info <- merge(all_addresses, all_lat_lon, by.x = "clean_address", by.y = "query")
keep_columns <- c("dbn", "clean_address", "borough", "zip_code", "lat", "lon")
spatial_info <- spatial_info[, names(spatial_info) %in% keep_columns]
spatial_info <- spatial_info[, c("dbn", "clean_address", "borough", "zip_code", "lat", "lon")]

# now that the DBN is in the dataset, we can merge it back to the graduation data

hs_data <- merge(hs_grad_data, spatial_info, by = "dbn")
names(hs_data) [names(hs_data) == "clean_address"] <- "address"
hs_data <- hs_data[, c(
  "dbn", "school_name", "address", "borough", "zip_code", "lat", "lon", "cohort_start", "cohort_type",
  "cohort_group", "group_attribute", "value"
)]

convert_to_factor <- c("address", "borough", "zip_code", "group_attribute", "cohort_start")

hs_data[convert_to_factor] <- lapply(hs_data[convert_to_factor], as.factor)

# Assigning each School a "Floor" --------------------------

hs_data$lat_lon <- paste0(hs_data$lat, ",", hs_data$lon)

length(unique(hs_data$lat_lon))

hs_data <- hs_data[order(
  hs_data$lat_lon, hs_data$cohort_start, hs_data$dbn,
  hs_data$cohort_type, hs_data$cohort_group, hs_data$group_attribute
), ]

# The goal of this project is to create a spatial visualization of high school graduation rates,
# There are 553 schools total nested within 293 campuses.

# for each year, assign each campus (unique latitude and longitude) a number
hs_data <-hs_data %>% 
  group_by(cohort_start) %>% 
  mutate(campus_number= match(lat_lon, sort(unique(lat_lon))))

# find the number of schools located in each campus for each year

hs_data <- hs_data %>%
  group_by(lat_lon, cohort_start) %>%
  mutate(total_schools_on_campus = length(unique(dbn)))




# Filtering the Data --------------------------

hs_data <- as.data.frame(hs_data)


hs_data <- hs_data[, c(
  "borough", "lat", "lon", "campus_number",  
  "total_schools_on_campus", "dbn",
  "school_name", "cohort_start",
  "cohort_type", "cohort_group",
  "group_attribute", "value"
)]

# Exporting Data  --------------------------

# I am only interested in the cohort group size and the graduation,
# dropout, and still enrolled totals and rates.

hs_data <-hs_data[hs_data$group_attribute %in% c(
  "group_size", "group_grad_total","group_grad_rate",
  "group_dropout_total", "percent_cohort_dropout",
  "group_still_enrolled_total", "percent_cohort_still_enrolled"
), ]

hs_data <-hs_data %>% pivot_wider(names_from = group_attribute, values_from=value)

hs_data <-hs_data[, c("borough","lat", "lon", "campus_number",
            "total_schools_on_campus", "dbn",
            "school_name", "cohort_start",
            "cohort_type", "cohort_group", "group_size" ,"group_grad_total",
            "group_grad_rate", "group_dropout_total", "percent_cohort_dropout",
            "group_still_enrolled_total", "percent_cohort_still_enrolled")]

hs_data <-hs_data %>% rename(group_dropout_rate=percent_cohort_dropout,
              group_still_enrolled_rate=percent_cohort_still_enrolled)


write.xlsx(hs_data, "data/processed/nyc_hs_grad_data.xlsx")

