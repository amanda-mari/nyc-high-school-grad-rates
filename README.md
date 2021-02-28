# New York City High School Graduation Rates
This project uses data from the NYC Department of Education on NYC high school graduation results and combines it with the spatial coordinates of each school.The goal of merging these two pieces of data is to create a spatial map of graduation results overtime in NYC. 

### Data Sources

The original data is from [New York City's Infohub.](https://infohub.nyced.org/reports/academics/graduation-results) It is from the "School" Excel file on that page. (Read the read-me on the first tab of that Excel workbook to learn more about this data.)

Note that the file is large, but it can be found in the [data/raw folder of this repo under the name "raw_graduation_data.xlsx"](https://github.com/amanda-mari/nyc-high-school-grad-rates/tree/main/data/raw)

To obtain the school's addresses for geocoding, I used 3 NYC High School directories from NYC Open Data. 
The PDFs are linked below:

[The 2008 NYC High School Directory](https://data.cityofnewyork.us/Education/2008-2009-NYC-High-School-Director/6wwu-giff)

[The 2011 NYC High School Directory](https://data.cityofnewyork.us/Education/2011-2012-NYC-High-School-directory/rek2-fjft)

[The 2016 NYC High School Directory](https://data.cityofnewyork.us/Education/2015-2016-NYC-High-School-Directory/pzz2-ca2q)


These files can be found in the [data/raw folder of this repository under the names "nyc_hs_directory_20XX.pdf."](https://github.com/amanda-mari/nyc-high-school-grad-rates/tree/main/data/raw) 

For the addresses that I could not locate in the above directories, I used [Inside Schools](https://insideschools.org/) to gather the information.

To geocode all the addresses, I used the [Open Street Map API](https://www.openstreetmap.org/)


### Technologies
* R 


## Project Description

Data Processing

The [cleaning](https://github.com/amanda-mari/nyc-high-school-grad-rates/blob/main/script/cleaning_script.R) and [functions](https://github.com/amanda-mari/nyc-high-school-grad-rates/blob/main/script/functions_script.R) scripts are located in the script folder.

Below are the steps I followed to clean this data and combine the graduation data with the geo-coordinates
1. Load the following packages
  - 'tidyverse' for data cleaning and restructuring
  - 'here' for referencing file paths
  - 'readxl' to read in the Excel sheets
  - 'data.table' to set old column names to new ones
  - 'naniar' to replace 's' values with NA
  - 'pdftools' to scrape the DBNs and addresses from the directory PDFs
  - 'rvest' for web-scraping
  - 'tmaptools' to geocode each school's address
  - 'openxlsx' to write the datasets to new Excel files
2. Source the functions I will be using from the functions script
3. Read in the Excel workbook from NYC DOE Infohub and keep only the sheets I need ("All", "ELL", "SWD", "Ethnicity",
  "Gender", "Poverty", "Ever ELL"")
4. Rename the columns of each sheet, merge all the sheets together, and pivot the data to long format
5. Replace the 's' value (NYCDOE could not reveal this data for privacy reasons) with NA
6. Convert dbn, school name, cohort type, and cohort group to factor variables
7. Read in all 3 PDF directories into a single list using the "pdf_data" function from PDFtools
8. Remove the blank pages from each PDF, add in the directory year to each PDF page, and flatten the list
9. Keep only the "text" and "year" variables from each page
10. Gather the addresses and DBNs of each school by directory year (since each year the book slightly changed, so the code differs), clean the addresses
and DBNs using regular expressions.
11. Merge the list of 553 DBNs from the original graduation data with the DBNs and addresses found from the PDF.
12. Use webscraping to find the 49 remaining addresses not in any of the PDFs. 
13. Manually input the 4 addresses that were not found in steps 10 or 12.
14. Standardized each address by extracting the borough from the DBN and replacing the neighborhood name with the borough. Fixed any lingering mistakes in addresses using regex and a for loop to clean certain addresses.
15. Geocoded the addresses using the "geocode_OSM" function
16. Combined the graduation data and the latitude/longitude coordinates by "DBN"
17. Created a variable "campus_number" to assign each latitude/longitude a unique number within a given year (cohort_start)
18. Created a variable "total_schools_on_campus" to count the number of schools located at the same latitude/longitude within a given year
19. Remove any redundant variables and filter the data to only include the following group attributes: cohort size, graduation total and rate, dropout total and rate, and still enrolled total and rate.
20. Write the data to an Excel file

## About the Final Dataset

The final dataset can be found in the ["data/processed" folder](https://github.com/amanda-mari/nyc-high-school-grad-rates/tree/main/data/processed). It is titled "nyc_hs_grad_data.xlsx" It contains data on the group size and graduation results for several different cohort trajectories and groups of students.

This file contain data on 553 NYC high schools within almost 300 campuses.

Each file contains the following variables:

- *borough* contains the borough of the school, this information was extracted from the DBN (more information can be found [here](https://teachnyc.zendesk.com/hc/en-us/articles/360053601831-What-is-a-DBN-District-Borough-Number-) )
- *lat* contains the latitude of the school
- *lon* contains the longitude of the school
- *campus_number* a number that represents a unique latitude/longitude for a school for the given year in *cohort_start* This number only makes grouping locations easier. This variable is not from the DOE and does not have any significance in that respect.
- *total_schools_on_campus* Total number of schools located at the same latitude/longitude (in the same campus) for the year in *cohort_start* . This number is only accurate for given *cohort_start* , since schools can open and close on a campus in any given year.
- *dbn* DBN (district borough number) of the school
- *school_name* Name of the school
- *cohort_start* This is the year that these students entered the school. It starts in the year 2001 and ends with 2016.
- *cohort_type*  This is the cohort trajectory that these students followed i.e. 4 year June, 5 year August, etc. There are 5 cohort trajectories in total, but each file only has one.
- *cohort_group* There are 19 groupings of students. There is "All Students", as well as 5 ethnicities (Asian, Black, Multi-Racial, Native American, and White),
ELL status (Never ELL, Former ELL, Ever ELL, ELL), Student with Disabilities (Not SWD, SWD), Economic Status (Econ Disadv, Not Econ Disadv), and Sex (Female, Male)
- *group_size* The size of the entering *cohort_group* for the given school, cohort start year, cohort type, and cohort group. This is a raw count.
- *group_grad_total* The total number of graduates for the given school, cohort start year, cohort type, and cohort group . This is a raw count.
- *group_grad_rate* calculated by taking the *group_grad_total*/*group_size*. This is a percentage.
- *group_dropout_total* The total number of students that dropped out for the given school, cohort start year, cohort type, and cohort group. This is a raw count.
- *group_dropout_rate* calculated by taking the *group_dropout_total*/*group_size*. This is a percentage.
- *group_still_enrolled_total* The total number of students still enrolled for the given school, cohort start year, cohort type, and cohort group. This is a raw count.
- *group_still_enrolled_rate* calculated as *group_still_enrolled_total*/*group_size*. This is a percentage.

For the variables *group_grad_total*, *group_grad_rate*, *group_dropout_total*, *group_dropout_rate*, *group_still_enrolled_total*, and *group_still_enrolled_rate* data where students could possibly be identified were removed from the original data (where the raw count variables were < 5), so some graduation outcomes are unavailable.


