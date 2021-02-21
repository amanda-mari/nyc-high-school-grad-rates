# New York City High School Graduation Rates
This project uses data from the NYC Department of Education on NYC high school graduation results and combines it with the spatial coordinates of each school.The goal of merging these two pieces of data is to create a 3D visualization of graduation results in NYC. 

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
15. Geocoded the 294 addresses using the "geocode_OSM" function
16. Combined the graduation data and the latitude/longitude coordinates by "DBN"
17. Created a variable "total_schools_on_campus" to count the number of schools located at the same latitude/longitude within a given year (cohort_start)
18. Created a variable "school_floor" to assign a "floor" (number) to each school within a campus on a given year
19. Remove any redundant variables and filter the data to only include the following group attributes: cohort size, graduation total, dropout total, and still enrolled total.
20. Remove any NAs in the "value" column
21. Split the dataset by cohort_type (graduation trajectory) and exported 5 excel files, one for each cohort type.

## About the Final Datasets

The final datasets can be found in the ["data/processed" folder](https://github.com/amanda-mari/nyc-high-school-grad-rates/tree/main/data/processed). There are 5 Excel files for graduation outcomes. Each file is for a cohort trajectory type i.e. 4 year June, 5 year August

These files contain data on 553 NYC high schools within 294 campuses. Each row is a count of a group attribute for the given school, student start year, student trajectory, and student group.

Each file contains the following variables:

- *lat_lon* contains the latitude and longitude of the school separated by a comma
- *borough* contains the borough of the school, this information was extracted from the DBN (more information can be found [here](https://teachnyc.zendesk.com/hc/en-us/articles/360053601831-What-is-a-DBN-District-Borough-Number-) )
- *zip_code* contains the zip code of the school
- *total_schools_on_campus* Total number of schools located at the same latitude/longitude (in the same campus) for the year in *cohort_start* . This number is only accurate for given *cohort_start* , since schools can open and close on a campus in any given year.
- *dbn* DBN (district borough number) of the school
- *school_name* Name of the school
- *school_floor* This is a sequenced variable to count the number of schools on a campus for the year in *cohort_start* i.e. if a campus has 3 schools in it in the *cohort_start* 2006, the first school will have *school_floor*=1, the second will have *school_floor*=2, and the third will have *school_floor*=3. **This variable is not an accurate representation of where the school is located on campus, it serves as the third dimension for mapping purposes only**
- *cohort_start* This is the year that these students entered the school. It starts in the year 2001 and ends with 2016.
- *cohort_type*  This is the cohort trajectory that these students followed i.e. 4 year June, 5 year August, etc. There are 5 cohort trajectories in total, but each file only has one.
- *cohort_group* There are 19 groupings of students. There is "All Students", as well as 5 ethnicities (Asian, Black, Multi-Racial, Native American, and White),
ELL status (Never ELL, Former ELL, Ever ELL, ELL), Student with Disabilities (Not SWD, SWD), Economic Status (Econ Disadv, Not Econ Disadv), and Sex (Female, Male)
- *group_attribute* This variable is a factor that has 4 levels. "group_size" is the size of the cohort group, "group_grad_total" is the total number of graduates from that cohort group, "group_dropout_total" is the total number of dropouts from the cohort group, and "group_still_enrolled_total" is the total number of students still enrolled from the cohort group. 
- *value* These are the raw counts (not percentages) for the given school, cohort start, cohort type, cohort group, and attribute. Since data where students could possibly be identified were removed from the original data, some graduation outcomes are unavailable.


