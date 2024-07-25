# Install and load required packages
require(sf)
require(tigris)
require(dplyr)
require(stringr)

# Preprocessing Step 1: Pull Previous Project geography data from DCA website
# Save to project directory
# Available at: https://data-georgia-dca.opendata.arcgis.com/maps/Georgia-DCA::multifamily-affordable-housing-properties-9-and-4-point-data/about

# process_project_locations()

#' This function processes project location data for two types of files/projects (9% and 4%) 
#' by loading the data, filtering and standardizing the columns, combining the data, 
#' adding a project year column, and saving the combined data to a specified output file.
# Parameters:
#
# @param file_9_percent Character. The file path to the 9% project locations file.
# @param file_4_percent Character. The file path to the 4% project locations file.
# @param output_file Character. The file path where the combined output file will be saved.
#'

process_project_locations <- function(file_9_percent, file_4_percent, output_file) {
  if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  # Load project locations
  data_9_percent <- st_read(file_9_percent)
  data_4_percent <- st_read(file_4_percent)
  
  # Filter columns and standardize 9% file to 4% file format
  data_9_percent <- data_9_percent %>% 
    select(id = ProjID, name = PropName, address = Address, type = F9per)
  data_4_percent <- data_4_percent %>% 
    select(id = ProjecID, name = PropName, address = Address, type = F4per)
  
  # Combine 9% and 4% shapefiles into one
  combined_data <- rbind(data_9_percent, data_4_percent)
  
  # Add ProjYear column
  combined_data <- combined_data %>% mutate(ProjYear = substr(id, 1, 4))
  
  # Save combined shapefile
  st_write(combined_data, output_file, delete_dsn = TRUE)
}

# Call the function for the "point-level" files
process_project_locations(
  file_9_percent = 'your_project_directory/data/lihtc_location_data/raw/9%_Point_Data.json',
  file_4_percent = 'your_project_directory/data/lihtc_location_data/raw/4%_Point_Data.json',
  output_file = 'your_project_directory/data/lihtc_location_data/clean/previous_projects.geojson'
)

# Call the function for the "1 mile buffer" files
process_project_locations(
  file_9_percent = 'your_project_directory/data/lihtc_location_data/raw/9%_1mi_buffer.json',
  file_4_percent = 'your_project_directory/data/lihtc_location_data/raw/4%_1mi_buffer.json',
  output_file = 'your_project_directory/data/lihtc_location_data/clean/previous_projects_1mi_buffer.geojson'
)


# Preprocessing Step 2: Pull Georgia municipality boundary shapefile
# Save to project directory

# Set your Census API key
require(tidycensus)
census_api_key("423cc7f78f81bb1ebfdfb028e354da46bd0ce84f", install = TRUE)

# Load all incorporated municipalities in Georgia
georgia_municipalities <- places(state = "GA", cb = TRUE)

# Ensure the NAMELSAD column is character
georgia_municipalities <- georgia_municipalities %>%
  mutate(NAMELSAD = as.character(NAMELSAD))

# Filter out CDPs (Census Designated Places)
georgia_incorporated <- georgia_municipalities %>%
  filter(!str_detect(NAMELSAD, 'CDP'))

# Save the filtered shapefile for future use
st_write(georgia_incorporated, 
         "your_project_directory/data/lihtc_location_data/clean/georgia_incorporated_municipalities.geojson", 
         delete_dsn = TRUE)


# Preprocessing Step 3: Join local government boundaries to project file
# Save to project directory

# Function: create_local_jurisdiction_award_file
# Purpose:
# This function creates a local jurisdiction award file based on the specified application year.
# It processes previous project data and municipal boundary data to calculate the number of projects
# and the most recent project year for each municipality. Based on these calculations, it assigns
# points to each municipality and saves the results to a GeoJSON file.
#
# Parameters:
# - application_year: The year of the application for which the award file is being created. This
#                     determines the look-back period for previous projects (15 years prior).
#
# Steps:
# 1. Loads previous projects and Georgia incorporated municipalities spatial data.
# 2. Transforms the coordinate reference systems of both datasets to EPSG:4326 (WGS84).
# 3. Filters previous projects to those that occurred within 15 years prior to the application year.
# 4. Conducts a spatial join to associate projects with municipalities.
# 5. Summarizes the joined data to count the number of projects and determine the year of the most recent project for each municipality.
# 6. Adds a points_awarded column based on specific criteria:
#    - If the count of previous projects is 0, the municipality receives 5 points.
#    - If the count of previous projects is 1 and the year of the most recent project is >= 2018, the municipality receives 4 points.
#    - Otherwise, the municipality receives 0 points.
# 7. Saves the resulting summarized data to a new GeoJSON file, with the application year included in the filename.

create_local_jurisdiction_award_file <- function(application_year) {

  # Step 3a: load previous projects
  previous_projects <- st_read("your_project_directory/data/lihtc_location_data/clean/previous_projects.geojson")
  georgia_incorporated <- st_read("your_project_directory/data/lihtc_location_data/clean/georgia_incorporated_municipalities.geojson")
  
  # Change both files to same coordinate system
  previous_projects <- st_transform(previous_projects, 4326)
  georgia_incorporated <- st_transform(georgia_incorporated, 4326)
  
  # Step 3b: Filter previous_projects to >= filter_year (15 years prior to application, per p. 113)
  previous_projects <- previous_projects %>% filter(ProjYear >= application_year - 15)
  
  # Step 3c: Spatial join: georgia_municipalities to previous_projects
  projects_jurisdictions_joined <- st_join(georgia_incorporated, previous_projects, join = st_intersects)
  
  # Step 3d: Summarize the join
  #          - Count # of projects
  #          - Year of most recent project
  jurisdiction_project_summary <- projects_jurisdictions_joined %>%
    group_by(AFFGEOID) %>%
    summarize(
      project_count = sum(!is.na(id)),
      most_recent_year = max(ProjYear, na.rm = TRUE),
      jurisdiction_name = max(NAME)
    )
  
  # Step 3e: Add points_awarded column based on QAP logic
  jurisdiction_project_summary <- jurisdiction_project_summary %>%
    mutate(
      local_jurisdiction_points = case_when(
        project_count == 0 ~ 5,
        project_count == 1 & most_recent_year < 2018 ~ 4,
        TRUE ~ 0  # Default case if other conditions are not met
      )
    )

  # Saving the result to a new shapefile with the year in the filename
  output_file <- paste0("your_project_directory/data/lihtc_location_data/clean/awards_in_local_jurisdiction_", application_year, "_lookback.geojson")
  st_write(jurisdiction_project_summary, output_file)
}

# Run function for 2024 and 2025 application years
create_local_jurisdiction_award_file(2024)
create_local_jurisdiction_award_file(2025)

































