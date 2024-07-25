# Load necessary libraries
require(sf)
require(dplyr)


#
# This function takes a data frame of parks with latitude and longitude coordinates, performs a spatial
# join to identify which polygons from the Greenspaces GeoJSON file each park falls inside, and categorizes
# the parks as either 'large' or 'small' based on the QAP definition. The purpose of this matching is to 
# determine if the parks are considered 'large' or 'small' based on their area in square feet.
#
# Parameters:
# - parks_df: A data frame containing parks information with nested list columns for latitude and longitude.
# - large_or_small: Either 'Large park' or 'Small park' (fed in via the desirable_activity_categories table)
#
# Returns:
# - A modified data frame with the same columns as the input parks_df, excluding longitude, latitude, NAME, and sq_ft.
#   The returned data frame contains only the parks that match the specified size category.
#
# The function performs the following steps:
# 1. Reads the Greenspaces GeoJSON file and validates the geometries.
# 2. Extracts latitude and longitude from the nested list columns in parks_df.
# 3. Converts the parks_df data frame to an sf object for spatial operations.
# 4. Performs a spatial join to find which Greenspaces polygons each park point falls inside.
# 5. Selects the polygon with the highest acreage if a point falls inside multiple polygons.
# 6. Extracts the NAME and ACRES columns from the joined data and calculates the area in square feet (sq_ft).
# 7. Merges the new columns (NAME and sq_ft) back into the original parks_df data frame.
# 8. Filters the parks_df data frame based on the large_or_small parameter:
#    - 'Large park' if the area is 25,000 sq ft or more.
#    - 'Small park' if the area is less than 25,000 sq ft.
# 9. Returns the modified parks_df data frame excluding the longitude, latitude, NAME, and sq_ft columns.


check_park_size <- function(parks_df, large_or_small) {
  
  # Read the GeoJSON file
  geojson_file <- "your_project_directory/data/park_sizes/greenspaces.geojson"
  greenspaces <- st_read(geojson_file)
  
  # Validate and clean geometries
  greenspaces <- st_make_valid(greenspaces)
  
  # Extract latitude and longitude from nested list columns
  parks_df$longitude <- parks_df$location$longitude
  parks_df$latitude <- parks_df$location$latitude
  
  # Drop duplicate rows (same address for multiple park names)
  parks_df <- parks_df %>%
    distinct(latitude, longitude, .keep_all = TRUE)

  # Convert the data frame to an sf object
  parks_sf <- st_as_sf(parks_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Perform a spatial join to find which polygon each point falls inside
  points_with_polygons <- st_join(parks_sf, greenspaces, join = st_intersects)
  
  # Extract latitude and longitude back from the location column
  points_with_polygons$latitude <- points_with_polygons$location$latitude
  points_with_polygons$longitude <- points_with_polygons$location$longitude
  
  # Handle points falling inside multiple polygons by selecting the one with the highest acreage
  points_with_polygons <- points_with_polygons %>%
    group_by(latitude, longitude) %>%
    slice_max(order_by = ACRES, n = 1) %>%
    ungroup()
  
  # Extract the NAME and ACRES columns from the joined data
  points_with_polygons <- st_drop_geometry(points_with_polygons) %>%
    mutate(sq_ft = ACRES * 43560) %>%
    select(NAME, sq_ft, latitude, longitude)
  
  # Merge the new columns back into the original data frame
  parks_df <- parks_df %>%
    left_join(points_with_polygons, by = c("latitude", "longitude"), relationship = "one-to-one")
  
 if(large_or_small == 'Large park') {
   parks_df <- parks_df %>% filter(sq_ft >= 25000)
 } else if (large_or_small == 'Small park') {
   parks_df <- parks_df %>% filter(sq_ft < 25000)
 }
  
  # Return the modified parks data frame
  return(parks_df %>% select(-longitude,-latitude,-NAME,-sq_ft))
}

