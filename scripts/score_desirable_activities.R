# Load necessary libraries
require(dplyr)

# Load required functions
source('your_project_directory/scripts/google_api_functions.R')
source('your_project_directory/scripts/qap_scoring_helper_functions.R')

#  Function: score_desirable_activities

#' Score Desirable Activities Near a Site
#'
#' This function calculates a score for desirable activities near a specified site based on its latitude and longitude.
#' It utilizes the Google Maps API to find points of interest (POI) within a 1.5-mile radius and evaluates their proximity
#' and relevance according to predefined categories. The function also logs detailed information about the closest POIs.
#'
# @param site_latitude Numeric. Latitude of the site to be evaluated.
# @param site_longitude Numeric. Longitude of the site to be evaluated.
# @return A dataframe with the desirable activity categories, the names, addresses, distances, and points assigned to each category.
# @examples
# score_desirable_activities(40.748817, -73.985428)

score_desirable_activities <- function(site_latitude,
                                       site_longitude,
                                       api_key)  {
  # Load required functions
  source('your_project_directory/scripts/google_api_functions.R')
  source('your_project_directory/scripts/qap_scoring_helper_functions.R')
  
  # Load Desirable Activities table
  desirable_activity_categories <- read.csv('your_project_directory/data/qap_scoring_tables/desirable_activity_categories.csv')
  
  # Add columnns for scoring this site
  desirable_activity_categories$name <- NA
  desirable_activity_categories$address <- NA
  desirable_activity_categories$miles_away <- NA
  desirable_activity_categories$points <- 0
  
  # Iterate through desirable activity row
  for (i in 1:nrow(desirable_activity_categories)) {
    
    row <- desirable_activity_categories[i,]
    
    # Run nearbySearch to get list of 3 closest POI within 1.5 mi
    nearby_poi_result <- get_nearby_poi(
      desirable_activity_category = row$qapDesirableActivity,
      poi_type = row$placeType,
      site_latitude = site_latitude,
      site_longitude = site_longitude,
      filter_text = row$filterText,
      filter_in = row$filterIn,
      api_key = api_key)
    
    # Run results through Google Maps Driving Distance Matrix API
    nearby_poi_result <- get_driving_distance(site_latitude = site_latitude,
                                              site_longitude = site_longitude, 
                                              destinations = nearby_poi_result, 
                                              api_key = api_key)
    
    # If a nearby_poi_result is found, log info and award points
    if(!is.null(nearby_poi_result) && nrow(nearby_poi_result)==1) {
      # Assign point value for POI based on the category scoring rubric
      desirable_activity_categories[i,"points"] <- assign_desirable_activity_points(nearby_poi_result$miles_away,
                                                                                    desirable_activity_categories[i,"pointsGroup"])
      
      # If this POI scores points, log name and address for reference                                                                                                                                                            desirable_activity_categories[i,"pointsGroup"])
      if(desirable_activity_categories[i,"points"] > 0) {
        desirable_activity_categories[i,"name"] <- nearby_poi_result$name
        desirable_activity_categories[i,"address"] <- nearby_poi_result$address
        desirable_activity_categories[i,"miles_away"] <- nearby_poi_result$miles_away
        desirable_activity_categories[i,"site_latitude"] <- nearby_poi_result$latitude
        desirable_activity_categories[i,"site_longitude"] <- nearby_poi_result$longitude
      }                                                                     
      
    }
    
    
  }
  
  # Return the scored activity table
  return(desirable_activity_categories)
  
}
