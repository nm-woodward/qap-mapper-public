# Google API functions

# Functions included:
#   get_nearby_poi()
  # Dependent function: check_park_size()
   source('your_project_directory/scripts/park_size_calc_function.R')
#   get_driving_distance()

require(httr)
require(jsonlite)
require(dplyr)
require(stringr)

# Function to get nearby points of interest (POI) using the Places API
# 
# Parameters:
# - poi_type: The type of point of interest (e.g., 'hospital', 'supermarket').
# - site_latitude: The latitude of the location to search around.
# - site_longitude: The longitude of the location to search around.
# - filter_text: Text used to filter the names of the places (either to keep or remove based on filter_in).
# - filter_in: Filter mode, either 'keep' or 'remove', to determine if the filter_text should be used to include or exclude places.
# - api_key: The API key for accessing the Places API.
#
# Returns:
# - A data frame containing up to 3 of the closest places that match the criteria, 
#   with columns for POI type, name, address, latitude, and longitude.
#
# The function performs the following steps:
# 1. Defines the request parameters for the Places API call, including the types of places to include, 
#    the location to search around, and the maximum search radius (1.5 miles).
# 2. Converts the request parameters to a JSON format.
# 3. Makes a POST request to the Places API with the specified headers and request body.
# 4. Parses the JSON response from the API.
# 5. If places are found, reformats the data into a data frame and applies any specified filters 
#    ('keep' or 'remove') based on the filter_text.
# 6. Subsets the results to the top 3 closest places, if any.
# 7. Returns the resulting data frame or an empty data frame if no places are found.

get_nearby_poi <- function(desirable_activity_category, 
                           poi_type, site_latitude, 
                           site_longitude, 
                           filter_text="", 
                           filter_in="", 
                           api_key) {
  
  # Define the request parameters
  request <- list(
    includedTypes = str_split(poi_type, ",\\s*") ,
    locationRestriction = list(
      circle = list(
        center = list(
          latitude = site_latitude,
          longitude = site_longitude
        ),
        radius = 2414.14 #1.5 miles (the furthest distance a POI can be to score points)
      )
    ),
    rankPreference = 'DISTANCE',
    maxResultCount=20
  )
  
  # Convert the request body to JSON
  json_request <- toJSON(request, auto_unbox = TRUE)
  
  field_mask <- 'places.displayName,places.location,places.shortFormattedAddress'
  
  # Make the POST request to the Places API
  response <- POST(
    url = 'https://places.googleapis.com/v1/places:searchNearby',
    add_headers(
      'Content-Type' = 'application/json',
      'X-Goog-Api-Key' = api_key,
      'X-Goog-FieldMask' = field_mask
    ),
    body = json_request
  )
  
  # Parse the response
  data <- fromJSON(content(response, as = "text"))
  
  # If running the 'park' category, check for park size and subset
  if (poi_type == 'park' && nrow(data$places) > 0) {
    data$places <- check_park_size(data$places, desirable_activity_category)
  }
  
  # Reformat as data frame
  if(!is.null(data$places) && nrow(data$places) > 0)  {
    nearby_places <- data.frame(poi_type = poi_type,
                                name = data$places$displayName$text,
                                address = data$places$shortFormattedAddress,
                                latitude = data$places$location$latitude,
                                longitude = data$places$location$longitude)
    
    # If using a 'keep' filter (e.g. 'hospital'), narrow in on matching records
    if (filter_in == 'keep' && filter_text != "") {
      nearby_places <- nearby_places %>% 
        filter(grepl(filter_text, name, ignore.case = TRUE))
    }
    
    # If using a 'remove' filter (e.g. 'Little free library'), narrow in on matching records
    if (filter_in == 'remove' && filter_text != "") {
      nearby_places <- nearby_places %>% 
        filter(!grepl(filter_text, name, ignore.case = TRUE))
    }
    
    # Filter down to 3 closest POI
    if(nrow(nearby_places) > 3){
      nearby_places <- nearby_places[1:3,]
    } 
    
  } else {
    # If no results within distance filter, return an empty df
    nearby_places <- data.frame()
  }
  
  return(nearby_places)
}


# Function to get driving distances

# Parameters:
# - site_latitude: The latitude of the origin location.
# - site_longitude: The longitude of the origin location.
# - destinations: A data frame containing the destination locations with columns for latitude and longitude.
# - api_key: The API key for accessing the Google Maps Distance Matrix API.
#
# Returns:
# - A data frame containing the closest destination with the calculated driving distance in miles.
#   If no destinations are listed, the function does not make an API call and returns nothing.
#
# The function performs the following steps:
# 1. Checks if there is at least one destination listed. If not, the function returns without making an API call.
# 2. Creates a latitude/longitude string for the origin location.
# 3. Creates a string of latitude/longitude pairs for the destination locations.
# 4. Constructs the URL for the Google Maps Distance Matrix API call.
# 5. Makes a GET request to the API using the constructed URL.
# 6. Parses the JSON response from the API.
# 7. Converts the distance from meters to miles and rounds to the nearest mile.
# 8. Creates a data frame containing the destinations and the calculated driving distances.
# 9. Filters the results to keep only the closest point of interest (POI).
# 10. Returns the resulting data frame.


# Function to get driving distances
get_driving_distance <- function(site_latitude, 
                         site_longitude, 
                         destinations, 
                         api_key, 
                         travelMode = "DRIVING") {
  
  # Only call API if >=1 destination is listed
  if (nrow(destinations) > 0) {
    
    # Create lat/long string for API
    site_lat_long <- paste0(site_latitude, ",", site_longitude)
    
    destination_str <- paste(apply(destinations, 1, 
                                   function(row) paste0(row["latitude"], ",", row["longitude"])), collapse = "|")
    url <- paste0(
      "https://maps.googleapis.com/maps/api/distancematrix/json?origins=",
      site_lat_long, "&destinations=", destination_str, 
      "&travelMode=", travelMode, "&key=", api_key
    )
    
    response <- GET(url)
    content <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    results <- data.frame(destinations,
                          miles_away = round(content$rows$elements[[1]]$distance$value/1609, 2))
    
    # Filter distance calculations to closest POI (remove others)
    results <- results %>% arrange(miles_away)
    results <- results[1,]
    
    return(results)
  } 
  
}
