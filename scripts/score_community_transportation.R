# Load required functions
source('your_project_directory/scripts/google_api_functions.R')

# This file contains functions designed to evaluate and score various aspects of 
# community transportation accessibility for a given site based on its 
# geographic coordinates. The functions interact with each other to assess the 
# proximity and relevance of transportation options, assigning scores according 
# to predefined criteria. 
# 
# Functions included:
#   
#   assign_community_transportation_points: Calculates points based on the distance 
#   to a transportation point and its category.
# score_community_transportation: Evaluates and scores the accessibility of 
#   community transportation options near a specified site, returning a comprehensive score.
# 
# Function Relationships:
#   
# assign_community_transportation_points is used within score_community_transportation 
# to determine the points for specific transportation categories based on their proximity to the site.
# score_community_transportation utilizes the assign_community_transportation_points 
# function as part of its process to evaluate and compare different transportation options.


# Function: assign_community_transportation_points

# Description:
#   The community_transportation_points function evaluates the proximity of 
#   a community transportation category and assigns a score based on its distance to a site.
#   Scoring is assigned according to tables on p.100-101 of the 2024-25 QAP.
# 
# Parameters:
#   mi_away (numeric): The distance, in miles, from the site to the nearest 
#                      community transportation point.
#   community_transportation_category (character): A string representing 
#                                     the category of the transportation point. 
#   Valid categories are:
#     Community Transportation Hub: 'subway_station,bus_depot'
#     Community Transportation Stop: 'bus_stop,transit_station'
# Returns:
#   A numeric score based on the distance to the transportation point and 
#   its category. The scoring system is as follows:
#   For subway_station,bus_depot:
#       Distance ≤ 0.25 miles: 5 points
#       Distance ≤ 5 miles: 4.5 points
#       Distance ≤ 1 mile: 4 points
# For bus_stop,transit_station:
#       Distance ≤ 0.25 miles: 3 points
#       Distance ≤ 5 miles: 2 points
#       Distance ≤ 1 mile: 1 point

assign_community_transportation_points <- function(mi_away, community_transportation_category) {
  print(community_transportation_category)
  # Category (A): Transit Hub
  if (community_transportation_category == 'subway_station,transit_depot') {
    if (mi_away <= 0.25) {
      return(5)
    } else if (mi_away <= 5) {
      return(4.5)
    } else if (mi_away <= 1) {
      return(4)
    } else {
      return(0)
    }    
  } else if (community_transportation_category == 'bus_stop,transit_station') {
    if (mi_away <= 0.25) {
      return(3)
    } else if (mi_away <= 5) {
      return(2)
    } else if (mi_away <= 1) {
      return(1)
    } else {
      return(0)
    }    
  } else {
    stop("Community Transportation scoring failed - poi_types don't
         match the categories in the community_transportation_points() function")
  }
}

#  Function: score_community_transportation

#' Score Community Transportation Accessibility
#'
#' This function evaluates and scores the accessibility of community transportation options
#' near a specified site. It assesses two main categories: Transit Oriented Development and
#' Access to Public Transportation, and returns a score based on the closest and most relevant
#' public transportation options available. The function also flags sites that are within 1 mile
#' of a major transit hub.
# # Parameters:
# @site_latitude: A site's latitude (numeric)
# @site_latitude: A site's latitude (numeric)
# 
# @return A dataframe with the scores for community transportation categories, total points, 
#'         the category with the highest score, and a flag indicating proximity to a transit hub.

score_community_transportation <- function(site_latitude,
                                           site_longitude,
                                           api_key) {
  
  # Create point variables for this category 
  community_transportation_a_points <- 0
  community_transportation_b_points <- 0
  community_transportation_total_points <- 0
  
  # For use in Section XVIII: Previous Projects
  transit_hub_1mi_flag <- FALSE
  
  # A. Transit Oriented Development
  nearby_transit_hub <- get_nearby_poi(
    desirable_activity_category = 'Transit Oriented Development',
    poi_type = 'subway_station,transit_depot',
    site_latitude = site_latitude,
    site_longitude = site_longitude,
    api_key = api_key)
  
  nearby_transit_hub <- get_driving_distance(site_latitude = site_latitude,
                                             site_longitude = site_longitude, 
                                             destinations = nearby_transit_hub, 
                                             api_key = api_key,
                                             travelMode = "WALKING")
  
  # Complete scoring for (A)
  if(!is.null(nearby_transit_hub)) {
    community_transportation_a_points <- assign_community_transportation_points(
      nearby_transit_hub[1,"miles_away"],
      nearby_transit_hub[1,"poi_type"])
    # Section XVIII: Previous Projects includes a "<1 mile from a transit hub" filter
    # We'll capture that filter here to avoid a second API call
    if (nearby_transit_hub[1,"miles_away"] <= 1) {
      transit_hub_1mi_flag <- TRUE
    }
  }
  
  # B. Access to Public Transportation
  nearby_transit_stop <- get_nearby_poi(
    desirable_activity_category = 'Access to Public Transportation',
    poi_type = 'bus_stop,transit_station',
    site_latitude = site_latitude,
    site_longitude = site_longitude,
    api_key = api_key)
  
  nearby_transit_stop <- get_driving_distance(site_latitude = site_latitude,
                                              site_longitude = site_longitude, 
                                              destinations = nearby_transit_stop, 
                                              api_key = api_key,
                                              travelMode = "WALKING")
  
  # Complete scoring for (B)
  if(!is.null(nearby_transit_stop)) {
    community_transportation_b_points <- assign_community_transportation_points(
      nearby_transit_stop[1,"miles_away"],
      nearby_transit_stop[1,"poi_type"])
  }
  
  # Compare (A) to (B) and choose highest score
  
  if (community_transportation_a_points > community_transportation_b_points) {
    # Case: (A) is higher than (B)
    community_transportation_total_points <- community_transportation_a_points
    community_transportation_total_points_category <- '(A) Transit-Oriented Development'
  } else if (community_transportation_b_points > community_transportation_a_points) {
    # Case: (B) is higher than (A)
    community_transportation_total_points <- community_transportation_b_points
    community_transportation_total_points_category <- '(B) Access to Public Transportation'
  } else {
    community_transportation_total_points_category <- 'N/A'
    # (A) and (B) should never be tied (per the rubric) - this check ensures
    # that the only possible tie is the case in which both are 0.
    if (community_transportation_a_points != 0 && community_transportation_b_points != 0) {
      stop("Problem with Community Transportation scoring logic")
    }
  }
  
  # Create dataframe with all variables of interest to return 
  community_transportation_scores <- data.frame(community_transportation_a_points,
                                                community_transportation_b_points,
                                                community_transportation_total_points,
                                                community_transportation_total_points_category,
                                                transit_hub_1mi_flag)
  # Return the scored dataframe
  return(list(community_transportation_scores, 
              nearby_transit_hub,
              nearby_transit_stop))
  
}
