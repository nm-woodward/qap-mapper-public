# Load necessary libraries
require(dplyr)

# Load required functions
source('your_project_directory/scripts/google_api_functions.R')
source('your_project_directory/scripts/qap_scoring_helper_functions.R')

# Scoring functions by category
source('your_project_directory/scripts/score_desirable_activities.R')
source('your_project_directory/scripts/score_community_transportation.R')
source('your_project_directory/scripts/score_previous_projects.R')

# Establish site address
my_site_latitude <- 37.7589525
my_site_longitude <- -84.386695
my_application_year <- 2024

# Establish my API key
my_api_key <- "your_google_api_key"

# SECTION 1: SCORE DESIRABLE ACTIVITIES

# Run function to create scored table for all desirable activity categories
scored_activity_table <- score_desirable_activities(my_site_latitude,
                                                    my_site_longitude)

# Add up all activity type points to get a total for this category
desirable_activity_total_points <- sum(scored_activity_table$points)

# SECTION 2: SCORE COMMUNITY TRANSPORTATION OPTIONS

# Run function to score the community transportation categories
# and return a dataframe including the points scored in each option,
# along with the highest (chosen) category and total value
scored_community_transportation <- score_community_transportation(my_site_latitude,
                                                                  my_site_longitude,
                                                                  my_api_key)

# SECTION 3: SCORE PREVIOUS PROJECTS
scored_previous_projects <- score_previous_projects(my_site_latitude,
                                                    my_site_longitude,
                                                    my_application_year,
                                                    scored_community_transportation[[1]])

# CONCLUSION: TOTAL POINTS
total_points <- sum(desirable_activity_total_points, 
                    scored_community_transportation[[1]]$community_transportation_total_points,
                    scored_previous_projects$previous_project_total_points)


