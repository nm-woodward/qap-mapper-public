library(plumber)
library(dplyr)

# Load required functions
source('your_project_directory/scripts/google_api_functions.R')
source('your_project_directory/scripts/qap_scoring_helper_functions.R')
source('your_project_directory/scripts/score_desirable_activities.R')
source('your_project_directory/scripts/score_community_transportation.R')
source('your_project_directory/scripts/score_previous_projects.R')

#* @param site_latitude numeric The latitude of the site
#* @param site_longitude numeric The longitude of the site
#* @param application_year numeric The year of the application
#* @param api_key character The API key for Google services
#* @get /score
function(site_latitude, site_longitude, application_year, api_key) {
  # Establish the site latitude, longitude, application year, and API key
  my_site_latitude <- as.numeric(site_latitude)
  my_site_longitude <- as.numeric(site_longitude)
  my_application_year <- as.numeric(application_year)
  my_api_key <- 'your_google_api_key_here'
  
  # SECTION 1: SCORE DESIRABLE ACTIVITIES
  scored_activity_table <- score_desirable_activities(my_site_latitude, 
                                                      my_site_longitude,
                                                      my_api_key)
  desirable_activity_total_points <- sum(scored_activity_table$points)
  
  # SECTION 2: SCORE COMMUNITY TRANSPORTATION OPTIONS
  scored_community_transportation <- score_community_transportation(my_site_latitude, 
                                                                    my_site_longitude,
                                                                    my_api_key)
  
  # SECTION 3: SCORE PREVIOUS PROJECTS
  scored_previous_projects <- score_previous_projects(my_site_latitude, 
                                                      my_site_longitude, 
                                                      my_application_year,
                                                      as.data.frame((scored_community_transportation[[1]]))) 
  
  # CONCLUSION: TOTAL POINTS
  total_points <- sum(min(desirable_activity_total_points,20), 
                      scored_community_transportation[[1]]$community_transportation_total_points,
                      scored_previous_projects$previous_project_total_points)
  
  list(
    scored_activity_table = scored_activity_table,
    desirable_activity_total_points = desirable_activity_total_points,
    scored_community_transportation = scored_community_transportation,
    scored_previous_projects = scored_previous_projects,
    total_points = total_points
  )
}


