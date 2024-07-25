# These functions determine the local jurisdiction of a site and score
# previous projects based on a site's location and the application year. 
# The functions interact with each other to assess the site's jurisdiction and 
# calculate points for previous projects. 
# 
# Functions included:
#   
# find_local_jurisdiction: Determines the local jurisdiction for a given site 
#     based on its latitude and longitude.
# calculate_previous_project_b_points: Calculates points for previous projects 
#     based on their proximity to the site and the presence of a nearby transit hub.
# score_previous_projects: Uses the previous two functions to calculate the total 
#     points for previous projects based on the site's jurisdiction and proximity 
#     to past projects.
# 
# Function Relationships:
# find_local_jurisdiction is used within score_previous_projects to determine the 
# jurisdiction of the site.
# 
# calculate_previous_project_b_points is used within score_previous_projects to 
# calculate points based on the proximity to previous projects.


# Function: find_local_jurisdiction

# This function determines the local jurisdiction for a given site based on its latitude and longitude.
# It reads a GeoJSON file containing jurisdiction boundaries and checks for an 
# intersection with the site's coordinates.

find_local_jurisdiction <- function(site_latitude, site_longitude, application_year) {
  
  jurisdiction_awards <- st_read(paste0("your_project_directory/data/lihtc_location_data/clean/awards_in_local_jurisdiction_", 
                                        application_year, "_lookback.geojson"))
  
  # Set coordinate system
  jurisdiction_awards <- st_transform(jurisdiction_awards, 4326)
  
  # Create a point geometry from the latitude and longitude
  site_point <- st_sfc(st_point(c(site_longitude, site_latitude)), crs = 4326)
  
  # Check for intersection between site and jurisdictions
  site_jurisdiction <- st_filter(jurisdiction_awards, site_point, .predicate = st_intersects)
  
  # Return matching jurisdiction
  return(st_drop_geometry(site_jurisdiction))
  
}

# Function: calculate_previous_project_b_points

# This function completes the distance-based search from a site to previous 
# LIHTC projects and (if the site is eligible) scores the site according to 
# the date/proximity of any nearby project.
# Parameters:
# @previous_projects_1mi_buffer: a spatial dataframe containg 1 mile buffer
#     circles around existing LIHTC developments (prepared by DCA)
# @site_latitude
# @site_longitude
# @transit_hub_1m_flag: This flag is set during the Community Transportation
#     section of the scoring process. At that stage, the flag should be set to 
#     TRUE if the site is <1 mile from a transit hub (according to the definition
#     laid out in that section)

# Function to calculate points
calculate_previous_project_b_points <- function(previous_projects_1mi_buffer, 
                                                site_latitude, 
                                                site_longitude, 
                                                transit_hub_1mi_flag, 
                                                application_year) {
  
  # Create a point geometry from the latitude and longitude
  site_point <- st_sfc(st_point(c(site_longitude, site_latitude)), crs = 4326)
  
  # Filter projects based on lookback period
  projects_6_years <-  st_filter(previous_projects_1mi_buffer %>% filter(ProjYear >= (application_year - 6)), 
                                 site_point, .predicate = st_intersects)
  projects_4_years <-  st_filter(previous_projects_1mi_buffer %>% filter(ProjYear >= (application_year - 4)), 
                                 site_point, .predicate = st_intersects)
  projects_3_years <-  st_filter(previous_projects_1mi_buffer %>% filter(ProjYear >= (application_year - 3)), 
                                 site_point, .predicate = st_intersects)
  projects_2_years <-  st_filter(previous_projects_1mi_buffer %>% filter(ProjYear >= (application_year - 2)), 
                                 site_point, .predicate = st_intersects)
  
  # Initialize variable to keep track of how many years back any nearby awards were given
  b_no_awards_1mi_since <- 0
  
  # Calculate points based on the number of projects within each lookback period
  if (nrow(projects_6_years) == 0) {
    points <- 3
    b_no_awards_1mi_since <- 6
  } else if (nrow(projects_4_years) == 0) {
    points <- 2
    b_no_awards_1mi_since <- 4
  } else {
    points <- 0
  }
  
  if (transit_hub_1mi_flag) {
    if (nrow(projects_3_years) == 0) {
      points <- max(points, 3)
      b_no_awards_1mi_since <- max(b_no_awards_1mi_since, 3)
    } else if (nrow(projects_2_years) == 0) {
      points <- max(points, 2)
      b_no_awards_1mi_since <- max(b_no_awards_1mi_since, 2)
    }
  }
  
  return(data.frame(points,b_no_awards_1mi_since))
}


# This function calculates the points for previous projects based on a site's location and the application year.
# It checks if the site is within a local jurisdiction and calculates points based on previous projects within that jurisdiction.
# Parameters:
# @site_latitude
# @site_longitude
# @application_year: The year of the application (used to establish the 
#     appropriate lookback period)


score_previous_projects <- function(site_latitude,
                                    site_longitude,
                                    application_year,
                                    scored_community_transportation)
{
  # Create point totals for this category
  previous_project_a_points <- 0
  previous_project_b_points <- 0
  previous_project_total_points <- 0
  jurisdiction_name <- NA
  most_recent_year <- NA
  jurisdiction_awards_in_last_15_years <- NA
  
  
  # Find the jurisdiction this site sits in
  site_jurisdiction <- find_local_jurisdiction(site_latitude, 
                                               site_longitude, 
                                               application_year) 
  
  # Calculate points for (A)
  
  # Ensure that site only falls inside one local government boundary
  if(nrow(site_jurisdiction) > 1) {
    # Handle case for 1+ local government boundary match 
    stop('Error: this site falls inside of two local government boundaries')
  } else if (nrow(site_jurisdiction) == 1) {
    # Handle case for 1 local government boundary match 
    previous_project_a_points <- site_jurisdiction[1,"local_jurisdiction_points"]
    jurisdiction_name <- site_jurisdiction[1,"jurisdiction_name"]
    most_recent_year <- site_jurisdiction[1,"most_recent_year"]
    jurisdiction_awards_in_last_15_years <- site_jurisdiction[1,"project_count"]
  } else {
    # Handle case for site in unincorporated territory (no match)
    local_government_name <- "No local government"
  } 
  
  # Calculate points for (B)
  
  # Load 'Previous Projects 1 mile buffer' file
  previous_projects_1mi_buffer <- st_read('your_project_directory/data/lihtc_location_data/clean/previous_projects_1mi_buffer.geojson')
  
  # Calculate the points
  previous_project_b_points <- calculate_previous_project_b_points(previous_projects_1mi_buffer, 
                                                                   site_latitude, 
                                                                   site_longitude, 
                                                                   scored_community_transportation$transit_hub_1mi_flag,
                                                                   application_year)
  
  # Pick higher point total: Category (A) or (B)
  previous_project_total_points <- max(previous_project_a_points,
                                       previous_project_b_points$points)
  
  previous_project_summary <- data.frame(previous_project_a_points,
                                         previous_project_b_points = previous_project_b_points$points,
                                         b_no_awards_1mi_since = previous_project_b_points$b_no_awards_1mi_since,
                                         previous_project_total_points,
                                         jurisdiction_name,
                                         most_recent_year,
                                         jurisdiction_awards_in_last_15_years)
  
  return(previous_project_summary)
  
  
}



