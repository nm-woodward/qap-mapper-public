# QAP Scoring Helper Functions

# Functions included:
#   assign_desirable_activity_points()
#   assign_points_desirable_activity_group1()
#   assign_points_desirable_activity_group2()
#   assign_community_transportation_points()

# Function assign_points_desirable_activity_group1

# Function to assign points based on distance for Group 1 (Metro Pool)
#
# Parameters:
# - mi_away: The distance in miles from the location.
#
# Returns:
# - Points based on the distance thresholds for Group 1:
#   - 2.5 points if the distance is 0.5 miles or less.
#   - 2 points if the distance is greater than 0.5 miles but 1 mile or less.
#   - 1.5 points if the distance is greater than 1 mile but 1.5 miles or less.
#   - 0 points if the distance is greater than 1.5 miles.
assign_points_desirable_activity_group1 <- function(mi_away) {
  if (mi_away <= 0.5) {
    return(2.5)
  } else if (mi_away <= 1) {
    return(2)
  } else if (mi_away <= 1.5) {
    return(1.5)
  } else {
    return(0)
  }
}

# Function assign_points_desirable_activity_group2

# Function to assign points based on distance for Group 2 (Metro Pool)
#
# Parameters:
# - mi_away: The distance in miles from the location.
#
# Returns:
# - Points based on the distance thresholds for Group 2:
#   - 2 points if the distance is 0.5 miles or less.
#   - 1.5 points if the distance is greater than 0.5 miles but 1 mile or less.
#   - 1 point if the distance is greater than 1 mile but 1.5 miles or less.
#   - 0 points if the distance is greater than 1.5 miles.
assign_points_desirable_activity_group2 <- function(mi_away) {
  if (mi_away <= 0.5) {
    return(2)
  } else if (mi_away <= 1) {
    return(1.5)
  } else if (mi_away <= 1.5) {
    return(1)
  } else {
    return(0)
  }
}

# Function assign_desirable_activity_points

# Description: assign points based on pointsCategory
#
# Parameters:
# - mi_away: The distance in miles from the location.
# - points_group: The group number (1 or 2) indicating which point assignment logic to use.
#
# Returns:
# - Points based on the distance and points group:
#   - Uses `assign_points_desirable_activity_group1` for Group 1.
#   - Uses `assign_points_desirable_activity_group2` for Group 2.
#   - Returns NA for unexpected points group values.
assign_desirable_activity_points <- function(mi_away, points_group) {
  if (points_group == 1) {
    return(assign_points_desirable_activity_group1(mi_away))
  } else if (points_group == 2) {
    return(assign_points_desirable_activity_group2(mi_away))
  } else {
    return(NA)  # Handle unexpected pointsCategory values
  }
}

