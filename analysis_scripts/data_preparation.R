#' ---
#' title: Data preparation
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import functions from other files.

source("utils.R")

#' ## Change log
#' Display the latest five commits to this file.

# print_latest_git_commits("data_preparation.R")

#' ## Set-up

#' Install packages.

vector_packages <- c("fossil", "stringr")
get_packages(vector_packages)

#' ## Initial data preparation
#' Prepare the data frames with observations and sites, to be used in the rest of the analysis.
#'
#' ### Prepare data frames
#' Join the site data with observations and create new site name which contains the elevation and
#' survey area.

rename_site_with_elevation <- function(observations_df) {
    #' Create a new site name within the dataframe which includes the elevation so that it is more useful
    #' when including it in analyses and figures.
    #' Return dataframe with the additional column.

  observations_df$site_elevation <- paste(observations_df$elevational_band_m,
                                         observations_df$site_name, sep = "_")
  return(observations_df)
}

get_study_area <- function(sites_df) {
 #' Get the site name and study area from the sites data. Create the new site name using elevation.
 #' Make the study area a factor. Replace the long study area names with shortened versions.
 #'
 #' Return dataframe with new site name and study area.

  study_areas <- sites_df[, c("area", "site_name", "elevational_band_m")]

  study_area_details <- rename_site_with_elevation(study_areas)

  return(study_area_details)
}

subset_data_frame <- function(dataframe, vector_columns) {
  #' Get a subset of a dataframe according to a vector of column names.
  #'
  #' Return the subsetted dataframe.

  subsetted_df <- dataframe[, vector_columns]

  return(subsetted_df)
}

join_observation_site <- function(observations_df, sites_df) {
    #' Join the observation, survey (some already with the observations) and site data frames using a left
    #' join, to have the elevation of the sites with the other data.
    #'
    #' Create a new column with a new site name in the format elevation_sitename, which will be more
    #' user-friendly in any outputs.
    #'
    #' Return data frame with the merged data and new site name column.

  observations <- (merge(x = observations_df, y = sites_df, by = "site_name", all.x = TRUE))[,
  c("specimen_label", "site_name", "elevational_band_m", "transect_length_m", "date_cest", "method",
    "method_repeat", "suborder", "family", "subfamily", "genus", "species", "id_confidence", "sex",
    "stage")]

  observations <- rename_site_with_elevation(observations)

  return(observations)
}

#' ### Create summaries of observation data
#' Depending on the analysis, the observations may need to be subsetted to take into account the taxonomic
#' level which they have been identified.

import_all_observations <- function(observations_file, sites_file) {
  #' Import all observations and prepare the data frame with the extra metadata from sites.
  #'
  #' Return a dataframe of all observations with site and survey metadata.

  observations_df <- read_csv_data_file(observations_file)
  sites_df <- read_csv_data_file(sites_file)

  observations_with_sites <- join_observation_site(observations_df, sites_df)

  return(observations_with_sites)
}

get_confirmed_observations_to_species <- function(observations_file, sites_file) {
    #' Observations that have not been identified to species level are removed. Observations that could
    #' not be identified to a specific taxa (that has not already been found in the surveys, i.e
    #' Chorthippus parallelus / montanus) are removed by considering only the confirmed observations.
    #'
    #' Return a dataframe with only confirmed observations to species.

  observations_df <- import_all_observations(observations_file, sites_file)
  observations_species <- observations_df[!(observations_df$species == ""),]
  confirmed_observations_species <- observations_species[(observations_species$id_confidence ==
                                                        "Confirmed"),]

  return(confirmed_observations_species)
}

replace_na_with_zero <- function(dataframe, column) {
  #' Replace all NA values in a column with 0 (to be able to do calculations).
  #'
  #' Return dataframe.

  dataframe[[column]] <- replace(dataframe[[column]], is.na(dataframe[[column]]),0)

  return(dataframe)
}