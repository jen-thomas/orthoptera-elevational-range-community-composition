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

print_latest_git_commits("data_preparation.R")

#' ## Set-up

#' Install packages and read in data files.

get_packages <- function(vector_packages) {
    #' Install and load packages from a vector of packages that are needed for the code.

  for (required_package in vector_packages) {
    if (!require(required_package, character.only = TRUE, quietly = TRUE)) install.packages(required_package)
    library(required_package, character.only = TRUE)
  }
}

read_csv_data_file <- function(file_path) {
    #' Read in a CSV data file using the first line as the header. Strings in different columns can be set as factors in
    #' the resulting dataframe.
    #' Return a dataframe of the input data.

  read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
}


vector_packages <- c("fossil", "stringr", "dplyr")
get_packages(vector_packages)

#' ## Initial data preparation.
#' Prepare the data frames with observations and sites, to be used in the rest of the analysis.
#'
#' ### Prepare data frames.
#' Firstly, join the site data with observations and create new site name which contains the elevation and survey area.

rename_site_with_altitude <- function(observations_df) {
    #' Create a new site name within the dataframe which includes the altitude so that it is more useful when including
    #' it in analyses and figures.
    #' Return dataframe with the additional column.

  observations_df$site_altitude <- paste(observations_df$altitude_band_m, observations_df$site_name, sep = "_")
  return(observations_df)
}

join_observation_site <- function(observations_df, sites_df) {
    #' Join the observation and site data frames using a left join, to have the altitude of the sites with the other
    #' data.
    #' Create a new column with a new site name in the format altitude_sitename, which will be more user-friendly
    #' in any outputs.
    #' Return data frame with the merged data and new site name column.

  observations <- (merge(x = observations_df, y = sites_df, by = "site_name", all.x = TRUE))[,
    c("specimen_label", "site_name", "altitude_band_m", "suborder", "family", "subfamily", "genus", "species",
      "id_confidence", "sex", "stage")]

  observations <- rename_site_with_altitude(observations)

  return(observations)
}

#' ### Create summaries of observation data.
#' Depending on the analysis, the observations may need to be subsetted to take into account the taxonomic level to
#' which they have been identified.

import_all_observations <- function(observations_file, sites_file) {
  #' Import all observations and prepare the data frame with the extra metadata from sites.
  #'
  #' Return a dataframe of all observations and site metadata.

  observations_df <- read_csv_data_file(observations_file)
  sites_df <- read_csv_data_file(sites_file)

  observations_with_sites <- join_observation_site(observations_df, sites_df)

  return(observations_with_sites)
}

get_confirmed_observations_to_species <- function(observations_df) {
    #' Observations that have not been identified to species level are removed. Observations that could not be
    #' identified to a specific taxa (that has not already been found in the surveys, i.e
    #' Chorthippus parallelus / montanus) are removed by considering only the confirmed observations.
    #'
    #' Return a dataframe with only confirmed observations to species.

  observations_species <- observations_df[!(observations_df$species == ""),]
  confirmed_observations_species <- observations_species[(observations_species$id_confidence == "Confirmed"),]

  return(confirmed_observations_species)
}