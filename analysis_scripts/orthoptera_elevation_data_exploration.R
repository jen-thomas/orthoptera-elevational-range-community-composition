#' ---
#' title: Initial data exploration
#' output: 
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import packages functions from other files.

source("data_preparation.R")
source("utils.R")

vector_packages <- c("fossil", "stringr", "dplyr")
get_packages(vector_packages)

#' ## Change log
#' Display the latest five commits to this file.

print_latest_git_commits("orthoptera_elevation_data_exploration.R")

#' ## Summarise species composition.
#' The following functions calculate and summarise the number of observations and species.

get_number_observations <- function(observations) {
    #' Get the total number of unique observations.

  unique_observations <- unique(observations[c("specimen_label")])
  number_observations <- nrow(unique_observations)
  print(number_observations)
}

get_number_species <- function(observations) {
    #' Get the total number of unique species observed. Use observations identified to species.

  unique_species <- unique(observations[c("species")])
  number_species <- nrow(unique_species)
  print(number_species)
}

get_number_species_suborder <- function(observations) {
    #' Get the number of species observed within each suborder. Use observations identified to species.

  observations %>%
    distinct(suborder, species) %>%
    group_by(suborder) %>%
    summarise("count" = n())
}

get_number_observations_suborder <- function(observations) {
    #' Get the number of observations for each suborder. Use confirmed and finalised identifications.
    #' They do not have to be to species level.

  observations %>%
    distinct(suborder, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(suborder) %>%
    summarise("count" = n())
}

#' ### Summarise all observations.
#'
#' Import all observation data.
observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

observations <- import_all_observations(observations_file, sites_file)

#' <br>The total number of observations was
get_number_observations(observations)

#' <br>The total number of individuals observed for each suborder was
get_number_observations_suborder(observations)

#' ### Use only observations identified to species.
#' Subset the observations to get only those that have been identified to species.
confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

#' <br>The total number of observations to species was
get_number_observations(confirmed_observations_species)

#' <br>The total number of species observed was
get_number_species(confirmed_observations_species)

#' <br>The total number of species observed within each suborder was
get_number_species_suborder(confirmed_observations_species)