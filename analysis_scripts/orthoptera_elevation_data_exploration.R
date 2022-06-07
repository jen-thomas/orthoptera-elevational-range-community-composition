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

#' ## Prepare observation data.

#' Import all observation data.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

observations <- import_all_observations(observations_file, sites_file)

#' ### Subset observations identified to species.
#'
#' Many small nymphs could not be identified to species level. Furthermore, some adults could only be
#' identified to genus or as far as the key would allow to the choice of two taxa. All individuals were
#' identified to the lowest taxonomic level possible.
#'
#' <br>In some parts of the analysis, only those observations identified to species will be used. Subset
#' the observations to get only those that have been identified to species.

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

#' ## Explore observation data.

#' ### Summarise all observations.
#'
#' The following functions calculate and summarise the number of observations and species seen across all
#' surveys.

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

get_species_summary_overview <- function(observations) {
  #' Get a list of the species seen across all the surveys.
  #'
  #' Return a dataframe of the species seen.

  species_summary <- observations %>%
    distinct(species) %>%
    arrange(species)

  return(species_summary)
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

#' <br>The total number of observations was
get_number_observations(observations)

#' <br>The total number of individuals observed for each suborder was
get_number_observations_suborder(observations)

#' <br>The total number of observations identified to species was
get_number_observations(confirmed_observations_species)

#' <br>The total number of species observed was
get_number_species(confirmed_observations_species)

#' <br>The total number of species observed within each suborder was
get_number_species_suborder(confirmed_observations_species)

#' <br>The following species were observed
get_species_summary_overview(confirmed_observations_species)

#' ### Summarise observations by site
#'
#' Three main <em>study areas</em> were visited (TOR, TAV, MOL). Visits to two smaller areas near to
#' TOR/MOL (BOR, BES) were used to cover lower elevations. Within each study area, there were numerous
#' <em>study sites</em>, which were numbered, but can be identified more easily by the elevational band
#' in which they are located. Each site name includes the elevation in m.
#'
#' The following functions calculate observation and species summary across the <em>sites</em> visited.

get_number_observations_site <- function(observations) {
  #' Get the observations dataframe and group it by site and observation.
  #'
  #' Return number of observations at each site.

  observations %>%
    distinct(site_elevation, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(site_elevation) %>%
    summarise("count" = n())
}

get_number_species_site <- function(observations) {
  #' Get the observations data frame and group it by site and species.
  #'
  #' Return the number of species seen at each site.

  observations %>%
    distinct(site_elevation, species) %>%
    group_by(site_elevation) %>%
    summarise("count" = n())
}

get_species_summary_site <- function(observations) {
  #' Get a list of the species seen at each site.
  #'
  #' Return a dataframe of the species seen at each site.

  species_summary <- observations %>%
    distinct(site_elevation, species) %>%
    group_by(site_elevation) %>%
    arrange(site_elevation, species)

  return(species_summary)
}

#' <br>Initially, consider all observations that were made and group them by site to get an overview of
#' the numbers and species.

#' <br>The number of observations at each site was
get_number_observations_site(observations)

#' <br>The number of species seen at each site was (using only those identified to species)
get_number_species_site(confirmed_observations_species)

#' <br>The species seen at each site were
get_species_summary_site(confirmed_observations_species)

#' ### Summarise the observations by survey
#'
#' Each time a study site was visited, <em>surveys</em> were undertaken using a sweep net and also hand
#' collection. At sites at 2000m or above, a sweep net survey was done, followed by a hand collection
#' then another sweep net collection.
#'
#' The following functions summarise observations and species seen during each survey.

get_number_observations_summary <- function(observations) {
  #' Get the observations data frame and group it by survey.
  #'
  #' Return the number of observations found during each survey.

  observations %>%
    distinct(site_elevation, date_cest, method, method_repeat, specimen_label) %>%
    group_by(site_elevation, date_cest, method, method_repeat) %>%
    summarise("count" = n())
}

#' <br>Consider all observations to get an overall count of those collected during each survey.
get_number_observations_summary(observations)