#' ---
#' title: Conservative finalised observations
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import packages and functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "dplyr")
get_packages(vector_packages)

#' Import data

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"

sites_df <- read_csv_data_file(sites_file)
surveys_df <- read_csv_data_file(surveys_file)
site_survey_df <- join_site_survey(sites_df, surveys_df)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

#' ## Count the number of specimens that have a finalised observation

get_unique_specimens_finalised <- function(finalised_observations) {
unique_specimens <- finalised_observations %>%
  distinct(specimen_label) %>%
  group_by(specimen_label)

  return(unique_specimens)
}

get_confirmed_obs_site <- function(confirmed_observations, site_name) {
  #' Get a dataframe of all confirmed observations from a particular site, identified by its site_name.

  confirmed_obs_site <- confirmed_observations[confirmed_observations$site_name == site_name, ]

  return(confirmed_obs_site)
}

unique_specimens_finalised <- get_unique_specimens_finalised(finalised_observations)
print(unique_specimens_finalised)

number_finalised_specimens <- n_distinct(finalised_observations$specimen_label)
print(number_finalised_specimens)

#' ## Create new record for each specimen with finalised observation (conservative)
#' Each specimen needs to have one record which can be used in the function to get the number of species
#' at each site. Therefore, the finalised identifications, and those that are already identified as
#' confirmed observations for each site, will be manually checked and a decision made about how the
#' specimen should be recorded so that it adds (or not) to the species richness for a particular site, as
#' required. See below for the logic.

BES0120210914H1C007 <- finalised_observations[finalised_observations$specimen_label == "BES01 20210914 H1 C007", ]
BES0120210914H1C007_new <- BES0120210914H1C007[1, ]
BES0120210914H1C007_new["genus"] <- "Gomphocerus / Gomphoceridius"

finalised_identifications_conservative <- BES0120210914H1C007_new

BES0220210720H1C002 <- finalised_observations[finalised_observations$specimen_label == "BES02 20210720 H1 C002", ]
confirmed_obs_bes02 <- get_confirmed_obs_site(confirmed_observations, "BES02")
BES0220210720H1C002_new <- BES0220210720H1C002[1, ]
BES0220210720H1C002_new["genus"] <- "Miramella / Podisma"

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, BES0220210720H1C002_new)

MOL0820210915N1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL08 20210915 N1 C002", ]
confirmed_obs_mol08 <- get_confirmed_obs_site(confirmed_observations, "MOL08")
MOL0820210915N1C002_new <- MOL0820210915N1C002[1, ]
MOL0820210915N1C002_new["species"] <- ""

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0820210915N1C002_new)

MOL0820210915N1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL08 20210915 N1 C002", ]
MOL0820210915N1C002_new <- MOL0820210915N1C002[1, ]
MOL0820210915N1C002_new["species"] <- ""

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0820210915N1C002_new)

#' ## Checks on finalised observation data
#' ### Check that all the specimens have a row in the new dataframe


#' ### Count that there are the same number of specimens as there were to begin with and none are repeated


#' Calculate the species richness (conservative) including the finalised observations
