#' ---
#' title: Hypothesis 2
#' subtitle: Elevational range extent increases with elevation (Rapoport's elevational rule).
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "ggplot2", "dplyr")
get_packages(vector_packages)

#' ## Create site-species matrix.
#+ message=FALSE, warning=FALSE

#' Create a dataframe of species abundance at each site (note that abundance is not really relevant in
#' this study because multiple capture techniques were used). Convert this into a presence-absence
#' site-species matrix.
#'
#' The following functions are used to create the species-site matrix.

create_site_species_abundance_df <- function(observations_df) {
    #' Create a data frame of the species abundance at each site from a set of observations at different
    #' sites. Each input observation is of one individual at a particular site.
    #'
    #' Return species abundance at each site.

  # Add presence to the observation data frame.
  presence <- rep(1, nrow(observations_df)) # create vector of 1s to act as presence
  observations_df$presence <- presence # join the vector with the observations data frame

  # Aggregate the observations of each species at each site (ignore dates for now).
  site_species_abundance <- setNames(aggregate(observations_df$presence,
                                               list(observations_df$site_elevation,
                                                    observations_df$species),
                                               FUN = sum), # sum over these to calculate abundance of
                                     # each species at each site
                                               c("site_elevation", "species", "abundance"))

  return(site_species_abundance)
}

create_presence_absence_site_species_matrix <- function(observations_df) {
    #' Create a presence-absence site-species matrix. Each value is either 0 (species not observed at a
    #' site) or 1 (species observed at a site).

  site_species_abundance <- create_site_species_abundance_df(observations_df)
  site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name = "species",
                                                         locality = "site_elevation", abund = FALSE,
                                                         abund.col = "abundance"))

  return(site_species_presenceabsence_matrix)
}

#'Create and preview the presence-absence site-species matrix. Site name is in the format altitude(m)_site
#' where the name is an abbreviation of the study area.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"

sites_df <- read_csv_data_file(sites_file)

#' See exploratory data analysis and hypothesis 1 for an explanation of confirmed and finalised
#' observations.
#'
#' Get all observations.
observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)

finalised_identifications <- create_finalised_observations(finalised_observations)

finalised_identifications_conservative <- finalised_identifications[[1]]

all_observations_conservative <- join_observations(confirmed_observations, finalised_identifications_conservative)

#' Get all observations to species only. **TODO**: consider if this should also be done to genus, or
#' include those that add another taxa (if there are enough observations).

observations_species <- get_observations_to_species(all_observations_conservative)

#' Count the number of observations of each species to see if they should be included in the analysis.

observations_count_species <- count_observations_of_species(observations_species)
observations_count_species

#' Count the number of sites at which each species has been observed.

observations_species_sites <- count_sites_where_species_observed(observations_species)
observations_species_sites

#' Subset the species considered, by selecting those where they were observed at three or more, and five
#' or more sites.
#'
#' Three sites:
species_three_or_more_sites <- observations_species_sites[(observations_species_sites$number_sites >= 3), ]
species_three_or_more_sites

#' Five sites:
species_five_or_more_sites <- observations_species_sites[(observations_species_sites$number_sites >= 5), ]
species_five_or_more_sites

species_for_analysis <- unique(species_three_or_more_sites["species"])
observations_to_use <- get_observations_of_particular_species(observations_species, species_for_analysis)

#' Create the site-species matrix (presence-absence).

site_species_matrix <- create_presence_absence_site_species_matrix(observations_to_use)
site_species_matrix