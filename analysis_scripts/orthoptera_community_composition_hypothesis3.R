#' ---
#' title: Hypothesis 3
#' subtitle: Orthoptera community composition is influenced by elevation and environmental variables.
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>**Project <a href="https://falciot.net/orthoptera-94940">homepage</a>**
#'
#' <br>Import functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "ggplot2", "dplyr")
get_packages(vector_packages)

#' ## Create site-species matrix.

#' The following functions are used to prepare the data and create the species-site matrix.

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
        #' Convert the abundance matrix into a presence-absence site-species matrix. Each value is either 0
        #' (species not observed at a site) or 1 (species observed at a site).

  site_species_abundance <- create_site_species_abundance_df(observations_df)
  site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name = "species",
                                                         locality = "site_elevation", abund = FALSE,
                                                         abund.col = "abundance"))

  return(site_species_presenceabsence_matrix)
}

#' #'Create and preview the presence-absence site-species matrix. Site name is in the format altitude(m)_site
#' where the name is an abbreviation of the study area.

site_species_matrix <- create_presence_absence_site_species_matrix(observations_to_use)
site_species_matrix