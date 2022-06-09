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

source("utils.R")

#' ## Create site-species matrix.

#' Create a dataframe of species abundance at each site (note that this is not really relevant in this study because
#' multiple capture techniques were used). Convert this into a presence-absence site-species matrix.

create_site_species_abundance_df <- function(observations_df) {
    #' Create a data frame of the species abundance at each site from a set of observations at different sites. Each
    #' input observation is of one individual at a particular site.
    #' Return species abundance at each site.

  # Add presence to the observation data frame.
  presence <- rep(1, nrow(observations_df)) # create vector of 1s to act as presence
  observations_df$presence <- presence # join the vector with the observations data frame

  # Aggregate the observations of each species at each site (ignore dates for now).
  site_species_abundance <- setNames(aggregate(observations_df$presence,
                                               list(observations_df$site_altitude, observations_df$species),
                                               FUN = sum), # sum over these to calculate abundance of each species at
                                                          # each site
                                               c("site_altitude", "species", "abundance"))

  return(site_species_abundance)
}

create_presence_absence_site_species_matrix <- function(observations_df) {
    #' Create a presence-absence site-species matrix. Each value is either 0 (species not observed at a site) or 1
    #' (species observed at a site).

  site_species_abundance <- create_site_species_abundance_df(observations_df)
  site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name = "species",
                                                         locality = "site_altitude", abund = FALSE,
                                                         abund.col = "abundance"))

  return(site_species_presenceabsence_matrix)
}

#' Use only observations that have been identified to species, for now. Create and preview the presence-absence
#' site-species matrix. Site name is in the format altitude(m)_site where the name is an
#' abbreviation of the study area.

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

site_species_matrix <- create_presence_absence_site_species_matrix(confirmed_observations_species)
head(site_species_matrix)