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

#' The following function is used to prepare the data and create the species-site matrix.

create_presence_absence_site_species_matrix <- function(observations_df) {
        #' Convert the abundance matrix into a presence-absence site-species matrix. Each value is either
        #' 0 (species not observed at a site) or 1 (species observed at a site).

  #site_species_abundance <- create_site_species_abundance_df(observations_df)
  site_species_presenceabsence_matrix <- t(create.matrix(observations_df, tax.name = "taxa",
                                                         locality = "site_elevation", abund = FALSE
    #,
  #                                                       abund.col = "abundance"
  ))

  return(site_species_presenceabsence_matrix)
}

#' Prepare the data for the matrix.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

sites_df <- read_csv_data_file(sites_file)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

all_observations_conservative <- get_conservative_observations(confirmed_observations,
                                                               finalised_observations)

unique_taxa_sites <- get_unique_taxa_site(all_observations_conservative)
unique_taxa_sites

#' Create and preview the presence-absence site-species matrix. Site name is in the format
#' altitude(m)_site where the name is an abbreviation of the study area.
#'
#' This matrix includes all unique taxa from each site (as was used for the species richness analysis).
#' This means though, that a higher taxonomic level, such as genus, may be represented elsewhere in the
#' matrix by a different taxa. The genus though in this example, would be a unique taxa from the
#' particular sites where it was observed. **TODO**: consider if these should be removed from the analysis
#' if they are not a unique taxa for the whole project, rather than just for a site.

site_species_matrix <- create_presence_absence_site_species_matrix(unique_taxa_sites)
site_species_matrix
