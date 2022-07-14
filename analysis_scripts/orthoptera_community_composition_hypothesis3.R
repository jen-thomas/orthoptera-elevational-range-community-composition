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
source("prepare_vegetation_data.R")

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

#' ## Environmental variables
#'
#' Environmental variables will be checked for collinearity, then the quantitative environmental variables
#' (average vegetation height, maximum vegetation height and vegetation density) will be normalised to
#' have a mean of 0 and standard deviation of 1. Site aspect will be converted to eight compass points and
#' other environmental variables transformed as needed.
#'
#' ### Prepare data

vegetation_averaged_df <- prepare_veg_data(sites_file, vegetation_file)

#' ### Check for collinearity between environmental variables
#'
#' ## Detrended canonical correspondance analysis
#'
#' It might be expected that this matrix will contain many zeros where species have not been observed at
#' sites along the elevational gradient, which in some ordination methods would lead to problems of
#' closely associating sites when they lack species (double-zero problem)
#' [@legendreEcologicallyMeaningfulTransformations2001]. To determine if the response of the species data
#' is linear or unimodal, a detrended canonical analysis will be used. If the results do not meet the
#' criteria for a linear ordination method, then one option would be to apply a Hellinger transformation,
#' which has been shown to be suitable for presence-absence data [@legendreNumericalEcology2012], to be
#' able to then use a principle components analysis (PCA)
#' [@legendreEcologicallyMeaningfulTransformations2001]; a second option would be to use a distance method
#' of unconstrained ordination which does not rely on linear ordination, to identify any natural clusters
#' that might occur in the data due to the environmental variables.
#'
#' ## Constrained canonical analysis
#'
#' Given there are likely lots of zeros in the dataset because it is possible that species occur
#' non-uniformly across the sites, an asymmetric, constrained canonical analysis (CCA) could be used
#' [@legendreNumericalEcology2012] to understand the relationship between Orthoptera community
#' composition, and altitude and the environment in which they are found. Site aspect, altitude,
#' vegetation structure and ground cover will be used to constrain the ordination. If this is significant,
#' forward selection of the environmental variables will be done to identify which influence the community
#' composition of Orthoptera in the Pyrenees. The variables will be ordered according to the variation
#' they explain, then a Monte Carlo permutation test will be used to test the significance of the
#' variation explained by the highest-ranking variable. If the permutation test is significant, then the
#' variable will be selected (and used in the next step as a covariate). PERMANOVA will be used to test
#' for variation between the groups. Beforehand, the dispersion within groups will be tested to ensure
#' that a false difference in means is not found [@wartonDistancebasedMultivariateAnalyses2012].