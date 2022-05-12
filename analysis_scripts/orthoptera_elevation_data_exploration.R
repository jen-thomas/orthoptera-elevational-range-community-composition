#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

# Set up

vector_packages <- c("fossil", "stringr")

install_packages <- function(vector_packages) {
  for (required_package in vector_packages) {
    if(!require(required_package)) install.packages(required_package, quiet=TRUE)}
}

#' ## Change log

# Get the latest Git commits to this file

this_file_latest_commits <- system("git log HEAD --pretty='tformat: %ci commit %h: %s' orthoptera_elevation_data_exploration.R", intern=TRUE)
this_file_latest_commits_formatted <- paste(this_file_latest_commits, "<br>")

#' ### Latest commits to this file:
{{ str_replace(this_file_latest_commits_formatted[1:5], ", ", "") }}

# Read in data files

observations <- read.csv("../data/observations.csv", header = TRUE, stringsAsFactors = TRUE)
sites <- read.csv("../data/sites.csv", header = TRUE, stringsAsFactors = TRUE)
surveys <- read.csv("../data/surveys.csv", header = TRUE, stringsAsFactors = TRUE)
vegetation_plots <- read.csv("../data/vegetation_plots.csv", header = TRUE, stringsAsFactors = TRUE)

#' ## Create species-site matrix

# Add presence to the observation data frame.

presence <- rep(1, nrow(observations)) # create vector of 1s to act as presence
observations$presence <- presence # join the vector with the observations data frame

# Combine site_name and altitude columns to get site names that are more useful when looking at the data.

observations_presence <- (merge(x = observations, y = sites, by = "site_name", all.x = TRUE))[, c('site_name', 'altitude_band_m', 'suborder', 'family', 'subfamily', 'genus', 'species', 'presence')]
observations_presence$site_altitude <- paste(observations_presence$site_name, observations_presence$altitude_band_m, sep="_")

# Aggregate the observations of each species at each site (ignore dates for now). Then create the species-site abundance and presence-absence matrices. Transform the matrices to have species names as column names and sites as row names.

site_species_abundance <- setNames(aggregate(observations_presence$presence, list(observations_presence$site_altitude, observations_presence$species), FUN=sum),
                                   c("site_altitude", "species", "abundance")) # sum over these to calculate abundance of each species at each site

site_species_abundance_matrix <- t(create.matrix(site_species_abundance, tax.name="species", locality="site_altitude", abund=TRUE, abund.col="abundance")) # transform it to have species names as columns and sites as rows
site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name="species", locality="site_altitude", abund=FALSE, abund.col="abundance")) # transform it to have species names as columns and sites as rows

