#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

# Set up

install.packages("fossil", quiet = TRUE)
install.packages("stringr", quiet = TRUE)
library(fossil, quietly = TRUE)
library(stringr, quietly = TRUE)

#' ## Change log

# Latest Git commits and updates

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
#' The species-site matrix with abundances will be created using the observation data. 

# Aggregate the observations of each species at each site (ignore dates for now).

# Create vector of 1s to act as presence, join the vector with the observations dataframe, then sum over these to calculate abundance of each species at each site.
presence <- rep(1, nrow(observations))

observations$presence <- presence
site_species_abundance <- setNames(aggregate(observations$presence, list(observations$site_name, observations$species), FUN=sum),
                                   c("site_name", "species", "abundance"))

site_species_abundance_matrix <- t(create.matrix(site_species_abundance, tax.name="species", locality="site_name", abund=TRUE, abund.col="abundance")) # transform it to have species names as columns and sites as rows
site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name="species", locality="site_name", abund=FALSE, abund.col="abundance")) # transform it to have species names as columns and sites as rows