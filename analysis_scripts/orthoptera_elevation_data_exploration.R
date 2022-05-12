#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

# Latest Git commits and updates

latest_commit <- system("git show -s --pretty='%h on %ci' HEAD", intern=TRUE)

#' The latest change to this project was **commit **
{{ latest_commit }}

this_file_latest_commits <- system("git log --pretty='tformat:- %h %s on %ci' orthoptera_elevation_data_exploration.R", intern=TRUE)
this_file_latest_commits <- paste("<ul>", this_file_latest_commits, "</ul>")

#' ## Change log

{{ this_file_latest_commits }}

#' ## Set up

install.packages("fossil")
library(fossil)

# Read in data files

observations <- read.csv("../data/observations.csv", header = TRUE, stringsAsFactors = TRUE)
sites <- read.csv("../data/sites.csv", header = TRUE, stringsAsFactors = TRUE)
surveys <- read.csv("../data/surveys.csv", header = TRUE, stringsAsFactors = TRUE)
vegetation_plots <- read.csv("../data/vegetation_plots.csv", header = TRUE, stringsAsFactors = TRUE)

#' ## Create species-site matrix
#' The species-site matrix with abundances will be created using the observation data. 

# Aggregate the observations of each species at each site (ignore dates for now).

# Create vector of 1s to act as presence, join the vector with the observations dataframe, then sum over these to calculate abundance.
presence <- rep(1, nrow(observations))

observations$presence <- presence
site_species_abundance <- setNames(aggregate(observations$presence, list(observations$site_name, observations$species), FUN=sum),
                                   c("site_name", "species", "abundance"))

create.matrix(site_species_abundance, tax.name="species", locality="site_name", abund=TRUE, abund.col="abundance")




