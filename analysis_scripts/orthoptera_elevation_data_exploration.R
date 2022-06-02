#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

install_packages <- function(vector_packages) {
  #' Install and load packages from a vector of packages that are needed for the code.

  for (required_package in vector_packages) {
    if (!require(required_package, character.only=TRUE, quietly=TRUE)) install.packages(required_package)
      library(required_package, character.only=TRUE)
}
}

read_csv_data_file <- function(file_path) {
  #' Read in a CSV data file using the first line as the header. Strings in different columns can be set as factors in
  #' the resulting dataframe.
  #' Return a dataframe of the input data.

    read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
}

rename_site_with_altitude <- function(observations_df) {
  #' Create a new site name within the dataframe which includes the altitude so that it is more useful when including
  #' it in analyses and figures.
  #' Return dataframe with the additional column.

  observations_df$site_altitude <- paste(observations_df$altitude_band_m, observations_df$site_name, sep="_")
  return(observations_df)
}

# Set up

vector_packages <- c("fossil", "stringr")
install_packages(vector_packages)

observations <- read_csv_data_file("../data/observations.csv")
sites <- read_csv_data_file("../data/sites.csv")
surveys <- read_csv_data_file("../data/surveys.csv")
vegetation_plots <- read_csv_data_file("../data/vegetation_plots.csv")

#' ## Change log

print_latest_git_commits <- function(file_path) {
  #' Get and print the latest five commits for a file.
  command <- paste("git log HEAD --pretty='tformat: %ci commit %h: %s", file_path)
  # print(command)
  this_file_latest_commits <- system(command, intern=TRUE)
  this_file_latest_commits_formatted <- paste(this_file_latest_commits, "<br>")
  # print(str_replace(this_file_latest_commits_formatted[1:5], ", ", ""))
  print(this_file_latest_commits[1:5])
}

print_latest_git_commits("orthoptera_elevation_data_exploration.R")

get_confirmed_observations_to_species <- function(observations_df) {
  #' Observations that have not been identified to species level are removed. Observations that could not be
  #' identified to a specific taxa (that has not already been found in the surveys, i.e
  #' Chorthippus parallelus / montanus) are removed by considering only the confirmed observations.
  #' Return a dataframe with only confirmed observations to species.

  observations_species <- observations_df[!(observations_df$species==""), ]
  confirmed_observations_species <- observations_species[(observations_species$id_confidence=="Confirmed"), ]
  return(confirmed_observations_species)
}

create_presence_absence_site_species_matrix <- function(observations_df) {
  #' Create a site-species matrix from a set of observations at different sites. Each observation is of one individual
  #' at a particular site.
  #' Return presence-absence site-species matrix.

  # Add presence to the observation data frame.
  presence <- rep(1, nrow(observations_df)) # create vector of 1s to act as presence
  observations_df$presence <- presence # join the vector with the observations data frame
  observations_presence <- (merge(x = observations_df, y = sites, by = "site_name", all.x = TRUE))[,
    c("site_name", "altitude_band_m", "suborder", "family", "subfamily", "genus", "species", "presence")]

  # create another site name column with the altitude
  observations_presence <- rename_site_with_altitude(observations_presence)

  # Aggregate the observations of each species at each site (ignore dates for now).
  site_species_abundance <- setNames(aggregate(observations_presence$presence,
                                             list(observations_presence$site_altitude, observations_presence$species),
                                             FUN=sum), # sum over these to calculate abundance of each species at each
                                   # site
                                   c("site_altitude", "species", "abundance"))

  site_species_abundance_matrix <- t(create.matrix(site_species_abundance, tax.name="species",
                                                   locality="site_altitude", abund=TRUE, abund.col="abundance"))
  site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name="species",
                                                         locality="site_altitude", abund=FALSE, abund.col="abundance"))

  return(site_species_presenceabsence_matrix)
}

#' ## Create species-site matrix

confirmed_observations_species <- get_confirmed_observations_to_species(observations)
site_species_matrix <- create_presence_absence_site_species_matrix(confirmed_observations_species)
head(site_species_matrix)
