#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

#' ## Change log
#' Display the latest five commits to this file.

print_latest_git_commits <- function(file_path) {
    #' Get and print the latest five commits for a file.
  command <- paste("git log HEAD --pretty='tformat: %ci commit %h: %s'", file_path)
  # print(command)
  this_file_latest_commits <- system(command, intern = TRUE)
  this_file_latest_commits_formatted <- paste(this_file_latest_commits, "<br>")
  # print(str_replace(this_file_latest_commits_formatted[1:5], ", ", ""))
  print(this_file_latest_commits[1:5])
}

print_latest_git_commits("orthoptera_elevation_data_exploration.R")

#' ## Set-up
#' Install packages and read in data files.

install_packages <- function(vector_packages) {
    #' Install and load packages from a vector of packages that are needed for the code.

  for (required_package in vector_packages) {
    if (!require(required_package, character.only = TRUE, quietly = TRUE)) install.packages(required_package)
    library(required_package, character.only = TRUE)
  }
}

read_csv_data_file <- function(file_path) {
    #' Read in a CSV data file using the first line as the header. Strings in different columns can be set as factors in
    #' the resulting dataframe.
    #' Return a dataframe of the input data.

  read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
}

vector_packages <- c("fossil", "stringr", "dplyr")
install_packages(vector_packages)

observations <- read_csv_data_file("../data/observations.csv")
sites <- read_csv_data_file("../data/sites.csv")
surveys <- read_csv_data_file("../data/surveys.csv")
vegetation_plots <- read_csv_data_file("../data/vegetation_plots.csv")

#' ## Initial data preparation.
#' Join site data with observations and create new, user-friendly site name. Create a summary of the observations that
#' have been identified to species.

rename_site_with_altitude <- function(observations_df) {
    #' Create a new site name within the dataframe which includes the altitude so that it is more useful when including
    #' it in analyses and figures.
    #' Return dataframe with the additional column.

  observations_df$site_altitude <- paste(observations_df$altitude_band_m, observations_df$site_name, sep = "_")
  return(observations_df)
}

join_observation_site <- function(observations_df) {
    #' Join the observation and site data frames using a left join, to have the altitude of the sites with the other
    #' data.
    #' Create a new column with a new site name in the format altitude_sitename, which will be more user-friendly
    #' in any outputs.
    #' Return data frame with the merged data and new site name column.

  observations <- (merge(x = observations_df, y = sites, by = "site_name", all.x = TRUE))[,
    c("specimen_label", "site_name", "altitude_band_m", "suborder", "family", "subfamily", "genus", "species", "id_confidence", "sex", "stage")]

  observations <- rename_site_with_altitude(observations)

  return(observations)
}

get_site_information <- function(observations_df) {
    #' Join the altitude of sites with the observations and output a data frame with a user-friendly site name.

  observations_site <- join_observation_site(observations_df)
  observations_site <- rename_site_with_altitude(observations_site)

  return(observations_site)
}

get_confirmed_observations_to_species <- function(observations_df) {
    #' Observations that have not been identified to species level are removed. Observations that could not be
    #' identified to a specific taxa (that has not already been found in the surveys, i.e
    #' Chorthippus parallelus / montanus) are removed by considering only the confirmed observations.
    #' Return a dataframe with only confirmed observations to species.

  observations_species <- observations_df[!(observations_df$species == ""),]
  confirmed_observations_species <- observations_species[(observations_species$id_confidence == "Confirmed"),]

  return(confirmed_observations_species)
}

observations <- get_site_information(observations)
confirmed_observations_species <- get_confirmed_observations_to_species(observations)

#' ## Initial data exploration and summary results.
#' ### Explore the species observed

get_number_observations <- function(observations) {
    #' Get the total number of unique observations.

  unique_observations <- unique(observations[c("specimen_label")])
  number_observations <- nrow(unique_observations)
  print(number_observations)
}

get_number_species <- function(observations) {
    #' Get the total number of unique species observed. Use observations identified to species.

  unique_species <- unique(observations[c("species")])
  number_species <- nrow(unique_species)
  print(number_species)
}

get_number_species_suborder <- function(observations) {
    #' Get the number of species observed within each suborder. Use observations identified to species.

  observations %>%
    distinct(suborder, species) %>%
    group_by(suborder) %>%
    summarise("count" = n())
}

get_number_observations_suborder <- function(observations) {
    #' Get the number of observations for each suborder. Use confirmed and finalised identifications. They do not have
    #' to be to species level.

  observations %>%
    distinct(suborder, specimen_label) %>% # account for multiple identifications for a finalised observation
    group_by(suborder) %>%
    summarise("count" = n())
}

#' <br>The total number of observations was
get_number_observations(observations)

#' <br>The total number of observations to species was
get_number_observations(confirmed_observations_species)

#' <br>The total number of species observed was
get_number_species(confirmed_observations_species)

#' <br>The total number of species observed within each suborder was
get_number_species_suborder(confirmed_observations_species)

#' <br>The total number of individuals observed for each suborder was
get_number_observations_suborder(observations)


#' ## Hypothesis 1
#' ### Species richness decreases with elevation.

calculate_species_richness_alt_bands <- function(observations) {
  #' Aggregate over the species observed within each altitude band and count how many there were.

    observations %>%
    distinct(altitude_band_m, species) %>%
    group_by(altitude_band_m) %>%
    summarise("count" = n())
}

#' For now, only consider identifications that are to species (there were none that were to a higher taxonomic level which have not been otherwise identified).

calculate_species_richness_alt_bands(confirmed_observations_species)

#' ## Hypothesis 2
#' Create a dataframe of species abundance at each site (note that this is not really relevant in this study because multiple capture techniques were used). Convert this into a presence-absence site-species matrix.

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
                                               FUN = sum), # sum over these to calculate abundance of each species at each
                                     # site
                                     c("site_altitude", "species", "abundance"))

  return(site_species_abundance)
}

create_presence_absence_site_species_matrix <- function(observations_df) {
    #' Create a presence-absence site-species matrix. Each value is either 0 (species not observed at a site) or 1
    #' (species observed at a site).

  site_species_abundance <- create_site_species_abundance_df(observations_df)
  site_species_presenceabsence_matrix <- t(create.matrix(site_species_abundance, tax.name = "species",
                                                         locality = "site_altitude", abund = FALSE, abund.col = "abundance"))

  return(site_species_presenceabsence_matrix)
}

#' Preview the presence-absence site-species matrix. Site name is in the format altitude(m)_site where the name is an
#' abbreviation of the study area.
site_species_matrix <- create_presence_absence_site_species_matrix(confirmed_observations_species)
head(site_species_matrix)