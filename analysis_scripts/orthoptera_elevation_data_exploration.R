#' ---
#' title: Initial data exploration
#' output: 
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import packages functions from other files.

source("data_preparation.R")
source("utils.R")

vector_packages <- c("fossil", "stringr", "dplyr")
get_packages(vector_packages)

#' ## Prepare observation data

#' Import data.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"

observations <- import_all_observations(observations_file, sites_file)
sites_df <- read_csv_data_file(sites_file)
surveys_df <- read_csv_data_file(surveys_file)

site_survey_df <- join_site_survey(sites_df, surveys_df)

#' ### Subset observations identified to species
#'
#' Many small nymphs could not be identified to species level. Furthermore, some adults could only be
#' identified to genus or as far as the key would allow to the choice of two taxa. All individuals were
#' identified to the lowest taxonomic level possible.
#'
#' <br>In some parts of the analysis, only those observations identified to species will be used. Subset
#' the observations to get only those that have been identified to species.

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

#' ## Explore observation data

#' ### Summarise all observations
#'
#' The following functions calculate and summarise the number of observations and species seen across all
#' surveys.

get_number_observations <- function(observations) {
    #' Get the total number of unique observations.

  unique_observations <- unique(observations[c("specimen_label")])
  number_observations <- nrow(unique_observations)

  return(number_observations)
}

get_number_species <- function(observations) {
    #' Get the total number of unique species observed. Use observations identified to species.

  unique_species <- unique(observations[c("species")])
  number_species <- nrow(unique_species)

  return(number_species)
}

get_number_species_suborder <- function(observations) {
    #' Get the number of species observed within each suborder. Use observations identified to species.

  all_species_suborder <- observations %>%
    distinct(suborder, species) %>%
    group_by(suborder) %>%
    summarise("number_species" = n())

  return(all_species_suborder)
}

get_species_summary_overview <- function(observations) {
  #' Get a list of the species seen across all the surveys.
  #'
  #' Return a dataframe of the species seen.

  species_summary <- observations %>%
    distinct(species) %>%
    arrange(species)

  return(species_summary)
}

get_number_observations_suborder <- function(observations) {
    #' Get the number of observations for each suborder. Use confirmed and finalised identifications.
    #' They do not have to be to species level.

  all_number_observations_suborder <- observations %>%
    distinct(suborder, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(suborder) %>%
    summarise("number_observations" = n())

  return(all_number_observations_suborder)
}

join_suborder_summary_data <- function(number_observations, number_species) {
  #' Join the summary data about suborders to present together in one table.

  joined_data <- full_join(number_observations, number_species, by = "suborder")

  return(joined_data)
}

#' <br>The total number of observations was
get_number_observations(observations)

#' <br>The total number of observations identified to species was
get_number_observations(confirmed_observations_species)

#' <br>The total number of species observed was
get_number_species(confirmed_observations_species)

#' <br>Summarise the number of observations and species seen within each suborder. Note that the number of
#' species only takes into account those identified to species.
number_observations_suborder <- get_number_observations_suborder(observations)
number_species_suborder <- get_number_species_suborder(confirmed_observations_species)

join_suborder_summary_data(number_observations_suborder, number_species_suborder)

#' <br>The following species were observed
get_species_summary_overview(confirmed_observations_species)

#' ### Summarise by site
#'
#' Three main <em>study areas</em> were visited (TOR, TAV, MOL). Visits to two smaller areas near to
#' TOR/MOL (BOR, BES) were used to cover lower elevations, although these were not part of the main study
#' areas. Within each study area, there were numerous <em>study sites</em>, which were numbered, but can
#' be identified more easily by the elevational band in which they are located. Each site name includes
#' the elevation in m.
#'
#' The following functions summarise the visits to each site, then calculate observation and species
#' summary across the <em>sites</em> visited.

get_number_visits_site <- function(site_survey_df) {
  #' Get the survey data frame and group it by site name and date.
  #'
  #' Return the number of visits to each site.

  number_visits_site <- site_survey_df %>%
    distinct(site_elevation, date_cest) %>%
    group_by(site_elevation) %>%
    summarise("number_visits" = n())

  return(number_visits_site)
}

get_number_observations_site <- function(observations) {
  #' Get the observations dataframe and group it by site and observation.
  #'
  #' Return number of observations at each site.

  number_observations_site <- observations %>%
    distinct(site_elevation, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(site_elevation) %>%
    summarise("number_observations" = n())

  return(number_observations_site)
}

get_number_species_site <- function(observations) {
  #' Get the observations data frame and group it by site and species.
  #'
  #' Return the number of species seen at each site.

  number_species_site <- observations %>%
    distinct(site_elevation, species) %>%
    group_by(site_elevation, .drop=FALSE) %>%
    summarise("number_species" = n())

  return(number_species_site)
}

get_species_summary_site <- function(observations) {
  #' Get a list of the species seen at each site.
  #'
  #' Return a dataframe of the species seen at each site.

  species_summary <- observations %>%
    distinct(site_elevation, species) %>%
    group_by(site_elevation) %>%
    arrange(site_elevation, species)

  return(species_summary)
}

get_transect_lengths <- function(site_survey_df) {
  #' Get the transect length for each site.
  #'
  #' Return a data frame with the transect length for each site.

  transect_lengths_sites <- select(site_survey_df, site_elevation, transect_length_m)

  transect_lengths_sites <- transect_lengths_sites %>%
    distinct(site_elevation, transect_length_m) %>%
    group_by(site_elevation)

  return(transect_lengths_sites)
}

get_number_surveys_site <- function(site_survey_df) {
  #' Get surveys dataframe and group it by site, method and repeat to get the number of surveys
  #' done at each site.
  #'
  #' Return data frame of this summary.

  number_surveys_site <- site_survey_df %>%
    distinct(site_elevation, date_cest, method, method_repeat) %>%
    group_by(site_elevation, method, .drop=FALSE) %>%
    summarise("number_surveys" = n())

  return(number_surveys_site)
}

get_number_hand_surveys <- function(number_surveys_site) {
  #' Get number of surveys for each site and split it so there is a number for hand surveys only.
  #'
  #' Return dataframe with site and number of hand surveys.

  number_hand_surveys_site <- filter(number_surveys_site, method == "Hand")
  subset_hand_surveys <- subset(number_hand_surveys_site, select = c("site_elevation", "number_surveys"))
  names(subset_hand_surveys) <- c("site_elevation", "number_hand_surveys")

  return(subset_hand_surveys)
}

get_number_net_surveys <- function(number_surveys_site) {
  #' Get number of surveys for each site and split it so there is a number for net surveys only.
  #'
  #' Return dataframe with site and number of net surveys.

  number_net_surveys_site <- filter(number_surveys_site, method == "Net")
  subset_net_surveys <- subset(number_net_surveys_site, select = c("site_elevation", "number_surveys"))
  names(subset_net_surveys) <- c("site_elevation", "number_net_surveys")

  return(subset_net_surveys)
}

join_site_summary_data <- function(number_visits, number_observations, number_species, transect_lengths,
                                   site_survey_summary) {
  #' Get the data which summarise the sites and join it together to present the information in one table.
  #'
  #' Return the data frame of the joined data.

  joined_visits <- full_join(transect_lengths, number_visits, by = "site_elevation")
  joined_visits_surveys <- full_join(joined_visits, site_survey_summary, by = "site_elevation")
  joined_visits_observations <- full_join(joined_visits_surveys, number_observations,
                                          by = "site_elevation")
  joined_visits_observations_species <- full_join(joined_visits_observations, number_species,
                                                  by = "site_elevation")

  joined_visits_observations_species <- replace_na_with_zero(joined_visits_observations_species, "number_species")
  joined_visits_observations_species <- replace_na_with_zero(joined_visits_observations_species, "number_observations")


  return(joined_visits_observations_species)
}

get_site_survey_summary_data <- function(site_survey_df) {
  #' Get the number of hand and net surveys for each site and join them into a dataframe.
  #'
  #' Return a dataframe of the site and number of hand and net surveys.

  number_surveys_site <- get_number_surveys_site(site_survey_df)
  number_hand_surveys_site <- get_number_hand_surveys(number_surveys_site)
  number_net_surveys_site <- get_number_net_surveys(number_surveys_site)

  joined_survey_data <- full_join(number_hand_surveys_site, number_net_surveys_site,
                                  by = "site_elevation")

  return(joined_survey_data)
}

#' <br>Summarise the number of visits to each site and how many observations were seen at each site during
#' the whole season. Finally, add the number of species observed at each site (note that the number of
#' species only considers those observations that have been identified to species).


number_visits_site <- get_number_visits_site(site_survey_df)
number_observations_site <- get_number_observations_site(observations)
number_species_site <- get_number_species_site(confirmed_observations_species)
transect_lengths <- get_transect_lengths(site_survey_df)

site_survey_summary <- get_site_survey_summary_data(site_survey_df)
joined_survey_summary_data <- join_site_summary_data(number_visits_site, number_observations_site, number_species_site,
                       transect_lengths, site_survey_summary)
joined_survey_summary_data[order(joined_survey_summary_data$site_elevation), ]


#' <br>The species seen at each site were
get_species_summary_site(confirmed_observations_species)

#' ### Summarise by elevational band
#'
#' Given that this study is looking at the patterns of species richness and elevational range with
#' elevation, this next section summarises the observations and taxa according to elevation band.
#'
#' The following functions create the summaries of observations, survey details and taxa within each
#' elevational band.

get_site_elevation <- function(site_survey_df) {
  #' Get the site and its elevational band from the observations dataframe.
  #'
  #' Return dataframe of sites and elevational band.

  site_elevations <- subset(site_survey_df, select = c("site_elevation", "elevational_band_m"))

  site_elevations <- site_elevations %>%
    distinct(site_elevation, elevational_band_m)

  return(site_elevations)
}

join_site_summary_data_with_elevation <- function(site_elevations, site_summary_data) {
  #' Join the site summary data with the elevational bands so that the data can be summarised.
  #'
  #' Return dataframe with all sites and elevational bands for each one, with the site summary data.

  site_summary_data_elevation <- left_join(site_summary_data, site_elevations, by = "site_elevation")

  return(site_summary_data_elevation)
}

get_elevation_summary_data <- function(site_elevations, site_summary_data) {
  #' Summarise the site summary data over the elevational bands to get summary values for each elevational
  #' band.
  #'
  #' Return dataframe of elevational bands and summary data.

  site_summary_data_elevation <- join_site_summary_data_with_elevation(site_elevations, site_summary_data)

  columns_with_na <- c("number_hand_surveys", "number_net_surveys", "number_species")

  for (column in columns_with_na) {
    replace_na_with_zero(site_summary_data_elevation, column)
  }
  replace_na_with_zero(site_summary_data_elevation, column)

  elevation_summary_data <- site_summary_data_elevation %>%
    group_by(elevational_band_m, .drop=FALSE) %>%
    summarise("number_sites" = n(),
              "number_visits" = sum(number_visits),
              "number_hand_surveys" = sum(number_hand_surveys),
              "number_net_surveys" = sum(number_net_surveys),
              "number_observations" = sum(number_observations),
              "number_species" = sum(number_species))

  return(elevation_summary_data)
}

site_elevations <- get_site_elevation(site_survey_df)
get_elevation_summary_data(site_elevations, joined_survey_summary_data)