#' ---
#' title: Data preparation
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import functions from other files.
#+ message=FALSE, warning=FALSE
source("utils.R")

#' ## Set-up

#' Install packages.
#+ message=FALSE, warning=FALSE
vector_packages <- c("fossil", "stringr", "tidyr")
get_packages(vector_packages)

#' ## Initial data preparation
#' Prepare the data frames with observations and sites, to be used in the rest of the analysis.
#'
#' ### Prepare data frames
#' Join the site data with observations and create new site name which contains the elevation and
#' survey area.

rename_site_with_elevation <- function(dataframe) {
    #' Create a new site name within the dataframe which includes the elevation so that it is more useful
    #' when including it in analyses and figures.
    #'
    #' Return dataframe with the additional column.

  dataframe$site_elevation <- paste(dataframe$elevational_band_m,
                                    dataframe$site_name, sep = "_")
  return(dataframe)
}

join_observation_site <- function(observations_df, sites_df) {
    #' Join the observation, survey (some already with the observations) and site data frames using a left
    #' join.
    #'
    #' Create a new column with a new site name in the format elevation_sitename, which will be more
    #' user-friendly in any outputs.
    #'
    #' Return data frame with the merged data and new site name column.

  observations <- (merge(x = observations_df, y = sites_df, by = "site_name", all.x = TRUE))[,
    c("specimen_label", "area", "site_name", "elevational_band_m", "transect_length_m", "date_cest",
      "method", "method_repeat", "suborder", "family", "subfamily", "genus", "species", "id_confidence",
      "sex", "stage")]

  observations <- rename_site_with_elevation(observations)

  return(observations)
}

join_site_survey <- function(sites_df, surveys_df) {
    #' Merge the site data onto the survey dataframe (there are more surveys than sites).
    #'
    #' Create a new column with a new site name in the format elevation_sitename, which will be more
    #' user-friendly in any outputs.
    #'
    #' Return a dataframe with both sets of data and new site name column.

  sites_surveys <- (merge(x = surveys_df, y = sites_df, by = "site_name", all.x = TRUE))[,
    c("area", "site_name", "elevational_band_m", "transect_length_m", "date_cest", "method", "method_repeat",
      "cloud_coverage_start", "wind_start", "rain_start", "cloud_coverage_end", "wind_end", "rain_end")]

  sites_surveys <- rename_site_with_elevation(sites_surveys)

  return(sites_surveys)
}

#' ### Create summaries of observation data
#' Depending on the analysis, the observations may need to be subsetted to take into account the taxonomic
#' level which they have been identified.

import_all_observations <- function(observations_file, sites_file) {
    #' Import all observations and prepare the data frame with the extra metadata from sites.
    #'
    #' Return a dataframe of all observations with site and survey metadata.

  observations_df <- read_csv_data_file(observations_file)
  sites_df <- read_csv_data_file(sites_file)

  observations_with_sites <- join_observation_site(observations_df, sites_df)

  return(observations_with_sites)
}

get_confirmed_observations <- function(observations_sites_df) {
    #' Get all observations that have been confirmed, i.e. one identification per observation. Confirmed
    #' identifications can be to any taxonomic level.
    #'
    #' Return a dataframe containing only confirmed observations.

  confirmed_observations <- observations_sites_df[(observations_sites_df$id_confidence == "Confirmed"),]

  return(confirmed_observations)
}

get_finalised_observations <- function(observations_sites_df) {
    #' Get all observations that have been finalised, i.e. multiple identifications per observation.
    #' Finalised identifications can be to any taxonomic level.
    #'
    #' Return a dataframe containing only finalised observations.

  finalised_observations <- observations_sites_df[(observations_sites_df$id_confidence == "Finalised"),]

  return(finalised_observations)
}

get_confirmed_observations_to_species <- function(observations_sites_df) {
    #' Observations that have not been identified to species level are removed. Observations that could
    #' not be identified to a specific taxa (that has not already been found in the surveys, i.e
    #' Chorthippus parallelus / montanus) are removed by considering only the confirmed observations.
    #'
    #' Return a dataframe with only confirmed observations to species.

  confirmed_observations_species <- observations_sites_df[(observations_sites_df$species != "") &
                                                            (observations_sites_df$id_confidence ==
                                                              "Confirmed"),]

  return(confirmed_observations_species)
}

get_unique_confirmed_taxa <- function(confirmed_observations) {
  #' Get the unique taxa observed from the confirmed observations.
  #'
  #' If the taxa is a species from a confirmed identification, these will all be counted separately. If
  #' the taxa is a higher taxonomic level, then identifications that have already been selected, will be
  #' checked to see if they will add to the taxa.

  #' Get the number of taxa from the confirmed observations. Return a dataframe of the confirmed, unique
  #' taxa.

  confirmed_observations_tax_levels <- select(confirmed_observations, suborder, family, subfamily,
                                              genus, species)
  observations_species <- filter(confirmed_observations_tax_levels, species != "")
  distinct_species <- distinct(observations_species)
  confirmed_taxa_df <- distinct_species

  observations_genus <- filter(confirmed_observations_tax_levels, (genus != "") & (species == ""))
  distinct_genus <- distinct(observations_genus)
  in_genus_not_species <- anti_join(distinct_genus, confirmed_taxa_df, by = c('suborder', 'family', 'subfamily', 'genus'))
  confirmed_taxa_df <- rbind(confirmed_taxa_df, in_genus_not_species)

  observations_subfamily <- filter(confirmed_observations_tax_levels, (subfamily != "") & (genus == "") & (species == ""))
  distinct_subfamily <- distinct(observations_subfamily)
  in_subfamily_not_taxa <- anti_join(distinct_subfamily, confirmed_taxa_df, by = c('suborder', 'family', 'subfamily'))
  confirmed_taxa_df <- rbind(confirmed_taxa_df, in_subfamily_not_taxa)

  observations_family <- filter(confirmed_observations_tax_levels, (family != "") & (subfamily == "") & (genus == "") & (species == ""))
  distinct_family <- distinct(observations_family)
  in_family_not_taxa <- anti_join(distinct_family, confirmed_taxa_df, by = c('suborder', 'family'))
  confirmed_taxa_df <- rbind(confirmed_taxa_df, in_family_not_taxa)

  return(confirmed_taxa_df)
}

get_caelifera_only <- function(observations) {
  #' Select only the observations of Caelifera from a dataframe.
  #'
  #' Return only the observations that are a Caelifera in a dataframe.

  caelifera_only <- filter(observations, suborder == "Caelifera")

  return(caelifera_only)
}

#' ### Create summaries of species richness data

calculate_sampling_weights <- function(observations) {
  #' Calculate sampling effort index. Sum the total number of observations, the total number caught
  #' by net and the total caught by hand. Find the proportion of observations caught by net and by hand,
  #' by dividing each of the totals by the total number of observations.
  #'
  #' For each site, multiply the number of specimens caught by hand, by the proportion of the total caught
  #' by hand. Do the same for those caught by net.
  #'
  #' This will result in an index for each site, which takes into account the number of
  #' observations and how they were caught, which is a proxy for the number of surveys done because
  #' generally, a higher number of individuals sampled will result in a higher number of species observed.

  observations_by_method <- observations %>%
    distinct(site_elevation, method, specimen_label) %>%
    group_by(site_elevation) %>%
    dplyr::summarise("number_observations_by_net" = sum(method == "Net"), "number_observations_by_hand" = sum(method == "Hand"))

  total_obs_hand <- colSums(observations_by_method[ , "number_observations_by_hand"])
  total_obs_net <- colSums(observations_by_method[ , "number_observations_by_net"])
  total_obs <- total_obs_hand + total_obs_net

  weighting_hand <- total_obs_hand / total_obs
  weighting_net <- total_obs_net / total_obs

  sampling_effort <- observations_by_method %>%
    mutate(sampling_effort_index = (number_observations_by_hand * weighting_hand) + (number_observations_by_net * weighting_net))

  return(sampling_effort)
}