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
#+ message=FALSE, warning=FALSE

source("data_preparation.R")
source("utils.R")
source("get_finalised_observations_species_richness_conservative.R")
source("get_physical_site_data.R")
source("prepare_vegetation_data.R")

vector_packages <- c("fossil", "stringr", "plyr", "dplyr", "psych", "rgdal")
get_packages(vector_packages)

#' ## Prepare observation data

#' Import data.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"
vegetation_file <- "../data/vegetation_plots.csv"

sites_df <- read_csv_data_file(sites_file)
surveys_df <- read_csv_data_file(surveys_file)
site_survey_df <- join_site_survey(sites_df, surveys_df)

observations_sites_df <- import_all_observations(observations_file, sites_file)

#' ### Subset observations
#'
#' All specimens were identified to the lowest taxonomic level possible. Many small nymphs could not be
#' identified to species level. Furthermore, some adults could only be identified to genus or a higher
#' taxonomic level due to hard-to-distinguish features or contradictory features. All specimens identified
#' to one particular taxa had a "confirmed" identification.
#'
#' In some cases, specimens could only be identified to one of a set of multiple taxa, for
#' instance where contradictory features in the keys meant that they could be identified to one of two
#' particular taxa. An identification was "finalised" when a specimen could be identified to a number of
#' taxa.
#'
#' In parts of the analysis, the observations will referred to as confirmed or finalised, and be subset
#' according to these different types of identification.

#' Get the confirmed identifications.

confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)

#' Get the finalised identifications.

finalised_observations <- get_finalised_observations(observations_sites_df)
finalised_identifications <- create_finalised_observations(finalised_observations)

#' When determining what to do with specimens that were identified to multiple taxa, a conservative
#' approach was taken. There were three cases:
#' <ul>
#' <li>none of the multiple taxa had been seen at a site -> a new taxa was included for the specimen</li>
#' <li>one of the multiple taxa had been seen at a site -> no new taxa was included for the specimen</li>
#' <li>all of the multiple taxa had been seen at site -> no new taxa was included for the specimen</li>
#'  </ul>
#' In order to compare the analysis for the case where the multiple taxa could have added to the taxa at
#' a site, an approch that was not as conservative, was also taken. Taking the same cases as above:
#' <ul>
#' <li>none of the multiple taxa had been seen at a site -> a new taxa was included for the specimen</li>
#' <li>one of the multiple taxa had been seen at a site -> the other taxa was included for the specimen</li>
#' <li>all of the multiple taxa had been seen at site -> no new taxa was included for the specimen</li>
#'  </ul>
#' The identifications from the conservative approach will be used for the analysis, but the
#' none-conservative approach will be used as a comparison.

finalised_identifications_conservative <- finalised_identifications[[1]]
finalised_identifications_notconservative <- finalised_identifications[[2]]

all_observations_conservative <- join_observations(confirmed_observations, finalised_identifications_conservative)
all_observations_notconservative <- join_observations(confirmed_observations, finalised_identifications_notconservative)

#' The rest of this data exploration will use the "conservative" data.
#'
#' ## Explore observation data
#'
#' ### Summarise all observations
#' The following functions summarise the number of observations and taxa from all surveys.

get_number_observations <- function(observations) {
  #' Get the total number of unique observations.

  unique_observations <- unique(observations[c("specimen_label")])
  number_observations <- nrow(unique_observations)

  return(number_observations)
}

get_number_observations_suborder <- function(observations) {
  #' Get the number of observations for each suborder. Use confirmed and finalised identifications.
  #' They do not have to be to species level.

  all_number_observations_suborder <- observations %>%
    distinct(suborder, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(suborder) %>%
    dplyr::summarise("number_observations" = n())

  return(all_number_observations_suborder)
}

get_number_observations_adult <- function(observations) {
  #' Get the number of adults recorded. Use all observations.
  #'
  #' Return a dataframe with the number of adults and nymphs.

    adults_nymphs <- observations %>%
    distinct(stage, specimen_label) %>% # account for multiple identifications for a finalised
    # observation
    group_by(stage) %>%
    dplyr::summarise("number_observations" = n())

    return(adults_nymphs)
}


#' <br>The total number of observations was
get_number_observations(observations_sites_df)

#' <br>The total number of taxa observed from conservative observations was
unique_confirmed_taxa <- get_unique_taxa(all_observations_conservative)
nrow(unique_confirmed_taxa)

#' <br>The following taxa were observed
unique_confirmed_taxa[order(unique_confirmed_taxa$species, unique_confirmed_taxa$genus,
                            unique_confirmed_taxa$subfamily),]

#' <br>Note that although this summary suggests that 41 taxa were observed, two of the possibilities
#' listed here have been included separately in the rest of the list, so this number is actually 39. Of
#' these, 9 were Ensifera and 30 Caelifera.

#' The following number of individuals were identified per suborder
get_number_observations_suborder(all_observations_conservative)

#' <br>Calculate the number of adults and nymphs.
get_number_observations_adult(all_observations_conservative)

#' ### Summarise by site
#'
#' Three main <em>study areas</em> were visited (TOR, TAV, MOL). Visits to two smaller areas near to
#' TOR/MOL (BOR, BES) were used to cover lower elevations, although these were not part of the main study
#' areas. Within each study area, <em>study sites</em> were surveyed at different elevations. These were 
#' numbered, but can be identified more easily by the elevational band in which they are located. In this 
#' analysis, each site name includes the elevation in m.
#'
#' The following functions summarise the visits to each site, including observations and taxa.
#'
#' Numbers of taxa (rather than species) will be considered within each hypothesis separately.

get_number_sites_area <- function(site_survey_df) {
  #' Get the number of sites visited in each survey area and calculate the minimum and maximum elevations
  #' surveyed within each area.
  #'
  #' Return the number of sites within each area and min and max elevations.

  number_sites_area <- site_survey_df %>%
    distinct(area, site_name, elevational_band_m) %>%
    group_by(area) %>%
    dplyr::summarise("number_visits" = n(), "minimum_elevation" = min(elevational_band_m),
              "maximum_elevation" = max(elevational_band_m))

  return(number_sites_area)
}

get_number_visits_site <- function(site_survey_df) {
  #' Get the survey data frame and group it by site name and date.
  #'
  #' Return the number of visits to each site.

  number_visits_site <- site_survey_df %>%
    distinct(site_elevation, date_cest) %>%
    group_by(site_elevation) %>%
    dplyr::summarise("number_visits" = n())

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
    dplyr::summarise("number_observations" = n())

  return(number_observations_site)
}

get_site_elevation <- function(site_survey_df) {
  #' Get the site and its elevational band from the observations dataframe.
  #'
  #' Return dataframe of sites and elevational band.

  site_elevations <- subset(site_survey_df, select = c("site_elevation", "area", "elevational_band_m"))

  site_elevations <- site_elevations %>%
    distinct(site_elevation, area, elevational_band_m)

  return(site_elevations)
}

calculate_species_richness_sites <- function(all_observations) {
  #' For each site in the dataset, go through and get the summary of the taxa. Count the taxa for each
  #' site to give the species richness for each site.
  #'
  #' Return a dataframe with the site, elevation band and species richness.

  site_elevations <- get_site_elevation(all_observations)
  unique_taxa_site <- site_elevations

  for (i in rownames(site_elevations)) {
    site <- (site_elevations[i, "site_elevation"])
    site_observations <- filter(all_observations, all_observations$site_elevation == site)
    taxa_site <- get_unique_taxa(site_observations)
    unique_taxa_site$species_richness[unique_taxa_site$site_elevation == site] <- nrow(taxa_site)
  }

  return(unique_taxa_site)
}

get_taxa_name <- function(taxa_record) {
  #' Get the taxa name of record, whichever taxonomic level it is from.
  #'
  #' Return the taxonomic name.

  if (taxa_record$species != "") {
    taxa_name <- taxa_record$species
  }
  else if ((taxa_record$genus != "") & (taxa_record$species == "")) {
    taxa_name <- taxa_record$genus
  }
  else if ((taxa_record$subfamily != "") & (taxa_record$genus == "") & (taxa_record$species == "")) {
    taxa_name <- taxa_record$subfamily
  }
  else if ((taxa_record$family != "") & (taxa_record$subfamily == "") & (taxa_record$genus == "") &
    (taxa_record$species == "")) {
    taxa_name <- taxa_record$family
  }
  else {print("XXXXXXXXxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")}
  return(as.character(taxa_name))
}

get_species_summary_site <- function(observations) {
  #' Get a list of the species seen at each site.
  #'
  #' Return a dataframe of the species seen at each site.

  species_summary <- observations %>%
    distinct(site_elevation, species, suborder) %>%
    group_by(site_elevation) %>%
    arrange(site_elevation, species, suborder)

  return(species_summary)
}

get_transect_lengths <- function(site_survey_df) {
  #' Get the transect length for each site.
  #'
  #' Return a data frame with the transect length for each site.

  transect_lengths_sites <- dplyr::select(site_survey_df, site_elevation, transect_length_m)

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
    dplyr::summarise("number_surveys" = n())

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

join_site_summary_data <- function(number_visits, number_observations, species_richness, transect_lengths,
                                   site_survey_summary) {
  #' Get the data which summarise the sites and join it together to present the information in one table.
  #'
  #' Return the data frame of the joined data.

  joined_visits <- full_join(transect_lengths, number_visits, by = "site_elevation")
  joined_visits_surveys <- full_join(joined_visits, site_survey_summary, by = "site_elevation")
  joined_visits_observations <- full_join(joined_visits_surveys, number_observations,
                                          by = "site_elevation")
  joined_visits_observations_species <- full_join(joined_visits_observations, species_richness,
                                                  by = "site_elevation")

  joined_visits_observations_species <- replace_na_with_zero(joined_visits_observations_species,
                                                             "species_richness")
  joined_visits_observations_species <- replace_na_with_zero(joined_visits_observations_species,
                                                             "number_observations")


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

#' <br>Summarise the number of sites within each study area and get the minimum and maximum elevations
#' surveyed within each survey area.

get_number_sites_area(site_survey_df)

#' <br>Summarise the number of visits to each site and how many observations were seen at each site during
#' the whole season. Finally, add the number of species observed at each site (note that the number of
#' species only considers those observations that have been identified to species).
#+ message=FALSE, warning=FALSE

number_visits_site <- get_number_visits_site(site_survey_df)
number_observations_site <- get_number_observations_site(all_observations_conservative)
number_taxa_site <- calculate_species_richness_sites(all_observations_conservative)[c("site_elevation",
                                                                                      "species_richness")]
transect_lengths <- get_transect_lengths(site_survey_df)

site_survey_summary <- get_site_survey_summary_data(site_survey_df)
joined_survey_summary_data <- join_site_summary_data(number_visits_site, number_observations_site,
                                                     number_taxa_site, transect_lengths,
                                                     site_survey_summary)
joined_survey_summary_data[order(joined_survey_summary_data$site_elevation), ]

#' List the species seen at each site

species_summary_site <- get_species_summary_site(all_observations_conservative)

#' ### Summarise by elevational band
#'
#' Given that this study is looking at the patterns of species richness and elevational range with
#' elevation, this next section summarises the observations according to elevation band. Taxa will be
#' considered within each hypothesis.
#'
#' The following functions create the summaries of observations and survey details within each
#' elevational band.
#+ message=FALSE, warning=FALSE

join_site_summary_data_with_elevation <- function(site_elevations, site_summary_data) {
  #' Join the site summary data with the elevational bands so that the data can be summarised.
  #'
  #' Return dataframe with all sites and elevational bands for each one, with the site summary data.

  site_summary_data_elevation <- left_join(site_elevations, site_summary_data, by = "site_elevation")

  return(site_summary_data_elevation)
}

get_elevation_summary_data <- function(site_elevations, site_summary_data) {
  #' Summarise the site summary data over the elevational bands to get summary values for each elevational
  #' band.
  #'
  #' Return dataframe of elevational bands and summary data.

  site_summary_data_elevation <- join_site_summary_data_with_elevation(site_elevations, site_summary_data)

  columns_with_na <- c("number_hand_surveys", "number_net_surveys")

  for (column in columns_with_na) {
    replace_na_with_zero(site_summary_data_elevation, column)
  }
  replace_na_with_zero(site_summary_data_elevation, column)

  elevation_summary_data <- site_summary_data_elevation %>%
    group_by(elevational_band_m, .drop=FALSE) %>%
    dplyr::summarise("number_sites" = n(),
              "number_visits" = sum(number_visits),
              "number_hand_surveys" = sum(number_hand_surveys),
              "number_net_surveys" = sum(number_net_surveys),
              "number_observations" = sum(number_observations))
  return(elevation_summary_data)
}

site_elevations <- get_site_elevation(site_survey_df)
get_elevation_summary_data(site_elevations, joined_survey_summary_data)

#' ## Environmental variables
#'
#' The following functions prepare the environmental data.
#+ message=FALSE, warning=FALSE

check_collinearity <- function(env_var_df) {
    #' Select the environmental variables to check against one another for collinearity. Plot histogram,
    #' scatterplot and correlation coefficient (Spearman's rank) for each combination. This measure was
    #' used in preference to Pearson's correlation coefficient because it does not make any assumptions
    #' about the distribution of the variables.

  veg_params_to_compare <- env_var_df %>%
    dplyr::select(elevational_band_m, slope, aspect, mean_perc_veg_cover, mean_perc_bare_ground,
                  mean_per_rock, mean_max_height_cm, mean_height_75percent_cm,
                  mean_density)

  collinearity_comparison <- pairs.panels(veg_params_to_compare, smooth = FALSE, scale = FALSE, density = FALSE, ellipses = FALSE,
               lm = FALSE, method = "spearman", factor = 2)

  return(collinearity_comparison)
}

#' ### Vegetation data
#+ message=FALSE, warning=FALSE

vegetation_averaged_df <- prepare_veg_data(sites_file, vegetation_file)

#' ### Site topography

#' Get digital elevation model (DEM) data for the study areas and calculate slope and aspect at each site.
#' These parameters were calculated along each transect and averaged to get one value per site. The DEM
#' data were provided by the Institut Cartogràfic i Geològic de Catalunya (ICGC) with a resolution of
#' 2x2m.
#+ message=FALSE, warning=FALSE

dem_study_areas <- get_dem_data()

#' Plot DEM to get an overview. Only the areas of the DEM files are displayed. Also look at the number of
#' layers within the raster, the coordinate system and get an overview of the data.
#+ message=FALSE, warning=FALSE

get_overview_dem(dem_study_areas)

#' Calculate the slope and aspect along each transect.
#'
#' Each transect was recorded as a number of points. These transects were imported into R
#' and using the sp package, were extrapolated into a line. Using the rgeos and terra packages, slope and
#' aspect values were averaged from the four nearest raster cells, every 2m along the transect. These
#' values of slope and aspect were then averaged, to get one value for each site.
#+ message=FALSE, warning=FALSE

site_topography <- get_site_topography(sites_df)

#' Put vegetation and terrain data into one dataframe.
#+ message=FALSE, warning=FALSE

site_env_var_data <- left_join(site_topography, vegetation_averaged_df, by = "site_elevation")
#'
#' Check correlation of the environmental parameters.
#+ message=FALSE, warning=FALSE

check_collinearity(site_env_var_data)

#' Test if there is a significant difference between the highly correlated parameters.
#+ message=FALSE, warning=FALSE

cor_veg_height <- cor.test(site_env_var_data$mean_height_75percent_cm,
                           site_env_var_data$mean_max_height_cm, method = "spearman")
cor_slope_vegcover <- cor.test(site_env_var_data$mean_perc_veg_cover,
                               site_env_var_data$slope, method = "spearman")
cor_vegheight_density <- cor.test(site_env_var_data$mean_height_75percent_cm,
                                  site_env_var_data$mean_density, method = "spearman")
cor_veg_height
cor_slope_vegcover
cor_vegheight_density