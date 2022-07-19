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
source("get_physical_site_data.R")

vector_packages <- c("visreg", "ggplot2", "dplyr", "raster", "terra", "XML", "lubridate", "sp",
                     "maptools", "leaflet", "rgeos", "corrplot", "psych", "vegan")
get_packages(vector_packages)

#' ## Create site-species matrix.

#' The following functions are used to prepare the data and create the species-site matrix, and matrix of
#' environmental variables.

create_presence_absence_site_species_matrix <- function(observations_df) {
  #' Create a presence-absence site-species matrix. Each value is either 0 (species not observed at a
  #' site) or 1 (species observed at a site).
  #'
  #' Return matrix.

  site_species_presenceabsence_matrix <- t(create.matrix(observations_df, tax.name = "taxa",
                                                         locality = "site_elevation", abund = FALSE))

  return(site_species_presenceabsence_matrix)
}

create_env_var_matrix <- function(env_var_df) {
  #' Create a matrix of the environmental variables for each site.
  #'
  #' Return matrix.

  #' set row names to be the site_elevation column
  rownames(env_var_df) <- env_var_df$site_elevation

  #' select only a subset of the parameters to use in the analysis
  env_var_matrix <- dplyr::select(env_var_df, elevational_band_m, slope, aspect,
                                  mean_perc_veg_cover, mean_height_75percent, mean_density)

  return(env_var_matrix)
}

#' Read in and prepare the site-species data for the matrix.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
vegetation_file <- "../data/vegetation_plots.csv"

sites_df <- read_csv_data_file(sites_file)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

all_observations_conservative <- get_conservative_observations(confirmed_observations,
                                                               finalised_observations)

unique_taxa_sites <- get_unique_taxa_site(all_observations_conservative)

#' Create and preview the presence-absence site-species matrix. Site name is in the format
#' elevation(m)_site where the name is an abbreviation of the study area.
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
#' have a mean of 0 and standard deviation of 1. Site aspect will be converted to four compass points and
#' other environmental variables transformed as needed.
#'
#' ### Vegetation data

vegetation_averaged_df <- prepare_veg_data(sites_file, vegetation_file)

#' ### Slope and aspect data of each site

#' Get digital elevation model for the area and calculate slope and aspect.

dem_data_besan <- "../data/dem/besan_20220601_165822.tif"
dem_data_bordes <- "../data/dem/bordes_de_viros_20220601_165754.tif"
dem_data_molinassa <- "../data/dem/la_molinassa_20220601_165849.tif"
dem_data_tavascan <- "../data/dem/tavascan_20220601_170011.tif"
dem_data_tor <- "../data/dem/tor_20220715_171056.tif"

dem_raster_besan <- raster(dem_data_besan)
dem_raster_bordes <- raster(dem_data_bordes)
dem_raster_molinassa <- raster(dem_data_molinassa)
dem_raster_tavascan <- raster(dem_data_tavascan)
dem_raster_tor <- raster(dem_data_tor)

dem_study_areas <- merge(dem_raster_besan, dem_raster_bordes, dem_raster_molinassa, dem_raster_tavascan,
                         dem_raster_tor)

#' Plot DEM to get an overview. Also look at the number of layers within the raster, the coordinate
#' system and get an overview of the data.

get_overview_dem(dem_study_areas)

#' Look at the data for each study area separately to make sure there are no bad elevation values.
#+ message=FALSE, warning=FALSE
#
# par(mfrow = c(2, 2))
# hist(dem_raster_tavascan, xlab = "Elevation (m a.s.l)", main ="Tavascan")
# hist(dem_raster_molinassa, xlab = "Elevation (m a.s.l)", main = "La Molinassa")
# hist(dem_raster_tor, xlab = "Elevation (m a.s.l)", main = "Tor")

#' Calculate the slope and aspect along each transect at the sites.

terrain_study_areas <- calculate_terrain_features(dem_study_areas)

#' Use a dictionary of site names and filenames to get the transect data for each site.
#+ message=FALSE, warning=FALSE

sites_transects_files <- c("BES01" = "../metadata/Besan site 01.gpx",
                           "BES02" = "../metadata/Besan site 02.gpx",
                           "BOR02" = "../metadata/Bordes de Viros site 02.gpx",
                           "MOL01" = "../metadata/La Molinassa site 01.gpx",
                           "MOL02" = "../metadata/La Molinassa site 02.gpx",
                           "MOL03" = "../metadata/La Molinassa site 03.gpx",
                           "MOL04" = "../metadata/La Molinassa site 04.gpx",
                           "MOL05" = "../metadata/La Molinassa site 05.gpx",
                           "MOL06" = "../metadata/La Molinassa site 06.gpx",
                           "MOL08" = "../metadata/La Molinassa site 08.gpx",
                           "MOL09" = "../metadata/La Molinassa site 09.gpx",
                           "TAV01" = "../metadata/Tavascan site 01.gpx",
                           "TAV03" = "../metadata/Tavascan site 03.gpx",
                           "TAV05" = "../metadata/Tavascan site 05.gpx",
                           "TAV06" = "../metadata/Tavascan site 06.gpx",
                           "TAV07" = "../metadata/Tavascan site 07.gpx",
                           "TAV08" = "../metadata/Tavascan site 08.gpx",
                           "TAV09" = "../metadata/Tavascan site 09.gpx",
                           "TOR01" = "../metadata/Tor site 01.gpx",
                           "TOR02" = "../metadata/Tor site 02.gpx",
                           "TOR03" = "../metadata/Tor site 03.gpx",
                           "TOR04" = "../metadata/Tor site 04.gpx",
                           "TOR05" = "../metadata/Tor site 05.gpx",
                           "TOR06" = "../metadata/Tor site 06.gpx",
                           "TOR07" = "../metadata/Tor site 07.gpx",
                           "TOR08" = "../metadata/Tor site 08.gpx",
                           "TOR09" = "../metadata/Tor site 09.gpx",
                           "TOR10" = "../metadata/Tor site 10.gpx"
)

site_terrain <- create_df_of_terrain_values_for_sites(sites_transects_files, sites_df)
site_terrain$aspect_cardinal <- apply(site_terrain, 1, convert_aspect_to_cardinal_direction)

#' Put vegetation and terrain data into one dataframe.

site_env_var_data <- left_join(site_terrain, vegetation_averaged_df, by = "site_elevation")

#' ### Check for collinearity between environmental variables

veg_params_to_compare <- site_env_var_data %>%
  dplyr::select(elevational_band_m, mean_perc_veg_cover, mean_perc_bare_ground,
                mean_per_rock, mean_max_height, mean_height_75percent,
                mean_density)
pairs.panels(veg_params_to_compare, smooth = FALSE, scale = FALSE, density = FALSE, ellipses = FALSE,
             lm = FALSE, method = "pearson", factor = 2)

#' Prepare the matrix of environmental variables to be used in the next part of the analysis. Only include
#' site elevation, aspect, slope, vegetation height, vegetation ground cover and vegetation density.

env_var_matrix <- create_env_var_matrix(site_env_var_data)

#' ## Detrended canonical correspondance analysis
#'
#' The species-site matrix contains many zeros where species were not observed at sites along the
#' elevational gradient, which in some ordination methods would lead to problems of closely associating
#' sites when they lack species (double-zero problem) [@legendreEcologicallyMeaningfulTransformations2001].
#' To determine if the response of the species data is linear or unimodal, a detrended canonical analysis
#' (DCA) will be used. If the results do not meet the criteria for a linear ordination method, then one option
#' would be to apply a Hellinger transformation, which has been shown to be suitable for presence-absence
#' data [@legendreNumericalEcology2012], to be able to then use a principle components analysis (PCA)
#' [@legendreEcologicallyMeaningfulTransformations2001]; a second option would be to use a distance method
#' of unconstrained ordination which does not rely on linear ordination, to identify any natural clusters
#' that might occur in the data due to the environmental variables.

dca <- decorana(site_species_matrix)
dca

#' Given that the length of the first axis is > 4 SD (6.03), then linear ordination cannot be used. Given
#' the heterogeneity of the data, try a Hellinger transformation to then be able to use a linear
#' ordination method.

site_species_matrix_hellinger <- decostand(site_species_matrix, "hellinger")
site_species_matrix_hellinger

tbpca_scaling_true <- rda(site_species_matrix_hellinger, scale = TRUE) # scale = TRUE -> correlation matrix **TODO**: maybe this doesn't need doing with
# the presence absence data. Make sure environmental variables are scaled
# to have a mean of 0 and variance of 1
tbpca_scaling_true
biplot(tbpca_scaling_true, scaling = "symmetric") # Best approximation to show the least distortion between sites and species
biplot(tbpca_scaling_true, scaling = "species", correlation = TRUE) # species scaling best represents the relationship between species (chi-squared diff in species composition). Relationships between sites are not as well preserved, but are approximate
screeplot(tbpca_scaling_true, bstick = TRUE, type = 'l', main = NULL)
summary(eigenvals(tbpca_scaling_true))

tbpca_scaling_false <- rda(site_species_matrix_hellinger, scale = FALSE) # scale = FALSE -> covariance matrix. Best representing the chi-squared difference between the samples (sites). **TODO**: maybe this doesn't need doing with
# the presence absence data. Make sure environmental variables are scaled
# to have a mean of 0 and variance of 1
tbpca_scaling_false
biplot(tbpca_scaling_false, scaling = "symmetric")
biplot(tbpca_scaling_false, scaling = "species")
screeplot(tbpca_scaling_false, bstick = TRUE, type = 'l', main = NULL)
summary(eigenvals(tbpca_scaling_false))

#' According to webinar, CA is used rather than PCA for presence - absence data.

cca_hellinger_species <- cca(site_species_matrix_hellinger, scaling = "species")
cca_hellinger_species
screeplot(cca_hellinger_species, bstick = TRUE, type = 'l', main = NULL)

plot(cca_hellinger_species,
     display = "species",
     scaling = "species",
     type = "n")
points(cca_hellinger_species,
       display = "species",
       scaling = "species",
       pch = 19,
       col = "blue")
set.seed(10)
ordipointlabel(cca_hellinger_species,
               display = "species",
               scaling = "species", add = TRUE)

#' Do the same but plotting sites and scaling by sites

cca_hellinger_sites <- cca(site_species_matrix_hellinger, scaling = "sites")
cca_hellinger_sites
screeplot(cca_hellinger_sites, bstick = TRUE, type = 'l', main = NULL)

plot(cca_hellinger_sites,
     display = "sites",
     scaling = "sites",
     type = "n")
points(cca_hellinger_sites,
       display = "sites",
       scaling = "sites",
       pch = 19,
       col = "blue")
set.seed(10)
ordipointlabel(cca_hellinger_sites,
               display = "sites",
               scaling = "sites", add = TRUE)

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

cca_env_data <- cca(env_var_matrix, scaling = "symmetric")
cca_env_data
#plot(cca_env_data, scaling = "symmetric")

plot(cca_env_data,
     display = "sites",
     scaling = "sites",
     type = "n")
points(cca_env_data,
       display = "sites",
       scaling = "sites",
       pch = 19,
       col = "blue")
set.seed(10)
ordipointlabel(cca_env_data,
               display = "sites",
               scaling = "sites", add = TRUE)

cca_env_formula <- cca(site_species_matrix_hellinger ~ elevational_band_m +
  aspect +
  mean_height_75percent +
  mean_density, data = env_var_matrix)
cca_env_formula
ordiplot(cca_env_formula)
ordipointlabel(cca_env_data, display = "sites", add = TRUE)

rda_env_formula <- rda(site_species_matrix_hellinger ~ elevational_band_m +
  aspect +
  mean_height_75percent +
  mean_density, data = env_var_matrix)
rda_env_formula
ordiplot(rda_env_formula)
ordipointlabel(rda_env_formula, display = "sites", add = TRUE)