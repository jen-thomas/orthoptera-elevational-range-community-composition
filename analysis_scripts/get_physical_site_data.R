#' ---
#' title: Get physical site data
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
#' <br>Import functions from other files and packages that are needed.
#+ message=FALSE, warning=FALSE

source("utils.R")

vector_packages <- c("raster", "terra", "XML", "lubridate")
get_packages(vector_packages)

#' ## Import raster data files

dem_data_besan <- "../data/dem/besan_20220601_165822.tif"
dem_data_bordes <- "../data/dem/bordes_de_viros_20220601_165754.tif"
dem_data_molinassa <- "../data/dem/la_molinassa_20220601_165849.tif"
dem_data_tavascan <- "../data/dem/tavascan_20220601_170011.tif"
dem_data_tor1 <- "../data/dem/tor_20220601_164907.tif"
dem_data_tor2 <- "../data/dem/tor_20220601_165633.tif"

dem_raster_besan <- raster(dem_data_besan)
dem_raster_bordes <- raster(dem_data_bordes)
dem_raster_molinassa <- raster(dem_data_molinassa)
dem_raster_tavascan <- raster(dem_data_tavascan)
dem_raster_tor <- merge(raster(dem_data_tor1), raster(dem_data_tor2))

dem_study_areas <- merge(dem_raster_besan, dem_raster_bordes, dem_raster_molinassa, dem_raster_tavascan, dem_raster_tor)

#' The following functions prepare the raster DEM files and calculate the parameters slope and aspect of
#' the study areas and sites.

get_overview_dem <- function(raster_dem) {
  #' Look at the summary information about a raster DEM. Look at the number of layers of data in the
  #' raster, coordinate reference system and look at the summary of the values.

  #' plot the raster data
  plot(raster_dem, main = "DEM study areas")

  #' look at the number of layers of data
  no_layers_inraster <- nlayers(raster_dem)
  no_layers_inraster

  #' check the coordinate reference system
  coord_ref_system <- crs(raster_dem)
  coord_ref_system

  #' look at summary of data values
  summary(raster_dem)

  #' look for outlying data points with histogram
  #hist(dem_study_areas)
}

calculate_terrain_features <- function(dem_raster) {
  #' Calculate the parameters, slope and aspect, of the terrain represented in the DEM.
  #'
  #' Return raster.

  dem_terrain <- terrain(dem_raster, opt = c("slope", "aspect"), unit = "degrees")
  plot(dem_terrain)

  return(dem_terrain)
}

#' Plot DEM to get an overview. Also look at the number of layers within the raster, the coordinate
#' system and get an overview of the data.

get_overview_dem(dem_study_areas)

#' Look at the data for each study area separately to make sure there are no bad elevation values.

# par(mfrow = c(2, 2))
# hist(dem_raster_tavascan)
# hist(dem_raster_molinassa)
# hist(dem_raster_tor)

#' Get slope and aspect from DEM.

terrain_study_areas <- calculate_terrain_features(dem_study_areas)

#' Read in GPX files containing transects at each study site.
#'

get_site_transect_data <- function(gpx_file) {
  #' Import a GPX file for the transect at a site. Read the coordinates and timestamps from the GPX file,
  #' then put this into a dataframe.
  #'
  #' Return dataframe of points of the transect.

  site_gpx_data <- htmlTreeParse(gpx_file, useInternalNodes = TRUE)
  coordinates <- xpathSApply(site_gpx_data, path = "//trkpt", xmlAttrs)
  timestamps <- xpathSApply(site_gpx_data, path = "//trkpt/time", xmlValue)

  site_transect_points <- data.frame(
    timestamps_cest = ymd_hms(timestamps, tz = "CEST"),
    lat = as.numeric(coordinates["lat",]),
    lon = as.numeric(coordinates["lon",])
  )

  return(site_transect_points)
}

plot_set_of_points <- function(datafrane) {
  #' Draw a simple plot of points in a dataframe.

  plot(x = datafrane$lon, y = datafrane$lat,
     type = "l", col = "blue", lwd = 3,
     xlab = "Longitude", ylab = "Latitude")
}

convert_points_to_line <- function(points) {
  #' Convert a set of ordered points to a line.
  #'
  #' Return line.
}

get_terrain_features_along_line <- function(line) {
  #' Get the slope and aspect from the DEM along a line.
  #'
  #' Return dataframe of average slope and aspect along the line.
}

bes01_transect_points <- get_site_transect_data("../metadata/Besan site 01.gpx")
plot_set_of_points(bes01_transect_points)