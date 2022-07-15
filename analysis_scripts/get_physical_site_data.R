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

vector_packages <- c("raster", "terra", "XML", "lubridate", "sp", "maptools", "leaflet", "rgeos")
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

#' Calculate slope and aspect from DEM.

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
    #timestamps_cest = ymd_hms(timestamps, tz = "CEST"),
    lon = as.numeric(coordinates["lon",]),
    lat = as.numeric(coordinates["lat",])
  )

  return(site_transect_points)
}

plot_set_of_points <- function(dataframe) {
  #' Draw a simple plot of points in a dataframe.

  plot(x = dataframe$lon, y = dataframe$lat,
     type = "l", col = "blue", lwd = 3,
     xlab = "Longitude", ylab = "Latitude")
}

convert_points_to_line <- function(site_transect_points) {
  #' Convert a set of ordered points to a set of SpatialPoints. Convert the coordinate reference system
  #' to that used by the raster, which in this case is EPSG 25831.
  #'
  #' Convert the points to a SpatialLine.
  #'
  #' Return SpatialLine object.

  site_transect_sp_points <- sp::SpatialPoints(site_transect_points, proj4string=CRS("+proj=longlat"))
  site_transect_sp_points_transformed <- spTransform(site_transect_sp_points, CRS("+init=epsg:25831"))

  site_transect_sp_line <- as(site_transect_sp_points_transformed,"SpatialLines")

  return(site_transect_sp_line)
}

get_terrain_features_along_line <- function(raster, line) {
  #' Get the slope and aspect from the DEM along a line.
  #'
  #' Return dataframe of average slope and aspect along the line.

  site_transect_terrain_all_pars <- extract(raster, line,
                                            method = "bilinear", # interpolate values from values of four
                                            # nearest raster cells
                                            buffer = 5,
                                            fun=mean, # calculate the mean value of each of the parameters
                                            # along the line
                                            na.rm=TRUE,
                                            cellnumbers=FALSE,
                                            df=TRUE, # return as dataframe
                                            exact = FALSE, # lines not polygons so not relevant
                                            factors=FALSE, # return numerical values
                                            along=FALSE,
                                            sp=FALSE
  )

  return(site_transect_terrain_all_pars)
}

get_terrain_features_at_points <- function(raster, points) {
  #' Get the slope and aspect from the DEM along a line.
  #'
  #' Return dataframe of average slope and aspect along the line.

  site_transect_terrain_all_pars <- extract(raster, points,
                                            method = "bilinear", # interpolate values from values of four
                                            # nearest raster cells
                                            buffer = 5,
                                            fun=mean, # calculate the mean value of each of the parameters
                                            # at the points
                                            na.rm=TRUE,
                                            cellnumbers=FALSE,
                                            df=TRUE, # return as dataframe
                                            exact = FALSE, # points not polygons so not relevant
                                            factors=FALSE, # return numerical values
                                            along=FALSE,
                                            sp=FALSE
  )

  return(site_transect_terrain_all_pars)
}

get_points_at_interval_along_line <- function(line, interval) {
  #' Get a point at every specified interval along a line.
  #'
  #' Return a dataframe of SpatialPoints.

  distances <- seq(0, rgeos::gLength(line), by = interval)

  points_along_line <- gInterpolate(line, distances, normalized = FALSE)

  return(points_along_line)
}

get_transect_mean_slope <- function(interval_terrain_values) {
  #' Calculate the mean slope from a set of values.
  #'
  #' Return the mean slope.

  mean_slope <- mean(interval_terrain_values$slope)

  return(mean_slope)
}

get_transect_mean_aspect <- function(interval_terrain_values) {
  #' Calculate the mean aspect from a set of values.
  #'
  #' Return the mean aspect.

  mean_aspect <- mean(interval_terrain_values$aspect)

  return(mean_aspect)
}

#' ## Example

get_terrain_site <- function(filename, raster, site_name) {
  transect_points <- get_site_transect_data(filename)
  plot_set_of_points(transect_points)

  transect_sp_line <- convert_points_to_line(transect_points)

  interval_points_along_line <- get_points_at_interval_along_line(transect_sp_line, 2)

  interval_terrain_values <- get_terrain_features_at_points(raster, interval_points_along_line)

  site_terrain_values_df <- data.frame(site_name=site_name,
                        slope=get_transect_mean_slope(interval_terrain_values),
                        aspect=get_transect_mean_aspect(interval_terrain_values))

  return(site_terrain_values_df)
}

get_gpx_filename <- function(site) {

  gpx_filename <- paste0("../metadata/", site, ".gpx")
  return(gpx_filename)
}

create_df_of_terrain_values_for_sites <- function(sites_files) {
  #' For each site, get the data file and calculate the terrain values along the transect.
  #'
  #' Put site name and values into data frame.
  #'
  #' Return data frame.

  # Create empty data frame
  terrain_df <- data.frame()

  # For each site, get the file name and use this to get the terrain values.
  for (site_name in names(sites_files)) {

    filename <- as.character(sites_files[site_name])

    site_terrain_values_df <- get_terrain_site(filename, terrain_study_areas, site_name)

    # Add the row of site name and values to the data frame.
    terrain_df <- rbind(terrain_df, site_terrain_values_df)
  }

  return(terrain_df)
}

sites_files <- c("BES01" = "../metadata/Besan site 01.gpx",
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

site_terrain <- create_df_of_terrain_values_for_sites(sites_files)
site_terrain