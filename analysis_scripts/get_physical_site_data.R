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

#' The following functions prepare the raster DEM files and calculate the parameters, slope and aspect, of
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
  plot(dem_terrain, xlab = "UTM", ylab = "UTM")

  return(dem_terrain)
}

get_site_transect_data <- function(gpx_file) {
  #' Import a GPX file for the transect at a site. Read the coordinates and timestamps from the GPX file,
  #' then put this into a dataframe.
  #'
  #' Return dataframe of points of the transect.

  site_gpx_data <- htmlTreeParse(gpx_file, useInternalNodes = TRUE)
  coordinates <- xpathSApply(site_gpx_data, path = "//trkpt", xmlAttrs)

  site_transect_points <- data.frame(
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

  return(round(mean_slope, 0))
}

get_transect_mean_aspect <- function(interval_terrain_values) {
  #' Calculate the mean aspect from a set of values.
  #'
  #' Return the mean aspect.

  mean_aspect <- mean(interval_terrain_values$aspect)

  return(round(mean_aspect, 0))
}

#' ## Example

get_terrain_site <- function(filename, raster, site_name) {
  transect_points <- get_site_transect_data(filename)

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