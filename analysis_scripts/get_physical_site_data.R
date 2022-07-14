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

vector_packages <- c("raster", "terra")
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

#' Plot data to get an overview

plot(dem_study_areas, main = "DEM study areas")
crs(dem_study_areas)
summary(dem_study_areas)

hist(dem_study_areas)

par(mfrow = c(2, 2))
hist(dem_raster_tavascan)
hist(dem_raster_molinassa)
hist(merge(dem_raster_tor1, dem_raster_tor2))
