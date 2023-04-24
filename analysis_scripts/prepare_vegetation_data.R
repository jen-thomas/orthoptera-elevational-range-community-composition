#' ---
#' title: Prepare vegetation data
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import packages and functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")

vector_packages <- c("visreg", "dplyr")
get_packages(vector_packages)

#' ## Vegetation data
#'
#' Calculate the mean value for each parameter at each site over both seasons.

calculate_mean_veg_parameters <- function(vegetation_df) {
  vegetation_averaged_df <- vegetation_df %>%
                      group_by(site_elevation) %>%
                      dplyr::summarise("mean_perc_veg_cover" = mean(percentage_vegetation_cover),
                                "mean_perc_bare_ground" = mean(percentage_bare_ground),
                                "mean_per_rock" = mean(percentage_rock),
                                "mean_height_75percent_cm" = mean(height_75percent_cm),
                                "mean_max_height_cm" = mean(max_height_cm),
                                "mean_density_01" = mean(density_01),
                                "mean_density_02" = mean(density_02),
                                "mean_density_03" = mean(density_03),
                                "mean_density_04" = mean(density_04),
                                "mean_density_05" = mean(density_05),
                      )

  vegetation_averaged_df$mean_density <- rowMeans(vegetation_averaged_df[ ,c("mean_density_01",
                                                                             "mean_density_02",
                                                                             "mean_density_03",
                                                                             "mean_density_04",
                                                                             "mean_density_05")])
  return(vegetation_averaged_df)
}

prepare_veg_data <- function(sites_file, vegetation_file) {

  sites_df <- read_csv_data_file(sites_file)
  vegetation_df <- read_csv_data_file(vegetation_file)

  sites_df <- rename_site_with_elevation(sites_df)
  vegetation_df <- left_join(vegetation_df, sites_df, by = "site_name", copy = TRUE)

  vegetation_averaged_df <- calculate_mean_veg_parameters(vegetation_df)

  return(vegetation_averaged_df)
}