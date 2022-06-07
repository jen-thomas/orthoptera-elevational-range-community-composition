#' ---
#' title: Hypothesis 1
#' subtitle: Species richness decreases with elevation.
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import packages functions from other files.

source("utils.R")
source("orthoptera_elevation_data_exploration.R")

#' ## Change log
#' Display the latest five commits to this file.

print_latest_git_commits("orthoptera_species_richness_hypothesis1.R")

#' ## Investigate effects of elevation on species richness.

calculate_species_richness_elevation_bands <- function(observations) {
  #' Aggregate over the species observed within each elevation band and count how many there were.
  #'
  #' Return data frame of elevation band and number of species.

  number_species_elevation <- observations %>%
    distinct(elevational_band_m, species) %>%
    group_by(elevational_band_m) %>%
    summarise("species_richness" = n())

  return(number_species_elevation)
}

plot_altitude_species_richness <- function(species_richness_elevation) {
  #' Plot elevation band against species richness.

  plot(species_richness ~ elevational_band_m, data = species_richness_elevation,
       xlab = "Elevation band (m a.s.l)", ylab = "Species richness")
}

model_species_richness_elevation <- function(species_richness_elevation) {
  #' Create a linear model of species richness as a function of elevation.
  #'
  #' Return the model.
  model <- lm(species_richness ~ elevational_band_m, data = species_richness_elevation)

  return(model)
}

#' ### Calculate species richness.
#'
#' Calculate species richness for each elevation band. For now, only consider identifications that are to
#' species (there were none that were to a higher taxonomic level which could be considered a separate
#' species unless we consider each site or elevation band separately - TODO).

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

species_richness_elevation <- calculate_species_richness_elevation_bands(confirmed_observations_species)
print(species_richness_elevation)

#' ### Plot species richness against elevation.

plot_altitude_species_richness(species_richness_elevation)

#' The plot shows a general decreasing trend of species richness with elevation. However, it does not
#' necessarily look to be linear. Survey effort, which will likely have affected the results, should be
#' taken into account: TODO.
#'
#' <em>Low species richness at 1700 m</em>. Only one site was surveyed at this altitude. This site was often hot and
#' in the Sun; Orthoptera were very active. It is likely Orthoptera were undersampled at this site because
#' of their ability to avoid the net (a lot were Ensifera) and the long vegetation which made it harder to
#' catch all individuals present.
#'
#' <em>Higher elevation sites</em>. Only one site was visited at both 2400m and 2500m (there were no other
#' accessible sites at this elevation) and given the windy conditions at these elevations during the
#' surveys, only a few individuals were captured.

#' ### Create linear model of species richness against elevation.
model_species_richness_elevation <- model_species_richness_elevation(species_richness_elevation)
summary(model_species_richness_elevation)