#' ---
#' title: Hypothesis 1
#' subtitle: Species richness decreases with elevation.
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import functions from other files.

source("utils.R")
source("orthoptera_elevation_data_exploration.R")

#' ## Change log
#' Display the latest five commits to this file.

print_latest_git_commits("orthoptera_species_richness_hypothesis1.R")

#' ## Calculate and plot species richness.

calculate_species_richness_elevation_bands <- function(observations) {
  #' Aggregate over the species observed within each elevation band and count how many there were.

  summary <- observations %>%
    distinct(altitude_band_m, species) %>%
    group_by(altitude_band_m) %>%
    summarise("count" = n())

  return(summary)
}

calculate_spearman_rank_correlation <- function(dataframe, parameter_a, parameter_b) {
  #' Calculate the Spearman's rank correlation for two parameters, a and b, in a data frame.

  spearman_corr <- cor(dataframe, method = "spearman")

  return(spearman_corr)
}

plot_altitude_species_richness <- function(species_richness_elevation) {
  #' Plot elevation band against species richness.

  plot(species_richness_elevation, xlab = "Elevation band (m a.s.l)", ylab = "Species richness")
}

#' Calculate species richness for each elevation band. For now, only consider identifications that are to species (there
#' were none that were to a higher taxonomic level which could be considered a separate species unless we consider each
#' site or elevation band separately).

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

species_richness_elevation <- calculate_species_richness_elevation_bands(confirmed_observations_species)
print(species_richness_elevation)

#' <br>Calculate Spearman rank correlation for species richness and elevation.
calculate_spearman_rank_correlation(species_richness_elevation, altitude_band_m, count)

#' <br>Plot species richness against elevation.
plot_altitude_species_richness(species_richness_elevation)