#' ---
#' title: Hypothesis 2
#' subtitle: Elevational range extent increases with elevation (Rapoport's elevational rule).
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "ggplot2", "dplyr", "rcompanion")
get_packages(vector_packages)

#' ## Prepare data.
#+ message=FALSE, warning=FALSE

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

#' See exploratory data analysis and hypothesis 1 for an explanation of confirmed and finalised
#' observations.
#'
#' Get all observations.
observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)

finalised_identifications <- create_finalised_observations(finalised_observations)

finalised_identifications_conservative <- finalised_identifications[[1]]

all_observations_conservative <- join_observations(confirmed_observations, finalised_identifications_conservative)

#' Get all observations to species only. **TODO**: consider if this should also be done to genus, or
#' include those that add another taxa (if there are enough observations).

observations_species <- get_observations_to_species(all_observations_conservative)

#' Count the number of observations of each species to see if they should be included in the analysis.

observations_count_species <- count_observations_of_species(observations_species)
observations_count_species

#' Count the number of sites at which each species has been observed.

observations_species_sites <- count_sites_where_species_observed(observations_species)
observations_species_sites

#' Subset the species considered, by selecting those where they were observed at three or more, and five
#' or more sites.
#'
#' Three sites:
species_three_or_more_sites <- observations_species_sites[(observations_species_sites$number_sites >= 3),]
species_three_or_more_sites

#' Five sites:
species_five_or_more_sites <- observations_species_sites[(observations_species_sites$number_sites >= 5),]
species_five_or_more_sites

species_for_analysis <- unique(species_three_or_more_sites["species"])
observations_to_use <- get_observations_of_particular_species(observations_species, species_for_analysis)

## Calculate elevational range for each taxa

#' The functions below calculate the elevational range of each taxa.

calculate_elevational_range <- function(observations) {
      #' Calculate the minimum, maximum and mid-point of the elevational range for each taxa. Assume here that
      #' all specimens are identified to species.

  elevational_ranges_species <- observations %>%
    distinct(species, elevational_band_m) %>%
    group_by(species) %>%
    dplyr::summarise("min_elevation" = min(elevational_band_m), "max_elevation" = max(elevational_band_m),
                     "elevational_range" = max_elevation - min_elevation,
                     "elevational_range_midpoint" = max_elevation - elevational_range / 2,
                     "mean_elevation" = mean(elevational_band_m))

  return(elevational_ranges_species)
}

elevational_ranges_species <- calculate_elevational_range(observations_to_use)

#' ## Linear regression testing Rapoport's Rule for elevation
#'
#' We hypothesise that species that live at a higher elevation will occupy a larger elevational range.
#' Elevational range was calculated by subtracting the minimum elevation at which a species was observed,
#' from the maximum.
#'
#' Two measures of elevation were calculated: the midpoint between the minimum and maximum elevation,
#' which was calculated by subtracting half of the elevational range from the maximum elevation; and the
#' mean elevation of all of the observations of each species.
#'
#' Look at the distribution of elevational range.

par(mfrow = c(2, 2))
elevational_ranges_species$sqrt_elevational_range <- sqrt(elevational_ranges_species$elevational_range)
elevational_ranges_species$log_elevational_range <- log(elevational_ranges_species$elevational_range)
hist(elevational_ranges_species$elevational_range, xlab = "Elevational range (m a.s.l)", ylab = "Frequency")
hist(elevational_ranges_species$sqrt_elevational_range, xlab = "Sqrt elevational range (m a.s.l)", ylab = "Frequency")
hist(elevational_ranges_species$log_elevational_range, xlab = "Log elevational range (m a.s.l)", ylab = "Frequency")

#' There is a slight left skew to the data. Transformations of the elevational range do not help.
#'
#' Plot the relationships between the measures of elevation and the elevational range at which each
#' species was observed.

plot(elevational_range ~ elevational_range_midpoint, data = elevational_ranges_species,
     xlab = "Elevational range midpoint (m a.s.l)", ylab = "Elevational range (m a.s.l)")

plot(elevational_range ~ mean_elevation, data = elevational_ranges_species,
     xlab = "Mean elevation (m a.s.l)", ylab = "Elevational range (m a.s.l)")

#' From these plots, we can see that the relationship does not appear to be linear.

lin_reg_elevational_range_midpoint <- lm(elevational_ranges_species[["elevational_range"]] ~
                                           elevational_ranges_species[["elevational_range_midpoint"]])
summary(lin_reg_elevational_range_midpoint)
par(mfrow = c(2, 2))
plot(lin_reg_elevational_range_midpoint)

#' Plot then test the relationship between the mean elevation and elevational range at which each species was observed.

lin_reg_mean_elevation <- lm(elevational_ranges_species[["elevational_range"]] ~
                               elevational_ranges_species[["mean_elevation"]])
summary(lin_reg_mean_elevation)
par(mfrow = c(2, 2))
plot(lin_reg_mean_elevation)

#' There is a clear non-linear relationship between the elevational range and both measures of elevation
#' (mean and mid-point) as seen on both of the plots. The linear regression shows the linear relationship
#' is not statistically significant.
#'
#' The relationship seems to be quadratic. Test the quadratic, cubic and quartic models. Calculate these
#' parameters first.

elevational_ranges_species$mean_elevation2 <- (elevational_ranges_species$mean_elevation)^2
elevational_ranges_species$mean_elevation3 <- (elevational_ranges_species$mean_elevation)^3
elevational_ranges_species$mean_elevation4 <- (elevational_ranges_species$mean_elevation)^4

nonlin_reg_quadratic <- lm(elevational_range ~ mean_elevation + mean_elevation2,
                        data = elevational_ranges_species)
nonlin_reg_cubic <- lm(elevational_range ~ mean_elevation + mean_elevation2 + mean_elevation3,
                       data = elevational_ranges_species)
nonlin_reg_quartic <- lm(elevational_range ~ mean_elevation + mean_elevation2 + mean_elevation3 +
                         mean_elevation4, data = elevational_ranges_species)

#' Compare the models to look at the AIC and adjusted R squared.

compareLM(lin_reg_mean_elevation, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

#' Do a direct comparison of the difference between the models using ANOVA.
#' <br>H<sub>0</sub>: there is no difference between the models.
#' <br>H<sub>1</sub>: there is a difference between the models.
#+ message=FALSE, warning=FALSE

anova(lin_reg_mean_elevation, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

summary(nonlin_reg_quadratic)