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

vector_packages <- c("visreg", "ggplot2", "lme4")
get_packages(vector_packages)

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



calculate_species_richness_sites <- function(observations, confirmed_observations_species) {
  #' Aggregate over the species observed at each site and count how many there were. 
  #' 
  #' Return dataframe of the site with elevation band and species richness.
  
  site_elevations <- get_site_elevation(observations)
  species_richness_site <- get_number_species_site(confirmed_observations_species)
  species_richness_site_elevation <- join_site_summary_data_with_elevation(site_elevations, species_richness_site)
  names(species_richness_site_elevation) <- c("site_elevation", "elevational_band_m", "species_richness")

  return(species_richness_site_elevation)
}

replace_na_with_zero <- function(dataframe, column) {
  #' Replace all NA values in a column with 0 (to be able to do calculations).
  #'
  #' Return dataframe.

  dataframe[[column]] <- replace(dataframe[[column]], is.na(dataframe[[column]]),0)

  return(dataframe)
}

plot_elevation_species_richness <- function(species_richness_elevation) {
  #' Plot elevation band against species richness.

  plot(species_richness ~ elevational_band_m, data = species_richness_elevation,
       xlab = "Elevation band (m a.s.l)", ylab = "Species richness")
}

linear_regression <- function(dataframe, response_variable, explanatory_variable) {
  #' Create a linear model (do a linear regression) of the explanatory variable on the response variable.
  #'
  #' Return the model.

  regression <- lm(dataframe[[response_variable]] ~ dataframe[[explanatory_variable]], data = dataframe)

  return(regression)
}

plot_model_residuals <- function(model, dataframe, explanatory_variable) {
  #' Calculate and plot the residuals from the regression as a function of the explanatory variable.

  plot(residuals(model) ~ dataframe[[explanatory_variable]], data = dataframe)
  abline(h=0)
}

calculate_correlation_coefficient <- function(dataframe, para1, para2) {
  #' Calculate the correlation coefficient between two variables.
  #'
  #' Return the coefficient.

  corr_coeff <- cor(x = dataframe[[para1]], y = dataframe[[para2]])

  return(corr_coeff)
}

calculate_coefficient_of_determination <- function(correlation_coefficient) {
  #' Calculate the coefficient of determination (R^2) for a correlation coefficient.
  #'
  #' Return the coefficient.

  coeff_det <- correlation_coefficient^2

  return(coeff_det)
}

get_predicted_values <- function(linear_regression) {
  #' Get the predicted values using the linear regression.
  #'
  #' Return data frame of predicted values.

  predicted <- predict(linear_regression, interval = "prediction")

  return(predicted)
}

plot_linear_regression_species_richness <- function(dataframe, linear_regression) {
  #' Plot the data points with 95% CI, the linear model and the lower and upper bounds for the predicted
  #' values.

  predicted_values <- get_predicted_values(linear_regression)
  df <- cbind.data.frame(dataframe, predicted_values, stringsAsFactors = FALSE)

  ggplot(df, aes(y = species_richness, x = elevational_band_m)) +
        geom_point(size = 3, col = "blue") +
        geom_smooth(method = "lm", se = TRUE) +
        geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
        geom_line(aes(y = upr), color = "black", linetype = "dashed") +
        labs(x = "Elevation (m a.s.l)", y = "Species richness") +
        theme_classic()
}

plot_elevation_species_richness_area <- function(dataframe) {
  #' Plot the species richness against elevation band. Colour by the study area.

  ggplot(dataframe, aes(x = elevational_band_m_x, y = species_richness, colour = area)) +
    geom_point(size = 2) +
    labs(x = "Elevation (m a.s.l)", y = "Species richness") +
    theme_classic()
}

#' ### Calculate species richness.
#'
#' Calculate species richness for each elevation band. For now, only consider identifications that are to
#' species (there were none that were to a higher taxonomic level which could be considered a separate
#' species unless we consider each site or elevation band separately - TODO).
#'
#' Replace NA values of species richness with 0.

confirmed_observations_species <- get_confirmed_observations_to_species(observations_file, sites_file)

species_richness_sites <- calculate_species_richness_sites(observations, confirmed_observations_species)
species_richness_sites <- replace_na_with_zero(species_richness_sites, "species_richness")

#' ### Correlation
#' Calculate the correlation coefficient
corr_coeff <- calculate_correlation_coefficient(species_richness_sites, "elevational_band_m",
                                                "species_richness")
print(corr_coeff)

#' <br>and the coefficient of determination (R<sup>2</sup>).
coeff_det <- calculate_coefficient_of_determination(corr_coeff)
print(coeff_det)

#' <br>These values confirm that there is a negative correlation between species richness and elevation,
#' however only 29% of the variation of species richness is explained by the elevation.
#'
#' <br>Plot species richness against elevation.

plot_elevation_species_richness(species_richness_sites)

#' The plot shows a general decreasing trend of species richness with elevation, which was confirmed by
#' the correlation coefficient above. However, it does not necessarily look to be linear. Survey effort,
#' which will likely have affected the results, should be taken into account: TODO.
#'
#' <em>Low species richness at 1700 m</em>. Only one site was surveyed at this altitude. This site was
#' often hot and in the Sun; Orthoptera were very active. It is likely Orthoptera were undersampled at
#' this site because of their ability to avoid the net (a lot were Ensifera) and the long vegetation which
#' made it harder to catch all individuals present.
#'
#' <em>High species richness at 2000m</em>. TODO: investigate why this is high for one of the sites (only
#' if this is still the case when the other observations identified to other taxonomic levels are included).
#'
#' <em>Higher elevation sites</em>. Only one site was visited at both 2400m and 2500m (there were no other
#' accessible sites at this elevation) and given the windy conditions at these elevations during the
#' surveys, only a few individuals were captured. At 2500m, the data currently show that no species were
#' detected there. Observations were made at this elevation, however because they were not identified to
#' species (small nymphs), they have not yet been included in the analysis: TODO (include the observations
#' made to other taxonomic levels when they add to the species richness of a site).
#'
#' ### Linear regression
#' Create linear model of species richness against elevation and look at the model.
linear_regression_species_richness <- linear_regression(species_richness_sites, "species_richness", "elevational_band_m")
summary(linear_regression_species_richness)

#' ### Checking the assumptions of linear regression

#' Plot the residuals to check if the assumptions of the residuals apply for this dataset.
plot_model_residuals(linear_regression_species_richness, species_richness_sites, "elevational_band_m")
plot(linear_regression_species_richness)

#' <br>Comments about assumptions:
#' <ul>* <em>linear relationship between elevation band and species richness</em>: according to the
#' scatterplot above, the relationship does not appear to be strongly linear, although there is not a
#' strong non-linear relationship either.
#' * <em>independence</em>: the plot of residuals shows a fairly random pattern indicating that a linear
#' model could be suitable.
#' * <em>heteroscedasticity</em>: looking at the Residuals vs Fitted plot above, there does not seem to be
#' any trend of increasing residuals as the fitted values increase, therefore this assumption seems to be
#' satisfied.
#' * <em>normally-distributed residuals</em>: the residuals appear to have a normal distribution (Q-Q plot
#' above) in general, but the point which corresponds to the highest species richness at 2000m, seems to
#' be a bit of an outlier, skewing the distribution somewhat. TODO: try refitting the model without this
#' data point (only if this is still an outlier when other observations are included).
#' </ul>
#'
#' ### Plot linear regression
#' Plot the data points with 95% CI, the linear model and the upper and lower bounds of the predicted
#' values.

plot_linear_regression_species_richness(species_richness_sites, linear_regression_species_richness)

#' ### Test linear regression using t-test.
#' Use a one-way t-test to check if there is a statistically significant relationship between the response
#' and explanatory variables.
#' <br>H<sub>0</sub>: the slope of the regression is equal to 0.
#' <br>H<sub>1</sub>: the slope of the regression is not equal to 0.
#'
#' Look again at the model output.

summary(linear_regression_species_richness)

#' ### Linear mixed model to check for effect of study area
#'
sites_df <- read_csv_data_file("../data/sites.csv")
sites_study_area <- get_study_area(sites_df)

species_richness_study_areas <- left_join(species_richness_sites, sites_study_area, by = "site_elevation",
                                          suffix = c("_x", "_y"))
species_richness_study_area_details <- subset_data_frame(species_richness_study_areas, c("site_elevation",
                                       "area", "species_richness", "elevational_band_m_x"))
as.factor(species_richness_study_area_details$area) # make sure that area is considered as a factor

#' Plot species richness against elevation again, but look to see if there is any difference that could be
#' explained by the sites being in different study areas.

plot_elevation_species_richness_area(species_richness_study_area_details)

#' Species richness at Tavascan and La Molinassa both show a general trend of decreasing species richness
#' with elevation, but at Tor, there does not seem to be such a trend.

#' Fit a linear mixed model, treating the study area as a factor variable.

lin_mixed_model <- lmer(species_richness ~ elevational_band_m_x + (1|area), data = species_richness_study_area_details)
summary(lin_mixed_model)

#' ## Results
#' A simple linear regression was used to investigate the relationship between elevation and species
#' richness. Species richness and elevation were negatively correlated (Pearson's correlation coefficient
#' = -0.54). Species richness was shown to decrease by four for an increase in elevation of 1000 m (<em>t</em> = -3.31, <em>p</em> = 0.003).
#' However, only 29% of the variation in species richness can be explained by elevation (<em>R<sup>2</sup></em> = 0.29, <em>F</em><sub>(1,27)</sub> = 10.95, <em>p</em> = 0.003).