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
    distinct(species, suborder, elevational_band_m) %>%
    group_by(species, suborder) %>%
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
#' The functions below manipulate the data needed for the modelling below.

transform_elevational_range <- function(dataframe) {
  #' Calculate the square-root and log of a parameter to transform it. Add it to a new column in the
  #' dataframe.
  #'
  #' Return dataframe.

  dataframe$sqrt_elevational_range <- sqrt(dataframe$elevational_range)
  dataframe$log_elevational_range <- log(dataframe$elevational_range)

  return(dataframe)
}

plot_histograms_elevational_range <- function(dataframe) {
  #' Plot histograms of the different elevational range parameters.

  par(mfrow = c(2, 2))
  ylab <- "Frequency"

  hist(dataframe$elevational_range, xlab = "Elevational range (m)", ylab = ylab)

  if (exists("dataframe$sqrt_elevational_range")) {
    hist(dataframe$sqrt_elevational_range, xlab = "Sqrt elevational range (m^0.5)", ylab = ylab)
  }

  if (exists("dataframe$log_elevational_range")) {
    hist(dataframe$log_elevational_range, xlab = "Log elevational range", ylab = ylab)
  }
}

plot_elevrange_elevation <- function(dataframe) {
  #' Plot the elevational range as a function of the measures of elevation.

  y_param <- dataframe$elevational_range
  ylab <- "Elevational range (m)"

  plot(y_param ~ elevational_range_midpoint, data = dataframe,
       xlab = "Elevational range midpoint (m a.s.l)", ylab = ylab)

  plot(y_param ~ mean_elevation, data = dataframe, xlab = "Mean elevation (m a.s.l)", ylab = ylab)
}

linear_regression_elevrange_elevation <- function(dataframe, elevation_parameter) {
  #' Do a linear regression of elevational range as a function of elevation. Plot and summarise the
  #' regression.
  #'
  #' Return the regression.

  lin_reg <- lm(dataframe[["elevational_range"]] ~ dataframe[[elevation_parameter]])
  summary(lin_reg)

  par(mfrow = c(2, 2))
  plot(lin_reg)

  return(lin_reg)
}

calculate_polynomials_elevation <- function(dataframe) {
  #' Calculate the square, cube and fourth degree of the measure of elevation. Add to dataframe as new
  #' columns.
  #'
  #' Return dataframe.

  parameter <- dataframe$mean_elevation

  dataframe$mean_elevation2 <- parameter^2
  dataframe$mean_elevation3 <- parameter^3
  dataframe$mean_elevation4 <- parameter^4

  return(dataframe)
}

linear_regression_elevrange_elevation_polynomial <- function(dataframe) {
  #' Do linear regressions of elevational range as a function of elevation. The measure of elevation will
  #' be a polynomial of varying order as specified in each regression.
  #'
  #' Return a list of the regressions.

  lin_regs_polynomial <- list()

  lin_reg <- lm(elevational_range ~ mean_elevation, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(lin_reg))

  nonlin_reg_quadratic <- lm(elevational_range ~ mean_elevation + mean_elevation2, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_quadratic))

  nonlin_reg_cubic <- lm(elevational_range ~ mean_elevation + mean_elevation2 + mean_elevation3,
                       data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_cubic))

  nonlin_reg_quartic <- lm(elevational_range ~ mean_elevation + mean_elevation2 + mean_elevation3 +
                         mean_elevation4, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_quartic))

  return(lin_regs_polynomial)
}

plot_quadratic_model <- function(dataframe, model) {
  #' Plot the elevational range as a function of elevation (data points) and include the line for the
  #' predicted values from the quadratic model.
  #'
  #' Get the parameters from the model output and add the equation of the line to the plot.

  plot_elevrange_elevation(dataframe)
  i <- seq(min(dataframe$mean_elevation), max(dataframe$mean_elevation), len=100) #  x-value limits for line
  predicted_values <- predict(model, data.frame(mean_elevation=i, mean_elevation2=i*i)) #  fitted values
  lines(i, predicted_values, lty=1, lwd=2, col="blue")

  # get the parameter values for the fitted line. Round the coefficients. Plot the equation on the graph.
  cf <- signif(coef(model), 2)

  equation <- paste0("ER = ", cf[1],
                     ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " E ",
                     ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " E^2 ")

  mtext(equation, side = 3, line = -13)
}

check_model_assumptions <- function(model) {
  #' Plot a histogram of the residuals to check for a normal distribution. Secondly, plot the residuals
  #' against the predicted values, to check for homoscedasticity.

  hist(residuals(model), col="darkgray")

  plot(fitted(model), residuals(model))
}

#' Look at the distribution of elevational range, as well as the transformed elevational range
#' (square-root and log).

elevational_ranges_species <- transform_elevational_range(elevational_ranges_species)

plot_histograms_elevational_range(elevational_ranges_species)

#' There is a slight left skew to the data. Transformations of the elevational range do not help.
#'
#' Plot the relationships between the measures of elevation and the elevational range at which each
#' species was observed.

plot_elevrange_elevation(elevational_ranges_species)

#' From these plots, we can see that the relationship does not appear to be linear. There appears to be
#' little difference between the two measures of elevation in the plot. **TODO** decide which measure of
#' elevation seems to be most appropriate from the literature.

#' Try a linear regression to see if there is any relationship (although this is not expected because
#' there seems to be a clear non-linear (likely quadratic) relationship.

lin_reg_elevational_range_midpoint <- linear_regression_elevrange_elevation(elevational_ranges_species, "elevational_range_midpoint")
lin_reg_mean_elevation <- linear_regression_elevrange_elevation(elevational_ranges_species, "mean_elevation")

#' There is a clear non-linear relationship between the elevational range and both measures of elevation
#' (mean and mid-point) as seen on both of the plots. The linear regression shows the linear relationship
#' is not statistically significant.
#'
#' The relationship seems to be quadratic. Test the quadratic, cubic and quartic models. Calculate these
#' parameters first.

elevational_ranges_species <- calculate_polynomials_elevation(elevational_ranges_species)

lin_regs_polynomial <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_species)

lin_reg_ <- lin_regs_polynomial[[1]]
nonlin_reg_quadratic <- lin_regs_polynomial[[2]]
nonlin_reg_cubic <- lin_regs_polynomial[[3]]
nonlin_reg_quartic <- lin_regs_polynomial[[4]]

#' Compare the models to look at the AIC and adjusted R squared.

compareLM(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

#' We can see that the quadratic model has the lowest AIC, suggesting it is the best model for these data.
#' This model also has the largest adjusted R-squared.
#'
#' Do a direct comparison of the difference between the models using ANOVA.
#' <br>H<sub>0</sub>: there is no difference between the models.
#' <br>H<sub>1</sub>: there is a difference between the models.
#+ message=FALSE, warning=FALSE

anova(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

#' The comparison of the models shows that the quadratic is significantly better than the linear model,
#' but that the cubic is not significantly better than quadratic, and the quartic is not significantly
#' better than the cubic.
#'
#' This evidence suggests that the quadratic model should be selected. View the summary of the quadratic
#' model.

summary(nonlin_reg_quadratic)

plot_quadratic_model(elevational_ranges_species, nonlin_reg_quadratic)

#' ### Check model assumptions

check_model_assumptions(nonlin_reg_quadratic)

#' There appears to be one data point which is skewing the residuals. Including this point, the residuals
#' do not appear to be distributed normally and there is a trend in the residuals with the
#' fitted values, suggesting that it violates the assumption of heteroscedasticity. However, without this
#' point, the assumption of heteroscedasticity does not appear to be violated. **TODO**: how
#' sensitive is this model to violation of the assumptions?
#'
#' ## Investigate Rapoport's Rule for Caelifera only
#'
#' The functions below prepare the data and run the models for Caelifera only.

get_caelifera_observations <- function(observations_df) {
  #' Subset the observations of Caelifera only from those that have been identified to use in this
  #' analysis.
  #'
  #' Return the dataframe of Caelifera.

  caelifera_observations <- filter(observations_df, suborder == "Caelifera")

  return(caelifera_observations)
}

plot_elevrange_elevation_suborder <- function(dataframe) {
  #' Plot the elevational range against elevation. Colour by the suborder.

  ggplot(dataframe, aes(x = mean_elevation, y = elevational_range, colour = suborder)) +
    geom_point(size = 2) +
    labs(x = "Elevation (m a.s.l)", y = "Elevational range (m)") +
    theme_classic()
}

#' Plot elevational range against elevation and colour the points by suborder. Only a small number of
#' Ensifera are being used in this analysis, so it might be worth considering Caelifera only.

plot_elevrange_elevation_suborder(elevational_ranges_species)

#' Only five Ensifera species were considered to have enough data to be used in the analysis above. There
#' are too few data points to extract a pattern, but is there potentially a peak as there is in Caelifera,
#' but at a lower elevation? This would be an interesting point to explore if there were more specimens.
#'
#' For this part of the analysis, consider only Caelifera.

#' Get the Caelifera observations to use.

caelifera_observations_to_use <- get_caelifera_observations(observations_to_use)

#' Calculate the elevational ranges (and elevation-related parameters).
#+ message=FALSE, warning=FALSE

elevational_ranges_caelifera <- calculate_elevational_range(caelifera_observations_to_use)

#' Check the distribution of the Caelifera species elevational range with elevation.

transform_elevational_range(elevational_ranges_caelifera)
plot_histograms_elevational_range(elevational_ranges_caelifera)

#' Plot elevational range against elevation for Caelifera only.

plot_elevrange_elevation(elevational_ranges_caelifera)

#' The relationship between elevation and elevational range again appears to be an inverse quadratic. Run
#' model for polynomials up to the fourth order and do model selection to choose the most parsimonious
#' model.

elevational_ranges_caelifera <- calculate_polynomials_elevation(elevational_ranges_caelifera)

lin_regs_polynomial_caelifera <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_caelifera)

lin_reg_caelifera <- lin_regs_polynomial_caelifera[[1]]
nonlin_reg_quadratic_caelifera <- lin_regs_polynomial_caelifera[[2]]
nonlin_reg_cubic_caelifera <- lin_regs_polynomial_caelifera[[3]]
nonlin_reg_quartic_caelifera <- lin_regs_polynomial_caelifera[[4]]

compareLM(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

anova(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

summary(nonlin_reg_quadratic_caelifera)

plot_quadratic_model(elevational_ranges_caelifera, nonlin_reg_quadratic_caelifera)

#' We can see that the quadratic model has the lowest AIC, suggesting it is the best model for these data.
#' This model also has the largest adjusted R-squared.
#'
#' Using ANOVA for a direct comparison between the models, the quadratic model is significantly better
#' than the linear model and the cubic and quartic models do not show any significant improvement on the
#' quadratic.
#'
#' ### Check models assumptions

check_model_assumptions(nonlin_reg_quadratic_caelifera)

#' It is hard to tell if the assumption of heteroscedasticity is violated or not because there are many
#' more data points for higher fitted values. The residuals have a left-skewed distribution.
#'
#' ## Results

