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

#' <br>**Project <a href="https://falciot.net/orthoptera-94940">homepage</a>**
#'
#' <br>Import functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "ggplot2", "dplyr", "rcompanion")
get_packages(vector_packages)

#' ## Prepare data
#+ message=FALSE, warning=FALSE

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

#' See exploratory data analysis and hypothesis 1 for an explanation of confirmed and finalised
#' observations. Get all observations.

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)

finalised_identifications <- create_finalised_observations(finalised_observations)

finalised_identifications_conservative <- finalised_identifications[[1]]

all_observations_conservative <- join_observations(confirmed_observations,
                                                   finalised_identifications_conservative)

#' The functions below do the preparation of the data ready for the analysis.
#+ message=FALSE, warning=FALSE

get_species_observed_more_than_once <- function(observations_df) {
  #' Count the number of observations of each species then get the species that do not occur once (known
  #' as singletons).
  #'
  #' Return a dataframe of these species.

  observations_count_species <- count_observations_of_species(observations_df)
  species_without_singletons <- observations_count_species[
                                (observations_count_species$number_observations > 1), ]

  return(species_without_singletons)
}

#' Get all observations to species only. For this part of the analysis, it is not meaningful to use those
#' only identified to genus unless this is a morphospecies that can be said to be so across all of the
#' sites. None of these cases occurred in this study.
#'
#' All species that were observed only once, will be removed from the analysis.

all_observations_species <- get_observations_to_species(all_observations_conservative)
species_without_singletons <- get_species_observed_more_than_once(all_observations_species)
species_for_analysis <- unique(species_without_singletons["species"])
observations_without_singletons <- get_observations_of_particular_species(all_observations_species,
                                                                          species_for_analysis)
number_observations <- nrow(observations_without_singletons)

#' ## Calculate elevational range for each taxa

#' The functions below, calculate the elevational range of each taxa.
#+ message=FALSE, warning=FALSE

calculate_elevational_range <- function(observations) {
      #' Calculate the minimum, maximum and mid-point of the elevational range for each taxa. Assume here
      #' that all specimens are identified to species.
      #'
      #' Return a dataframe of each species with the elevational range parameters.

  elevational_ranges_species <- observations %>%
    distinct(species, suborder, elevational_band_m) %>%
    group_by(species, suborder) %>%
    dplyr::summarise("min_elevation" = min(elevational_band_m), "max_elevation" = max(elevational_band_m),
                     "elevational_range" = max_elevation - min_elevation + 100, # assign those that are
                     # only in one band to have a range of 100m; count the elevational range of the others
                     # as the full potential range they could have been recorded at, which is the max of
                     # the highest band to the min of the lowest band. Because we take the elevational
                     # band of a site to be the minimum elevation, then we just need to add 100.
                     "elevational_range_midpoint" = min_elevation + elevational_range / 2,
                     "mean_elevation" = mean(elevational_band_m))

  return(elevational_ranges_species)
}

plot_elevrange_elevation_species <- function(observations) {
  #' Plot the elevational range against elevation, with each data point coloured by species. Elevation
  #' here is considered as the midpoint of the elevational range.

    ggplot(observations, aes(x = elevational_range_midpoint, y = elevational_range, colour = species)) +
    geom_point(size = 2) +
    labs(x = "Elevational range midpoint (m a.s.l)", y = "Elevational range (m)") +
    theme_classic()
}

elevational_ranges_species <- calculate_elevational_range(observations_without_singletons)
elevational_ranges_species

par(mfrow = c(1,1))
plot_elevrange_elevation_species(elevational_ranges_species)

#'
#' ## Test Rapoport's Rule for elevation
#'
#' We hypothesise that species that live at a higher elevation will occupy a larger elevational range.
#' Elevational range was calculated by subtracting the minimum elevation at which a species was observed,
#' from the maximum. It incorporates the full possible range, so species only found in one band are
#' artificially given a range of 100m, and all others are given the range of the max from the highest
#' band to the min of the lower band.
#'
#' Two measures of elevation were calculated: the midpoint between the minimum and maximum elevation,
#' which was calculated by subtracting half of the elevational range from the maximum elevation; and the
#' mean elevation of all of the observations of each species. Midpoint is preferred because it is not
#' biased by potentially more observations at one end of the elevational range.
#'
#' The functions below, manipulate the data needed for the modelling below.

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

  if ("sqrt_elevational_range" %in% colnames(dataframe)) {
    hist(dataframe$sqrt_elevational_range, xlab = "Sqrt elevational range (m^0.5)", ylab = ylab)
  }

  if ("log_elevational_range" %in% colnames(dataframe)) {
    hist(dataframe$log_elevational_range, xlab = "Log elevational range", ylab = ylab)
  }
}

plot_elevrange_midpointelevation <- function(dataframe) {
  #' Plot the elevational range as a function of the measures of elevation.

  y_param <- dataframe$elevational_range
  ylab <- "Elevational range (m)"

  plot(jitter(y_param, amount = 20) ~ jitter(elevational_range_midpoint, amount = 20), data = dataframe,
       xlab = "Elevational range midpoint (m a.s.l)", ylab = ylab)
}

calculate_polynomials_elevation <- function(dataframe) {
  #' Calculate the square, cube and fourth degree of the measure of elevation. Add to dataframe as new
  #' columns.
  #'
  #' Return dataframe.

  parameter <- dataframe$elevational_range_midpoint

  dataframe$elevational_range_midpoint2 <- parameter^2
  dataframe$elevational_range_midpoint3 <- parameter^3
  dataframe$elevational_range_midpoint4 <- parameter^4

  return(dataframe)
}

linear_regression_elevrange_elevation_polynomial <- function(dataframe) {
  #' Do linear regressions of elevational range as a function of elevation. The measure of elevation will
  #' be a polynomial of varying order as specified in each regression.
  #'
  #' Return a list of the regressions.

  lin_regs_polynomial <- list()

  lin_reg <- lm(elevational_range ~ elevational_range_midpoint, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(lin_reg))

  nonlin_reg_quadratic <- lm(elevational_range ~ elevational_range_midpoint + elevational_range_midpoint2, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_quadratic))

  nonlin_reg_cubic <- lm(elevational_range ~ elevational_range_midpoint + elevational_range_midpoint2 + elevational_range_midpoint3,
                       data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_cubic))

  nonlin_reg_quartic <- lm(elevational_range ~ elevational_range_midpoint + elevational_range_midpoint2 + elevational_range_midpoint3 +
                         elevational_range_midpoint4, data = dataframe)
  lin_regs_polynomial <- append(lin_regs_polynomial, list(nonlin_reg_quartic))

  return(lin_regs_polynomial)
}

plot_elevrange_elevation_suborder <- function(dataframe) {
  #' Plot the elevational range against elevation. Colour by the suborder.

  ggplot(dataframe, aes(x = elevational_range_midpoint, y = elevational_range, colour = suborder)) +
    geom_point(size = 2) +
    labs(x = "Elevational range midpoint (m a.s.l)", y = "Elevational range (m)") +
    theme_classic()
}

plot_quadratic_model <- function(dataframe, model) {
  #' Plot the elevational range as a function of elevation (data points) and include the line for the
  #' predicted values from the quadratic model.
  #'
  #' Get the parameters from the model output and add the equation of the line to the plot.

  #' Get minimum and maximum values in the dataset
  i <- seq(min(dataframe$elevational_range_midpoint), max(dataframe$elevational_range_midpoint), len=100) #  x-value limits for line

  #' Calculate the predicted values from the regression so they can be plotted as a line
  predicted_values <- predict(model, data.frame(elevational_range_midpoint=i, elevational_range_midpoint2=i*i)) #  fitted values

  plot(x = jitter(dataframe$elevational_range_midpoint, amount = 20),
       y = jitter(dataframe$elevational_range, amount = 20),
       xlab = "Elevational range midpoint (m a.s.l)", ylab = "Elevational range (m)"
  )

  lines(i, predicted_values, lty=1, lwd=2, col="black")

  # get the parameter values for the fitted line. Round the coefficients. Plot the equation on the graph.
  cf <- signif(coef(model), 2)

  equation <- paste0("ER = ", cf[1],
                     ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " E ",
                     ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " E^2 ")

  mtext(equation, side = 3, line = -1)
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

#' Plot the relationships between the measures of elevation and the elevational range at which each
#' species was observed.

par(mfrow = c(1,1))

plot_elevrange_midpointelevation(elevational_ranges_species)

#' Test the quadratic, cubic and quartic models. Calculate these parameters first.

elevational_ranges_species <- calculate_polynomials_elevation(elevational_ranges_species)

lin_regs_polynomial <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_species)

lin_reg_ <- lin_regs_polynomial[[1]]
nonlin_reg_quadratic <- lin_regs_polynomial[[2]]
nonlin_reg_cubic <- lin_regs_polynomial[[3]]
nonlin_reg_quartic <- lin_regs_polynomial[[4]]

#' Compare the models to look at the AIC and adjusted R-squared.

compareLM(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

#' Do a direct comparison of the difference between the models using ANOVA.
#' <br>H<sub>0</sub>: there is no difference between the models.
#' <br>H<sub>1</sub>: there is a difference between the models.
#+ message=FALSE, warning=FALSE

anova(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

#' View the summary of the quadratic model.

summary(nonlin_reg_quadratic)

par(mfrow = c(1,1))
plot_quadratic_model(elevational_ranges_species, nonlin_reg_quadratic)

#' Get the predicted values for this model.

elevational_ranges_species_predicted <- cbind(elevational_ranges_species, predict(nonlin_reg_quadratic, interval = "confidence"))

#' ### Check model assumptions

check_model_assumptions(nonlin_reg_quadratic)

#'
#' ## Test Rapoport's Rule for Caelifera only
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

#' Plot elevational range against elevation and colour the points by suborder.

plot_elevrange_elevation_suborder(elevational_ranges_species)

#' For this part of the analysis, consider only Caelifera.

#' Get the Caelifera observations to use.

caelifera_observations_to_use <- get_caelifera_observations(observations_without_singletons)

#' Calculate the elevational ranges (and elevation-related parameters).
#+ message=FALSE, warning=FALSE

elevational_ranges_caelifera <- calculate_elevational_range(caelifera_observations_to_use)

#' Check the distribution of the Caelifera species elevational range with elevation.

transform_elevational_range(elevational_ranges_caelifera)
par(mfrow = c(1,1))
plot_histograms_elevational_range(elevational_ranges_caelifera)

#' Plot elevational range against elevation for Caelifera only.

par(mfrow = c(1,1))
plot_elevrange_midpointelevation(elevational_ranges_caelifera)

#' Run model for polynomials up to the fourth order and do model selection to choose the most parsimonious
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

#' Get predicted values for this model.

elevational_ranges_caelifera_predicted <- cbind(elevational_ranges_caelifera, predict(nonlin_reg_quadratic_caelifera, interval = "confidence"))

par(mfrow = c(1,1))
plot_quadratic_model(elevational_ranges_caelifera, nonlin_reg_quadratic_caelifera)

#'
#' ### Check models assumptions

check_model_assumptions(nonlin_reg_quadratic_caelifera)

#'
#' ## Plots
#'
#' Plot the elevational ranges from all observations that are used in the main results (this excludes
#' the singletons). Filled circles are Caelifera; crosses are Ensifera. The first plot shows the
#' relationship between elevational range of all species with midpoint. The second plot shows the
#' relationship between elevational range with midpoint elevation for Caelifera only. Lines show the
#' best-fitted regression for each set of species.

#' ### All species

#' Get min and max of values on plot ready for predictions.

i_all_sp <- seq(min(elevational_ranges_species_predicted$elevational_range_midpoint),
         max(elevational_ranges_species_predicted$elevational_range_midpoint), len=100) #  x-value limits for line

#' Calculate the predicted values from the regression so they can be plotted as a line.

predicted_values_all_sp <- predict(nonlin_reg_quadratic,
                            data.frame(elevational_range_midpoint=i_all_sp, elevational_range_midpoint2=i_all_sp*i_all_sp)) #  fitted values
intervals_all_sp <-  predict(nonlin_reg_quadratic,
                      data.frame(elevational_range_midpoint=i_all_sp, elevational_range_midpoint2=i_all_sp*i_all_sp), interval = "confidence")

#' Put the values into a dataframe.

confidence_bands_all_sp <- data.frame(i_all_sp, intervals_all_sp)

#' Get the coefficients of the equation and put these into text.

cf_all_sp <- signif(coef(nonlin_reg_quadratic), 2)

int_term_all_sp <- cf_all_sp[1]
lin_term_all_sp <- cf_all_sp[2]
quadratic_term_all_sp <- abs(cf_all_sp[3])

equation_all_sp <- bquote(italic(E[R]) == .(int_term_all_sp) + .(lin_term_all_sp)*italic(E) - .(quadratic_term_all_sp)*italic(E)^2)

#' Create the output plot file.

path <- "../analysis_plots/"
filepath <- file.path(path, "hypothesis2_elevational_range_model.png")
png(file = filepath, width = 1000, height = 1000, units = "px", bg = "white", res = 300)

#' Do the plot.

plot_elev_range_all_species <- ggplot(data = elevational_ranges_species_predicted,
             aes(x = elevational_range_midpoint, y = elevational_range)) +
  geom_jitter(aes(shape = suborder), size = 1.8, show.legend = FALSE, width = 30, height = 20) +
  scale_shape_manual(values = c(1, 4)) +
  ylim(0, 1600) +
  xlim(1000, 2501) +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = fit), lty=1, lwd=0.5, col="black") +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = lwr),
            lwd=0.5, col="darkgrey", linetype = "dashed") +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = upr),
            lwd=0.5, col="darkgrey", linetype = "dashed") +
  annotate("text", label = equation_all_sp, x = 2000, y = 1600, cex = 3) +
  annotate("text", label = "(a)", x = 1200, y = 1500, cex = 4) +
  labs(x = "Elevational range midpoint (m a.s.l)",
       y = "Elevational range (m)") +
  theme_classic()

#' Plot and save.

plot_elev_range_all_species
dev.off()

#' ### Only Caelifera

#' Get min and max of values on plot ready for predictions.

i_cael <- seq(min(elevational_ranges_caelifera_predicted$elevational_range_midpoint),
         max(elevational_ranges_caelifera_predicted$elevational_range_midpoint), len=100) #  x-value limits for line

#' Calculate the predicted values from the regression so they can be plotted as a line.

predicted_values_cael <- predict(nonlin_reg_quadratic_caelifera,
                            data.frame(elevational_range_midpoint=i_cael, elevational_range_midpoint2=i_cael*i_cael)) #  fitted values
intervals_cael <-  predict(nonlin_reg_quadratic_caelifera,
                      data.frame(elevational_range_midpoint=i_cael, elevational_range_midpoint2=i_cael*i_cael), interval = "confidence")

#' Put the values into a dataframe.

confidence_bands_cael <- data.frame(i_cael, intervals_cael)

#' Get the coefficients of the equation and put these into text.

cf_cael <- signif(coef(nonlin_reg_quadratic_caelifera), 2)

int_term_cael <- cf_cael[1]
lin_term_cael <- cf_cael[2]
quadratic_term_cael <- abs(cf_cael[3])

equation_cael <- bquote(italic(E[R]) == .(int_term_cael) + .(lin_term_cael)*italic(E) - .(quadratic_term_cael)*italic(E)^2)

#' Create the output plot file.

path <- "../analysis_plots/"
filepath <- file.path(path, "hypothesis2_elevational_range_model_caelifera.png")
png(file = filepath, width = 1000, height = 1000, units = "px", bg = "white", res = 300)

#' Do the plot.

plot_elev_range_caelifera <- ggplot(data = elevational_ranges_caelifera_predicted,
             aes(x = elevational_range_midpoint, y = elevational_range)) +
  geom_jitter(shape = 1, size = 1.8, show.legend = FALSE, width = 30, height = 20) +
  ylim(0, 1600) +
  xlim(1000, 2501) +
  geom_line(data = confidence_bands_cael, aes(x = i_cael, y = fit), lty=1, lwd=0.5, col="black") +
  geom_line(data = confidence_bands_cael, aes(x = i_cael, y = lwr),
            lwd=0.5, col="darkgrey", linetype = "dashed") +
  geom_line(data = confidence_bands_cael, aes(x = i_cael, y = upr),
            lwd=0.5, col="darkgrey", linetype = "dashed") +
  annotate("text", label = equation_cael, x = 2000, y = 1600, cex = 3) +
  annotate("text", label = "(b)", x = 1200, y = 1500, cex = 4) +
  labs(x = "Elevational range midpoint (m a.s.l)",
       y = "Elevational range (m)") +
  theme_classic()

#' Plot and save.

plot_elev_range_caelifera
dev.off()

#' ### Elevational range and mean elevation
#'
#' Ordered by decreasing elevational range midpoint.

#' Create the output plot file.

path <- "../analysis_plots/"
filepath <- file.path(path, "hypothesis2_species_elevational_range_vertical.png")
png(file = filepath, width = 1400, height = 1400, units = "px", bg = "white", res = 300)

species_elevationalrange_plot_vertical <- ggplot(elevational_ranges_species_predicted,
                                        aes(y = reorder(species, elevational_range_midpoint), x = elevational_range_midpoint)) +
  geom_point(aes(shape = suborder), size = 1.5) +
  scale_shape_manual(values = c(1, 4)) +
  geom_vline(xintercept = 1100, linetype = "dashed", col = "grey", lwd = 0.5) +
  geom_vline(xintercept = 2600, linetype = "dashed", col = "grey", lwd = 0.5) +
  geom_segment(aes(y = species, yend = species, x = min_elevation, xend = min_elevation + elevational_range), lwd = 0.5) +
  xlim(min(elevational_ranges_species_predicted$min_elevation) - 100,
       max(elevational_ranges_species_predicted$max_elevation) + 100) +
  labs(y = "Species",
       x = "Elevation (m)")

species_elevationalrange_plot_vertical <- format_theme_ggplot_vertical_xaxis_labels(species_elevationalrange_plot_vertical)
species_elevationalrange_plot_vertical
dev.off()