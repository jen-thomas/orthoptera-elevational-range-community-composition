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

remove_observations_chorthippus_biguttulus_mollis <- function(observations_df) {
  #' Some observations could not be separated between Chorthippus biguttulus and Chorthippus mollis. These
  #' species have been identified at other sites and therefore are not a morphospecies. These observations
  #' will be removed from the analysis.
  #'
  #' Return the observations dataframe without these observations.

  observations_without_chorthippus_biguttulus_mollis <- observations_df[(observations_df$species !=
    "Chorthippus biguttulus / Chorthippus mollis"), ]

  return(observations_without_chorthippus_biguttulus_mollis)
}

only_observations_tav_mol <- function(observations_df) {
  #' Remove observations from Tor, Besan and Bordes de Viros to see if there is any difference in the
  #' relationship at the more semi-natural sites.
  #'
  #' Return dataframe of this subset of observations.

  observations_to_use <- observations_df[(observations_df$area == "La Molinassa" |
                                          observations_df$area == "Tavascan"), ]

  return(observations_to_use)
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
                     "elevational_range" = max_elevation - min_elevation,
                     "elevational_range_midpoint" = max_elevation - elevational_range / 2,
                     "mean_elevation" = mean(elevational_band_m))

  return(elevational_ranges_species)
}

plot_elevrange_elevation_species <- function(observations) {
  #' Plot the elevational range against elevation, with each data point coloured by species.

    ggplot(observations, aes(x = mean_elevation, y = elevational_range, colour = species)) +
    geom_point(size = 2) +
    labs(x = "Elevation (m a.s.l)", y = "Elevational range (m)") +
    theme_classic()
}

elevational_ranges_species <- calculate_elevational_range(observations_without_singletons)
elevational_ranges_species

par(mfrow = c(1,1))
plot_elevrange_elevation_species(elevational_ranges_species)

#' This plot isn't particularly useful in itself, it was just helpful to pick out some of the different
#' species and think about the results a bit more.
#'
#' ## Test Rapoport's Rule for elevation
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

  if ("sqrt_elevational_range" %in% colnames(dataframe)) {
    hist(dataframe$sqrt_elevational_range, xlab = "Sqrt elevational range (m^0.5)", ylab = ylab)
  }

  if ("log_elevational_range" %in% colnames(dataframe)) {
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

plot_elevrange_meanelevation <- function(dataframe) {
  #' Plot the elevational range as a function of the mean elevation.

  y_param <- dataframe$elevational_range
  ylab <- "Elevational range (m)"

  plot(y_param ~ mean_elevation, data = dataframe, xlab = "Mean elevation (m a.s.l)", ylab = ylab)
}

plot_elevrange_midpointelevation <- function(dataframe) {
  #' Plot the elevational range as a function of the measures of elevation.

  y_param <- dataframe$elevational_range
  ylab <- "Elevational range (m)"

  plot(y_param ~ elevational_range_midpoint, data = dataframe,
       xlab = "Elevational range midpoint (m a.s.l)", ylab = ylab)
}

linear_regression_elevrange_elevation <- function(dataframe, elevation_parameter) {
  #' Do a linear regression of elevational range as a function of elevation. Plot and summarise the
  #' regression.
  #'
  #' Return the regression.

  lin_reg <- lm(dataframe[["elevational_range"]] ~ dataframe[[elevation_parameter]])
  summary(lin_reg)

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

#' There is a slight left skew to the data, although it is not strong. Transformations of the elevational
#' range do not help.
#'
#' Plot the relationships between the measures of elevation and the elevational range at which each
#' species was observed.

par(mfrow = c(1,1))
plot_elevrange_meanelevation(elevational_ranges_species)
plot_elevrange_midpointelevation(elevational_ranges_species)

#' From these plots, we can see that the relationship does not appear to be linear.
#'
#' There seems to be a non-linear relationship between the elevational range and both measures of elevation
#' (mean and mid-point) as seen on both of the plots. The relationship could be quadratic. Test the
#' quadratic, cubic and quartic models. Calculate these parameters first.

elevational_ranges_species <- calculate_polynomials_elevation(elevational_ranges_species)

lin_regs_polynomial <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_species)

lin_reg_ <- lin_regs_polynomial[[1]]
nonlin_reg_quadratic <- lin_regs_polynomial[[2]]
nonlin_reg_cubic <- lin_regs_polynomial[[3]]
nonlin_reg_quartic <- lin_regs_polynomial[[4]]

#' Compare the models to look at the AIC and adjusted R-squared.

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

par(mfrow = c(1,1))
plot_quadratic_model(elevational_ranges_species, nonlin_reg_quadratic)

#' Get the predicted values for this model

elevational_ranges_species_predicted <- cbind(elevational_ranges_species, predict(nonlin_reg_quadratic, interval = "confidence"))

#' ### Check model assumptions

check_model_assumptions(nonlin_reg_quadratic)

#' There appears to be one data point which is skewing the residuals. Including this point, the residuals
#' do not appear to be distributed normally and there is a trend in the residuals with the
#' fitted values, suggesting that it violates the assumption of heteroscedasticity. **TODO**: how
#' sensitive is this model to violation of the assumptions? The data appeared to be bimodal to begin with.
#' Perhaps this should be corrected?
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

#' Six Ensifera species were used in the analysis above. There are too few data points to extract a
#' pattern, or  do a regression. There are notably two species that do not follow the general pattern of
#' the overall model. These species may have been harder to catch given their life history, and therefore
#' they may be undersampled. The Ensifera often occurred at elevations which had a higher species richness
#'  and therefore, it is possible that they were undersampled (there is a reference for this somewhere).
#' It is also possible that these and some Caelifera species require certain habitats which were not found
#' in the vicinity of where they were caught, thereby limiting their elevational range compared to what is
#' suggested by the model.
#'
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
plot_elevrange_elevation(elevational_ranges_caelifera)

#' The relationship between elevation and elevational range again appears to be a quadratic. Run
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

par(mfrow = c(1,1))
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
#' ## Remove Tor observations from analysis
#'
#' When looking at the relationship of species richness with elevation, there was a difference between
#' the relationships at the different sites, in particular, Tor. Here, we will remove observations from
#' Tor, Besan and les Bordes de Viros for the analysis and fit the model without these observations.
#' The two additional sites will then remove observations from lower elevations which were covered in
#' these areas. Furthermore, this will then leave only the observations at the semi-natural sites of
#' La Molinassa and Tavascan.
#'
#' ### Get observations to use
#'
#' Get all observations and select only those from Tavascan and La Molinassa
observations_tav_mol <- only_observations_tav_mol(all_observations_species)

#' Remove those that were only found at one site
species_without_singletons_tav_mol <- get_species_observed_more_than_once(observations_tav_mol)
species_for_analysis_tav_mol <- unique(species_without_singletons_tav_mol["species"])
observations_without_singletons_tav_mol <- get_observations_of_particular_species(observations_tav_mol,
                                                                          species_for_analysis_tav_mol)
number_observations_tav_mol <- nrow(observations_without_singletons_tav_mol)

#' ### Calculate elevational ranges and associated parameters

elevational_ranges_tav_mol <- calculate_elevational_range(observations_without_singletons_tav_mol)

par(mfrow = c(1,1))
plot_elevrange_meanelevation(elevational_ranges_tav_mol)

#' ### Create models

#' Calculate the polynomials then compare the models

elevational_ranges_tav_mol <- calculate_polynomials_elevation(elevational_ranges_tav_mol)

lin_regs_polynomial_tav_mol <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_tav_mol)

lin_reg_tav_mol <- lin_regs_polynomial_tav_mol[[1]]
nonlin_reg_quadratic_tav_mol <- lin_regs_polynomial_tav_mol[[2]]
nonlin_reg_cubic_tav_mol <- lin_regs_polynomial_tav_mol[[3]]
nonlin_reg_quartic_tav_mol <- lin_regs_polynomial_tav_mol[[4]]

#' Compare the models to look at the AIC and adjusted R-squared.

compareLM(lin_reg_tav_mol, nonlin_reg_quadratic_tav_mol,
          nonlin_reg_cubic_tav_mol, nonlin_reg_quartic_tav_mol)

#' We can see that the quadratic model has the lowest AIC, suggesting it is the best model for these data.
#' This model also has the largest adjusted R-squared.
#'
#' Do a direct comparison of the difference between the models using ANOVA.
#' <br>H<sub>0</sub>: there is no difference between the models.
#' <br>H<sub>1</sub>: there is a difference between the models.
#+ message=FALSE, warning=FALSE

anova(lin_reg_tav_mol, nonlin_reg_quadratic_tav_mol, nonlin_reg_cubic_tav_mol,
      nonlin_reg_quartic_tav_mol)

summary(nonlin_reg_quadratic_tav_mol)

par(mfrow = c(1,1))
plot_quadratic_model(elevational_ranges_tav_mol, nonlin_reg_quadratic_tav_mol)

#' For the observations just from Tavascan and La Molinassa, there was a similar quadratic relationship.
#' I'm not sure if this is worth showing at the moment in the results. **TODO**: decide if this
#' relationship is worth displaying on a plot with the main result and the observations from all sites.
#'
#' ## Plots
#'
#' Plot the elevational ranges from all observations that are used in the main results (this excludes
#' the singletons). Points should be different shapes for Caelifera and Ensifera. Overlay the overall
#' model. Consider overlaying the models just for Caelifera and just for Tavascan and La Molinassa.

elevationalrange_elevation_plot <- ggplot(elevational_ranges_species_predicted, aes(mean_elevation, elevational_range)) +
  geom_point(aes(shape = suborder), size = 1.5) +
  scale_shape_manual(values = c(1, 4)) +
  geom_line(aes(mean_elevation, fit), colour = "black") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Mean elevation (m a.s.l.)",
       y = "Elevational range (m)")

elevationalrange_elevation_plot <- format_theme_ggplot(elevationalrange_elevation_plot)
save_plot(elevationalrange_elevation_plot, "hypothesis2_elevational_range_model.png")

species_elevationalrange_plot <- ggplot(elevational_ranges_species_predicted,
                                        aes(x = reorder(species, -mean_elevation), y = mean_elevation)) +
  geom_point(size = 1.5) +
  geom_segment(aes(x = species, xend = species, y = min_elevation, yend = max_elevation)) +
  ylim(min(elevational_ranges_species_predicted$min_elevation),
       max(elevational_ranges_species_predicted$max_elevation)) +
  labs(x = "Species",
       y = "Elevational range (m)")

species_elevationalrange_plot <- format_theme_ggplot_vertical_xaxis_labels(species_elevationalrange_plot)
species_elevationalrange_plot
save_plot(species_elevationalrange_plot, "hypothesis2_species_elevational_range.png")

#' ## Results
#' **TODO**: results need updating here.
#' There was a significant relationship between elevation and elevational range (<em>R<sup>2</sup></em> =
#' 0.60; <em>df</em> = 2, 19; <em>p</em> = < 0.001). A maximum elevational range ($ER$) of 857 m was
#' predicted at an elevation ($E$) of 1,739 m a.s.l ($ER = -6100 + 8E - 0.0023 ER^2$).
#'
#' There was a similar significant relationship between elevation and elevational range when considerng
#' only the Caelifera (<em>R<sup>2</sup></em> = 0.78; <em>df</em> = 2, 14; <em>p</em> = < 0.001). A
#' maximum elevational range ($ER$) of 1044 m was predicted at an elevation ($E$) of 1,760 m a.s.l ($ER =
#' -6700 + 8.8E - 0.0025 ER^2$).
#'
#' **TODO**: scatterplot of elevational range against elevation.
#'
#' ## Grouping species by elevational range
#'
#' **TODO**: Species which have their elevational range midpoint within the same altitude band, will be
#' grouped. The mean and standard deviations of the elevational ranges of these groups will be calculated
#' and plotted against elevation. This will give an idea of the spread of the elevational range of species
#' groups that occur at similar elevations.
#'
#' ## Compare elevational range to climatic conditions
#'
#' **TODO**: In order to better understand the outcomes of these tests, it would be useful to compare the
#' elevational range of Orthoptera species to climatic conditions (humidity, temperature and
#' precipitation). It is unlikely there are such data available from the study region with sufficient
#' resolution (i.e ~100-m altitudinal resolution) but these could be used to instead to provide a
#' description of the conditions in this area. Sources of such data will be investigated.
#'
#' ## TODO / questions
#' <ol>
#' <li>This analysis has been done with only the identifications to species. This reduces the number of
#' observations to ~600. It didn't seem meaningful to use those to genus (or higher taxonomic order).</li>
#' <li>Do I need to justify statistically, why I have chosen to use species that occur at 3 or more
#' sites?</li>
#' <li>Is it suitable to compare the results of the analysis for Caelifera only, and overall results? i.e.
#' can we say that the addition of Ensifera to the analysis lowered the maximum elevational range, and the
#' elevation at which it occurred?</li>
#' <li>Is it okay to accept the assumptions of both of the models?</li>
#' </ol>
#' ## Questions from email 2022-07-13
#' <ol>
#' <li>I haven't included any of the specimens that were only identified to genus (or higher taxonomic
#' levels) because I don't think this is meaningful. The species within a genus have different life
#' histories and therefore I don't think the results would tell us very much. It would also reduce the
#' number of data points that are being used. Does this sound reasonable?</li>
#' <li>I would have liked to be able to compare the results for Caelifera only with the Ensifera, because
#' it seems as though there could be a similar pattern but at a lower elevation (see plot:
#' https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_elevational_range_hypothesis2.html#investigate-rapoports-rule-for-caelifera-only).
#' But there are only five Ensifera species in the subsetted data so I don't think we could do this.
#' Would it instead, be valid to say that addition of the Ensifera species lowered the maximum elevational
#' range and the elevation at which it occurred, suggesting that Ensifera may occur overall at a lower
#' elevation than Caelifera?</li>
#' <li>The quadratic relationship between elevational range and elevation is statistically significant and
#' I was thinking about the possible reasons for this pattern. My feeling is that it is possibly due to
#' an effect of the sampling. Three possibilities occur to me at the moment: a) the elevational ranges
#' that were observed here are possibly curtailed at the upper and lower levels because of the elevational
#' limits of sampling.; b) A lot fewer specimens were caught at higher elevations, therefore we are just
#' not getting the numbers or species richness to demonstrate the actual occurrences of Orthoptera at
#' these elevations; c) I do not have samples from 1300m or 1400m so this may also add to smaller ranges
#' in some species.  I am not sure how to go about testing this. One way I had thought of would be to
#' include abundance of each species, but given the use of both hand and net sampling, I cannot do this.
#' A different way would be to look at the ranges that touch the upper and / or lower elevations of the
#' sampling. I'm not sure if there is any way to test this, but it could be represented on a plot and
#' discussed? Actually, looking again at the data, I wonder if the species with smaller elevational ranges
#' are affecting the shape of the relationship and it could in fact be more linear (significant or not).
#' If I had more time (or maybe for the publication rather than my MSc), I would probably try and get hold
#' of some data from other observations in the area to add to the dataset. I don't feel it is right to
#' just remove these points even if I could try to justify it saying that they are at the edges of the
#' elevations studied and therefore it is likely their elevational ranges are smaller than they really
#' are.</li>
#' <li>I had considered covariates that would help to explain some of the patterns seen in this analysis.
#' I could use parameters of vegetation / habitat, but these would be a compilation of all of the
#' vegetation measures from each of the sites within the elevational band, so given the differences
#' depending on the study area, I don't think this would be particularly valuable. Temperature data
#' (as a summary over weeks / months / years) is not available at 100-m resolution for this area, so this
#' couldn't be used either.</li>
#' <li>Finally, I had considered exploring the relationship within each study area separately. We can
#' discuss if this is worth considering (or I will have a go if I have time before the meeting). My main
#' concern is there will not be enough observations within each study area to consider them
#' separately.</li>
#' </ol>
#'
#' ## Questions after new version after meeting
#' <ol>
#' <li>Wasn't convinced of removing the points for Tor and the other smaller sites. Removes quite a lot of
#' data and then other species are removed because they become singletons. Maybe this could just be
#' discussed.</li>
#' <li>Consider if the plot with the elevational ranges plotted for each species is necessary.</li>