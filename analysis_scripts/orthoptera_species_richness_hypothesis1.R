#' ---
#' title: Hypothesis 1
#' subtitle: Species richness decreases with elevation.
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 4
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' **TODO (as of 2022-07-04):**
#' <ul>
#'  <li>incorporate "finalised" observations</li>
#'  <li>**all numbers in the text need to be updated to what is in the stats output**.</li>
#' </ul>
#' <br>Import packages and functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "ggplot2", "lmerTest")
get_packages(vector_packages)

#' ## Investigate effects of elevation on species richness
#+ message=FALSE, warning=FALSE

calculate_species_richness_sites <- function(all_observations, confirmed_observations) {
  #' For each site in the dataset, go through and get the summary of the taxa. Count the taxa for each
  #' site to give the species richness for each site.
  #'
  #' Return a dataframe with the site, elevation band and species richness.

  site_elevations <- get_site_elevation(all_observations)
  unique_taxa_site <- site_elevations

  for (i in rownames(site_elevations)) {
    site <- (site_elevations[i, "site_elevation"])
    site_observations <- filter(confirmed_observations, confirmed_observations$site_elevation == site)
    taxa_site <- get_unique_confirmed_taxa(site_observations)
    unique_taxa_site$species_richness[unique_taxa_site$site_elevation == site] <- nrow(taxa_site)
  }

  return(unique_taxa_site)
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

linear_regression_species_richness_elevation <- function(dataframe, species_richness,
elevational_band_m) {
  #' Create a linear model (do a linear regression) of the explanatory variable (elevation) on the
  #' response variable (species richness).
  #'
  #' Return the model.

  regression <- lm(dataframe[[species_richness]] ~ dataframe[[elevational_band_m]])

  return(regression)
}

plot_model_residuals <- function(model, dataframe, explanatory_variable) {
  #' Calculate and plot the residuals from the regression as a function of the explanatory variable.

  plot(residuals(model) ~ dataframe[[explanatory_variable]], data = dataframe)
  abline(h=0)
}

correlation_test <- function(dataframe, para1, para2) {
  #' Calculate the correlation coefficient between two variables.
  #'
  #' Return the coefficient.

  corr_test <- cor.test(x = dataframe[[para1]], y = dataframe[[para2]])

  return(corr_test)
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
        #geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
        #geom_line(aes(y = upr), color = "black", linetype = "dashed") +
        labs(x = "Elevation (m a.s.l)", y = "Species richness") +
        theme_classic()
}

plot_elevation_species_richness_area <- function(dataframe) {
  #' Plot the species richness against elevation band. Colour by the study area.

  ggplot(dataframe, aes(x = elevational_band_m, y = species_richness, colour = area)) +
    geom_point(size = 2) +
    labs(x = "Elevation (m a.s.l)", y = "Species richness") +
    theme_classic()
}

plot_lin_reg_sampling_effort_species_richness <- function(dataframe, linear_regression) {
  #' Plot the data points with 95% CI, the linear model and the lower and upper bounds for the predicted
  #' values.

  predicted_values <- get_predicted_values(linear_regression)
  df <- cbind.data.frame(dataframe, predicted_values, stringsAsFactors = FALSE)

  ggplot(df, aes(y = species_richness, x = sampling_effort_index)) +
        geom_point(size = 3, col = "blue") +
        geom_smooth(method = "lm", se = TRUE) +
        #geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
        #geom_line(aes(y = upr), color = "black", linetype = "dashed") +
        labs(x = "Sampling effort index", y = "Species richness") +
        theme_classic()
}

plot_lin_reg_sampling_effort_elevation <- function(dataframe, linear_regression) {
  #' Plot the data points with 95% CI, the linear model and the lower and upper bounds for the predicted
  #' values.

  predicted_values <- get_predicted_values(linear_regression)
  df <- cbind.data.frame(dataframe, predicted_values, stringsAsFactors = FALSE)

  ggplot(df, aes(y = sampling_effort_index, x = elevational_band_m)) +
        geom_point(size = 3, col = "blue") +
        geom_smooth(method = "lm", se = TRUE) +
        #geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
        #geom_line(aes(y = upr), color = "black", linetype = "dashed") +
        labs(x = "Elevation (m a.s.l)", y = "Sampling effort index") +
        theme_classic()
}

#' ### Calculate species richness
#'
#' Calculate species richness for each site.
#' <br>**TODO**: add the taxa from the finalised observations.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"

sites_df <- read_csv_data_file(sites_file)
surveys_df <- read_csv_data_file(surveys_file)
site_survey_df <- join_site_survey(sites_df, surveys_df)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)

species_richness_sites <- calculate_species_richness_sites(observations_sites_df, confirmed_observations)
print(species_richness_sites)

#' ### Correlation
#' Do a correlation test between the species richness at each site and elevation.
#' <br>H<sub>0</sub>: the correlation coefficient is equal to 0.
#' <br>H<sub>1</sub>: the correlation coefficient is different from 0.
#'
#' View the test
corr_test <- correlation_test(species_richness_sites, "elevational_band_m",
                                                "species_richness")
corr_coeff <- corr_test$estimate
print(corr_test)

#' <br>and calculate the coefficient of determination (R<sup>2</sup>).
coeff_det <- calculate_coefficient_of_determination(corr_coeff)
print(coeff_det)

#' <br>The relationship between species richness and elevation was tested using a Pearson's correlation.
#' There was evidence to suggest a significant negative relationship between species richness and
#' elevation (<em>r</em> = -0.63, <em>t<sub>26</sub></em> = -4.12, <em>p</em> = 0.0004), however only
#' 39% of the variation in species richness is explained by the elevation.
#'
#' ### Plot species richness against elevation.

plot_elevation_species_richness(species_richness_sites)

#' The plot shows a general decreasing trend of species richness with elevation, which was confirmed by
#' the correlation coefficient above.
#'
#' ### Linear regression
#' Create linear model of species richness against elevation and look at the model.
linear_regression_species_richness <- linear_regression_species_richness_elevation(species_richness_sites, "species_richness", "elevational_band_m")
summary(linear_regression_species_richness)

#' The linear regression shows that both the intercept and slope are statistically significant. Species
#' richness decreases by 3.8 with an increase in elevation of 1000 m (<em>t</em> = -3.4, <em>p</em> = 0.002).
#'
#' ### Checking the assumptions of linear regression

#' Plot the residuals to check if the assumptions of the residuals apply for this dataset.
plot_model_residuals(linear_regression_species_richness, species_richness_sites, "elevational_band_m")
par(mfrow = c(2,2))
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
#' be a bit of an outlier, skewing the distribution somewhat.
#' </ul>
#'
#' ### Plot linear regression
#' Plot the data points with 95% CI, the linear model and the upper and lower bounds of the predicted
#' values.
#+ message=FALSE, warning=FALSE

plot_linear_regression_species_richness(species_richness_sites, linear_regression_species_richness)

#' ### Test linear regression using t-test
#' Use a one-way t-test to check if there is a statistically significant relationship between the response
#' and explanatory variables.
#' <br>H<sub>0</sub>: the slope of the regression is equal to 0.
#' <br>H<sub>1</sub>: the slope of the regression is not equal to 0.
#'
#' Look again at the model output.

summary(linear_regression_species_richness)

#' ## Account for the effects of sampling effort
#'
#' Given the difference in the number of hand and net surveys undertaken at each site, an index of
#' sampling effort was calculated.
#'
#' First, the proportion of all specimens captured by hand ($prop_{hand}$) and by the net ($prop_{net}$)
#' was calculated. Secondly, the index for sampling effort was calculated for each site according
#' to the following equation: $$weighting = obs_{hand} * prop_{hand} + obs_{net} * prop_{net}$$ where
#' $obs_{hand}$ and $obs_{net}$ were the number of specimens captured at the site by each sampling
#' method.

sampling_effort <- calculate_sampling_weights(confirmed_observations) # TODO change this to all
# observations when including the finalised observations as well

species_richness_sites <- left_join(species_richness_sites, sampling_effort, by = "site_elevation")

#' Regress the elevation and species richness against the sampling effort index.
#+ message=FALSE, warning=FALSE

lin_reg_species_richness_sampling_effort <- lm(species_richness_sites[["species_richness"]] ~ species_richness_sites[["sampling_effort_index"]], data = species_richness_sites)
summary(lin_reg_species_richness_sampling_effort)
plot_lin_reg_sampling_effort_species_richness(species_richness_sites, lin_reg_species_richness_sampling_effort)

lin_reg_elevation_sampling_effort <- lm(species_richness_sites[["sampling_effort_index"]] ~ species_richness_sites[["elevational_band_m"]], data = species_richness_sites)
summary(lin_reg_elevation_sampling_effort)
plot_lin_reg_sampling_effort_elevation(species_richness_sites, lin_reg_elevation_sampling_effort)

#' Do the linear regression with sampling effort as a random factor.
#' #+ message=FALSE, warning=FALSE

lmm_species_richness_elev_sampling_effort <- lmer(species_richness ~ elevational_band_m + (1|sampling_effort_index),
                                       data = species_richness_sites)
summary(lmm_species_richness_elev_sampling_effort)

#' ## Linear mixed model to check for effect of study area
#'

species_richness_sites$area <- as.factor(species_richness_sites$area) # make sure that area is considered
# as a factor

#' Plot species richness against elevation again with different colours representing the different study
#' areas to visualise and possible differences in the relationship which could depend on the study area.

plot_elevation_species_richness_area(species_richness_sites)

#' Species richness at Tavascan and La Molinassa both show a general trend of decreasing species richness
#' with elevation, but at Tor, there does not seem to be such a trend.
#'
#' ### Fit a linear mixed model
#' A linear mixed model will be fitted, treating study area and sampling effort as a random factor.
#+ message=FALSE, warning=FALSE

lmm_species_richness_elev_area_sampling_effort <- lmer(species_richness ~ elevational_band_m + (1|area) + (1|sampling_effort_index),
                                       data = species_richness_sites)
summary(lmm_species_richness_elev_area_sampling_effort)

#' The value of variance for area (random effects section) is 0 (or presumably just very small if only
#' 3dp are used). I understand this can occur because of sampling error. TODO: check if this is important.
#'
#' I realise that I'm not sure how to interpret this output. The parameter estimates are the same as
#' those in the model above. How does this output tell us that area is important?
#'
#' ### Plot linear mixed model

plot(lmm_species_richness_elev_area_sampling_effort)

#'
#' ### Test the assumptions of the linear mixed model

qqnorm(resid(lmm_species_richness_elev_area_sampling_effort))
qqline(resid(lmm_species_richness_elev_area_sampling_effort))

#' The plot of the residuals doesn't show an obvious pattern. It might be possible to discern a slight
#' decrease overall. The residuals seem to have a normal distribution, however there is one obvious
#' outlier in both plots.
#'
#' ## Investigate effects of elevation on species richness within different study areas
#' Given the differences between Tor and the other two study areas, La Molinassa and Tavascan, a linear
#' regression will be used to look at the relationship between species richness and elevation at each of
#' these sites separately.
#'
#' ### Linear regression

#'  Run linear regression for each site separately. Ignore the two small sites which do not have an
#' elevational gradient.
#'
#' #### Tor
#+ message=FALSE, warning=FALSE

species_richness_tor <- species_richness_sites[species_richness_sites$area == "Tor", ]

lin_reg_species_richness_area_tor <- linear_regression(species_richness_tor,
                                                       "species_richness", "elevational_band_m")
summary(lin_reg_species_richness_area_tor)

plot_linear_regression_species_richness(species_richness_tor, lin_reg_species_richness_area_tor)

#' #### La Molinassa
#+ message=FALSE, warning=FALSE

species_richness_mol <- species_richness_sites[species_richness_sites$area == "La Molinassa", ]

lin_reg_species_richness_area_mol <- linear_regression(species_richness_mol,
                                                                 "species_richness", "elevational_band_m")
summary(lin_reg_species_richness_area_mol)

plot_linear_regression_species_richness(species_richness_mol, lin_reg_species_richness_area_mol)

#' #### Tavascan
#+ message=FALSE, warning=FALSE

species_richness_tav <- species_richness_sites[species_richness_sites$area == "Tavascan", ]

lin_reg_species_richness_area_tav <- linear_regression(species_richness_tav,
                                                       "species_richness", "elevational_band_m")
summary(lin_reg_species_richness_area_tav)

plot_linear_regression_species_richness(species_richness_tav, lin_reg_species_richness_area_tav)

#' ### Plot linear regression

#' TODO: I'll put the above plots on one set of axes.

#'
#' ## Compare the models so far
#'
#' So far we have used a linear model to investigate the relationship between species richness and
#' elevation. We have incorportated both study area and sampling effort as random factors, included them individually, and also run the model without any random factors.
#'
#' Display the model output for each of these models to compare the results.

#' ### Linear regression of species richness against elevation. No random factors.
summary(linear_regression_species_richness)

#' ### Linear regression of species richness against elevation. Sampling effort as a random factor.
summary(lmm_species_richness_elev_sampling_effort)

#' ### Linear regression of species richness against elevation. Study area as a random factor.
lmm_species_richness_elev_area <- lmer(species_richness ~ elevational_band_m + (1|area),
                                       data = species_richness_sites)
summary(lmm_species_richness_elev_area)

#' ### Linear regression of species richness against elevation. Study area and sampling effort as a random factor.
summary(lmm_species_richness_elev_area_sampling_effort)

#'
#' ## Investigate effects of elevation on Caelifera species richness
#' Only five species of Ensifera were detected during the surveys, so species richness relationships will
#' be explored further just for the Caelifera.

caelifera_observations <- get_caelifera_only(confirmed_observations)

caelifera_species_richness_sites <- calculate_species_richness_sites(observations_sites_df,
                                                                     caelifera_observations)
caelifera_species_richness_sites$area <- as.factor(caelifera_species_richness_sites$area) # make sure
# that area is considered as a factor

plot_elevation_species_richness_area(caelifera_species_richness_sites)

#'  Run linear regression for each site separately. Ignore the two small sites which do not have an
#' elevational gradient.
#'
#' #### Tor
#+ message=FALSE, warning=FALSE

caelifera_species_richness_tor <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "Tor", ]

lin_reg_caelifera_species_richness_area_tor <- linear_regression(caelifera_species_richness_tor,
                                                       "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_tor)

plot_linear_regression_species_richness(caelifera_species_richness_tor, lin_reg_caelifera_species_richness_area_tor)

#' #### La Molinassa
#+ message=FALSE, warning=FALSE

caelifera_species_richness_mol <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "La Molinassa", ]

lin_reg_caelifera_species_richness_area_mol <- linear_regression(caelifera_species_richness_mol,
                                                                 "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_mol)

plot_linear_regression_species_richness(caelifera_species_richness_mol, lin_reg_caelifera_species_richness_area_mol)

#' #### Tavascan
#+ message=FALSE, warning=FALSE

caelifera_species_richness_tav <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "Tavascan", ]

lin_reg_caelifera_species_richness_area_tav <- linear_regression(caelifera_species_richness_tav,
                                                       "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_tav)

plot_linear_regression_species_richness(caelifera_species_richness_tav, lin_reg_caelifera_species_richness_area_tav)

#'
#' ## Results
#' **NOTE: As of 2022-07-04**, the results described below have not yet been updated to account for the changes to the code and data above.
#' The numbers do not reflect the output from R and any changes in trends have not yet been changed.
#'
#' <br>A simple linear regression was used to investigate the relationship between elevation and species richness.
#' Overall species richness and elevation were negatively correlated (Pearson's correlation coefficient
#' = -0.56). Species richness was shown to decrease by 3.8 for an increase in elevation of 1000 m (<em>t</em> = -3.43; <em>p</em> = 0.002).
#' However, only 31% of the variation in species richness can be explained by elevation (<em>F</em> = 11.74; <em>df</em> = 1, 27; <em>p</em> = 0.002).
#'
#' Looking at the plot of the species richness coloured by study area, there seems to be a strong decrease
#' in species richness with elevation at both La Molinassa and Tavascan, but no change at Tor. A linear
#' mixed model fitting elevation as a fixed effect and study area as a random effect, was used to
#' investigate if there was any effect of study area on species richness. [TODO: add interpretation of
#' this]. Given the effect of area on species richness, simple linear regressions were used to model the
#' species richness with elevation within each study area separately. Whilst there was no relationship at Tor (<em>F</em> = 0.067; <em>df</em> = 1, 8; <em>R</em> = 0.09; <em>p</em> = 0.80),
#' a clear decrease in species richness with elevation can be seen at La Molinassa (<em>F</em> = 11.1; <em>df</em> = 1, 6; <em>R</em> = 0.81; <em>p</em> = 0.02)
#' and Tavascan (<em>F</em> = 304.5; <em>df</em> = 1, 5; <em>R</em> = 0.99; <em>p</em> < 0.01).
#'
#' ## Questions
#' <ol>
#'  <li>I have included the weightings for sampling effort in the linear model. Now that I have done this, do I need to change anything regarding the models?</li>
#'  <li>There is quite a big difference in the results when using the weightings, e.g. La Molinassa (https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_species_richness_hypothesis1.html#la-molinassa).</li>
#'  <li>Simon: When reporting the decrease in species richness with elevation and variation explained (first paragraph of results above), should both t and F be reported? Should the p-value only be reported once?</li>
#'  <li>Simon: I'm not sure how to interpret the output of the linear mixed model with elevation as a fixed effect and study area as a random effect (https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_species_richness_hypothesis1.html#fit-a-linear-mixed-model). How can I use this to say that from this result, we decided to look at the relationship for each study area separately? In the output for area there are no p-values.</li>
#'  <li>In the same output, does it matter that the variance of area is very small (0)?</li>
#'  <li>When doing the linear model for each area separately, should I recalculate the weightings so they are for each area separately?</li>
#'  <li>To calculate <em>R</em> which is included in the last part of the results section above where we report the relationships for the areas separately, I have used the multiple R-squared (rather than adjusted R-squared) output because I understand that we are not adjusting for the number of predictors in the model. Is this correct?</li>
#' </ol>