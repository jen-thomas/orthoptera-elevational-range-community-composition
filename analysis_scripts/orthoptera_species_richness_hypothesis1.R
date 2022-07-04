#' ---
#' title: Hypothesis 1
#' subtitle: Species richness decreases with elevation.
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' **TODO (as of 2022-07-04):**
#' <ul>
#'  <li>incorporate sampling effort into models</li>
#'  <li>incorporate "finalised" observations</li>
#'  <li>check calculation of species richness for BES01 - 23 seems extremely high</li>
#' </ul>
#' <br>Import packages functions from other files.
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
    unique_taxa_site$species_richness[unique_taxa_site$site_elevation == site] = nrow(taxa_site)
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
        geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
        geom_line(aes(y = upr), color = "black", linetype = "dashed") +
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

#' ### Calculate species richness
#'
#' Calculate species richness for each site. TODO: add the taxa from the finalised observations.

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
#' elevation (<em>r</em> = -0.56, <em>t<sub>26</sub></em> = -3.43, <em>p</em> = 0.002), however only
#' 31% of the variation of species richness is explained by the elevation.
#'
#' ### Plot species richness against elevation.

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
#' A linear mixed model will be fitted, treating study area as a random factor.
#+ message=FALSE, warning=FALSE

lmm_species_richness_elev_area <- lmer(species_richness ~ elevational_band_m + (1|area),
                                       data = species_richness_sites)
summary(lmm_species_richness_elev_area)

#' The value of variance for area (random effects section) is 0 (or presumably just very small if only
#' 3dp are used). I understand this can occur because of sampling error. TODO: check if this is important.
#'
#' I realise that I'm not sure how to interpret this output. The parameter estimates are the same as
#' those in the model above. How does this output tell us that area is important?
#'
#' ### Plot linear mixed model

plot(lmm_species_richness_elev_area)

#'
#' ### Test the assumptions of the linear mixed model

qqnorm(resid(lmm_species_richness_elev_area))
qqline(resid(lmm_species_richness_elev_area))

#' The plot of the residuals doesn't show an obvious pattern. It might be possible to discern a slight
#' decrease overall. The residuals seem to have a normal distribution, however there is one obvious
#' outlier in both plots.

#' ## Species richness and elevation within study areas
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
#' ## Questions:
#' <ol>
#'  <li>When reporting the decrease in species richness with elevation and variation explained (first paragraph of results above), should both t and F be reported? Should the p-value only be reported once?</li>
#'  <li>I'm not sure how to interpret the output of the linear mixed model with elevation as a fixed effect and study area as a random effect (https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_species_richness_hypothesis1.html#fit-a-linear-mixed-model). How can I use this to say that from this result, we decided to look at the relationship for each study area separately? In the output for area there are no p-values.</li>
#'  <li>To calculate <em>R</em> which is included in the last part of the results section above where we report the relationships for the areas separately, I have used the multiple R-squared (rather than adjusted R-squared) output because I understand that we are not adjusting for the number of predictors in the model. Is this correct?</li>
#' </ol>