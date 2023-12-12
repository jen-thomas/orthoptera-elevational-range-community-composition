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

#' <br>Import packages and functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")
source("get_finalised_observations_species_richness_conservative.R")

vector_packages <- c("visreg", "ggplot2", "lmerTest", "dplyr", "car", "lme4", "AICcmodavg", "MuMIn",
                     "stats", "knitr", "tab", "xtable")
get_packages(vector_packages)

#' ## Investigate effects of elevation on species richness
#+ message=FALSE, warning=FALSE

#' The following functions calculate the species richness and do the statistics for the analysis of this
#' hypothesis.

plot_elevation_species_richness <- function(species_richness_elevation) {
    #' Plot elevation band against species richness.

  plot(species_richness ~ elevational_band_m, data = species_richness_elevation,
       xlab = "Elevation band (m a.s.l)", ylab = "Species richness")
}

correlation_test <- function(dataframe, para1, para2) {
    #' Calculate the correlation coefficient between two variables.
    #'
    #' Return the coefficient.

  corr_test <- cor.test(x = dataframe[[para1]], y = dataframe[[para2]], method = "spearman")

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
    #' Get the predicted values using a linear regression.
    #'
    #' Return data frame of predicted values.

  predicted <- predict(linear_regression, interval = "prediction")

  return(predicted)
}

#' ## Import and set up the data

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
vegetation_file <- "../data/vegetation_plots.csv"

sites_df <- read_csv_data_file(sites_file)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

#' ### Get the finalised identifications
#'
#' #### Explanation of confirmed and finalised identifications (copied from data exploration section)
#' All specimens were identified to the lowest taxonomic level possible. Many small nymphs could not be
#' identified to species level. Furthermore, some adults could only be identified to genus or a higher
#' taxonomic level due to hard-to-distinguish features or contradictory features. All specimens identified
#' to one particular taxa had a "confirmed" identification.
#'
#' In some cases, specimens could only be identified to one of a set of multiple taxa, for
#' instance where contradictory features in the keys meant that they could be identified to one of two
#' particular taxa. An identification was "finalised" when a specimen could be identified to a number of
#' taxa.
#'
#' In parts of the analysis, the observations will referred to as confirmed or finalised, and be subset
#' according to these different types of identification.

all_observations_conservative <- get_conservative_observations(confirmed_observations,
                                                               finalised_observations)
all_observations_notconservative <- get_nonconservative_observations(confirmed_observations,
                                                                     finalised_observations)

#' Calculate species richness at each site

species_richness_sites <- calculate_species_richness_sites(all_observations_conservative)

species_richness_sites_notconservative <-
  calculate_species_richness_sites(all_observations_notconservative)

display_species_richness <- left_join(species_richness_sites, species_richness_sites_notconservative,
                                      by = c("site_elevation", "area", "elevational_band_m"),
                                      suffix = c("_conservative", "_notconservative"))
both_species_richness <- display_species_richness %>%
  dplyr::select(site_elevation, area, species_richness_conservative, species_richness_notconservative)
both_species_richness

#'
#' ### Get environmental data

#' Get vegetation data.

vegetation_averaged_df <- prepare_veg_data(sites_file, vegetation_file)

#' Calculate the slope and aspect along each transect.
#+ message=FALSE, warning=FALSE

site_topography <- get_site_topography(sites_df)

#' Put vegetation and terrain data into one dataframe.
#+ message=FALSE, warning=FALSE

site_env_var_data <- left_join(site_topography, vegetation_averaged_df, by = "site_elevation")

#' Join environmental data onto species richness dataframe.

species_richness_sites <- left_join(species_richness_sites, site_env_var_data,
                                    by = c("site_elevation", "area", "elevational_band_m"))

#' ### Account for the effects of sampling effort
#'
#' Due to poor weather, the sampling effort varied across the sampling sites.
#' Additionally, both and hand and net sampling were used. In order to account for the difference in
#' sampling effort across the sites, an index of sampling effort was calculated.
#'
#' We calculated the number of surveys undertaken by net ($surveys_{NET}$), the mean number of
#' specimens caught in a net transect sample ($mean_obs_{NET}$), and the total number of specimens caught
#' in hand transect samples at each site ($total_obs_{HAND}).
#'
#' We calculated SE for each site using the equation,
#' $SE = surveys_{NET} + total_obs_{HAND} / mean_obs_{NET}$
#'

#' Calculate sampling effort. All further analysis will use these values.

sampling_effort <- calculate_sampling_effort(all_observations_conservative, site_survey_summary)
min(sampling_effort$sampling_effort_index)
max(sampling_effort$sampling_effort_index)
species_richness_sites <- left_join(species_richness_sites, sampling_effort, by = "site_elevation")

#' It is likely that the number of species recorded will depend on the number of specimens captured, and
#' given this varied across sites, it should be accounted for.
#'
#' <br>Look at the relationship between species richness and sampling effort
#+ message=FALSE, warning=FALSE

corr_test_samplingeffort_speciesrichness <- correlation_test(species_richness_sites, "sampling_effort_index",
                                              "species_richness")
corr_test_samplingeffort_speciesrichness

#' Print rho
#+ message=FALSE, warning=FALSE

corr_test_samplingeffort_speciesrichness_rho <- corr_test_samplingeffort_speciesrichness$estimate
print(corr_test_samplingeffort_speciesrichness_rho)

#' <br>and calculate the coefficient of determination (R<sup>2</sup>).

coeff_det_samplingeffort_speciesrichnessn <- calculate_coefficient_of_determination(corr_test_samplingeffort_speciesrichness_rho)
print(coeff_det_samplingeffort_speciesrichnessn)

#' Calculate the correlation between elevation and sampling effort.
#+ message=FALSE, warning=FALSE

plot(sampling_effort_index ~ elevational_band_m, data = species_richness_sites,
       xlab = "Elevation band (m a.s.l)", ylab = "SE")

corr_test_samplingeffort_elevation <- correlation_test(species_richness_sites, "elevational_band_m",
                                              "sampling_effort_index")
corr_test_samplingeffort_elevation

#' Print rho
#+ message=FALSE, warning=FALSE

corr_test_samplingeffort_elevation_rho <- corr_test_samplingeffort_elevation$estimate
print(corr_test_samplingeffort_elevation_rho)

#' <br>and calculate the coefficient of determination (R<sup>2</sup>).
coeff_det_samplingeffort_elevation <- calculate_coefficient_of_determination(corr_test_samplingeffort_elevation_rho)
print(coeff_det_samplingeffort_elevation)

#' Sampling effort will be incorporated into the generalised linear mixed models to see if it affected the
#' species richness.
#'
#' ## Correlate species richness and elevation
#' Do a correlation test between the species richness at each site and elevation using Spearman's rank
#' correlation.
#' <br>H<sub>0</sub>: the correlation coefficient is equal to 0.
#' <br>H<sub>1</sub>: the correlation coefficient is different from 0.
#'
#' View the test
#+ message=FALSE, warning=FALSE

corr_test <- correlation_test(species_richness_sites, "elevational_band_m",
                              "species_richness")
corr_coeff <- corr_test$estimate
print(corr_test)

#' <br>and calculate the coefficient of determination (R<sup>2</sup>).
coeff_det <- calculate_coefficient_of_determination(corr_coeff)
print(coeff_det)

#'
#' ### Plot species richness against elevation

plot_elevation_species_richness(species_richness_sites)

#'
#' ## Generalised linear model
#'
#' Look at the distribution of the species richness across all sites.

par(mfrow=c(1,1))
hist(species_richness_sites$species_richness)

#' Fit a GLM to the data using species richness as the response variable and the environmental variables
#' as predictor variables. It looks as though a Poisson distribution might be the best distrbution to use
#' for species richness overall.
#'
#' ### Fit full model

glm_species_richness_full <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full)

#' Do ANOVA.
Anova(glm_species_richness_full)

#' Calculate AICC.
AICcmodavg::AICc(glm_species_richness_full, return.K = FALSE, second.ord = TRUE)

plot(glm_species_richness_full)

#'
#' ### Test for overdispersion
#'
#' If the ratio of the residual deviance to the residual degrees of freedom exceeds 1.5, then the model is
#' overdispersed.

ratio_dispersion <- summary(glm_species_richness_full)$deviance /
                    summary(glm_species_richness_full)$df.residual
paste0("ratio: ", ratio_dispersion)

#' Fit a quasipoisson distribution to double-check that there is no problem with overdispersion. A poisson
#' distribution assumes that the overdispersion is 1, so if the overdispersion from the quasipoisson is
#' greater than 1, then we have this problem with the Poisson distribution. This model is overdispersed.

glm_species_richness_full_quasipoisson <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm + mean_density,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson")

summary(glm_species_richness_full_quasipoisson, test = "F")
Anova(glm_species_richness_full_quasipoisson, test = "F")
anova(glm_species_richness_full_quasipoisson, test = "F")

#' Given the overdispersion parameter lies between 1 and 15, quasipoisson distribution should be suitable
#' for this model (there is no need to try a negative binomial instead).
#'
#' ### Model selection
#'
#' Stepwise selection will be done on the model to find the best reduced model.
#'
#' Two methods will be used:
#' - manual selection
#' - R's drop1 function
#' Both of these models are trying to minimise the deviance (for the quasipoisson model, rather than
#' minimising the AIC which is not valid for use with a quasipoisson distribution).
#'
#' The parameters will be considered according to their meaning within the model and for the hypotheses,
#' their p-value and the improvement they make to the deviance (i.e. those that decrease it by the
#' largest amount will be retained).

#' #### Manual selection new model with new data
#'
#' Try removing veg density which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density <- glm(species_richness ~ elevational_band_m +
                                        as.factor(area) + slope + as.factor(aspect_cardinal) +
                                        sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density")
summary(glm_species_richness_full_quasipoisson_remove_density, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson, glm_species_richness_full_quasipoisson_remove_density,
      test = "F")

#' Try removing slope which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope <- glm(species_richness ~ elevational_band_m +
                                        as.factor(area) + as.factor(aspect_cardinal) +
                                        sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density_slope")
summary(glm_species_richness_full_quasipoisson_remove_density_slope, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density_slope, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density_slope, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density,
      glm_species_richness_full_quasipoisson_remove_density_slope,
      test = "F")

#' Try removing aspect which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_aspect <- glm(species_richness ~
                                        elevational_band_m +
                                        as.factor(area) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density_slope_aspect")
summary(glm_species_richness_full_quasipoisson_remove_density_slope_aspect, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_aspect, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density_slope_aspect, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope,
      glm_species_richness_full_quasipoisson_remove_density_slope_aspect,
      test = "F")

#' Try removing area instead of aspect which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_area <- glm(species_richness ~
                                        elevational_band_m +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density_slope_area")
summary(glm_species_richness_full_quasipoisson_remove_density_slope_area, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_area, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope,
      glm_species_richness_full_quasipoisson_remove_density_slope_area,
      test = "F")

#' This has made elevation become very significant. Try removing the aspect as well.

#' Try removing aspect which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect <- glm(species_richness ~
                                        elevational_band_m +
                                        sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect")
summary(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area,
      glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect,
      test = "F")

#' Try adding slope back in
glm_species_richness_full_quasipoisson_remove_density_area_aspect <- glm(species_richness ~
                                        elevational_band_m + slope + sampling_effort_index +
                                          mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)
print("glm_species_richness_full_quasipoisson_remove_density_area_aspect")
summary(glm_species_richness_full_quasipoisson_remove_density_area_aspect, test = "F")
Anova(glm_species_richness_full_quasipoisson_remove_density_area_aspect, test = "F")
anova(glm_species_richness_full_quasipoisson_remove_density_area_aspect, test = "F")

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect,
      glm_species_richness_full_quasipoisson_remove_density_area_aspect,
      test = "F")

#' Adding slope did not improve the model so leave it out of the reduced model

#' #### Model reduction with drop1 for the new data
#'
drop1(glm_species_richness_full_quasipoisson)

#' Output suggests removing slope would lead to the smallest increase in deviance.
full_qp_remove_slope <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm + mean_density,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

Anova(full_qp_remove_slope)

drop1(full_qp_remove_slope)

#' Output suggests removing density would lead to the smallest increase in deviance.
full_qp_remove_slope_density <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

Anova(full_qp_remove_slope_density)

drop1(full_qp_remove_slope_density)

#' There is now very little difference in the differences between deviance, so given this, remove area
#' or aspect as these have 4 df.
full_qp_remove_slope_density_aspect <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

Anova(full_qp_remove_slope_density_aspect)

drop1(full_qp_remove_slope_density_aspect)

#' There is still very little difference. Remove area to see if this makes any difference.
full_qp_remove_slope_density_aspect_area <- glm(species_richness ~ elevational_band_m +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

Anova(full_qp_remove_slope_density_aspect_area)

drop1(full_qp_remove_slope_density_aspect_area)

#' Try adding slope back into the model.
full_qp_remove_density_aspect_area <- glm(species_richness ~ elevational_band_m + slope +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

Anova(full_qp_remove_density_aspect_area)

drop1(full_qp_remove_density_aspect_area)

# Slope does not improve the model so leave it out.

#' #### Define reduced model

glm_species_richness_reduced <- full_qp_remove_slope_density_aspect_area

#' Summary of model.

summary(glm_species_richness_reduced)

#' ANOVA.

car::Anova(glm_species_richness_reduced, type="II", test.statistic = "LR", error.estimate = "deviance")

#'
#' ### Plot the modelled variables

par(mfrow=c(2, 2))
visreg(glm_species_richness_reduced, xvar = "elevational_band_m")
visreg(glm_species_richness_reduced, xvar = "sampling_effort_index")
visreg(glm_species_richness_reduced, xvar = "mean_perc_veg_cover")
visreg(glm_species_richness_reduced, xvar = "mean_max_height_cm")

#'
#' ## GLM model assessment
#'
#' Plot the observed against fitted values for the full model and each of the final reduced models. Also
#' plot the residuals.
#'
#' Also test if the model is suitable. H0: model is correct. H1: model is not correct. To do this,
#' calculate the p-value for the model where the deviance and degrees of freedom are used.
#'
#' ### Overall reduced model
#'
#' Test the reduced model which was the outcome of the manual stepwise selection and drop1.

par(mfrow = c(1,2))
plot(species_richness_sites$species_richness, fitted(glm_species_richness_reduced),
     xlab = "Observed values", ylab = "Fitted values")
abline(0,1)
plot(fitted(glm_species_richness_reduced), residuals(glm_species_richness_reduced, type = "pearson"))
abline(h = 0)

reduced_test <- 1-pchisq(30.321, 23) # from residual deviance in the model output
reduced_test

#'
#' ## Plot for report
#'
#' Plot the predicted values on top of the actual data points.
species_richness_elevation_plot <- visreg(glm_species_richness_reduced, xvar = "elevational_band_m",
                                     scale = "response",
                                     rug = FALSE,
                                     line.par = list(col = "black", lwd = 0.5),
                                     xlab = "Elevation (m a.s.l)", xlim = c(1000, 2550),
                                     ylab = "Species richness", ylim = c(0, 17))

#' Get the fitted values from the visreg object to plot in the next plot.

fitted_glm_values <- data.frame(species_richness_elevation_plot$fit)

#' Create and save the output plot.

path <- "../analysis_plots/"
filepath <- file.path(path, "hypothesis1_sr_elevation_glm.tiff")
tiff(file = filepath, width = 1000, height = 1000, units = "px", bg = "white", res = 300)

par(mfrow = c(1,1))
species_richness_elevation_plot2 <- visreg(glm_species_richness_reduced, xvar = "elevational_band_m",
                                     scale = "response",
                                     rug = FALSE,
                                     band = FALSE,
                                     line.par = list(col = "black", lwd = 0.5),
                                     xlab = "Elevation (m a.s.l)",
                                     ylab = "Species richness",
                                     gg = TRUE) +
  geom_point(shape = 16, colour = "black", size = 2, data = species_richness_sites,
             aes(x = elevational_band_m, y = species_richness), show.legend = FALSE) +
  geom_line(data = fitted_glm_values, aes(x = elevational_band_m,
            y = visregUpr),
            linetype = "dashed", col = "darkgrey", lwd = 0.5) +
  geom_line(data = fitted_glm_values, aes(x = elevational_band_m,
            y = visregLwr),
            linetype = "dashed", col = "darkgrey", lwd = 0.5) +
  ylim(min(species_richness_sites$species_richness) - 1,
       max(species_richness_sites$species_richness) + 1) +
  xlim(min(species_richness_sites$elevational_band_m) - 100,
       max(species_richness_sites$elevational_band_m) + 100) +
  theme_classic()
species_richness_elevation_plot2

dev.off()

#' ## Output tables for report
#'
#' Reduced model for overall species richness.

glm_species_richness_reduced_table <- xtable(glm_species_richness_reduced)
glm_species_richness_reduced_table
print(glm_species_richness_reduced_table)
knitr::kable(glm_species_richness_reduced_table)
knitr::kable(glm_species_richness_reduced_table, format = "simple")
knitr::kable(glm_species_richness_reduced_table, format = "pipe")