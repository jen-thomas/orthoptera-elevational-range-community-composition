#' ---
#' title: Hypothesis 1 REVIEW
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
#' Due to logistical reasons and poor weather, the sampling effort varied a lot across the sampling sites.
#' Additionally, both and hand and net sampling were used. In order to account for the difference in
#' sampling effort across the sites, an index of sampling effort was calculated.
#'
#' First, the proportion of all specimens captured by hand ($prop_{hand}$) and by net ($prop_{net}$)
#' was calculated. Secondly, the index for sampling effort was calculated for each site according
#' to the following equation: $$weighting = obs_{hand} * prop_{hand} + obs_{net} * prop_{net}$$ where
#' $obs_{hand}$ and $obs_{net}$ were the number of specimens captured at the site by each sampling
#' method.

# sampling_effort <- calculate_sampling_effort(all_observations_conservative)
#' Calculate sampling effort according to the method during review. All further analysis will use these
#' values.
sampling_effort <- calculate_sampling_effort_review(all_observations_conservative, site_survey_summary)

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
#' Look at the distribution of the species richness including all sites and separately in each study area.

par(mfrow=c(1,1))
hist(species_richness_sites$species_richness)

species_richness_tor <- species_richness_sites[species_richness_sites$area == "Tor",]
species_richness_mol <- species_richness_sites[species_richness_sites$area == "La Molinassa",]
species_richness_tav <- species_richness_sites[species_richness_sites$area == "Tavascan",]

par(mfrow=c(2,2))
hist(species_richness_tor$species_richness)
hist(species_richness_mol$species_richness)
hist(species_richness_tav$species_richness)

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
#' Try different methods below to solve this.

glm_species_richness_full_quasipoisson <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm + mean_density,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson)
Anova(glm_species_richness_full_quasipoisson)
anova(glm_species_richness_full_quasipoisson)

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
#' Try removing veg density which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density)
Anova(glm_species_richness_full_quasipoisson_remove_density)
anova(glm_species_richness_full_quasipoisson_remove_density)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson, glm_species_richness_full_quasipoisson_remove_density,
      test = "F")

#' Try removing slope which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density_slope)
Anova(glm_species_richness_full_quasipoisson_remove_density_slope)
anova(glm_species_richness_full_quasipoisson_remove_density_slope)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density, glm_species_richness_full_quasipoisson_remove_density_slope,
      test = "F")

#' Try removing aspect which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_aspect <- glm(species_richness ~ elevational_band_m +
                                        as.factor(area) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density_slope_aspect)
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_aspect)
anova(glm_species_richness_full_quasipoisson_remove_density_slope_aspect)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope, glm_species_richness_full_quasipoisson_remove_density_slope_aspect,
      test = "F")

#' Try removing area which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_area <- glm(species_richness ~ elevational_band_m +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density_slope_area)
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_area)
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope, glm_species_richness_full_quasipoisson_remove_density_slope_area,
      test = "F")

#' This has made elevation become very significant. Try removing the aspect as well.

#' Try removing aspect which reduces the deviance by a small amount and has a large p value
glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect <- glm(species_richness ~ elevational_band_m +
                                        sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect)
Anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect)
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area, glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect,
      test = "F")

#' Try adding slope back in
glm_species_richness_full_quasipoisson_remove_density_area_aspect <- glm(species_richness ~ elevational_band_m +
                                        slope + sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson_remove_density_area_aspect)
Anova(glm_species_richness_full_quasipoisson_remove_density_area_aspect)
anova(glm_species_richness_full_quasipoisson_remove_density_area_aspect)

#' Compare the reduced model and full model.
anova(glm_species_richness_full_quasipoisson_remove_density_slope_area_aspect, glm_species_richness_full_quasipoisson_remove_density_area_aspect,
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
#' ### Test species richness from three main study areas (excluding Besan and les Bordes de Viros)

#' Calculate the species richness for only the sites in the three main study areas. Leave combined. There
#' are not enough data to split the data and model each site differently.

species_richness_tortavmol <- species_richness_sites[species_richness_sites$area %in%
                                                       c("La Molinassa", "Tor", "Tavascan"), ]

#' GLM of species richness (Tor, Tavascan, La Molinassa) with the set of parameters as used in the full
#' model of overall species richness.

glm_species_richness_full_tortavmol <- glm(species_richness ~ elevational_band_m +
                                        as.factor(area) + slope + as.factor(aspect_cardinal) +
                                        sampling_effort_index + mean_perc_veg_cover + mean_max_height_cm +
                                        mean_density,
    family = quasipoisson(link = "log"),
    data = species_richness_tortavmol)

#' Summarise the GLM.

summary(glm_species_richness_full_tortavmol)

#' Do ANOVA of GLM.

Anova(glm_species_richness_full_tortavmol)
anova(glm_species_richness_full_tortavmol)

#' Manually reduce the model

#' Remove the slope which has a large p value and only leads to a small increase in deviance
glm_species_richness_full_tortavmol_remove_slope <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_tortavmol)

summary(glm_species_richness_full_tortavmol_remove_slope)
Anova(glm_species_richness_full_tortavmol_remove_slope)
anova(glm_species_richness_full_tortavmol_remove_slope)

#' Remove the area which has a large p value and only leads to a small increase in deviance
glm_species_richness_full_tortavmol_remove_slope_area <- glm(species_richness ~ elevational_band_m +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_tortavmol)

summary(glm_species_richness_full_tortavmol_remove_slope_area)
Anova(glm_species_richness_full_tortavmol_remove_slope_area)
anova(glm_species_richness_full_tortavmol_remove_slope_area)

#' Remove the aspect which has a large p value and only leads to a small increase in deviance
glm_species_richness_full_tortavmol_remove_slope_area_aspect <- glm(species_richness ~ elevational_band_m +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_tortavmol)

summary(glm_species_richness_full_tortavmol_remove_slope_area_aspect)
Anova(glm_species_richness_full_tortavmol_remove_slope_area_aspect)
anova(glm_species_richness_full_tortavmol_remove_slope_area_aspect)

#' Try adding slope back in
glm_species_richness_full_tortavmol_remove_area_aspect <- glm(species_richness ~ elevational_band_m +
                                        sampling_effort_index + slope +
                                        mean_perc_veg_cover + mean_max_height_cm,
    family = quasipoisson(link = "log"),
    data = species_richness_tortavmol)

summary(glm_species_richness_full_tortavmol_remove_area_aspect)
Anova(glm_species_richness_full_tortavmol_remove_area_aspect)
anova(glm_species_richness_full_tortavmol_remove_area_aspect)

#' Slope did not improve the model fit, so leave it out. Retain this as the reduced model which is the
#' same as the reduced model for the full set of data. The parameter estimates are also very similar,
#' so this shows that the data from the sites in other study areas are not affecting the model greatly.

glm_species_richness_reduced_tortavmol <- glm_species_richness_full_tortavmol_remove_slope_area_aspect

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

#' TODO check if this is valid now
reduced_test <- 1-pchisq(13.92499, 18)
reduced_test

#'
#' ### Species richness main study areas reduced model

par(mfrow = c(1,2))
plot(species_richness_tortavmol$species_richness, fitted(glm_species_richness_reduced_tortavmol),
     xlab = "Observed values", ylab = "Fitted values")
abline(0,1)
plot(fitted(glm_species_richness_reduced_tortavmol), residuals(glm_species_richness_reduced_tortavmol,
                                                               type = "pearson"))
abline(h = 0)

#' TODO check if this is valid now

reduced_test_tortavmol <- 1-pchisq(13.92499, 18)
reduced_test_tortavmol

#'
#' ## Plots for report
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
filepath <- file.path(path, "hypothesis1_sr_elevation_glm_review.png")
print(filepath)
png(file = filepath, width = 1000, height = 1000, units = "px", bg = "white", res = 300)

filepath_pdf <- file.path(path, "figure_3_species_richness_review.pdf")
print(filepath_pdf)
pdf(file = filepath_pdf, width = 7, height = 7)

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
knitr::kable(glm_species_richness_reduced_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P <- 0.05) for variables included in the
reduced GLM for overall species richness (AICc = 145.24)")
knitr::kable(glm_species_richness_reduced_table, format = "simple", caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P <- 0.05) for variables included in the
reduced GLM for overall species richness (AICc = 145.24)")
knitr::kable(glm_species_richness_reduced_table, format = "pipe", caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P <- 0.05) for variables included in the
reduced GLM for overall species richness (AICc = 145.24)")

#' Reduced model including the interaction between vegetation cover and slope.

glm_species_richness_inter_slope_vegcover_table <- xtable(glm_species_richness_inter_slope_vegcover)
glm_species_richness_inter_slope_vegcover_table
print(glm_species_richness_inter_slope_vegcover_table)
knitr::kable(glm_species_richness_inter_slope_vegcover_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P <- 0.05) for variables included in the
reduced GLM for overall species richness including an interaction between vegetation cover and slope
(AICc = 140)")

#' Reduced model for species richness from all main study areas.

glm_species_richness_tortavmol_reduced_table <- xtable(glm_species_richness_tortavmol_reduced)
glm_species_richness_tortavmol_reduced_table
print(glm_species_richness_tortavmol_reduced_table)
knitr::kable(glm_species_richness_tortavmol_reduced_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P < 0.05) for variables included in the reduced
 GLM for species richness from all main study areas (AIC = )")

#' Reduced model for Caelifera species richness.

glm_species_richness_reduced_caelifera_table <- xtable(glm_species_richness_caelifera_reduced)
glm_species_richness_reduced_caelifera_table
print(glm_species_richness_reduced_caelifera_table)
knitr::kable(glm_species_richness_reduced_caelifera_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P < 0.05) for variables included in the reduced
 GLM for Caelifera species richness (AIC = 132.0)")

#' Reduced model for Caelifera species richness without elevation.

glm_species_richness_caelifera_reduced_no_elevation_table <-
  xtable(glm_species_richness_caelifera_reduced_no_elevation)
glm_species_richness_caelifera_reduced_no_elevation_table
print(glm_species_richness_caelifera_reduced_no_elevation_table)
knitr::kable(glm_species_richness_caelifera_reduced_no_elevation_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P < 0.05) for variables included in the reduced
 GLM for Caelifera species richness after removing elevation (AIC = 132.0)")

#' ## Appendix
#'
#' ### GLM model reduction
#'
#' The sections below show the different methods of stepwise selection that were used. R's automatic
#' backwards selection is in the main section of code above.
#'
#' #### Manual backwards selection
#'
#' Attempt manual stepwise selection, removing the parameter with the highest p-value each time.
#'
#' Drop aspect first.

glm_species_richness_step1 <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height_cm + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

#' Model summary.

summary(glm_species_richness_step1)

#' ANOVA.

Anova(glm_species_richness_step1)

#' AICC.

AICcmodavg::AICc(glm_species_richness_step1, return.K = FALSE, second.ord = TRUE)

#' Drop max vegetation height next.

glm_species_richness_step2 <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        sampling_effort_index + mean_perc_veg_cover + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

#' Model summary.

summary(glm_species_richness_step2)

#' ANOVA.

Anova(glm_species_richness_step2)

#' AICC.

AICcmodavg::AICc(glm_species_richness_step2, return.K = FALSE, second.ord = TRUE)

#'
#' #### Test R's built in stepwise selection from both directions

glm_species_richness_step <- stats::step(glm_species_richness_full, direction = "both")

#' Show the summary of the reduced model as found by R's stepwise selection.

summary(glm_species_richness_step)

#' Generate an ANOVA table for the model.

Anova(glm_species_richness_step)

#' AICC.

AICcmodavg::AICc(glm_species_richness_step, return.K = FALSE, second.ord = TRUE)

#' #### Test forward stepwise selection using R's built in stepwise selection

glm_species_richness_step_forward <- stats::step(glm_species_richness_full, direction = "forward")

#' Show the summary of the reduced model as found by R's stepwise selection.

summary(glm_species_richness_step_forward)

#' Generate an ANOVA table for the model.

Anova(glm_species_richness_step_forward)

#' AICC.

AICcmodavg::AICc(glm_species_richness_step_forward, return.K = FALSE, second.ord = TRUE)

#' #### Dredge for best model fit
#'
#' Using the MuMIn package, we can dredge for the best model. Begin with the full model.

options(na.action = "na.fail")
glm_species_richness_full_dredge <- dredge(glm_species_richness_full, rank = "AICc")

#' View the model summary and AICC.

best_glm_species_richness_full_dredge <- get.models(glm_species_richness_full_dredge, 1)[[1]]
glm_species_best_dredge <- glm(best_glm_species_richness_full_dredge,
                               family = poisson(link = "log"),
                               data = species_richness_sites)

#' Model summary.

summary(glm_species_best_dredge)

#' AICC.

AICcmodavg::AICc(glm_species_best_dredge, return.K = FALSE, second.ord = TRUE)