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

linear_regression_species_richness_elevation <- function(dataframe, species_richness,
                                                         elevational_band_m) {
    #' Create a linear model (do a linear regression) of the explanatory variable (elevation) on the
    #' response variable (species richness).
    #'
    #' Return the model.

  regression <- lm(dataframe[[species_richness]] ~ dataframe[[elevational_band_m]])

  return(regression)
}

plot_model_residuals_species_richness_elev <- function(model, dataframe, elevational_band_m) {
    #' Calculate and plot the residuals from the regression as a function of the explanatory variable
    #' (elevational band).

  plot(residuals(model) ~ dataframe[[elevational_band_m]])
  abline(h = 0)
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

#' Eight sites had a higher species richness when using the conservative identifications. The analysis
#' will only use conservative identifications until the regression is compared to see if there is any
#' statistical difference in the relationship between species richness and elevation.

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

sampling_effort <- calculate_sampling_weights(all_observations_conservative)

species_richness_sites <- left_join(species_richness_sites, sampling_effort, by = "site_elevation")

#' It is likely that the number of species recorded will depend on the number of specimens captured, and
#' given this varied across sites, it should be accounted for.
#'
#' <br>Look at the relationship between species richness and sampling effort
#+ message=FALSE, warning=FALSE

corr_test_samplingeffort_speciesrichness <- correlation_test(species_richness_sites, "sampling_effort_index",
                                              "species_richness")
corr_test_samplingeffort_speciesrichness
corr_test_samplingeffort_speciesrichness_rho <- corr_test_samplingeffort_speciesrichness$estimate
print(corr_test_samplingeffort_speciesrichness)

#' and elevation and sampling effort.
#+ message=FALSE, warning=FALSE

corr_test_samplingeffort_elevation <- correlation_test(species_richness_sites, "elevational_band_m",
                                              "sampling_effort_index")
corr_test_samplingeffort_elevation
corr_test_samplingeffort_elevation_rho <- corr_test_samplingeffort_elevation$estimate
print(corr_test_samplingeffort_elevation)

#' As expected, species richness significantly increases with sampling effort (<em>r<sub>S</sub></em> =
#' 0.77, <em>p</em> < 0.001). Sampling effort was lower at higher elevations (<em>r<sub>S</sub></em> =
#' -0.56, <em>p</em> = 0.002).

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
corr_test <- correlation_test(species_richness_sites, "elevational_band_m",
                              "species_richness")
corr_coeff <- corr_test$estimate
print(corr_test)

#' <br>and calculate the coefficient of determination (R<sup>2</sup>).
coeff_det <- calculate_coefficient_of_determination(corr_coeff)
print(coeff_det)

#' <br>The relationship between species richness and elevation was tested using a Spearman's rank
#' correlation. There was evidence to suggest a significant negative relationship between species richness
#' and elevation (<em>r</em> = -0.66, <em>P</em> = 0.0001), however only
#' 44% of the variation in species richness is explained by the elevation.
#'
#' ### Plot species richness against elevation

plot_elevation_species_richness(species_richness_sites)

#' The plot shows a general decreasing trend of species richness with elevation, which was confirmed by
#' the correlation coefficient above.
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

#' Species richness at all sites has a Poisson distribution.
#' Species richness in each of the three main study areas do not conform to a Gaussian distribution. Fit
#' a GLM to the data using species richness as the response variable and the environmental variables as
#' predictor variables. It looks as though a Poisson distribution might be the best distrbution to use for
#' species richness overall.
#'
#' ### Fit full model

glm_species_richness_full <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full)
Anova(glm_species_richness_full)
logLik(glm_species_richness_full)
AICcmodavg::AICc(glm_species_richness_full, return.K = FALSE, second.ord = TRUE)

#' Elevation was almost significant (P = 0.053). Study area, slope, sampling effort and percentage
#' vegetation cover were all significant (P < 0.05). Predictive power of the full model AIC = 125.
#'
#' ### Test for overdispersion
#'
#' If the ratio of the residual deviance to the residual degrees of freedom exceeds 1.5, then the model is
#' overdispersed.

ratio_dispersion <- summary(glm_species_richness_full)$deviance /
                    summary(glm_species_richness_full)$df.residual
paste0("ratio: ", ratio_dispersion)

#' Given that the ratio < 1.5, then there is no overdispersion.

#' Fit a quasipoisson distribution to double-check that there is no problem with overdispersion. A poisson
#' distribution assumes that the overdispersion is 1, so if the overdispersion from the quasipoisson is
#' greater than 1, then we have this problem with the Poisson distribution. (This part is not needed as
#' we have already shown that there is no overdispersion).

glm_species_richness_full_quasipoisson <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoisson)
Anova(glm_species_richness_full_quasipoisson)

#' ### Model selection
#'
#' #### R's backwards stepwise selection
#'
#' Attempt stepwise selection from both directions to reduce the number of parameters in the model.

glm_species_richness_step <- stats::step(glm_species_richness_full, direction = "both")

#' Show the summary of the reduced model as found by R's stepwise selection.

summary(glm_species_richness_step)

#' Generate an ANOVA table for the model.

Anova(glm_species_richness_step)
logLik(glm_species_richness_step)
AICcmodavg::AICc(glm_species_richness_step, return.K = FALSE, second.ord = TRUE)

#' Test forward stepwise selection

glm_species_richness_step_forward <- stats::step(glm_species_richness_full, direction = "forward")

#' Show the summary of the reduced model as found by R's stepwise selection.

summary(glm_species_richness_step_forward)

#' Generate an ANOVA table for the model.

Anova(glm_species_richness_step_forward)
logLik(glm_species_richness_step_forward)
AICcmodavg::AICc(glm_species_richness_step_forward, return.K = FALSE, second.ord = TRUE)

#' Test backward stepwise selection
#'
glm_species_richness_step_backward <- stats::step(glm_species_richness_full, direction = "backward")

#' Show the summary of the reduced model as found by R's stepwise selection.

summary(glm_species_richness_step_backward)

#' Generate an ANOVA table for the model.

Anova(glm_species_richness_step_backward)
logLik(glm_species_richness_step_backward)
AICcmodavg::AICc(glm_species_richness_step_backward, return.K = FALSE, second.ord = TRUE)

#' #### Manual backwards selection
#'
#' Attempt manual stepwise selection, removing the parameter with the highest p-value each time.
#'
#' Drop aspect
glm_species_richness_step1 <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_step1)
Anova(glm_species_richness_step1)
AICcmodavg::AICc(glm_species_richness_step1, return.K = FALSE, second.ord = TRUE)

#' Drop max vegetation height

glm_species_richness_step2 <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        sampling_effort_index + mean_perc_veg_cover + mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_step2)
Anova(glm_species_richness_step2)
AICcmodavg::AICc(glm_species_richness_step2, return.K = FALSE, second.ord = TRUE)

#' All parameters are now significant. Let this be the reduced model. AICC = 145.2388. Vegetation density
#' was borderline significant (P = 0.0506). Removing this parameter from the model was tested, but AICC =
#' 144, so it didn't make any great improvement to the model. Therefore it was retained.

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
summary(glm_species_best_dredge)
AICcmodavg::AICc(glm_species_best_dredge, return.K = FALSE, second.ord = TRUE)

#' The dredge results in a simpler model than using backwards selection. AICC is reduced to 136.10 (from
#' 145.24) but given that the other parameters are significant in the model and they are biologically
#' important, the reduced model obtained through backwards selection is retained.
#'
#' #### Define reduced model

glm_species_richness_reduced <- glm_species_richness_step2

#' ### Test for overdispersion on reduced model

ratio_dispersion_reduced <- summary(glm_species_richness_reduced)$deviance /
                    summary(glm_species_richness_reduced)$df.residual
paste0("ratio: ", ratio_dispersion_reduced)

#' Given that this ratio < 1.5, we can say that the model does not suffer from overdispersion.
#'
#' ### Test interaction slope and vegetation cover
#'
#' Test an interaction between slope and vegetation cover rather than as an addition to the reduced model,
#' given that they are significantly correlated.

glm_species_richness_inter_slope_vegcover <- glm(species_richness ~ elevational_band_m + as.factor(area) +
                                                 slope + sampling_effort_index*mean_perc_veg_cover +
                                                 mean_density,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_inter_slope_vegcover)
Anova(glm_species_richness_inter_slope_vegcover)
logLik(glm_species_richness_inter_slope_vegcover)
AICcmodavg::AICc(glm_species_richness_inter_slope_vegcover, return.K = FALSE, second.ord = TRUE)

#' Including the interaction of slope and vegetation cover did not increase the predictive power of the
#' model (AICC = 140) and neither the individual parameters nor the interaction were significant
#' predictors.
#'
#' ### Test species richness in study areas separately
#'
#' Study area is an important predictor of species richness. Given the differences in land use in the
#' three different study areas, model the species richness for each area separately. As study area is now
#' being separated, this will not be used as a predictive factor in the model.
#'
#' #### Tor

glm_species_richness_full_tor <- glm(species_richness ~ elevational_band_m + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = species_richness_tor)

summary(glm_species_richness_full_tor)
Anova(glm_species_richness_full_tor)
logLik(glm_species_richness_full_tor)
AICcmodavg::AICc(glm_species_richness_full_tor, return.K = FALSE, second.ord = TRUE)

#' Test stepwise selection for model at Tor
glm_species_richness_full_tor_step <- stats::step(glm_species_richness_full_tor, direction = "backward")

summary(glm_species_richness_full_tor_step)
Anova(glm_species_richness_full_tor_step)
logLik(glm_species_richness_full_tor_step)
AICcmodavg::AICc(glm_species_richness_full_tor_step, return.K = FALSE, second.ord = TRUE)

#' #### Tavascan

glm_species_richness_full_tav <- glm(species_richness ~ elevational_band_m + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = species_richness_tav)

summary(glm_species_richness_full_tav)
Anova(glm_species_richness_full_tav)
logLik(glm_species_richness_full_tav)
AICcmodavg::AICc(glm_species_richness_full_tav, return.K = FALSE, second.ord = TRUE)

#' Test stepwise selection for model at Tavascan
glm_species_richness_full_tav_step <- stats::step(glm_species_richness_full_tav, direction = "backward")

summary(glm_species_richness_full_tav_step)
Anova(glm_species_richness_full_tav_step)
logLik(glm_species_richness_full_tav_step)
AICcmodavg::AICc(glm_species_richness_full_tav_step, return.K = FALSE, second.ord = TRUE)

#' #### La Molinassa

glm_species_richness_full_mol <- glm(species_richness ~ elevational_band_m + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = species_richness_mol)

summary(glm_species_richness_full_mol)
Anova(glm_species_richness_full_mol)
logLik(glm_species_richness_full_mol)
AICcmodavg::AICc(glm_species_richness_full_mol, return.K = FALSE, second.ord = TRUE)

#' Test stepwise selection for model at La Molinassa
glm_species_richness_full_mol_step <- stats::step(glm_species_richness_full_mol, direction = "backward")

summary(glm_species_richness_full_mol_step)
Anova(glm_species_richness_full_mol_step)
logLik(glm_species_richness_full_mol_step)
AICcmodavg::AICc(glm_species_richness_full_mol_step, return.K = FALSE, second.ord = TRUE)

#' ### Remove outliers of sampling effort
#' **TODO**
#'
#' ### Test species richness of Caelifera in GLM

#' Calculate the species richness for only Caelifera at each site.

caelifera_observations <- get_caelifera_only(all_observations_conservative)

caelifera_species_richness <- calculate_species_richness_sites(caelifera_observations)

caelifera_species_richness_sites <- left_join(caelifera_species_richness, site_env_var_data,
                                    by = c("site_elevation", "area", "elevational_band_m"))

caelifera_species_richness_sites <- left_join(caelifera_species_richness_sites, sampling_effort, by = "site_elevation")

#' GLM of Caelifera species richness with the set of parameters as used in the full model of overall
#' species richness.

glm_species_richness_full_caelifera <- glm(species_richness ~ elevational_band_m + as.factor(area) + slope +
                                        as.factor(aspect_cardinal) + sampling_effort_index +
                                        mean_perc_veg_cover + mean_max_height + mean_density,
    family = poisson(link = "log"),
    data = caelifera_species_richness_sites)

#' Summarise the GLM

summary(glm_species_richness_full_caelifera)

#' Do ANOVA of GLM

Anova(glm_species_richness_full_caelifera)
logLik(glm_species_richness_full_caelifera)

#' Get AICC of GLM

AICcmodavg::AICc(glm_species_richness_full_caelifera, return.K = FALSE, second.ord = TRUE)

#' Do backwards stepwise selection on the GLM to get the reduced model

glm_species_richness_caelifera_step <- stats::step(glm_species_richness_full_caelifera, direction = "backward")

#' Summarise the reduced GLM

summary(glm_species_richness_caelifera_step)

#' Do ANOVA of reduced GLM

Anova(glm_species_richness_caelifera_step)
logLik(glm_species_richness_caelifera_step)

#' Get AICC of reduced GLM

AICcmodavg::AICc(glm_species_richness_caelifera_step, return.K = FALSE, second.ord = TRUE)

#' Define the reduced model for Caelifera species richness

glm_species_richness_caelifera_reduced <- glm_species_richness_caelifera_step

#' Test removing elevation because it is not significant in the reduced model. See if it changes the
#' predictive power of the model.

glm_species_richness_caelifera_reduced_no_elevation <- glm(species_richness ~ slope +
                                        sampling_effort_index + mean_perc_veg_cover + mean_density,
    family = poisson(link = "log"),
    data = caelifera_species_richness_sites)

summary(glm_species_richness_caelifera_reduced_no_elevation)
Anova(glm_species_richness_caelifera_reduced_no_elevation)
logLik(glm_species_richness_caelifera_reduced_no_elevation)
AICcmodavg::AICc(glm_species_richness_caelifera_reduced_no_elevation, return.K = FALSE, second.ord = TRUE)

#' Removing elevation from the model does not improve the predictive power of the model (AICC = 132 still)
#' and the parameter estimates for the other variables remain the same, therefore we leave it in the
#' reduced model.
#'
#' ### Model assessment
#'
#' Plot the observed against fitted values for the full model and each of the final reduced models. Also
#' plot the residuals.
#'
#' Also test if the model is suitable. H0: model is correct. H1: model is not correct. To do this, calculate
#' the p-value for the model where the deviance and degrees of freedom are used.

#' Test the reduced model which was the outcome of the manual stepwise selection

par(mfrow = c(1,2))
plot(species_richness_sites$species_richness, fitted(glm_species_richness_reduced),
     xlab = "Observed values", ylab = "Fitted values")
abline(0,1)
plot(fitted(glm_species_richness_reduced), residuals(glm_species_richness_reduced, type = "pearson"))
abline(h = 0)

reduced_test <- 1-pchisq(13.92499, 18)
reduced_test

#' As P is large (0.77), we have no evidence against the hypothesis that the model is adequate, therefore
#' we accept the model is satisfactory.
#'
#' ### Plot the GLM
#'
#' Plot the modelled variables

par(mfrow=c(3, 2))
visreg(glm_species_richness_reduced, xvar = "elevational_band_m")
visreg(glm_species_richness_reduced, xvar = "area")
visreg(glm_species_richness_reduced, xvar = "slope")
visreg(glm_species_richness_reduced, xvar = "sampling_effort_index")
visreg(glm_species_richness_reduced, xvar = "mean_perc_veg_cover")
visreg(glm_species_richness_reduced, xvar = "mean_density")

#' Plot the predicted values on top of the actual data points.

par(mfrow = c(1,1))
visreg(glm_species_richness_reduced, xvar = "elevational_band_m",
                                     scale = "response",
                                     rug = FALSE,
                                     line = list(col = "black"),
                                     xlab = "Elevation (m a.s.l)", xlim = c(1000, 2600),
                                     ylab = "Species richness", ylim = c(0, 17))
points(species_richness ~ elevational_band_m, data = species_richness_sites, pch = 1.5, col = "black", lwd = 1.5)

# elevationalrange_elevation_plot <- format_theme_ggplot(elevationalrange_elevation_plot)
# save_plot(elevationalrange_elevation_plot, "hypothesis2_elevational_range_model.png")

#' ### Output tables for report
#'
#' Reduced model for overall species richness

glm_species_richness_reduced_table <- xtable(glm_species_richness_reduced)
glm_species_richness_reduced_table
print(glm_species_richness_reduced_table)
knitr::kable(glm_species_richness_reduced_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P <- 0.05) for variables included in the
reduced GLM for overall species richness (AICc = 145.24)")

#' Reduced model for Caelifera species richness

glm_species_richness_reduced_caelifera_table <- xtable(glm_species_richness_caelifera_reduced)
glm_species_richness_reduced_caelifera_table
print(glm_species_richness_reduced_caelifera_table)
knitr::kable(glm_species_richness_reduced_caelifera_table, caption = "Parameter estimate and
standard error, Wald's chi-squared and significance level (P < 0.05) for variables included in the reduced
 GLM for Caelifera species richness (AIC = 132.0)")

#'
#'
#' ## Linear regression - NOT CHANGED
#' Create a linear model of species richness against elevation and look at the model output.
linear_regression_species_richness <- linear_regression_species_richness_elevation(species_richness_sites,
                                                                                   "species_richness", "elevational_band_m")
summary(linear_regression_species_richness)

#' The linear regression shows that both the intercept and slope are statistically significant. Species
#' richness decreases by 6.1 with an increase in elevation of 1000 m (<em>t</em> = -4.29,
#' <em>p</em> = 0.0002).
#'
#' ### Check the assumptions of linear regression

#' Plot the residuals to check if the assumptions of the residuals apply for this dataset.
plot_model_residuals_species_richness_elev(linear_regression_species_richness, species_richness_sites,
                                           "elevational_band_m")
par(mfrow = c(2, 2))
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
#' above) in general, but the point which corresponds to the highest species richness, seems to
#' be a bit of an outlier, skewing the distribution somewhat.
#' </ul>
#'
#' ### Plot linear regression
#' The plot below shows the species richness at each site (blue dots). Upper and lower bounds of the
#' predicted values are bounded within the grey area. The blue line represents the linear relationship
#' between species richness ($SR$) and elevation ($E$), which can be represented by the equation,
#' $SR = 17.76 - 0.0061E$.
#+ message=FALSE, warning=FALSE

plot_linear_regression_species_richness(species_richness_sites, linear_regression_species_richness)

#' ### Test linear regression using t-test
#' Use a one-way t-test to check if there is a statistically significant relationship between the response
#' and explanatory variables.
#' <br>H<sub>0</sub>: the slope of the regression is equal to 0.
#' <br>H<sub>1</sub>: the slope of the regression is not equal to 0.
#'
#' Looking again at the model output above, we can see that the slope is not equal to 0
#' (<em>p</em> = 0.0002), therefore we can say there is a significant relationship between the species
#' richness and elevation.
#'
#' ## Check the regression if non-conservative identifications are used

#' Some specimens were identified to one of multiple species or genus, because of, for example, missing or
#' contradictory features. These are used in a conservative way in the analysis above. This part of the
#' analysis will check if there is any difference to the relationship between species richness and
#' elevation, if the less conservative identifications are used.
#'
#' ### Linear regression

#' Create a linear model of species richness against elevation and look at the model output.
#+ message=FALSE, warning=FALSE

linear_regression_species_richness_notconservative <- linear_regression_species_richness_elevation(species_richness_sites_notconservative,
                                                                                                   "species_richness", "elevational_band_m")
summary(linear_regression_species_richness_notconservative)

plot_linear_regression_species_richness(species_richness_sites_notconservative, linear_regression_species_richness_notconservative)

#' The linear regression shows that both the intercept and slope are statistically significant. Species
#' richness decreases by 6.6 with an increase in elevation of 1000 m (<em>t</em> = -4.53,
#' <em>p</em> = 0.0001).
#'
#' ### Test for statistical significance between regressions
#'
#' Use a hypothesis test to check for any statistical difference between the slopes of the regressions
#' with the conservative and non-conservative identifications.
#' <br>H<sub>0</sub>: regression lines are parallel.
#' <br>H<sub>1</sub>: regression lines are not parallel.
#'
#' **TODO**: would it be valid to use ANCOVA for this? The values would not be independent because they are
#' from the same sites.
#'
#' ## Check for effect of study area

species_richness_sites$area <- as.factor(species_richness_sites$area) # make sure that area is considered
# as a factor

#' Plot species richness against elevation again with different colours representing the different study
#' areas to visualise and possible differences in the relationship which could depend on the study area.

plot_elevation_species_richness_area(species_richness_sites)

#' Species richness at Tavascan and La Molinassa both show a general trend of decreasing species richness
#' with elevation, but at Tor, there does not seem to be such a trend. Two additional study areas, Besan
#' and Bordes de Viros are included in this plot. These sites were located further down the valley to
#' include lower elevations which were not available within the other study areas.
#'
#' ### Fit linear mixed model
#' Fit a linear mixed model, treating study area and sampling effort as a random factor.
#+ message=FALSE, warning=FALSE

lmm_species_richness_elev_area_sampling_effort <- lmer(species_richness ~ elevational_band_m + (1 | area),
                                                       data = species_richness_sites)
summary(lmm_species_richness_elev_area_sampling_effort)

#' The value of variance for area (random effects section) is 0 (or presumably just very small if only
#' 3dp are used). Can this can occur because of sampling error? **TODO**: check if this is important.
#'
#' I realise that I'm not sure how to interpret this output. The parameter estimates are the same as
#' those in the model above. **TODO**: how does this output tell us that area is important?
#'
#' ### Test assumptions of linear mixed model

plot(lmm_species_richness_elev_area_sampling_effort)

qqnorm(resid(lmm_species_richness_elev_area_sampling_effort))
qqline(resid(lmm_species_richness_elev_area_sampling_effort))

#' The plot of the residuals does not show an obvious pattern, although there is one obvious outlier. The
#' data seem to fit a normal distribution.
#'
#' ## Investigate effects of elevation on species richness within different study areas
#'
#' A linear regression will be used to look at the relationship between species richness and elevation at
#' each of the study areas separately.
#'
#' ### Linear regression

#'  Run linear regression for each study area separately. Ignore the two small areas (Besan and Bordes de
#' Viros) which do not have an elevational gradient.
#'
#' #### Tor
#+ message=FALSE, warning=FALSE

species_richness_tor <- species_richness_sites[species_richness_sites$area == "Tor",]

lin_reg_species_richness_area_tor <- lm(species_richness ~ elevational_band_m,
                                        data = species_richness_tor)
summary(lin_reg_species_richness_area_tor)

plot_linear_regression_species_richness(species_richness_tor, lin_reg_species_richness_area_tor)

#' #### La Molinassa
#+ message=FALSE, warning=FALSE

species_richness_mol <- species_richness_sites[species_richness_sites$area == "La Molinassa",]

lin_reg_species_richness_area_mol <- lm(species_richness ~ elevational_band_m,
                                        data = species_richness_mol)
summary(lin_reg_species_richness_area_mol)

plot_linear_regression_species_richness(species_richness_mol, lin_reg_species_richness_area_mol)

#' #### Tavascan
#+ message=FALSE, warning=FALSE

species_richness_tav <- species_richness_sites[species_richness_sites$area == "Tavascan",]

lin_reg_species_richness_area_tav <- lm(species_richness ~ elevational_band_m,
                                        data = species_richness_tav)
summary(lin_reg_species_richness_area_tav)

plot_linear_regression_species_richness(species_richness_tav, lin_reg_species_richness_area_tav)

#' Whilst there was no relationship at Tor (<em>r</em> = -0.30; <em>F</em> = 0.77; <em>df</em> = 1, 8;
#' <em>p</em> = 0.41), a clear decrease in species richness with elevation can be seen at La Molinassa
#' (<em>r</em> = -0.80; <em>F</em> = 10.34; <em>df</em> = 1, 6; <em>p</em> = 0.02) and Tavascan
#' (<em>r</em> = -0.97; <em>F</em> = 79.95; <em>df</em> = 1, 5; <em>p</em> < 0.01).
#'
#' ### Plot linear regression

#' **TODO**: I'll put the above plots on one set of axes.
#'
#' ## Investigate effects of elevation on Caelifera species richness
#' Only five species of Ensifera were detected during the surveys, so species richness relationships will
#' be explored further just for the Caelifera.

caelifera_observations <- get_caelifera_only(all_observations_conservative)

caelifera_species_richness_sites <- calculate_species_richness_sites(caelifera_observations)
caelifera_species_richness_sites$area <- as.factor(caelifera_species_richness_sites$area) # make sure
# that area is considered as a factor

plot_elevation_species_richness_area(caelifera_species_richness_sites)

#' Run linear regression for each site separately. Ignore the two small sites which do not have an
#' elevational gradient.
#'
#' #### Tor
#+ message=FALSE, warning=FALSE

caelifera_species_richness_tor <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "Tor",]

lin_reg_caelifera_species_richness_area_tor <- linear_regression_species_richness_elevation(caelifera_species_richness_tor,
                                                                                            "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_tor)

plot_linear_regression_species_richness(caelifera_species_richness_tor, lin_reg_caelifera_species_richness_area_tor)

#' #### La Molinassa
#+ message=FALSE, warning=FALSE

caelifera_species_richness_mol <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "La Molinassa",]

lin_reg_caelifera_species_richness_area_mol <- linear_regression_species_richness_elevation(caelifera_species_richness_mol,
                                                                                            "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_mol)

plot_linear_regression_species_richness(caelifera_species_richness_mol, lin_reg_caelifera_species_richness_area_mol)

#' #### Tavascan
#+ message=FALSE, warning=FALSE

caelifera_species_richness_tav <- caelifera_species_richness_sites[caelifera_species_richness_sites$area == "Tavascan",]

lin_reg_caelifera_species_richness_area_tav <- linear_regression_species_richness_elevation(caelifera_species_richness_tav,
                                                                                            "species_richness", "elevational_band_m")
summary(lin_reg_caelifera_species_richness_area_tav)

plot_linear_regression_species_richness(caelifera_species_richness_tav, lin_reg_caelifera_species_richness_area_tav)

#' There was no evidence of a relationship between Caelifera species richness and elevation at Tor
#' (<em>r</em> = -0.11; <em>F</em> = 0.09; <em>df</em> = 1, 8; <em>p</em> = 0.77), but a clear and
#' significant decrease with elevation at both La Molinassa (<em>r</em> = -0.82; <em>F</em> = 12.35;
#' <em>df</em> = 1, 6; <em>p</em> = 0.01) and Tavascan (<em>r</em> = -0.98; <em>F</em> = 141.2;
#' <em>df</em> = 1, 5; <em>p</em> = < 0.001>).
#'
#'


#' ## Results
#'
#' <br>A simple linear regression was used to investigate the relationship between elevation and species richness.
#' Overall species richness and elevation were negatively correlated (Pearson's correlation coefficient
#' = -0.64). Species richness was shown to decrease by 6.1 for an increase in elevation of 1000 m
#' (<em>t</em> = -4.29; <em>p</em> < 0.001>). However, only 41% of the variation in species richness can
#' be explained by elevation (<em>F</em> = 18.39; <em>df</em> = 1, 26; <em>p</em> < 0.001).
#'
#' **TODO**: add details of effects of sampling effort.
#'
#' Looking at the plot of the species richness coloured by study area, there seems to be a strong decrease
#' in species richness with elevation at both La Molinassa and Tavascan, but no change at Tor. A linear
#' mixed model fitting elevation as a fixed effect and study area as a random effect, was used to
#' investigate if there was any effect of study area on species richness. [**TODO**: add interpretation of
#' this]. Given the effect of area on species richness, simple linear regressions were used to model the
#' species richness with elevation within each study area separately. Whilst there was no relationship at
#' Tor (<em>r</em> = -0.30; <em>F</em> = 0.77; <em>df</em> = 1, 8; <em>p</em> = 0.41), a clear decrease
#' in species richness with elevation can be seen at La Molinassa (<em>r</em> = -0.80; <em>F</em> = 10.34;
#' <em>df</em> = 1, 6; <em>p</em> = 0.02) and Tavascan (<em>r</em> = -0.97; <em>F</em> = 79.95;
#' <em>df</em> = 1, 5; <em>p</em> < 0.01).
#'
#' Out of 37 species observed, 29 were Caelifera. A linear mixed model with elevation as a fixed effect
#' and study area as a random effect, to investigate if the relationship between species richness and
#' elevation followed a similar trend when using observations just of the Caelifera. Trends at all three
#' study areas were similar to those already reported, with no evidence of a relationship between
#' Caelifera species richness and elevation at Tor (<em>r</em> = -0.11; <em>F</em> = 0.09; <em>df</em> =
#' 1, 8; <em>p</em> = 0.77), but a clear and significant decrease with elevation at both La Molinassa
#' (<em>r</em> = -0.82; <em>F</em> = 12.35; <em>df</em> = 1, 6; <em>p</em> = 0.01) and Tavascan
#' (<em>r</em> = -0.98; <em>F</em> = 141.2; <em>df</em> = 1, 5; <em>p</em> = < 0.001).
#'
#' ## Questions
#' <ol>
#'  <li>When reporting the decrease in species richness with elevation and variation explained (first
#' paragraph of results above), should both t and F be reported? Should the p-value only be reported
#' once?</li>
#'  <li>I'm not sure how to interpret the output of the linear mixed model with elevation as a fixed
#' effect and study area as a random effect
#' (https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_species_richness_hypothesis1.html#fit-linear-mixed-model).
#' How can I use this to say that from this result, we decided to look at the relationship for each study
#' area separately? In the output for area there are no p-values.</li>
#'  <li>In the same output, does it matter that the variance of area is very small (0)?</li>
#'  <li>To calculate <em>r</em> which is included in the last part of the results section above where we
#' report the relationships for modelling species richness separately for each study area, I have used the
#' multiple R-squared (rather than adjusted R-squared) output because I understand that we are not
#' adjusting for the number of predictors in the model. Is this correct or should I be using something else?</li>
#' <li>Accounting for sampling effort: I have calculated an index of sampling effort using the proportion
#' of Orthoptera captured by hand and net (from all surveys) and then summing the product of the relevant
#' proportion and number of Orthoptera captured by hand and net at each site. We were thinking about
#' including this as a factor in the linear model, but does this sound like the right way to go?</li>
#' <li>Where specimens could only be identified to one of two (or more) taxa, the identification was
#' chosen to be conservative in terms of affecting the species richness at a site. In order to make sure
#' this did not affect the results, I wanted to compare the models when using the conservative and
#' non-conservative identifications. Having done the models, would it be correct to use something like
#' ANCOVA to compare them and check if they are statistically different? (see https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_species_richness_hypothesis1.html#test-for-statistical-significance-between-regressions)</li>
#' </ol>