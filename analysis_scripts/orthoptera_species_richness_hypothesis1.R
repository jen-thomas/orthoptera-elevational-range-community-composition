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

vector_packages <- c("visreg", "ggplot2", "lmerTest", "dplyr", "car")
get_packages(vector_packages)

#' ## Investigate effects of elevation on species richness
#+ message=FALSE, warning=FALSE

#' The following functions calculate the species richness and do the statistics for the analysis of this hypothesis.

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

#' ## Import and set up the data

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"

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
#'
#' ### Correlate species richness and elevation
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
#' elevation (<em>r</em> = -0.64, <em>t<sub>26</sub></em> = -4.29, <em>p</em> = 0.0002), however only
#' 41% of the variation in species richness is explained by the elevation.
#'
#' ### Plot species richness against elevation

plot_elevation_species_richness(species_richness_sites)

#' The plot shows a general decreasing trend of species richness with elevation, which was confirmed by
#' the correlation coefficient above.
#'
#' ### Linear regression
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
#' ## Account for the effects of sampling effort
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
#' <br>Look at the relationship between species richness and sampling effort,
#+ message=FALSE, warning=FALSE

lin_reg_species_richness_sampling_effort <- lm(species_richness_sites[["species_richness"]] ~
                                                 species_richness_sites[["sampling_effort_index"]],
                                               data = species_richness_sites)
summary(lin_reg_species_richness_sampling_effort)
plot_lin_reg_sampling_effort_species_richness(species_richness_sites,
                                              lin_reg_species_richness_sampling_effort)

#' and how the sampling effort varied across the elevational range.
#+ message=FALSE, warning=FALSE

lin_reg_elevation_sampling_effort <- lm(species_richness_sites[["sampling_effort_index"]] ~
                                          species_richness_sites[["elevational_band_m"]],
                                        data = species_richness_sites)
summary(lin_reg_elevation_sampling_effort)
plot_lin_reg_sampling_effort_elevation(species_richness_sites,
                                       lin_reg_elevation_sampling_effort)

corr_test_sampling_effort <- correlation_test(species_richness_sites, "elevational_band_m",
                                              "sampling_effort_index")
corr_coeff_sampling_effort <- corr_test_sampling_effort$estimate
print(corr_test_sampling_effort)

#' As expected, species richness significantly increases with sampling effort (<em>R<sup>2</sup></em> =
#' 0.41; <em>p</em> = 0.0003). Sampling effort was lower at higher elevations (<em>R<sup>2</sup></em> =
#' 0.14; <em>p</em> = 0.05), but only 40% of the variation in the sampling effort index was explained by
#' the elevation.
#'
#' ## Generalised linear model

#' Look at the distribution of the species richness.
par(mfrow=c(1,1))
hist(species_richness_sites$species_richness)
#' Predict species richness using elevation (fixed effect). Include area and sampling effort as random factors.
#'
#' Fit the basic model.

glm_species_richness_basic_poiss <- glm(species_richness ~ elevational_band_m,
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_basic_poiss)
Anova(glm_species_richness_basic_poiss)

#' Add just sampling effort

glm_species_richness_basic_sampling_poiss <- glm(species_richness ~ elevational_band_m + (1 | sampling_effort_index),
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_basic_sampling_poiss)
Anova(glm_species_richness_basic_sampling_poiss)

#' Add just area as factor

glm_species_richness_basic_areafac_poiss <- glm(species_richness ~ elevational_band_m + (1 | as.factor(area)),
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_basic_areafac_poiss)
Anova(glm_species_richness_basic_areafac_poiss)

#' Add just area

glm_species_richness_basic_area_poiss <- glm(species_richness ~ elevational_band_m + (1 | area),
    family = poisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_basic_area_poiss)
Anova(glm_species_richness_basic_area_poiss)

#' Do full model with area not as a factor
glm_species_richness_full_area_notfac_poiss <- glm(species_richness ~ elevational_band_m + (1 | area) + (1 | sampling_effort_index),
    family = poisson(link = "log"),
    data = species_richness_sites)

# Check to see the importance of each covariate.
summary(glm_species_richness_full_area_notfac_poiss)
Anova(glm_species_richness_full_area_notfac_poiss)

#' Fit the full model with all covariates. Area will be used as a factor and no offset will be used.
#' Sampling effort will instead be included as a factor.

glm_species_richness_full_poiss <- glm(species_richness ~ elevational_band_m + (1 | as.factor(area)) + (1 | sampling_effort_index),
    family = poisson(link = "log"),
    data = species_richness_sites)

# Check to see the importance of each covariate.
summary(glm_species_richness_full_poiss)
Anova(glm_species_richness_full_poiss)

print("*****************************************************")
logLik(glm_species_richness_basic_poiss)
logLik(glm_species_richness_basic_sampling_poiss)
logLik(glm_species_richness_full_poiss)
logLik(glm_species_richness_full_area_notfac_poiss)
logLik(glm_species_richness_basic_areafac_poiss)
logLik(glm_species_richness_basic_area_poiss)

AIC(glm_species_richness_basic_poiss)
AIC(glm_species_richness_basic_sampling_poiss)
AIC(glm_species_richness_full_poiss)
AIC(glm_species_richness_full_area_notfac_poiss)
AIC(glm_species_richness_basic_areafac_poiss)
AIC(glm_species_richness_basic_area_poiss)
print("*****************************************************")


#' Elevation and sampling effort are both significant; area is almost significant (p = 0.05 if rounded
#' down). No individual areas have a significant effect.
#'
#' If the ratio of the residual deviance to the residual degrees of freedom exceeds 1.5, then the model is
#' overdispersed.

ratio_dispersion <- summary(glm_species_richness_full_poiss)$deviance / summary(glm_species_richness_full_poiss)$df.residual
ratio_dispersion

#' Given that the ratio < 1.5, then there is no overdispersion.

#' Fit a quasipoisson distribution to see if there is a problem with overdispersion. A poisson
#' distribution assumes that the overdispersion is 1, so if the overdispersion from the quasipoisson is
#' greater than 1, then we have this problem with the Poisson distribution. (This part is not needed as
#' we have already shown that there is no overdispersion).

glm_species_richness_full_quasipoiss <- glm(species_richness ~ elevational_band_m + (1 | as.factor(area)) + (1 | sampling_effort_index),
    family = quasipoisson(link = "log"),
    data = species_richness_sites)

summary(glm_species_richness_full_quasipoiss)
Anova(glm_species_richness_full_quasipoiss)

#' Try removing sampling effort as a factor and including it in the model as an offset.

glm_species_richness_full_samplingoffset <- glm(species_richness ~ elevational_band_m + (1 | as.factor(area)),
    family = poisson(link = "log"),
    offset = log(sampling_effort_index),
    data = species_richness_sites)
summary(glm_species_richness_full_samplingoffset)
Anova(glm_species_richness_full_samplingoffset)

#' Removing sampling effort makes area become a significant parameter. However, it has been included in
#' the other models as a factor because species richness does not depend on sampling effect by a fixed
#' amount.

#' Model selection.

#' Set up the null model for the stepwise selection process.
glm_species_richness_null <- glm(species_richness ~ 1,
                 data = species_richness_sites,
                 family = poisson(link = "log")
                 )

#' Try step-wise selection.
glm_species_richness_full_poiss_step <- step(glm_species_richness_null,
                                             scope = list(upper = glm_species_richness_full_poiss),
                                             direction = "both",
                                             test="Chisq",
                                             data = species_richness_sites)
print("--------Stepwise selection-----------------")
glm_species_richness_full_poiss_step

#' This shows us that AIC gets worse as covariates are removed from the model, so the model should retain
#' the parameters that were included to begin with.

#' Model assessment

par(mfrow = c(1,2))
plot(species_richness_sites$species_richness, fitted(glm_species_richness_full_poiss), xlab = "Observed values", ylab = "Fitted values")
abline(0,1)
plot(fitted(glm_species_richness_full_poiss), residuals(glm_species_richness_full_poiss, type = "pearson"))
abline(h = 0)

#' Test if the model is suitable. H0: model is correct. H1: model is not correct. To do this, calculate
#' the p-value for the model where the deviance and degrees of freedom are used.
p_model_test <- 1-pchisq(21.452, 21)
p_model_test

#' As p is large, we have no evidence against the hypothesis that the model is adequate, therefore we
#' accept the model is satisfactory.

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