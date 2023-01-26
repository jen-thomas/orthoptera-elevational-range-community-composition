#' ---
#' title: Testing data from Claridge and Singhrao, 1978
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
source("orthoptera_elevational_range_hypothesis2.R")

vector_packages <- c("ggplot2", "dplyr")
get_packages(vector_packages)

#' ## Prepare data
#+ message=FALSE, warning=FALSE

elevational_ranges_species <- "../data/elevational_observations_claridge_singhrao_1978.csv"
elevational_ranges_species_df <- read_csv_data_file(elevational_ranges_species)

#' ### Calculate polynomials
elevational_ranges_species_df <- calculate_polynomials_elevation(elevational_ranges_species_df)

lin_regs_polynomial <- linear_regression_elevrange_elevation_polynomial(elevational_ranges_species_df)

lin_reg_ <- lin_regs_polynomial[[1]]
nonlin_reg_quadratic <- lin_regs_polynomial[[2]]
nonlin_reg_cubic <- lin_regs_polynomial[[3]]
nonlin_reg_quartic <- lin_regs_polynomial[[4]]

#' ### Compare the polynomial models
compareLM(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

anova(lin_reg_, nonlin_reg_quadratic, nonlin_reg_cubic, nonlin_reg_quartic)

elevational_ranges_species_predicted <- cbind(elevational_ranges_species_df, predict(nonlin_reg_quadratic, interval = "confidence"))

### Prepare predicted and fitted values
#' Get min and max of values of plot ready for predictions
i_all_sp <- seq(min(elevational_ranges_species_predicted$elevational_range_midpoint),
         max(elevational_ranges_species_predicted$elevational_range_midpoint), len=100) #  x-value limits for line

#' Calculate the predicted values from the regression so they can be plotted as a line
predicted_values_all_sp <- predict(nonlin_reg_quadratic,
                            data.frame(elevational_range_midpoint=i_all_sp, elevational_range_midpoint2=i_all_sp*i_all_sp)) #  fitted values
intervals_all_sp <-  predict(nonlin_reg_quadratic,
                      data.frame(elevational_range_midpoint=i_all_sp, elevational_range_midpoint2=i_all_sp*i_all_sp), interval = "confidence")

#' Put the values into a dataframe
confidence_bands_all_sp <- data.frame(i_all_sp, intervals_all_sp)

#' Get the coefficients of the equation and put these into text
cf_all_sp <- signif(coef(nonlin_reg_quadratic), 2)

int_term_all_sp <- cf_all_sp[1]
lin_term_all_sp <- cf_all_sp[2]
quadratic_term_all_sp <- abs(cf_all_sp[3])

equation_all_sp <- bquote(italic(E[R]) == .(int_term_all_sp) + .(lin_term_all_sp)*italic(E) - .(quadratic_term_all_sp)*italic(E)^2)

#' ## Create the plot

ggplot(data = elevational_ranges_species_predicted,
           aes(x = elevational_range_midpoint, y = elevational_range)) +
  geom_jitter(size = 1.8, show.legend = FALSE, width = 30, height = 20) +
  scale_shape_manual(values = c(1, 4)) +
  ylim(0, 1600) +
  xlim(200, 1701) +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = fit), lty=1, lwd=0.5, col="black") +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = lwr),
                    lwd=0.5, col="darkgrey", linetype = "dashed") +
  geom_line(data = confidence_bands_all_sp, aes(x = i_all_sp, y = upr),
         lwd=0.5, col="darkgrey", linetype = "dashed") +
  annotate("text", label = equation_all_sp, x = 1000, y = 1550, cex = 3) +
  labs(x = "Elevational range midpoint (m a.s.l)",
       y = "Elevational range (m)") +
  theme_classic()