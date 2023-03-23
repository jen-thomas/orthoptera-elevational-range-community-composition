#' ---
#' title: Hypothesis 3
#' subtitle: Orthoptera community composition is influenced by elevation and environmental variables.
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
source("prepare_vegetation_data.R")
source("get_physical_site_data.R")

vector_packages <- c("visreg", "ggplot2", "dplyr", "raster", "terra", "XML", "lubridate", "sp",
                     "maptools", "leaflet", "rgeos", "corrplot", "vegan", "goeveg", "phytools", "tibble",
"factoextra")
get_packages(vector_packages)

#' ## Create site-species matrix.

#' The following functions are used to prepare the data and create the species-site matrix, and matrix of
#' environmental variables.

create_presence_absence_site_species_matrix <- function(observations_df) {
    #' Create a presence-absence site-species matrix. Each value is either 0 (species not observed at a
    #' site) or 1 (species observed at a site).
    #'
    #' Return matrix.

  site_species_presenceabsence_matrix <- t(create.matrix(observations_df, tax.name = "taxa",
                                                         locality = "site_elevation", abund = FALSE))

  return(site_species_presenceabsence_matrix)
}

create_env_var_matrix <- function(env_var_df) {
    #' Create a matrix of the environmental variables for each site.
    #'
    #' Return matrix.

      rownames(env_var_df) <- env_var_df$site_elevation

    #' select only a subset of the parameters to use in the analysis
    env_var_matrix <- dplyr::select(env_var_df, elevational_band_m, sampling_effort_index, slope, aspect_cardinal, area,
                                  mean_perc_veg_cover, mean_height_75percent_cm, mean_density)

  return(env_var_matrix)
}

#' Read in and prepare the site-species data for the matrix.

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
vegetation_file <- "../data/vegetation_plots.csv"

sites_df <- read_csv_data_file(sites_file)
sites_df <- rename_site_with_elevation(sites_df)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

all_observations_conservative <- get_conservative_observations(confirmed_observations,
                                                               finalised_observations)
sampling_effort <- calculate_sampling_weights(all_observations_conservative)

unique_taxa_sites <- get_unique_taxa_site(all_observations_conservative)

#' Get unique taxa across all sites so that higher taxa do not include taxa that may already have been
#' observed at the same or different sites. Also remove records where the taxa was one of multiple taxa,
#' but only if they have all been observed, or could have been observed at the same or other sites. The
#' site-species matrix that this produces will therefore be on the conservative side.

unique_taxa_all_sites <- get_unique_taxa_all_sites(unique_taxa_sites)

#' Create and preview the presence-absence site-species matrix. Site name is in the format
#' elevation(m)_site where the name is an abbreviation of the study area.

site_species_matrix <- create_presence_absence_site_species_matrix(unique_taxa_all_sites)
site_species_matrix

#' ## Environmental variables
#'
#' Environmental variables will be checked for collinearity. Site aspect will be converted to
#' four compass points and other environmental variables transformed as needed.
#'
#' ### Vegetation data

vegetation_averaged_df <- prepare_veg_data(sites_file, vegetation_file)

#' ### Site topography

#' Get digital elevation model (DEM) data for the study areas and calculate slope and aspect at each site.
#' These parameters were calculated along each transect and averaged to get one value per site. The DEM
#' data were provided by the Institut Cartogràfic i Geològic de Catalunya (ICGC) with a resolution of
#' 2x2m.

dem_study_areas <- get_dem_data()

#' Plot DEM to get an overview. Only the areas of the DEM files are displayed. Also look at the number of
#' layers within the raster, the coordinate system and get an overview of the data.
#+ message=FALSE, warning=FALSE

get_overview_dem(dem_study_areas)

#' Calculate the slope and aspect along each transect.
#'
#' Each transect was recorded as a number of points. These transects were imported into R
#' and using the sp package, were extrapolated into a line. Using the rgeos and terra packages, slope and
#' aspect values were averaged from the four nearest raster cells, every 2m along the transect. These
#' values of slope and aspect were then averaged, to get one value for each site.
#+ message=FALSE, warning=FALSE

site_topography <- get_site_topography(sites_df)

#' Put vegetation and terrain data into one dataframe.
#+ message=FALSE, warning=FALSE

site_env_var_data <- left_join(site_topography, vegetation_averaged_df, by = "site_elevation")
site_var_data <- left_join(site_env_var_data, sampling_effort, by = "site_elevation")

#' ### Check for collinearity between environmental variables

check_collinearity(site_var_data)

#' Looking at the previous plots we can see that there seems to be some collinearity between the two
#' measures of vegetation height. Given that the maximum vegetation height can just represent one piece of
#' vegetation within the plots, the mean 75% height of vegetation in the plot is a more suitable measure
#' of the vegetation across the plots at a site. As expected, the compositional data representing three
#' measures of ground cover in terms of percentages, are also correlated. The vegetation ground cover will
#' be used in this analysis.
#'
#' Prepare the matrix of environmental variables to be used in the next part of the analysis. Only include
#' site elevation, aspect, slope, vegetation height, vegetation ground cover and vegetation density.

env_var_matrix <- create_env_var_matrix(site_var_data)

#' ## Calculate dissimilarity matrix

#' Calculate a dissimilarity matrix using the Jaccard distance. This is used because it excludes double
#' zeros whilst giving equal weight to non-zero agreements and disagreements. Data are already binary so
#' there is no need to convert the matrix to presence-absence before calculating the matrix.

species_jaccard_dist <- vegdist(site_species_matrix, method = "jaccard")
species_jaccard_dist

#' Convert dissimilarity indices to matrix because they cannot be used in the kmeans function as they are.
#' Now both the upper and lower halves of the matrix will be the same.

species_jaccard_dist_matrix <- as.matrix(species_jaccard_dist)
species_jaccard_dist_matrix

#' ## Test beta diversity between sites - 2023-01-13
#'
#' Beta diversity is a measure of the similarity in community composition between samples, in this case,
#' sites.
#'
#' This calculation will be based on the Jaccard distance matrix calculated above.
#' Code modified from https://grunwaldlab.github.io/analysis_of_microbiome_community_data_in_r/07--diversity_stats.html

mds <- metaMDS(species_jaccard_dist_matrix, k = 2, distance = "jaccard", trymax = 1000, trace = TRUE)
mds_df <- as.data.frame(mds$points)
mds_df$site_elevation <- rownames(mds_df)

#' Join the MDS data with the site species matrix and plot it, coloured by elevation and with different
#' point shapes for each study area so that any similarities can be distinguished according to these
#' parameters.

mds_site_data <- dplyr::left_join(mds_df, site_survey_df)

ggplot(mds_site_data, aes(x = MDS1, y = MDS2, color = elevational_band_m, shape = area)) +
  geom_point(size=3.5) +
  scale_color_gradient(low = "#0072B2", high = "#C4961A")

#' The three lower-elevation sites (coloured in dark blue) are fairly similar in terms of their species
#' composition. There is a very lose grouping of the sites at Tor, and another for those at La Molinassa.
#' The trend from low elevation to higher elevations can be seen as well. In itself this is interesting
#' and knowing the sites, it is a nice way to see which have a similar community composition. I am not
#' sure though, if this plot adds anything that cannot be seen in figure 2 (in the manuscript). I would
#' welcome your thoughts on this though.
#'
#' ## K-means cluster analysis

#' Determine the number of clusters to use for K-means clustering. Firstly, calculate the within group sum
#' of squares if we consider up to fifteen clusters. Plot these and look for the step of where the sum of
#' squares decreases. Code adapted from https://www.statmethods.net/advstats/cluster.html

wss <- (nrow(species_jaccard_dist_matrix) - 1) * sum(apply(species_jaccard_dist_matrix, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(species_jaccard_dist_matrix, centers=i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")

#' There is no clear difference in this scree plot to identify the number of clusters to choose. From 5
#' onwards, there seems to be a slight flattening, so this might be reasonable.
#'
#' Use the Silhouette scores to see if 5 sounds like a reasonable number of clusters. Find where the
#' maximum is reached.

fviz_nbclust(species_jaccard_dist_matrix, kmeans, method='silhouette')

#' From the Silhouette plot, choose to use K-means with 5 clusters. Do the cluster analysis.

kmeans_fit <- kmeans(species_jaccard_dist_matrix, 5, nstart = 100)

#' Add the cluster means to the matrix (this is probably not needed at this point. Adding it to the plot
#' would make it a bit too cluttered).

for (name in names(kmeans_fit$cluster)) {
  cluster_number <- as.numeric(kmeans_fit$cluster[name])
  env_var_matrix[name, "cluster_group"] <- cluster_number
  }

#' Look at the clusters.

kmeans_fit_clusters <- kmeans_fit$cluster
species_jaccard_dist_matrix[order(kmeans_fit_clusters), ]
kmeans_fit_clusters

#' There seems to be one cluster which includes the low sites at Besan (BES) and les Bordes de Viros
#' (BOR), a few mid-elevation sites at Tor (where there was quite a lot of grazing and generally human
#' activity, and strangely, the two highest sites of all the survey sites, at la Molinassa (MOL). Maybe
#' these high-altitude sites are included because there was only one species recorded at each.
#'
#' Another cluster includes the higher elevation sites at Tor which range from 1900-2300 m, and two
#' mid-high elevation sites from Tavascan (1800-1900).
#'
#' One cluster has only two sites at la Molinassa (2100-2200 m) and another has the higher elevation sites
#' at Tavascan with one at 2300 at La Molinassa. This latter cluster includes sites which were grazed only
#' sparsely and for a short period during the height of the summer.
#'
#' Finally, the last cluster has a mixture of one low-elevation site from Tor, some mid-elevation sites
#' from Tavascan, which were meadows with long grass. The lower of the two was well-grazed and fairly
#' damp. There was quite a lot of human activity in this area. It also included three mid-high elevation
#' sites at La Molinassa.
#'

#' ### Test for any difference between the clusters in terms of each of the environmental variables.

#' First check for normality and constant variance to see if parametric ANOVA tests can be used.

with(env_var_matrix, {
  print(shapiro.test(resid(aov(elevational_band_m ~ as.factor(kmeans_fit_clusters)))))
  print(shapiro.test(resid(aov(sampling_effort_index ~ as.factor(kmeans_fit_clusters)))))
  print(shapiro.test(resid(aov(slope ~ as.factor(kmeans_fit_clusters)))))
  print(shapiro.test(resid(aov(mean_density ~ as.factor(kmeans_fit_clusters)))))
  print(shapiro.test(resid(aov(mean_height_75percent_cm ~ as.factor(kmeans_fit_clusters)))))
  print(shapiro.test(resid(aov(mean_perc_veg_cover ~ as.factor(kmeans_fit_clusters)))))

  print(bartlett.test(elevational_band_m, as.factor(kmeans_fit_clusters)))
  print(bartlett.test(sampling_effort_index, as.factor(kmeans_fit_clusters)))
  print(bartlett.test(slope, as.factor(kmeans_fit_clusters)))
  print(bartlett.test(mean_density, as.factor(kmeans_fit_clusters)))
  print(bartlett.test(mean_height_75percent_cm, as.factor(kmeans_fit_clusters)))
  print(bartlett.test(mean_perc_veg_cover, as.factor(kmeans_fit_clusters)))

  print(summary(aov(elevational_band_m ~ as.factor(kmeans_fit_clusters))))
  print(summary(aov(slope ~ as.factor(kmeans_fit_clusters))))
  print(summary(aov(mean_density ~ as.factor(kmeans_fit_clusters))))
  print(summary(aov(mean_perc_veg_cover ~ as.factor(kmeans_fit_clusters))))

  print(summary(lm(elevational_band_m ~ as.factor(kmeans_fit_clusters))))
  print(summary(lm(slope ~ as.factor(kmeans_fit_clusters))))
  print(summary(lm(mean_density ~ as.factor(kmeans_fit_clusters))))
  print(summary(lm(mean_perc_veg_cover ~ as.factor(kmeans_fit_clusters))))

  #' Vegetation height and sampling effort were not normally distributed so use a non-parametric
  #' Kruskal-Wallis test to test for any difference in this factor between the different clusters.
  print(kruskal.test(mean_height_75percent_cm ~ as.factor(kmeans_fit_clusters)))
  print(kruskal.test(sampling_effort_index ~ as.factor(kmeans_fit_clusters)))
})

#' None of the tests resulted in significant P-values so we were not able to reject the null hypothesis
#' that there was any difference between each of the individual environmental variables within each
#' cluster.

#' ## NMDS
#'
#' Plot the Shepard stress plot to check how many dimensions should be used. Look for where the line
#' starts to flatten.

par(mfrow=c(1,1))
dimcheckMDS(site_species_matrix, dist = "jaccard")

#' The results of this show there is still quite a change in stress between 2 and 3 dimensions so consider
#' both. Two dimensions would be preferable for visualisation and the stress is still reasonable.
#'
#' The metaMDS function automatically transforms data, runs the NMDS and checks solution robustness

set.seed(10)
species_jaccard_dist_mds_2dim <- metaMDS(site_species_matrix, k = 2, distance = "jaccard", trymax = 1000,
                                         trace = TRUE)
print("Do NMDS")

#' Get the stress value for 2 dimension

print(species_jaccard_dist_mds_2dim["stress"])

#' A Shepard plot can then be used to assess goodness of ordination fit.

par(mfrow=c(1,1))
stressplot(species_jaccard_dist_mds_2dim)

#' There is not a lot of scatter around the line but there are lots of points where the dissimilarity is
#' 1 (which is where we have lots of zeros in our matrix).

#' Calculate and plot environmental variable correlations with the axes. Display the environmental
#' variables. Note that even though they are plotted, this does not mean they are significant.
#+ message=FALSE, warning=FALSE

env_data_fit_sites <- envfit(species_jaccard_dist_mds_2dim,
                       choices = 1:2,
                       env_var_matrix[, c("elevational_band_m", "sampling_effort_index", "slope", "aspect_cardinal", "area",
                                          "mean_perc_veg_cover", "mean_height_75percent_cm",
                                          "mean_density")],
                       scaling = "sites",
                       permutations = 1000, display = "sites")
env_data_fit_sites

#' ## Plots

#' Do the same plot but for sites with the environmental variables and colour the points by the cluster
#' group
#+ message=FALSE, warning=FALSE

#' Method modified from https://www.davidzeleny.net/anadat-r/doku.php/en:ordiagrams_examples

#' Set up the env var matrix to use the row names in the plot
env_var_matrix_code <- create_short_area_code(env_var_matrix)
remove_rownames(env_var_matrix_code)
rownames(env_var_matrix_code) <- env_var_matrix_code$short_code_elevation

#' Create the output plot file
path <- "../analysis_plots/"
filepath <- file.path(path, "hypothesis3_nmds.png")
png(file = filepath, width = 2000, height = 1900, units = "px", bg = "white", res = 300)

#' Use these vectors to label the environmental variables.
list_vectors <- c("Elevation band", "Sampling effort", "Slope", "Vegetation cover",
                       "Vegetation height", "Vegetation density")
list_factors <- c("", "", "", "", "", "", "", "", "") # hacky way to avoid printing the study areas

#' Create the plot and add the environmental variables and a legend.
ordination_plot <- ordiplot(species_jaccard_dist_mds_2dim, display = "sites", type = "none",
                            xlim = c(-2.5, 2.8), ylim = c(-2.1, 1.6))
plot(env_data_fit_sites,
     col = "darkgrey", cex = 0.6,
     labels = list(vectors = list_vectors, factors = list_factors))
orditorp(ordination_plot, "sites", # I like this, it looks much better
     col = c("orange", "skyblue", "blue", "#CC79A7", "#009E73")[as.numeric(env_var_matrix_code$cluster_group)],
         air = 0.3, cex = 0.7, labels = env_var_matrix_code$short_code_elevation)
legend("topright", legend = sort(unique(env_var_matrix_code$cluster_group)), bty = "n",
            col = c("orange", "skyblue", "blue", "#CC79A7", "#009E73"), pch = 21, cex = 0.8,
title = "Cluster")

dev.off()

#' ## Permanova
#'
#' Use PERMANOVA to test if there is any differences between communities. Do this for elevation, study
#' area and the clusters. Then put all variables in.
#'
#' Null hypothesis: the Jaccard distance is equivalent for all groups, i.e. the community composition of
#' sites between the different groupings, is the same. See https://rpubs.com/an-bui/vegan-cheat-sheet

site_elevation_permanova <- adonis2(species_jaccard_dist ~ elevational_band_m,
                                    data = env_var_matrix, perm=999)
site_elevation_permanova

site_area_permanova <- adonis2(species_jaccard_dist ~ area,
                               data = env_var_matrix, perm=999)
site_area_permanova

cluster_group_permanova <- adonis2(species_jaccard_dist ~ cluster_group, data = env_var_matrix, perm=999)
cluster_group_permanova

all_env_vars_permanova <- adonis2(species_jaccard_dist ~ area + elevational_band_m +
                                  sampling_effort_index +slope + aspect_cardinal + mean_height_75percent_cm +
                                  mean_density + mean_perc_veg_cover,
                                  data = env_var_matrix, perm=999)
all_env_vars_permanova