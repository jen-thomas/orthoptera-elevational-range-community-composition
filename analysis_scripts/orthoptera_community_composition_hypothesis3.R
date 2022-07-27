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
                     "maptools", "leaflet", "rgeos", "corrplot", "vegan", "goeveg", "phytools")
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

    #' set row names to be the site_elevation column
  rownames(env_var_df) <- env_var_df$site_elevation

    #' select only a subset of the parameters to use in the analysis
  env_var_matrix <- dplyr::select(env_var_df, elevational_band_m, slope, aspect, area,
                                  mean_perc_veg_cover, mean_height_75percent, mean_density)

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

unique_taxa_sites <- get_unique_taxa_site(all_observations_conservative)

#' Create and preview the presence-absence site-species matrix. Site name is in the format
#' elevation(m)_site where the name is an abbreviation of the study area.
#'
#' This matrix includes all unique taxa from each site (as was used for the species richness analysis).
#' This means though, that a higher taxonomic level, such as genus, may be represented elsewhere in the
#' matrix by a different taxa. The genus though in this example, would be a unique taxa from the
#' particular sites where it was observed. **TODO**: consider if these should be removed from the analysis
#' if they are not a unique taxa for the whole project, rather than just for a site.

site_species_matrix <- create_presence_absence_site_species_matrix(unique_taxa_sites)
site_species_matrix

#' ## Environmental variables
#'
#' Environmental variables will be checked for collinearity, then the quantitative variables describing
#' the vegetation (average vegetation height, maximum vegetation height and vegetation density) will be
#' normalised (**TODO**) to have a mean of 0 and standard deviation of 1. Site aspect will be converted to
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

#' ### Check for collinearity between environmental variables

check_collinearity(site_env_var_data)

#' Looking at the previous plots we can see that there seems to be some collinearity between the two
#' measures of vegetation height. Given that the maximum vegetation height can just represent one piece of
#' vegetation within the plots, the mean 75% height of vegetation in the plot is a more suitable measure
#' of the vegetation across the plots at a site. As expected, the compositional data representing three
#' measures of ground cover in terms of percentages, are also correlated. The vegetation ground cover will
#' be used in this analysis.
#'
#' Prepare the matrix of environmental variables to be used in the next part of the analysis. Only include
#' site elevation, aspect, slope, vegetation height, vegetation ground cover and vegetation density.

env_var_matrix <- create_env_var_matrix(site_env_var_data)

#' ## Hierarchical cluster analysis

#' Calculate a dissimilarity matrix using the Jaccard distance. This is used because it excludes double
#' zeros whilst giving equal weight to non-zero agreements and disagreements. Data are already binary so
#' there is no need to convert the matrix to presence-absence before calculating the matrix.

species_jaccard_dist <- vegdist(site_species_matrix, method = "jaccard")

#' Cluster communities using the average-linkage algorithm. This is a standard method to use in ecology
#' and particularly in biogeography. It "links sites and species by considering the distances to a
#' subgroup's arithmetic average" (https://ourcodingclub.github.io/tutorials/data-clustering/).

species_jaccard_dist_cluster_average <- hclust(species_jaccard_dist, method = "average")

#' Do some kind of bootstrapping (this doesn't seem to be used for presence-absence data) or resampling
#' to assess the stability of the clustering solution.

#' Plot cluster diagram

plot(species_jaccard_dist_cluster_average, ylab = "Jaccard distance", main = "Average")

#' Split into the subgroups using the nodes

species_jaccard_dist_cluster_average_groups <- cutree(species_jaccard_dist_cluster_average, k = 4)
species_jaccard_dist_cluster_average_groups

plot(x = species_jaccard_dist_cluster_average, labels =  row.names(species_jaccard_dist_cluster_average), cex = 0.5)
rect.hclust(tree = species_jaccard_dist_cluster_average, k = 4, which = 1:4, border = 1:4, cluster = species_jaccard_dist_cluster_average_groups)

sites_df$cluster_group <- species_jaccard_dist_cluster_average_groups[sites_df$site_elevation]


map(xlim = c(1.2, 1.4), ylim = c(42.5, 42.75))  # setting the lat and long limits on our map
map.axes()
points(sites_df$longitude_start_e, sites_df$latitude_start_n, pch = 100, col = sites_df$group)

#' The metaMDS function automatically transforms data, runs the NMDS and checks solution robustness

set.seed(10)
species_jaccard_dist_mds <- metaMDS(site_species_matrix, distance = "jaccard", trymax = 1000, trace = TRUE)

#' A stress plot can then be used to assess goodness of ordination fit.
#'
par(mfrow=c(1,1))
stressplot(species_jaccard_dist_mds)

#' As we have a
par(mfrow=c(1,1))
dimcheckMDS(site_species_matrix, dist = "jaccard")

#plot site scores as text
ordiplot(species_jaccard_dist_mds, display = "sites", type = "text")

#use automated plotting of results to try and eliminate overlapping labels
#this may take a while to run

#ordipointlabel(species_jaccard_dist_mds)

#the previous plot isn’t easy to understand but ordination plots are highly customizable
#set up the plotting area but don't plot anything yet

mds_fig <- ordiplot(species_jaccard_dist_mds, type = "none")
#plot just the samples
#colour by habitat
#pch=19 means plot a circle
# points(mds_fig, "sites", pch = 19, col = "green", select = site_species_matrix[str_detect(site_species_matrix$site, "TAV"), ])
# points(mds_fig, "sites", pch = 19, col = "blue", select = site_species_matrix[str_detect(site_species_matrix$site, "TOR"), ])
# points(mds_fig, "sites", pch = 19, col = "red", select = site_species_matrix[str_detect(site_species_matrix$site, "MOL"), ])

points(mds_fig, "sites", pch = 19, col = "green", select = env_var_matrix$area == "Tor")
points(mds_fig, "sites", pch = 19, col = "blue", select = env_var_matrix$area == "Tavascan")
points(mds_fig, "sites", pch = 19, col = "red", select = env_var_matrix$area == "La Molinassa")
points(mds_fig, "sites", pch = 19, col = "black", select = env_var_matrix$area == "Besan")
points(mds_fig, "sites", pch = 19, col = "black", select = env_var_matrix$area == "Bordes de Viros")

# add confidence ellipses around habitat types
ordiellipse(species_jaccard_dist_mds, env_var_matrix$area, conf = 0.95, label = TRUE)
# overlay the cluster results we calculated earlier
ordicluster(species_jaccard_dist_mds, species_jaccard_dist_cluster_average, col = "gray")

#calculate and plot environmental variable correlations with the axes

env_data_fit <- envfit(species_jaccard_dist_mds,
                       choices = 1:2,
                       env_var_matrix[, c("elevational_band_m", "slope", "aspect", "mean_perc_veg_cover", "mean_height_75percent", "mean_density")],
                       scaling = "sites",
                       permutations = 1000)
env_data_fit
plot(env_data_fit)

#ordisurf(species_jaccard_dist_mds ~ elevational_band_m, site_species_matrix, isotropic = TRUE, main = NULL, cex = 3)

#' The lengths of the arrows represent the strength of the correlation of the environmental variable with
#' the axis.


#'
#' ## Questions
#' ### Environmental variables
#' <ol>
#' <li>I was considering also calculating a radiation index from the aspect, slope and elevation
#' parameters. It could perhaps be used instead of slope and / or aspect, given that it could affect the
#' microclimate.</li>
#' <li>I converted aspect (degs) to N, S, E, W taking each point as a 90-degree segment, e.g 45-135degs is
#' East. Given the orientation of the Pyrenees and the climate there, the difference between a northerly
#' and southerly aspect seems to be the most important, but I wanted to capture those sites that do not
#' face directly to the North or South. Whilst I treated them all as 90-deg segments, another paper used
#' a smaller segment of E/W +/- 20 degs (or similar).</li>
#' </ol>
#' ### Ordination
#' Some questions copied from above: https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_community_composition_hypothesis3.html#points-to-think-about-for-this-part-of-the-analysis
#' <ol>
#' <li>I am not sure I am using the correct methods for each part of the ordination. See the sections
#' above.</li>
#' <li>Is scale=TRUE still needed if the presence-absence data are always either 0 or 1? I think this
#' might be the case because the Hellinger transformation has been done.</li>
#' <li>Should we use SCALE = TRUE (correlation matrix) or SCALE = FALSE (covariance matrix). TRUE
#' would best represent the chi-squared differences between the species. FALSE would best represent the
#' chi-squared difference between the sites. This is also somehow represented in the scaling parameter.
#' Symmetric shows the least distorion between sites and species.</li>
#' <li>Should the Ochiai transformation be used instead of Hellinger (because the data are binary
#' presence-absence)? See https://peerj.com/articles/9777/#p-1</li>
#' <li>According to the same article https://peerj.com/articles/9777/#p-1, should a GLM be used
#' (likely a binomial GLM with a logit link function) and AIC model selection, instead of ordination? In
#' this example, GLMs outperformed RDA when considering two different possibilities for absences (1 -
#' species not present; 2 - poor sampling, species not found). GLMs always selected the correct
#' environmental variables.</li>
#' <li>For this same part, is the arch effect in this plot important? See https://falciot.net/orthoptera-94940/analysis_outputs/orthoptera_community_composition_hypothesis3.html#first-attempt</li>
#' <li>Do I understand correctly, that the scale = TRUE parameter in the CCA would standardise the
#' environmental variables, giving them a mean of 0 and variance of 1?</li>
#' </ol>