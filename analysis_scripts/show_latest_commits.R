#' ---
#' title: Git commits to project
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#'     df_print: paged
#' ---

#' <br>Import packages functions from other files.

source("utils.R")

#' ## Change log
#' Display the latest five commits to the files in this project.

filenames <- c("data_preparation.R", "orthoptera_elevation_data_exploration.R", "orthoptera_species_richness_hypothesis1.R", "orthoptera_elevational_range_hypothesis2.R")

for (filename in filenames) {
  print(paste("********", filename, "*******"))
  print_latest_git_commits(filename)
}