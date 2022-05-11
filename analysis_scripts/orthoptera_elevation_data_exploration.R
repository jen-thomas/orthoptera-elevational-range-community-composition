#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---
setwd("analysis_scripts/")
# Latest Git commits and updates

latest_commit <- system("git show -s --pretty='%h on %ci' HEAD", intern=TRUE)

#' The latest change to this project was **commit **
{{ latest_commit }}

this_file_latest_commits <- system("git log --pretty='tformat:- %h %s on %ci' orthoptera_elevation_data_exploration.R", intern=TRUE)
this_file_latest_commits <- paste("<ul>", this_file_latest_commits, "</ul>")

#' ## Change log
{{ this_file_latest_commits }}

# Read in data files
getwd()
observations <- read.csv("../data/observations.csv", header = TRUE, stringsAsFactors = TRUE)
sites <- read.csv("../data/sites.csv", header = TRUE, stringsAsFactors = TRUE)
surveys <- read.csv("../data/surveys.csv", header = TRUE, stringsAsFactors = TRUE)
vegetation_plots <- read.csv("../data/vegetation_plots.csv", header = TRUE, stringsAsFactors = TRUE)



