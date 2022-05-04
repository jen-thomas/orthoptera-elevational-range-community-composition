#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

# Latest Git commits and updates
setwd <- 'orthoptera-elevational-range-community-composition' # relative path

latest_commit <- system("git show -s --pretty='%h on %ci' HEAD", intern=TRUE)

#' The latest change to this project was **commit **
{{ latest_commit }}

system("pwd")


this_file_latest_commits <- system("git log --pretty='tformat:%h <<%s>> on %ci<br />' orthoptera_elevation_data_exploration.R  | tr -d '\n'", intern=TRUE)

#' ## Change log
{{ this_file_latest_commits }}

#' Test output as HTML

print('hello')

# Project set-up


# Read in data files


