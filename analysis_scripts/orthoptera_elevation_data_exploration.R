#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' subtitle: Data exploration
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#'     code_folding: hide
#' ---

# Latest Git commits and updates
system("ls")
setwd <- 'orthoptera-elevational-range-community-composition'
latest_commit <- system("git show -s --pretty='%h on %ci' HEAD", intern=TRUE)

#' The latest change to this project was **commit **
{{ latest_commit }}

this_file_latest_commits <- system("git log --pretty='tformat:%h <<%s>> on %ci %n' orthoptera_elevation_data_exploration.R", intern=TRUE)

#' The most recent changes to this file were
{{ this_file_latest_commits }}
# TODO improve the formatting of this output

#' Test output as HTML

print('hello')