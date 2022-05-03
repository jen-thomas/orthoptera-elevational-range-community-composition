#' ---
#' title: Orthoptera species richness, elevational range and community composition
#' subtitle: Data exploration
#' output: 
#'   html_document:
#'     toc: false
#'     theme: yeti
#' ---

# Latest Git commit
system("ls")
setwd <- 'orthoptera-elevational-range-community-composition'
latest_commit <- system("git show -s --pretty='%H on %ci' HEAD", intern=TRUE)
print(latest_commit)

this_file_latest_commits <- system("git log --pretty='format:%h <<%s>> on %ci' analysis_scripts/orthoptera_elevation_data_exploration.R", intern=TRUE)
print(this_file_latest_commits)

# Test output as HTML

print('hello')