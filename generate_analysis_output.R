#renv::activate()
renv::load(".")
getwd()
rmarkdown::metadata
rmarkdown::render("analysis_scripts/orthoptera_elevation_data_exploration.R", 
                  output_dir = "analysis_outputs/")
getwd()

