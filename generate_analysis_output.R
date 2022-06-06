renv::activate()
renv::load(".")
rmarkdown::metadata
rmarkdown::render("analysis_scripts/data_preparation.R",
                  output_dir = "analysis_outputs/")
rmarkdown::render("analysis_scripts/orthoptera_elevation_data_exploration.R",
                  output_dir = "analysis_outputs/")
rmarkdown::render("analysis_scripts/orthoptera_species_richness_hypothesis1.R",
                  output_dir = "analysis_outputs/")
rmarkdown::render("analysis_scripts/orthoptera_elevational_range_hypothesis2.R",
                  output_dir = "analysis_outputs/")

