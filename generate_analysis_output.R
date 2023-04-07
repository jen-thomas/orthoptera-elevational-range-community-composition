renv::activate()
renv::load(".")
rmarkdown::metadata
# rmarkdown::render("analysis_scripts/show_latest_commits.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/data_preparation.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/prepare_vegetation_data.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/get_physical_site_data.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/get_finalised_observations_species_richness_conservative.R",
#                   output_dir = "analysis_outputs/")
rmarkdown::render("analysis_scripts/orthoptera_elevation_data_exploration.R",
                  output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/orthoptera_species_richness_hypothesis1.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/orthoptera_elevational_range_hypothesis2.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/orthoptera_community_composition_hypothesis3.R",
#                   output_dir = "analysis_outputs/")
# rmarkdown::render("analysis_scripts/test_claridge_singhrao_1978_data.R",
#                   output_dir = "analysis_outputs/")