# Investigating the relationship of elevational range and Orthoptera species richness along an elevational gradient in the Pyrenees. 

This repository contains the code to do the analysis for this project. The analysis for the associated publication was done in `R` (version 4.2.2) using Debian (bookworm).

## Project structure

The code structure is a balance between trying to use functions to keep the code organised, and putting it together in a logical order so that when looking at the HTML files, there is no need to jump between them for the analysis of each hypothesis. 

### Analysis scripts

The directory `analysis_scripts` contains the R code to manipulate the data and do the analysis needed for each hypothesis.  

Contents of `analysis_scripts`: 
* `data_preparation.R`: various data manipulation functions which prepares the data for analysis.
* `get_finalised_observations_species_richness_conservative.R`: read the details about finalised observations in the file. This code creates the set of observations that are used for the rest of the analysis.
* `get_physical_site_data.R`: digital elevation models were used to get data for topography parameters at each of the study sites.
* `get_temperature_data.sh`: temperature data from automatic weather stations in Catalunya (XEMA) and Andorra (Meteo Andorra) were used to compile summary temperature data to describe the conditions at different elevations. This data was used in the manuscript introduction, rather than in the analysis.
* `orthoptera elevation_data_exploration.R`: initial investigations of the data to summarise visits to sites, number of surveys and number of taxa found.
* `orthoptera_elevational_range_hypothesis2.R`: nonlinear regressions used to analyse the relationship between elevational range over an elevational gradient (Rapoport's elevational rule).
* `orthoptera_species_richness_hypothesis1.R`: generalised linear models used to find the important predictors of species richness along the elevational gradient.
* `prepare_pyrenees_temperature_data.py`: temperature data were summarised at different elevations.
* `prepare_vegetation_data.R`: vegetation structure was characterised along each transect. This code calculates the mean of each parameter for each transect.
* `utils.R`: some generic bits of code that can be used throughout the analysis.

### Data

The directory `data` was used as the source of all of the data files that are used in the analysis. The data created as part of this project is now held in another [repository](https://github.com/jen-thomas/orthoptera-elevational-range-community-composition-observation-data), and data produced by other sources (digital elevation models and temperature) is not made available here for licensing and practical (data set size) reasons. The published (and citable) version is available at [https://doi.org/zenodo.7763502](https://doi.org/zenodo.7763502).

#### Digital elevation model data

The data required to reproduce this analysis can be obtained from the Institut Cartogràfic i Geològic de Catalunya at http://www.icc.cat/appdownloads/index.html?c=dlfxmde2m (data were downloaded in June 2022). Specific locations for each of the transects can be found in the `metadata` directory within this repository. Data should be downloaded to include each of the transects at a site, with a buffer of around 100m. The code uses the data within +/- 2 raster cells along the transect, where each cell measures 2x2m. 

#### Temperature data

Temperature data were downloaded from two different organisations who have automatic weather stations in the area of the surveys. Data from la Xarxa de Estacions Meteorològiques Automàtiques (XEMA) were downloaded from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Dades-meteorol-giques-de-la-XEMA/nzvn-apee/data in June 2023. All data were downloaded, then a `grep` command in a bash script was used to filter the data for 2021, from three sites: Tírvia (990 m), Salòria (2451 m) and Certascan (2400 m). Data from an intermediate elevation was downloaded from the Servei Meteorològic Nacional d'Andorra at https://www.meteo.ad/climatologia. Data from 2021 was downloaded for meteorological stations at Setúria (1900 m) and les Bordes de Setúria (1910 m) in June 2023. Data have not been made available here due to the size of the dataset.

## Running the code

A virtual environment, `renv`, encapsulates the packages and set-up used to run the R code. Package details are listed in `renv.lock`. The virtual environment should be activated before running the code.

For practical reasons, data can be found in a separate [repository](https://github.com/jen-thomas/orthoptera-elevational-range-community-composition-observation-data/tree/main/data). To run this code, it would make sense to copy the required files to the `data` directory. These are: 
* [digital elevation model data](#digital-elevation-model-data)
* the four raw CSV files (`observations.csv`, `sites.csv`, `surveys.csv` and `vegetation_plots.csv`) from the [data repository](https://github.com/jen-thomas/orthoptera-elevational-range-community-composition-observation-data/tree/main/data)

Running the script, `generate_analysis_output.R`, will create HTML files from each of the individual files. This provides a more human-friendly view of the comments, code and output. Output files (HTML and plot files) are not included in this repository because they can be generated by running the code.

Alternatively, each of the `R` scripts can be run individually.

The virtual environment, `venv`, should be activated before running the Python code. The packages listed in `requirements.txt` should be installed. This code is only required to summarise the temperature data. Before running the code, the data should be downloaded from the repositories described above, for the meteorological stations that are required. Paths in the code will need to be adjusted accordingly. The then bash script should be used to reduce the size of the data that are analysed in Python. 