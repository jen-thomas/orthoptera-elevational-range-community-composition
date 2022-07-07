#' ---
#' title: Conservative finalised observations
#' output:
#'   html_document:
#'     toc: true
#'     theme: yeti
#'     code_folding: hide
#' ---

#' <br>Import packages and functions from other files.
#+ message=FALSE, warning=FALSE

source("utils.R")
source("data_preparation.R")
source("orthoptera_elevation_data_exploration.R")

vector_packages <- c("visreg", "dplyr")
get_packages(vector_packages)

#' Import data

observations_file <- "../data/observations.csv"
sites_file <- "../data/sites.csv"
surveys_file <- "../data/surveys.csv"

sites_df <- read_csv_data_file(sites_file)
surveys_df <- read_csv_data_file(surveys_file)
site_survey_df <- join_site_survey(sites_df, surveys_df)

observations_sites_df <- import_all_observations(observations_file, sites_file)
confirmed_observations <- get_confirmed_observations(observations_sites_df)
confirmed_observations_species <- get_confirmed_observations_to_species(observations_sites_df)
finalised_observations <- get_finalised_observations(observations_sites_df)

#' ## Count the number of specimens that have a finalised observation

get_unique_specimens_finalised <- function(finalised_observations) {
  #' Get a dataframe of all unique specimens from the finalised observations.
  
  unique_specimens <- finalised_observations %>%
    distinct(specimen_label) %>%
    group_by(specimen_label)

  return(unique_specimens)
}

get_confirmed_obs_site <- function(confirmed_observations, site_name) {
  #' Get a dataframe of all confirmed observations from a particular site, identified by its site_name.

  confirmed_obs_site <- confirmed_observations[confirmed_observations$site_name == site_name, ]

  return(confirmed_obs_site)
}

unique_specimens_finalised <- get_unique_specimens_finalised(finalised_observations)
print(unique_specimens_finalised)

number_finalised_specimens <- n_distinct(finalised_observations$specimen_label)
print(number_finalised_specimens)

#' ## Create new record for each specimen with finalised observation
#' Each specimen needs to have one record which can be used in the function to get the number of species
#' at each site. Therefore, the finalised identifications, and those that are already identified as
#' confirmed observations for each site, will be manually checked and a decision made about how the
#' specimen should be recorded so that it adds (or not) to the species richness for a particular site, as
#' required.
#'
#' Both a conservative record and a non-conservative record will be created. The conservative record will
#' identify the specimen as though it is not contributing another taxa to those listed at the site if only
#' one of the options has already been seen at the site. If none of them have been seen, then it will be
#' added. The non-conservative one will ensure that it adds a taxa if only one of the options has been
#' seen.

#' ### BES01 20210914 H1 C007
#' Neither of these genus are currently listed at the site.
BES0120210914H1C007 <- finalised_observations[finalised_observations$specimen_label == "BES01 20210914 H1 C007", ]
confirmed_obs_bes01 <- get_confirmed_obs_site(confirmed_observations, "BES01")
BES0120210914H1C007_conservative <- BES0120210914H1C007[1, ]
BES0120210914H1C007_conservative["genus"] <- "Gomphocerus / Gomphoceridius" # adds a new taxa; cannot
# determine which
BES0120210914H1C007_notconservative <- BES0120210914H1C007[1, ]
BES0120210914H1C007_notconservative["genus"] <- "Gomphocerus / Gomphoceridius" # adds a new taxa; cannot
# determine which

finalised_identifications_conservative <- BES0120210914H1C007_conservative
finalised_identifications_notconservative <- BES0120210914H1C007_notconservative

#' ### BES02 20210720 H1 C002
#' #' Neither of these genus are currently listed at the site.
BES0220210720H1C002 <- finalised_observations[finalised_observations$specimen_label == "BES02 20210720 H1 C002", ]
confirmed_obs_bes02 <- get_confirmed_obs_site(confirmed_observations, "BES02")
BES0220210720H1C002_conservative <- BES0220210720H1C002[1, ]
BES0220210720H1C002_conservative["genus"] <- "Miramella / Podisma" # adds a new taxa; cannot
# determine which
BES0220210720H1C002_notconservative <- BES0220210720H1C002[1, ]
BES0220210720H1C002_notconservative["genus"] <- "Miramella / Podisma" # adds a new taxa; cannot
# determine which

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, BES0220210720H1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, BES0220210720H1C002_notconservative)

#' ### MOL08 20210915 H1 C002
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0820210915H1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL08 20210915 H1 C002", ]
confirmed_obs_mol08 <- get_confirmed_obs_site(confirmed_observations, "MOL08")
MOL0820210915H1C002_conservative <- MOL0820210915H1C002[1, ]
MOL0820210915H1C002_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0820210915H1C002_notconservative <- MOL0820210915H1C002[1, ]
MOL0820210915H1C002_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0820210915H1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0820210915H1C002_notconservative)

#' ### MOL08 20210915 N1 C002
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0820210915N1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL08 20210915 N1 C002", ]
MOL0820210915N1C002_conservative <- MOL0820210915N1C002[1, ]
MOL0820210915N1C002_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0820210915N1C002_notconservative <- MOL0820210915N1C002[1, ]
MOL0820210915N1C002_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0820210915N1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0820210915N1C002_notconservative)

#' ### MOL09 20210915 H1 C014
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0920210915H1C014 <- finalised_observations[finalised_observations$specimen_label == "MOL09 20210915 H1 C014", ]
confirmed_obs_mol09 <- get_confirmed_obs_site(confirmed_observations, "MOL09")
MOL0920210915H1C014_conservative <- MOL0920210915H1C014[1, ]
MOL0920210915H1C014_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0920210915H1C014_notconservative <- MOL0920210915H1C014[1, ]
MOL0920210915H1C014_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0920210915H1C014_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0920210915H1C014_notconservative)

#' ### MOL09 20210915 N1 C005
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0920210915N1C005 <- finalised_observations[finalised_observations$specimen_label == "MOL09 20210915 N1 C005", ]
MOL0920210915N1C005_conservative <- MOL0920210915N1C005[1, ]
MOL0920210915N1C005_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0920210915N1C005_notconservative <- MOL0920210915N1C005[1, ]
MOL0920210915N1C005_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0920210915N1C005_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0920210915N1C005_notconservative)

#' MOL09 20211002 H1 C001
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0920211002H1C001 <- finalised_observations[finalised_observations$specimen_label == "MOL09 20211002 H1 C001", ]
MOL0920211002H1C001_conservative <- MOL0920211002H1C001[1, ]
MOL0920211002H1C001_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0920211002H1C001_notconservative <- MOL0920211002H1C001[1, ]
MOL0920211002H1C001_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0920211002H1C001_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0920211002H1C001_notconservative)

#' ### MOL09 20210915 N1 C002
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0920210915N1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL09 20210915 N1 C002", ]
MOL0920210915N1C002_conservative <- MOL0920210915N1C002[1, ]
MOL0920210915N1C002_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0920210915N1C002_notconservative <- MOL0920210915N1C002[1, ]
MOL0920210915N1C002_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0920210915N1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0920210915N1C002_notconservative)

#' ### MOL09 20211002 H1 C002
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
MOL0920211002H1C002 <- finalised_observations[finalised_observations$specimen_label == "MOL09 20211002 H1 C002", ]
MOL0920211002H1C002_conservative <- MOL0920211002H1C002[1, ]
MOL0920211002H1C002_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
MOL0920211002H1C002_notconservative <- MOL0920211002H1C002[1, ]
MOL0920211002H1C002_notconservative["species"] <- "Chorthippus mollis" # could be a different species
# though, so add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, MOL0920211002H1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, MOL0920211002H1C002_notconservative)

#' ### TAV07 20210618 N1 C029
#' One of the genus (Gomphocerus) has been seen at the site.
TAV0720210618N1C029 <- finalised_observations[finalised_observations$specimen_label == "TAV07 20210618 N1 C029", ]
confirmed_obs_tav07 <- get_confirmed_obs_site(confirmed_observations, "TAV07")
TAV0720210618N1C029_conservative <- TAV0720210618N1C029[1, ]
TAV0720210618N1C029_conservative["genus"] <- "Gomphocerus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0720210618N1C029_notconservative <- TAV0720210618N1C029[1, ]
TAV0720210618N1C029_notconservative["genus"] <- "Gomphoceridius" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0720210618N1C029_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0720210618N1C029_notconservative)

#' ### TAV08 20210916 H1 C010
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TAV0820210916H1C010 <- finalised_observations[finalised_observations$specimen_label == "TAV08 20210916 H1 C010", ]
confirmed_obs_tav08 <- get_confirmed_obs_site(confirmed_observations, "TAV08")
TAV0820210916H1C010_conservative <- TAV0820210916H1C010[1, ]
TAV0820210916H1C010_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0820210916H1C010_notconservative <- TAV0820210916H1C010[1, ]
TAV0820210916H1C010_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0820210916H1C010_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0820210916H1C010_notconservative)

#' ### TAV08 20210916 H1 C006
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TAV0820210916H1C006 <- finalised_observations[finalised_observations$specimen_label == "TAV08 20210916 H1 C006", ]
TAV0820210916H1C006_conservative <- TAV0820210916H1C006[1, ]
TAV0820210916H1C006_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0820210916H1C006_notconservative <- TAV0820210916H1C006[1, ]
TAV0820210916H1C006_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0820210916H1C006_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0820210916H1C006_notconservative)

#' ### TAV08 20210916 H1 C008
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TAV0820210916H1C008 <- finalised_observations[finalised_observations$specimen_label == "TAV08 20210916 H1 C008", ]
TAV0820210916H1C008_conservative <- TAV0820210916H1C008[1, ]
TAV0820210916H1C008_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0820210916H1C008_notconservative <- TAV0820210916H1C008[1, ]
TAV0820210916H1C008_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0820210916H1C008_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0820210916H1C008_notconservative)

#' ### TAV08 20211006 H1 C007
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TAV0820211006H1C007 <- finalised_observations[finalised_observations$specimen_label == "TAV08 20211006 H1 C007", ]
TAV0820211006H1C007_conservative <- TAV0820211006H1C007[1, ]
TAV0820211006H1C007_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0820211006H1C007_notconservative <- TAV0820211006H1C007[1, ]
TAV0820211006H1C007_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0820211006H1C007_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0820211006H1C007_notconservative)

#' ### TAV08 20210916 H1 C012
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TAV0820210916H1C012 <- finalised_observations[finalised_observations$specimen_label == "TAV08 20210916 H1 C012", ]
TAV0820210916H1C012_conservative <- TAV0820210916H1C012[1, ]
TAV0820210916H1C012_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TAV0820210916H1C012_notconservative <- TAV0820210916H1C012[1, ]
TAV0820210916H1C012_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0820210916H1C012_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0820210916H1C012_notconservative)

#' ### TAV09 20211006 N1 C008
#' Neither of these species have been seen at this site before.
TAV0920211006N1C008 <- finalised_observations[finalised_observations$specimen_label == "TAV09 20211006 N1 C008", ]
TAV0920211006N1C008_conservative <- TAV0920211006N1C008[1, ]
TAV0920211006N1C008_conservative["species"] <- "Chorthippus biguttulus / Chorthippus mollis" # add new
# taxa which is definitely different
TAV0920211006N1C008_notconservative <- TAV0920211006N1C008[1, ]
TAV0920211006N1C008_notconservative["species"] <- "Chorthippus biguttulus / Chorthippus mollis" # add new
# taxa which is definitely different

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TAV0920211006N1C008_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TAV0920211006N1C008_notconservative)

#' ### TOR01 20210629 N1 E007
#' One of these genus (Tettogonia) has already been seen at this site
TOR0120210629N1E007 <- finalised_observations[finalised_observations$specimen_label == "TOR01 20210629 N1 E007", ]
confirmed_obs_tor01 <- get_confirmed_obs_site(confirmed_observations, "TOR01")
TOR0120210629N1E007_conservative <- TOR0120210629N1E007[1, ]
TOR0120210629N1E007_conservative["genus"] <- "Tettigonia" # don't add another taxa because it could have
#been the one that has already been seen there
TOR0120210629N1E007_notconservative <- TOR0120210629N1E007[1, ]
TOR0120210629N1E007_notconservative["genus"] <- "Conocephalus" # add the different taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0120210629N1E007_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0120210629N1E007_notconservative)

#' ### TOR02 20210629 N1 E001
#' Both of these genus have already been seen at this site so it won't add any new taxa
TOR0220210629N1E001 <- finalised_observations[finalised_observations$specimen_label == "TOR02 20210629 N1 E001", ]
confirmed_obs_tor02 <- get_confirmed_obs_site(confirmed_observations, "TOR02")
TOR0220210629N1E001_conservative <- TOR0220210629N1E001[1, ]
TOR0220210629N1E001_conservative["genus"] <- "" # cannot select just one genus because it could have been
# either
TOR0220210629N1E001_notconservative <- TOR0220210629N1E001[1, ]
TOR0220210629N1E001_notconservative["genus"] <- "" # cannot select just one genus because it could have
# been either

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0220210629N1E001_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0220210629N1E001_notconservative)

#' ### TOR02 20210919 N1 C005
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TOR0220210919N1C005 <- finalised_observations[finalised_observations$specimen_label == "TOR02 20210919 N1 C005", ]
TOR0220210919N1C005_conservative <- TOR0220210919N1C005[1, ]
TOR0220210919N1C005_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TOR0220210919N1C005_notconservative <- TOR0220210919N1C005[1, ]
TOR0220210919N1C005_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0220210919N1C005_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0220210919N1C005_notconservative)

#' ### TOR02 20210919 N1 C001
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TOR0220210919N1C001 <- finalised_observations[finalised_observations$specimen_label == "TOR02 20210919 N1 C001", ]
TOR0220210919N1C001_conservative <- TOR0220210919N1C001[1, ]
TOR0220210919N1C001_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TOR0220210919N1C001_notconservative <- TOR0220210919N1C001[1, ]
TOR0220210919N1C001_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0220210919N1C001_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0220210919N1C001_notconservative)

#' ### TOR02 20211004 H1 C001
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TOR0220211004H1C001 <- finalised_observations[finalised_observations$specimen_label == "TOR02 20211004 H1 C001", ]
TOR0220211004H1C001_conservative <- TOR0220211004H1C001[1, ]
TOR0220211004H1C001_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TOR0220211004H1C001_notconservative <- TOR0220211004H1C001[1, ]
TOR0220211004H1C001_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0220211004H1C001_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0220211004H1C001_notconservative)

#' ### TOR05 20210919 H1 C008
#' Both species have already been seen at this site so it will not add any taxa
TOR0520210919H1C008 <- finalised_observations[finalised_observations$specimen_label == "TOR05 20210919 H1 C008", ]
confirmed_obs_tor05 <- get_confirmed_obs_site(confirmed_observations, "TOR05")
TOR0520210919H1C008_conservative <- TOR0520210919H1C008[1, ]
TOR0520210919H1C008_conservative["species"] <- ""
TOR0520210919H1C008_notconservative <- TOR0520210919H1C008[1, ]
TOR0520210919H1C008_notconservative["species"] <- ""

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0520210919H1C008_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0520210919H1C008_notconservative)

#' ### TOR09 20210616 N1 C004
#' Chorthippus  have already been seen at this site
TOR0920210616N1C004 <- finalised_observations[finalised_observations$specimen_label == "TOR09 20210616 N1 C004", ]
confirmed_obs_tor09 <- get_confirmed_obs_site(confirmed_observations, "TOR09")
TOR0920210616N1C004_conservative <- TOR0920210616N1C004[1, ]
TOR0920210616N1C004_conservative["genus"] <- "Chorthippus" # keep the same taxa
TOR0920210616N1C004_notconservative <- TOR0920210616N1C004[1, ]
TOR0920210616N1C004_notconservative["genus"] <- "Omocestus" # could be a different taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR0920210616N1C004_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR0920210616N1C004_notconservative)

#' ### TOR10 20211004 H1 C002
#' Chorthippus biguttulus (one of the options) has already been seen at this site.
TOR1020211004H1C002 <- finalised_observations[finalised_observations$specimen_label == "TOR10 20211004 H1 C002", ]
confirmed_obs_tor10 <- get_confirmed_obs_site(confirmed_observations, "TOR10")
TOR1020211004H1C002_conservative <- TOR1020211004H1C002[1, ]
TOR1020211004H1C002_conservative["species"] <- "Chorthippus biguttulus" # don't add another taxa, because it could be one that
# has already been seen there
TOR1020211004H1C002_notconservative <- TOR1020211004H1C002[1, ]
TOR1020211004H1C002_notconservative["species"] <- "Chorthippus mollis" # could be a different genus though, so
# add a taxa

finalised_identifications_conservative <- rbind(finalised_identifications_conservative, TOR1020211004H1C002_conservative)
finalised_identifications_notconservative <- rbind(finalised_identifications_notconservative, TOR1020211004H1C002_notconservative)

#' ## Checks on finalised observation data
#'
#' ### Count the number of rows in the finalised set of identifications
number_conservative_specimens <- n_distinct(finalised_identifications_conservative$specimen_label)
paste("Conservative: ", number_conservative_specimens)
number_notconservative_specimens <- n_distinct(finalised_identifications_notconservative$specimen_label)
paste("Not conservative: ", number_notconservative_specimens)

#' ### Check that all the specimens have a row in the new dataframe


#' ### Count that there are the same number of specimens as there were to begin with and none are repeated


#' Calculate the species richness (conservative) including the finalised observations
