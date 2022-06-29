print_latest_git_commits <- function(file_path) {
    #' Get and print the latest five commits for a file.

  command <- paste("git log HEAD --pretty='tformat: %ci commit %h: %s'", file_path)
  this_file_latest_commits <- system(command, intern = TRUE)
  print(this_file_latest_commits[1:5])
}

get_packages <- function(vector_packages) {
    #' Install and load packages from a vector of packages that are needed for the code.

  for (required_package in vector_packages) {
    if (!require(required_package, character.only = TRUE, quietly = TRUE))
      install.packages(required_package)
    library(required_package, character.only = TRUE)
  }
}

read_csv_data_file <- function(file_path) {
    #' Read in a CSV data file using the first line as the header. Strings in different columns can be
    #' set as factors in the resulting dataframe.
    #'
    #' Return a dataframe of the input data.

  read.csv(file_path, header = TRUE, stringsAsFactors = TRUE)
}

subset_data_frame <- function(dataframe, vector_columns) {
  #' Get a subset of a dataframe according to a vector of column names.
  #'
  #' Return the subsetted dataframe.

  subsetted_df <- dataframe[, vector_columns]

  return(subsetted_df)
}