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

replace_na_with_zero <- function(dataframe, column) {
  #' Replace all NA values in a column with 0 (to be able to do calculations).
  #'
  #' Return dataframe.

  dataframe[[column]] <- replace(dataframe[[column]], is.na(dataframe[[column]]),0)

  return(dataframe)
}

format_theme_ggplot <- function(plot_with_ggplot) {
  #' Add the following format to the theme of a plot made with ggplot.
  #'
  #' Return the plot.

  formatted_plot <- plot_with_ggplot +
                    theme_bw() +
                    theme(axis.text = element_text(size = 8),
                          axis.title = element_text(size = 10),
                          legend.position = "none",
                          axis.line = element_line(),
                          panel.grid = element_blank(),
                          panel.border = element_blank())

  return(formatted_plot)
}

format_theme_ggplot_vertical_xaxis_labels <- function(plot_with_ggplot) {
  #' Add the following format to the theme of a plot made with ggplot.
  #'
  #' Return the plot.

  formatted_plot <- plot_with_ggplot +
                    theme_bw() +
                    theme(axis.text.x = element_text(size = 5, face = "italic", hjust = 1, vjust = 1, angle = 90),
                          axis.text.y = element_text(size = 8),
                          axis.title = element_text(size = 10),
                          legend.position = "none",
                          axis.line = element_line(),
                          panel.grid = element_blank(),
                          panel.border = element_blank())

  return(formatted_plot)
}

save_plot <- function(plot_name, filename) {
  #' Save a specified ggplot to a specific filename.
  #'
  #' This function will provide the filepath and concatenate it with the filename.

  filepath <- "../analysis_plots/"

  ggsave(filename, plot_name, path = filepath, device = "png",
         scale = 1, width = 1000, height = 1000, units = "px", dpi = 300,
         bg = NULL)
}