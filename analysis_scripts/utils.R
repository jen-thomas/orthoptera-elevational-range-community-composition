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

replace_na_with_zero <- function(dataframe, column) {
  #' Replace all NA values in a column with 0 (to be able to do calculations).
  #'
  #' Return dataframe.

  dataframe[[column]] <- replace(dataframe[[column]], is.na(dataframe[[column]]),0)

  return(dataframe)
}

format_theme_ggplot_vertical_xaxis_labels <- function(plot_with_ggplot) {
  #' Add the following format to the theme of a plot made with ggplot.
  #'
  #' Return the plot.

  formatted_plot <- plot_with_ggplot +
                    theme_bw() +
                    theme(axis.text.y = element_text(size = 8, face = "italic", colour = "black"),
                          axis.text.x = element_text(size = 9),
                          axis.title = element_text(size = 10),
                          legend.position = "none",
                          axis.line = element_line(),
                          panel.grid = element_blank(),
                          panel.border = element_blank())

  return(formatted_plot)
}