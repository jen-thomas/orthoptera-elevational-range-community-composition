print_latest_git_commits <- function(file_path) {
    #' Get and print the latest five commits for a file.

  command <- paste("git log HEAD --pretty='tformat: %ci commit %h: %s'", file_path)
  this_file_latest_commits <- system(command, intern = TRUE)
  print(this_file_latest_commits[1:5])
}