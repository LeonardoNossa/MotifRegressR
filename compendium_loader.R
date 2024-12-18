#' Load Data from a Compendium File
#'
#' This function loads data from a specified file path and returns it as a data
#' frame. It uses `read.table()` to read the file, assuming a comma-separated 
#' values (CSV) format.
#'
#' @param path A string specifying the path to the file to be loaded.
#' @param header A logical value indicating whether the first row of the file 
#' contains column headers. Defaults to `TRUE`.
#' @param row.names A vector or column index specifying the row names for the 
#' data frame. 
#'
#' @return A data frame containing the data read from the specified file.
#'
#' @export
compendium_loader <- function(path, header = TRUE, row.names){
  data <- utils::read.table(file = path,
                            sep = ",",
                            header = header,
                            row.names = row.names)
  return(data)
}

