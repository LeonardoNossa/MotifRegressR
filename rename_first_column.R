#' Rename the First Column of a Data Frame
#'
#' This function renames the values in the first column of a data frame by 
#' extracting the portion after the first period (`.`).
#'
#' @param dataframe A data frame where the first column (`V1`) has to be 
#' renamed.
#'
#' @return A data frame with the first column (`V1`) updated to include only 
#' the part of each value after the first period (`.`).
#'
#' @export
rename_first_column <- function(dataframe) {
  dataframe$V1 <- lapply(strsplit(dataframe$V1, "\\."), function(x) x[2])
  return(dataframe)
}
