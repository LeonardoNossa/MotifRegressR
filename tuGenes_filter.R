#' Filter the `X5.tuGenes` Column of a Data Frame
#'
#' This function processes the `X5.tuGenes` column of a data frame by 
#' extracting the portion before the first semicolon (`;`).
#' If the column does not exist, the function stops with an error.
#'
#' @param x A data frame containing a column named `X5.tuGenes`.
#'
#' @return A data frame with the `X5.tuGenes` column updated to include only 
#' the portion before the first semicolon (`;`).
#'
#' @export
tuGenes_filter <- function(x) {
  if ("X5.tuGenes" %in% colnames(x)) {
    x$X5.tuGenes <- sapply(strsplit(as.character(x$X5.tuGenes), ";"), `[`, 1)
  } else {
    stop("The column 'X5.tuGenes' does not exist")
  }
  return(x)
}
