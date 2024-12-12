#' Filter Data Based on Gene IDs and Rename Rows
#'
#' This function filters a data frame by retaining only the rows whose row 
#' names match the specified Gene IDs from a given set. It also renames the 
#' rows based on the corresponding Gene Names from the `to_keep` data frame.
#'
#' @param to_filter A data frame or matrix to be filtered.
#' @param to_keep A data frame in which the `Gene_ID` column specifies the 
#' IDs of the genes to retain, while `Gene_Name` provides the new row names 
#' for the filtered data.
#'
#' @return A data frame or matrix that contains only the rows corresponding to 
#' the specified Gene IDs, with the row names updated to the corresponding 
#' Gene Names.
#'
#' @export

filter <- function(to_filter, to_keep){
  ids_to_keep <- unlist(to_keep$Gene_ID)
  names_to_keep <- to_keep$Gene_Name
  filtered <- to_filter[rownames(to_filter)%in%ids_to_keep,]
  rownames(filtered) <- names_to_keep[which(rownames(filtered) %in% ids_to_keep)]
  return(filtered)
}
