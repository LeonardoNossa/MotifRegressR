#' Loader
#' 
#' This function allows the user to load a Position Frequency Matrix (PFM) list.
#' This can be done by either providing a .meme file using `path` argument or by
#' providing the `tax_id` parameter to perform a query to JAPSPAR database.
#' If both parameters are provided, then only the local file will be uploaded.
#'
#' @param path A character string representing the path to the file containing 
#' PFMs.The file should follow the MEME motif format, with motifs identified by
#' the keyword "MOTIF". 
#' 
#' @param tax_id A numeric or character vector representing the taxonomy ID of 
#' the species for which to retrieve motifs from JASPAR(e.g., 4932 for S.cerevisiae).
#'
#' @return A named list of data frames, where each data frame represents the 
#' PFM of a motif. Each PFM contains the nucleotide frequencies 
#' ("A", "C", "G", "T") at each position.
#'
#' @export
loader <- function(path = NULL, tax_id = NULL){
  if(is.null(tax_id) & !is.null(path)){
    PFMs <- local_PFM_loader(path)
    return(PFMs)
  } else if(is.null(path) & !is.null(tax_id)) {
    PFMs <- online_PFM_loader(tax_id)
    return(PFMs)
  } else if(!is.null(path) & !is.null(tax_id)){
    PFMs <- local_PFM_loader(path)
    return(PFMs)
  } else {
    stop('Either "path" or "tax_id" MUST be defined')
  }
}
