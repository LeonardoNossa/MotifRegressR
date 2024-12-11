#' loader
#'
#' @param path A character string representing the path to the file containing 
#' PFMs.The file should follow the MEME motif format, with motifs identified by 
#' the keyword "MOTIF". 
#' 
#' @param tax_id A numeric or character vector representing the taxonomy ID of 
#' the species for which to retrieve motifs (e.g., 9606 for Homo sapiens).
#'
#' @return A named list of data frames, where each data frame represents the 
#' PFM of a motif. Each PFM contains the nucleotide frequencies ("A", "C", "G", "T") 
#' at each position.
#' @export
#'
#' @examples
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