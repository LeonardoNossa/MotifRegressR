#' Match Genes to Protein Information 
#'
#' This function matches a vector of gene names to a protein information 
#' dataset and returns the corresponding protein IDs and preferred names.
#'
#' @param tuGenes_vector A character vector containing the gene names to match.
#' @param protein_info_data A data frame where the first column contains 
#' protein IDs and the second column contains gene names.
#'
#' @return A data frame containing two columns: the matched protein IDs (from 
#' the first column) and the preferred names (from the second column) for the 
#' input gene names.
#'
#' @export
match_genes <- function(tuGenes_vector, protein_info_data) {
  
  matched_genes <- protein_info_data[protein_info_data[, 2] %in% 
                                       tuGenes_vector, ]
  
  #string_protein_id (colonna 1) e preferred_name (colonna 2)
  result <- matched_genes[, c(1, 2)]
  return(result)
}
