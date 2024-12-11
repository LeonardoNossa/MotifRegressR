match_genes <- function(tuGenes_vector, protein_info_data) {
   
    matched_genes <- protein_info_data[protein_info_data[, 2] %in% tuGenes_vector, ]
    
    #string_protein_id (colonna 1) e preferred_name (colonna 2)
    result <- matched_genes[, c(1, 2)]
    return(result)
  }