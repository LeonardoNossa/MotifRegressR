table_first_genes <- function(tsv,pro){
  TUSet <- read.table(tsv, 
                      sep = "\t", 
                      header = TRUE, 
                      quote = "",
                      comment.char = "#",
                      stringsAsFactors = FALSE)
  
  TUSet <- tuGenes_filter(TUSet)
  tuGenes_filtered <- TUSet$X5.tuGenes
  
  protein_info <- read.table(pro, 
                             sep = "\t", 
                             header = FALSE, 
                             quote = "",
                             comment.char = "#",
                             stringsAsFactors = FALSE)
  
  result_table <- match_genes(tuGenes_filtered, protein_info)
  result_table<-rename_first_column(result_table)
  colnames(result_table) [c(1,2)] <- c('Gene_ID', 'Gene_Name')
  
  return(result_table)
}
  




























