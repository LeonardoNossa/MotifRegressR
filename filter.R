filter <- function(to_filter, to_keep){
  ids_to_keep <- unlist(to_keep$Gene_ID)
  names_to_keep <- to_keep$Gene_Name
  filtered <- to_filter[rownames(to_filter)%in%ids_to_keep,]
  rownames(filtered) <- names_to_keep[which(rownames(filtered) %in% ids_to_keep)]
  return(filtered)
}
