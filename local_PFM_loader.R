#' local_PFM_loader
#'
#' This function reads a file containing position frequency matrices (PFMs) and
#' extracts them into a list of data frames. Each PFM represents the nucleotide
#' frequencies ("A", "C", "G", "T") at each position for a given motif.
#' 
#' @param path A character string representing the path to the file containing 
#' PFMs.The file should follow the MEME motif format, with motifs identified by 
#' the keyword "MOTIF".
#'
#' @return A named list of data frames. Each data frame corresponds to the PFM
#' of a motif and contains the nucleotide frequencies at each position 
#' ("A", "C", "G", "T").The names of the list elements are the motif 
#' names extracted from the file.
#' 
#' @importFrom utils read.table
local_PFM_loader <- function(path){
  
  Tab <-  utils::read.table(path , skip = 9, fill = T)
  motif_indices <- grep("MOTIF", Tab$V1)
  if (all(nchar(Tab[Tab$V1 == "MOTIF",]$V3) > 0)) {
    motif_names <- paste0(Tab[Tab$V1 == "MOTIF",]$V3,"_",Tab[Tab$V1 == "MOTIF",]$V2)
  } else {
    motif_names <- Tab[Tab$V1 == "MOTIF",]$V2
  }
  

  motifs <- list()
  for (i in seq_along(motif_indices)) {
    
    start <- motif_indices[i]
    end <- if (i < length(motif_indices)) motif_indices[i + 1] - 1 else nrow(Tab)
    
    motif_name <- motif_names[i]
    motif_df <- Tab[(start + 2) : (end - 1),1:4]
    colnames(motif_df) <- c("A", "C", "G", "T")
    rownames(motif_df) <- seq(nrow(motif_df))
    
    if (any(motif_df == "URL")) {
      idx <- which(motif_df == "URL")
      motif_df <- motif_df[-(idx:nrow(motif_df)),]
    }
    
    motif_df <- as.data.frame(apply(motif_df,2,as.numeric), stringsAsFactors = FALSE)
    motifs[[motif_name]] <- motif_df
  }
  return(motifs)
}
