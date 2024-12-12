#' Extract DNA Sequences Based on GFF and FASTA Files
#'
#' This function extracts DNA sequences based on gene regions defined in a GFF 
#' file and retrieves the corresponding sequences from a FASTA file.
#' The function returns a data frame with the extracted sequences for each gene.
#'
#' @param gff A string specifying the path to the GFF file containing genomic 
#' features and regions.
#' @param fasta A string specifying the path to the FASTA file containing the 
#' DNA sequences.
#' @param is_circular A logical value indicating whether the chromosomes are 
#' circular.
#' @param upstream An integer specifying the number of base pairs to include 
#' upstream of each gene. Default is 300.
#'
#' @return A data frame with two columns:
#' \item{Chr}{The chromosome name for each extracted sequence.}
#' \item{Sequence}{The DNA sequence for each gene, extracted from the specified
#' region in the FASTA file.}
#'
#' @export
get_sequences <- function(gff, fasta, is_circular, upstream = 300){
  
  data <- read.table(gff, 
                     sep = "\t", 
                     header = FALSE, 
                     quote = "",
                     comment.char = "#",
                     stringsAsFactors = FALSE)
  
  colnames(data) <- c("seqname",
                      "source",
                      "feature",
                      "start",
                      "end",
                      "score",
                      "strand",
                      "frame",
                      "group")
  
  chrom_size <- data[data$feature == "region",]$end
  names(chrom_size) <- data$seqname[data$feature == "region"]
  
  genes <- data[data$feature == "gene",]
  
  tmp <- strsplit(x = genes$group, split = ";")
  GeneName <- unlist(lapply(X = tmp, function(x) x[1])) 
  GeneName <- sub(pattern = "ID=gene-", x = GeneName, replacement = "")
  
  regions_complete <- as.data.frame(t(apply(X = genes,
                                            MARGIN = 1,
                                            FUN = get_regions,
                                            chrom_size,
                                            is_circular,
                                            upstream)))
  
  colnames(regions_complete) <- c("chromosome", "start", "end")
  rownames(regions_complete) <- c(GeneName)
  
  regions_complete$start <- as.integer(regions_complete$start)
  regions_complete$end <- as.integer(regions_complete$end)
  regions_complete <- regions_complete[regions_complete$start <= 
                                         regions_complete$end,]
  
  FASTA <- Biostrings::readDNAStringSet(fasta)
  FASTA <- as.character(FASTA)
  
  new_names <- sapply(strsplit(names(FASTA), " "), `[`, 1)
  names(FASTA) <- new_names
  
  all_seqs <- t(apply(X = regions_complete, MARGIN = 1, 
                      FUN = extract_sequences, FASTA = FASTA))
  colnames(all_seqs) <- c('Chr', 'Sequence')
  return(all_seqs)
}
