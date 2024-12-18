#' Extract DNA Sequences from a FASTA Object
#'
#' This function extracts a DNA sequence from a FASTA object based on the 
#' provided chromosome, start, and end positions. It returns the sequence along
#' with its associated chromosome and gene name.
#'
#' @param FASTA A named list or vector of DNA sequences, where each element is
#' a sequence. FASTA is obtained using Biostrings::readDNAStringSet
#' @param row A vector containing the chromosome name, start position, and 
#' end position of the sequence to extract. The row should be in the 
#' format: c(chr, start, end).
#'
#' @return A character vector with three elements:
#' \item{chromosome}{The name of the chromosome from which the sequence is 
#' extracted.}
#' \item{gene_name}{The gene name, extracted from the row names.}
#' \item{sequence}{The extracted DNA sequence.}
#'
#' @export
extract_sequences <- function(FASTA,row){
  chr <- row[1]
  start <- row[2]
  end <- row[3]
  gene_name <- rownames(row)
  sequence <- substr(FASTA[chr], start, end)
  return(c(chromosome = chr, gene_name = gene_name, sequence = sequence))
}
