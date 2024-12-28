#' Starting from FASTA file obtain the nucleotide frequencies
#'
#' This function takes in input FASTA file and computes the frequencies of each 
#' nucleotide.
#' @param fasta The path to the input FASTA file 
#' @return A named vector with frequencies for each nucleotide
#' (in this order A,C,G,T)
#' @importFrom Biostrings readDNAStringSet
#' @importFrom Biostrings alphabetFrequency
#' @export
retrieve_frequencies <- function(fasta){
    FASTA <- Biostrings::readDNAStringSet(fasta)
    freq <-  Biostrings::alphabetFrequency(FASTA, baseOnly = TRUE)[, c("A", "C", "G", "T")]
    if (is.vector(freq)) {
        frequencies <- freq / sum(freq)
    } else {
        frequencies <- colSums(freq) / sum(freq)
    }
    return(frequencies)
}



