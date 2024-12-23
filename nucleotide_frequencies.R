#' Starting from FASTA file obtain the nucleotide frequencies
#'
#' This function takes in input FASTA fie and compute the freqeucnies of each 
#' nucleotide.
#' @param fasta The path to the input FASTA file 
#' @return A named vector with frequencies for each nucleotide.
#' 
#' @export
nucleotide_frequencies <- function(fasta){
    FASTA <- Biostrings::readDNAStringSet(fasta)
    freq <-  Biostrings::alphabetFrequency(FASTA, baseOnly = TRUE)[, c("A", "C", "G", "T")]
    if (is.vector(freq)) {
        frequencies <- freq / sum(freq)
    } else {
        frequencies <- colSums(freq) / sum(freq)
    }
    return(frequencies)
}



