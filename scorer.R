#' Compute All PWM Scores
#'
#' This function wraps the `AllScores` function and allows for the evaluation
#' of scores for both DNA strands. The function computes the two score matrices
#' and take the maximum for each entry
#'
#' @param PWMs A list of Position Weight Matrices (PWMs). Each PWM in the list
#'  should be a numeric matrix with 4 rows (corresponding to A, C, G, T).
#' @param dataSeq A matrix containing chromosomes names and the DNA sequences to be
#' scored. Each sequence must consist of the characters "A", "C", "G", and "T".
#' The names of the vector will be used as row names in the output.
#' @param workers integer, the number of cores to assign for parallel computations
#'
#' @return A numeric matrix where:
#' \item{Rows}{Correspond to the DNA sequences (named from `dataSeq`).}
#' \item{Columns}{Correspond to each PWM in `PWMs`.}
#' Each entry is the maximum score between the matrix of the two strands
#' @importFrom Biostrings DNAString
#' @importFrom Biostrings reverseComplement
#' @export
scorer <- function(PWMs,dataSeq,workers = 4, both = TRUE){

  S_for <- AllScores(PWMs = PWMs,
                     dataSeq = dataSeq,
                     workers = workers)

  if (both) {

    revs <- sapply(X = seq_along(1:nrow(dataSeq)),
                   FUN = function(idx){
                     row <- dataSeq[idx,]
                     seq <- Biostrings::DNAString(row[2])
                     rev_seq <- as.character(Biostrings::reverseComplement(x = seq))
                     return(rev_seq)
                   })

    rev_sequences <- cbind(dataSeq[,1],revs)
    colnames(rev_sequences) <- colnames(dataSeq)

    S_rev <- AllScores(PWMs = PWMs,
                       dataSeq = rev_sequences,
                       workers = workers)

    final_mat <- ifelse(S_for>S_rev,S_for,S_rev)
    return(final_mat)
  }
  return(S_for)
}
