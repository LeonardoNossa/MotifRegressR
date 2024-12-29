#' Compute All PWM Scores
#'
#' This function wraps the `AllScores` function and allows for the evaluation
#' of scores for both DNA strands. The function computes the two score matrices
#' and take the maximum for each entry
#'
#' @param PWMs A list of Position Weight Matrices (PWMs). Each PWM in the list
#'  should be a numeric matrix with 4 rows (corresponding to A, C, G, T).
#' @param dataSeq A matrix containing chromosomes names and the DNA sequences to
#' be scored. Each sequence must consist of the characters "A", "C", "G", and 
#' "T". The names of the vector will be used as row names in the output.
#' @param workers integer, the number of cores to assign for parallel 
#' computations
#' @param both logical, indicating whether to compute scores for both DNA strands.
#' If TRUE, the function computes the scores for both strands and returns the 
#' maximum of the two. If FALSE, it computes scores only for the forward strand.
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
                     rev_seq <- 
                       as.character(Biostrings::reverseComplement(x = seq))
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


#' Compute All PWM Scores
#'
#' This function calculates the maximum scores for a list of Position Weight
#' Matrices (PWMs) across a set of DNA sequences using parallel computation.
#' For each PWM, it computes the maximum score for every sequence and returns
#' a matrix of scores.
#'
#' @param PWMs A list of Position Weight Matrices (PWMs). Each PWM in the list
#'  should be a numeric matrix with 4 rows (corresponding to A, C, G, T).
#' @param dataSeq A matrix containing chromosomes names and the DNA sequences to
#' be scored. Each sequence must consist of the characters "A", "C", "G", and 
#' "T". The names of the vector will be used as row names in the output.
#' @param workers integer, the number of cores to assign for parallel 
#' computations
#'
#' @return A numeric matrix where:
#' \item{Rows}{Correspond to the DNA sequences (named from `dataSeq`).}
#' \item{Columns}{Correspond to each PWM in `PWMs`.}
#' Each entry is the maximum score for the given sequence and PWM.
#' @import future
#' @importFrom future.apply future_lapply
AllScores <- function(PWMs, dataSeq, workers = 4){
  plan(multisession, workers = workers)
  Scores <- future_lapply(PWMs, function(pwm) get_scores(pwm, dataSeq))
  Total <- do.call(cbind, lapply(Scores, function(score) score[,2]))
  plan(sequential)
  rownames(Total) <- rownames(dataSeq)
  return(Total)
}

#' Compute PWM Scores
#'
#' This function calculates the maximum position-specific scoring matrix (PSSM)
#' scores for a list of DNA sequences using a given Position Weight Matrix(PWM).
#' It identifies the position with the highest score for each sequence and
#' returns both the position and the corresponding score.
#'
#' @param PWM A numeric matrix representing the Position Weight Matrix (PWM).
#' The matrix should have 4 rows (for A, C, G, T).
#' @param dataSeq A character vector containing DNA sequences to be scored.
#' Each sequence must be composed of the characters "A", "C", "G", and "T".
#'
#' @return A numeric matrix with two columns and a number of rows equal to the
#' length of `dataSeq`:
#' \item{Column 1}{The position in the sequence with the highest score.}
#' \item{Column 2}{The maximum score for the sequence.}
#' @importFrom seqinr s2c
get_scores <- function(PWM,dataSeq){
  
  n_seq = nrow(dataSeq)
  Result <-matrix(0,ncol = 2,nrow= n_seq)
  PWM = matrix(as.numeric(as.matrix(PWM)),ncol = 4)
  
  for (s in (1: n_seq)) {
    UPSTREAM = seqinr::s2c(dataSeq[s,2])
    MATRIX<-matrix(0,nrow=4,ncol=length(UPSTREAM))
    mA<-which(UPSTREAM=="A")
    mC<-which(UPSTREAM=="C")
    mG<-which(UPSTREAM=="G")
    mT<-which(UPSTREAM=="T")
    
    MATRIX[1,mA]<-1
    MATRIX[2,mC]<-1
    MATRIX[3,mG]<-1
    MATRIX[4,mT]<-1
    
    n_col = dim(PWM)[2]
    ScoreMatrix<-matrix(0,ncol = 1, nrow=(length(UPSTREAM)-n_col))
    
    for (i in 1:(dim(MATRIX)[2] - n_col)-1){
      seq = MATRIX[,i:(nrow(PWM))]
      score = sum(diag(PWM %*% seq))
      ScoreMatrix[i,1] <- score
    }
    MaxIndex = which(ScoreMatrix == max(ScoreMatrix))
    Result[s,1] = MaxIndex[1]
    Result[s,2] = max(ScoreMatrix)[1]
  }
  return(Result)
}
