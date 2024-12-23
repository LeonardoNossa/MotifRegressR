#' Pivot data.frame
#'
#' This function takes as input a data.frame coming
#' from `models2dataframe` call and pivots it, assigning the importance score
#' for each motif, in each condition.
#'
#' @param df the data.frame coming from `models2dataframe`, containing the top
#' k motif in each condition and their importance score
#'
#' @return A data.frame, where rows are motifs, and columns are the conditions.
#' If a motif is not among the top k motifs in that condition, the function will
#' assign 0.
pivot_df <- function(df){
  if (nrow(df) == 1) {
    matrix <- matrix(data = df$coef)
    rownames(matrix) <- df$motifs
    colnames(matrix) <- df$conditions
  } else {
    rownames <- unique(df$motifs)
    colnames <- unique(df$conditions)
    matrix <- sapply(X = colnames, FUN = function(condition){
      sapply(X = rownames,
             FUN = function(x){return(return_score(df,x,condition))})
   })
  }
  return(matrix)
}

#' Retrieve importance score
#'
#' This function takes as input a data.frame coming
#' from `models2dataframe` call, a single motif name and a single condition,
#' retrieving the importance score for that motif in that condition.
#' If the score cannot be retrieved (i.e. the motif is not among the top k
#' motifs in that condition) 0 is assigned
#'
#' @param df the data.frame coming from `models2dataframe`, containing the top
#' k motif in each condition and their importance score
#'
#' @param motif_name the name of a single motif
#'
#' @param condition the name of a single condition
#'
#' @return the importance score for that particular motif, in that particular
#' condition. If the score cannot be retrieved (i.e. the motif is not among the top k
#' motifs in that condition) 0 is assigned
return_score <- function(df,motif_name,condition){
  val <- df[(df$motifs == motif_name & df$conditions == condition),]$coef
  val <- ifelse(identical(val,numeric(0)),0,val)
  return(val)
}


