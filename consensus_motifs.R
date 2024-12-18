#' Consensus Motifs
#'
#' This function calculates the consensus motifs across a list of data frames.
#'
#' @param df_list A list of data frames.
#' @param by_regressor Logical. If \code{TRUE}, the consensus motifs are 
#' calculated by condition across all data frames. If \code{FALSE}, the 
#' consensus motifs are calculated for each data frame individually.
#'
#' @return A named list where each element contains the consensus motifs as a 
#' named vector of percentages.
#'
#' @export
consensus_motifs <- function(df_list, by_regressor = TRUE) {
  for (object in df_list) {
    
    if (!is(object, "data.frame")) {
      err_msg <- paste0("Please, the provided list must contain only",
                        " dataframes, consider providing the output of",
                        " 'models2dataframe' function.")
      stop(err_msg)
    }
  }
  
  output <- list()
  conditions <- unique(unlist(df_list)[grepl("conditions",
                                             names(unlist(df_list)))])
  if (by_regressor) {
    for (condition in conditions) {
      all_motifs_in_condition <- lapply(X = df_list, FUN = function(df) {
        subset_df <- df$motifs[df$conditions == condition]
        return(subset_df)
      })
      all_motifs_in_condition <- unlist(all_motifs_in_condition)
      sorted_motifs_summary <- sort(table(all_motifs_in_condition),
                                    decreasing = TRUE)
      sorted_motifs_summary <- ((sorted_motifs_summary/length(df_list))*100)
      output[[condition]] <- as.vector(sorted_motifs_summary)
      names(output[[condition]]) <- names(sorted_motifs_summary)
      
    }
  } else {
    regressions <- names(df_list)
    for (i in seq_len(length(df_list))) {
      df_regr <- df_list[[i]]
      sorted_motifs_summary <- sort(table(df_regr$motifs), decreasing = TRUE)
      sorted_motifs_summary <- ((sorted_motifs_summary/length(conditions))*100)
      output[[regressions[i]]] <- as.vector(sorted_motifs_summary)
      names(output[[i]]) <- names(sorted_motifs_summary)
    }
    
    
  }
  
  return(output)
}
