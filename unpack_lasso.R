#' Unpack Lasso
#'
#' This function extracts the top `k` motifs from a lasso regression model based
#'  on their stability selection probabilities.
#'
#' @param lasso_sublist A list containing a single lasso regression model. The 
#' model must include metadata with stability selection parameters.
#' @param k An integer specifying the number of top motifs to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The stability selection probabilities (in percentages) of the
#'    motifs.
#' }
#' If no motifs are selected, the function returns `NULL`.
#'
#' @export
unpack_lasso <- function(lasso_sublist, k){
  
  lasso_model <- lasso_sublist[[1]]
  metadata <- lasso_model@metadata
  coldata <- lasso_model@colData
  probabilities <- sort(metadata$stabsel.params.max[coldata$selected],
                        decreasing = TRUE) * 100
  
  if (length(probabilities) == 0) {
    return(NULL)
  }
  
  condition_name <- rep(x = names(lasso_sublist),
                        times = min(k,length(probabilities)))
  
  top_k <- probabilities[1:min(k,length(probabilities))]
  top_k_names <- names(top_k)
  
  output_df <- data.frame(motifs = top_k_names,
                          conditions = condition_name,
                          coef = top_k)
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}
