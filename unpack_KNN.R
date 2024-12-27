#' Unpack KNN
#'
#' This function extracts the top `k` most important motifs from a
#' trained KNN model based on their feature importance.
#'
#' @param KNN_sublist A list containing a single SVM model (object of
#' class `rfe`).
#' @param k An integer specifying the number of top features to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The feature importance values for the motifs.
#' }
unpack_KNN <- function(KNN_sublist, k){
  rfe_results <- KNN_sublist[[1]]
  rfe_importance <- caret::varImp(rfe_results, scale = TRUE)

  output_df <- data.frame(motifs = rownames(rfe_importance)[1:min(k,nrow(rfe_importance))],
                          conditions = rep(x = names(KNN_sublist), times = min(k,nrow(rfe_importance))),
                          coef = rfe_importance$Overall[1:min(k,nrow(rfe_importance))])
  return(output_df)
}
