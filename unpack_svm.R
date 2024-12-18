#' Unpack svm
#'
#' This function extracts the top `k` most important motifs from a 
#' trained SVM model based on their feature importance.
#'
#' @param svm_sublist A list containing a single SVM model (object of 
#' class `svm`).
#' @param k An integer specifying the number of top features to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The feature importance values for the motifs.
#' }
#'
#' @export
unpack_svm <- function(svm_sublist, k){
  svm_model <- svm_sublist[[1]]
  weights <- t(svm_model$coefs) %*% svm_model$SV
  
  feature_importance <- apply(weights, 2, function(x) sqrt(sum(x^2)))
  
  top_k_indices <- 
    order(feature_importance, 
          decreasing = TRUE)[1:min(k, length(feature_importance))]
  top_k_importance <- feature_importance[top_k_indices]
  
  motif_names <- colnames(as.matrix(svm_model$SV))[top_k_indices]
  
  condition_name <- rep(names(svm_sublist), times = length(top_k_importance))
  
  output_df <- data.frame(
    motifs = motif_names,
    conditions = condition_name,
    coef = top_k_importance
  )
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}
