unpack_svm <- function(svm_sublist, k){
  svm_model <- svm_sublist[[1]]
  weights <- t(svm_model$coefs) %*% svm_model$SV
  
  feature_importance <- apply(weights, 2, function(x) sqrt(sum(x^2)))
  
  top_k_indices <- order(feature_importance, decreasing = TRUE)[1:min(k, length(feature_importance))]
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
