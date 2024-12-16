unpack_lasso <- function(lasso_sublist, k){
  
  lasso_model <- lasso_sublist[[1]]
  metadata <- lasso_model@metadata
  coldata <- lasso_model@colData
  probabilities <- sort(metadata$stabsel.params.max[coldata$selected],
                        decreasing = TRUE) * 100
  
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
