unpack_simple <- function(simple_sublist,k, alpha){
  simple_model <- simple_sublist[[1]]
  
  coefficients <- summary(simple_model)$coefficients
  
  filtered_coefficients <- coefficients[coefficients[,"Pr(>|t|)"]<=alpha,]
  
  ordered_filtered_coefficients <- filtered_coefficients[order(filtered_coefficients[,"Pr(>|t|)"], decreasing = FALSE), , drop = FALSE]
  ordered_filtered_coefficients <- ordered_filtered_coefficients[rownames(ordered_filtered_coefficients) != "(Intercept)",]
  top_k_coeffients <- ordered_filtered_coefficients[1:min(k,nrow(ordered_filtered_coefficients)), , drop = FALSE]
  condition_name <- rep(names(simple_sublist), times = nrow(top_k_coeffients))
  
  output_df <- data.frame(
    motifs = rownames(top_k_coeffients),
    conditions = condition_name,
    coef = as.vector(top_k_coeffients[,"Estimate"])
  )
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}
