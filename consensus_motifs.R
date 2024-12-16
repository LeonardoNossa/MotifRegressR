consensus_motifs <- function(...) {
  inputs <- list(...)
  final_inputs <- list()
  for (object in inputs) {
    if (!is(object, "data.frame")) {
      regressor_type <- retrieve_regressor_type(object[[1]])
      object <- models2dataframe(object, regressor_type, k = 5)
      final_inputs <- append(final_inputs, list(object))
    } else {
      final_inputs <- append(final_inputs, list(object))
    }
  }
  
  output <- list()
  
  conditions <- unique(unlist(final_inputs)[grepl("^conditions",
                                                  names(unlist(final_inputs)))])
  for (condition in conditions) {
    all_motifs_in_condition <- lapply(X = final_inputs, FUN = function(df) {
      subset_df <- df$motifs[df$conditions == condition]
      return(subset_df)
    })
    all_motifs_in_condition <- unlist(all_motifs_in_condition)
    sorted_motifs_summary <- sort(table(all_motifs_in_condition),
                                  decreasing = TRUE)
    sorted_motifs_summary <- (sorted_motifs_summary/length(inputs))*100
    output[[condition]] <- as.vector(sorted_motifs_summary)
    names(output[[condition]]) <- names(sorted_motifs_summary)
    
  }
  
  return(output)
}
