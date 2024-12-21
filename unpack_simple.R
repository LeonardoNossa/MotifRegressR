#' Unpack simple
#'
#' This function extracts the top `k` motifs from a simple linear regression 
#' model based on their p-values and coefficients.
#'
#' @param simple_sublist A list containing a single linear regression model 
#' (object of class `lm`).
#' @param k An integer specifying the number of top motifs to extract.
#' @param alpha A numeric value specifying the significance threshold for 
#' p-values (e.g., 0.05).
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs (excluding the intercept term).
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The estimated coefficients of the motifs.
#' }
#' If no coefficients pass the p-value threshold `alpha`, the function returns 
#' `NULL`.
#'
#' @export
unpack_simple <- function(simple_sublist, k, alpha){
    simple_model <- simple_sublist[[1]]
    
    coefficients <- summary(simple_model)$coefficients
    
    filtered_coefficients <- coefficients[coefficients[,"Pr(>|t|)"] <= alpha,]
    
    if (length(filtered_coefficients) == 0) {
        return(NULL)
    }
    
    ordered_filtered_coefficients <- 
        filtered_coefficients[order(abs(filtered_coefficients[,"Estimate"]),
                                    decreasing = TRUE), , drop = FALSE]
       
    ordered_filtered_coefficients <- 
        ordered_filtered_coefficients[rownames(ordered_filtered_coefficients) 
                                      != "(Intercept)",]
    top_k_coeffients <- 
        ordered_filtered_coefficients[1:min(k, 
                                            nrow(ordered_filtered_coefficients)), 
                                      , drop = FALSE]
    condition_name <- rep(names(simple_sublist), times = nrow(top_k_coeffients))
    
    output_df <- data.frame(
        motifs = rownames(top_k_coeffients),
        conditions = condition_name,
        coef = as.vector(top_k_coeffients[,"Estimate"])
    )
    output_df$motifs <- gsub("`", "", output_df$motifs)
    rownames(output_df) <- seq_along(1:nrow(output_df))
    return(output_df)
}