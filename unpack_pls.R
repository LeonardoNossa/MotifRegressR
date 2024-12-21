#' Unpack PLS
#'
#' This function extracts the top `k` motifs from a PLS regression model based
#'  on their regression coefficients.
#'
#' @param pls_sublist A list containing a single PLS regression model. The model
#'  must include coefficients for motifs.
#' @param k An integer specifying the number of top motifs to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The regression coefficients of the motifs.
#' }
#'
#' @export
unpack_pls <- function(pls_sublist, k) {
    pls_model <- pls_sublist[[1]]
    coefficients <- coef(pls_model)
    motif_names <- rownames(coefficients)
    
    condition_name <- rep(x = names(pls_sublist),
                          times = min(k, length(coefficients)))
    
    ranking_idxs <- order(abs(coefficients), decreasing = TRUE)
    coefficients <- coefficients[ranking_idxs]
    motif_names <- motif_names[ranking_idxs]
    
    top_k <- coefficients[1:min(k, length(coefficients))]
    top_k_names <- motif_names[1:min(k, length(coefficients))]
    
    output_df <- data.frame(motifs = top_k_names,
                            conditions = condition_name,
                            coef = top_k)
    rownames(output_df) <- seq_along(1:nrow(output_df))
    
    return(output_df)
}
