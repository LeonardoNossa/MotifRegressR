#' Perform Principal Component Analysis (PCA)
#'
#' This function performs Principal Component Analysis (PCA) on a given matrix
#' and extracts the components needed to explain a specified percentage of the
#' variance.
#'
#' @param matrix A numeric matrix to perform PCA on. Each row represents an
#' observation, and each column represents a variable.
#' @param explained_var A numeric value (default: 95) indicating the percentage
#' of variance that the retained principal components should explain.
#' @param axis An integer (default: 2) indicating whether to perform
#' PCA on the columns (`axis = 2`) or on the rows of the matrix (`axis = 1`).
#' @param scale A logical value (default: TRUE) indicating whether the variables
#' should be scaled to unit variance before performing PCA.
#'
#' @return A numeric matrix containing the coordinates of the input data
#' projected onto the principal components that explain the specified variance.
#'
#'
#' @import FactoMineR
#' @export
principal_comps <- function(matrix,explained_var = 95, axis = 2, scale = TRUE){
  
  if(!(is.matrix(matrix))){
    stop("Please provide as input a matrix!")
  }
  
  if (axis == 1) {
    matrix <- t(matrix)
  }
  
  pca <- FactoMineR::PCA(matrix, scale.unit = scale, ncp = ncol(matrix), 
                         graph = FALSE)
  idx <- which(pca$eig[,3]>=explained_var)[1]
  final_coords <- pca$ind$coord[,seq_len(idx)]
  if (axis == 1){
    return(t(final_coords))
  }
  return(final_coords)
}
