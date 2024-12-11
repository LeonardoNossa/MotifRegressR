#' principal_comps
#'
#' @param matrix 
#' @param explained_var 
#' @param axis 
#' @param scale 
#'
#' @return
#' @import FactoMineR
#' @export
#' @examples
principal_comps <- function(matrix,explained_var = 95, axis = 2, scale = TRUE){
  
  if(!(is.matrix(matrix))){
    stop("Please provide as input a matrix!")
  }
  
  if (axis == 1) {
    matrix <- t(matrix)
  }
  
  pca <- FactoMineR::PCA(matrix, scale.unit = scale, ncp = ncol(matrix), graph = FALSE)
  idx <- which(pca$eig[,3]>=explained_var)[1]
  final_coords <- pca$ind$coord[,seq_len(idx)]
  if (axis == 1){
    return(t(final_coords))
  }
  return(final_coords)
}










