#' Regressor
#'
#' This function trains multiple regression models on the provided scores and 
#' compendium data. It filters the rows to retain only the common ones between 
#' the scores and the compendium before training the models.
#'
#' @param scores A data frame or matrix of scores (e.g., feature scores or 
#' experimental data).
#' @param compendium A data frame or matrix of the compendium data (e.g., 
#' known conditions or targets for regression).
#' @param conditions A vector of conditions or responses used for training the 
#' regression models.
#' @param regression_type A character vector specifying the types of regression 
#' models to be trained. Available types depend on the `train_models` function.
#'
#' @return A list of regression models, each corresponding to a specified 
#' regression type.
#'
#' @export
regressor <- function(scores, compendium, conditions, regression_type){
  
  scores <- as.data.frame(scores)
  
  common_rownames <- intersect(rownames(scores), rownames(compendium))
  scores <- scores[common_rownames, , drop = FALSE]
  compendium <- compendium[common_rownames, , drop = FALSE]
  
  output <- list()
  for (regression in regression_type){
    regression_models <- train_models(scores, compendium, conditions, 
                                      regression)
    output[[regression]] <- regression_models
  }
  return(output)
}

