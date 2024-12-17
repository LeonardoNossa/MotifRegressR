regressor <- function(scores,compendium,conditions,regression_type){

  scores <- as.data.frame(scores)

  common_rownames <- intersect(rownames(scores),rownames(compendium))
  scores <- scores[common_rownames, , drop = FALSE]
  compendium <- compendium[common_rownames, , drop = FALSE]

  output <- list()
  for (regression in regression_type){
    regression_models <- train_models(scores, compendium, conditions, regression)
    output[[regression]] <- regression_models
  }
  return(output)
}

