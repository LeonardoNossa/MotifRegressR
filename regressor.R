regressor <- function(scores,compendium,conditions,regression_type){

  output <- list()
  for (regression in regression_type){
    regression_models <- train_models(scores, compendium, conditions, regression)
    output[[regression]] <- regression_models
  }
  return(output)
}

