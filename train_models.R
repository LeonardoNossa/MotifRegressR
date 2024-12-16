train_models <- function(S, EXPN, conditions, regression = "simple") {
  regression_func <- switch(
    as.character(regression),
    "lasso" = Regression_lasso,
    "pls" = Regression_pls,
    "svm" = Regression_svm,
    "simple" = Regression_simple,
    NULL
  )
  
  if (is.null(regression_func)) {
    stop("Please, choose among the following options: \n- lasso\n- pls\n- svm\n- simple")
  }
  
  if (all(is.character(conditions)) && conditions == 'all') {
    output_all <- sapply(
      X = seq_along(1:ncol(EXPN)), FUN = function(condition_id) {
        output <- regression_func(S, EXPN, condition_id)
        return(output)},
      simplify = FALSE
    )
    names(output_all) <- colnames(EXPN)
  } else {
    output_all <- sapply(
      X = conditions, FUN = function(condition_id) {
        output <- regression_func(S, EXPN, condition_id)
        return(output)},
      simplify = FALSE
    )
    names(output_all) <- colnames(EXPN)[conditions]
  }
  
  return(output_all)
}
