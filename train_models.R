#' Train Regression Models
#'
#' This function trains regression models for specified experimental conditions
#'  using the selected regression method.
#'
#' @param S A numeric matrix or data frame of scores
#' @param EXPN A numeric matrix or data frame of expression values
#' @param conditions A vector specifying the conditions to be analyzed. Can be 
#' column indices or names in `EXPN`. Use `"all"` to include all columns.
#' @param regression A character string indicating the regression method to use.
#'  Options are:
#' \itemize{
#'   \item `"Randomized_lasso"` for Lasso regression with stability selection.
#'   \item `"PLS"` for Partial Least Squares regression.
#'   \item `"SVM"` for Support Vector Machine regression.
#'   \item `"LM"` for simple linear regression.
#' }
#'
#' @return A named list where each element corresponds to a regression model for
#'  a specific condition.
#'
#' @export
train_models <- function(S, EXPN, conditions, regression = "LM") {
  regression_func <- switch(
    as.character(regression),
    "Randomized_lasso" = Regression_lasso,
    "PLS" = Regression_pls,
    "SVM" = Regression_svm,
    "LM" = Regression_simple,
    NULL
  )
  
  if (is.null(regression_func)) {
    stop("Please, choose among the following options:
         \n- lasso\n- pls\n- svm\n- simple")
  }
  
  if (is.character(conditions)){
    if (all(conditions == "all")){
      conditions <- seq_len(ncol(EXPN))
    } else {
      conditions <- match(conditions, colnames(EXPN))
      if (any(is.na(conditions))){
        stop("One or more column names in 'conditions' do not exist in 'EXPN'")
      }
    }
  }
  
  output_all <- sapply(
    X = conditions, FUN = function(condition_id){
      output <- regression_func(S, EXPN, condition_id)
      return(output)
    },
    simplify = FALSE
  )
  names(output_all) <- colnames(EXPN)[conditions]
  
  return(output_all)
}
