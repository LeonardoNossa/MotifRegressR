#' Retrieve the Type of a Regression Model
#'
#' This function determines the type of a given regression model based on its 
#' class.
#'
#' @param model An object representing a regression model. The model's class is
#'  used to determine its type.
#'
#' @return A character string indicating the type of regression model:
#' \itemize{
#'   \item `"svm"` for Support Vector Machine models.
#'   \item `"pls"` for Partial Least Squares regression models.
#'   \item `"lasso"` for Lasso regression models (stored as 
#'   `SummarizedExperiment` objects).
#'   \item `"simple"` for simple linear regression models.
#' }
#'
#' @export
retrieve_regressor_type <- function(model) {
  if (class(model) == "svm") {
    return("svm")
  } else if (class(model) == "mvr") {
    return("pls")
  } else if (class(model) == "SummarizedExperiment") {
    return("lasso")
  } else if (class(model) == "lm") {
    return("simple")
  }
}
