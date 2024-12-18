#' Simple Linear Regression
#'
#' This function performs a simple linear regression using the \code{lm} 
#' function.
#'
#' @param Scores A data frame or matrix of scores to be used as predictors in 
#' the regression model.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values. 
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#'  corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{lm} returned by the \code{lm} function, 
#' representing the fitted linear model.
#'
#' @importFrom stats lm
#' @export
Regression_simple <- function(Scores, TPMs, Condition){
  regr_simple <- lm(TPMs[,Condition] ~ ., data = Scores)
  return(regr_simple)
}
