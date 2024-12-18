#' Support Vector Machine (SVM) Regression
#'
#' This function performs SVM regression using the \code{svm} function from 
#' the \code{e1071} package with a linear kernel.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in 
#' the SVM regression. This is converted to a matrix internally.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values. 
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{svm} returned by the \code{svm} function, 
#' representing the fitted SVM regression model.
#'
#' @importFrom e1071 svm
#' @export
Regression_svm <- function(Scores, TPMs, Condition){
  regr_svm <- e1071::svm(as.matrix(Scores), TPMs[,Condition], kernel = "linear")
  return(regr_svm)
}
