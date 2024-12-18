#' Lasso Regression
#'
#' This function performs Lasso regression for stability selection using the 
#' \code{randLassoStabSel} function from the \code{monaLisa} package.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in 
#' the Lasso regression. This is converted to a matrix internally.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values. 
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object returned by the \code{randLassoStabSel} function, 
#' representing the results of the stability selection process.
#'
#' @importFrom monaLisa randLassoStabSel
#' @export
Regression_lasso <- function(Scores, TPMs, Condition){
  regr_mona <- monaLisa::randLassoStabSel(as.matrix(Scores), TPMs[,Condition])
  return(regr_mona)
}
