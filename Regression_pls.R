#' Partial Least Squares (PLS) Regression
#'
#' This function performs Partial Least Squares (PLS) regression using the 
#' \code{plsr} function from the \code{pls} package. It determines the optimal
#'  number of components using cross-validation and builds the final model 
#'  accordingly.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in 
#' the PLS regression. This is converted to a matrix internally.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values. 
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{mvr} returned by the \code{plsr} function, 
#' representing the PLS regression model with the optimal number of components.
#'
#' @importFrom pls plsr
#' @export
Regression_pls <- function(Scores, TPMs, Condition) {
  ncomp = dim(Scores)[2]
  y <- TPMs[,Condition]
  x <- as.matrix(Scores)
  
  model_all_component <- pls::plsr(y ~ x, ncomp = ncomp, validation = "CV")
  optimal_ncomp <- which.min(model_all_component$validation$PRESS)
  pls_model <- pls::plsr(y ~ x, ncomp = optimal_ncomp, validation = "CV")
  return(pls_model)
}
