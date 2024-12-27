#' K-Nearest Neighbors (KNN) Regression
#'
#' This function performs KNN regression using the \code{caret} package.
#' Best model is evaluated using the  Recursive Feature Elimination.
#' Computation of the best model is parallelized using \code{future}
#' and \code{doFuture} packages
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the SVM regression. This is internally converted to a matrix.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{RFE} representing the fitted SVM regression model.
#'
#' @importFrom future multisession plan future value
#' @importFrom caret rfeControl caretFuncs rfe
#' @importFrom doFuture registerDoFuture
Regression_KNN <- function(Scores, TPMs, Condition) {

  set.seed(42)

  doFuture::registerDoFuture()
  future::plan(future::multisession, workers = 10)

  rfe_control <- caret::rfeControl(functions = caret::caretFuncs,
                                   method = "cv",
                                   number = 3,
                                   allowParallel = TRUE)

  rfe_results <- future::future({
    caret::rfe(
      x = Scores,
      y = TPMs[, Condition],
      sizes = c(1:(ncol(Scores) - 1)),
      rfeControl = rfe_control,
      method = "knn"
    )
  }, seed = TRUE)

  rfe_results <- future::value(rfe_results)
  return(rfe_results)
}
