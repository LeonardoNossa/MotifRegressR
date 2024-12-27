#' K-Nearest Neighbors (KNN) Regression
#'
#' This function performs KNN regression using the \code{caret} package.
#' The best model is evaluated using Recursive Feature Elimination (RFE).
#' Computation of the best model is parallelized using \code{doParallel}.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the KNN regression. This is internally converted to a matrix.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{RFE} representing the fitted KNN regression model.
#'
#' @details This function uses Recursive Feature Elimination (RFE) to select
#' the best set of predictors for KNN regression. Cross-validation is employed
#' to evaluate model performance. The computation is parallelized using
#' \code{doParallel} to improve efficiency.
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom caret rfeControl caretFuncs rfe
Regression_KNN <- function(Scores, TPMs, Condition) {

  suppressPackageStartupMessages(library(caret))

  num_cores <- 10
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)

  rfe_control <- suppressWarnings(caret::rfeControl(
    functions = caret::caretFuncs,
    method = "cv",
    number = 3,
    allowParallel = TRUE
  ))

  rfe_results <- caret::rfe(
    x = Scores,
    y = TPMs[, Condition],
    sizes = c(1:(ncol(Scores) - 1)),
    rfeControl = rfe_control,
    method = "knn"
  )

  suppressWarnings(parallel::stopCluster(cl))
  return(rfe_results)
}
