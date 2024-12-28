#' Regressor
#'
#' This function trains multiple regression models on the provided scores and
#' compendium data. It filters the rows to retain only the common ones between
#' the scores and the compendium before training the models.
#'
#' @param scores A data frame or matrix of scores (e.g., feature scores or
#' experimental data).
#' @param compendium A data frame or matrix of the compendium data (e.g.,
#' known conditions or targets for regression).
#' @param conditions A vector of conditions or responses used for training the
#' regression models.
#' @param regression_type A character vector specifying the types of regression
#' models to be trained. Available types depend on the `train_models` function.
#'
#' @return A list of regression models, each corresponding to a specified
#' regression type.
#'
#' @export
regressor <- function(scores, compendium, conditions, regression_type){

  scores <- as.data.frame(scores)

  common_rownames <- intersect(rownames(scores), rownames(compendium))
  scores <- scores[common_rownames, , drop = FALSE]
  compendium <- compendium[common_rownames, , drop = FALSE]

  output <- list()
  for (regression in regression_type){
    regression_models <- train_models(scores, compendium, conditions,
                                      regression)
    output[[regression]] <- regression_models
  }
  return(output)
}


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
#'   \item `"KNN"` for KNN regression.
#' }
#'
#' @return A named list where each element corresponds to a regression model for
#'  a specific condition.
train_models <- function(S, EXPN, conditions, regression = "LM") {
  regression_func <- switch(
    as.character(regression),
    "Randomized_lasso" = Regression_lasso,
    "PLS" = Regression_pls,
    "SVM" = Regression_svm,
    "LM" = Regression_simple,
    "KNN" = Regression_KNN,
    NULL
  )

  if (is.null(regression_func)) {
    stop("Please, choose among the following options:
         \n- Randomized_lasso\n- PLS\n- SVM\n- LM\n- KNN")
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

#' Randomized Lasso Regression
#'
#' This function performs Randomized Lasso regression for stability selection
#' using the \code{randLassoStabSel} function from the \code{monaLisa} package.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the Randomized Lasso regression. This is internally converted to a matrix.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object returned by the \code{randLassoStabSel} function,
#' representing the results of the stability selection process.
#'
#' @importFrom monaLisa randLassoStabSel
Regression_lasso <- function(Scores, TPMs, Condition){
  regr_mona <- monaLisa::randLassoStabSel(as.matrix(Scores), TPMs[,Condition])
  return(regr_mona)
}


#' Partial Least Squares (PLS) Regression
#'
#' This function performs Partial Least Squares (PLS) regression using the
#' \code{plsr} function from the \code{pls} package. It determines the optimal
#'  number of components using cross-validation and builds the final model
#'  accordingly.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the PLS regression. This is internally converted to a matrix.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{mvr} returned by the \code{plsr} function,
#' representing the PLS regression model with the optimal number of components.
#'
#' @importFrom pls plsr
Regression_pls <- function(Scores, TPMs, Condition) {
  ncomp = dim(Scores)[2]
  y <- TPMs[,Condition]
  x <- as.matrix(Scores)

  model_all_component <- pls::plsr(y ~ x, ncomp = ncomp, validation = "CV")
  optimal_ncomp <- which.min(model_all_component$validation$PRESS)
  pls_model <- pls::plsr(y ~ x, ncomp = optimal_ncomp, validation = "CV")
  return(pls_model)
}


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
Regression_simple <- function(Scores, TPMs, Condition){
  regr_simple <- lm(TPMs[,Condition] ~ ., data = Scores)
  return(regr_simple)
}


#' Support Vector Machine (SVM) Regression
#'
#' This function performs SVM regression using the \code{svm} function from
#' the \code{e1071} package with a linear kernel.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the SVM regression. This is internally converted to a matrix.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#'
#' @return An object of class \code{svm} returned by the \code{svm} function,
#' representing the fitted SVM regression model.
#'
#' @importFrom e1071 svm
Regression_svm <- function(Scores, TPMs, Condition){
  regr_svm <- e1071::svm(as.matrix(Scores), TPMs[,Condition], kernel = "linear")
  return(regr_svm)
}

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
