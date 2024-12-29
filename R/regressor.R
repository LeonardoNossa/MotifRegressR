#' Regressor
#'
#' This function trains multiple regression models on the provided scores and
#' compendium data. It filters the rows to retain only the common ones between
#' the scores and the compendium before training the models. The function allows
#' for training various types of regression models based on the specified 
#' regression types and their respective parameters.
#'
#' @param scores A data frame or matrix of scores (e.g., feature scores or
#' experimental data).
#' @param compendium A data frame or matrix of the compendium data (e.g.,
#' known conditions or targets for regression).
#' @param conditions A vector of conditions or responses used for training the
#' regression models.
#' @param regression_type A character vector specifying the types of regression
#' models to be trained. Available types depend on the `train_models` function.
#' @param regression_params_list A list where each element corresponds to a 
#' regression model type from `regression_type` and contains a list of 
#' parameters required for training the respective model.
#'
#' @return A list of regression models, each corresponding to a specified
#' regression type.
#'
#' @export
regressor <- function(scores, compendium, conditions, regression_type, 
                      regression_params_list){
  
  scores <- as.data.frame(scores)
  
  common_rownames <- intersect(rownames(scores), rownames(compendium))
  scores <- scores[common_rownames, , drop = FALSE]
  compendium <- compendium[common_rownames, , drop = FALSE]
  
  output <- list()
  for (regression in regression_type){
    regression_params <- regression_params_list[[regression]]
    regression_models <- train_models(scores, compendium, conditions,
                                      regression,regression_params)
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
#' @param regression_params A list of parameters required for the selected 
#' regression model.
#' The structure and content of this list depend on the chosen regression method
#'  and should be specified according to the needs of the individual regression 
#'  function.
#'
#' @return A named list where each element corresponds to a regression model for
#'  a specific condition.
train_models <- function(S, EXPN, conditions, regression,regression_params) {
  regression_func <- switch(
    as.character(regression),
    "Randomized_lasso" = Regression_lasso,
    "PLS" = Regression_pls,
    "SVM" = Regression_svm,
    "LM" = Regression_simple,
    "KNN" = Regression_KNN,
    "RF" = Regression_RF,
    NULL
  )
  
  if (is.null(regression_func)) {
    stop("Please, choose among the following options:
         \n- Randomized_lasso\n- PLS\n- SVM\n- LM\n- KNN\n- RF")
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
      output <- regression_func(S, EXPN, condition_id, regression_params)
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
#' @param params A named list or vector containing parameters for the regression
#'  model. The parameter `cutoff` defines the threshold for stability selection.
#'  If not provided, a default value is used.
#'
#' @return An object returned by the \code{randLassoStabSel} function,
#' representing the results of the stability selection process.
#'
#' @importFrom monaLisa randLassoStabSel
Regression_lasso <- function(Scores, TPMs, Condition, params){
  
  cutoff <- params["cutoff"]
  if (all(is.na(params))) {
    regr_mona <- monaLisa::randLassoStabSel(as.matrix(Scores), TPMs[,Condition])
  } else {
    if (cutoff <= 0.5){
      warning("'cutoff' value is <= 0.5, setting it to 0.51 to avoid errors!")
      cutoff <- 0.51
    } else if (cutoff > 1) {
      warning("'cutoff' value is > 1, setting it to 1 to avoid errors!")
      cutoff <- 1
    }
  }
  regr_mona <- monaLisa::randLassoStabSel(as.matrix(Scores), TPMs[,Condition], 
                                          cutoff = cutoff)
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
#' @param params A named list or vector containing parameters for the regression
#'  model.
#'
#' @return An object of class \code{mvr} returned by the \code{plsr} function,
#' representing the PLS regression model with the optimal number of components.
#'
#' @importFrom pls plsr
Regression_pls <- function(Scores, TPMs, Condition, params) {
  
  if (all(is.na(params))) {
    ncomp = dim(Scores)[2]
    y <- TPMs[,Condition]
    x <- as.matrix(Scores)
    
    model_all_component <- pls::plsr(y ~ x, ncomp = ncomp, validation = "CV")
    optimal_ncomp <- which.min(model_all_component$validation$PRESS)
    pls_model <- pls::plsr(y ~ x, ncomp = optimal_ncomp, validation = "CV")
  }
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
#' @param params A named list or vector containing parameters for the regression
#'  model.
#'
#' @return An object of class \code{lm} returned by the \code{lm} function,
#' representing the fitted linear model.
#'
#' @importFrom stats lm
Regression_simple <- function(Scores, TPMs, Condition, params){
  
  if (all(is.na(params))) {
    regr_simple <- lm(TPMs[,Condition] ~ ., data = Scores)
  }
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
#' @param params A named list or vector containing parameters for the regression
#'  model.
#' The parameter \code{kernel} specifies the kernel function to be used in the 
#' SVM. Supported kernels are: "linear", "radial", "polynomial", and "sigmoid". 
#' If not specified, the default is "radial".
#'
#' @return An object of class \code{svm} returned by the \code{svm} function,
#' representing the fitted SVM regression model.
#'
#' @importFrom e1071 svm
Regression_svm <- function(Scores, TPMs, Condition, params){
  
  kernel <- params["kernel"]
  accepted_kernels <- c("linear", "radial", "polynomial", "sigmoid")
  
  if (all(is.na(params))) {
    regr_svm <- e1071::svm(as.matrix(Scores), TPMs[,Condition])
  } else {
    if (!kernel%in%accepted_kernels) {
      warning("'kernel' NOT supported, defaulting to radial kernel")
      kernel <- "radial"
    }
    
    regr_svm <- e1071::svm(as.matrix(Scores), TPMs[,Condition], kernel = kernel)
  }
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
#' @param params A named list or vector containing parameters for the regression
#' model:
#' \describe{
#'   \item{\code{num_cores}}{(Optional) Number of CPU cores to use for 
#'   parallelization. Default is the total number of available cores. 
#'   If a value greater than the available cores is provided, it will be 
#'   adjusted accordingly.}
#'   \item{\code{cv}}{(Optional) Number of cross-validation folds. Default is 3.
#'   If a value greater than the number of observations is provided, it will 
#'   default to Leave-One-Out Cross-Validation (LOOCV).}
#' }
#'
#' @return An object of class \code{RFE} representing the fitted KNN regression 
#' model.
#'
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom caret rfeControl caretFuncs rfe
Regression_KNN <- function(Scores, TPMs, Condition, params){
  
  suppressPackageStartupMessages(library(caret))
  
  num_cores <- params["num_cores"]
  cv <- params["cv"]
  
  if (all(is.na(params))) {
    num_cores <- parallel::detectCores()
    cv <- 3
    
  } else if ((is.na(num_cores)) && (!is.na(cv))) {
    num_cores <- parallel::detectCores()
    if (cv <= 1){
      warning("'cv' value is <= 1, setting it to 2 to avoid errors!")
      cv <- 2
    } else if (cv > nrow(Scores)){
      warning("'cv' value is > [number of data points], setting it to 
              [number of data points] to avoid errors! [LOOCV]")
      cv <- nrow(Scores)
    }
    
  } else if ((is.na(cv)) && (!is.na(num_cores))) {
    cv <- 3
    if (num_cores < 1){
      warning("'num_cores' value is < 1, setting it to 1 to avoid errors!")
      num_cores <- 1
    } else if (num_cores > parallel::detectCores()){
      warning("'num_cores' value is > [number of available cores], setting it to
              [number of available cores] to avoid errors!")
      num_cores <- parallel::detectCores()
    }
    
  } else {
    if (cv <= 1){
      warning("'cv' value is <= 1, setting it to 2 to avoid errors!")
      cv <- 2
    } else if (cv > nrow(Scores)){
      warning("'cv' value is > [number of data points], setting it to [number of
              data points] to avoid errors! [LOOCV]")
      cv <- nrow(Scores)
    }
    if (num_cores < 1){
      warning("'num_cores' value is < 1, setting it to 1 to avoid errors!")
      num_cores <- 1
    } else if (num_cores > parallel::detectCores()){
      warning("'num_cores' value is > [number of available cores], setting it to
              [number of available cores] to avoid errors!")
      num_cores <- parallel::detectCores()
    }
  }
  
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  rfe_control <- suppressWarnings(caret::rfeControl(
    functions = caret::caretFuncs,
    method = "cv",
    number = cv,
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

#' Random Forest Regression
#'
#' This function performs Random Forest regression using the
#' \code{randomForest} function from the \code{randomForest} package.
#'
#' @param Scores A matrix or data frame of scores to be used as predictors in
#' the Random Forest regression. This is converted to a matrix internally.
#' @param TPMs A data frame or matrix of TPM (Transcripts Per Million) values.
#' Columns correspond to different conditions or samples.
#' @param Condition A character string specifying the column name in \code{TPMs}
#' corresponding to the condition to be analyzed.
#' @param params A named list or vector containing parameters for the Random
#' Forest model:
#' \describe{
#'   \item{\code{ntree}}{(Optional) Number of trees to grow in the Random 
#'   Forest. Default is 500. If a value less than 1 is provided, it will be set 
#'   to 1.}
#'   \item{\code{mtry}}{(Optional) Number of variables randomly sampled as 
#'   candidates at each split. Default is \code{sqrt(number of predictors)}. 
#'   If a value less than 1 is provided, it will be set to 1. If a value greater
#'    than the number of predictors is provided, it will be adjusted to match 
#'    the number of predictors.}
#' }
#'
#' @return An object of class \code{randomForest} returned by the
#' \code{randomForest} function, representing the fitted Random Forest
#' regression model.
#'
#' @importFrom randomForest randomForest
Regression_RF <- function(Scores, TPMs, Condition, params) {
  
  ntree <- params["ntree"]
  mtry <- params["mtry"]
  
  if (all(is.na(params))) {
    ntree <- 500
    mtry <- floor(sqrt(ncol(Scores)))
    
  } else if ((is.na(ntree)) && (!is.na(mtry))) {
    ntree <- 500
    if (mtry < 1) {
      warning("'mtry' < 1, setting it to 1 to avoid errors!")
      mtry <- 1
    } else if (mtry > ncol(Scores)){
      warning("'mtry' > [number of features], setting it to [number of features]
              to avoid errors!")
      mtry <- ncol(Scores)
    }
    
  } else if ((is.na(mtry)) && (!is.na(ntree))) {
    mtry <- floor(sqrt(ncol(Scores)))
    if (ntree < 1){
      warning("'ntree' < 1, setting it to 1 to avoid errors!")
      ntree <- 1
    }
    
  } else {
    if (ntree < 1){
      warning("'ntree' < 1, setting it to 1 to avoid errors!")
      ntree <- 1
    }
    if (mtry < 1) {
      warning("'mtry' < 1, setting it to 1 to avoid errors!")
      mtry <- 1
    } else if (mtry > ncol(Scores)){
      warning("'mtry' > [number of features], setting it to [number of features]
              to avoid errors!")
      mtry <- ncol(Scores)
    }
  }
  
  Scores <- as.matrix(Scores)
  EXP <- TPMs[, Condition]
  model <- randomForest::randomForest(x = Scores,
                                      y = EXP,
                                      ntree = ntree,
                                      mtry = mtry)
  return(model)
}


#' Retrieve Regression Parameter List
#'
#' This function generates a named list containing the parameters for various 
#' regression models, including Randomized Lasso, PLS (Partial Least Squares), 
#' Linear Model (LM), K-Nearest Neighbors (KNN), Support Vector Machines (SVM), 
#' and Random Forest (RF).
#' Users can supply specific values for each regression method's parameters, 
#' or leave them as `NA` to use defaults.
#'
#' @param Regression_lasso_cutoff Numeric. Cutoff value for Randomized Lasso 
#' regression. Defaults to `NA`.
#' @param Regression_svm_kernel Character. Specifies the kernel type for SVM 
#' (e.g., "linear", "radial"). Defaults to `NA`.
#' @param Regression_KNN_num_cores Integer. Number of cores to use for parallel 
#' processing in KNN. Defaults to `NA`.
#' @param Regression_KNN_cv Integer. Number of cross-validation folds for KNN.
#' Defaults to `NA`.
#' @param Regression_RF_ntree Integer. Number of trees to grow in the Random 
#' Forest. Defaults to `NA`.
#' @param Regression_RF_mtry Integer. Number of variables randomly sampled as 
#' candidates at each split in Random Forest. Defaults to `NA`.
#'
#' @return A named list of vectors, where each vector contains parameters 
#' specific to one regression model:
#' \itemize{
#'   \item \code{Randomized_lasso}: A vector containing the \code{cutoff} 
#'   parameter.
#'   \item \code{PLS}: `NA` (placeholder for Partial Least Squares parameters).
#'   \item \code{LM}: `NA` (placeholder for Linear Model parameters).
#'   \item \code{KNN}: A vector containing \code{num_cores} and \code{cv} 
#'   parameters.
#'   \item \code{SVM}: A vector containing the \code{kernel} parameter.
#'   \item \code{RF}: A vector containing \code{ntree} and \code{mtry} 
#'   parameters.
#' }
#'
#' @export
retrieve_params_list <- function(Regression_lasso_cutoff = NA,
                                 Regression_svm_kernel = NA,
                                 Regression_KNN_num_cores = NA,
                                 Regression_KNN_cv = NA,
                                 Regression_RF_ntree = NA,
                                 Regression_RF_mtry = NA){
  
  pls <- NA
  simple <- NA
  
  lasso <- c(cutoff = Regression_lasso_cutoff)
  svm <- c(kernel = Regression_svm_kernel)
  
  knn <- c(num_cores = Regression_KNN_num_cores,
           cv = Regression_KNN_cv)
  
  rf <- c(ntree = Regression_RF_ntree,
          mtry = Regression_RF_mtry)
  
  params_list <- list(Randomized_lasso = lasso,
                      PLS = pls,
                      LM = simple,
                      KNN = knn,
                      SVM = svm,
                      RF = rf)
  
  return(params_list)
}


