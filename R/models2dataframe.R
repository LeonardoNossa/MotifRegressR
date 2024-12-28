#' Convert Regression Models to Dataframes
#'
#' This function processes a list of regression models (Randomized Lasso, PLS,
#'  SVM, and LM) and converts them into a data frame for further analysis.
#'  The models are unpacked based on their type and conditions.
#'
#' @param regression_list A list of regression models. The names of the list
#' elements should correspond to the model types ("Randomized_lasso", "PLS",
#' "SVM", "LM", "KNN").
#' @param k An integer specifying the number of top features to include for
#' unpacking. Default is 5.
#' @param alpha A numeric value specifying the significance threshold for
#' linear models (required only for LM). Default is NULL.
#'
#' @return A list of data frames, each corresponding to a model type and
#' containing unpacked results for each condition.
#'
#' @export
models2dataframe <- function(regression_list, k = 5, alpha = NULL){

  regression_type <- names(regression_list)

  if (("LM" %in% regression_type && is.null(alpha))) {
    stop("\u03B1 MUST be defined when simple regression is employed")
  }

  if((!("LM" %in% regression_type) && !is.null(alpha))){
    stop("\u03B1 MUST NOT be defined when simple regression is employed")
  }

  output <- list()
  for (idx in seq_along(1:length(regression_list))){

    model_name <- names(regression_list[idx])
    models_list <- regression_list[[idx]]

    if (model_name == "Randomized_lasso"){
      out_list <- lapply(X = seq_along(1:length(models_list)),
                         FUN = function(condition){
                           lasso_sublist <- models_list[condition]
                           output_df <- unpack_lasso(lasso_sublist, k)})

      final_df <- do.call(rbind,out_list)
      output[[model_name]] <- final_df

    } else if (model_name == "PLS") {
      out_list <- lapply(X = seq_along(1:length(models_list)),
                         FUN = function(condition){
                           pls_sublist <- models_list[condition]
                           output_df <- unpack_pls(pls_sublist, k)})

      final_df <- do.call(rbind,out_list)
      output[[model_name]] <- final_df

    } else if(model_name == "SVM"){
      out_list <- lapply(X = seq_along(1:length(models_list)),
                         FUN = function(condition){
                           svm_sublist <- models_list[condition]
                           output_df <- unpack_svm(svm_sublist, k)})

      final_df <- do.call(rbind,out_list)
      output[[model_name]] <- final_df

    } else if(model_name == "LM"){
      out_list <- lapply(X = seq_along(1:length(models_list)),
                         FUN = function(condition){
                           simple_sublist <- models_list[condition]
                           output_df <- unpack_simple(simple_sublist, k, alpha = alpha)})

      final_df <- do.call(rbind,out_list)
      output[[model_name]] <- final_df

    } else if(model_name == "KNN"){
      out_list <- lapply(X = seq_along(1:length(models_list)),
                         FUN = function(condition){
                           KNN_sublist <- models_list[condition]
                           output_df <- unpack_KNN(KNN_sublist, k)})

      final_df <- do.call(rbind,out_list)
      output[[model_name]] <- final_df
    }
  }
  return(output)
}

#' Unpack Lasso
#'
#' This function extracts the top `k` motifs from a Randomized lasso
#' regression model based on their stability selection probabilities.
#'
#' @param lasso_sublist A list containing a single lasso regression model. The
#' model must include metadata with stability selection parameters.
#' @param k An integer specifying the number of top motifs to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The stability selection probabilities (in percentages) of the
#'    motifs.
#' }
#' If no motifs are selected, the function returns `NULL`.
unpack_lasso <- function(lasso_sublist, k){

  lasso_model <- lasso_sublist[[1]]
  metadata <- lasso_model@metadata
  coldata <- lasso_model@colData
  probabilities <- sort(metadata$stabsel.params.max[coldata$selected],
                        decreasing = TRUE) * 100

  if (length(probabilities) == 0) {
    return(NULL)
  }

  condition_name <- rep(x = names(lasso_sublist),
                        times = min(k,length(probabilities)))

  top_k <- probabilities[1:min(k,length(probabilities))]
  top_k_names <- names(top_k)

  output_df <- data.frame(motifs = top_k_names,
                          conditions = condition_name,
                          coef = top_k)
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}

#' Unpack PLS
#'
#' This function extracts the top `k` motifs from a PLS regression model based
#'  on their regression coefficients.
#'
#' @param pls_sublist A list containing a single PLS regression model. The model
#'  must include coefficients for motifs.
#' @param k An integer specifying the number of top motifs to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The regression coefficients of the motifs.
#' }
unpack_pls <- function(pls_sublist, k) {
  pls_model <- pls_sublist[[1]]
  coefficients <- coef(pls_model)
  motif_names <- rownames(coefficients)

  condition_name <- rep(x = names(pls_sublist),
                        times = min(k, length(coefficients)))

  ranking_idxs <- order(abs(coefficients), decreasing = TRUE)
  coefficients <- coefficients[ranking_idxs]
  motif_names <- motif_names[ranking_idxs]

  top_k <- coefficients[1:min(k, length(coefficients))]
  top_k_names <- motif_names[1:min(k, length(coefficients))]

  output_df <- data.frame(motifs = top_k_names,
                          conditions = condition_name,
                          coef = top_k)
  rownames(output_df) <- seq_along(1:nrow(output_df))

  return(output_df)
}

#' Unpack simple
#'
#' This function extracts the top `k` motifs from a simple linear regression
#' model based on their p-values and coefficients.
#'
#' @param simple_sublist A list containing a single linear regression model
#' (object of class `lm`).
#' @param k An integer specifying the number of top motifs to extract.
#' @param alpha A numeric value specifying the significance threshold for
#' p-values (e.g., 0.05).
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs (excluding the intercept term).
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The estimated coefficients of the motifs.
#' }
#' If no coefficients pass the p-value threshold `alpha`, the function returns
#' `NULL`.
unpack_simple <- function(simple_sublist, k, alpha){
  simple_model <- simple_sublist[[1]]

  coefficients <- summary(simple_model)$coefficients

  filtered_coefficients <- coefficients[coefficients[,"Pr(>|t|)"] <= alpha,
                                        , drop = FALSE]

  ordered_filtered_coefficients <-
    filtered_coefficients[order(abs(filtered_coefficients[,"Estimate"]),
                                decreasing = TRUE), , drop = FALSE]

  ordered_filtered_coefficients <-
    ordered_filtered_coefficients[rownames(ordered_filtered_coefficients)
                                  != "(Intercept)", , drop = FALSE]

  if (length(ordered_filtered_coefficients) == 0) {
    return(NULL)
  }

  top_k_coeffients <-
    ordered_filtered_coefficients[1:min(k,
                                        nrow(ordered_filtered_coefficients)),
                                  , drop = FALSE]
  condition_name <- rep(names(simple_sublist), times = nrow(top_k_coeffients))

  output_df <- data.frame(
    motifs = rownames(top_k_coeffients),
    conditions = condition_name,
    coef = as.vector(top_k_coeffients[,"Estimate"])
  )
  output_df$motifs <- gsub("`", "", output_df$motifs)
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}


#' Unpack svm
#'
#' This function extracts the top `k` most important motifs from a
#' trained SVM model based on their feature importance.
#'
#' @param svm_sublist A list containing a single SVM model (object of
#' class `svm`).
#' @param k An integer specifying the number of top features to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The feature importance values for the motifs.
#' }
unpack_svm <- function(svm_sublist, k){
  svm_model <- svm_sublist[[1]]
  weights <- t(svm_model$coefs) %*% svm_model$SV

  feature_importance <- apply(weights, 2, function(x) sqrt(sum(x^2)))

  top_k_indices <-
    order(feature_importance,
          decreasing = TRUE)[1:min(k, length(feature_importance))]
  top_k_importance <- feature_importance[top_k_indices]

  motif_names <- colnames(as.matrix(svm_model$SV))[top_k_indices]

  condition_name <- rep(names(svm_sublist), times = length(top_k_importance))

  output_df <- data.frame(
    motifs = motif_names,
    conditions = condition_name,
    coef = top_k_importance
  )
  rownames(output_df) <- seq_along(1:nrow(output_df))
  return(output_df)
}

#' Unpack KNN
#'
#' This function extracts the top `k` most important motifs from a
#' trained KNN model based on their feature importance.
#'
#' @param KNN_sublist A list containing a single SVM model (object of
#' class `rfe`).
#' @param k An integer specifying the number of top features to extract.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item `motifs`: The names of the top motifs.
#'   \item `conditions`: The name of the condition corresponding to the motifs.
#'   \item `coef`: The feature importance values for the motifs.
#' }
unpack_KNN <- function(KNN_sublist, k){
  rfe_results <- KNN_sublist[[1]]
  rfe_importance <- caret::varImp(rfe_results, scale = TRUE)

  output_df <- data.frame(motifs = rownames(rfe_importance)[1:min(k,nrow(rfe_importance))],
                          conditions = rep(x = names(KNN_sublist), times = min(k,nrow(rfe_importance))),
                          coef = rfe_importance$Overall[1:min(k,nrow(rfe_importance))])
  return(output_df)
}

