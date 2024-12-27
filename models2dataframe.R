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
