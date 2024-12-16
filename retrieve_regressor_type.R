retrieve_regressor_type <- function(model) {
  if (class(model) == "svm") {
    return("svm")
  } else if (class(model) == "mvr") {
    return("pls")
  } else if (class(model) == "SummarizedExperiment") {
    return("lasso")
  } else if (class(model) == "lm"){
    return("simple")
  }
}