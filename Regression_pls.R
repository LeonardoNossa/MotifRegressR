Regression_pls <- function(Scores, TPMs, Condition) {
  ncomp = dim(Scores)[2]
  y <- TPMs[,Condition]
  x <- as.matrix(Scores)
  
  model_all_component <- pls::plsr(y ~ x, ncomp = ncomp, validation = "CV")
  optimal_ncomp <- which.min(model_all_component$validation$PRESS)
  pls_model <- pls::plsr(y ~ x, ncomp = optimal_ncomp, validation = "CV")
  return(pls_model)
}