Regression_lasso <- function(Scores, TPMs, Condition){
  regr_mona <- monaLisa::randLassoStabSel(as.matrix(Scores), TPMs[,Condition])
  return(regr_mona)
}