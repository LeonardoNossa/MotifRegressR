Regression_svm <- function(Scores, TPMs, Condition){
  regr_svm <- e1071::svm(as.matrix(Scores), TPMs[,Condition], kernel = "linear")
  return(regr_svm)
}