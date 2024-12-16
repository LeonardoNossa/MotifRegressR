Regression_simple <- function(Scores, TPMs, Condition, alpha){
  regr_simple <- lm(TPMs[,Condition] ~ ., data = Scores)
  return(regr_simple)
}
