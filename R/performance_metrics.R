#' Performance metrics
#'
#' Calculates goodness-of-fit for a combination of predicted and reference values.
#' @param predicted A numeric vector.
#' @param trueValues A numeric vector.
#' @param p The number of predictor variables in the model. Required to calculate adjusted R^2 (optional)
#' @export
GoodnessOfFit = function(predicted, trueValues, p = NULL) {
  n <- length(trueValues)
  TSS <- sum((trueValues - mean(trueValues))^2)
  MSE <- mean((predicted - trueValues)^2)
  cor <- cor(trueValues, predicted)
  R2 <- 1 - sum((trueValues - predicted)^2)/TSS
  if(!is.null(p)) {
    R2.Adj <- 1 - ((n-1)*(1-R2) / (n-p-1))
  } else {
    R2.Adj <- 0
  }
  return(data.frame("MSE" = MSE, "Cor" = cor, "R2" = R2, "AdjR2" = R2.Adj))
}

#' Provides classification diagnostics for a combination of predicted and reference values.
#' @param predicted A numeric vector.
#' @param trueValues A numeric vector.
#' @export
ClassificationDiagnostics <- function (prediction, trueValues) {
  cm <- confusionMatrix(factor(prediction, levels = unique(c(prediction, trueValues))),
                        factor(trueValues, levels = unique(c(prediction, trueValues))))
  result <- data.frame(
    Accuracy = cm$overall["Accuracy"],
    Recall = cm$byClass["Recall"],
    Precision = cm$byClass["Precision"],
    Fscore = cm$byClass["F1"],
    BalancedAccuracy = cm$byClass["Balanced Accuracy"]
  )
  return(result)
}
