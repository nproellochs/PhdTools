#' Regression helpers
#'
#' Normalizes a given numeric input vector.
#' @param x A numeric vector.
#' @export
normalize <- function(x) {(x-min(x))/(max(x)-min(x))}

#' Performs simple stepwise regression.
#' @param dependent Name of the dependent variable.
#' @param independent A vector containing the names of the independent variables.
#' @param data A data frame.
#' @param controlVariables A vector containing the names of the control variables. Default is \code{NULL}.
#' @export
lm_stepwise <- function(dependent, independent, data, controlVariables = NULL) {
  fit <- lapply(1:length(independent), function(x) lm(paste0(dependent, " ~ ",
                                                             paste0(independent[1:x], collapse = " + ")),
                                                      data = data))
  if(!is.null(controlVariables)) {
    fit[[length(fit) + 1]] <- lm(paste0(dependent, " ~ ",
                                        paste0(c(independent, controlVariables), collapse = " + ")),
                                 data = data)
  }
  return(fit)
}

#' Winsorizes a given numeric input vector.
#' @param x A numeric vector.
#' @param fraction Fraction of values to be winsorized. Default is 0.05.
#' @export
winsorize <- function (x, fraction = .05) {
  if (length(fraction) != 1 || fraction < 0 ||
      fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs = c(fraction, 1 - fraction))
  x[x < lim[1]] <- lim[1]
  x[x > lim[2]] <- lim[2]
  x
}

#' Calculates variance inflation factors for dataframe and response variable.
#' @param x A dataframe.
#' @param y A numeric response variable.
#' @return Vector with variance inflation factors for all variables in x.
#' @export
variance_inflation_factors <- function(x, y) {
  ##OLS using all words as regressors
  ols.mod = lm(y ~ . , data = x)
  #remove the linearly dependent variables variables
  ld.vars <- attributes(alias(ols.mod)$Complete)$dimnames[[1]]
  #Refit model
  if (!is.null(ld.vars)) {
    ols.mod.refit = lm(y ~ . , data = x[, -which(colnames(x) %in% ld.vars)])
  } else {
    ols.mod.refit = lm(y ~ . , data = x)
  }
  ols.mod.vif <- vif(ols.mod.refit)
  return(ols.mod.vif)
}

#' Function to perform \code{textreg} export of \code{spikeslab} objects.
#' @param model Object of type \code{spikeslab}.
#' @param standarized A logical value. If \code{TRUE}, standardized coefficient are used (default).
#' @param exludeNoise A logical value. If \code{TRUE}, non-relevant regressors are excluded (default).
#' @return Object of type \code{\link[texreg]{texreg}}.
#' @export
extract.spikeslab <- function(model, standardized = FALSE, excludeNoise = TRUE, ...) {
  # extract information from model object
  coefnames <- model$names
  if (standardized == TRUE) {
    coefs <- model$gnet.scale
  } else {
    coefs <- model$gnet
  }

  predictors <- data.frame(coefnames, coefs, stringsAsFactors = F)
  predictors$inclusionProb <- sapply(c(1:model$verbose[[6]]),
                                     function(x)
                                       sum(sapply(model$model,
                                                  function(y)
                                                    x %in% y)) / model$verbose[[8]])

  if (excludeNoise == TRUE) {
    predictors <- predictors[predictors$coefs != 0,]
  }

  # create and return a texreg object
  tr <- texreg::createTexreg(
    coef.names = predictors$coefnames,
    coef = predictors$coefs,
    gof.names = c("Observations", "Regressors", "MSE"),
    gof = c(length(model$y), sum(predictors$coefs != 0), model$verbose[[9]]),
    gof.decimal = c(TRUE, TRUE, TRUE),
    se = predictors$inclusionProb
  )
  return(tr)
}

#' @importFrom texreg extract
#' @export
setMethod("extract",
          signature = className("spikeslab", "spikeslab"),
          definition = extract.spikeslab)
