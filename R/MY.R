#' @rdname MY
#' @aliases MY
#' @aliases print.MY
#' @title Expected Adjusted Median or Generalized Mean
#' @description Computes expected measures of the response variable under a linear,
#'  logistic or Poisson regression fitted model with transformed variables. Measures
#'  can be reported in both the original and the transformed space. The function
#'  automatically provides the name of the measure depending on the fitted model.
#'  
#' @param object object of class \code{tlm}, a result of a call to \code{\link{tlm}}.
#' @param x For \code{MY}, a number or a numeric vector containing the values of
#'  the explanatory variable of interest for which the expected measure of the
#'  response variable are required. Default is \code{NULL}. For \code{print.MY}, an
#'  object of class "\code{MY}" (an output of the \code{MY} function).
#' @param npoints numeric. If \code{x} is \code{NULL}, the number of points where
#'  the measure should be measured. Default is 10. See Details below.
#' @param space character. If "\code{original}" (default), the measure is reported
#'  in the original space of the variables. If "\code{transformed}", the measure
#'  is reported in the transformed space of the variables, where the model is fitted.
#' @param level numeric. The confidence level for measures. Default is 0.95.
#'
#' @details In order to compute adjusted measures, all explanatory variables in
#'  the model different than the explanatory variable of interest are set at their
#'  means.
#'  
#'  If \code{space} is "\code{original}", then the mean (for Poisson response) or
#'  the probability (for binary response) is computed. For gaussian response, the
#'  mean is computed if the response variable is not transformed; otherwise, the
#'  geometric mean (for log transformation in the response) or the median (for
#'  power transformation) is computed.
#'  
#'  If \code{space} is "\code{transformed}", then the mean (for Poisson response
#'  or transformed gaussian response), or the logodds (for binary response) is
#'  computed.
#'  
#'  If \code{x} argument in \code{MY} is \code{NULL}, the measure is computed in
#'  \code{npoints} values of the explanatory variable of interest. Those values
#'  are chosen to be in arithmetic progression in the given \code{space}, inside
#'  the observed range of the explanatory variable. 
#' @return A list with class "\code{MY}" including the following items:
#'  \describe{
#'    \item{M}{adjusted measure of the response variable. See Details below.}
#'    \item{ymeasure}{the type of measure for \code{M}.}
#'    \item{ypow}{numeric power transformation assumed in the response variable.
#'     See \code{\link{tlm}}.}
#'    \item{xpow}{numeric power transformation assumed in the explanatory variable
#'     of interest. See \code{\link{tlm}}.}
#'   }
#' @references Barrera-Gomez J, Basagana X. Models with transformed variables:
#'  interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.
#' @seealso \code{\link{tlm}}, \code{\link{effectInfo}}, \code{\link{effect}}.
#' @examples
#' data(feld1)
#' head(feld1)
#' 
#' # Linear model with log-log transformation, adjusting for variable 'cat':
#' modcat <-  tlm(logroom ~ logmattress + cat, data = feld1, ypow = 0, xpow = 0)
#' summary(modcat)
#' 
#' # Geometric mean of the response as a function of the explanatory variable,
#' # adjusted for 'cat':  
#' MY(modcat)
#' MY(modcat, npoints = 3)
#' # computed at 1st and 3rd quartiles of the explanatory variable:
#' MY(modcat, x = quantile(feld1$mattress, probs = c(1, 3)/4))
#' 
#' # Mean of the log(response) as a function of the log explanatory variable,
#' # adjusted for 'cat':  
#' MY(modcat, space = "transformed")
#' @export
#' @importFrom stats coef family model.frame model.matrix 
MY <- function(object, x = NULL, npoints = 10, space = c("original", "transformed"), level = 0.95) { 
  if (!inherits(object, "tlm"))
    stop("argument 'object' must be of class 'tlm'")
  
  if(any(is.na(coef(object$model))))
    stop("MY is not available for models with any missing estimated coefficient")
  
  if (!is.null(x) && !inherits(x, "numeric") && !inherits(x, "integer"))
    stop("'x' must be a number or a numeric vector")
  
  ypow <- object$ypow
  xpow <- object$xpow
  
  if (!is.null(x) && xpow != 1) {
    xbij <- bijectivityforward(x, power = xpow)
    if (!xbij)
      stop("non bijectivity of the provided 'x'")
  }
  
  if ((!inherits(npoints, "numeric") && !inherits(npoints, "integer")) || npoints <= 0 || length(npoints) != 1)
    stop("'npoints' must be a positive integer")
  
  space <- match.arg(space) 
  
  if (!inherits(level, "numeric")  || level <= 0 || level >= 1 || length(level) != 1)
    stop("'level' must be a number in (0, 1)")
  
  if (space == "transformed" && family(object$model)$family == "gaussian" && xpow == 1 && object$ypow == 1)
    stop("there is no transformations in this model")
  
  mod <- object$model
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  dessignMatrix <- model.matrix(mt, data = mf)
  Xclass <- attr(mt, "dataClasses")[2]
  cond1 <- Xclass != "factor"
  cond2 <- is.null(x) || !inherits(x, "numeric") && !inherits(x, "integer")
  cond3 <- is.null(npoints) || !inherits(npoints, "numeric") && !inherits(npoints, "integer") || length(npoints) != 1L
  if (cond1 && cond1 && cond3)
    stop("either a numeric value (or vector) 'x' or the number of points 'npoints' must be provided")
  
  if (Xclass == "factor") {
    xvector <- levels(mf[, 2])
  } else {
    if (!is.null(x))
      xvector <- as.list(x) else
      {
        # Puntos equiespaciados en el espacio correspondiente:
        xtransmin <- min(dessignMatrix[, 2])	
        xtransmax <- max(dessignMatrix[, 2])
        if (space == "original")
          # secuencia de npoints puntos equidistantes en original
          xvector <- seq(powerUntransform(xtransmin, xpow), powerUntransform(xtransmax, xpow), length.out = npoints) else
            # secuencia de npoints puntos equidistantes en transformed y destransformar
            xvector <- powerUntransform(seq(xtransmin, xtransmax, length.out = npoints), xpow)
      }	
  }
  res <- as.data.frame(t(sapply(xvector, FUN = function(y) getM(object = object, x = y, untransform = space == "original", level = level))))
  if (Xclass == "factor") {
    res[, 1] <- as.factor(res[, 1])
    levels(res[, 1]) <- levels(mf[, 2])
    rownames(res) <- NULL
  }
  if (Xclass != "factor") {
    ord <- order(res[, 1])
    res <- res[ord, ]
    rownames(res) <- NULL
  }
  ymeasure <- switch(family(mod)$family, gaussian = "mean", binomial = "probability", poisson = "mean")
  Mname <- switch(family(mod)$family, gaussian = "mean(Y)", binomial = "P(Y)", poisson = "mean(Y)")
  Xname <- "X"
  if (space == "original" && family(mod)$family == "gaussian" && ypow != 1) {
    if (ypow == 0) {
      ymeasure <- "geometric mean"
      Mname <- "geomMean(Y)" 
    } else {
      ymeasure <- "median"
      Mname <- "median(Y)"
    }
  }
  if (space == "transformed") {
    if (xpow != 1) {
      if (xpow == 0) Xname <- "log(X)" else Xname <- "X transf."
    }
    ymeasure <- switch(family(mod)$family, gaussian = "mean", binomial = "log(odds)", poisson = "log(mean)")
    Mname <- switch(family(mod)$family, gaussian = "mean(Y)", binomial = "log(odds(Y))", poisson = "log(mean(Y))")
    if (ypow != 1) {
      if (ypow == 0) Mname <- "mean(log(Y))" else Mname <- "mean(Ytrans)"
    }
  }
  names(res)[2] <- Mname
  if (Xclass != "factor")
    names(res)[1] <- Xname
  M <- list(M = res, ymeasure = ymeasure, space = space, ypow = ypow, xpow = xpow)
  class(M) <- "MY"
  return(M)
}



#-------------------------------------------
## S3Methods print.MY
#-------------------------------------------

#' S3Methods for Printing
#'
#' @usage
#' \method{print}{MY}(x, ...)
#'
#' @param \dots additional arguments for \code{print.MY}.
#' 
#' @export
#' @rdname MY
print.MY <- function(x, ...) {
  if (!inherits(x, "MY"))
    stop("argument 'x' must be of class 'MY'")
  cat("\n") 
  if (x$ypow == 1 & x$xpow == 1)
    cat("Estimated adjusted ", x$ymeasure, " of the response variable:\n\n", sep = "")
  else
    cat("Estimated adjusted ", x$ymeasure, " of the response variable in the ", x$space, " space:\n\n", sep = "")
  print(x$M, ...)
  cat("\n")
}
