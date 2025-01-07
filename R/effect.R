#' @rdname effect
#' @aliases effect
#' @aliases print.effect
#' @title Effects Estimate in Linear, Logistic and Poisson Regression Models
#'  with Transformed Variables
#' @description Estimates the effect of a explanatory variable of interest on a
#'  response variable, under a fitted linear, logistic or Poisson regression model
#'  with transformed variables. The effect is reported in the original scale of the
#'  variables.
#'  
#' @param object object of class "\code{tlm}", a result of a call to \code{\link{tlm}}.
#' @param x1 numeric. The values of the explanatory variable where the effect
#'  should be computed. See Details below.
#' @param x2 numeric. The alternative values of the explanatory variable
#'  (changing from \code{x1}) for which the effect should be computed. See
#'  Details below.
#' @param c numeric. The additive change in the explanatory variable. See Details
#'  below.
#' @param q numeric. The multiplicative change in the explanatory variable. See
#'  Details below.
#' @param r numeric. The percent change in the explanatory variable. See Details
#'  below.
#' @param npoints numeric. The number of points where the effect should be
#'  computed. See Details below.
#' @param level numeric. Confidence level for the effect estimate. Default is 0.95.
#' @param nboot numeric. The number of non parametric bootstrap samples to compute
#'  confidence intervals. Default is 999. See Details below.
#' @param seed numeric. A single value, the seed for bootstrapping. Default is
#'  4321.
#' @param verbose logical. Whether to print detailed progress on R prompt.
#'  Default is \code{TRUE}.
#' @details In order to compute the effect, both the initial and the final values
#'  of the explanatory should be provided. It can be done in several ways. For
#'  instance, providing, \code{x1} and \code{x2}; \code{x1} and one of \code{c},
#'  \code{q} or \code{r}; \code{x1}, \code{npoints} and one of \code{c},
#'  \code{q} or \code{r}. Only one of the arguments \code{c}, \code{q} or
#'  \code{r} is used, prevailing \code{c} and then \code{q}. If no enough
#'  arguments are passed, the interquartile range will be considered and a
#'  summary effect is computed, if it exists.
#'  
#'  Confidence intervals are computed by transforming the endpoints of the
#'  intervals in the transformed scale when it is possible, while non-parametric
#'  bootstrap is used otherwise.
#' @return A list with class "\code{effect}" including the following items:
#'  \describe{
#'    \item{effect}{point estimate and confidence interval for the effect size.}
#'    \item{info}{information on how to interpret the effect. Used by the function
#'     \code{\link{effectInfo}}.}
#'   }
#' @references Barrera-Gomez J, Basagana X. Models with transformed variables:
#'  interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.
#' @seealso \code{\link{tlm}}, \code{\link{effectInfo}}, \code{\link{MY}}.
#' 
#' @examples
#' ### Linear model with log transformation in the response variable:
#' data(imt)
#' head(imt)
#' 
#' # model fitting:
#' modimt <- tlm(logimt ~ age, data = imt, ypow = 0)
#' modimt
#' 
#' # information on interpreting the effect:
#' effectInfo(modimt)
#' 
#' # the function effect provides as default the expected change in IMT
#' # for an additive change in age equal to the interquartile range:
#' effect(modimt)
#' 
#' # other effects:
#' (minage <- min(imt$age))
#' (maxage <- max(imt$age))
#' effect(modimt, c = maxage - minage)
#' 
#' \dontrun{
#'  effect(modimt, x1 = minage, r = 50, npoints = 3)}
#' @export
#' @importFrom stats coef model.frame model.matrix
effect <- function(object, x1 = NULL, x2 = NULL, c = NULL, q = NULL, r = NULL, npoints = NULL, level = 0.95, nboot = 999, seed = 4321, verbose = TRUE) {
  if (!inherits(object, "tlm"))
    stop("argument 'object' must be of class 'tlm'")
  
  if(any(is.na(coef(object$model))))
    stop("effect is not available for models with any missing estimated coefficient")
  
  if (!inherits(level, "numeric") || level <= 0 || level >= 1 || length(level) != 1)
    stop("'level' must be a number in (0, 1)")
  
  checknbootseed(nboot = nboot, seed = seed)
  if (verbose)
    cat("\nComputing effects...\n\n")
  set.seed(seed)
  modeltype <- modelType(object = object)     
  mod <- object$model
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  dessignMatrix <- model.matrix(mt, data = mf)
  X <- colMeans(dessignMatrix)
  Xclass <- attr(mt, "dataClasses")[2] 
  if (Xclass == "factor") {
    res <- effectFactor(object = object, level = level, nboot = nboot, modeltype = modeltype)
    res$redux <- -1
    unusedarg <- FALSE
    if (!is.null(c(x1, x2, c, q, r, npoints)))
      unusedarg <- TRUE
    res$unusedarg <- unusedarg
  } else {
    res <- effectContinuous(object = object, x1 = x1, x2 = x2, c = c, q = q, r = r, npoints = npoints, level = level, nboot = nboot, modeltype = modeltype)
  }
  res$modeltype <- modeltype
  # check for the order of CI borders:
  effect <- res$effect
  CIlowername <- paste("lower", 100 * level, "%", sep = "")  
  # 1 or 2 effects?:
  neffects <- sum(colnames(effect) == CIlowername)
  aux <- effect
  for (i in 1:neffects) {
    # where is the i-th "lower"?
    wherelower <- which(colnames(effect) == CIlowername)[i]
    if (any(effect[, wherelower] >= effect[, wherelower + 1])) {
      aux[, wherelower] <- effect[, wherelower + 1]
      aux[, wherelower + 1] <- effect[, wherelower]
    }
  }
  res$effect <- aux
  attr(res, "redux") <- res$redux
  res$redux <- NULL
  attr(res, "unusegarg") <- res$unusedarg
  res$unusegarg <- NULL
  attr(res, "modeltype") <- res$modeltype
  res$modeltype <- NULL  
  class(res) <- "effect"
  return(res)
}


#-------------------------------------------
## S3Methods print.effect
#-------------------------------------------

#' S3Methods for Printing

#' @usage
#' \method{print}{effect}(x, ...)
#' 
#' @param x for \code{print.effect}, an object of class "\code{effect}" (an output
#'  of the \code{effect} function).
#' @param \dots additional arguments for \code{print.effect}.
#' @export
#' @rdname effect
print.effect <- function(x, ...) {
  if (!inherits(x, "effect"))
    stop("argument 'x' must be of class 'effecttlm'")
  cat("\n")
  if (attr(x, "unusegarg") == TRUE)
    cat("WARNING: any effect corresponds to the provided arguments.\n\n")
  cat(x$info)
  print(x$effect, ...)
  if (attr(x, "modeltype") > 0) {
    cat("\n")
    cat("For further information on interpreting the effect use effectInfo().\n\n")
  }
}
