#' @rdname effectInfo
#' @aliases effectInfo
#' @aliases print.effectInfo
#' @title Interpretation of Effects in Linear, Logistic and Poisson Models with
#'  Transformed Variables
#' @description Provides information on interpreting effects in linear, logistic
#'  and Poisson models with transformed variables. Specifically, if a summary
#'  measure for the effect exists, the function details how to obtain and interpret it.
#'  
#' @param object object of class "\code{tlm}" obtained with the function \code{\link{tlm}}.
#' @return A list with class "\code{effectInfo}" including the following items:
#'  \describe{
#'    \item{beta}{regression coefficient estimate in the fitted model which is
#'     associated to the effect of the explanatory variable of interest on the
#'     response variable. \code{NA} corresponds to those models for which a
#'     summary effect does not exist.}
#'    \item{Xincrease}{type of change in the exploratory variable of interest
#'     (additive or realtive) for which a summary effect exists. \code{NA}
#'     corresponds to those models for which a summary effect does not exist.}
#'    \item{effecttype}{type of effect on the response variable for which a
#'     summary effect exists. \code{NA} corresponds to those models for which a
#'     summary effect is not available.}
#'    \item{effectsize}{formula for the summary effect size, if any. \code{NA}
#'     corresponds to those models for which a summary effect is not available.}
#'    \item{furtherinfo}{further information about how to interpret effects.}
#'    }
#' @references Barrera-Gomez J, Basagana X. Models with transformed variables:
#'  interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.
#' @seealso \code{\link{tlm}}, \code{\link{effect}}, \code{\link{MY}}.
#' @examples
#' ### Linear model with log transformation in the explanatory variable:
#' data(cotinine)
#' head(cotinine)
#' # model fitting:
#' modcot <- tlm(weight ~ logcotinine, data = cotinine, xpow = 0)
#' modcot
#' # information on interpreting the effect:
#' effectInfo(modcot)
#' 
#' ### Linear model with no summary measure of effect:
#' data(glucose)
#' head(glucose)
#' # transformations Y^(-2) and X^(-1/2): 
#' modgluco <- tlm(inv2glu ~ inv12tri, data = glucose, ypow = -2, xpow = -1/2)
#' modgluco
#' effectInfo(modgluco)
#' @export
#' @importFrom stats coef
effectInfo <- function(object) {  
  if (!inherits(object, "tlm"))
    stop("argument 'object' must be of class 'tlm'")
    
  if(any(is.na(coef(object$model))))
     stop("effectInfo is not available for models with any missing estimated coefficient")
 
  modeltype <- modelType(object = object)
  eval(parse(text = paste("res <- effectInfomod", modeltype, "(object = object)", sep = "")))
  attr(res, "modeltype") <- modeltype
  class(res) <- "effectInfo"
  return(res)
}



#-------------------------------------------
## S3Methods print.effectInfo
#-------------------------------------------

#' S3Methods for Printing
#'
#' @usage
#' \method{print}{effectInfo}(x, ...)
#'
#' @param x for \code{print.effectInfo}, an object of class "\code{effectInfo}" (an output
#'  of the \code{effectInfo} function).
#' @param \dots additional arguments for \code{print.effectInfo}.
#' 
#' @export
#' @rdname effectInfo
print.effectInfo <- function(x, ...) {  
  if (!inherits(x, "effectInfo"))
    stop("argument 'x' must be of class 'effectInfo'")
  if (attr(x, "modeltype") > 0) {
    cat("\n")
    cat("The effect of X on Y can be summarized with a single number as follows:\n\n")
    cat(" - Change in X:", x$Xincrease, "\n")
    cat(" - Type of effect on Y:", x$effecttype, "\n")
    cat(" - Effect size:", x$effectsize, "\n\n")
    cat("   beta coefficient estimate:\n")
    print(x$beta, ...)
    cat("\nFurther details can be obtained using effect().\n\n")
  } else cat("\nThe effect of X on Y cannot be summarized with a single number.\nIts behavior can be explored using effect().\n\n")
}
