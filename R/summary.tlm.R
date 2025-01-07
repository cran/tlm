#' @rdname summary.tlm
#' @aliases summary.tlm
#' @aliases print.summary.tlm
#' @title Summarizing the Model Fitting
#' @description A \code{summary} method for an object created by the function \code{tlm}.
#'
#' @param object an object of class "\code{tlm}", an output of the \code{\link{tlm}}
#'  function.
#' @param \dots additional arguments.
#'  methods.
#' @details Essentially, the output of \code{\link{summary.lm}} or \code{\link{summary.glm}}
#'  is displayed. In addition, further information on the fitted model is also
#'  displayed.
#' @return A list with class "\code{summary.tlm}" including the following items:
#'  \describe{
#'    \item{model}{the fitted model in the transformed space.}
#'    \item{ypow}{the value of \code{ypow}.}
#'    \item{xpow}{the value of \code{xpow}.}
#'    \item{summary}{the summary of the fitted model provide by \code{summary.lm}
#'     (for gaussian response) or \code{summary.glm} (otherwise).}
#'     }
#' @references Barrera-Gomez J, Basagana X. Models with transformed variables:
#'  interpretation and software. Epidemiology. 2015;26(2):e16-17.
#' @seealso \code{\link{tlm}}, \code{\link{effectInfo}}, \code{\link{MY}}.
#' @examples
#' ### linear model with log-log transformation:
#' data(feld1)
#' modcat <- tlm(logroom ~ logmattress + cat, data = feld1, ypow = 0, xpow = 0)
#' modcat
#' summary(modcat)
#' @usage
#' \method{summary}{tlm}(object, ...)
#'
#' @export
summary.tlm <- function(object, ...) {
  object$summary <- summary(object$model, ...)
  class(object) <- "summary.tlm"
  return(object)
}




#-------------------------------------------
## S3Methods print.summary.tlm
#-------------------------------------------

#' S3Methods for Printing
#'
#' @usage
#' \method{print}{summary.tlm}(x, ...)
#'
#' @param x an object of class "\code{summary.tlm}" (an output of \code{summary.tlm}).
#' @param \dots additional arguments.
#' @export
#' @rdname summary.tlm
print.summary.tlm <- function(x, ...) {
  printPreamble(x)
  print(x$summary, ...)
  cat("\n")
}

