#' Internal function to apply a power transformation.
#'
#' @param x numeric. Vector with values of the variable.
#' @param power numeric. Power transformation.
#' @return numeric. Vector with transformed values of the variable.
#' @keywords internal
#' @noRd
powerTransform <- function(x, power) {
  if (power == 0) xt <- log(x) else xt <- x^power
  return(xt)
}


#' Internal function to apply a power untransformation.
#'
#' @param xt numeric. Vector with transformed values of the variable.
#' @param power numeric. Power transformation.
#' @return numeric. Vector with original values of the variable.
#' @keywords internal
#' @noRd
powerUntransform <- function(xt, power) {
  if (power == 0) x <- exp(xt) else x <- xt^(1/power)
  return(x)
}


#' Internal function to check bijectivity back of a power transformation.
#'
#' @param xt numeric. Vector with values of the variable.
#' @param power numeric. Power transformation.
#' @return TRUE or FALSE.
#' @keywords internal
#' @noRd
bijectivityback <- function(xt, power) {
  if (power == 0) {
    res <- TRUE
  } else {
    x <- powerUntransform(xt = xt, power = power)
    newxt <- powerTransform(x = x, power = power)
    res <- all.equal(xt, newxt)
  }
  return(res)
}


#' Internal function to check bijectivity forward of a power transformation.
#'
#' @param xt numeric. Vector with values of the variable.
#' @param power numeric. Power transformation.
#' @return TRUE or FALSE.
#' @keywords internal
#' @noRd
bijectivityforward <- function(x, power) {
  if (power == 0) {
    res <- !any(x <= 0)
  } else {
    xt <- powerTransform(x = x, power = power)
    newx <- powerUntransform(xt = xt, power = power)
    res <- all.equal(xt, newx)
  }
  return(res)
}
