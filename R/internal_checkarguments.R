#' Internal function to check the value of an additive change in X.
#'
#' @param c numeric. Additive change.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkc <- function(c) {
  if ((!inherits(c, "integer") && !inherits(c, "numeric")) || length(c) > 1)
    stop("the additive change in X, 'c', must be a number")
}


#' Internal function to check the value of a multiplicative change in X.
#'
#' @param q numeric. Multiplicative change.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkq <- function(q) {
  if ((!inherits(q, "integer") && !inherits(q, "numeric")) || length(q) > 1 || q <= 0)
    stop("the multiplicative change in X, 'q', must be a positive number")
}


#' Internal function to check the value of a percent change in X.
#'
#' @param r numeric. Percent change.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkr <- function(r) {
  if ((!inherits(r, "integer") && !inherits(r, "numeric")) || length(r) > 1)
    stop("the percent change in X, 'r', must be a number")
}


#' Internal function to check validity of x (number).
#'
#' @param x numeric of length 1.
#' @param name character.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkfornumber <- function(x, name) {
  if ((!inherits(x, "integer") && !inherits(x, "numeric")) || length(x) > 1)
    stop(paste("if '", name, "' is provided, it must be a single number", sep = ""))
}


#' Internal function to check validity of x (number or vector).
#'
#' @param x numeric.
#' @param name character.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkfornumberorvector <- function(x, name) {
  if ((!inherits(x, "integer") && !inherits(x, "numeric")))
    stop(paste("'", name, "' must be a number or a vector", sep = ""))
}


#' Internal function to check validity of x (vector).
#'
#' @param x numeric.
#' @param name character.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checkforvector <- function(x, name) {
  mess <- paste("'", name, "' must be a numeric vector", sep = "") 
  cond <- !inherits(x, "integer") && !inherits(x, "numeric")
  if (cond || (!cond && length(x) <= 1))
    stop(mess)
}


#' Internal function to check number of bootstrap samples and seed.
#'
#' @param nboot numeric. Number of bootstrap samples.
#' @param seed numeric. Seed for reproducibility of results.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checknbootseed <- function(nboot, seed) {
  if ((!inherits(nboot, "numeric") && !inherits(nboot, "integer")) || nboot <= 0 || length(nboot) != 1 || ceiling(nboot) != floor(nboot))
    stop("'nboot' must be a positive integer")
  
  if ((!inherits(seed, "numeric") && !inherits(seed, "integer")))
    stop("'seed' must be an integer")
}


#' Internal function to check number of points for effects computation.
#'
#' @param npoints numeric. Number of points.
#' @return stop if not valid.
#' @keywords internal
#' @noRd
checknpoints <- function(npoints) {
  if ((!inherits(npoints, "integer") && !inherits(npoints, "numeric")) || length(npoints) > 1 || ceiling(npoints) != floor(npoints) || npoints < 1)
    stop("'npoints' must be a positive integer")
}

