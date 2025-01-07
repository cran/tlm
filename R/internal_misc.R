#' Other internal functions.
#'
#' @description Internal function to detect model type.
#' @param object A fitted tlm.
#' @return internal results.
#' @keywords internal
#' @importFrom stats model.frame
#' @noRd
modelType <- function(object) {
  mod <- object$model
  family <- family(mod)$family
  xpow <- object$xpow
  ypow <- object$ypow
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  Xclass <- attr(mt, "dataClasses")[2]
  nmodels <- 12
  cond <- rep(NA, nmodels)
  cond[1] <- family == "gaussian" && Xclass != "factor" && ypow == 1 && xpow == 1
  cond[2] <- family == "gaussian" && Xclass == "factor" && ypow == 1
  cond[3] <- family == "gaussian" && Xclass != "factor" && ypow == 0 && xpow == 1
  cond[4] <- family == "gaussian" && Xclass == "factor" && ypow == 0
  cond[5] <- family == "gaussian" && Xclass != "factor" && ypow == 1 && xpow == 0
  cond[6] <- family == "gaussian" && Xclass != "factor" && ypow == 0 && xpow == 0
  cond[7] <- family == "binomial" && Xclass != "factor" && xpow == 1
  cond[8] <- family == "binomial" && Xclass == "factor" && xpow == 1
  cond[9] <- family == "binomial" && Xclass != "factor" && xpow == 0
  cond[10] <- family == "poisson" && Xclass != "factor" && xpow == 1
  cond[11] <- family == "poisson" && Xclass == "factor" && xpow == 1
  cond[12] <- family == "poisson" && Xclass != "factor" && xpow == 0
  modeltype <- as.numeric(1:nmodels %*% cond)
  return(modeltype)
}


#' @description Internal function to print preamble for print.tlm.
#' @param x A fitted tlm.
#' @return prints preamble.
#' @keywords internal
#' @noRd
printPreamble <- function(x) {
  family <- family(x$model)$family
  modelname <- switch(family,
                      "gaussian" = "Linear",
                      "binomial" = "Logistic",
                      "poisson" = "Poisson")
  modelname <- paste(modelname, "regression") 
  ytrans <- switch(family,
                   "gaussian" = "none",
                   "binomial" = "logit link for logistic regression",
                   "poisson" = "log link for Poisson regression")
  if (family == "gaussian" && x$ypow != 1) {
    if (x$ypow == 0) ytrans <- "log" else ytrans <- paste("power, exponent =", attr(x, "ypowlabel"))
  }
  if (x$xpow == 1)
    xtrans <- "none" else {
      if (x$xpow == 0) xtrans <- "log" else xtrans <- paste("power, exponent =", attr(x, "xpowlabel"))
    }
  cat("\n")
  if (family == "gaussian" && x$ypow == 1 & x$xpow == 1) {
    cat(modelname, "fitted model \n")
    cat(rep("-", nchar(modelname) + 13, sep = ""), sep = "")
    cat("\n")
  } else {
    cat(modelname, "fitted model in the transformed space\n")
    cat(rep("-", nchar(modelname) + 38, sep = ""), sep = "")
    cat("\n\n")
    cat("Transformations:\n")
    if ((family == "gaussian" && x$ypow != 1) || family != "gaussian")
      cat("   In the response variable:", ytrans, "\n")
    if (x$xpow != 1)
      cat("   In the explanatory variable:", xtrans, "\n")	
  }
}
