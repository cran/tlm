#' Internal main functions to get internal results for effects and info effect.
#'
#' @description Internal function to compute L.
#' @param object A fitted tlm.
#' @param x numeric (vector) with X values.
#' @return internal results.
#' @keywords internal
#' @importFrom stats model.frame model.matrix vcov 
#' @noRd
getL <- function(object, x) {  
  mod <- object$model
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  dessignMatrix <- model.matrix(mt, data = mf)
  X <- colMeans(dessignMatrix)
  Xclass <- attr(mt, "dataClasses")[2]
  if (Xclass != "factor") {
    xt <- powerTransform(x, object$xpow)
    X[2] <- xt
  } else {
    xt <- x
    Xlevels <- levels(mf[, 2])
    whichlevel <- (levels(mf[, 2]) %in% xt)[-1]
    X[2:length(Xlevels)] <- whichlevel
  }
  aux <- summary(mod)$coefficients
  betas <- aux[, "Estimate"]
  Vbetas <- vcov(mod)	
  muL <- as.numeric(betas %*% X)
  varL <- as.numeric(t(X) %*% Vbetas %*% X)
  L <- c(muL, sqrt(varL))
  names(L) <- c("mean", "sd")
  return(L)
}

#' @description Internal function to compute M.
#' @param object A fitted tlm.
#' @param x numeric (vector) with X values.
#' @param untransform logical. Untransform results?
#' @param level numeric. Confidence level.
#' @return internal results.
#' @keywords internal
#' @importFrom stats model.frame qnorm qt
#' @noRd
getM <- function(object, x, untransform, level) {  
  L <- getL(object = object, x = x)
  mod <- object$model 
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  Xclass <- attr(mt, "dataClasses")[2]
  family <- family(mod)$family
  prob <- (1 + level) / 2
  if (any(family == c("binomial", "poisson")))
    z <- qnorm(prob) else  z <- qt(prob, df = mod$df.residual)
  L <- L["mean"] + c(0, -1, 1) * z * L["sd"]
  if (untransform) {
    M <- switch(family,
                gaussian = L,
                binomial = 1 / (1 + exp(-L)),
                poisson = exp(L))
    ypow <- object$ypow
    if (family == "gaussian" && ypow != 1) {
      Mbij <- bijectivityback(M, power = ypow)
      if (!Mbij)
        stop("non bijectivity of the computed expected values")
      M <- powerUntransform(M, ypow) 
    }
  } else {
    M <- L
  }
  names(M) <- c("Estimate", paste(c("lower", "upper"), 100 * level,  "%", sep = ""))
  if (Xclass != "factor") {
    if (untransform) {
      M <- c(x, M)
      names(M)[1] <- "x"
    } else {
      xt <- powerTransform(x, object$xpow)
      M <- c(xt, M)
      names(M)[1] <- "xtrans"
      names(M)[2] <- paste(names(M)[2], "trans", sep = "")
    } } else {
      Xlevels <- levels(mf[, 2])
      xval <- which(Xlevels == x)
      M <- c(xval, M)
      names(M)[1] <- "xlevel"
      if (!untransform)
        names(M)[2] <- paste(names(M)[2], "trans", sep = "")
    }	
  auxlo <- M[paste("lower", 100 * level,  "%", sep = "")]
  auxup <- M[paste("upper", 100 * level,  "%", sep = "")]
  if (any(auxlo > auxup)) {
    M[paste("lower", 100 * level,  "%", sep = "")] <- auxup
    M[paste("upper", 100 * level,  "%", sep = "")] <- auxlo
  }
  return(M)
}


#' @description Internal function to compute effect between x1 and x2.
#' @param object A fitted tlm.
#' @param x1 numeric. Baseline X value.
#' @param x2 numeric. Alternative X value.
#' @param level numeric. Confidence level.
#' @param nboot numeric. Number of bootstrap samples.
#' @return internal results.
#' @keywords internal
#' @importFrom boot boot boot.ci 
#' @noRd
geteffectx1x2 <- function(object, x1, x2, level, nboot) {
  # Point estimate:
  M1 <- getM(object = object, x = x1, untransform = TRUE, level = level)["Estimate"]
  M2 <- getM(object = object, x = x2, untransform = TRUE, level = level)["Estimate"]
  DeltaM <- M2 - M1
  DeltaM100 <- 100 * (M2 / M1 - 1) 
  # Bootstrap CI:
  effectboot <- boot(data = object$model$model, statistic = effectboot, R = nboot, stype = "i", object = object, x1 = x1, x2 = x2, level = level)
  CIDM <- boot.ci(effectboot, conf = level, type = "perc", index = 1)[[4]][4:5]
  CIDM100 <- boot.ci(effectboot, conf = level, type = "perc", index = 2)[[4]][4:5]
  DM <- c(DeltaM, CIDM, DeltaM100, CIDM100)
  aux <- paste(c("lower", "upper"), 100 * level,  "%", sep = "")
  names(DM) <- c("DM", aux, "DM100", aux)
  return(DM)
}


#' @description Internal function to compute effect using bootstrap.
#' @param data Data.
#' @param i Sample indicator.
#' @param object A fitted tlm.
#' @param x1 numeric. Baseline X value.
#' @param x2 numeric. Alternative X value.
#' @param level numeric. Confidence level.
#' @return internal results.
#' @keywords internal
#' @importFrom stats update 
#' @noRd
effectboot <- function(data, i, object, x1, x2, level) {
  d <- data[i, ]
  objecti <- object
  objecti$model <- update(object$model, data = d)
  M1i <- getM(object = objecti, x = x1, untransform = TRUE, level = level)["Estimate"]
  M2i <- getM(object = objecti, x = x2, untransform = TRUE, level = level)["Estimate"]
  res <- c(M2i - M1i, 100 * (M2i / M1i - 1))
  return(res)
}


#' @description Internal function to compute summarized effect with formula.
#' @param object A fitted tlm.
#' @param level numeric. Confidence level.
#' @return internal results.
#' @keywords internal
#' @importFrom stats family qnorm qt
#' @noRd
betaCIContinuousExactTrans <- function(object, level) {
  mod <- object$model
  coefs <- summary(mod)$coefficients
  beta <- coefs[2, 1]
  sdbeta <- coefs[2, 2]
  prob <- (1 + level) / 2
  if (any(family(mod)$family == c("binomial", "poisson")))
    z <- qnorm(prob) else  z <- qt(prob, df = mod$df.residual)
  betaCI <- beta + c(0, -1, 1) * z * sdbeta
  betaCI <- matrix(betaCI, nrow = 1, ncol = 3, byrow = TRUE)
  colnames(betaCI) <- c("beta", paste(50 * (1 + c(-1, 1) * level), "%"))
  betaCI
}


#' @description Internal function to compute summarized effect with formula.
#' @param object A fitted tlm.
#' @param level numeric. Confidence level.
#' @return internal results.
#' @keywords internal
#' @importFrom stats model.frame confint
#' @noRd
effectFactorExactTrans <- function(object, level) {
  mod <- object$model
  mf <- model.frame(mod)
  coefs <- summary(mod)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  x1 <- rep(Xlevels[1], nlevels - 1)
  x2 <- Xlevels[-1]
  x2lab <- paste(x2, ":", sep = "")
  rownameseffect <- paste(x1, x2, sep = " -> ")
  effect <- coefs[2:nlevels, 1]
  CI <- confint(mod, level = level)[2:nlevels, ]
  if (nlevels == 2) {
    effect <- matrix(c(effect, CI), nrow = 1, ncol = 3, byrow = TRUE)
    names(effect)[1] <- "Estimate"
  } else {
    effect <- cbind(effect, CI)
  }
  res <- list(effect = effect, Xbasal = Xlevels[1], rownameseffect = rownameseffect)
  return(res)
}


#' @description Internal function to compute effect with bootstrap.
#' @param object A fitted tlm.
#' @param level numeric. Confidence level.
#' @param nboot numeric. Number of bootstrap samples.
#' @return internal results.
#' @keywords internal
#' @importFrom stats model.frame
#' @noRd
effectFactorBoot <- function(object, level, nboot) {
  mod <- object$model
  mf <- model.frame(mod)
  coefs <- summary(mod)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  x1 <- rep(Xlevels[1], nlevels - 1)
  x2 <- Xlevels[-1]
  x2lab <- paste(x2, ":", sep = "")
  rownameseffect <- paste(x1, x2, sep = " -> ")
  aux <- data.frame(x1 = x1, x2 = x2)
  effect <- t(apply(aux, 1, FUN = function(x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  if (!is.matrix(effect))
    # effect <- matrix(effect, nrow = 1, ncol = 3, byrow = TRUE)
    effect <- matrix(effect, nrow = 1, ncol = 6, byrow = TRUE)
  res <- list(effect = effect, Xbasal = Xlevels[1], rownameseffect = rownameseffect)
  return(res)
}
