#' Internal functions to get effect and info effect for numeric X in model types
#'  1, 3, 5, 6, 7, 9, 10 and 12.
#'
#' @param object A fitted tlm.
#' @param x1x2 numeric vector of length 2. Baseline and alternative X values.
#' @param level numeric. Confidence level.
#' @param nboot numeric. Number of bootstrap samples.
#' @param IQReffect logical. X changing from Q1 to Q3?
#' @param effectsOutRange logical. X out of observed range?
#' @return a list with the effect and its information.
#' @keywords internal
#' @noRd
effectContinuousmod1 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectDiff <- t(apply(x1x2, 1, FUN = function(x) betaCI * (x[2] - x[1])))
  # Percent effect (needs bootstrap):
  auxp <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectPerc <- auxp[, 4:6]
  if (!is.matrix(effectPerc))
    effectPerc <- t(as.matrix(effectPerc))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
   	info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile (confidence interval\nfor the percent change based on ", nboot, " bootstrap samples)", sep = "")
   	} else {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from x1 to x2 (confidence interval for the percent change\nbased on ", nboot, " bootstrap samples)", sep = "")
   	}
  if (any(MY(object)$M$Estimate < 0))
   info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (effectsOutRange)
   info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod3 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (needs bootstrap):
  auxd <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectDiff <- auxd[, 1:3]
  if (!is.matrix(effectDiff))
    effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectPerc <- t(apply(x1x2, 1, FUN = function(x) 100 * (exp(betaCI * (x[2] - x[1])) - 1)))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
    info <- paste("Adjusted change in the geometric mean of the response variable when the\nexplanatory variable changes from the 1st to the 3rd quartile (confidence\ninterval for the difference change based on ", nboot, " bootstrap\nsamples)", sep = "")
  } else {
    info <- paste("Adjusted change in the geometric mean of the response variable when the\nexplanatory variable changes from x1 to x2 (confidence interval for the\ndifference change based on ", nboot, " bootstrap samples)", sep = "")
  }
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod5 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectDiff <- t(apply(x1x2, 1, FUN = function(x) betaCI * log(x[2] / x[1])))
  # Percent effect (needs bootstrap):
  auxp <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectPerc <- auxp[, 4:6]
  if (!is.matrix(effectPerc)) effectPerc <- t(as.matrix(effectPerc))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile (confidence interval\nfor the percent change based on ", nboot, " bootstrap samples)", sep = "")
  } else {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from x1 to x2 (confidence interval for the percent change\nbased on ", nboot, " bootstrap samples)", sep = "")  		
  }
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod6 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (needs bootstrap):
  auxd <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectDiff <- auxd[, 1:3]
  if (!is.matrix(effectDiff))
    effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectPerc <- t(apply(x1x2, 1, FUN = function(x) 100 * ((x[2] / x[1])^betaCI - 1)))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
    info <- paste("Adjusted change in the geometric mean of the response variable when the\nexplanatory variable changes from the 1st to the 3rd quartile (confidence\ninterval for the difference change based on ", nboot, " bootstrap\nsamples)", sep = "")
  } else {
    info <- paste("Adjusted change in the geometric mean of the response variable when the\nexplanatory variable changes from x1 to x2 (confidence interval for the\ndifference change based on ", nboot, " bootstrap samples)", sep = "")
  }
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod7 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # OR effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- t(apply(x1x2, 1, FUN = function(x) exp((x[2] - x[1]) * betaCI)))
  effect <- cbind(x1x2, effect)
  names(effect)[3:5] <- c("OR", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted odds ratio of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile", sep = "")
  } else {
    info <- paste("Adjusted odds ratio of the response variable when the explanatory\nvariable changes from x1 to x2", sep = "")  		
  }
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod9 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # OR effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- t(apply(x1x2, 1, FUN = function(x) (x[2] / x[1]) ^ betaCI))
  effect <- cbind(x1x2, effect)
  names(effect)[3:5] <- c("OR", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted odds ratio of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile", sep = "")
  } else {
    info <- paste("Adjusted odds ratio of the response variable when the explanatory\nvariable changes from x1 to x2", sep = "")
  }
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod10 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (needs bootstrap):
  auxd <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectDiff <- auxd[, 1:3]
  if (!is.matrix(effectDiff))
    effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectPerc <- t(apply(x1x2, 1, FUN = function(x) 100 * (exp(betaCI * (x[2] - x[1])) - 1)))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile\n(confidence interval for the difference change based on\n", nboot, " bootstrap samples)", sep = "")
  } else {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from x1 to x2\n(confidence interval for the difference change based on\n", nboot, " bootstrap samples)", sep = "")
  }
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectContinuousmod1
#' @noRd
effectContinuousmod12 <- function(object, x1x2, level, nboot, IQReffect, effectsOutRange) {
  # Difference effect (needs bootstrap):
  auxd <- t(apply(x1x2, 1, FUN = function (x) geteffectx1x2(object = object, x1 = x[1], x2 = x[2], level = level, nboot = nboot)))
  effectDiff <- auxd[, 1:3]
  if (!is.matrix(effectDiff))
    effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effectPerc <- t(apply(x1x2, 1, FUN = function(x) 100 * ((x[2] / x[1])^betaCI - 1)))
  effect <- cbind(x1x2, effectDiff, effectPerc)
  colnames(effect) <- c(paste("x", 1:2, sep = ""), rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2))
  colnames(effect)[c(3, 6)] <- paste(names(effect)[3], c("Diff", "Percent"), sep = "")
  if (IQReffect) {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from the 1st to the 3rd quartile\n(confidence interval for the difference change based on\n", nboot, " bootstrap samples)", sep = "")
  } else {
    info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from x1 to x2\n(confidence interval for the difference change was based on\n", nboot, " bootstrap samples)", sep = "")
  }
  if (effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}
