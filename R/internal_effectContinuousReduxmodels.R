#' Internal functions to get summarized effect and info effect for numeric X in
#'  model types 1, 3, 7 and 10 (additive change in X).
#'
#' @param object see getL.
#' @param c numeric. Additive change in X.
#' @param IQReffect logical. X changing from Q1 to Q3?
#' @param effectsOutRange logical. X out of observed range?
#' @param level numeric. Confidence level.
#' @return a list with the effect and its information.
#' @keywords internal
#' @noRd
effectContinuousReduxmod1 <- function(object, c, IQReffect, effectsOutRange, level) {
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- betaCI * c
  effect <- data.frame(c = c, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted additive change in the mean of the response variable for a\n'c' units additive change in the explanatory variable equivalent to the\ninterquartile range")
  } else {
    info <- paste("Adjusted additive change in the mean of the response variable for a\n'c' =", c, " units additive change in the explanatory variable")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousReduxmod1
#' @noRd
effectContinuousReduxmod3 <- function(object, c, IQReffect, effectsOutRange, level) {
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- 100 * (exp(betaCI * c) - 1)
  effect <- data.frame(c = c, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted percent change in the geometric mean of the response variable\nfor a 'c' units additive change in the explanatory variable equivalent\nto the interquartile range")
  } else {
    info <- paste("Adjusted percent change in the geometric mean of the response variable\nfor a 'c' =", c, "units additive change in the explanatory variable")
  }
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousReduxmod1
#' @noRd
effectContinuousReduxmod7 <- function(object, c, IQReffect, effectsOutRange, level) {
  # OR effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- exp(c * betaCI)
  effect <- data.frame(c = c, effect)
  names(effect)[-1] <- c("OR", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted odds ratio of the response variable for a 'c' units additive\nchange in the explanatory variable equivalent to the interquartile range")
  } else {
    info <- paste("Adjusted odds ratio of the response variable for a 'c' =", c, "\nunits additive change in the explanatory variable")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

#' @rdname effectContinuousReduxmod1
#' @noRd
effectContinuousReduxmod10 <- function(object, c, IQReffect, effectsOutRange, level) {
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  effect <- 100 * (exp(betaCI * c) - 1)
  effect <- data.frame(c = c, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted percent change in the mean of the response variable for a\n 'c' units additive change in the explanatory variable equivalent to\nthe interquartile range")
  } else {
    info <- paste("Adjusted percent change in the mean of the response variable for a\n'c' =", c, "units additive change in the explanatory variable")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}



#' Internal functions to get summarized effect and info effect for numeric X in
#'  model types 5, 6, 9 and 12 (multiplicative change in X).
#'
#' @param object see getL.
#' @param r numeric. Percent change in X.
#' @param IQReffect logical. X changing from Q1 to Q3?
#' @param effectsOutRange logical. X out of observed range?
#' @param level numeric. Confidence level.
#' @return a list with the effect and its information.
#' @keywords internal
#' @noRd
effectContinuousReduxmod5 <- function(object, r, IQReffect, effectsOutRange, level) {
  # Difference effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  q <- 1 + r / 100
  effect <- betaCI * log(q)
  effect <- data.frame(r = r, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted additive change in the mean of the response variable for an\n'r'% change in the explanatory variable equivalent to the interquartile\nratio")
  } else {
    info <- paste("Adjusted additive change in the mean of the response variable for an\n'r' = ", r, "% change in the explanatory variable", sep = "")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectContinuousReduxmod5
#' @noRd
effectContinuousReduxmod6 <- function(object, r, IQReffect, effectsOutRange, level) {
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  q <- 1 + r / 100
  effect <- 100 * (q^betaCI - 1)
  effect <- data.frame(r = r, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted percent change in the geometric mean of the response variable\nfor an 'r'% change in the explanatory variable equivalent to the\ninterquartile ratio")
  } else {
    info <- paste("Adjusted percent change in the geometric mean of the response variable\nfor an 'r' = ", r, "% change in the explanatory variable", sep = "")
  }
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectContinuousReduxmod5
#' @noRd
effectContinuousReduxmod9 <- function(object, r, IQReffect, effectsOutRange, level) {
  # OR effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  q <- 1 + r / 100
  effect <- q ^ betaCI
  effect <- data.frame(r = r, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted odds ratio of the response variable for an 'r'% change in\nthe explanatory variable equivalent to the interquartile ratio", sep = "")
  } else {
    info <- paste("Adjusted odds ratio of the response variable for an 'r' = ", r, "% change\nin the explanatory variable", sep = "")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectContinuousReduxmod5
#' @noRd
effectContinuousReduxmod12 <- function(object, r, IQReffect, effectsOutRange, level) {
  # Percent effect (doesn't needs bootstrap):
  betaCI <- betaCIContinuousExactTrans(object = object, level = level)
  q <- 1 + r / 100
  effect <- 100 * (q ^ betaCI - 1)
  effect <- data.frame(r = r, effect)
  names(effect)[-1] <- c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  if (IQReffect) {
    info <- paste("Adjusted percent change in the mean of the response variable for an\n'r'% change in the explanatory variable equivalent to the interquartile\nratio", sep = "")
  } else {
    info <- paste("Adjusted percent change in the mean of the response variable for an 'r' = ", r, "%\nchange in the explanatory variable", sep = "")
  }
  if (!IQReffect & effectsOutRange)
    info <- paste("WARNING: computing effects out of the observed range of X.\n\n", info, sep = "")          
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}

