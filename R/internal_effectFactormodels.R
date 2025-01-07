#' Internal functions to get effect and info effect for factor X in model types
#'  2, 4, 8 and 11, and main function.
#'
#' @param object A fitted tlm.
#' @param level numeric. Confidence level.
#' @param nboot numeric. Number of bootstrap samples.
#' @return a list with the effect and its information.
#' @keywords internal
#' @noRd
effectFactormod2 <- function(object, level, nboot) {
  # Difference effect (doesn't needs bootstrap):
  auxd <- effectFactorExactTrans(object = object, level = level)
  effectDiff <- auxd$effect
  # Percent effect (needs bootstrap):
  auxp <- effectFactorBoot(object = object, level = level, nboot = nboot)
  effectPerc <- auxp$effect[, 4:6]
  if (!is.matrix(effectPerc)) effectPerc <- t(as.matrix(effectPerc))
  effect <- cbind(effectDiff, effectPerc)
  colnames(effect) <- rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2)
  colnames(effect)[c(1, 4)] <- paste(colnames(effect)[1], c("Diff", "Percent"), sep = "")  
  rownames(effect) <- auxd$rownameseffect
  info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from its reference level, '", auxd$Xbasal, "', to an alternative level\n(confidence interval for the percent change based on ", nboot, " bootstrap samples)", sep = "")
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")      
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}



#' @rdname effectFactormod2
#' @noRd
effectFactormod4 <- function(object, level, nboot) {
  # Difference effect (needs bootstrap):
  auxd <- effectFactorBoot(object = object, level = level, nboot = nboot)
  effectDiff <- auxd$effect[, 1:3]
  if (!is.matrix(effectDiff)) effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doesn't needs bootstrap):
  auxp <- effectFactorExactTrans(object = object, level = level)
  effectPerc <- 100 * (exp(auxp$effect) - 1)
  effect <- cbind(effectDiff, effectPerc)
  colnames(effect) <- rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2)
  colnames(effect)[c(1, 4)] <- paste(colnames(effect)[1], c("Diff", "Percent"), sep = "")  
  rownames(effect) <- auxd$rownameseffect
  info <- paste("Adjusted change in the geometric mean of the response variable when\nthe explanatory variable changes from its reference level, '",auxd$Xbasal, "', to\nan alternative level (confidence interval for the difference based\non ", nboot, " bootstrap samples)", sep = "")
  if (any(MY(object)$M$Estimate < 0))
    info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted mean.\n\n", info, sep = "")      
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectFactormod2
#' @noRd
effectFactormod8 <- function(object, level, nboot) {
  # OR effect (doesn't needs bootstrap):
  aux <- effectFactorExactTrans(object = object, level = level)
  effect <- exp(aux$effect)
  info <- paste("Adjusted odds ratio of the response variable when the explanatory\nvariable changes from its reference level, '", aux$Xbasal, "', to an alternative level", sep = "")
  info <- paste(info, ":\n\n", sep = "")
  colnames(effect) <- c("OR", paste(c("lower", "upper"), 100 * level, "%", sep = ""))
  rownames(effect) <- aux$rownameseffect
  res <- list(effect = effect, info = info)
  return(res)
}


#' @rdname effectFactormod2
#' @noRd
effectFactormod11 <- function(object, level, nboot) {
  # Difference effect (needs bootstrap):
  auxd <- effectFactorBoot(object = object, level = level, nboot = nboot)
  effectDiff <- auxd$effect[, 1:3]
  if (!is.matrix(effectDiff)) effectDiff <- t(as.matrix(effectDiff))
  # Percent effect (doen't needs bootstrap):
  auxp <- effectFactorExactTrans(object = object, level = level)
  effectPerc <- 100 * (exp(auxp$effect) - 1)
  effect <- cbind(effectDiff, effectPerc)
  colnames(effect) <- rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2)
  colnames(effect)[c(1, 4)] <- paste(colnames(effect)[1], c("Diff", "Percent"), sep = "")  
  rownames(effect) <- auxd$rownameseffect
  info <- paste("Adjusted change in the mean of the response variable when the explanatory\nvariable changes from its reference level, '", auxd$Xbasal, "', to an alternative level\n(confidence interval for the difference change based on\n", nboot, " bootstrap samples)", sep = "")
  info <- paste(info, ":\n\n", sep = "")
  res <- list(effect = effect, info = info)
  return(res)
}


#' Main function.
#'
#' @param object A fitted tlm.
#' @param level numeric. Confidence level.
#' @param nboot numeric. Number of bootstrap samples.
#' @param modeltype numeric. Model type (set internally).
#' @return a list with the effect and its information.
#' @keywords internal
#' @noRd
effectFactor <- function(object, level, nboot, modeltype) { 
  # No/Yes bootstrap cases (depending on the effect scale):
  if(any(modeltype %in% c(2, 4, 8, 11))) {
    eval(parse(text = paste("res <- effectFactormod", modeltype, "(object = object, level = level, nboot = nboot)", sep = "")))
  } else {
    # Bootstrap cases:
    aux <- effectFactorBoot(object = object, level = level, nboot = nboot)
    effect <- aux$effect
    info <- paste("Adjusted change in the median of the response variable when the explanatory\nvariable changes from its reference level, '", aux$Xbasal, "', to an alternative level\n(based on ", nboot, " bootstrap samples)", sep = "")    
    if (any(MY(object)$M$Estimate < 0))
      info <- paste("WARNING: percent scale for effects not suitable because of negative values\nfor the adjusted median.\n\n", info, sep = "")    
    info <- paste(info, ":\n\n", sep = "")
    colnames(effect) <- rep(c("Estimate", paste(c("lower", "upper"), 100 * level, "%", sep = "")), 2)
    colnames(effect)[c(1, 4)] <- paste(colnames(effect)[1], c("Diff", "Percent"), sep = "")  
    rownames(effect) <- aux$rownameseffect
    res <- list(effect = effect, info = info)
  }
  return(res)
}
