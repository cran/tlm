#' Internal functions to get info effect for model types 0 to 12.
#'
#' @param object A fitted tlm.
#' @return a list with the effect information.
#' @keywords internal
#' @noRd
effectInfomod0 <- function(object) {
  res <- list(beta = NA, Xincrease = NA, effecttype = NA, effectsize = NA)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod1 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "additive of c units"
  effecttype <- "additive change in the mean of Y"
  effectsize <- "c * beta units of Y"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing the increase in X\n, 'c',\nand the level for the confidence interval, 'level'."	
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @importFrom stats model.matrix 
#' @noRd
effectInfomod2 <- function(object) {
  mf <- model.frame(object$model)
  aux <- summary(object$model)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  beta <- aux[2:nlevels, ]
  Xincrease <- paste("changing X from its reference, '", Xlevels[1], "', to the alternative level", sep = "")
  effecttype <- "additive change in the mean of Y"
  effectsize <- "beta units of Y"
  furtherinfo <- "\nFurther details can be obtained using effect() and providing the level for the\nconfidence interval, 'level'." 
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod3 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "additive of c units"
  effecttype <- "percent change in the geometric mean of Y"
  effectsize <- "100 * [exp(c * beta) - 1]%"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing the increase in X, 'c', and the\nlevel for the confidence interval, 'level'."  
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @importFrom stats model.matrix 
#' @noRd
effectInfomod4 <- function(object) {
  mf <- model.frame(object$model) 
  aux <- summary(object$model)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  beta <- aux[2:nlevels, ]
  Xincrease <- paste("changing X from its reference, '", Xlevels[1], "', to the alternative level", sep = "")
  effecttype <- "percent change in the geometric mean of Y"
  effectsize <- "100 * [exp(beta) - 1]%"
  furtherinfo <- "\nFurther details can be obtained using effect() and providing the level for the\nconfidence interval, 'level'."
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod5 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2] 	
  Xincrease <- "multiplicative of factor q (equivalently, adding an r = 100 * (q - 1)% to X)"
  effecttype <- "additive change in the mean of Y"
  effectsize <- "beta * log(q) units of Y"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing either the multiplicative ('q') or\nthe percent ('r') change in X, and the level for the confidence interval, 'level'."
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod6 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "multiplicative of factor q (equivalently, adding an r = 100 * (q - 1)% to X)"
  effecttype <- "percent change in the geometric mean of Y"
  effectsize <- "100 * (q^beta - 1)%"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing either the multiplicative ('q') or\nthe percent ('r') change in X, and the level for the confidence interval, 'level'."
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod7 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "additive of c units"
  effecttype <- "odds ratio of Y"
  effectsize <- "exp(c * beta)"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing the increase in X, 'c', and the\nlevel for the confidence interval, 'level'."	
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @importFrom stats model.matrix 
#' @noRd
effectInfomod8 <- function(object) {
  mf <- model.frame(object$model) 
  aux <- summary(object$model)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  beta <- aux[2:nlevels, ]
  Xincrease <- paste("changing X from its reference, '", Xlevels[1], "', to the alternative level", sep = "")
  effecttype <- "odds ratio of Y"
  effectsize <- "exp(beta)"
  furtherinfo <- "\nFurther details can be obtained using effect() and providing the level for the\nconfidence interval, 'level'."   	
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod9 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "multiplicative of factor q (equivalently, adding an r = 100 * (q - 1)% to X)"
  effecttype <- "odds ratio of Y"
  effectsize <- "q^beta"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing either the multiplicative ('q') or\nthe percent ('r') change in X, and the level for the confidence interval, 'level'."
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod10 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "additive of c units"
  effecttype <- "percent change in the mean of Y"
  effectsize <- "100 * [exp(c * beta) - 1]%"
  furtherinfo <- "\nFurther details can be obtained using effect(), providing the increase in X, 'c', and the\nlevel for the confidence interval, 'level'."  
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @importFrom stats model.matrix 
#' @noRd
effectInfomod11 <- function(object) {
  mf <- model.frame(object$model)
  aux <- summary(object$model)$coefficients
  Xlevels <- levels(mf[, 2])
  nlevels <- length(Xlevels)
  beta <- aux[2:nlevels, ]
  Xincrease <- paste("changing X from its reference, '", Xlevels[1], "', to the alternative level", sep = "")
  effecttype <- "percent change in the mean of Y"
  effectsize <- "100 * [exp(beta) - 1]%"
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}


#' @rdname effectInfomod0
#' @noRd
effectInfomod12 <- function(object) {
  aux <- summary(object$model)$coefficients
  beta <- t(as.matrix(aux[2, ]))
  rownames(beta) <- rownames(aux)[2]
  Xincrease <- "multiplicative of factor q (equivalently, adding an r = 100 * (q - 1)% to X)"
  effecttype <- "percent change in the mean of Y"
  effectsize <- "100 * (q^beta - 1)%"
  res <- list(beta = beta, Xincrease = Xincrease, effecttype = effecttype, effectsize = effectsize)
  return(res)
}

