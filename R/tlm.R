#' @rdname tlm
#' @aliases tlm
#' @aliases print.tlm
#' @aliases plot.tlm
#' @title Fitting, Reporting and Visualizing Linear, Logistic and Poisson
#'  Regression Models with Transformed Variables
#' @description \code{tlm} is the main function of the package. It fits a linear,
#'  logistic or Poisson regression model with transformed variables and creates
#'  an object which can be subsequently used to compute adjusted measures of the
#'  response variable (with \code{\link{MY}}) and compute and interpret adjusted
#'  effects of the explanatory variable of interest on the response variable (with
#'  \code{\link{effect}} and \code{\link{effectInfo}}, respectively), in the natural
#'  scale of the variables. In addition, the fitted model can be visualized with
#'  the \code{plot.tlm} method.
#'
#' @param formula model \code{\link{formula}}. Left-hand-side indicates the response
#'  variable (whose values are assumed to be already transformed). First term in
#'  right-hand-side indicates the explanatory variable of interest (whose values
#'  are assumed to be already transformed). Right-hand-side can include additional
#'  terms (e.g. adjusting variables) but the explanatory variable of interest
#'  cannot be involved in any of them.
#' @param family the response variable \code{\link{family}}. Allowed values are: 1) \code{gaussian}
#'  (default), for linear regression; 2) \code{binomial}, for logistic regression,
#'  and 3) \code{poisson}, for Poisson regression with logarithmic link. Quoted
#'  values are allowed.
#' @param data a \code{data.frame} containing the variables in the model.
#' @param ypow numeric. Power transformation already done in the response variable.
#'  See Details below.
#' @param xpow numeric. Power transformation already done in the explanatory
#'  variable of interest. See Details below.
#' @param x for \code{tlm}, old argument for back compatibility only. To be
#'  removed, see Details below. For \code{print.tlm} and \code{plot.tlm}, an
#'  object of class "\code{tlm}" (an output of the \code{tlm} function).
#' @param type For \code{plot.tlm}, character indicating the type of plot for
#'  the fitted model. If "\code{original}" (default), the fitted model is plotted
#'  in the original space of the variables. If "\code{diagnosis}", a model
#'  diagnostics plot is shown. If "\code{transformed}", the fitted model is
#'  plotted in the transformed space of the variables (where the model
#'  has been fitted). Resulting plots under "\code{transformed}" should not be
#'  reported (see Details below). 
#' @param observed For \code{plot.tlm}, logical indicating whether the observations
#'  are shown in the plot. Assumed to be \code{FALSE} if the explanatory variable
#'  of interest is categorical. Default is \code{FALSE}.
#' @param xname,yname For \code{plot.tlm}, character indicating the name of the
#'  explanatory and the response variable of interest for labeling the plot axes.
#'  Default are "\code{x}" and "\code{y}", respectively.
#' @param level For \code{plot.tlm}, numeric indicating the confidence level for
#'  the confidence of the expectation of the response variable according to the
#'  fitted model. Default is 0.95.
#' @param \dots for \code{tlm}, additional arguments for the underlying \code{lm}
#'  or \code{glm} fitting. For \code{print.tlm} and \code{plot.tlm}, additional
#'  arguments for generic methods \code{print} and \code{plot}, respectively.
#' @param y,z old arguments for back compatibility only. To be removed, see
#'  Details below.
#' @details The transformations already done in the response variable and in the
#'  explanatory variable of interest are passed by \code{ypow} and \code{xpow},
#'  respectively, and must be numbers. Default is 1, meaning no transformation.
#'  The value 0 corresponds to the logarithmic transformation. If \code{family}
#'  is not \code{gaussian}, the response variable is assumed non transformed. If
#'  the explanatory variable of interest is categorical or takes only two different
#'  values, the value of \code{xpow} is assumed to be 1. If the explanatory
#'  variable of interest takes only two different values, it is handled as a
#'  binary variable.
#'  
#'  Plots obtained for \code{type = "transformed"} are intended to visually explore
#'  the model goodness of fit and should not be reported because values of the
#'  transformed variables are meaningless (e.g. log(cotinine) has no sense). 
#'  
#'  Old arguments \code{y}, \code{x} and \code{z}, are deprecated and bugs are
#'  no longer fixed. They will be removed in the first version posterior to 0.2.0.
#'  Use argument \code{formula} instead.
#'  
#' @return A list with class "\code{tlm}" including the following items:
#'  \describe{
#'    \item{model}{the fitted model in the transformed space.}
#'    \item{ypow}{the value of \code{ypow}.}
#'    \item{xpow}{the value of \code{xpow}.}
#'   }
#' @references Barrera-Gomez J, Basagana X. Models with transformed variables:
#'  interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.
#' @seealso \code{\link{MY}}, \code{\link{effect}}, \code{\link{formula}}.
#' @examples
#' ### Linear model with log-log transformation:
#' ### effect of mattress levels on room levels, adjusting for cat:
#' # model fitting in the transformed space:
#' data(feld1)
#' head(feld1)
#' modcat <-  tlm(logroom ~ logmattress + cat, data = feld1, ypow = 0, xpow = 0)
#' modcat
#' summary(modcat)
#' # plot of the geometric mean of the response (original space), adjusting for 'cat':
#' plot(modcat, xname = "Mattress levels", yname = "room levels") 
#' # plot of the mean of the log of response (transformed space), adjusting for 'cat' and
#' # adding the observations:
#' plot(modcat, type = "transformed", xname = "mattress levels", yname = "room levels",
#'      observed = TRUE)
#' # diagnosis plot:
#' plot(modcat, type = "diagnosis")
#'
#' ### effect of cat in house on room levels, adjusting for matress levels:
#' modcat2 <-  tlm(logroom ~ cat + logmattress, data = feld1, ypow = 0)
#' summary(modcat2)
#' # plot of the geometric mean of the response (original space), adjusting
#' # for mattress levels:
#' plot(modcat2, xname = "Cat", yname = "room levels") 
#' # plot of the mean of the log of response (transformed space), adjusting
#' # for mattress levels:
#' plot(modcat2, type = "transformed", xname = "Cat", yname = "room levels")
#' @export
#' @importFrom stats as.formula gaussian glm lm model.frame
tlm <- function(formula, family = gaussian, data, ypow = 1, xpow = 1, ...,
                y, x, z = "none") {
  
  ### get model formula:
  
  ### check arguments provided to get model formula (taking into account old
  ### usage with (x, y, z):
  formulamiss <- missing(formula)
  ymiss <- missing(y)
  xmiss <- missing(x)
  
  ### case of missing formula and missing at least one of y or x:
  if (formulamiss & (ymiss | xmiss))
    stop("'formula' need to be provided.")
  
  ### case of missing formula but both y and x provided (old usage):
  if (formulamiss & (!ymiss) & (!xmiss)) {
    warning("\n\nArguments 'y', 'x' and 'z' are deprecated and bugs are no longer fixed.\nThey will be removed in next package version.\nPlease, use 'formula' instead.\n\n")
    ### formula building ('formulal', as character):
    yl <- deparse1(substitute(y))
    xl <- deparse1(substitute(x))
    yxl <- paste(yl, xl, sep = " ~ ")
    zl <- deparse(substitute(z), width.cutoff = 500L)
    if (zl == "\"none\"") {
      formulal <- yxl
      } else {
        if (zl == "." & ncol(data) > 2)
          zl <- paste(names(data)[!names(data) %in% c(yl, xl)], collapse = " + ")
        formulal <- paste(yxl, zl, sep = " + ")
      }
    formula <- as.formula(formulal)
  }
  
  
  ### case of provided formula (new usage):
  if (!formulamiss)
    formulal <- format(formula) # 'formula' as character

  ################ family gaussian, binomial or poisson:
  if (is.character(family)) {
    responses <- c("gaussian", "binomial", "poisson")
    familymatch <- charmatch(family, responses, nomatch = 0)
    if (familymatch == 0)
      stop("'family' not allowed")
    family <- get(responses[familymatch], mode = "function", envir = parent.frame())
  }
  if (is.function(family))
    family <- family()
  
  ################# 'ypow' control (in addition, family binomial or poisson --> ypow = 1):
  if (!inherits(ypow, c("numeric", "integer")) || length(ypow) != 1L)
    stop("'ypow' must be a number")
  
  if ((ypow != 1) && (family$family != "gaussian")) {
    warning(paste("assuming 'ypow' equal to 1 because the response is", family$family))
    ypow <- 1
  }
  
  ################# 'xpow' control:
  if (!inherits(xpow, c("numeric", "integer")) || length(xpow) != 1L)
    stop("'xpow' must be a number")
  
  
  ### check data:
  if (missing(data))
    stop("'data' need to be provided.")
  
  
  ################# extract names of variables x and y from the formula:
  allvarnames <- all.vars(formula)
  yname <- allvarnames[1]
  xname <- allvarnames[2]
  
  
  ################ y existence (needed here to check 'ypow' below):
  if (!yname %in% names(data))
    stop(paste0("variable '", yname, "' not found"))
  
  ################ x existence (needed here to check 'xpow' below):
  if (!xname %in% names(data))
    stop(paste0("variable '", xname, "' not found"))
  
  
  ################# x to factor
  xaux <- data[, xname]
  
  if (inherits(xaux, c("character", "logical")) || (inherits(xaux, c("numeric", "integer")) && length(unique(xaux[!is.na(xaux)])) == 2))
    data[, xname] <- as.factor(data[, xname])
  
  if ((xpow != 1) && inherits(data[, xname], "factor")) {
    warning("assuming 'xpow' equal to 1 because the explanatory variable of interest is categorical")
    xpow <- 1
  }
  
  ################# y to factor
  yaux <- data[, yname]
  
  if (inherits(yaux, c("character", "logical")) || (inherits(yaux, c("numeric", "integer")) && length(unique(yaux[!is.na(yaux)])) == 2))
    data[, yname] <- as.factor(data[, yname])
  
  if (inherits(data[, yname], "factor") && length(levels(data[, yname])) > 2)
    stop("categorical response variable with more than 2 levels is not allowed")
  
  if (inherits(data[, yname], "factor") && family$family != "binomial")
    stop("categorical response variable is only allowed under 'binomial' family")
  
  if (ypow != 1 && inherits(data[, yname], "factor")) {
    warning("assuming 'ypow' equal to 1 because the response variable is categorical")
    ypow <- 1
  }
  
  ################# bijectivity control
  
  xaux <- data[, xname]
  
  xbij <- TRUE
  if (!inherits(xaux, "factor") && xpow != 1)
    xbij <- bijectivityback(xt = xaux, power = xpow)
  
  if (!xbij)
    stop("explanatory variable is not bijective for the provided value for 'xpow'")
  
  
  yaux <- data[, yname]
  
  ybij <- TRUE
  if (!inherits(yaux, "factor") && ypow != 1)
    ybij <- bijectivityback(xt = yaux, power = ypow)
  
  if (!ybij)
    stop("response variable is not bijective for the provided value for 'ypow'")
  
  ################ power labels for plot and print:
  ypowlabel <- as.character(ypow)
  xpowlabel <- as.character(xpow)
  
  ################ fitting model in the transformed space:
  # mod <- NULL
  # if (family$family == "gaussian") {
  #   mod <- lm(formula = formula, data = data, ...)
  #   } else {
  #     mod <- glm(formula = formula, family = family$family, data = data, ...)
  #   }
  mod <- NULL
  datal <- deparse(substitute(data))  # name of 'data'
  if (family$family == "gaussian")
    modtext <- paste("mod <- lm(formula = ", formulal, ", data = ", datal, ", ...)", sep = "") else
      modtext <- paste("mod <- glm(formula = ", formulal, ", family = ", family$family, ", data = ", datal, ", ...)", sep = "")
  eval(parse(text = modtext))
  

  ################ control that 'x' is not involved in 'z':
  allnam <- all.names(mod$model$call$formula)
  if (sum(allnam %in% xname) > 1) # i.e. if xname appears more than one time in the formula
    stop(paste0("only a linear main effect of the explanatory variable '", xname, "' is allowed in 'formula'"))
  ################ results:
  mf <- model.frame(mod)
  mt <- attr(mf, "terms")
  Xclass <- attr(mt, "dataClasses")[2]
  if (Xclass == "factor" && xpow != 1) {
    warning(paste0("assuming 'xpow' = 1 because the explanatory variable '", xname, "' is a factor"))
    xpow <- 1
  }
  res <- list(model = mod, ypow = ypow, xpow = xpow)
  attr(res, "ypowlabel") <- ypowlabel
  attr(res, "xpowlabel") <- xpowlabel
  class(res) <- "tlm"
  return(res)
}

#-------------------------------------------
## S3Methods print.tlm
#-------------------------------------------

#' S3Methods for Printing
#'
#' @usage
#' \method{print}{tlm}(x, ...)
#' @export
#' @rdname tlm
print.tlm <- function(x, ...) {
  printPreamble(x)
  print(x$model, ...)
  cat("\n")
}




#-------------------------------------------
## S3Methods plot.tlm
#-------------------------------------------

#' S3Methods for Plotting
#'
#' @usage
#' \method{plot}{tlm}(x, type = c("original", "transformed", "diagnosis"),
#'  observed = FALSE, xname = "x", yname = "y", level = 0.95, ...)
#' @export
#' @importFrom stats coef model.frame model.response 
#' @importFrom graphics axis lines par points segments
#' @rdname tlm
plot.tlm <- function(x,
                     type = c("original", "transformed", "diagnosis"),
                     observed = FALSE,
                     xname = "x",
                     yname = "y",
                     level = 0.95,
                     ...) {
  if (!inherits(x, "tlm"))
    stop("argument 'x' must be of class 'tlm'")
  
  if(any(is.na(coef(x$model))))
    stop("plot is not available for models with any missing estimated coefficient")
  
  ### 'type' control:
  type <- match.arg(type)
  
  ### 'xname' control:
  if (!is.null(xname) && (!inherits(xname, "character") || is.na(xname) || length(xname) != 1))
    stop("the name for the explanatory variable X 'xname' must be a character")
  
  ### 'yname' control:
  if (!is.null(yname) && (!inherits(yname, "character") || is.na(yname) || length(yname) != 1))
    stop("the name for the response variable Y 'yname' must be a character")
  
  ### 'level' control:
  if (!inherits(level, "numeric") || level <= 0 || level >= 1 || length(level) != 1)
    stop("'level' must be a number in (0, 1)")
  
  #oldpar <- par(no.readonly = TRUE)      # make a copy of current par settings
  #on.exit(par(oldpar))                   # reset par setting on exit
  
  if (type == "diagnosis") {
    par(mfrow = c(2, 2), ...)
    # Diagnosis plot for the fitted model (transformed space):
    plot(x$model, ...)
    } else {
      ### BEGIN case of no "diagnosis":
      mod <- x$model
      family <- family(mod)$family
      mf <- model.frame(mod)
      mt <- attr(mf, "terms")
      Xclass <- attr(mt, "dataClasses")[2]
      if (missing(xname)) xlabel <- names(Xclass) else xlabel <- xname
      if (missing(yname)) yname <- names(mf)[1]
    if (Xclass == "factor") {
      ### BEGIN case of no "diagnosis" and X factor:
      if (observed)
        warning("\n\nThe observations are not shown in the plot if the explanatory variable is categorical.\n")
      dat <- MY(x, space = type, level = level)
      M <- dat$M
      ymin <- min(M[, -1])
      ymax <- max(M[, -1])
      nlevels <- nrow(M)
      if (type == "original") {
        ### BEGIN case of "original" and X factor:
        ylabelpre <- switch(dat$ymeasure,
                            "geometric mean" = "Geometric mean",
                            "mean" = "Mean",
                            "median" = "Median",
                            "probability" = "Probability")
        ylabel <- paste(ylabelpre, "of", yname)
        ### END case of "original" and X factor:
        } else {
          ### BEGIN case of "transformed" and X factor:
          ylabel <- switch(family,
                         "gaussian" = paste("Mean of", yname),
                         "binomial" = paste0("Log(Odds of ", yname, ")"),
                         "poisson" = paste0("Log(Mean of ", yname, ")"))
        if (x$ypow == 0)
          ylabel <- paste0("Mean of log(", yname, ")")
        if (x$ypow != 1 & x$ypow != 0)
          ylabel <- substitute("Mean of " * ynam * phantom("")^power, list(ynam = yname, power = attr(x, "ypowlabel")))
        ### END case of "transformed" and X factor:
        }
      delta <- 0.2
      plot(c(1 - delta, nlevels + delta), xlim = c(1 - delta, nlevels + delta), ylim = c(ymin, ymax), type = "n", xaxt = "n", xlab = xlabel, ylab = ylabel, ...)
      axis(1, at = 1:nlevels, labels = levels(mf[, 2]))
      lines(1:nlevels, M[, 2], lty = 2, col = "black")
      points(1:nlevels, M[, 2], pch = 19, ...)
      segments(1:nlevels, M[, 3], 1:nlevels, M[, 4], lwd = 1.5, ...)
      ### END case of no "diagnosis" and X factor:
      
    } else {
      dat <- MY(x, npoints = 500, space = type, level = level)
      M <- dat$M
      if (type == "original") {
        ylabelpre <- switch(dat$ymeasure,
                            "geometric mean" = "Geometric mean",
                            "mean" = "Mean",
                            "median" = "Median",
                            "probability" = "Probability")
        ylabel <- paste(ylabelpre, "of", yname)
      } else {
        ylabel <- switch(family,
                         "gaussian" = paste("Mean of", yname),
                         "binomial" = paste0("Log(Odds of ", yname, ")"),
                         "poisson" = paste0("Log(Mean of ", yname, ")"))   
        if (x$xpow == 0)
          xlabel <- paste0("Log(", xname, ")")
        if (x$xpow != 1 & x$xpow != 0)
          xlabel <- substitute(xnam * phantom("")^power, list(xnam = xname, power = attr(x, "xpowlabel")))
        if (x$ypow == 0)
          ylabel <- paste0("Mean of log(", yname, ")")
        if (x$ypow != 1 & x$ypow != 0)
          ylabel <- substitute("Mean of " * ynam * phantom("")^power, list(ynam = yname, power = attr(x, "ypowlabel")))
      }
      if (observed) {
        if (family != "gaussian")
          warning("\n\nThe observations are not shown in the plot for models different than the linear regression model (i.e., family 'gaussian').\n")
        if (family == "gaussian") {
          # Plot with observations:
          yobs <- model.response(mf)
          xobs <- model.frame(mod)[, 2]
          if (type == "original") {
            if (x$ypow == 0) yobs <- exp(yobs) else yobs <- yobs^(1 / x$ypow)
            if (x$xpow == 0) xobs <- exp(xobs) else xobs <- xobs^(1 / x$xpow)
          }
          ymin <- min(M[, -1], yobs)
          ymax <- max(M[, -1], yobs)
          x <- xobs
          y <- yobs
          #plot(x, y, type = "p", col = "gray", pch = 19, cex = 0.6, ylim = c(ymin, ymax), xlab = xlabel, ylab = ylabel, ...)
          plot(x, y, type = "p", col = "gray", ylim = c(ymin, ymax), xlab = xlabel, ylab = ylabel, ...)
          lines(M[, 1], M[, 2], ...)
          lines(M[, 1], M[, 3], lty = 2, ...)
          lines(M[, 1], M[, 4], lty = 2, ...)
        }
      } else {
        # Plot with no observations:
        ymin <- min(M[, -1])
        ymax <- max(M[, -1])
        plot(M[, 1], M[, 2], type = "l", ylim = c(ymin, ymax), xlab = xlabel, ylab = ylabel, ...)
        lines(M[, 1], M[, 3], lty = 2, ...)
        lines(M[, 1], M[, 4], lty = 2, ...)
      }
    }
    ### END case of no "diagnosis":
  }
}
