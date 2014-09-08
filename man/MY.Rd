\name{MY}
\alias{MY}
\alias{print.MY}
\title{Expected Adjusted Median or Generalized Mean}
\description{
This function computes expected measures of the response variable under a linear, logistic or Poisson regression fitted model with transformed variables. Measures can be calculated in both the original and the transformed space. The function automatically provides the name of the measure depending on the fitted model.
}
\usage{
MY(object, x = NULL, npoints = 10, space = c("original", "transformed"), level = 0.95)
\method{print}{MY}(x, ...)
}
\arguments{
  \item{object}{an object of class "\code{tlm}", a result of a call to \code{\link{tlm}}.}
  \item{x}{for \code{MY}: numeric. a number or a numeric vector containing the values of the explanatory variable of interest for which the expected measure of the response variable are required. Default is \code{NULL}. For \code{print.MY}: an object of class "\code{MY}", a result of a call to \code{MY}.}
  \item{npoints}{numeric. If \code{x} is \code{NULL}, the number of points where the measure should be measured. Default is 10. See `Details'. }
  \item{space}{character. If "\code{original}" (default), the measure is computed in the original space of the variables. If "\code{transformed}", the measure is computed in the transformed space of the variables, where the model is fitted.}
  \item{level}{numeric. The confidence level for measures. Default is 0.95.}
  \item{\dots}{further additional arguments for the \code{print.MY} method.}
}
\details{
In order to compute adjusted measures, all explanatory variables in the model different than the explanatory variable of interest are set at their means.

If \code{space} is "\code{original}", then the mean (for Poisson response) or the probability (for binary response) is computed. For gaussian response, the mean is computed if the response variable is not transformed; otherwise, the geometric mean (for log transformation in the response) or the median (for power transformation) is computed.   

If \code{space} is "\code{transformed}", then the mean (for Poisson response or transformed gaussian response), or the logodds (for binary response) is computed.

If \code{x} is \code{NULL}, the measure is computed in \code{npoints} values of the explanatory variable of interest. Those values are choosen to be in arithmetic progression in the given \code{space}, inside the observed range of the explanatory variable. 
}
\value{
\item{M}{adjusted measure of the response variable. See `Details'.}
\item{ymeasure}{the type of measure for \code{M}.}
\item{space}{space where measures has been computed ("\code{original}" or "\code{transformed}").}
\item{ypow}{numeric power transformation assumed in the response variable. See \code{\link{tlm}}.}
\item{xpow}{numeric power transformation assumed in the explanatory variable of interest. See \code{\link{tlm}}.}
}
\references{
Barrera-Gomez J, Basagana X. Interpretation of effects in linear,
logistic and Poisson models with transformed variables: guidance for
practice and software. (submitted).
}
\author{Barrera-Gomez J and Basagana X.}

\seealso{\code{\link{tlm}}, \code{\link{effectInfo}}, \code{\link{effect}}.}
\examples{
data(feld1)
head(feld1)

# Linear model with log-log transformation, adjusting for variable 'cat':
modcat <- tlm(y = logroom, x = logmattress, z = cat, data = feld1, ypow = 0, xpow = 0)
summary(modcat)

# Geometric mean of the response as a function of the explanatory variable, adjusted for
# variable 'cat':  
MY(modcat)
MY(modcat, npoints = 3)
# computed at 1st and 3rd quartiles of the explanatory variable:
MY(modcat, x = quantile(feld1$mattress, probs = c(1, 3)/4))

# Mean of the log(response) as a function of the log explanatory variable, adjusted for
# variable 'cat':  
MY(modcat, space = "transformed")
}
