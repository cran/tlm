\name{effectInfo}
\alias{effectInfo}
\alias{print.effectInfo}
\title{Interpretation of Effects in Linear, Logistic and Poisson Models with Transformed Variables}
\description{
This function provides information on interpreting effects in linear, logistic and Poisson models with transformed variables. Specifically, if a summary measure for the effect exists, the function details how to obtain it.
}
\usage{
effectInfo(object)
\method{print}{effectInfo}(x, ...)
}
\arguments{
  \item{object}{an object of class "\code{tlm}", a result of a call to \code{\link{tlm}}.}
  \item{x}{an object of class "\code{effectInfo}", a result of a call to \code{effectInfo}.}
  \item{\dots}{further additional arguments for the \code{print} method.}
}
\value{
\item{beta}{regression coefficient estimate in the fitted model which is associated to the effect of the explanatory variable of interest on the response variable. \code{NA} corresponds to those models for which a summary effect does not exist.}
\item{Xincrease}{type of change in the exploratory variable of interest (additive or realtive) for which a summary effect exists. \code{NA} corresponds to those models for which a summary effect does not exist.}
\item{effecttype}{type of effect on the response variable for which a summary effect exists. \code{NA} corresponds to those models for which a summary effect is not available.}
\item{effectsize}{formula for the summary effect size, if any. \code{NA} corresponds to those models for which a summary effect is not available.}
\item{furtherinfo}{further information about how to interpret effects.}
}
\references{Barrera-Gomez J, Basagana X. Models with transformed variables: interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.}
\author{Barrera-Gomez J and Basagana X.}

\seealso{\code{\link{tlm}}, \code{\link{effect}}, \code{\link{MY}}.}

\examples{
### Linear model with log transformation in the explanatory variable:
data(cotinine)
head(cotinine)
# model fitting:
modcot <- tlm(y = weight, x = logcotinine, data = cotinine, xpow = 0)
modcot
# information on interpreting the effect:
effectInfo(modcot)

### Linear model with no summary measure of effect:
data(glucose)
head(glucose)
# transformations Y^(-2) and X^(-1/2): 
modgluco <- tlm(y = inv2glu, x = inv12tri, data = glucose, ypow = -2, xpow = -1/2)
modgluco
effectInfo(modgluco)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
