\name{effect}
\alias{effect}
\alias{print.effect}
\title{Effects Estimate in Linear, Logistic and Poisson Regression Models with Transformed Variables}
\description{
This function estimates the effect of a explanatory variable of interest on a response variable, under a fitted linear, logistic or Poisson regression model with transformed variables. The effect is estimated in the original scale of the variables.
}
\usage{
effect(object, x1 = NULL, x2 = NULL, c = NULL, q = NULL, r = NULL, npoints = NULL,
       level = 0.95, nboot = 999, seed = 4321, verbose = TRUE)
\method{print}{effect}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class "\code{tlm}", a result of a call to \code{\link{tlm}}.}
  \item{x1}{numeric. The values of the explanatory variable where the effect should be computed. See `Details'.}
  \item{x2}{numeric. The alternative values of the explanatory variable (changing from \code{x1}) for which the effect should be computed. See `Details'.}
  \item{c}{numeric. The additive change in the explanatory variable. See `Details'.}
  \item{q}{numeric. The multiplicative change in the explanatory variable. See `Details'.}
  \item{r}{numeric. The percent change in the explanatory variable. See `Details'.}
  \item{npoints}{numeric. The number of points where the effect should be computed. See `Details'.}
  \item{level}{numeric. Confidence level for the effect estimate. Default is 0.95.}
  \item{nboot}{numeric. The number of non parametric bootstrap samples to compute confidence intervals. Default is 999. See `Details'.}
  \item{seed}{numeric. A single value, the seed for bootstrapping. Default is 4321.}
  \item{verbose}{logical. Whether to print detailed progress on R prompt. Default is \code{TRUE}.}
  \item{x}{an object of class "\code{effect}", a result of a call to \code{effect}.}
  \item{\dots}{further additional arguments passed to the method \code{print}.}
}
\details{
In order to compute the effect, both the initial and the final values of the explanatory should be provided. It can be done in several ways. For instance, providing, \code{x1} and \code{x2}; \code{x1} and one of \code{c}, \code{q} or \code{r}; \code{x1}, \code{npoints} and one of \code{c}, \code{q} or \code{r}. Only one of the arguments \code{c}, \code{q} or \code{r} is used, prevailing \code{c} and then \code{q}. If no enough argument are passed, the interquartile range will be considered and a summary effect should be computed, if it exists.

Confidence intervals are computed by transforming the endpoints of the intervals in the transformed scale when it is possible, while non-parametric bootstrap is used otherwise.
}
\value{
\item{effect}{point estimate and confidence interval for the effect size.}
\item{info}{information on how to interpret the effect. Used by the function \code{\link{effectInfo}}.}
}
\references{Barrera-Gomez J, Basagana X. Models with transformed variables: interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.}
\author{Barrera-Gomez J and Basagana X.}

\seealso{\code{\link{tlm}}, \code{\link{effectInfo}}, \code{\link{MY}}.}

\examples{
### Linear model with log transformation in the response variable:
data(imt)
head(imt)

# model fitting:
modimt <- tlm(y = logimt, x = age, data = imt, ypow = 0)
modimt

# information on interpreting the effect:
effectInfo(modimt)

# the function effect provides as default the expected change in IMT for an additive change in age
# equal to the interquartile range:
effect(modimt)

# other effects:
(minage <- min(imt$age))
(maxage <- max(imt$age))
effect(modimt, c = maxage - minage)

\dontrun{
effect(modimt, x1 = minage, r = 50, npoints = 3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
