\name{tlm}
\alias{tlm}
\alias{print.tlm}
\alias{plot.tlm}

\title{Fitting Linear, Logistic and Poisson Regression Models with Transformed Variables}
\description{
This function fits a linear, logistic or Poisson regression model with transformed variables and creates an object which can be used to extract effects of the explanatory variable of interest on the response variable, in the original scale of the variables.
}
\usage{
tlm(y, x, z = "none", family = gaussian, data, ypow = 1, xpow = 1, ...)
\method{print}{tlm}(x, ...)
\method{plot}{tlm}(x, type = c("original", "transformed", "diagnosis"),
observed = FALSE,
                   xname = "x", yname = "y", level = 0.95, ...)
}
\arguments{
  \item{y}{name of the response variable (unquoted).}
  \item{x}{name of the explanatory variable of interest (unquoted).}
  \item{z}{additional explanatory variables in the model. Default is "\code{none}". See `Details'.}
  \item{family}{the link function. For linear model, \code{gaussian} (default); for logistic regression, \code{binomial}; for Poisson regression, \code{poisson}. Quoted values are optional. In the case of \code{binomial}, the logit link is assumed.}
  \item{data}{a \code{data.frame} containing the variables in the model.}
  \item{ypow}{numeric power transformation already done in \code{y}. See `Details'.}
  \item{xpow}{numeric power transformation already done in \code{x}. See `Details'.}
 \item{type}{character indicating the type of plot for the fitted model. If "\code{original}" (default), the fitted model is plotted in the original space of the variables; if "\code{transformed}", the fitted model is plotted in the transformed space of the variables (where the model has been fitted); if "\code{diagnosis}", a model diagnostics plot is shown.}
  \item{observed}{logical indicating whether the observations should be added to the plot. Assumed to be \code{FALSE} if \code{x} is categorical. Default is \code{FALSE}.}
  \item{xname}{character. The name of the \code{x} variable for labeling the horizontal plot axis. Default is "\code{x}".}
  \item{yname}{character. The name of the \code{y} variable for labeling the vertical plot axis. Default is "\code{y}". The name of the measure for the response variable is automatically added before \code{yname}.}
 \item{level}{numeric. Confidence level for the prediction of the measure of \code{y} plotted. Default is 0.95.}
  \item{\dots}{further additional arguments passed to \code{tlm} or methods \code{print} and \code{plot}.}
}
\details{
\code{z} can be any expression allowed as a right-hand-side in
\code{\link{formula}}. However, expressions involving the variable
passed in \code{x} are not allowed. To include all the remaining
variables in \code{data}, use just a period (.).

The transformations already done in \code{y} and \code{x} are passed by
\code{ypow} and \code{xpow}, respectively. They should be a
number. Default is 1 (no transformation). The value 0 corresponds to the
logarithmic transformation. If \code{family} is not \code{gaussian},
\code{y} is assumed non transformed. If \code{x} is categorical or
presents only two different values, the value of \code{xpow} is assumed to be 1. If \code{x} presents only two different values, it is considered as a binary variable.}
\value{
\item{model}{the fitted model in the transformed space.}
\item{ypow }{the value of \code{ypow}.}
\item{xpow }{the value of \code{xpow}.}
}
\references{Barrera-Gomez J, Basagana X. Models with transformed variables: interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.}
\author{Barrera-Gomez J and Basagana X.}

\seealso{
\code{\link{MY}}, \code{\link{effect}}, \code{\link{formula}}. 
}
\examples{
### Linear model with log-log transformation:
data(feld1)
head(feld1)
# model fitting in the transformed space:
modcat <-  tlm (y = logroom, x = logmattress, z = cat, data = feld1, ypow = 0, xpow = 0)
modcat
summary(modcat)
# plot of the geometric mean of the response (original space), adjusting for 'cat':
plot(modcat, xname = "Mattress levels", yname = "room levels") 
# plot of the mean of the log of response (transformed space), adjusting for 'cat' and
# adding the observations:
plot(modcat, type = "transformed", xname = "mattress levels", yname = "room levels",
     observed = TRUE)
# diagnosis plot:
plot(modcat, type = "diagnosis")

### The same model but now considering 'cat' as the explanatory variable of interest:
modcat2 <-  tlm (y = logroom, x = cat, z = logmattress, data = feld1, ypow = 0)
summary(modcat2)
# plot of the geometric mean of the response (original space), adjusting
# for mattress levels:
plot(modcat2, xname = "Cat", yname = "room levels") 
# plot of the mean of the log of response (transformed space), adjusting
# for mattress levels:
plot(modcat2, type = "transformed", xname = "Cat", yname = "room levels")
}
