\name{summary.tlm}
\alias{summary.tlm}
\alias{print.summary.tlm}
\title{Summarizing Linear, Logistic and Poisson Models Fits with Transformed Variables.}
\description{\code{summary} method for an object created by the the function \code{tlm}.}
\usage{
\method{summary}{tlm}(object, ...)
\method{print}{summary.tlm}(x, ...)
}
\arguments{
  \item{object}{an object of class "\code{tlm}", a result of a call to \code{\link{tlm}}.}
  \item{x}{an object of class "\code{summary.tlm}", a result of a call to \code{summary.tlm}.}
  \item{\dots}{further additional arguments for \code{summary} and \code{print} methods.}
}
\details{
Essentially, the output of \code{summary.lm} or \code{summary.glm} is displayed. In addition, further information on the fitted model is also displayed.}
\value{
\item{model}{the fitted model in the transformed space.}
\item{ypow }{the value of \code{ypow}.}
\item{xpow }{the value of \code{xpow}.}
\item{summary}{the summary of the fitted model provide by \code{summary.lm} (for gaussian response) or \code{summary.glm} (otherwise).}
}
\references{
Barrera-Gomez J, Basagana X. Models with transformed variables:
interpretation and software. \emph{Epidemiology} (in press).
}
\author{Barrera-Gomez J and Basagana X.}

\seealso{
\code{\link{tlm}}, \code{\link{summary.lm}}, \code{\link{summary.glm}}.
}
\examples{
### linear model with log-log transformation:
data(feld1)
modcat <-  tlm (y = logroom, x = logmattress, z = cat, data = feld1, ypow = 0, xpow = 0)
modcat
summary(modcat)
}
