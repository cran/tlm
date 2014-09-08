\name{cotinine}
\alias{cotinine}
\docType{data}
\title{Birth Weight and Cord Serum Cotinine}
\description{Simulated data for birth weight and cord serum cotinine levels in 351 newborns.}
\usage{data(cotinine)}
\format{
  A data frame with 351 observations on the following 4 variables.
  \describe{
    \item{\code{cotinine}}{numeric cord serum cotinine level in the mother (ng/ml).}
    \item{\code{logcotinine}}{logarithm of \code{cotinine}.}
    \item{\code{weight}}{numeric birth weight (g).}
    \item{\code{underweight}}{a factor with levels \code{no} and \code{yes}, indicating underweight (\code{weight} < 2500 g).}
  }
}
\details{
Data were simulated to emulate true data pattern observed in a real study (see `References').
}
\source{See `References'.}
\references{
Pichini S \emph{et al}. Cord serum cotinine as a biomarker of fetal exposure to cigarette smoke at the end of pregnancy. \emph{Environmental Health Perspectives}. 2000;108(11):1079-1083.
 
}
\examples{
data(cotinine)
par(las = 1, mfrow = c(2, 2))
with(cotinine, plot(cotinine, weight))
with(cotinine, plot(logcotinine, weight))
with(cotinine, boxplot(cotinine ~ underweight))
with(cotinine, boxplot(logcotinine ~ underweight))
}
\keyword{datasets}
