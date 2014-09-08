\name{imt}
\alias{imt}
\docType{data}
\title{Intima Media Thickness of the Carotid Artery}
\description{
Simulated data for intima media thickness of the carotid artery and age in 2784 adults.
}
\usage{data(imt)}
\format{
  A data frame with 2784 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{numeric age of the individual (years).}
    \item{\code{imt}}{numeric intima media thickness of the carotid artery (mm).}
    \item{\code{logimt}}{logarithm of \code{imt}.}
  }
}
\details{
Data were simulated to emulate true data pattern observed in a real study (see `References').
}
\source{See `References'.}
\references{
Rivera M \emph{et al}. Association between long-term exposure to traffic-related air pollution and subclinical atherosclerosis: the REGICOR Study. \emph{Environmental Health Perspectives}. 2013;121(2):223-230.
}
\examples{
data(imt)
par(las = 1, mfrow = c(1, 2))
with(imt, plot(age, imt))
with(imt, plot(age, logimt))
}
\keyword{datasets}
