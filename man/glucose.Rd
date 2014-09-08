\name{glucose}
\alias{glucose}
\docType{data}
\title{Glucose and Triglycerides Levels in Blood}
\description{Simulated data for glucose and triglycerides levels in blood in 400 adults.}
\usage{data(glucose)}
\format{
  A data frame with 400 observations on the following 4 variables.
  \describe{
    \item{\code{trigly}}{numeric triglycerides levels in blood (mg/dl).}
    \item{\code{gluco}}{numeric glucose levels in blood (mg/dl).}
    \item{\code{inv12tri}}{numeric reciprocal of the square root of \code{trigly} (i.e., -1/2 power transformation).}
    \item{\code{inv2glu}}{numeric reciprocal of the \code{gluco} square (i.e., -2 power transformation).}
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
data(glucose)
par(las = 1, mfrow = c(1, 2))
with(glucose, plot(trigly, gluco))
with(glucose, plot(inv12tri, inv2glu))
}
\keyword{datasets}
