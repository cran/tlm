\name{tlm-package}
\alias{tlm-package}
\docType{package}

\title{Effects under Linear, Logistic and Poisson Regression Models with Transformed Variables}

\description{Computation of effects under linear, logistic and Poisson regression models with transformed variables. Logarithm and power transformations are allowed. Effects can be displayed both numerically and graphically in both the original and the transformed space of the variables.}
\details{
\tabular{ll}{
Package: \tab tlm\cr
Type: \tab Package\cr
Version: \tab 0.1.5\cr
Date: \tab 2017-04-07\cr
License: \tab GPL (>=2)\cr
URL: \tab http://www.creal.cat/xbasagana/software.html\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jose Barrera-Gomez and Xavier Basagana

Maintainer: Jose Barrera-Gomez <jbarrera@creal.cat>
}
\references{Barrera-Gomez J, Basagana X. Models with transformed variables: interpretation and software. \emph{Epidemiology}. 2015;26(2):e16-17.}

\keyword{ package }

\examples{
# linear regression model with log-log tansformation:
data(feld1)
head(feld1)
modcat <- tlm(y = logroom, x = logmattress, z = cat, ypow = 0, xpow = 0, data = feld1)

# print and summary:
modcat
summary(modcat)

# plotting the fitted model in the original space:
plot(modcat, xname = "Mattress levels", yname = "room levels")

# plotting the fitted model in the transformed space (and adding the observations):
plot(modcat, xname = "Mattress levels", yname = "room levels", type = "transformed",
     observed = TRUE)

# diagnosis plot for the fitted model:
plot(modcat, type = "diagnosis")

# Expected geometric mean of the response, adjusting for variable 'cat':
MY(modcat)

# Expected mean of the transformed response, adjusting for variable 'cat':
MY(modcat, space = "transformed")

# Information on how to interpret effects:
effectInfo(modcat)

# Summary effect (default effect):
effect(modcat)
}
