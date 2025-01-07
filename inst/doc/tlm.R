## ----options, include=FALSE---------------------------------------------------
library(knitr)
library(xtable)
#options(scipen = 999) # to avoid scientic notation in knitr outputs
opts_chunk$set(size = 'small',
               fig.align = 'center')

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
library(tlm)

## ----eval=FALSE---------------------------------------------------------------
#  help(package = "tlm")

## ----eval=FALSE---------------------------------------------------------------
#  browseVignettes("tlm")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ?tlm

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ?MY

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ?effect

## -----------------------------------------------------------------------------
data(imt)
dim(imt)
head(imt)
summary(imt)

## -----------------------------------------------------------------------------
modimt <- tlm(logimt ~ age, data = imt, ypow = 0)

## -----------------------------------------------------------------------------
modimt

## -----------------------------------------------------------------------------
summary(modimt)

## -----------------------------------------------------------------------------
MY(modimt)

## -----------------------------------------------------------------------------
MY(modimt, npoints = 3)

## -----------------------------------------------------------------------------
q13 <- quantile(imt$age, probs = c(1, 3)/4)
MY(modimt, x = q13)

## -----------------------------------------------------------------------------
MY(modimt, x = q13, space = "transformed")

## ----plotimt, eval=FALSE------------------------------------------------------
#  plot(modimt, type = "transformed", observed = TRUE, xname = "Age (years)", yname = "IMT")
#  plot(modimt, observed = TRUE, xname = "Age (years)", yname = "IMT (mm)")

## ----echo=FALSE, fig.width=8, fig.height=4------------------------------------
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "transformed", observed = TRUE, xname = "Age (years)", yname = "IMT")
plot(modimt, observed = TRUE, xname = "Age (years)", yname = "IMT (mm)")

## ----plotimtdiag, eval=FALSE--------------------------------------------------
#  plot(modimt, type = "diagnosis")

## -----------------------------------------------------------------------------
effectInfo(modimt)

## ----echo=FALSE, fig.width=8, fig.height=8------------------------------------
#par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
par(las = 1, mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "diagnosis") 

## -----------------------------------------------------------------------------
effect(modimt)

## -----------------------------------------------------------------------------
q123 <- quantile(imt$age, probs = 1:3/4)   # quartiles
effect(modimt, x1 = q123)

## -----------------------------------------------------------------------------
data(cotinine)
dim(cotinine)
head(cotinine)
summary(cotinine)

## -----------------------------------------------------------------------------
modcot <- tlm(weight ~ logcotinine, data = cotinine, xpow = 0)

## -----------------------------------------------------------------------------
summary(modcot)

## ----plotcot, eval=FALSE------------------------------------------------------
#  plot(modcot, type = "transformed", observed = TRUE, xname = "Cotinine", yname = "weight (kg)")
#  plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")

## ----echo=FALSE, fig.width=8, fig.height=4.5----------------------------------
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3),  mgp = c(2.8, 0.6, 0))
plot(modcot, type = "transformed", observed = TRUE, xname = "Cotinine", yname = "weight (kg)")
plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")

## -----------------------------------------------------------------------------
effectInfo(modcot)

## -----------------------------------------------------------------------------
effect(modcot)

## -----------------------------------------------------------------------------
effect(modcot, q = 10)

## -----------------------------------------------------------------------------
range(cotinine$cotinine)
effect(modcot, x1 = 100, c = 200, npoints = 4)

## -----------------------------------------------------------------------------
data(feld1)
dim(feld1)
head(feld1)
summary(feld1)

## -----------------------------------------------------------------------------
modcat <- tlm(logroom ~ logmattress + cat, data = feld1, ypow = 0, xpow = 0)

## -----------------------------------------------------------------------------
summary(modcat)

## ----plotcat, eval=FALSE------------------------------------------------------
#  plot(modcat, type = "transformed", observed = TRUE, xname = "Mattress levels",
#       yname = "living room levels")
#  plot(modcat, xname = "Mattress levels", yname = "living room levels")

## ----echo=FALSE, fig.width=8, fig.height=4.5----------------------------------
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcat, type = "transformed", observed = TRUE, xname = "Mattress levels",
     yname = "living room levels")
plot(modcat, xname = "Mattress levels", yname = "living room levels")

## -----------------------------------------------------------------------------
effectInfo(modcat)

## -----------------------------------------------------------------------------
effect(modcat)

## -----------------------------------------------------------------------------
modcat2 <- tlm(logroom ~ cat, data = feld1, ypow = 0)
modcat2

## -----------------------------------------------------------------------------
MY(modcat2)

## -----------------------------------------------------------------------------
effect(modcat2)

## ----plotcat2, eval=FALSE-----------------------------------------------------
#  plot(modcat2, yname = "room levels")

## ----echo=FALSE, fig.width=8, fig.height=4------------------------------------
m <- matrix(0, nrow = 2, ncol = 4)
m[, 2:3] <- 1
layout(m)
par(las = 1)
plot(modcat2, yname = "room levels")

## -----------------------------------------------------------------------------
data(glucose)
dim(glucose)
head(glucose)
summary(glucose)

## -----------------------------------------------------------------------------
modglucose <- tlm(inv2glu ~ inv12tri, data = glucose, ypow = -2, xpow = -1/2)
summary(modglucose)

## -----------------------------------------------------------------------------
MY(modglucose)

## ----plotgluco, eval=FALSE----------------------------------------------------
#  plot(modglucose, type = "transformed", observed = TRUE, xname = "Triglycerides (mg/dl)",
#       yname = "glucose (mg/dl)")
#  plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")

## ----echo=FALSE, fig.width=8, fig.height=5------------------------------------
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(3.6, 0.6, 0))
plot(modglucose, type = "transformed", observed = TRUE, xname = "Triglycerides (mg/dl)",
     yname = "glucose (mg/dl)")
plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")

## -----------------------------------------------------------------------------
effectInfo(modglucose)

## ----echo=FALSE---------------------------------------------------------------
# 2.5 and 97.5 percentiles of trigli (for text):
summtrigli <- quantile(glucose$trigly, probs = c(2.5, 25, 50, 75, 97.5) / 100)
trigli2.5 <- round(as.numeric(summtrigli[1]), 1)
trigli97.5 <- round(as.numeric(summtrigli[5]), 1)

## -----------------------------------------------------------------------------
# Effects for an additive change in triglycerides level:
xc <- 50 * (1:5)
xc
effectXdiff <- effect(modglucose, x1 = xc)
effectXdiff

## -----------------------------------------------------------------------------
# Effects for an percent change in triglycerides level:
xq <- 50 * 1.5^(0:4)
xq
effectXperc <- effect(modglucose, x1 = xq)
effectXperc

## -----------------------------------------------------------------------------
modcot2 <- tlm(underweight ~ logcotinine, family = binomial, data = cotinine, xpow = 0)

## -----------------------------------------------------------------------------
summary(modcot2)

## -----------------------------------------------------------------------------
MY(modcot2)

## ----plotcot2, eval=FALSE-----------------------------------------------------
#  plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels",
#       yname = "low birth weight")
#  plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")

## ----echo=FALSE, fig.width=8, fig.height=4.5----------------------------------
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels",
     yname = "low birth weight")
plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")

## -----------------------------------------------------------------------------
effectInfo(modcot2)

## -----------------------------------------------------------------------------
effect(modcot2)

## -----------------------------------------------------------------------------
effect(modcot2, q = 10)

