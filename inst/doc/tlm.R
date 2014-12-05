### R code from vignette source 'tlm.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tlm.Rnw:73-74 (eval = FALSE)
###################################################
## #options(width = 100, continue = "  ")


###################################################
### code chunk number 2: tlm.Rnw:113-114 (eval = FALSE)
###################################################
## library(tlm)


###################################################
### code chunk number 3: tlm.Rnw:116-117
###################################################
require(tlm)


###################################################
### code chunk number 4: tlm.Rnw:120-121 (eval = FALSE)
###################################################
## vignette("tlm")


###################################################
### code chunk number 5: tlm.Rnw:124-125 (eval = FALSE)
###################################################
## help(package = "tlm")


###################################################
### code chunk number 6: tlm.Rnw:138-139 (eval = FALSE)
###################################################
## ?tlm


###################################################
### code chunk number 7: tlm.Rnw:164-165 (eval = FALSE)
###################################################
## ?MY


###################################################
### code chunk number 8: tlm.Rnw:192-193 (eval = FALSE)
###################################################
## ?effect


###################################################
### code chunk number 9: tlm.Rnw:242-246
###################################################
data(imt)
dim(imt)
head(imt)
summary(imt)


###################################################
### code chunk number 10: tlm.Rnw:251-252
###################################################
modimt <- tlm(y = logimt, x = age, data = imt, ypow = 0)


###################################################
### code chunk number 11: tlm.Rnw:255-256
###################################################
modimt


###################################################
### code chunk number 12: tlm.Rnw:260-261
###################################################
summary(modimt)


###################################################
### code chunk number 13: tlm.Rnw:265-266
###################################################
MY(modimt)


###################################################
### code chunk number 14: tlm.Rnw:270-271
###################################################
MY(modimt, npoints = 3)


###################################################
### code chunk number 15: tlm.Rnw:275-277
###################################################
q13 <- quantile(imt$age, probs = c(1, 3)/4)
MY(modimt, x = q13)


###################################################
### code chunk number 16: tlm.Rnw:281-282
###################################################
MY(modimt, x = q13, space = "transformed")


###################################################
### code chunk number 17: tlm.Rnw:286-288 (eval = FALSE)
###################################################
## plot(modimt, type = "transformed", observed = T, xname = "Age (years)", yname = "IMT")
## plot(modimt, observed = T, xname = "Age (years)", yname = "IMT (mm)")


###################################################
### code chunk number 18: plotimttrans (eval = FALSE)
###################################################
## plot(modimt, type = "transformed", observed = T, xname = "Age (years)", yname = "IMT")


###################################################
### code chunk number 19: plotimt (eval = FALSE)
###################################################
## plot(modimt, observed = T, xname = "Age (years)", yname = "IMT (mm)")


###################################################
### code chunk number 20: tlm.Rnw:299-302
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "transformed", observed = T, xname = "Age (years)", yname = "IMT")
plot(modimt, observed = T, xname = "Age (years)", yname = "IMT (mm)")


###################################################
### code chunk number 21: plotimtdiag (eval = FALSE)
###################################################
## plot(modimt, type = "diagnosis") 


###################################################
### code chunk number 22: tlm.Rnw:317-318
###################################################
effectInfo(modimt)


###################################################
### code chunk number 23: tlm.Rnw:323-326
###################################################
#par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
par(las = 1, mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "diagnosis") 


###################################################
### code chunk number 24: tlm.Rnw:336-337
###################################################
effect(modimt)


###################################################
### code chunk number 25: tlm.Rnw:341-343
###################################################
q123 <- quantile(imt$age, probs = 1:3/4)   # quartiles
effect(modimt, x1 = q123)


###################################################
### code chunk number 26: tlm.Rnw:381-385
###################################################
data(cotinine)
dim(cotinine)
head(cotinine)
summary(cotinine)


###################################################
### code chunk number 27: tlm.Rnw:389-390
###################################################
modcot <- tlm(y = weight, x = logcotinine, data = cotinine, xpow = 0)


###################################################
### code chunk number 28: tlm.Rnw:393-394
###################################################
summary(modcot)


###################################################
### code chunk number 29: tlm.Rnw:398-400 (eval = FALSE)
###################################################
## plot(modcot, type = "transformed", observed = T, xname = "Cotinine", yname = "weight (kg)")
## plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")


###################################################
### code chunk number 30: plotcottrans (eval = FALSE)
###################################################
## plot(modcot, type = "transformed", observed = T, xname = "Cotinine", yname = "weight (kg)")


###################################################
### code chunk number 31: plotcot (eval = FALSE)
###################################################
## plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")


###################################################
### code chunk number 32: tlm.Rnw:411-414
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3),  mgp = c(2.8, 0.6, 0))
plot(modcot, type = "transformed", observed = T, xname = "Cotinine", yname = "weight (kg)")
plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")


###################################################
### code chunk number 33: tlm.Rnw:422-423
###################################################
effectInfo(modcot)


###################################################
### code chunk number 34: tlm.Rnw:427-428
###################################################
effect(modcot)


###################################################
### code chunk number 35: tlm.Rnw:431-432
###################################################
effect(modcot, q = 10)


###################################################
### code chunk number 36: tlm.Rnw:436-438
###################################################
range(cotinine$cotinine)
effect(modcot, x1 = 100, c = 200, npoints = 4)


###################################################
### code chunk number 37: tlm.Rnw:482-486
###################################################
data(feld1)
dim(feld1)
head(feld1)
summary(feld1)


###################################################
### code chunk number 38: tlm.Rnw:490-491
###################################################
modcat <-  tlm (y = logroom, x = logmattress, z = cat, data = feld1, ypow = 0, xpow = 0)


###################################################
### code chunk number 39: tlm.Rnw:494-495
###################################################
summary(modcat)


###################################################
### code chunk number 40: tlm.Rnw:499-501 (eval = FALSE)
###################################################
## plot(modcat, type = "transformed", observed = T, xname = "Mattress levels", yname = "living room levels")
## plot(modcat, xname = "Mattress levels", yname = "living room levels")


###################################################
### code chunk number 41: plotcattrans (eval = FALSE)
###################################################
## plot(modcat, type = "transformed", observed = T, xname = "Mattress levels", yname = "living room levels")


###################################################
### code chunk number 42: plotcat (eval = FALSE)
###################################################
## plot(modcat, xname = "Mattress levels", yname = "living room levels")


###################################################
### code chunk number 43: tlm.Rnw:513-516
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcat, type = "transformed", observed = T, xname = "Mattress levels", yname = "living room levels")
plot(modcat, xname = "Mattress levels", yname = "living room levels")


###################################################
### code chunk number 44: tlm.Rnw:524-525
###################################################
effectInfo(modcat)


###################################################
### code chunk number 45: tlm.Rnw:531-532
###################################################
effect(modcat)


###################################################
### code chunk number 46: tlm.Rnw:536-538
###################################################
modcat2 <-  tlm (y = logroom, x = cat, data = feld1, ypow = 0)
modcat2


###################################################
### code chunk number 47: tlm.Rnw:542-543
###################################################
MY(modcat2)


###################################################
### code chunk number 48: tlm.Rnw:547-548
###################################################
effect(modcat2)


###################################################
### code chunk number 49: plotcat2 (eval = FALSE)
###################################################
## plot(modcat2, yname = "room levels")


###################################################
### code chunk number 50: tlm.Rnw:558-563
###################################################
m <- matrix(0, nrow = 2, ncol = 4)
m[, 2:3] <- 1
layout(m)
par(las = 1)
plot(modcat2, yname = "room levels")


###################################################
### code chunk number 51: tlm.Rnw:582-586
###################################################
data(glucose)
dim(glucose)
head(glucose)
summary(glucose)


###################################################
### code chunk number 52: tlm.Rnw:590-592
###################################################
modglucose <- tlm(y = inv2glu, x = inv12tri, data = glucose, ypow = -2, xpow = -1/2)
summary(modglucose)


###################################################
### code chunk number 53: tlm.Rnw:596-597
###################################################
MY(modglucose)


###################################################
### code chunk number 54: tlm.Rnw:600-602 (eval = FALSE)
###################################################
## plot(modglucose, type = "transformed", observed = T, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")
## plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")


###################################################
### code chunk number 55: plotglucotrans (eval = FALSE)
###################################################
## plot(modglucose, type = "transformed", observed = T, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")


###################################################
### code chunk number 56: plotgluco (eval = FALSE)
###################################################
## plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")


###################################################
### code chunk number 57: tlm.Rnw:613-616
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(3.6, 0.6, 0))
plot(modglucose, type = "transformed", observed = T, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")
plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")


###################################################
### code chunk number 58: tlm.Rnw:624-625
###################################################
effectInfo(modglucose)


###################################################
### code chunk number 59: tlm.Rnw:628-632
###################################################
# 2.5 and 97.5 percentiles of trigli (for text):
summtrigli <- quantile(glucose$trigly, probs = c(2.5, 25, 50, 75, 97.5) / 100)
trigli2.5 <- round(as.numeric(summtrigli[1]), 1)
trigli97.5 <- round(as.numeric(summtrigli[5]), 1)


###################################################
### code chunk number 60: tlm.Rnw:636-641
###################################################
# Effects for an additive change in triglycerides level:
xc <- 50 * (1:5)
xc
effectXdiff <- effect(modglucose, x1 = xc)
effectXdiff


###################################################
### code chunk number 61: tlm.Rnw:675-680
###################################################
# Effects for an percent change in triglycerides level:
xq <- 50 * 1.5^(0:4)
xq
effectXperc <- effect(modglucose, x1 = xq)
effectXperc


###################################################
### code chunk number 62: tlm.Rnw:724-725
###################################################
modcot2 <- tlm(y = underweight, x = logcotinine, data = cotinine, xpow = 0, family = binomial)


###################################################
### code chunk number 63: tlm.Rnw:728-729
###################################################
summary(modcot2)


###################################################
### code chunk number 64: tlm.Rnw:733-734
###################################################
MY(modcot2)


###################################################
### code chunk number 65: tlm.Rnw:738-740 (eval = FALSE)
###################################################
## plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels", yname = "low birth weight")
## plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")


###################################################
### code chunk number 66: plotcot2trans (eval = FALSE)
###################################################
## plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels", yname = "low birth weight")


###################################################
### code chunk number 67: plotcot2 (eval = FALSE)
###################################################
## plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")


###################################################
### code chunk number 68: tlm.Rnw:752-755
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels", yname = "low birth weight")
plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")


###################################################
### code chunk number 69: tlm.Rnw:763-764
###################################################
effectInfo(modcot2)


###################################################
### code chunk number 70: tlm.Rnw:767-768
###################################################
effect(modcot2)


###################################################
### code chunk number 71: tlm.Rnw:771-772
###################################################
effect(modcot2, q = 10)


