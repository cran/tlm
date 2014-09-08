### R code from vignette source 'tlm.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tlm.Rnw:73-75
###################################################
#options(width = 100, continue = "  ")
options(width = 100, continue = "  ")


###################################################
### code chunk number 2: tlm.Rnw:114-115 (eval = FALSE)
###################################################
## library(tlm)


###################################################
### code chunk number 3: tlm.Rnw:117-118
###################################################
require(tlm)


###################################################
### code chunk number 4: tlm.Rnw:121-122 (eval = FALSE)
###################################################
## vignette("tlm")


###################################################
### code chunk number 5: tlm.Rnw:125-126 (eval = FALSE)
###################################################
## help(package = "tlm")


###################################################
### code chunk number 6: tlm.Rnw:139-140 (eval = FALSE)
###################################################
## ?tlm


###################################################
### code chunk number 7: tlm.Rnw:165-166 (eval = FALSE)
###################################################
## ?MY


###################################################
### code chunk number 8: tlm.Rnw:193-194 (eval = FALSE)
###################################################
## ?effect


###################################################
### code chunk number 9: tlm.Rnw:243-247
###################################################
data(imt)
dim(imt)
head(imt)
summary(imt)


###################################################
### code chunk number 10: tlm.Rnw:252-253
###################################################
modimt <- tlm(y = logimt, x = age, data = imt, ypow = 0)


###################################################
### code chunk number 11: tlm.Rnw:256-257
###################################################
modimt


###################################################
### code chunk number 12: tlm.Rnw:261-262
###################################################
summary(modimt)


###################################################
### code chunk number 13: tlm.Rnw:266-267
###################################################
MY(modimt)


###################################################
### code chunk number 14: tlm.Rnw:271-272
###################################################
MY(modimt, npoints = 3)


###################################################
### code chunk number 15: tlm.Rnw:276-278
###################################################
q13 <- quantile(imt$age, probs = c(1, 3)/4)
MY(modimt, x = q13)


###################################################
### code chunk number 16: tlm.Rnw:282-283
###################################################
MY(modimt, x = q13, space = "transformed")


###################################################
### code chunk number 17: tlm.Rnw:287-289 (eval = FALSE)
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
### code chunk number 20: tlm.Rnw:300-303
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "transformed", observed = T, xname = "Age (years)", yname = "IMT")
plot(modimt, observed = T, xname = "Age (years)", yname = "IMT (mm)")


###################################################
### code chunk number 21: plotimtdiag (eval = FALSE)
###################################################
## plot(modimt, type = "diagnosis") 


###################################################
### code chunk number 22: tlm.Rnw:318-319
###################################################
effectInfo(modimt)


###################################################
### code chunk number 23: tlm.Rnw:324-327
###################################################
#par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
par(las = 1, mar = c(5, 5, 3, 3),  mgp = c(2.8, 0.6, 0))
plot(modimt, type = "diagnosis") 


###################################################
### code chunk number 24: tlm.Rnw:337-338
###################################################
effect(modimt)


###################################################
### code chunk number 25: tlm.Rnw:342-344
###################################################
q123 <- quantile(imt$age, probs = 1:3/4)   # quartiles
effect(modimt, x1 = q123)


###################################################
### code chunk number 26: tlm.Rnw:382-386
###################################################
data(cotinine)
dim(cotinine)
head(cotinine)
summary(cotinine)


###################################################
### code chunk number 27: tlm.Rnw:390-391
###################################################
modcot <- tlm(y = weight, x = logcotinine, data = cotinine, xpow = 0)


###################################################
### code chunk number 28: tlm.Rnw:394-395
###################################################
summary(modcot)


###################################################
### code chunk number 29: tlm.Rnw:399-401 (eval = FALSE)
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
### code chunk number 32: tlm.Rnw:412-415
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3),  mgp = c(2.8, 0.6, 0))
plot(modcot, type = "transformed", observed = T, xname = "Cotinine", yname = "weight (kg)")
plot(modcot, xname = "Cotinine (ng/ml)", yname = "weight (kg)")


###################################################
### code chunk number 33: tlm.Rnw:423-424
###################################################
effectInfo(modcot)


###################################################
### code chunk number 34: tlm.Rnw:428-429
###################################################
effect(modcot)


###################################################
### code chunk number 35: tlm.Rnw:432-433
###################################################
effect(modcot, q = 10)


###################################################
### code chunk number 36: tlm.Rnw:437-439
###################################################
range(cotinine$cotinine)
effect(modcot, x1 = 100, c = 200, npoints = 4)


###################################################
### code chunk number 37: tlm.Rnw:483-487
###################################################
data(feld1)
dim(feld1)
head(feld1)
summary(feld1)


###################################################
### code chunk number 38: tlm.Rnw:491-492
###################################################
modcat <-  tlm (y = logroom, x = logmattress, z = cat, data = feld1, ypow = 0, xpow = 0)


###################################################
### code chunk number 39: tlm.Rnw:495-496
###################################################
summary(modcat)


###################################################
### code chunk number 40: tlm.Rnw:500-502 (eval = FALSE)
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
### code chunk number 43: tlm.Rnw:514-517
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcat, type = "transformed", observed = T, xname = "Mattress levels", yname = "living room levels")
plot(modcat, xname = "Mattress levels", yname = "living room levels")


###################################################
### code chunk number 44: tlm.Rnw:525-526
###################################################
effectInfo(modcat)


###################################################
### code chunk number 45: tlm.Rnw:532-533
###################################################
effect(modcat)


###################################################
### code chunk number 46: tlm.Rnw:537-539
###################################################
modcat2 <-  tlm (y = logroom, x = cat, data = feld1, ypow = 0)
modcat2


###################################################
### code chunk number 47: tlm.Rnw:543-544
###################################################
MY(modcat2)


###################################################
### code chunk number 48: tlm.Rnw:548-549
###################################################
effect(modcat2)


###################################################
### code chunk number 49: plotcat2 (eval = FALSE)
###################################################
## plot(modcat2, yname = "room levels")


###################################################
### code chunk number 50: tlm.Rnw:559-564
###################################################
m <- matrix(0, nrow = 2, ncol = 4)
m[, 2:3] <- 1
layout(m)
par(las = 1)
plot(modcat2, yname = "room levels")


###################################################
### code chunk number 51: tlm.Rnw:583-587
###################################################
data(glucose)
dim(glucose)
head(glucose)
summary(glucose)


###################################################
### code chunk number 52: tlm.Rnw:591-593
###################################################
modglucose <- tlm(y = inv2glu, x = inv12tri, data = glucose, ypow = -2, xpow = -1/2)
summary(modglucose)


###################################################
### code chunk number 53: tlm.Rnw:597-598
###################################################
MY(modglucose)


###################################################
### code chunk number 54: tlm.Rnw:601-603 (eval = FALSE)
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
### code chunk number 57: tlm.Rnw:614-617
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(3.6, 0.6, 0))
plot(modglucose, type = "transformed", observed = T, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")
plot(modglucose, xname = "Triglycerides (mg/dl)", yname = "glucose (mg/dl)")


###################################################
### code chunk number 58: tlm.Rnw:625-626
###################################################
effectInfo(modglucose)


###################################################
### code chunk number 59: tlm.Rnw:629-633
###################################################
# 2.5 and 97.5 percentiles of trigli (for text):
summtrigli <- quantile(glucose$trigly, probs = c(2.5, 25, 50, 75, 97.5) / 100)
trigli2.5 <- round(as.numeric(summtrigli[1]), 1)
trigli97.5 <- round(as.numeric(summtrigli[5]), 1)


###################################################
### code chunk number 60: tlm.Rnw:637-642
###################################################
# Effects for an additive change in triglycerides level:
xc <- 50 * (1:5)
xc
effectXdiff <- effect(modglucose, x1 = xc)
effectXdiff


###################################################
### code chunk number 61: tlm.Rnw:676-681
###################################################
# Effects for an percent change in triglycerides level:
xq <- 50 * 1.5^(0:4)
xq
effectXperc <- effect(modglucose, x1 = xq)
effectXperc


###################################################
### code chunk number 62: tlm.Rnw:725-726
###################################################
modcot2 <- tlm(y = underweight, x = logcotinine, data = cotinine, xpow = 0, family = binomial)


###################################################
### code chunk number 63: tlm.Rnw:729-730
###################################################
summary(modcot2)


###################################################
### code chunk number 64: tlm.Rnw:734-735
###################################################
MY(modcot2)


###################################################
### code chunk number 65: tlm.Rnw:739-741 (eval = FALSE)
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
### code chunk number 68: tlm.Rnw:753-756
###################################################
par(las = 1, mfrow = c(1, 2), mar = c(5, 5, 4, 3) + 0.1,  mgp = c(2.8, 0.6, 0))
plot(modcot2, type = "transformed", xname = "Cotinine (ng/ml) levels", yname = "low birth weight")
plot(modcot2, xname = "Cotinine (ng/ml) levels", yname = "low birth weight")


###################################################
### code chunk number 69: tlm.Rnw:764-765
###################################################
effectInfo(modcot2)


###################################################
### code chunk number 70: tlm.Rnw:768-769
###################################################
effect(modcot2)


###################################################
### code chunk number 71: tlm.Rnw:772-773
###################################################
effect(modcot2, q = 10)


