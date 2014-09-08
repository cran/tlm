\name{tlm-internal}
\alias{tlm-internal}
\alias{betaCIContinuousExactTrans}
\alias{bijectivityback}
\alias{bijectivityforward}
\alias{checkc}
\alias{checkfornumber}
\alias{checkfornumberorvector}
\alias{checkforvector}
\alias{checknbootseed}
\alias{checknpoints}
\alias{checkq}
\alias{checkr}
\alias{effectboot}
\alias{effectContinuous}
\alias{effectContinuousGeneral}
\alias{effectContinuousmod1}
\alias{effectContinuousmod3}
\alias{effectContinuousmod5}
\alias{effectContinuousmod6}
\alias{effectContinuousmod7}
\alias{effectContinuousmod9}
\alias{effectContinuousmod10}
\alias{effectContinuousmod12}
\alias{effectContinuousReduxmod1}
\alias{effectContinuousReduxmod10}
\alias{effectContinuousReduxmod12}
\alias{effectContinuousReduxmod3}
\alias{effectContinuousReduxmod5}
\alias{effectContinuousReduxmod6}
\alias{effectContinuousReduxmod7}
\alias{effectContinuousReduxmod9}
\alias{effectFactor}
\alias{effectFactorBoot}
\alias{effectFactorExactTrans}
\alias{effectFactormod2}
\alias{effectFactormod4}
\alias{effectFactormod8}
\alias{effectFactormod11}
\alias{effectInfomod0}
\alias{effectInfomod1}
\alias{effectInfomod2}
\alias{effectInfomod3}
\alias{effectInfomod4}
\alias{effectInfomod5}
\alias{effectInfomod6}
\alias{effectInfomod7}
\alias{effectInfomod8}
\alias{effectInfomod9}
\alias{effectInfomod10}
\alias{effectInfomod11}
\alias{effectInfomod12}
\alias{geteffectx1x2}
\alias{getL}
\alias{getM}
\alias{modelType}
\alias{powerTransform}
\alias{powerUntransform}
\alias{printPreamble}

\title{Internal tlm functions}
\description{Internal tlm functions}
\usage{
betaCIContinuousExactTrans(object, level)
bijectivityback(xt, power)
bijectivityforward(x, power)
checkc(c)
checkfornumber(x, name)
checkfornumberorvector(x, name)
checkforvector(x, name)
checknbootseed(nboot, seed)
checknpoints(npoints)
checkq(q)
checkr(r)
effectboot(data, i, object, x1, x2, level)
effectContinuous(object, x1, x2, c, q, r, npoints, level, nboot, modeltype)
effectContinuousGeneral(object, x1, x2, c, q, r, npoints, level, nboot, modeltype)

effectContinuousmod1(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod3(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod5(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod6(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod7(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod9(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod10(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousmod12(object, x1x2, level, nboot, IQReffect, effectsOutRange)
effectContinuousReduxmod1(object, c, IQReffect, effectsOutRange, level)
effectContinuousReduxmod3(object, c, IQReffect, effectsOutRange, level)
effectContinuousReduxmod5(object, r, IQReffect, effectsOutRange, level)
effectContinuousReduxmod6(object, r, IQReffect, effectsOutRange, level)
effectContinuousReduxmod7(object, c, IQReffect, effectsOutRange, level)
effectContinuousReduxmod9(object, r, IQReffect, effectsOutRange, level)
effectContinuousReduxmod10(object, c, IQReffect, effectsOutRange, level)
effectContinuousReduxmod12(object, r, IQReffect, effectsOutRange, level)
effectFactor(object, level, nboot, modeltype)
effectFactorBoot(object, level, nboot)
effectFactorExactTrans(object, level)
effectFactormod2(object, level, nboot)
effectFactormod4(object, level, nboot)
effectFactormod8(object, level, nboot)
effectFactormod11(object, level, nboot)
effectInfomod0(object)
effectInfomod1(object)
effectInfomod2(object)
effectInfomod3(object)
effectInfomod4(object)
effectInfomod5(object)
effectInfomod6(object)
effectInfomod7(object)
effectInfomod8(object)
effectInfomod9(object)
effectInfomod10(object)
effectInfomod11(object)
effectInfomod12(object)
geteffectx1x2(object, x1, x2, level, nboot)
getL(object, x)
getM(object, x, untransform, level)
modelType(object)
powerTransform(x, power)
powerUntransform(xt, power)
printPreamble(x)
}

\details{These are not to be called by the user}
\keyword{internal}
