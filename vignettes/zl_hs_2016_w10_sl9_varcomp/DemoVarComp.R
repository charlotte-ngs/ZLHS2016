###
###
###
###   Purpose:   Demo Varianzkomponenten
###   started:   2016/12/02 (pvr)
###
### ###################################### ###

nNrAniInPed <- 8
nIdxFirstAniWithData <- 4
dfWwgRed <- data.frame(Kalb = c(nIdxFirstAniWithData:nNrAniInPed),
                    Geschlecht = c("M","F","F","M","M"),
                    WWG = c(4.5,2.9,3.9,3.5,5.0))

lmWwg <- lm(WWG ~ -1 + Geschlecht, data = dfWwgRed)
summary(lmWwg)

### # selber rechnen
n <- nrow(dfWwgRed)
p <- length(unique(dfWwgRed$Geschlecht))
vecResiduals <- residuals(lmWwg)
nResVarEst <- crossprod(vecResiduals) / (n-p)
(nResSd <- sqrt(nResVarEst))


### # anova
dfWwgSire <- data.frame(Kalb = c(nIdxFirstAniWithData:nNrAniInPed),
                    Vater = c(1,3,1,4,3),
                    WWG = c(4.5,2.9,3.9,3.5,5.0))
dfWwgSire[dfWwgSire$Kalb == 7,"Vater"] <- 3
aovWwgSire <- aov(formula = WWG ~ Vater,
                  data = dfWwgSire)
summary(aovWwgSire)

nNrObs <- length(dfWwgSire)
### # allgemeines Mittel
nMeanObs <- mean(dfWwgSire$WWG)
### # Beobachtungen korrigiert um Mittelwert
vecWwgSireCorrected <- dfWwgSire$WWG - nMeanObs
### # Summenquadrate der Residuen
nSsqRes <- crossprod(residuals(aovWwgSire))
nResVarEst <- nSsqRes / aovWwgSire$df.residual
### # Summenquadrate der Vaeter
nSsqVater <- crossprod(vecWwgSireCorrected) - nSsqRes
ndfVater <- nNrObs - aovWwgSire$df.residual - 1 ### -1 comes due to intercept in model
nMsqVater <- nSsqVater / ndfVater
nVaterVarEst <- (nMsqVater - nResVarEst)/nNrObs



