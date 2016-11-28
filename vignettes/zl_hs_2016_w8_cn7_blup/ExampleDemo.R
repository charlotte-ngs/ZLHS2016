
### # clean up
rm(list = ls())

### Daten
nNrAniInPed <- 8
nIdxFirstAniWithData <- 4
dfWwg <- data.frame(Kalb = c(nIdxFirstAniWithData:nNrAniInPed),
                    Geschlecht = c("M","F","F","M","M"),
                    Vater = c(1,3,1,4,3),
                    Mutter = c(NA,2,2,5,6),
                    WWG = c(4.5,2.9,3.9,3.5,5.0))
### # Varianzen
sigmae2 <- 40
sigmaa2 <- 20
alpha <- sigmae2/sigmaa2


### # Inzidenzmatrizen
(matX <- matrix(c(1,0,0,1,1,0,1,1,0,0), ncol = 2))
nNrObs <- nrow(dfWwg)
nNrFounder <- nNrAniInPed - nIdxFirstAniWithData - 1
(matZ <- cbind(matrix(0, nrow = nNrObs, ncol = nNrFounder),diag(1,nrow = nNrObs, ncol = nNrObs)))

(matXtX <- crossprod(matX))
(matXtZ <- crossprod(matX,matZ))
(matZtZ <- crossprod(matZ))

### # Pedigree
suppressPackageStartupMessages(require(pedigreemm))
vecSire <- c(rep(NA,nNrFounder),dfWwg$Vater)
vecDam <-  c(rep(NA,nNrFounder),dfWwg$Mutter)
pedEx1 <- pedigree(sire = vecSire, dam = vecDam, label = 1:nNrAniInPed)
print(pedEx1)
### # inverse
matAInv <- as.matrix(getAInv(ped = pedEx1))

(matZtZAInvAlpha <- matZtZ + matAInv * alpha)

### # Recht Handseite
vecY <- dfWwg$WWG
(vecXtY <- crossprod(matX,vecY))
(vecZtY <- crossprod(matZ,vecY))

### # MMG
(matM <- cbind(rbind(matXtX, t(matXtZ)), rbind(matXtZ, matZtZAInvAlpha)))
(vecRhs <- rbind(vecXtY, vecZtY))

### # Loesung
(vecNumSol <- solve(matM,vecRhs))



