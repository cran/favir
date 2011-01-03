###################################################
### chunk number 1: SourceR
###################################################
library(favir)
library(actuar)
library(MASS)
library(POT)
InitPaper()


###################################################
### chunk number 2: LatexPrelude
###################################################
IncludePrelude("The POT package", "Avraham Adler")


###################################################
### chunk number 3: Initialize
###################################################
options(scipen=5, digits=5)


###################################################
### chunk number 4: RawData
###################################################
set.seed(254)
test.data <- rpareto(n=1000, shape = 1.5, scale = 100000)


###################################################
### chunk number 5: EmpiricalDiag
###################################################
summary(test.data)


###################################################
### chunk number 6: Histograms
###################################################
hist.basic <- qplot(test.data, geom="histogram") + xlab("Value") + ylab("Count")
IncludeGraph(hist.basic, caption="Basic Histogram", label="hist.basic",
             width=6 * 2.54, height=3 * 2.54)

hist.log <- hist.basic + scale_x_log10()
IncludeGraph(hist.log, caption="Log-scale Histogram", label="hist.log",
             width=6 * 2.54, height=3 * 2.54)


###################################################
### chunk number 7: 
###################################################
par(mfrow=c(1,2))
mrlplot(test.data)
mrlplot(test.data, xlim=c(0,1000000))


###################################################
### chunk number 8: 
###################################################
GPD1<-fitgpd(test.data, threshold=300000)
GPD1


###################################################
### chunk number 9: 
###################################################
GPD2<-fitgpd(test.data, threshold=300000, control=list(parscale=c(100000,.1)))
GPD2


###################################################
### chunk number 10: 
###################################################
par(mfrow=c(2,2))
plot(GPD1)


###################################################
### chunk number 11: 
###################################################
par(mfrow=c(2,2))
plot(GPD2)


###################################################
### chunk number 12: Legal
###################################################
IncludeLegal("Avraham Adler", 2010)


