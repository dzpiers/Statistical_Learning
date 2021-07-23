library(MASS)
library(mlogit)
library(lme4)
library(flexmix)

summary(fitlm)
summary(fitlogit)
summary(fitord)
fitord_p <- polr_pval(fitord)
fitord_p
summary(fitmlog)
summary(fitpois)

AIC(fitlm)
AIC(fitpois)

summary(fitmult)
summary(fitmix)
summary(fitmix.refit)





#Overdispersion test
pred=fitpois$fitted.values #fitted lambdas
zi=(quine$Days-pred)/sqrt(pred)
hist(zi)
odis=sum(zi^2)/fitpois$df.residual
odis_pval=1-pchisq(sum(zi^2),fitpois$df.residual) 
odis_pval#p-value for the test

hist(quine$Days)