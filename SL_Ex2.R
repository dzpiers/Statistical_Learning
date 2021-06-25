## Loading in packages
library(stargazer)
library(forcats)
library(MASS)
library(stats4)
library(readr)
library(boot)
library(foreign)
library(mlogit)
library(VGAM)
options(scipen=2)


## Week 4
## Loading in data
fishing <- read.csv("fishing.csv", header = TRUE, sep=",")


## Factoring mode
fishing$mode <- factor(fishing$mode, levels = c("beach", "pier", "boat", "charter"))
levels(fishing$mode)


## Correlation matrix just in case because we're smart cookies
cor1 <- cor(fishing[,2:9])
cor1
stargazer(cor1)


## Ordered logit model
fit_olog1 <- polr(mode~price.pier+price.boat+price.charter+catch.beach+catch.pier+catch.boat+catch.charter,data=fishing,method="logistic")
summary(fit_olog1)
stargazer(fit_olog1, title = "Ordered Logit Model")


## Function to calculate p-values
polr_pval<-function(fit){
  fit.c=coef(summary(fit))
  fit.c=cbind(fit.c,"p-val"=pnorm(abs(fit.c[,"t value"]),lower.tail=FALSE)*2)
  return(fit.c)
}


## Computing the p-values of each coefficient
fit_olog1.c <- polr_pval(fit_olog1)
fit_olog1.c


## Odds ratio
odds_pier_beach <- fit_olog1$fitted.values[,2]/fit_olog1$fitted.values[,1]
odds_boat_pier <- fit_olog1$fitted.values[,3]/fit_olog1$fitted.values[,2]
odds_charter_boat <- fit_olog1$fitted.values[,4]/fit_olog1$fitted.values[,3]

png("Ordered Odds 1.png", width = 960, height = 320)
par(mfrow=c(1,3))
boxplot(odds_pier_beach,main="Odds ratio - Pier/Beach", ylim=c(0, 7), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_boat_pier,main="Odds ratio - Boat/Pier", ylim=c(0, 7), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_charter_boat,main="Odds ratio - Charter/Boat", ylim=c(0, 7), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()


## Odds vs price.pier
png("Ordered Odds 2.png", width = 960, height = 960)
par(mfrow=c(3,3))

plot(fishing$price.pier,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.pier,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.pier,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds43)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")

## Odds vs price.boat
plot(fishing$price.boat,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.boat,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.boat,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds43)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")

## Odds vs price.charter
plot(fishing$price.charter,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.charter,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds32)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$price.charter,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds43)),7), 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()

## Odds vs catch.pier
png("Ordered Odds 3.png", width = 960, height = 320)
par(mfrow=c(1,3))
plot(fishing$catch.pier,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="catch.pier", ylim=c(0, 6), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
plot(fishing$catch.pier,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="catch.pier",ylim=c(0, 6), cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
abline(h=1.0,col="red")
plot(fishing$catch.pier,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="catch.pier",ylim=c(0, 6), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()


## Hit or miss table
HitMissMult<-function(y,prob){
  m=ncol(prob)
  ypred=as.numeric(max.col(prob))
  if(nrow(prob)==1) #the case of unconditional prob
  {
    ypred=rep(ypred,length(y))
  }
  HM=matrix(0,m,m) #table is now m by m
  for(i in 1:m){
    for(j in 1:m){
      HM[i,j]=mean(ypred==i & y==j)
    }
  }
  return(HM)
}

y_app <- 1*(fishing$mode=="beach")+2*(fishing$mode=="pier")+3*(fishing$mode=="boat")+4*(fishing$mode=="charter")
HM_olog1 <- HitMissMult(y_app,fit_olog1$fitted.values)
colnames(HM_olog1) <- levels(fishing$mode)
rownames(HM_olog1) <- levels(fishing$mode)
HM_olog1

## No model
unc_pr <- cbind(mean(y_app==1),mean(y_app==2),mean(y_app==3),mean(y_app==4))
HM_unc <- HitMissMult(y_app,unc_pr)
colnames(HM_unc) <- levels(fishing$mode)
rownames(HM_unc) <- levels(fishing$mode)
HM_unc



## Multinomial logit model
fishing_m <- mlogit.data(fishing, choice="mode",sep="",shape="wide")
fit_m1 <- mlogit(mode~0|price.pier+price.boat+price.charter+catch.beach+catch.pier+catch.boat+catch.charter,data=fishing_m)
summary(fit_m1)
AIC(fit_m1)


## Putting it into a more readable table
table_m1 <- matrix(unlist(fit_m1$coefficients), nrow=8, ncol=3, byrow=TRUE)
colnames(table_m1) <- c("Boat", "Charter", "Pier")
rownames(table_m1) <- c("(Intercept)", "price.pier", "price.boat", "price.charter", "catch.beach", "catch.pier", "catch.boat", "catch.charter")
table_m1
stargazer(table_m1, title = "Multinomial Logit Model")
stargazer(AIC(fit_m1), title = "AIC for Multinomial Logit Model")


## Odds ratio
prob_m1 <- fit_m1$probabilities
odds_bb <- prob_m1[,2]/prob_m1[,1]
odds_cb <- prob_m1[,3]/prob_m1[,1]
odds_pb <- prob_m1[,4]/prob_m1[,1]

## Plotting unordered odds ratios
png("Unordered Odds 1.png", width = 960, height = 320)
par(mfrow=c(1,4))
boxplot(odds_bb, main="Odds Boat vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_cb, main="Odds Charter vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_pb, main="Odds Pier vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_pb, main="Odds Pier vs Beach",ylim=c(0, 20), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()


#Plotting Odds Sensitivity to price of Boat&Charter
png("Unordered Odds 2.png", width = 960, height = 960)
par(mfrow=c(3, 3))
plot(fishing$price.boat,odds_bb*fit_m1$coefficients["price.boat:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.boat,odds_cb*fit_m1$coefficients["price.boat:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.boat,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.charter,odds_bb*fit_m1$coefficients["price.charter:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Charter",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.charter,odds_cb*fit_m1$coefficients["price.charter:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Charter",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.charter,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.pier,odds_bb*fit_m1$coefficients["price.pier:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Pier",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.pier,odds_cb*fit_m1$coefficients["price.pier:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Pier",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
plot(fishing$price.pier,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
dev.off()

## Pier odds zoomed in
png("Unordered Odds 3.png", width = 480, height = 480)
boxplot(odds_pb, main="Odds Pier vs Beach",ylim=c(0, 20), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()

## Hit or miss table
## Checking levels align
ychoice <- factor(fishing$mode, levels = c("beach", "boat", "charter", "pier"))
levels(ychoice) 
colnames(prob_m1) 


HM_m1 <- HitMissMult(as.numeric(ychoice),prob_m1)
## Rename columns and rows for ease of use
colnames(HM_m1) <- colnames(prob_m1)
rownames <- colnames(prob_m1)
HM_m1
sum(diag(HM_m1))



## Week 5
## Reading bank data
credit <- read.csv("credit.csv", header = TRUE, sep=",")


## Creating balance ratio
credit$BalanceRatio <- credit$Balance/credit$Limit


## OG regression
ogreg <- lm(Balance~Income+I(Income^2)+Rating+I(Rating^2)+Age+Student,data=credit)
summary(ogreg)

testreg <- lm(BalanceRatio~Income+I(Income^2)+Rating+I(Rating^2)+Age+Student,data=credit)

stargazer(ogreg, testreg, title = "Week 1 OLS vs Week 5 Tobit")  ## Edit testreg model to match tobit


## Modelling tobit regression
tobit1 <- vglm(BalanceRatio~Income+I(Income^2)+Rating+I(Rating^2)+Age+Student, tobit(Lower = 0,Upper=1), data = credit)
summary(tobit1) #Note: (Intercept):2 in the vglm output is the log of sigma
xb=fitted(tobit1)
sig=exp(coefficients(tobit1)["(Intercept):2"])



## Poisson Model
## Load data
credit1 <- read.csv(file="credit.csv", header = TRUE, sep = ',')


## Redefine the data into rate
credit2=credit1
credit2$Balance=(credit2$Balance/credit2$Limit)
hist(credit2$Balance,15,main="Level of credit card account utilisation relative to available limit",
     xlab="Balance Ratio")


## Fitting the Poisson regression
fit1p=glm(Cards~Income+Rating+Age+Education+Gender+Student+Married+Ethnicity,family=poisson(link="log"),data=credit1)
summary(fit1p)

## Remove ethnicity
fit2p=glm(Cards~Income+Rating+Age+Education+Gender+Student+Married,family=poisson(link="log"),data=credit1)
summary(fit2p)

## Remove married
fit3p=glm(Cards~Income+Rating+Age+Education+Gender+Student,family=poisson(link="log"),data=credit1)
summary(fit3p)

## Remove student
fit4p=glm(Cards~Income+Rating+Age+Education+Gender,family=poisson(link="log"),data=credit1)
summary(fit4p)

## Remove gender
fit4p=glm(Cards~Income+Rating+Age+Education,family=poisson(link="log"),data=credit1)
summary(fit4p)

## Remove education
fit5p=glm(Cards~Income+Rating+Age,family=poisson(link="log"),data=credit1)
summary(fit5p)

## Remove age
fit6p=glm(Cards~Income+Rating,family=poisson(link="log"),data=credit1)
summary(fit6p)

stargazer(fit1p, fit2p, fit3p, fit4p, fit5p, fit6p, title = "Poisson Model Selection")

## Overdispersion test
pred1=fit6p$fitted.values #fitted lambdas
zi=(credit1$Cards-pred1)/sqrt(pred1)
hist(zi)
odis1=sum(zi^2)/fit6p$df.residual
odis1
odis_pval1=1-pchisq(sum(zi^2),fit6p$df.residual) 
odis_pval1 #p-value for the test

## A function that computes the overdispersion and its p-value 
## inputs: y = observed data; fit=glm fit of poisson regression
overdis<-function(y,fit){
  pred=fit$fitted.values
  zi=(y-pred)/sqrt(pred)
  odis=sum(zi^2)/fit$df.residual
  odis_pval=1-pchisq(sum(zi^2),fit$df.residual) #p-value for the test
  return(list(odis=odis,pval=odis_pval))
}

overdis(credit1$Cards,fit6p)

overdis(credit1$Cards,fit5p)

overdis(credit1$Cards,fit4p)


## Quasi-Poisson model
fit6p_q=glm(Cards~Income+Rating,family=quasipoisson(link="log"),data=credit1)
summary(fit6p_q) #The only difference is in the standard errors

## Quasi-Poisson model
fit5p_q=glm(Cards~Income+Rating+Age,family=quasipoisson(link="log"),data=credit1)
summary(fit5p_q) #The only difference is in the standard errors

## Quasi-Poisson model
fit4p_q=glm(Cards~Income+Rating+Age+Education,family=quasipoisson(link="log"),data=credit1)
summary(fit4p_q) #The only difference is in the standard errors


## Linear model for comparison
fit1l = lm(Cards~Income+Rating,data=credit1)
summary(fit1l)

stargazer(fit6p, fit6p_q, fit1l, title = "Poisson vs Quasi vs Linear Regression")
