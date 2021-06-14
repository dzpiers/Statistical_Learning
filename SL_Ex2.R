## Loading in packages
library(stargazer)
library(forcats)
library(MASS)
library(stats4)
library(readr)
library(boot)
library(foreign)
library(mlogit)
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

plot(fishing$price.pier,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.pier,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.pier,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Pier", ylim=c(min(0.5,min(odds43)),7))
abline(h=1.0,col="red")

## Odds vs price.boat
plot(fishing$price.boat,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.boat,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.boat,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Boat", ylim=c(min(0.5,min(odds43)),7))
abline(h=1.0,col="red")

## Odds vs price.charter
plot(fishing$price.charter,odds_pier_beach,main="Odds ratio - Pier/Beach",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.charter,odds_boat_pier,main="Odds ratio - Boat/Pier",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds32)),7))
abline(h=1.0,col="red")
plot(fishing$price.charter,odds_charter_boat,main="Odds ratio - Charter/Boat",ylab="Odds",xlab="Price.Charter", ylim=c(min(0.5,min(odds43)),7))
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
par(mfrow=c(1,3))
boxplot(odds_bb, main="Odds Boat vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_cb, main="Odds Charter vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
boxplot(odds_pb, main="Odds Pier vs Beach",ylim=c(0, 1200), cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
abline(h=1.0,col="red")
dev.off()


#Plotting Odds Sensitivity to price of Boat&Charter
png("Unordered Odds 2.png", width = 960, height = 960)
par(mfrow=c(3, 3))
plot(fishing$price.boat,odds_bb*fit_m1$coefficients["price.boat:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000))
plot(fishing$price.boat,odds_cb*fit_m1$coefficients["price.boat:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000))
plot(fishing$price.boat,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000))
plot(fishing$price.charter,odds_bb*fit_m1$coefficients["price.charter:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Charter",ylim=c(-1000,1000))
plot(fishing$price.charter,odds_cb*fit_m1$coefficients["price.charter:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Charter",ylim=c(-1000,1000))
plot(fishing$price.charter,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000))
plot(fishing$price.pier,odds_bb*fit_m1$coefficients["price.pier:boat"],main="Odds sensitivity: Boat/Beach",
     ylab="Sensitivity",xlab="Price of Pier",ylim=c(-1000,1000))
plot(fishing$price.pier,odds_cb*fit_m1$coefficients["price.pier:charter"],main="Odds sensitivity: Charter/Beach",
     ylab="Sensitivity",xlab="Price of Pier",ylim=c(-1000,1000))
plot(fishing$price.pier,odds_cb*fit_m1$coefficients["price.boat:pier"],main="Odds sensitivity: Pier/Beach",
     ylab="Sensitivity",xlab="Price of Boat",ylim=c(-1000,1000))
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
