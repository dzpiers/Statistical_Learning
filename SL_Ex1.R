 ## Loading in packages
library(stargazer)
library(forcats)
library(MASS)
library(stats4)
library(readr)
library(boot)
options(scipen=2)

## Week 1
## Reading in the data
credit <- read.csv("credit.csv", header = TRUE, sep=",")

## Changing relevant variables to factor variables
credit$Gender <- as.factor(credit$Gender)
credit$Education <- as.factor(credit$Education)
credit$Student <- as.factor(credit$Student)
credit$Married <- as.factor(credit$Married)
credit$Ethnicity <- as.factor(credit$Ethnicity)

## Results from Question 2
reg1 <- lm(Balance~Income+Limit+Rating+Cards+Age+Education+Gender+Student+Married+Ethnicity,data=credit)
summary(reg1)
stargazer(reg1, title="Multiple Linear Regression")

## Splitting education into bins
Edu_Bins <- fct_collapse(credit$Education,"High School"=c("5","6","7","8","9","10","11","12"),"Bachelors"=c("13","14","15"),"Post-Grad"=c("16","17","18","19","20"))
levels(Edu_Bins)

## Education binned
reg2 <- lm(Balance~Income+Limit+Rating+Cards+Age+Edu_Bins+Gender+Student+Married+Ethnicity,data=credit)
summary(reg2)

## Residual plots to check for multicollinearity
png(filename = "Residual Plots.png", width=1000, height=1000)
par(mfrow=c(2,2))
plot(reg2$residuals~Income,data=credit, cex.lab=1.5)
abline(h=0)
plot(reg2$residuals~Rating,data=credit, cex.lab=1.5)
abline(h=0)
plot(reg2$residuals~Cards,data=credit, cex.lab=1.5)
abline(h=0)
plot(reg2$residuals~Age,data=credit, cex.lab=1.5)
abline(h=0)
dev.off()

## Correlation matrix
credit_num <- credit[,c(2:6, 12)]
credit_cor <- cor(credit_num)
stargazer(credit_cor, title='Correlation Matrix (numeric data only)')

## Correlation between Rating and Limit
cor(credit$Rating, credit$Limit)

## Importing Wald test function
wald<-function(R,B,S,c){
  stats<-matrix(0,1,2)
  dif=(R%*%B-c)
  VV=R%*%(S)%*%t(R)
  W=t(dif)%*%ginv(VV)%*%dif
  stats[1]=W
  stats[2]=pchisq(W,nrow(c),lower.tail=FALSE)
  colnames(stats)<-c("Wald stat","p-value")
  return(stats)
}

## Wald test for Rating and Limit
RR1 <- cbind(0,0,1,-1,0,0,0,0,0,0,0,0,0)
cc1 <- rbind(0)
bhat <- (reg2$coefficients)
Shat <- vcov(reg2)
wald1 <- wald(RR1,bhat,Shat,cc1)
stargazer(wald1, title="Wald Test for Rating and Limit")

## Wald test for Income and Rating
RR1 <- cbind(0,1,0,-1,0,0,0,0,0,0,0,0,0)
cc1 <- rbind(0)
bhat <- (reg2$coefficients)
Shat <- vcov(reg2)
wald2 <- wald(RR1,bhat,Shat,cc1)
stargazer(wald2)

## Losing Limit and adding squared terms for Income and Rating
reg3 <- lm(Balance~Income+I(Income^2)+Rating+I(Rating^2)+Cards+Age+Edu_Bins+Gender+Student+Married+Ethnicity,data=credit)
summary(reg3)

## Losing insignificant variables
reg4 <- lm(Balance~Income+I(Income^2)+Rating+I(Rating^2)+Age+Student,data=credit)
summary(reg4)

## Stargazer output
stargazer(reg2, reg3, reg4,  title="Model Selection", table.placement = "H")


## Week 2
## Loading in the data
bank <- read.csv("bankTD.csv", header = TRUE, sep=",")

## Fitting the logistic model using MLE.
## The likelihood function definition.
LL_logistic<-function(beta0,beta1){
  xb=beta0+beta1*bank$duration
  lpy=bank$y*log(exp(xb)/(1+exp(xb)))+(1-bank$y)*log(1/(1+exp(xb)))
  -sum(lpy)
}

## Implementing the MLE
mle_logistic <- mle(minuslogl = LL_logistic, start = list(beta0=0, beta1=0), method = "BFGS")
summary(mle_logistic)

## Using the glm function
fit1 <- glm(y~duration,binomial(link="logit"),data=bank)
summary(fit1)

## Odds for duration (note: results zoomed in on for clarity)
odds_HH <- exp(fit1$linear.predictors)
par(mfrow=c(1,2))
boxplot(odds_HH,main="Odds ratio for y",ylab="Odds ratio",ylim=c(0,10))
abline(h=1,col="red",lty=2)
plot(bank$duration,odds_HH,main="Odds ratio to y vs duration",xlab="duration",ylab="Odds ratio",ylim=c(0,10))
abline(h=1,col="red",lty=2)

## Post fit analysis
exb <- exp(predict(fit1,newdata=data.frame(duration=500)))
ME <- fit1$coefficients[2]*exb/((1+exb)^2)
ME*(-450)
Deltap <- predict(fit1,newdata=data.frame(duration=50),type="response")-predict(fit1,newdata=data.frame(duration=500),type="response")
Deltap

## Changing relevant variables to factor variables
bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$poutcome <- as.factor(bank$poutcome)

## GLM with multiple predictors
fit2 <- glm(y~age+job+marital+education+default+balance+housing+loan+duration+previous+poutcome,binomial(link="logit"),data=bank)
summary(fit2)

