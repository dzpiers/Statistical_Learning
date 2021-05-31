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
mle_logistic <- mle(minuslogl = LL_logistic, 
                    start = list(beta0=0, beta1=0), method = "BFGS")
summary(mle_logistic)

## Using the glm function
fit1 <- glm(y~duration,binomial(link="logit"),data=bank)
summary(fit1)

stargazer(fit1, fit1)

## Question 4
## Odds for duration (note: results zoomed in on for clarity)
png(filename = "Odds.png", width = 960, height = 960)
odds_HH <- exp(fit1$linear.predictors)
par(mfrow=c(1,2))
boxplot(odds_HH,main="Odds ratio for y",ylab="Odds ratio",ylim=c(0,10))
abline(h=1,col="red",lty=2)
plot(bank$duration,odds_HH,main="Odds ratio to y vs duration",xlab="duration",ylab="Odds ratio",ylim=c(0,10))
abline(h=1,col="red",lty=2)
dev.off()

## Post fit analysis
exb <- exp(predict(fit1,newdata=data.frame(duration=500)))
ME <- fit1$coefficients[2]*exb/((1+exb)^2)
ME*(-450)
Deltap <- predict(fit1,newdata=data.frame(duration=50),type="response")-predict(fit1,newdata=data.frame(duration=500),type="response")
Deltap


## Week 3
## Question 5
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
stargazer(fit2, title = "Multiple Logistic Regression")

## Deviance and AIC comparison
dev_aic <- matrix(1:6, 2, 3)
dev_aic[1,1] <- fit1$null.deviance
dev_aic[2,1] <- fit1$null.deviance + 2
dev_aic[1,2] <- fit1$deviance
dev_aic[2,2] <- fit1$aic
dev_aic[1,3] <- fit2$deviance
dev_aic[2,3] <- fit2$aic
rownames(dev_aic) <- c("Deviance", "AIC")
colnames(dev_aic) <- c("Null", "Single Predictor", "Multiple Predictors")
stargazer(dev_aic)


## Question 6
## Re-leveling education
bank$job <- relevel(bank$job, ref = "unemployed")
fit3 <- glm(y~age+job+marital+education+default+balance+housing+loan+duration+previous+poutcome,binomial(link="logit"),data=bank)
summary(fit3)

## Adding in non-linear terms
fit4 <- glm(y~age+job+marital+education+default+balance+I(balance^2)+housing+loan+duration+I(duration^2)+previous+poutcome+previous:housing+previous:loan,binomial(link="logit"),data=bank)
summary(fit4)

## Removing insignificant terms
fit5 <- glm(y~job+education+default+balance+housing+loan+duration+I(duration^2)+poutcome+previous:housing,binomial(link="logit"),data=bank)
summary(fit5)
stargazer(fit3, fit4, fit5, title = "Model Selection")

## Selection process
sel_pro <- matrix(1:8,2,4)
sel_pro[1,1] <- fit3$null.deviance
sel_pro[2,1] <- fit3$null.deviance + 2
sel_pro[1,2] <- fit3$deviance
sel_pro[2,2] <- fit3$aic
sel_pro[1,3] <- fit4$deviance
sel_pro[2,3] <- fit4$aic
sel_pro[1,4] <- fit5$deviance
sel_pro[2,4] <- fit5$aic
colnames(sel_pro) <- c("Null", "Model (1)", "Model (2)", "Model (3)")
rownames(sel_pro) <- c("Deviance", "AIC")
stargazer(sel_pro)

## LR Test
LR1 <- fit5$null.deviance-fit3$deviance
LR1_pval <- 1-pchisq(LR1,fit5$df.null-fit3$df.residual)
LR1
LR1_pval

## Question 7
## Hit or Miss table
HitMiss<-function(y,prob,c){
  HM=matrix(0,2,2)
  HM[1,1]=mean(prob>c & y==1)
  HM[1,2]=mean(prob>c & y==0)
  HM[2,1]=mean(prob<=c & y==1)
  HM[2,2]=mean(prob<=c & y==0)
  return(HM)
}

probs1 <- fit5$fitted.values

HM1 <- HitMiss(bank$y,probs1,0.5)
colnames(HM1) <- c("yhat = 1", "yhat = 0")
rownames(HM1) <- c("y=1", "y=0")
stargazer(HM1)

sum(diag(HM1))
1-sum(diag(HM1))

## Precision
precision <- HM1[1,1]/(HM1[1,1]+HM1[1,2])
precision

## Sensitivity
sens <- HM1[1,1]/(HM1[1,1]+HM1[2,1])
sens

## Specificity
spec <- HM1[2,2]/(HM1[2,1]+HM1[2,2])
spec

## Error rate of false negatives
false_neg <- HM1[2,1]/(HM1[2,1]+HM1[2,2])
false_neg

## False positives
false_pos <- HM1[1,2]/(HM1[1,1]+HM1[1,2]) 
false_pos

col1 <- (seq(0.05, 0.95, by=0.05))
col2 <- (seq(0.05, 0.95, by=0.05))
col3 <- (seq(0.05, 0.95, by=0.05))

cutoff <- cbind(col1, col2, col3)

j <- 1
for(i in seq(0.05, 0.95, by=0.05)) {
  HM2 <- HitMiss(bank$y,probs1,i)
  precision1 <- HM2[1,1]/(HM2[1,1]+HM2[1,2])
  false_neg1 <- HM2[2,1]/(HM2[2,1]+HM2[2,2])
  false_pos1 <- HM2[1,2]/(HM2[1,1]+HM2[1,2]) 
  cutoff[j,1] <- precision1
  cutoff[j,2] <- false_neg1
  cutoff[j,3] <- false_pos1
  j <- j+1
}
colnames(cutoff) <- c("Precision", "False-Negatives", "False-Positives")
rownames(cutoff) <- c(seq(0.05, 0.95, by=0.05))
cutoff
stargazer(cutoff)
