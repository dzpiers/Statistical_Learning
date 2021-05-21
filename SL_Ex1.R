 ## Loading in stargazer
library(stargazer)
library(forcats)
library(MASS)
options(scipen=2)
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

## Splitting education into bins
Edu_Bins <- fct_collapse(credit$Education,"High School"=c("5","6","7","8","9","10","11","12"),"Bachelors"=c("13","14","15"),"Post-Grad"=c("16","17","18","19","20"))
levels(Edu_Bins)

## Education binned
reg2 <- lm(Balance~Income+Limit+Rating+Cards+Age+Edu_Bins+Gender+Student+Married+Ethnicity,data=credit)
summary(reg2)

## Residual plots to check for multicollinearity
png(filename = "Residual Plots.png", width=1000, height=1000)
par(mfrow=c(2,2))
plot(reg2$residuals~Income,data=credit)
abline(h=0)
plot(reg2$residuals~Rating,data=credit)
abline(h=0)
plot(reg2$residuals~Cards,data=credit)
abline(h=0)
plot(reg2$residuals~Age,data=credit)
abline(h=0)
dev.off()

## Correlation matrix
credit_num <- credit[,c(2:6, 12)]
credit_cor <- cor(credit_num)
stargazer(credit_cor, title='Correlation Matrix (numeric data only)')x

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
stargazer(reg2, reg3, reg4,  title="Model Selection")


