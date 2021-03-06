#Loading in packages
library(stargazer)
library(forcats)
library(MASS)
library(stats4)
library(readr)
library(boot)
library(foreign)
library(mlogit)
library(VGAM)
library(lme4)
library(ggplot2)
library(gridExtra)
library(flexmix)

options(scipen=2)
set.seed(100)

#Week 6
#Loading in data
hiv <- read.csv("hiv.csv", header = TRUE, sep=",")

#Question 1
fithiv1 <- lmer(CD4PCT ~ time + (1|pid), data=hiv)
summary(fithiv1)
coef(fithiv1)
fixef(fithiv1)
ranef(fithiv1)

#Plotting random effect on intercept
#Creating vector of each random intercept effect
fithiv1_alpha<-data.frame(x=1:226,est=coef(fithiv1)$pid[,1],
                       U=rep(fixef(fithiv1)[1],226)+qnorm(0.975)*11.413  ,
                       L=rep(fixef(fithiv1)[1],226)+qnorm(0.025)*11.413  ,
                       fixed=rep(fixef(fithiv1)[1],226))

#Plotting the effect using ggplot2
q1 <- ggplot(fithiv1_alpha, aes(x = x, y = est)) +
  geom_point(size = 1) +
  geom_hline(aes(yintercept=fixed),colour="red")+
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Baseline variation across group") +
  xlab("Patient ID") +
  ylab("Intercept") +
  theme_classic(base_size = 15)

png(filename = "Q1.png", width = 720, height = 480)
q1
dev.off()


#Question 2
fithiv2 <- lmer(CD4PCT ~ time + treatment + baseage + (1|pid), data=hiv)
summary(fithiv2)
coef(fithiv2)
fixef(fithiv2)
ranef(fithiv2)

#Plotting the intercept
#Creating vector of random intercept effects (diff to question 1) (black)
fithiv2_a=coef(fithiv2)$pid[,1]+ coef(fithiv2)$pid[,3]*unique(hiv$treatment)+ coef(fithiv2)$pid[,4]*unique(hiv$baseage)
#Creating a vector of mu_alphaj (red)
fithiv2_af=fixef(fithiv2)[1]+ coef(fithiv2)$pid[,3]*unique(hiv$treatment)+ coef(fithiv2)$pid[,4]*unique(hiv$baseage)
fithiv2_alpha<-data.frame(x=1:226,est=fithiv2_a,
                       U=fithiv2_af+qnorm(0.975)*11.220,
                       L=fithiv2_af+qnorm(0.025)*11.220,
                       fixed=fithiv2_af)

#Plotting intercept effect
q2 <- ggplot(fithiv2_alpha, aes(x = x, y = est)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Baseline variation across group - varies with treatment and baseage") +
  xlab("Patient ID") +
  ylab("Intercept") +
  theme_classic(base_size = 15) +
  geom_point(aes(x=x,y=fixed),size=1,colour="red")

png(filename = "Q2.png", width = 720, height = 480)
q2
dev.off()

grid.arrange(q1, q2)


#Question 3
#Running a multi linear regression
hivlinear <- lm(CD4PCT ~ time + treatment + baseage, data=hiv)
summary(hivlinear)

#AICs for comparison of model
AIC(fithiv1)
AIC(fithiv2)
AIC(hivlinear)

#Stargazer output
stargazer(fithiv1, fithiv2, hivlinear)


#Week 7
credit <- read.csv("credit.csv", header = TRUE, sep=",")

#Latent mixture model
simcreditfit <- stepFlexmix(Balance~Income+Rating+Cards+Age+Education+Student+Married+Gender+Ethnicity,k=1:5,data=credit)
simcreditfit
stargazer(simcreditfit)

sfit1 <- flexmix(Balance~Income+Rating+Cards+Age+Education+Student+Married+Gender+Ethnicity,k=2,data=credit)
print(sfit1)
sfit1@cluster
parameters(sfit1)
AIC(sfit1)

stargazer(parameters(sfit1))

#Extracting more statistics
sfit1.refit <- refit(sfit1)
summary(sfit1.refit) #viewing the refitted object
sfit1.refit@vcov
plot(sfit1)


