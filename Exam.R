#Conduct a t-test of mean difference between the two Genders
#Ha: Two means are not equal
t.test(salary~Gender,data=data) 
#Conduct a t-test of mean difference between the two Genders
#Ha: Mean of females less than mean of males
t.test(salary~Gender,data=data,alternative="less") 


#####################################################
#Defining a function for the wald test - require MASS library
#H0: RB=c vs Ha: RB!=c
#R=the R matrix
#B=the estimated coefficients
#S=the covariance matrix of B
#c=the vector of constants
#####################################################
library(MASS)
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

#define inputs to the wald test - joint significance of the three slope parameters
RR=cbind(rbind(0,0,0),diag(3))
cc=rbind(0,0,0)
bhat=(fit1$coefficients)
Shat=vcov(fit1)
wald1=wald(RR,bhat,Shat,cc)
wald1






#optimal experience based on the model, with other factors held fixed
#-a/2b turning point
opt_exp=-fit6$coefficients[4]/(2*fit6$coefficients[5])
opt_age=-fit6$coefficients[2]/(2*fit6$coefficients[3])







#LR Test
#Testing the statistical significance of the coefficient of "dist" in model 1
LR1=fit1$null.deviance-fit1$deviance
LR1_pval=1-pchisq(LR1,fit1$df.null-fit1$df.residual)
LR1
LR1_pval










##########################################################
#Hit and miss table
##########################################################
HitMiss<-function(y,prob,c){
  HM=matrix(0,2,2)
  HM[1,1]=mean(prob>c & y==1)
  HM[1,2]=mean(prob>c & y==0)
  HM[2,1]=mean(prob<=c & y==1)
  HM[2,2]=mean(prob<=c & y==0)
  return(HM)
}

HM1=HitMiss(wells$switch,probs1,0.5)
HM2=HitMiss(wells$switch,probs2,0.5)


Summarising the accuary and error rates from the table:
  ```{r}
#accuracy and error rates
sum(diag(HM1))
1-sum(diag(HM1))

sum(diag(HM2))
1-sum(diag(HM2))



#ROC Curve ~fitted values
library(pROC)
par(mfrow=c(1,2))
fit1.ROC=roc(wells$switch~probs1, plot=TRUE, print.auc=TRUE, quiet=TRUE)

fit2.ROC=roc(wells$switch~probs2, plot=TRUE, print.auc=TRUE, quiet=TRUE)





#A function that summarize the accuracy of a binary outcome model
summariseAccuracy <- function(HM,mROC){
  #Storage for various accuracy measures
  metrics=matrix(0,1,6)
  colnames(metrics)=c("Overall","Precision","Sensitivity",
                      "Specificity","AUC","Gini")
  metrics[1,"Overall"]=sum(diag(HM))
  metrics[1,"Precision"]=HM[1,1]/sum(HM[1,])
  metrics[1,"Sensitivity"]=HM[1,1]/sum(HM[,1])
  metrics[1,"Specificity"]=HM[2,2]/sum(HM[,2])
  
  metrics[1,"AUC"]=mROC$auc
  metrics[1,"Gini"]=2*mROC$auc-1
  
  #Function returns a list, including the hit and miss table
  #the summary metrics and the ROC curve object
  return(metrics)
}

#single predictor accuracy
summariseAccuracy(HM1,fit1.ROC)
#multiple predictor accuracy
summariseAccuracy(HM2,fit2.ROC)










#########################################
#Function for computing the hit-miss table in ordinal data
#Note that the input y must be recoded to numeric values
#predicted y is the category with highest probability
#########################################
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
y_app=1*(college$apply=="unlikely")+2*(college$apply=="somewhat likely")+3*(college$apply=="very likely")
HM_olog6=HitMissMult(y_app,fit_olog6$fitted.values)
colnames(HM_olog6)=levels(college$apply)
rownames(HM_olog6)=levels(college$apply)
HM_olog6

#HM with no models
unc_pr=cbind(mean(y_app==1),mean(y_app==2),mean(y_app==3))
HM_unc=HitMissMult(y_app,unc_pr)
colnames(HM_unc)=levels(college$apply)
rownames(HM_unc)=levels(college$apply)
HM_unc








#Likelihood ratio test on two unordered models
lrtest(fit_m1,fit_m2)



#Overdispersion test
pred=preg$fitted.values #fitted lambdas
zi=(lung$y-pred)/sqrt(pred)
hist(zi)
odis=sum(zi^2)/preg$df.residual
odis_pval=1-pchisq(sum(zi^2),preg$df.residual) 
odis_pval#p-value for the test


#A function that computes the overdispersion and its p-value 
#inputs: y = observed data; fit=glm fit of poisson regression
overdis<-function(y,fit){
  pred=fit$fitted.values
  zi=(y-pred)/sqrt(pred)
  odis=sum(zi^2)/fit$df.residual
  odis_pval=1-pchisq(sum(zi^2),fit$df.residual) #p-value for the test
  return(list(odis=odis,pval=odis_pval))
}

overdis(lung$y,preg)