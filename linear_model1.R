setwd("C:/Users/i life/Desktop/VISION IT/5.Data Preparation/Data")
getwd()
ld=read.csv("loans dataa.csv",stringsAsFactors = F)
library(dplyr)
glimpse(ld)
ld=ld %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)),
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)),
         Amount.Requested=as.numeric(Amount.Requested),
         Open.CREDIT.Lines=as.numeric( Open.CREDIT.Lines),
         Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )%>%
  select(-ID)%>%
  na.omit()

apply(ld,2,function(x) sum(is.na(x)))
##################################--------------------------
table(ld$Loan.Length)
ld=ld%>%
  mutate(ll_36=as.numeric(Loan.Length =="36 months"))%>%
  select(-Loan.Length)
glimpse(ld)
#################################---------------------------
table(ld$Loan.Purpose)
ld=ld%>%
  mutate(lp_cc=as.numeric(Loan.Purpose=="credit_card"),
         lp_dc=as.numeric(Loan.Purpose=="debt_consolidation")
         )%>%
  select(-Loan.Purpose)
glimpse(ld)
################################----------------------------
table(ld$State)
ld=ld%>%
  mutate(ca=as.numeric(State=="CA"),
         ny=as.numeric(State=="NY"),
         fl=as.numeric(State=="FL"),
         il=as.numeric(State=="IL"),
         tx=as.numeric(State=="TX")
  )%>%
  select(-State)
glimpse(ld)
################################----------------
table(ld$Home.Ownership)
ld=ld%>%
  mutate(mort=as.numeric(Home.Ownership=="MORTGAGE"),
         rent=as.numeric(Home.Ownership=="RENT")
  )%>%
  select(-Home.Ownership)
glimpse(ld)
##################################----------------------------
table(ld$FICO.Range)
ld=ld %>%
  mutate(f1=as.numeric(substr(FICO.Range,1,3)),
         f2=as.numeric(substr(FICO.Range,5,7)),
         fico=(f1+f2)/2
  )%>%
  select(-FICO.Range,-f1,-f2)
glimpse(ld)
###############################---------------------------------
ld=ld%>%
  select(-Amount.Funded.By.Investors)
glimpse(ld)
###############################----------------
ld=ld%>%
  select(-Employment.Length)
glimpse(ld)
###############################----------------------------------
apply(ld,2,function(x)sum(is.na(x)))

set.seed(1)
s=sample(1:nrow(ld),0.7*nrow(ld))
ld_train=ld[s,]
ld_test=ld[-s,]

######################################----------------------
m1=lm(Interest.Rate~.,data = ld_train)
######################################------------------------
library(car)
vif(m1)
#####################################---------------------------

m1=lm(Interest.Rate~. -rent ,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl -Revolving.CREDIT.Balance,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl -Revolving.CREDIT.Balance -ca,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl -Revolving.CREDIT.Balance -ca -ny,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl -Revolving.CREDIT.Balance -ca -ny -Debt.To.Income.Ratio,data = ld_train)
summary(m1)

m1=lm(Interest.Rate~. -rent -fl -Revolving.CREDIT.Balance -ca -ny -Debt.To.Income.Ratio -mort,data = ld_train)
summary(m1)
###############################################################################################################
library(ggplot2)
ld_train %>%
  mutate(pred_IR=predict(m1,newdata=ld_train)) %>%
  ggplot(aes(x=Interest.Rate,y=pred_IR))+geom_point(alpha=0.6)
#########################################################################################
m2=lm(Interest.Rate~.,data = ld_test)
vif(m2)
summary(m2)
##########
m2=lm(Interest.Rate~.-ca,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny -Revolving.CREDIT.Balance ,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny -Revolving.CREDIT.Balance -Debt.To.Income.Ratio ,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny -Revolving.CREDIT.Balance -Debt.To.Income.Ratio -fl,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny -Revolving.CREDIT.Balance -Debt.To.Income.Ratio -fl -lp_dc,data = ld_test)
summary(m2)

m2=lm(Interest.Rate~.-ca -Open.CREDIT.Lines -rent -Monthly.Income -il -ny -Revolving.CREDIT.Balance -Debt.To.Income.Ratio -fl -lp_dc -lp_cc,data = ld_test)
summary(m2)

#####################
library(ggplot2)
ld_test %>%
  mutate(pred_IR=predict(m2,newdata=ld_test)) %>%
  ggplot(aes(x=Interest.Rate,y=pred_IR))+geom_point(alpha=0.6)
################################################
train_vars=names(m1$coefficients)

test_vars=names(m2$coefficients)

train_vars[train_vars %in% test_vars][-1]
###############
##test_vars[test_vars %in% train_vars][-1]
##test_vars[train_vars %in% test_vars][-1]
paste(train_vars[train_vars %in% test_vars][-1],collapse="+")

final=lm(Interest.Rate~ Amount.Requested+Inquiries.in.the.Last.6.Months+ll_36+tx+fico,data = ld_train)
summary(final)



###################################
plot(final,which=1)
plot(final,which=2)
plot(final,which=3)
plot(final,which=4)

#############Rmse for train###########
mean((ld_train$Interest.Rate-predict(final,newdata=ld_train))**2)%>%
  sqrt()

#############Rmse for test###########
mean((ld_test$Interest.Rate-predict(final,newdata=ld_test))**2) %>%
  sqrt()

# normal scatter plot
ld_temp=ld_train[ld_train$Monthly.Income<25000,]
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point()+ggtitle("Normal Scatter Plot")

###Scatter Plot with density mapped
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point(alpha=0.4,size=5)

#scatter plot with hex binning or 2d binning , you'll need to install package "hexbin" for the same
library(hexbin)
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_binhex()
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_bin2d()
