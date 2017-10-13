
## ---- Setting Working Directory and importing Data ----------------------------------------
# Remember you need to set working directory to the folder which contains this data
ld=read.csv("loans data.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(ld)


## ---- Converting character to numbers -----------------------------------------

ld=ld %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )
glimpse(ld)



## ------- Removing column which will not be available at this point of the process -----------------------------------------------------------------

ld = ld %>%
  select(-Amount.Funded.By.Investors)


## -------- Converting FICO range into a Mean Score to get better utilisation ----------------------------------------------------------------

ld= ld %>%
  mutate(f1=as.numeric(substr(FICO.Range,1,3)),
         f2=as.numeric(substr(FICO.Range,5,7)),
         fico=0.5*(f1+f2)
  ) %>%
  select(-FICO.Range,-f1,-f2)
glimpse(ld)


## ----Converting Work years into numerical variable -------------------------------------------------------
ld=ld %>%
  mutate(el=ifelse(substr(Employment.Length,1,2)=="10",10,Employment.Length),
         el=ifelse(substr(Employment.Length,1,1)=="<",0,el),
         el=gsub("years","",el),
         el=gsub("year","",el),
         el=as.numeric(el)
  ) %>%
  select(-Employment.Length) %>%
  na.omit()

glimpse(ld)

## ----------Converting Home Ownership into Dummy Variable --------------------------------------------------------------
table(ld$Home.Ownership)


ld=ld %>%
  mutate(HW_RENT=as.numeric(Home.Ownership=="RENT"),
         HW_MORT=as.numeric(Home.Ownership=="MORTGAGE"),
         HW_OWN=as.numeric(Home.Ownership=="OWN")) %>%
  select(-Home.Ownership)

glimpse(ld)

## ------- Converting Loan Purpose into Dummy Variables -----------------------------------------------------------------
## ------- Will only consider 3 categories to dummify ---------------------------------

table(ld$Loan.Purpose)

ld=ld %>%
  mutate(LP_cc=as.numeric(Loan.Purpose=="credit_card"),
         LP_dc=as.numeric(Loan.Purpose=="debt_consolidation"),
         LP_other=as.numeric(Loan.Purpose=="other")
  ) %>%
  select(-Loan.Purpose)

glimpse(ld)

## ------- Converting Loan Length into dummy variables ---------------------------------------------------------------
## ------- Also removing State from the dataframe ---------------------------------
table(ld$Loan.Length)

ld=ld %>%
  mutate(LL_36=as.numeric(Loan.Length=="36 months")) %>%
  select(-Loan.Length)

table(ld$State)

ld= ld %>%
  select(-State)

## --------- Breaking data into Training and Test datasets ---------------------------------------------------------------

set.seed(2)
s=sample(1:nrow(ld),0.7*nrow(ld))
ld_train=ld[s,]
ld_test=ld[-s,]

glimpse(ld_train)

## ----- Training the linear regression model on train data and storing it in fit -------------------------------------------------------------

fit=lm(Interest.Rate~. -ID,data=ld_train)

## ---- Checking VIF values of variables ----------------------------------------
library(car)
vif(fit)

## ------Removing HW_MORT and checking the VIF values again ------------------------------------------------------------------
fit=lm(Interest.Rate~. -ID - HW_MORT,data=ld_train)
vif(fit)

## ------ Checking summary for p values of variables ------------------------------------------------------------------
summary(fit)


## ------ Using step function to remove variables with high AIC values ------------------------------------------------------------------
fit = step(fit)

## ------ Checking summary for p values of variables ------------------------------------------------------------------
summary(fit)

## ------ Using formula function to get all the variables of the model ------------------------------------------------------------------
formula(fit)

## ------ Running linear regression agian on filtered variables (removed Monthly Income) ------------------------------------------------------------------
fit = lm(Interest.Rate ~ Amount.Requested + Open.CREDIT.Lines + 
           Revolving.CREDIT.Balance + Inquiries.in.the.Last.6.Months + 
           fico + HW_RENT + HW_OWN + LP_cc + LP_dc + LL_36, data = ld_train)

## ------ Checking summary for Estimates of variables ------------------------------------------------------------------
summary(fit)

## ------ Checking plot between the Fitted values and Residuals ------------------------------------------------------------
plot(fit, which = 1)

## ------ Checking plot between the FICO and Interest Rate ------------------------------------------------------------
library(ggplot2)
ggplot(ld_train, aes(x = fico, y = Interest.Rate)) + geom_smooth()

## ------ Checking plot for Mormality of the Variables ------------------------------------------------------------
plot(fit, which = 2)

## ------- Checking Density plots in comparison with Standard Normal Deviation -----------------------------------------------------------------
df=data.frame(res=fit$residual)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm, args=list(mean=mean(df$res), sd=sd(df$res)),color="green")

## ------ Checking plot for Variance of Errors ------------------------------------------------------------
plot(fit, which = 3)

## ------ Checking plot for Leverage points with Cook's distance ------------------------------------------------------------
plot(fit, which = 4)

## ------ Checking RMSE for the prediction with the model ------------------------------------------------------------
mean((ld_test$Interest.Rate-predict(fit,newdata=ld_test))**2) %>%
sqrt()

