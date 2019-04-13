#setting workspace environment
library(PerformanceAnalytics)
library(ggplot2)

path <- c("C:/Users/cuddly/Desktop/Credit_Risk_Kaggle")
setwd(paste0(path,"/02_Data"))

#Reference Link for EDA : https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/

#imporitn data into R
app_dt <- read.csv("application_train.csv",as.is = T)

names(app_dt)

#Variable 1
#Checking for duplicates in SK_ID_CURR
sum(duplicated(app_dt$SK_ID_CURR))
#Checking for NA
sum(is.na(app_dt$SK_ID_CURR))

#Variable 2
#Checking for duplicates in TARGET
table(app_dt$TARGET)
#Checking for NA
sum(is.na(app_dt$TARGET))

#Variable 3
#Reference Links
#Cash Loans : https://cashloans.cashconverters.com.au/cash-loans
#Revolving loans : http://www.businessdictionary.com/definition/revolving-loan.html

#Checking for duplicates in NAME_CONTRACT_TYPE
table(app_dt$NAME_CONTRACT_TYPE)
#Checking for NA
sum(is.na(app_dt$NAME_CONTRACT_TYPE))

table(app_dt$TARGET,app_dt$NAME_CONTRACT_TYPE)

#Variable 4
#Checking for duplicates in CODE_GENDER
table(app_dt$CODE_GENDER)
#Checking for NA
sum(is.na(app_dt$CODE_GENDER))
#Removing XNA from the data
# ind <- which(app_dt$CODE_GENDER=="XNA")
# app_dt <- app_dt[-ind,]

table(app_dt$NAME_CONTRACT_TYPE,app_dt$CODE_GENDER, app_dt$TARGET)

#Variable 5
#Checking for duplicates in FLAG_OWN_CAR
table(app_dt$FLAG_OWN_CAR)
#Checking for NA
sum(is.na(app_dt$FLAG_OWN_CAR))

table(app_dt$TARGET,app_dt$FLAG_OWN_CAR)

#Variable 6
#Checking for duplicates in FLAG_OWN_REALTY
table(app_dt$FLAG_OWN_REALTY)
#Checking for NA
sum(is.na(app_dt$FLAG_OWN_REALTY))

table(app_dt$TARGET,app_dt$FLAG_OWN_REALTY)

#Variable 7
#Reference link : http://www.pewsocialtrends.org/2015/05/07/family-size-among-mothers/

#Checking for duplicates in CNT_CHILDREN
table(app_dt$CNT_CHILDREN)

#Children count categorization
ind <- which(app_dt$CNT_CHILDREN >= 4)
app_dt$CNT_CHILDREN[ind] <- "4+"
#Converting o a factor
app_dt$CNT_CHILDREN <- as.factor(app_dt$CNT_CHILDREN)

#Checking for NA
sum(is.na(app_dt$CNT_CHILDREN))

table(app_dt$CNT_CHILDREN)
table(app_dt$TARGET,app_dt$CNT_CHILDREN)

#Variable 8
#Reference Link : Outlier detection :http://colingorrie.github.io/outlier-detection.html

#Checking for duplicates in AMT_INCOME_TOTAL
summary(app_dt$AMT_INCOME_TOTAL)
#Checking for NA
sum(is.na(app_dt$AMT_INCOME_TOTAL))

#Checking income for skewness
skewness(app_dt$AMT_INCOME_TOTAL)
datasim <- data.frame(app_dt$AMT_INCOME_TOTAL)
ggplot(datasim, aes(x = app_dt$AMT_INCOME_TOTAL), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Income'))) + 
  ylab(expression(bold('Density')))

#Applying log transformation
app_dt$AMT_INCOME_TOTAL_TRANSFORMED <- log(app_dt$AMT_INCOME_TOTAL)
skewness(app_dt$AMT_INCOME_TOTAL_TRANSFORMED)
datasim <- data.frame(app_dt$AMT_INCOME_TOTAL_TRANSFORMED)
ggplot(datasim, aes(x = app_dt$AMT_INCOME_TOTAL_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Income'))) + 
  ylab(expression(bold('Density')))

#Outlier Detection
Q1 <- summary(app_dt$AMT_INCOME_TOTAL_TRANSFORMED)[2]
Q3 <- summary(app_dt$AMT_INCOME_TOTAL_TRANSFORMED)[5]
IQR <- Q3-Q1

#hard outliers 
hard_left <- Q1-(3*IQR)
hard_right <- Q3+(3*IQR)
ind <- which(app_dt$AMT_INCOME_TOTAL_TRANSFORMED > hard_right)
skewness(app_dt$AMT_INCOME_TOTAL_TRANSFORMED[-ind])

#Variable 9
#Checking for duplicates in AMT_CREDIT
summary(app_dt$AMT_CREDIT)
#Checking for NA
sum(is.na(app_dt$AMT_CREDIT))

#Checking for skewness
skewness(app_dt$AMT_CREDIT)
datasim <- data.frame(app_dt$AMT_CREDIT)
ggplot(datasim, aes(x = app_dt$AMT_CREDIT), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Credit/Loan'))) + 
  ylab(expression(bold('Density')))

#Applying log transformation
app_dt$AMT_CREDIT_TRANSFORMED <- log(app_dt$AMT_CREDIT)
skewness(app_dt$AMT_CREDIT_TRANSFORMED)
datasim <- data.frame(app_dt$AMT_CREDIT_TRANSFORMED)
ggplot(datasim, aes(x = app_dt$AMT_CREDIT_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Credit/Loan'))) + 
  ylab(expression(bold('Density')))

#Variable 10
#Checking for duplicates in AMT_ANNUITY
summary(app_dt$AMT_ANNUITY)
#Checking for NA
sum(is.na(app_dt$AMT_ANNUITY))

#Checking NAs
ind <- which(is.na(app_dt$AMT_ANNUITY))
sub1 <- app_dt[ind,]

#Checking skewness
skewness(app_dt$AMT_ANNUITY)
#Applying log transformation
app_dt$AMT_ANNUITY_TRANSFORMED <- log(app_dt$AMT_ANNUITY)
skewness(app_dt$AMT_ANNUITY_TRANSFORMED)
datasim <- data.frame(app_dt$AMT_ANNUITY_TRANSFORMED)
ggplot(datasim, aes(x = app_dt$AMT_ANNUITY_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Annuity'))) + 
  ylab(expression(bold('Density')))


#Variable 11
#Checking for duplicates in AMT_GOODS_PRICE
#Refrence Link : https://www.bankbazaar.com/car-loan/loan-to-value-ratio-for-car-loan.html?ck=Y%2BziX71XnZjIM9ZwEflsyO92kDgzqhG%2FepZDgnQl2WeZC0TqFEkhF6ljN6nbC1ZXXBwImSfaF7%2BS%0AmJOG7qYyyg%3D%3D&rc=1
#Refrence Lnk : https://www.thebalance.com/loan-to-value-ratio-315629

summary(app_dt$AMT_GOODS_PRICE)
#Checking for NA
View(app_dt[which(is.na(app_dt$AMT_GOODS_PRICE)),])
unique(app_dt[which(is.na(app_dt$AMT_GOODS_PRICE)),c("NAME_CONTRACT_TYPE")])
#All NA's corresponds to revolving loans only

#Checking skewness
skewness(app_dt$AMT_GOODS_PRICE)
#Applying log transformation
app_dt$AMT_GOODS_PRICE_TRANSFORMED <- log(app_dt$AMT_GOODS_PRICE)
skewness(app_dt$AMT_GOODS_PRICE_TRANSFORMED)
datasim <- data.frame(app_dt$AMT_GOODS_PRICE_TRANSFORMED)
ggplot(datasim, aes(x = app_dt$AMT_GOODS_PRICE_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Goods Price'))) + 
  ylab(expression(bold('Density')))

#Checking cases if loan amount is more than goods price
View(app_dt[which(app_dt$AMT_CREDIT > app_dt$AMT_GOODS_PRICE),])
#Checking cases if loan amount is less than goods price
View(app_dt[which(app_dt$AMT_CREDIT < app_dt$AMT_GOODS_PRICE),])

#Calculating Loan to Value 
#Using original variable
app_dt$LTV <- (app_dt$AMT_CREDIT/app_dt$AMT_GOODS_PRICE)*100
summary(app_dt$LTV)
#Using transformed variables
app_dt$LTV_TRANSFORMED <- (app_dt$AMT_CREDIT_TRANSFORMED/app_dt$AMT_GOODS_PRICE_TRANSFORMED)*100
summary(app_dt$LTV_TRANSFORMED)

#Variable 12
table(app_dt$NAME_TYPE_SUITE)
sum(is.na(app_dt$NAME_TYPE_SUITE))

#Variable 13,41 and 29
#Checking for duplicates in NAME_INCOME_TYPE
table(app_dt$OCCUPATION_TYPE)
sum(is.na(app_dt$NAME_INCOME_TYPE))
sum(is.na(app_dt$ORGANIZATION_TYPE))
sum(is.na(app_dt$OCCUPATION_TYPE))

#getting all occupations combination
a <- app_dt[,c(13,41,29)]
ind <- which(duplicated(a))
a <- a[-ind,]
write.csv(a,paste0(path,"/04_Interim/Occupation_Income_Organization_Mapping.csv"), row.names=F)

#Creating occupational rating variable from occupation_type
app_dt$OCCUPATIONAL_RATING <- ifelse(app_dt$OCCUPATION_TYPE %in% c('Accountants','Core staff','High skill tech staff','HR staff','IT staff','Managers','Medicine staff','Realty agents','Sales staff','Secretaries'),'White collar',
                                     ifelse(app_dt$OCCUPATION_TYPE %in% c('Low-skill Laborers'),'Non-insurable',ifelse(app_dt$OCCUPATION_TYPE %in% c('Drivers','Private service staff'),'Manual worker', ifelse(app_dt$OCCUPATION_TYPE %in% c('Laborers','Security staff'),'High risk',ifelse(app_dt$OCCUPATION_TYPE %in% c('Cleaning staff','Cooking staff','Waiters/barmen staff'),'Heavy manual worker', NA)))))

table(app_dt$OCCUPATIONAL_RATING)
sum(is.na(app_dt$OCCUPATIONAL_RATING))

#Variable 14,15,16
t <- table(app_dt$NAME_HOUSING_TYPE)
prop.table(t)
t <- table(app_dt$NAME_EDUCATION_TYPE,app_dt$OCCUPATIONAL_RATING)
View(t)

#Variable 17
unique(app_dt$REGION_POPULATION_RELATIVE)
skewness(app_dt$REGION_POPULATION_RELATIVE)
datasim <- data.frame(app_dt$REGION_POPULATION_RELATIVE)
ggplot(datasim, aes(x = app_dt$REGION_POPULATION_RELATIVE), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Goods Price'))) + 
  ylab(expression(bold('Density')))

ind <- which(app_dt$REGION_POPULATION_RELATIVE>0.04)
unique(app_dt[ind,"REGION_POPULATION_RELATIVE"])

#Variable 18
summary(app_dt$DAYS_BIRTH)
#Converting age from days to years
app_dt$DAYS_BIRTH_YEARS <- round(abs(app_dt$DAYS_BIRTH*0.00273973))
t <- table(app_dt$DAYS_BIRTH_YEARS,app_dt$TARGET)
prop.table(t,margin = 2)*100

datasim <- data.frame(app_dt$DAYS_BIRTH_YEARS)
ggplot(datasim, aes(x = app_dt$DAYS_BIRTH_YEARS), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Goods Price'))) + 
  ylab(expression(bold('Density')))

#Grouping age into groups
app_dt$AGE_GROUP <- ifelse(app_dt$DAYS_BIRTH_YEARS >= 21 &  app_dt$DAYS_BIRTH_YEARS <= 25,"21-25",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 26 &  app_dt$DAYS_BIRTH_YEARS <= 30,"26-30",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 31 &  app_dt$DAYS_BIRTH_YEARS <= 35,"31-35",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 36 &  app_dt$DAYS_BIRTH_YEARS <= 40,"36-40",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 41 &  app_dt$DAYS_BIRTH_YEARS <= 45,"41-45",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 46 &  app_dt$DAYS_BIRTH_YEARS <= 50,"46-50",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 51 &  app_dt$DAYS_BIRTH_YEARS <= 55,"51-55",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 56 &  app_dt$DAYS_BIRTH_YEARS <= 60,"56-60",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 61 &  app_dt$DAYS_BIRTH_YEARS <= 65,"61-65",
                    ifelse(app_dt$DAYS_BIRTH_YEARS >= 66 &  app_dt$DAYS_BIRTH_YEARS <= 70,"66-70",NA
                                  ))))))))))
app_dt$AGE_GROUP <- as.factor(app_dt$AGE_GROUP)
table(app_dt$AGE_GROUP)

#Variable 19
summary(app_dt$DAYS_EMPLOYED)
#Converting age from days to years
app_dt$DAYS_EMPLOYED_YEARS <- round(abs(app_dt$DAYS_EMPLOYED*0.00273973))
summary(app_dt$DAYS_EMPLOYED_YEARS)

t <- table(app_dt$DAYS_EMPLOYED_YEARS)
ind <- which(app_dt$DAYS_EMPLOYED_YEARS == 1001)
app_dt$DAYS_EMPLOYED_YEARS[ind] <- NA

t <- table(app_dt$DAYS_EMPLOYED_YEARS,app_dt$TARGET)
prop.table(t,margin = 2)*100

#Classifying RIsk bawsed on years of employment

app_dt$YEARS_EMPLOYMENT_RISK_CLASS <- 
  ifelse(app_dt$DAYS_EMPLOYED_YEARS >= 0 &  app_dt$DAYS_EMPLOYED_YEARS <= 3,"High Risk",
  ifelse(app_dt$DAYS_EMPLOYED_YEARS >= 4 &  app_dt$DAYS_EMPLOYED_YEARS <= 8,"Medium Risk",
  ifelse(app_dt$DAYS_EMPLOYED_YEARS >= 9 &  app_dt$DAYS_EMPLOYED_YEARS <= 14,"Light Risk",
  ifelse(app_dt$DAYS_EMPLOYED_YEARS >= 15 ,"Very Light Risk",NA))))
                                                
table(app_dt$YEARS_EMPLOYMENT_RISK_CLASS)
sum(is.na(app_dt$YEARS_EMPLOYMENT_RISK_CLASS))

#Variable 20
summary(app_dt$DAYS_REGISTRATION)
#Converting age from days to years
app_dt$DAYS_REGISTRATION_YEARS <- round(abs(app_dt$DAYS_REGISTRATION*0.00273973))
summary(app_dt$DAYS_REGISTRATION_YEARS)

t <- table(app_dt$DAYS_REGISTRATION_YEARS,app_dt$TARGET)
prop.table(t,margin = 2)*100

#Classifying Risk based on years in chnage in registration
app_dt$DAYS_REGISTRATION_RISK_CLASS <- 
  ifelse(app_dt$DAYS_REGISTRATION_YEARS >= 0 &  app_dt$DAYS_REGISTRATION_YEARS <= 3,"High Risk",
  ifelse(app_dt$DAYS_REGISTRATION_YEARS >= 4 &  app_dt$DAYS_REGISTRATION_YEARS <= 15,"Medium Risk",
  ifelse(app_dt$DAYS_REGISTRATION_YEARS >= 16 &  app_dt$DAYS_REGISTRATION_YEARS <= 34,"Light Risk",
  ifelse(app_dt$DAYS_REGISTRATION_YEARS >= 35 ,"Very Light Risk",NA))))
table(app_dt$DAYS_REGISTRATION_RISK_CLASS)

#Variable 21
summary(app_dt$DAYS_ID_PUBLISH)
#Converting age from days to years
app_dt$DAYS_ID_PUBLISH_YEARS <- round(abs(app_dt$DAYS_ID_PUBLISH*0.00273973))
summary(app_dt$DAYS_ID_PUBLISH_YEARS)

View(table(app_dt$DAYS_ID_PUBLISH_YEARS))
t <- table(app_dt$DAYS_ID_PUBLISH_YEARS,app_dt$TARGET)
prop.table(t,margin = 1)*100

#Classifying Risk based on years in chnage in registration
app_dt$DAYS_ID_PUBLISH_RISK_CLASS <- 
  ifelse(app_dt$DAYS_ID_PUBLISH_YEARS >= 0 &  app_dt$DAYS_ID_PUBLISH_YEARS <= 3,"High Risk",
  ifelse(app_dt$DAYS_ID_PUBLISH_YEARS >= 4 &  app_dt$DAYS_ID_PUBLISH_YEARS <= 11,"Medium Risk",
  ifelse(app_dt$DAYS_ID_PUBLISH_YEARS >= 12 &  app_dt$DAYS_ID_PUBLISH_YEARS <= 20,"Light Risk",NA)))

table(app_dt$DAYS_ID_PUBLISH_RISK_CLASS)

#Variable 29
t <- table(app_dt$CNT_FAM_MEMBERS, app_dt$TARGET)
prop.table(t,margin = 2)*100
sum(is.na(app_dt$CNT_FAM_MEMBERS))
#Classifying Risk based on years in chnage in registration
ind <- which(app_dt$CNT_FAM_MEMBERS >= 4)
app_dt$CNT_FAM_MEMBERS[ind] <- "4+"
app_dt$CNT_FAM_MEMBERS <- as.factor(app_dt$CNT_FAM_MEMBERS)
table(app_dt$CNT_FAM_MEMBERS)

#Variable 30-39
#creating 2 flags for region and city if there is any discrepancies in the communicated address
app_dt$REG_ADDRESS_DICRIPANCY <- ifelse (app_dt$REG_REGION_NOT_LIVE_REGION==1 | app_dt$REG_REGION_NOT_WORK_REGION == 1 | app_dt$LIVE_REGION_NOT_WORK_REGION == 1, 1, 0)
table(app_dt$REG_ADDRESS_DICRIPANCY)
t <- table(app_dt$REG_ADDRESS_DICRIPANCY,app_dt$TARGET)
prop.table(t,margin = 2)

app_dt$CITY_ADDRESS_DICRIPANCY <- ifelse (app_dt$REG_CITY_NOT_LIVE_CITY==1 | app_dt$REG_CITY_NOT_WORK_CITY == 1 | app_dt$LIVE_CITY_NOT_WORK_CITY == 1, 1, 0)
table(app_dt$CITY_ADDRESS_DICRIPANCY)

#Variable 40
table(app_dt$AMT_REQ_CREDIT_BUREAU_HOUR)
summary(app_dt$AMT_REQ_CREDIT_BUREAU_YEAR)
sum(is.na(app_dt$AMT_REQ_CREDIT_BUREAU_YEAR))/307511

#Variable 96
#Converting age from days to years
app_dt$DAYS_LAST_PHONE_CHANGE_YEARS <- round(abs(app_dt$DAYS_LAST_PHONE_CHANGE*0.00273973))
summary(app_dt$DAYS_LAST_PHONE_CHANGE_YEARS)

t <- table(app_dt$DAYS_LAST_PHONE_CHANGE_YEARS,app_dt$TARGET)
prop.table(t,margin = 2)*100

app_dt$DAYS_LAST_PHONE_CHANGE_CLASS <- 
  ifelse(app_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 0 &  app_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 2,"High Risk",
  ifelse(app_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 3 &  app_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 5,"Medium Risk",
  ifelse(app_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 6 &  app_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 12,"Light Risk",NA)))

table(app_dt$DAYS_LAST_PHONE_CHANGE_CLASS)
