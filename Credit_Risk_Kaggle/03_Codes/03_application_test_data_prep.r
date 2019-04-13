##setting workspace environment-------------------------------

library(PerformanceAnalytics)
library(ggplot2)

path <- c("C:/Users/cuddly/Desktop/Credit_Risk_Kaggle")
setwd(paste0(path,"/02_Data"))

#Reference Link for EDA : https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/

##imporitn data into R-------------------------------

app_test_dt <- read.csv("application_test.csv",as.is = T)

##Removing irrelevant columns-------------------------------

col <- c("NAME_TYPE_SUITE","NAME_INCOME_TYPE","ORGANIZATION_TYPE","OWN_CAR_AGE","FLAG_MOBIL","FLAG_EMP_PHONE","FLAG_WORK_PHONE","FLAG_CONT_MOBILE","FLAG_PHONE","FLAG_EMAIL","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY","WEEKDAY_APPR_PROCESS_START","HOUR_APPR_PROCESS_START","APARTMENTS_AVG","BASEMENTAREA_AVG","YEARS_BEGINEXPLUATATION_AVG","YEARS_BUILD_AVG","COMMONAREA_AVG","ELEVATORS_AVG","ENTRANCES_AVG","FLOORSMAX_AVG","FLOORSMIN_AVG","LANDAREA_AVG","LIVINGAPARTMENTS_AVG","LIVINGAREA_AVG","NONLIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG","APARTMENTS_MODE","BASEMENTAREA_MODE","YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE","COMMONAREA_MODE","ELEVATORS_MODE","ENTRANCES_MODE","FLOORSMAX_MODE","FLOORSMIN_MODE","LANDAREA_MODE","LIVINGAPARTMENTS_MODE","LIVINGAREA_MODE","NONLIVINGAPARTMENTS_MODE","NONLIVINGAREA_MODE","APARTMENTS_MEDI","BASEMENTAREA_MEDI","YEARS_BEGINEXPLUATATION_MEDI","YEARS_BUILD_MEDI","COMMONAREA_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI","FLOORSMAX_MEDI","FLOORSMIN_MEDI","LANDAREA_MEDI","LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI","NONLIVINGAPARTMENTS_MEDI","NONLIVINGAREA_MEDI","FONDKAPREMONT_MODE","HOUSETYPE_MODE","TOTALAREA_MODE","WALLSMATERIAL_MODE","EMERGENCYSTATE_MODE","FLAG_DOCUMENT_2","FLAG_DOCUMENT_3","FLAG_DOCUMENT_4","FLAG_DOCUMENT_5","FLAG_DOCUMENT_6","FLAG_DOCUMENT_7","FLAG_DOCUMENT_8","FLAG_DOCUMENT_9","FLAG_DOCUMENT_10","FLAG_DOCUMENT_11","FLAG_DOCUMENT_12","FLAG_DOCUMENT_13","FLAG_DOCUMENT_14","FLAG_DOCUMENT_15","FLAG_DOCUMENT_16","FLAG_DOCUMENT_17","FLAG_DOCUMENT_18","FLAG_DOCUMENT_19","FLAG_DOCUMENT_20","FLAG_DOCUMENT_21","EXT_SOURCE_1")

app_test_dt <- app_test_dt[,!(names(app_test_dt) %in% col)]

##Variable CNT_CHILDREN-------------------------------

#Children count categorization
ind <- which(app_test_dt$CNT_CHILDREN >= 4)
app_test_dt$CNT_CHILDREN[ind] <- "4+"
#Converting o a factor
app_test_dt$CNT_CHILDREN <- as.factor(app_test_dt$CNT_CHILDREN)

##Variable AMT_INCOME_TOTAL-------------------------------

#Checking for skewness
skewness(app_test_dt$AMT_INCOME_TOTAL)
datasim <- data.frame(app_test_dt$AMT_INCOME_TOTAL)
ggplot(datasim, aes(x = app_test_dt$AMT_INCOME_TOTAL), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Income'))) + 
  ylab(expression(bold('Density')))

#Applying log transformation
app_test_dt$AMT_INCOME_TOTAL_TRANSFORMED <- log(app_test_dt$AMT_INCOME_TOTAL)
skewness(app_test_dt$AMT_INCOME_TOTAL_TRANSFORMED)
datasim <- data.frame(app_test_dt$AMT_INCOME_TOTAL_TRANSFORMED)
ggplot(datasim, aes(x = app_test_dt$AMT_INCOME_TOTAL_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Income'))) + 
  ylab(expression(bold('Density')))

##Variable AMT_CREDIT-------------------------------
#Checking for skewness
skewness(app_test_dt$AMT_CREDIT)
datasim <- data.frame(app_test_dt$AMT_CREDIT)
ggplot(datasim, aes(x = app_test_dt$AMT_CREDIT), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Credit/Loan'))) + 
  ylab(expression(bold('Density')))

#Applying log transformation
app_test_dt$AMT_CREDIT_TRANSFORMED <- log(app_test_dt$AMT_CREDIT)
skewness(app_test_dt$AMT_CREDIT_TRANSFORMED)
datasim <- data.frame(app_test_dt$AMT_CREDIT_TRANSFORMED)
ggplot(datasim, aes(x = app_test_dt$AMT_CREDIT_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Credit/Loan'))) + 
  ylab(expression(bold('Density')))

##Variable AMT_ANNUITY-------------------------------
#Checking skewness
skewness(app_test_dt$AMT_ANNUITY)
#Applying log transformation
app_test_dt$AMT_ANNUITY_TRANSFORMED <- log(app_test_dt$AMT_ANNUITY)
skewness(app_test_dt$AMT_ANNUITY_TRANSFORMED)
datasim <- data.frame(app_test_dt$AMT_ANNUITY_TRANSFORMED)
ggplot(datasim, aes(x = app_test_dt$AMT_ANNUITY_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Annuity'))) + 
  ylab(expression(bold('Density')))

##Variable AMT_GOODS_PRICE-------------------------------
#Checking skewness
skewness(app_test_dt$AMT_GOODS_PRICE)
#Applying log transformation
app_test_dt$AMT_GOODS_PRICE_TRANSFORMED <- log(app_test_dt$AMT_GOODS_PRICE)
skewness(app_test_dt$AMT_GOODS_PRICE_TRANSFORMED)
datasim <- data.frame(app_test_dt$AMT_GOODS_PRICE_TRANSFORMED)
ggplot(datasim, aes(x = app_test_dt$AMT_GOODS_PRICE_TRANSFORMED), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Goods Price'))) + 
  ylab(expression(bold('Density')))

##Creating new variable loan to value using transformed variable-------------------------------
app_test_dt$LTV_TRANSFORMED <- (app_test_dt$AMT_CREDIT_TRANSFORMED/app_test_dt$AMT_GOODS_PRICE_TRANSFORMED)*100
summary(app_test_dt$LTV_TRANSFORMED)
skewness(app_test_dt$LTV_TRANSFORMED)

##Creating occupational rating variable from occupation_type-------------------------------

app_test_dt$OCCUPATIONAL_RATING <- ifelse(app_test_dt$OCCUPATION_TYPE %in% c('Accountants','Core staff','High skill tech staff','HR staff','IT staff','Managers','Medicine staff','Realty agents','Sales staff','Secretaries'),'White collar',
                                     ifelse(app_test_dt$OCCUPATION_TYPE %in% c('Low-skill Laborers'),'Non-insurable',ifelse(app_test_dt$OCCUPATION_TYPE %in% c('Drivers','Private service staff'),'Manual worker', ifelse(app_test_dt$OCCUPATION_TYPE %in% c('Laborers','Security staff'),'High risk',ifelse(app_test_dt$OCCUPATION_TYPE %in% c('Cleaning staff','Cooking staff','Waiters/barmen staff'),'Heavy manual worker', NA)))))

##Variable DAYS_BIRTH_YEARS-------------------------------
summary(app_test_dt$DAYS_BIRTH)
#Converting age from days to years
app_test_dt$DAYS_BIRTH_YEARS <- round(abs(app_test_dt$DAYS_BIRTH*0.00273973))
#Grouping age into groups
app_test_dt$AGE_GROUP <- ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 21 &  app_test_dt$DAYS_BIRTH_YEARS <= 25,"21-25",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 26 &  app_test_dt$DAYS_BIRTH_YEARS <= 30,"26-30",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 31 &  app_test_dt$DAYS_BIRTH_YEARS <= 35,"31-35",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 36 &  app_test_dt$DAYS_BIRTH_YEARS <= 40,"36-40",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 41 &  app_test_dt$DAYS_BIRTH_YEARS <= 45,"41-45",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 46 &  app_test_dt$DAYS_BIRTH_YEARS <= 50,"46-50",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 51 &  app_test_dt$DAYS_BIRTH_YEARS <= 55,"51-55",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 56 &  app_test_dt$DAYS_BIRTH_YEARS <= 60,"56-60",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 61 &  app_test_dt$DAYS_BIRTH_YEARS <= 65,"61-65",
                    ifelse(app_test_dt$DAYS_BIRTH_YEARS >= 66 &  app_test_dt$DAYS_BIRTH_YEARS <= 70,"66-70",NA
                                  ))))))))))
app_test_dt$AGE_GROUP <- as.factor(app_test_dt$AGE_GROUP)
table(app_test_dt$AGE_GROUP)

##Variable DAYS_EMPLOYED-------------------------------

#Converting age from days to years
app_test_dt$DAYS_EMPLOYED_YEARS <- round(abs(app_test_dt$DAYS_EMPLOYED*0.00273973))
summary(app_test_dt$DAYS_EMPLOYED_YEARS)
table(app_test_dt$DAYS_EMPLOYED_YEARS)

ind <- which(app_test_dt$DAYS_EMPLOYED_YEARS == 1001)
app_test_dt$DAYS_EMPLOYED_YEARS[ind] <- NA

#Classifying RIsk bawsed on years of employment
app_test_dt$YEARS_EMPLOYMENT_RISK_CLASS <- 
  ifelse(app_test_dt$DAYS_EMPLOYED_YEARS >= 0 &  app_test_dt$DAYS_EMPLOYED_YEARS <= 3,"High Risk",
  ifelse(app_test_dt$DAYS_EMPLOYED_YEARS >= 4 &  app_test_dt$DAYS_EMPLOYED_YEARS <= 8,"Medium Risk",
  ifelse(app_test_dt$DAYS_EMPLOYED_YEARS >= 9 &  app_test_dt$DAYS_EMPLOYED_YEARS <= 14,"Light Risk",
  ifelse(app_test_dt$DAYS_EMPLOYED_YEARS >= 15 ,"Very Light Risk",NA))))
                                                
table(app_test_dt$YEARS_EMPLOYMENT_RISK_CLASS)
sum(is.na(app_test_dt$YEARS_EMPLOYMENT_RISK_CLASS))

##Variable DAYS_REGISTRATION-------------------------------

#Converting age from days to years
app_test_dt$DAYS_REGISTRATION_YEARS <- round(abs(app_test_dt$DAYS_REGISTRATION*0.00273973))
#Classifying Risk based on years in chnage in registration
app_test_dt$DAYS_REGISTRATION_RISK_CLASS <- 
  ifelse(app_test_dt$DAYS_REGISTRATION_YEARS >= 0 &  app_test_dt$DAYS_REGISTRATION_YEARS <= 3,"High Risk",
  ifelse(app_test_dt$DAYS_REGISTRATION_YEARS >= 4 &  app_test_dt$DAYS_REGISTRATION_YEARS <= 15,"Medium Risk",
  ifelse(app_test_dt$DAYS_REGISTRATION_YEARS >= 16 &  app_test_dt$DAYS_REGISTRATION_YEARS <= 34,"Light Risk",
  ifelse(app_test_dt$DAYS_REGISTRATION_YEARS >= 35 ,"Very Light Risk",NA))))
table(app_test_dt$DAYS_REGISTRATION_RISK_CLASS)

##Variable DAYS_ID_PUBLISH-------------------------------
#Converting age from days to years
app_test_dt$DAYS_ID_PUBLISH_YEARS <- round(abs(app_test_dt$DAYS_ID_PUBLISH*0.00273973))
#Classifying Risk based on years in chnage in registration
app_test_dt$DAYS_ID_PUBLISH_RISK_CLASS <- 
  ifelse(app_test_dt$DAYS_ID_PUBLISH_YEARS >= 0 &  app_test_dt$DAYS_ID_PUBLISH_YEARS <= 3,"High Risk",
  ifelse(app_test_dt$DAYS_ID_PUBLISH_YEARS >= 4 &  app_test_dt$DAYS_ID_PUBLISH_YEARS <= 11,"Medium Risk",
  ifelse(app_test_dt$DAYS_ID_PUBLISH_YEARS >= 12 &  app_test_dt$DAYS_ID_PUBLISH_YEARS <= 20,"Light Risk",NA)))

table(app_test_dt$DAYS_ID_PUBLISH_RISK_CLASS)

##Variable CNT_FAM_MEMBERS-------------------------------
#Classifying Risk based on years in chnage in registration
ind <- which(app_test_dt$CNT_FAM_MEMBERS >= 4)
app_test_dt$CNT_FAM_MEMBERS[ind] <- "4+"
app_test_dt$CNT_FAM_MEMBERS <- as.factor(app_test_dt$CNT_FAM_MEMBERS)
table(app_test_dt$CNT_FAM_MEMBERS)

##Variable for checking address discrepancies-------------------------------
#creating 2 flags for region and city if there is any discrepancies in the communicated address
app_test_dt$REG_ADDRESS_DICRIPANCY <- ifelse (app_test_dt$REG_REGION_NOT_LIVE_REGION==1 | app_test_dt$REG_REGION_NOT_WORK_REGION == 1 | app_test_dt$LIVE_REGION_NOT_WORK_REGION == 1, 1, 0)

app_test_dt$CITY_ADDRESS_DICRIPANCY <- ifelse (app_test_dt$REG_CITY_NOT_LIVE_CITY==1 | app_test_dt$REG_CITY_NOT_WORK_CITY == 1 | app_test_dt$LIVE_CITY_NOT_WORK_CITY == 1, 1, 0)

##Variable DAYS_LAST_PHONE_CHANGE_YEARS---------------------------
#Converting age from days to years
app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS <- round(abs(app_test_dt$DAYS_LAST_PHONE_CHANGE*0.00273973))

app_test_dt$DAYS_LAST_PHONE_CHANGE_CLASS <- 
  ifelse(app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 0 &  app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 2,"High Risk",
  ifelse(app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 3 &  app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 5,"Medium Risk",
  ifelse(app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS >= 6 &  app_test_dt$DAYS_LAST_PHONE_CHANGE_YEARS <= 12,"Light Risk",NA)))

table(app_test_dt$DAYS_LAST_PHONE_CHANGE_CLASS)



# Missing Value SUmmary ------------------------------------------------
missing_dt_summary <- data.frame()
nm <- names(app_test_dt)
for (i in 1: length(nm)){
  nm1 <- nm[i]
  temp <- data.frame(c(nm1),sum(is.na(app_test_dt[,i])))
  colnames(temp) <- c("Variable","NA_cnt") 
  missing_dt_summary <- rbind(missing_dt_summary,temp)
}


write.csv(app_test_dt,paste0(path,"/04_Interim/application_test_processed_V1.csv"), row.names=F)
