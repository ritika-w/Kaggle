
# 00 seeting workspace environment ----------------------------------------
path <- c("C:/Users/cuddly/Desktop/Credit_Risk_Kaggle")
setwd(paste0(path,"/02_Data"))

library(dplyr)
library(plyr)
library(reshape2)
library(reshape)
library(PerformanceAnalytics)

# 01 Importing data into R ------------------------------------------------
previous_application <- read.csv("previous_application.csv", as.is=T)
pos_cash_balance <- read.csv("POS_CASH_balance.csv", as.is=T)
installment_payments <- read.csv("installments_payments.csv", as.is=T)
credit_card_balance <- read.csv("credit_card_balance.csv", as.is=T)

# 02 previous_application --------------------------------------------------------
View(head(previous_application))

#filtering out data for application in last 2 years
ind <- which(previous_application$DAYS_DECISION >= -730)
previous_application <- previous_application[ind,]
rm(ind)

names(previous_application)
rownames(previous_application) <- 1:958838

ind <- which(previous_application$AMT_CREDIT == 0)
previous_application <- previous_application[-ind,]
rm(ind)

#contract type
table(previous_application$NAME_CONTRACT_TYPE)
summary(previous_application$AMT_DOWN_PAYMENT)

previous_application$temp <- paste(previous_application$SK_ID_CURR,previous_application$NAME_CONTRACT_STATUS,sep = "_")

temp <- previous_application %>%
        group_by(temp) %>%
        summarise(count=n())

temp$SK_ID_CURR <- sapply(strsplit(temp$temp, "_"),'[',1)
temp$NAME_CONTRACT_STATUS <- sapply(strsplit(temp$temp, "_"),'[',2)  

temp1 <- cast(temp, SK_ID_CURR ~ NAME_CONTRACT_STATUS, sum, value = 'count')

previous_application$temp <- paste(previous_application$SK_ID_CURR,previous_application$NAME_CONTRACT_TYPE,sep = "_")

temp <- previous_application %>%
  group_by(temp) %>%
  summarise(count=n())

temp$SK_ID_CURR <- sapply(strsplit(temp$temp, "_"),'[',1)
temp$NAME_CONTRACT_TYPE <- sapply(strsplit(temp$temp, "_"),'[',2)  

temp1 <- cast(temp, SK_ID_CURR ~ NAME_CONTRACT_TYPE, sum, value = 'count')

ind <- which(previous_application$DAYS_TERMINATION>0)
temp <- aggregate(AMT_ANNUITY~SK_ID_CURR, previous_application[ind,], sum)

temp <- aggregate(AMT_CREDIT~SK_ID_CURR, previous_application[ind,], sum)


#temp_previous <- temp1
temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)
temp_previous[which(is.na(temp_previous$AMT_ANNUITY)),9] <- 0

#new flag 1
previous_application$Flag_actual_credit_approved_credit_different <- ifelse(previous_application$AMT_APPLICATION - previous_application$AMT_CREDIT ==0, 1,0)

ind <- which(previous_application$DAYS_TERMINATION>0)
temp <- aggregate(Flag_actual_credit_approved_credit_different~SK_ID_CURR, previous_application[ind,], sum)

temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)

ind <- which(previous_application$AMT_DOWN_PAYMENT < 0 | is.na(previous_application$AMT_DOWN_PAYMENT))
previous_application$AMT_DOWN_PAYMENT[ind] <- 0
rm(ind)

#flag2 
previous_application$if_made_down_payment <- ifelse(previous_application$AMT_DOWN_PAYMENT > 0,1,0)
table(previous_application$if_made_down_payment)

ind <- which(previous_application$DAYS_TERMINATION>0)
temp <- aggregate(if_made_down_payment~SK_ID_CURR, previous_application[ind,], sum)

temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)


previous_application$total_credit_downpay <- previous_application$AMT_CREDIT+ previous_application$AMT_DOWN_PAYMENT

previous_application$AMT_CREDIT_PERCENT <- previous_application$AMT_CREDIT/previous_application$total_credit_downpay*100

previous_application$AMT_DOWN_PAYMENT_PERCENT <- previous_application$AMT_DOWN_PAYMENT/previous_application$total_credit_downpay*100

temp <- aggregate(AMT_DOWN_PAYMENT_PERCENT~SK_ID_CURR, previous_application[ind,], mean)

temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)

skewness(previous_application$AMT_GOODS_PRICE)
skewness(previous_application$AMT_CREDIT)

ind <- which(is.na(previous_application$AMT_GOODS_PRICE))
previous_application$AMT_GOODS_PRICE[ind] <- 0

previous_application$AMT_GOODS_PRICE_TRANSFORMED <- log(previous_application$AMT_GOODS_PRICE)
previous_application$AMT_CREDIT_TRANSFORMED <- log(previous_application$AMT_CREDIT)

skewness(previous_application$AMT_GOODS_PRICE_TRANSFORMED)
skewness(previous_application$AMT_CREDIT_TRANSFORMED)

previous_application$LTV_TRANSFORMED <- (previous_application$AMT_CREDIT_TRANSFORMED/previous_application$AMT_GOODS_PRICE_TRANSFORMED)*100

summary(previous_application$LTV_TRANSFORMED)

temp <- aggregate(LTV_TRANSFORMED~SK_ID_CURR, previous_application[ind,], mean)

temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)

previous_application$temp <- paste(previous_application$SK_ID_CURR,previous_application$NAME_CLIENT_TYPE,sep = "_")


temp <- previous_application %>%
  group_by(temp) %>%
  summarise(count=n())

temp$SK_ID_CURR <- sapply(strsplit(temp$temp, "_"),'[',1)
temp$NAME_CLIENT_TYPE <- sapply(strsplit(temp$temp, "_"),'[',2)  

temp1 <- cast(temp, SK_ID_CURR ~ NAME_CLIENT_TYPE, sum, value = 'count')

temp_previous <- merge(temp_previous, temp1, by = "SK_ID_CURR", all.x= TRUE)


previous_application$temp <- paste(previous_application$SK_ID_CURR,previous_application$NAME_YIELD_GROUP,sep = "_")


temp <- previous_application %>%
  group_by(temp) %>%
  summarise(count=n())

temp$SK_ID_CURR <- sapply(strsplit(temp$temp, "_"),'[',1)
temp$NAME_YIELD_GROUP <- sapply(strsplit(temp$temp, "_"),'[',2)  

temp1 <- cast(temp, SK_ID_CURR ~ NAME_YIELD_GROUP, sum, value = 'count')

temp_previous <- merge(temp_previous, temp1, by = "SK_ID_CURR", all.x= TRUE)

temp <- aggregate(NFLAG_INSURED_ON_APPROVAL~SK_ID_CURR, previous_application[ind,], sum)

temp_previous <- merge(temp_previous, temp, by = "SK_ID_CURR", all.x= TRUE)

write.csv(temp_previous,paste0(path,"/04_Interim/previous_applocation_processed_V1.csv"), row.names=F)


# 03 pos_cash_balance -----------------------------------------------------
View(head(pos_cash_balance))
sk_id_prev <- as.vector(unique(previous_application$SK_ID_PREV))
pos_cash_balance <- pos_cash_balance[ind,]
rm(ind)
rm(sk_id_prev)

ind <- which(pos_cash_balance$MONTHS_BALANCE == -1)

pos_cash_balance <- pos_cash_balance[order(pos_cash_balance$SK_ID_PREV,-pos_cash_balance$MONTHS_BALANCE), ]

temp <- ddply(pos_cash_balance,.(SK_ID_PREV),function(x) head(x,1))
temp$cnt_future_installment <- temp$CNT_INSTALMENT_FUTURE+temp$MONTHS_BALANCE+1

ind <- which(temp$cnt_future_installment <= 0)
temp$cnt_future_installment[ind] <- 0

temp1 <- aggregate(cnt_future_installment~SK_ID_CURR, temp, sum)
#temp_pos <- temp1

temp$temp1 <- paste(temp$SK_ID_CURR,temp$NAME_CONTRACT_STATUS,sep = "_")

temp2 <- temp %>%
  group_by(temp1) %>%
  summarise(count=n())

temp2$SK_ID_CURR <- sapply(strsplit(temp2$temp1, "_"),'[',1)
temp2$NAME_CONTRACT_STATUS <- sapply(strsplit(temp2$temp1, "_"),'[',2)  

temp2 <- cast(temp2, SK_ID_CURR ~ NAME_CONTRACT_STATUS, sum, value = 'count')
temp2$others <- temp2$Canceled+temp2$Approved+temp2$Demand+temp2$`Returned to the store`+temp2$Signed


temp_pos <- merge(temp_pos, temp2[,c(1,2,4,9)], by = "SK_ID_CURR", all.x= TRUE)

ind <- which(pos_cash_balance$SK_DPD > 14)
temp11 <- pos_cash_balance[ind,]

temp112 <- ddply(temp11,.(SK_ID_CURR),function(x) nrow(x))

temp_pos <- merge(temp_pos, temp112, by = "SK_ID_CURR", all.x= TRUE)
colnames(temp_pos)[6] <- "Number_of_times_due_past_date"
is.na(temp_pos$Number_of_times_due_past_date) <- 0

write.csv(temp_pos,paste0(path,"/04_Interim/pos_cash_balances_processed_V1.csv"), row.names=F)


# 04 installment_payments -----------------------------------------------------
View(head(installment_payments))
ind <- which(installment_payments$SK_ID_PREV %in% sk_id_prev)
installment_payments <- installment_payments[ind,]
rm(ind)

installment_payments$PAST_DUE_DAYS <- ifelse(installment_payments$DAYS_INSTALMENT < installment_payments$DAYS_ENTRY_PAYMENT,1,0)

installment_payments$REMANING_CREDIT <- installment_payments$AMT_INSTALMENT - installment_payments$AMT_PAYMENT

temp1 <- aggregate(PAST_DUE_DAYS~SK_ID_CURR, installment_payments, sum)
temp_installments <- temp1

temp1 <- aggregate(REMANING_CREDIT~SK_ID_CURR, installment_payments, sum)
ind <- which(temp1$REMANING_CREDIT < 0)
temp1$REMANING_CREDIT[ind] <- 0
temp_installments <- merge(temp_installments, temp1, by = "SK_ID_CURR", all.x= TRUE)

write.csv(temp_installments,paste0(path,"/04_Interim/installment_payments_processed_V1.csv"), row.names=F)


# 05 credit_card_balance -----------------------------------------------------
View(head(credit_card_balance))
ind <- which(credit_card_balance$SK_ID_PREV %in% sk_id_prev)
credit_card_balance <- credit_card_balance[ind,]
rm(ind)
rm(previous_application)

credit_card_balance <- credit_card_balance[order(credit_card_balance$SK_ID_CURR,-credit_card_balance$MONTHS_BALANCE),]

credit_card_summarized <- data.frame()

SK_ID_CURR <- unique(credit_card_balance$SK_ID_CURR)
credit_card_summarized <- data.frame(SK_ID_CURR)
credit_card_summarized$Active <- 0
credit_card_summarized$COmpleted <- 0
credit_card_summarized$Signed <- 0
credit_card_summarized$proposal <- 0
credit_card_summarized$Refused <- 0
credit_card_summarized$Approved <- 0
credit_card_summarized$Demand <- 0

for ( i in 1:length(SK_ID_CURR)){
  sub <- credit_card_balance[credit_card_balance$SK_ID_CURR == SK_ID_CURR[i],]
  status <- unique(sub$NAME_CONTRACT_STATUS)
  for ( j in status){
    sub1 <- sub[sub$NAME_CONTRACT_STATUS == j,]
    if (j == 'Active'){
      credit_card_summarized$Active[i] <- length(unique(sub1$SK_ID_PREV))
    } else if (j == 'Completed'){
      credit_card_summarized$COmpleted[i] <- length(unique(sub1$SK_ID_PREV))
    } else if ( j == "Signed"){
      credit_card_summarized$Signed[i] <- length(unique(sub1$SK_ID_PREV))
    } else if (j == "Sent proposal"){
      credit_card_summarized$proposal[i] <- length(unique(sub1$SK_ID_PREV))
    } else if (j == "Refused"){
      credit_card_summarized$Refused[i] <- length(unique(sub1$SK_ID_PREV))
    } else if (j == "Approved"){
      credit_card_summarized$Approved[i] <- length(unique(sub1$SK_ID_PREV))
    } else if (j == "Demand"){
      credit_card_summarized$Demand[i] <- length(unique(sub1$SK_ID_PREV))
    } else {
      NA
    }
  }
  print (i)
}

credit_card_balance$test <- -credit_card_balance$AMT_DRAWINGS_ATM_CURRENT+ credit_card_balance$AMT_DRAWINGS_CURRENT+ credit_card_balance$AMT_DRAWINGS_OTHER_CURRENT
credit_card_balance$test <-  round(credit_card_balance$test,3)
ind <- which(!(credit_card_balance$test == credit_card_balance$AMT_DRAWINGS_POS_CURRENT))

View(credit_card_balance[ind,c("SK_ID_CURR","AMT_DRAWINGS_ATM_CURRENT","AMT_DRAWINGS_CURRENT","AMT_DRAWINGS_OTHER_CURRENT","AMT_DRAWINGS_POS_CURRENT","test")])

ind <- which(is.na(credit_card_balance$test))

# issues_cases <- credit_card_balance[ind,]
# credit_card_balance <- credit_card_balance[-ind,]

credit_card_balance$credit_utilization <- credit_card_balance$test/credit_card_balance$AMT_CREDIT_LIMIT_ACTUAL*100
ind <- which(credit_card_balance$credit_utilization == 'Inf')
credit_card_balance$credit_utilization[ind] <- 0

ind <- which(is.nan(credit_card_balance$credit_utilization))
credit_card_balance$credit_utilization[ind] <- 0

View(credit_card_balance[,c("SK_ID_CURR","AMT_DRAWINGS_ATM_CURRENT","AMT_DRAWINGS_CURRENT","AMT_DRAWINGS_OTHER_CURRENT","AMT_DRAWINGS_POS_CURRENT","test","AMT_CREDIT_LIMIT_ACTUAL","credit_utilization")])

credit_card_balance <- credit_card_balance[order(credit_card_balance$SK_ID_PREV,-credit_card_balance$MONTHS_BALANCE), ]

temp <- ddply(credit_card_balance,.(SK_ID_PREV),function(x) head(x,1))
View(head(credit_card_balance))
ind <- which(is.na(temp$test))
temp$test[ind] <- 0
