
# 00 seeting workspace environment ----------------------------------------
path <- c("C:/Users/cuddly/Desktop/Credit_Risk_Kaggle")
setwd(paste0(path,"/02_Data"))

library(dplyr)

# 01 Importing data into R ------------------------------------------------
bureau <- read.csv("bureau.csv", as.is=T)
View(bureau[which(bureau$SK_ID_CURR==215354),])


# 02 Checking for NAs and unique labels in each columns -------------------
# Missing Value SUmmary ------------------------------------------------
missing_dt_summary <- data.frame()
nm <- names(bureau)
for (i in 1: length(nm)){
  nm1 <- nm[i]
  temp <- data.frame(c(nm1),sum(is.na(bureau[,i])),length(unique(bureau[,i])))
  colnames(temp) <- c("Variable","NA_cnt","level_cnt") 
  missing_dt_summary <- rbind(missing_dt_summary,temp)
}


# 03 Data Processing ------------------------------------------------------
bureau[is.na(bureau)] <- 0
sk_id <- unique(bureau$SK_ID_CURR)
i=58947

bureau_processed <- data.frame()
for ( i in 1:length(sk_id)){
  
id <- sk_id[i]
sub <- subset(bureau,SK_ID_CURR == id)
SK_ID_CURR <- data.frame("SK_ID_CURR" = id)
ACTIVE_LOAN_CNT <- nrow(subset(sub,CREDIT_ACTIVE == "Active"))
CLOSED_LOAN_CNT <- nrow(subset(sub,CREDIT_ACTIVE == "Closed"))
SOLD_LOAN_CNT <- nrow(subset(sub,CREDIT_ACTIVE == "Sold"))
BAD_DEBT_LOAN_CNT <- nrow(subset(sub,CREDIT_ACTIVE == "Bad debt"))

DAYS_CREDIT_WEEK <- nrow(subset(sub,DAYS_CREDIT >= -7))
DAYS_CREDIT_MONTH <- nrow(subset(sub,DAYS_CREDIT >= -30 & DAYS_CREDIT <= -7))
DAYS_CREDIT_QTR <- nrow(subset(sub,DAYS_CREDIT >= -90 & DAYS_CREDIT <= -30))
DAYS_CREDIT_YEAR <- nrow(subset(sub,DAYS_CREDIT >= -365 & DAYS_CREDIT <= -90))

AVERAGE_CREDIT_ENDDATE <- sub %>%
                              filter(CREDIT_ACTIVE == "Active") %>%
                              select(DAYS_CREDIT_ENDDATE) %>%
                              summarize(mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE))/365.25


SUM_CREDIT_PROLONG <-  sub %>%
                            select(CNT_CREDIT_PROLONG) %>%
                            summarize(sum(CNT_CREDIT_PROLONG, na.rm = TRUE))
                            
AMT_CREDIT_DUE <-  sub %>%
                        filter(CREDIT_ACTIVE == "Active") %>%
                        select(AMT_CREDIT_SUM,AMT_CREDIT_SUM_DEBT) %>%
                        summarize(sum(AMT_CREDIT_SUM_DEBT))

CREDIT_TYPE_CAR_LOAN <- nrow(subset(sub,CREDIT_TYPE == "Car loan"))
CREDIT_TYPE_CONSUMER_CREDIT <- nrow(subset(sub,CREDIT_TYPE == "Consumer credit"))
CREDIT_TYPE_CREDIT_CARD <- nrow(subset(sub,CREDIT_TYPE == "Credit card"))
CREDIT_TYPE_OTHERS <- nrow(subset(sub,!(CREDIT_TYPE %in% c('Car loan','Consumer credit','Credit card'))))

sub1 <- data.frame()
sub1 <- cbind(SK_ID_CURR,ACTIVE_LOAN_CNT, CLOSED_LOAN_CNT, SOLD_LOAN_CNT, BAD_DEBT_LOAN_CNT, DAYS_CREDIT_WEEK, DAYS_CREDIT_MONTH, DAYS_CREDIT_QTR, DAYS_CREDIT_YEAR, AVERAGE_CREDIT_ENDDATE, SUM_CREDIT_PROLONG, AMT_CREDIT_DUE, CREDIT_TYPE_CAR_LOAN, CREDIT_TYPE_CONSUMER_CREDIT, CREDIT_TYPE_CREDIT_CARD, CREDIT_TYPE_OTHERS)

rownames(sub1) <- i

bureau_processed <- rbind(bureau_processed,sub1)
print (i)

}

colnames(bureau_processed) <- c("SK_ID_CURR","ACTIVE_LOAN_CNT", "CLOSED_LOAN_CNT", "SOLD_LOAN_CNT", "BAD_DEBT_LOAN_CNT", "DAYS_CREDIT_WEEK", "DAYS_CREDIT_MONTH", "DAYS_CREDIT_QTR", "DAYS_CREDIT_YEAR", "AVERAGE_CREDIT_ENDDATE", "SUM_CREDIT_PROLONG",  "CREDIT_TYPE_CAR_LOAN", "CREDIT_TYPE_CONSUMER_CREDIT", "CREDIT_TYPE_CREDIT_CARD", "CREDIT_TYPE_OTHERS","AMT_CREDIT_DUE")

bureau_processed[is.na(bureau_processed)] <- 0

bureau_processed$AVERAGE_CREDIT_ENDDATE <- abs(bureau_processed$AVERAGE_CREDIT_ENDDATE)
bureau_processed$AMT_CREDIT_DUE <- abs(bureau_processed$AMT_CREDIT_DUE)


# 04 Saving data ----------------------------------------------------------

write.csv(bureau_processed,paste0(path,"/04_Interim/bureau_processed_V1.csv"), row.names=F)
