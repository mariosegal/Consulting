heqad(setwd("Z:/M&T Projects/IFM")
library(RODBC)


mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

accts <- sqlQuery(mtdata,"SELECT ACCT_ID, ACCT_PTYPE, ACCT_STYPE, 
ACCT_DATE_OPENED_FOR_PRIME, ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME from 
IQRMT.dbo.ACCT_201403_V2 where (ACCT_SBU_GROUP = 'CON' or 
( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) and  
ACCT_STATUS_FOR_PRIME <> 'X'")
accts$open_date <- as.Date(accts$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts,file='Z:/M&T Projects/accts.rdata')
accts$open_date <- as.Date(accts$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts,file='Z:/M&T Projects/accts.rdata')

accts_201410 <- sqlQuery(mtdata,"SELECT ACCT_ID, ACCT_PTYPE, ACCT_STYPE, 
ACCT_DATE_OPENED_FOR_PRIME, ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME, 
ACCT_CREDIT_BUREAU_SCORE_CURRENT, EXPRESSION_8 from 
IQRMT.dbo.ACCT_201410 where (ACCT_SBU_GROUP = 'CON' or 
( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) and  
ACCT_STATUS_FOR_PRIME <> 'X'")

accts_201410$open_date <- as.Date(accts_201410$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts_201410,file='Z:/M&T Projects/accts_201410.rdata')





hhlds <- sqlQuery(mtdata,"SELECT *  from 
IQRMT.dbo.HHLD_201403")
save(hhlds,file='Z:/M&T Projects/hhlds.rdata')


ixi <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IXI_CODE_201312")
ixi <- ixi[-c(24:25)]


eservice <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.ESERVICE_201312")
eservice <- eservice[1:12]
names(eservice)[11:12] <- c('web_acct_id','acct_id')

trans <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.TRAN_201312")
trans <- trans[1:30]
names(trans)[30] <-  'acct_id'

save(ixi,file='Z:/M&T Projects/IFM/ixi.rdata')
save(eservice,file='Z:/M&T Projects/IFM/eservice.rdata')
save(trans,file='Z:/M&T Projects/IFM/trans.rdata')

trans_hh <- trans %>% group_by(TRANS_ID) %>% summarise_each(funs(sum))
save(trans_hh,file='Z:/M&T Projects/IFM/trans_hh.rdata')


cols <- sqlQuery(mtdata,"SELECT * from 
IQRMT.INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = 'ACCT_201312' ")

keep = c(78,150,5,19,32:60,63,83,84,92,94,106,111,112,118:119,103,144,140:141,120)
columns1 <- paste(levels(cols$COLUMN_NAME)[keep[1:17]],collapse=',')
columns2 <- paste(levels(cols$COLUMN_NAME)[keep[c(1:2,18:32)]],collapse=',')
columns3 <- paste(levels(cols$COLUMN_NAME)[keep[c(1:2,33:48)]],collapse=',')
condition <- "((ACCT_SBU_GROUP ='CON' and ACCT_PTYPE in ('MMS','TDA','IRA','SAV','CCS','ILN','INS','HEQ','MTG','SEC','SDB'))  or ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) and  ACCT_STATUS_FOR_PRIME <> 'X'"

accts_201312a <- sqlQuery(mtdata,paste("SELECT ", columns1, "from IQRMT.dbo.ACCT_201312 where ",condition),rows_at_time=100)
accts_201312b <- sqlQuery(mtdata,paste("SELECT ", columns2, "from IQRMT.dbo.ACCT_201312 where ",condition),rows_at_time=100)
accts_201312c <- sqlQuery(mtdata,paste("SELECT ", columns3, "from IQRMT.dbo.ACCT_201312 where ",condition),rows_at_time=100)

rm(list=c('columns1','columns2','columns3','cols','keep','condition'))

#create factors and other transformations as needed
accts_201312c$open_date <- as.Date(accts_201312c$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')

accts_201312a$ACCT_COMMUNITY_BANK_MARKET <- as.factor(accts_201312a$ACCT_COMMUNITY_BANK_MARKET)
accts_201312b <- accts_201312b[-5]

accts_201312b[12:16] <- lapply(accts_201312b[12:16],as.factor)

accts_201312c[c(3,5,9,10,11,12,13)] <- lapply(accts_201312c[c(3,5,9,10,11,12,13)],as.factor)

save(accts_201312a,accts_201312b,accts_201312c,file='Z:/M&T Projects/accts_201312.rdata')


cols1 <- sqlQuery(mtdata,"SELECT * from 
IQRMT.INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = 'HHLD_201312' ")



hhlds_201312 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.HHLD_201312",rows_at_time=100)
hhlds_201312 <- hhlds_201312[hhlds_201312$HHLD_ID %in% unique(c(accts_201312a$ACCT_ID,accts_201312b$ACCT_ID,accts_201312c$ACCT_ID)),]
save(hhlds_201312,file='Z:/M&T Projects/hhld_201312.rdata')
rm(cols1)

ifm <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201312")
save(ifm,file='Z:/M&T Projects/ifm.rdata')


#crete addtl hlhd measures
library(reshape2)
library(dplyr)
mylength <- function(x) {ifelse(length(x)>=1,1,0)}
accts_201312c <- cbind((accts_201312c),(accts_201312a$ACCT_AMT_BAL_FOR_PRIME)+(accts_201312c$ACCT_MARKET_VALUE),accts_201312a$ACCT_CONTR_NET_CONTRIBUTION_MTD)
names(accts_201312c)[20:21] <- c('balance','contr')


aux <- accts_201312c %>% group_by(ACCT_ID,ACCT_PTYPE) %>% summarise(N=min(n(),1),bal=sum(balance),contr=sum(contr))



prods <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var = 'N',fill=0)
bals <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var = 'bal',fun.aggregate = sum)
contr <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var = 'contr',fun.aggregate = sum)
contr[prods==0] <- NA


save(prods,bals,contr,file='hhld_extra.rdata')



#read 201410 data
dda_201410 <- sqlQuery(mtdata,"SELECT ACCT_ID, EXPRESSION_8, ACCT_PTYPE, ACCT_STYPE,
ACCT_DATE_OPENED_FOR_PRIME, ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME , ACCT_WEB_BILL_PAYS_PRIOR_MONTH,
ACCT_NBR_POS_PIN_TRANS,ACCT_NSF_TOTAL,ACCT_NSF_TOTAL_CHECK,ACCT_NUMBER_DEBITS_MTD,
ACCT_NUMBER_OF_DEPOSITS_MTD,ACCT_NUMBER_OF_WITHDRAWALS_MTD,ACCT_WEB_NBR_PMNT_MNT_ACCT,ACCT_WEB_NBR_PMNT_NON_MNT,
ACCT_WEB_NBR_XFER_FROM_CKG, ACCT_CQI, ACCT_CQI_BILL_PAY, ACCT_CQI_CHECK_CARD,
ACCT_CQI_WEB, ACCT_CQI_DIRECT_DEPOSIT, ACCT_CQI_OVERDRAFT, ACCT_WEB_BILL_PAYS_PRIOR_MONTH,
ACCT_AMT_BAL_FOR_PRIME  from 
IQRMT.dbo.ACCT_201403_V2 where (
( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) and  
ACCT_STATUS_FOR_PRIME <> 'X'")

web <- sqlQuery(mtdata,"SELECT ACCT_ID, EXPRESSION_8, EXPRESSION_9, EXPRESSION_10, EXPRESSION_11,
EXPRESSION_12, EXPRESSION_13, EXPRESSION_14,EXPRESSION_15,EXPRESSION_16,EXPRESSION_17, ACCT_PTYPE, ACCT_STYPE,
ACCT_DATE_OPENED_FOR_PRIME, ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME , ACCT_WEB_BILL_PAYS_PRIOR_MONTH, 
ACCT_AMT_BAL_FOR_PRIME  from 
IQRMT.dbo.ACCT_201403_V2 where (
( ACCT_PTYPE='WEB')) and  
ACCT_STATUS_FOR_PRIME <> 'X'")

ifm_201410 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201410")

close(mtdata)
rm(mtdata)

save(dda_201410,file='Z:/M&T Projects/IFM/dda_201410.rdata')
save(ifm_201410,file='Z:/M&T Projects/IFM/ifm_201410.rdata')

load('ifm_201410.rdata')
load('dda_201410.rdata')

names(dda_201410)[2] <- "acct_id"

ifm_201410$ifm_flag <- 1
merged <- merge(dda_201410,ifm_201410,by.x='acct_id',by.y= 'EXPRESSION_1',all.x=T)

merged$INTELLIGENTSIA_MORTGAGE_1 <- sapply(merged$INTELLIGENTSIA_MORTGAGE_1, function (x) gsub('[ ]+$','',x))
merged$ifm_flag <- ifelse(is.na(merged$ifm_flag),0,merged$ifm_flag)
table(merged$ifm_flag,useNA='ifany')

names(merged) <- gsub('^INTELLIGENTSIA_','',names(merged))

head(merged$MORTGAGE_1)

sum(merged$MORTGAGE_1_PAYMENT_COUNT >0,na.rm = T)
summary(merged$MORTGAGE_1_AMT[merged$MORTGAGE_1_PAYMENT_COUNT >0],na.rm = T)
sum(merged$CREDIT_CARD_PAYMENT_COUNT > 0,na.rm=T)
summary(merged$CREDIT_CARD_AMT[merged$CREDIT_CARD_PAYMENT_COUNT >0],na.rm = T)
sum(merged$INVEST_DEBIT_COUNT > 0)
mean(merged$INVEST_DEBIT_AMT[merged$INVEST_DEBIT_COUNT >0],na.rm = T)

sum(merged$INVEST_CREDIT_COUNT > 0)
mean(merged$INVEST_CREDIT_AMT[merged$INVEST_CREDIT_COUNT >0],na.rm = T)


sum(merged$BANK_TRANS_DEBIT_COUNT > 0)
mean(merged$BANK_TRANS_DEBIT_AMT[merged$BANK_TRANS_DEBIT_COUNT >0],na.rm = T)

sum(merged$BANK_TRANS_CREDIT_COUNT > 0)
mean(merged$BANK_TRANS_CREDIT_AMT[merged$BANK_TRANS_CREDIT_COUNT >0],na.rm = T)


sum(merged$INSTALL_LOAN_PAYMENT_COUNT > 0)
mean(merged$INSTALL_LOAN_AMT[merged$BANK_TRANS_CREDIT_COUNT >0],na.rm = T)


sum(merged$EARNED_INCOME_COUNT>0 | merged$FIXED_INCOME_COUNT > 0 | merged$SSN_COUNT>0)
summary(merged$EARNED_INCOME_AMT[merged$EARNED_INCOME_COUNT>0],na.rm=T)
summary(merged$FIXED_INCOME_AMT[merged$FIXED_INCOME_COUNT>0],na.rm=T)
summary(merged$SSN_AMT[merged$SSN_COUNT>0],na.rm=T)

library(plyr)
library(dplyr)
merged %>% group_by(ifm_flag) %>% summarise(N=n(),cqi=mean(ACCT_CQI,na.rm=T),
                                            bal=mean(ACCT_AMT_BAL_FOR_PRIME,na.rm=T))

merged %>% group_by(ifm_flag,ACCT_CQI) %>% summarise(N=n())
summary(merged$ACCT_AMT_BAL_FOR_PRIME[merged$ifm_flag==0])
summary(merged$ACCT_AMT_BAL_FOR_PRIME[merged$ifm_flag==1])


merged$trans <- merged$ACCT_NUMBER_OF_DEPOSITS_MTD + merged$ACCT_NUMBER_DEBITS_MTD
table(merged$trans)

sum(merged$trans>=5)/1790250


library(reshape2);library(reshape)
#do amelted to be able to do all analysis faster
melt1 <- melt(merged,id.vars = c(1,5,4,177,179),
              measure.vars = c(53:58,83,84,103,104,110,111,120,121,127,128,141,142,134,135,148,149,61,62,68,69,75,76))

melt1$type <- ifelse(grepl('COUNT$',melt1$variable),'COUNT','AMT')
melt1$measure <- gsub('_COUNT|_AMT','',melt1$variable)
melt1$measure <- gsub('_PAYMENT','',melt1$measure)
melt1$measure <- gsub('_TOTAL','',melt1$measure)
unique(melt1$measure)

#reshape to have side by side columns with the data
melt2 <- dcast(melt1,acct_id+ifm_flag+active+measure~type,fun.aggregate = sum)

#calc measures for all
results <- melt2 %>% filter(ifm_flag==1)  %>% group_by(ifm_flag,active,measure) %>% summarise(N=n(),with=sum(COUNT>=1),avg=sum(AMT,na.rm=T)/sum(COUNT>=1,na.rm=T))
write.table(results,'clipboard-128',sep='\t',row.names=F)

merged %>% group_by(ifm_flag) %>% summarise(N=n(),active=sum(trans>=5),bal1k=sum(ACCT_AMT_BAL_FOR_PRIME>=1000))
merged %>% group_by(ifm_flag) %>% summarise(N=n(),active=sum(trans>=5)/n(),bal1k=sum(ACCT_AMT_BAL_FOR_PRIME>=1000)/n(),
                                            both=sum(ACCT_AMT_BAL_FOR_PRIME>=1000 & trans>=5)/n())

merged$active <- ifelse(merged$ACCT_AMT_BAL_FOR_PRIME >= 500 & merged$trans >=5,1,0)
prop.table(table(merged$ifm_flag,merged$active),2)

#merged %>% group_by(ifm_flag,active) %>% summarise(mtg=sum(MORTGAGE_1_PAYMENT_COUNT>=1)/n(),amt=sum(MORTGAGE_1_AMT)/sum(MORTGAGE_1_PAYMENT_COUNT>=1))
merged %>% group_by(ifm_flag,active) %>% summarise(card=sum(INSURANCE_PAYMENT_COUNT>=1)/n(),amt=sum(CREDIT_CARD_AMT)/sum(INSURANCE_PAYMENT_COUNT>=1))

prop.table(table(merged$INSURANCE_PAYMENT_COUNT>0,merged$active,merged$ifm_flag),2)

#top insurance
merged %>% filter(AUTO_INSURANCE_PAYMENT_COUNT>=1) %>% group_by(AUTO_INSURANCE) %>% 
  summarise(N=n()) %>% arrange(desc(N))

life <- merged %>% filter(LIFE_INSURANCE_PAYMENT_COUNT>=1) %>% group_by(LIFE_INSURANCE) %>% 
  summarise(N=n()) %>% arrange(desc(N))
write.table(life,'clipboard-128',sep='\t',row.names=F)


other <- merged %>% filter(INSURANCE_PAYMENT_COUNT>=1) %>% group_by(INSURANCE_CARRIER) %>% 
  summarise(N=n()) %>% arrange(desc(N))
write.table(other,'clipboard-128',sep='\t',row.names=F)
#how many trans
table(merged$ACH_CREDIT_TRANS_COUNT+merged$ACH_CREDIT_TRANS_COUNT)
#noifm <-  dda_201410[!(dda_201410$acct_id %in% merged$acct_id),]
#prop.table(table(noifm$ACCT_NUMBER_DEBITS_MTD>0,noifm$ACCT_NUMBER_OF_DEPOSITS_MTD>0,dnn=c('Debits','Deposits')))
