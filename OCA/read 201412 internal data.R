# read data for 201412


library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

part1 = 'SELECT   ACCT_ID, ACCT_PTYPE, ACCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME, ACCT_CONTR_NET_CONTRIBUTION_MTD,'
part2= 'ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME, EXPRESSION_8,EXPRESSION_18,EXPRESSION_19,ACCT_NUMBER_OF_DEPOSITS_MTD,ACCT_NUMBER_DEBITS_MTD,ACCT_MARKET_VALUE,'
part2a = 'ACCT_CQI,ACCT_CQI_BILL_PAY,ACCT_CQI_CHECK_CARD,ACCT_CQI_OVERDRAFT,ACCT_CQI_DIRECT_DEPOSIT,ACCT_CQI_WEB,'

part3 = 'ACCT_WEB_NBR_PMNT_MNT_ACCT,ACCT_WEB_NBR_PMNT_NON_MNT,ACCT_AMT_BAL_FOR_PRIME, ACCT_CONTR_NET_CONTRIBUTION_YTD from '
table = 'IQRMT.dbo.ACCT_201412'
part4="where (ACCT_SBU_GROUP = 'CON' or ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) "
part5 = "and ACCT_STATUS_FOR_PRIME <> 'X'"
qry = paste(part1, part2,part2a,part3,table,part4,part5)

accts_201412_oca <- sqlQuery(mtdata,qry)

dda_201401_oca <- sqlQuery(mtdata,"SELECT  ACCT_ID, EXPRESSION_8,ACCT_AMT_BAL_FOR_PRIME, ACCT_REG_E_FLAG_CUR from IQRMT.dbo.ACCT_201401 where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')")


dda_201412_oca <- sqlQuery(mtdata,"SELECT ACCT_ID, EXPRESSION_8,ACCT_AMT_BAL_FOR_PRIME,ACCT_REG_E_FLAG_CUR, ACCT_DATE_OF_BIRTH  from IQRMT.dbo.ACCT_201412 where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')")

dda_201401_oca_cqi <- sqlQuery(mtdata,"SELECT  ACCT_ID, EXPRESSION_8,ACCT_CQI,ACCT_CQI_BILL_PAY,ACCT_CQI_CHECK_CARD,ACCT_CQI_OVERDRAFT,ACCT_CQI_DIRECT_DEPOSIT,ACCT_CQI_WEB from IQRMT.dbo.ACCT_201401 where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')")

hhlds_201412 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.HHLD_201412")

ixi <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IXI_CODE_201412")

trans <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.TRAN_201412")

demog <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.CONDEM_201412")

eservice <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.ESERVICE_201412")

eactive <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.EACTIVE_201412")

names(eservice) <- tolower(gsub("-+","_",names(eservice)))
names(eservice) <- tolower(gsub("^e_service_","",names(eservice)))

names(eactive) <- tolower(gsub("-+","_",names(eactive)))

eservice$date_enrollment <- as.Date(eservice$date_enrollment ,'%m/%d/%Y')
eservice$date_cancel <- as.Date(eservice$date_cancel ,'%m/%d/%Y')

card_tran <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.Card_201412")
names(card_tran) <- tolower(gsub("^CARD_TRAN_","",names(card_tran)))


ifm <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201412")



#####Process the data as needed

#1) calculate the HHLD level measures by product

library(dplyr)
library(reshape2)
library(tidyr)

accts_201412_oca$ACCT_PTYPE <- as.character(accts_201412_oca$ACCT_PTYPE)
accts_201412_oca$ACCT_PTYPE[accts_201412_oca$ACCT_PTYPE=='CCS' & accts_201412_oca$ACCT_STYPE %in% c('NOR',"REW","SIG")] <- "CRD"
accts_201412_oca$ACCT_PTYPE <- as.factor(accts_201412_oca$ACCT_PTYPE)
accts_201412_oca$balance <- rowSums(accts_201412_oca[c('ACCT_AMT_BAL_FOR_PRIME','ACCT_MARKET_VALUE')],na.rm=T)


aux <- select(accts_201412_oca,c(ACCT_ID:ACCT_PTYPE,balance,ACCT_CONTR_NET_CONTRIBUTION_MTD))  %>% group_by(ACCT_ID,ACCT_PTYPE) %>% summarise(N=n_distinct(ACCT_PTYPE),bal=sum(balance,na.rm=T)/length(ACCT_PTYPE),contr=sum(ACCT_CONTR_NET_CONTRIBUTION_MTD,na.rm=T)/length(ACCT_PTYPE))

mysum <- function(x) {
  y = sum(!is.na(x),na.rm=T) 
  s=ifelse(y>0,sum(x,na.rm=T),NA)
  return(s)
}

#penet <-  aux %>% spread(ACCT_PTYPE,N,fill=0)
penet <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var='N',fun.aggregate = sum,fill=0)
#bals <-  aux %>% spread(ACCT_PTYPE,bal,fill=NA)
bals <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var='bal',fun.aggregate = sum,na.rm=T)
bals[penet==0] <- NA 
#contr <-  aux %>% spread(ACCT_PTYPE,contr,fill=NA)
contr <- dcast(aux,ACCT_ID~ACCT_PTYPE,value.var='contr',fun.aggregate = sum,na.rm=T)
contr[penet==0] <- NA 


names(bals)[-1] <- paste0(names(bals)[-1],'_bal')
names(contr)[-1] <- paste0(names(contr)[-1],'_contr')





# calculate debit and ATM transaction counts
aux1 <- card_tran %>% group_by(id,transaction_channel_no_wsid,transaction_type) %>% summarise(trans=sum(number_of_transaction,na.rm=T),amt=sum(amount_of_transaction,na.rm=T))
aux1 <- aux1 %>% filter((transaction_type=='WDRAL' & transaction_channel_no_wsid %in% c('ATMO','ATMT')) |
                          (transaction_channel_no_wsid %in% c('MPOS','VPOS') & 
                             transaction_type %in% c('DBPCB','DBPUR','DBPNP','DBPWV')))
card_trans <- aux1 %>% group_by(id,transaction_channel_no_wsid) %>% summarise_each(funs(sum),c(trans,amt)) %>% select(c(id,transaction_channel_no_wsid,trans)) %>% spread(transaction_channel_no_wsid,trans,fill=0)
names( card_trans)[-1] <- paste0(names(card_trans)[-1],'_trans')

card_tran1 <- inner_join(card_trans,card_amt)
  
card_amt <- aux1 %>% group_by(id,transaction_channel_no_wsid) %>% summarise_each(funs(sum),c(trans,amt)) %>% select(c(id,transaction_channel_no_wsid,amt)) %>% spread(transaction_channel_no_wsid,amt,fill=0)
names( card_amt)[-1] <- paste0(names(card_amt)[-1],'_amt')

card_tran1 <- inner_join(card_trans,card_amt)

# summarise eservices, and eactivity
esvc <- filter(eservice,(!is.na(date_enrollment) & date_cancel  < date_enrollment) | (!is.na(date_enrollment) & is.na(date_cancel))) %>% select(c(id,service)) %>% group_by(id,service) %>% summarise(n=n_distinct(service)) %>% spread(service,n,fill=0)




# create master dataset


#merge with accts

rm(aux)
save.image('oca_data.rdata')
