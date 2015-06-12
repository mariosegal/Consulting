library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')



qry1 <- "SELECT EXPRESSION_8,  ACCT_NSF_TOTAL ,ACCT_CONTR_TOTAL_NSF_FEES, ACCT_CONTR_TOTAL_NSF_FEES_WAIVED from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') AND  (CAST(ACCT_CONTR_TOTAL_NSF_FEES as numeric(10,2)) > 0 )"

for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  data$period <- paste0('2013',i)
  assign(paste0('nsf','2013',i),data)
  
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  data1 <- sqlQuery(mtdata,paste(qry1,table1,qry2))
  data1$period <- paste0('2014',i)
  assign(paste0('nsf','2014',i),data1)
  
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  data2 <- sqlQuery(mtdata,paste(qry1,table2,qry2))
  data2$period <- paste0('2012',i)
  assign(paste0('nsf','2012',i),data2)
}

library(dplyr)
nsf_accts <- bind_rows(lapply(ls(pattern='nsf201'),get)) 
save(nsf_accts,file='nsf_accts.rdata',compress='xz')

rm(data);rm(data1)



for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  data = sqlQuery(mtdata,paste('SELECT EXPRESSION_8 FROM',table,"WHERE ACCT_STATUS_FOR_PRIME = 'X' AND ACCT_PTYPE='DDA' AND LEFT(ACCT_STYPE,1)='R' AND left(ACCT_DATE_CLOSED,2) = ",i))
  data$period <- paste0('2013',i)
  assign(paste0('closed','2013',i),data)
  
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  data1 = sqlQuery(mtdata,paste('SELECT EXPRESSION_8 FROM',table1,"WHERE ACCT_STATUS_FOR_PRIME = 'X' AND ACCT_PTYPE='DDA' AND LEFT(ACCT_STYPE,1)='R' AND left(ACCT_DATE_CLOSED,2) = ",i))
  data1$period <- paste0('2014',i)
  assign(paste0('closed','2014',i),data1)
  
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  data2 = sqlQuery(mtdata,paste('SELECT EXPRESSION_8 FROM',table2,"WHERE ACCT_STATUS_FOR_PRIME = 'X' AND ACCT_PTYPE='DDA' AND LEFT(ACCT_STYPE,1)='R' AND left(ACCT_DATE_CLOSED,2) = ",i))
  data2$period <- paste0('2012',i)
  assign(paste0('closed','2012',i),data2)
}

rm(data); rm(data1)

closed_accts <- bind_rows(lapply(ls(pattern='closed201'),get)) 
save(closed_accts,file='closed_accts.rdata',compress='xz')

#GET ALL accts details, I will get all accts for 201201 and all new ones for later

a = "SELECT  ACCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME, EXPRESSION_8 FROM "
b = " WHERE ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R' AND ACCT_STATUS_FOR_PRIME <> 'X' AND left(ACCT_DATE_OPENED_FOR_PRIME,2) = "
for (i in c('02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  p1 = paste0('2013',i)
  p2 = paste0('2014',i)
  p3 = paste0('2012',i)
  data = sqlQuery(mtdata,paste0(a,table,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2013'"))
  data1 = sqlQuery(mtdata,paste0(a,table1,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2014'"))
  data2 = sqlQuery(mtdata,paste0(a,table2,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2012'"))
  assign(paste0('base','2013',i),data)
  assign(paste0('base','2014',i),data1)
  assign(paste0('base','2012',i),data2)
}

i = '01'
table = paste0("IQRMT.dbo.ACCT_2014",i)
p1 = paste0('2014',i)
data = sqlQuery(mtdata,paste0(a,table,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2014'"))
assign(paste0('base','2014',i),data)


table = paste0("IQRMT.dbo.ACCT_2013",i)
p1 = paste0('2034',i)
data = sqlQuery(mtdata,paste0(a,table,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2013'"))
assign(paste0('base','2013',i),data)

b = " WHERE ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R' AND ACCT_STATUS_FOR_PRIME <> 'X' "
table = paste0(" IQRMT.dbo.ACCT_2012",i)
p1 = paste0('2012',i)
data = sqlQuery(mtdata,paste(a,table,b))
assign(paste0('base','2012',i),data)

base <- bind_rows(lapply(ls(pattern='base'),get)) 
save(base,file='base.rdata',compress='xz')


#read contribution details

qry1 <- "SELECT  EXPRESSION_8,  ACCT_CONTR_TOTAL_NSF_FEES, ACCT_CONTR_TOTAL_NSF_FEES_WAIVED ,CAST(ACCT_CONTR_INTEREST_INCOME as numeric(10,2))  - CAST(ACCT_CONTR_INTEREST_EXPENSE as numeric(10,2)) as NII, ACCT_CONTR_MAINT_FEES_NET_WAIVERS as maint, CAST(ACCT_CONTR_POS_FEES_NET_WAIVERS as numeric(10,2)) + CAST(ACCT_CONTR_FOREIGN_ATM_FEE_NET_WVRS as numeric(10,2)) + CAST(ACCT_CONTR_BILLPAY_FEES_NET_WAIVERS as numeric(10,2)) AS other_fees, ACCT_CONTR_INTERCHANGE_INCOME ,ACCT_CONTR_BALANCE  from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') "



for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  data$period <- paste0('2013',i)
  assign(paste0('contr','2013',i),data)
  
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  data1 <- sqlQuery(mtdata,paste(qry1,table1,qry2))
  data1$period <- paste0('2014',i)
  assign(paste0('contr','2014',i),data1)
  
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  data2 <- sqlQuery(mtdata,paste(qry1,table2,qry2))
  data2$period <- paste0('2012',i)
  assign(paste0('contr','2012',i),data2)
}

#for 201201 it is ACCT_CONTR_MAINT_FEE_NET_WAIVERS, had to adjust it manually
i<- '01'
qry1 <- "SELECT  EXPRESSION_8,  ACCT_CONTR_TOTAL_NSF_FEES, ACCT_CONTR_TOTAL_NSF_FEES_WAIVED ,CAST(ACCT_CONTR_INTEREST_INCOME as numeric(10,2))  - CAST(ACCT_CONTR_INTEREST_EXPENSE as numeric(10,2)) as NII, ACCT_CONTR_MAINT_FEEs_NET_WAIVERS as maint, CAST(ACCT_CONTR_POS_FEES_NET_WAIVERS as numeric(10,2)) + CAST(ACCT_CONTR_FOREIGN_ATM_FEES_NET_WVRS as numeric(10,2)) + CAST(ACCT_CONTR_BILLPAY_FEES_NET_WAIVERS as numeric(10,2)) AS other_fees, ACCT_CONTR_INTERCHANGE_INCOME ,ACCT_CONTR_BALANCE  from "
table2 = paste0("IQRMT.dbo.ACCT_2012",i)
data2 <- sqlQuery(mtdata,paste(qry1,table2,qry2))
data2$period <- paste0('2012',i)
assign(paste0('contr','2012',i),data2)

contrib <- bind_rows(lapply(ls(pattern='contr201'),get)) 
save(contrib,file='contrib.rdata',compress='xz')

##############
###############

#also get more NSf detail

qry1 <- "SELECT EXPRESSION_8,  ACCT_NSF_TOTAL_CHECK  , ACCT_NSF_TOTAL_OTHER from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') AND  (CAST(ACCT_CONTR_TOTAL_NSF_FEES as numeric(10,2)) > 0 )"

for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  data$period <- paste0('2013',i)
  assign(paste0('extra','2013',i),data)
  
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  data1 <- sqlQuery(mtdata,paste(qry1,table1,qry2))
  data1$period <- paste0('2014',i)
  assign(paste0('extra','2014',i),data1)
  
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  data2 <- sqlQuery(mtdata,paste(qry1,table2,qry2))
  data2$period <- paste0('2012',i)
  assign(paste0('extra','2012',i),data2)
}

extra <- bind_rows(lapply(ls(pattern='extra201'),get)) 
save(extra,file='extra.rdata',compress='xz')



#read opt in

a = "SELECT  top 10 EXPRESSION_8, ACCT_REG_E_FLAG_CUR FROM "
b = " WHERE ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R' AND ACCT_STATUS_FOR_PRIME <> 'X' AND left(ACCT_DATE_OPENED_FOR_PRIME,2) = "
for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  p1 = paste0('2013',i)
  p2 = paste0('2014',i)
  p3 = paste0('2012',i)
  data = sqlQuery(mtdata,paste0(a,table,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2013'"))
  data1 = sqlQuery(mtdata,paste0(a,table1,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2014'"))
  data2 = sqlQuery(mtdata,paste0(a,table2,b,"'",i,"'"," AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2012'"))
  assign(paste0('optin','2013',i),data)
  assign(paste0('optin','2014',i),data1)
  assign(paste0('optin','2012',i),data2)
}

optin <- bind_rows(lapply(ls(pattern='optin201'),get)) 
save(optin,file='optin.rdata',compress='xz')
rm(list=ls(pattern='optin201'))



#for all accts that were opened on or before 201301 (to cover all the existing book on both periods)
#extract in each period the debits, deposits# 
#also the debit card purchases 

a = "SELECT  EXPRESSION_8, ACCT_NUMBER_DEBITS_MTD, ACCT_NUMBER_OF_DEPOSITS_MTD, ACCT_NUMBER_OF_WITHDRAWALS_MTD FROM "
b = " WHERE ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R' AND ACCT_STATUS_FOR_PRIME <> 'X' AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) <= '2013' "

for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2013",i)
  table1 = paste0("IQRMT.dbo.ACCT_2014",i)
  table2 = paste0("IQRMT.dbo.ACCT_2012",i)
  p1 = paste0('2013',i)
  p2 = paste0('2014',i)
  p3 = paste0('2012',i)
  data = sqlQuery(mtdata,paste0(a,table,b))
  data$period <- p1
  data1 = sqlQuery(mtdata,paste0(a,table1,b))
  data1$period <- p2
  data2 = sqlQuery(mtdata,paste0(a,table2,b))
  data2$period <- p3
  assign(paste0('act','2013',i),data)
  assign(paste0('act','2014',i),data1)
  assign(paste0('act','2012',i),data2)
}

activity <- bind_rows(lapply(ls(pattern="act201"),get)) 
save(activity,file='activity.rdata',compress='xz')
#rm(list=ls(pattern='optin201'))

rm(data,data1,data2)

c1 <- "SELECT  A.[EXPRESSION_8] as debit, B.[EXPRESSION_8] as dda,A.[ACCT_PTYPE] as ptype_deb, B.[ACCT_PTYPE] as ptype_dda, SUM(CAST(C.[CARD_TRAN_NUMBER_OF_TRANSACTION] as numeric(14,2))) as num, SUM(CAST(C.[CARD_TRAN_AMOUNT_OF_TRANSACTION] as numeric(14,2))) as amt from "

c2 <- "where A.[ACCT_PTYPE]='DEB' AND A.[EXPRESSION_9]=B.[EXPRESSION_8] and B.[ACCT_PTYPE]='DDA' and (C.[CARD_TRAN_TRANSACTION_CHANNEL_NO_WSID] IN ('MPOS','VPOS') AND left( C.[CARD_TRAN_TRANSACTION_TYPE],2) = 'DB') and (A.[EXPRESSION_8] = C.[EXPRESSION_1]) group by A.[EXPRESSION_8], B.[EXPRESSION_8],A.[ACCT_PTYPE] , B.[ACCT_PTYPE] "

for (year in 2013:2014) {
  for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    
    table1 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"] as A,")  
    table2 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"] as B,")
    table3 <- paste0("[IQRMT].[dbo].[CARD_",year,i,"] as C ")
    
    data <- sqlQuery(mtdata,paste0(c1,table1,table2,table3,c2))
    
    data$period <- paste0(year,i)
    assign(paste0("debit",year,i),data)
  }
}



debit <- bind_rows(lapply(ls(pattern="debit201"),get)) 
save(debit,file='debit_partial.rdata',compress='xz')
#rm(list=ls(pattern='debit201'))


#read cqi
a = "SELECT  EXPRESSION_8,ACCT_CQI_BILL_PAY, ACCT_CQI_CHECK_CARD, ACCT_CQI_OVERDRAFT, ACCT_CQI_DIRECT_DEPOSIT,ACCT_CQI_WEB FROM "
b = " WHERE ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R' AND ACCT_STATUS_FOR_PRIME <> 'X' "
for (period in c('201201','201312')) {
  table = paste0("IQRMT.dbo.ACCT_",period)
  
  #p1 = paste0('2013',i)
  data = sqlQuery(mtdata,paste0(a,table,b))
  assign(paste0('cqi',period),data)
  
}

save(cqi201201,file='cqi201201.rdata',compress='xz')
save(cqi201312,file='cqi201312.rdata',compress='xz')