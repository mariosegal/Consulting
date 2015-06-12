library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')


c1 <- "SELECT  A.[EXPRESSION_8] as debit, B.[EXPRESSION_8] as dda,A.[ACCT_PTYPE] as ptype_deb, B.[ACCT_PTYPE] as ptype_dda, SUM(CAST(C.[CARD_TRAN_NUMBER_OF_TRANSACTION] as numeric(14,2))) as num, SUM(CAST(C.[CARD_TRAN_AMOUNT_OF_TRANSACTION] as numeric(14,2))) as amt from "

c2 <- "where A.[ACCT_PTYPE]='DEB' AND A.[EXPRESSION_9]=B.[EXPRESSION_8] and B.[ACCT_PTYPE]='DDA' and (C.[CARD_TRAN_TRANSACTION_CHANNEL_NO_WSID] IN ('MPOS','VPOS') AND left( C.[CARD_TRAN_TRANSACTION_TYPE],2) = 'DB') and (A.[EXPRESSION_8] = C.[EXPRESSION_1]) group by A.[EXPRESSION_8], B.[EXPRESSION_8],A.[ACCT_PTYPE] , B.[ACCT_PTYPE] "

for (year in 2013) {
  for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    
    table1 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"_V2] as A,")  
    table2 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"_V2] as B,")
    table3 <- paste0("[IQRMT].[dbo].[CARD_",year,i,"] as C ")
    
    data <- sqlQuery(mtdata,paste0(c1,table1,table2,table3,c2))
    
    data$period <- paste0(year,i)
    assign(paste0("debit",year,i),data)
  }
}

for (year in 2014) {
  for (i in c('01','02','03','04','08','09')) {
    
    table1 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"_V2] as A,")  
    table2 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"_V2] as B,")
    table3 <- paste0("[IQRMT].[dbo].[CARD_",year,i,"] as C ")
    
    data <- sqlQuery(mtdata,paste0(c1,table1,table2,table3,c2))
    
    data$period <- paste0(year,i)
    assign(paste0("debit",year,i),data)
  }
}


for (year in 2012) {
  for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    
    table1 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"] as A,")  
    table2 <- paste0("[IQRMT].[dbo].[ACCT_",year,i,"] as B,")
    table3 <- paste0("[IQRMT].[dbo].[CARD_",year,i,"] as C ")
    
    data <- sqlQuery(mtdata,paste0(c1,table1,table2,table3,c2))
    
    data$period <- paste0(year,i)
    assign(paste0("debit",year,i),data)
  }
}



load("Z:/M&T Projects/debit_partial.rdata")  #load all data

library(dplyr)

debit_all <- bind_rows(lapply(ls(pattern="debit"),get)) 
save(debit_all,file='Z:/M&T Projects/NSF/debit_all.rdata',compress='xz')
#rm(list=ls(pattern='debit201'))
