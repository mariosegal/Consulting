library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

qry1 <- "SELECT ACCT_ID, EXPRESSION_8,  ACCT_NUMBER_OF_DEPOSITS_MTD  from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') AND  CAST(ACCT_NUMBER_OF_DEPOSITS_MTD  as numeric(10,2)) > 0"



for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2014",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  assign(paste0('dep','2014',i),data)
}

rm(data)

library(dplyr)

aux <- bind_rows(lapply(ls(pattern='dep2014'),get))
dep2014 <- aux %>% group_by(EXPRESSION_8) %>% summarise_each(funs(sum),ACCT_NUMBER_OF_DEPOSITS_MTD) %>% filter(ACCT_NUMBER_OF_DEPOSITS_MTD>0)

save(dep2014,file='Z:/M&T Projects/OCA/dep2014.rdata')

qry1 <- "SELECT EXPRESSION_8,  ACCT_AMT_BAL_AVG_MONTH  from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') "

for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2014",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  assign(paste0('bal','2014',i),data)
}

aux <- bind_rows(lapply(ls(pattern='bal2014'),get))
bal2014 <- aux %>% group_by(EXPRESSION_8) %>% summarise_each(funs(mean),ACCT_AMT_BAL_AVG_MONTH) 
save(bal2014,file='Z:/M&T Projects/OCA/bal2014.rdata',compress='xz')
