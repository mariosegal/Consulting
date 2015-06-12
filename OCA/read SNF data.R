library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

qry1 <- "SELECT EXPRESSION_8,  ACCT_NSF_TOTAL ,ACCT_CONTR_TOTAL_NSF_FEES from "
qry2<- "where ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA') AND  CAST(ACCT_CONTR_TOTAL_NSF_FEES as numeric(10,2)) > 0"



for (i in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  table = paste0("IQRMT.dbo.ACCT_2014",i)
  data <- sqlQuery(mtdata,paste(qry1,table,qry2))
  assign(paste0('nsf','2014',i),data)
  
}

rm(data)

library(dplyr)

aux <- bind_rows(lapply(ls(pattern='nsf2014'),get))
nsf2014b <- aux %>% group_by(EXPRESSION_8) %>% summarise(N=n(),nsf_num=sum(ACCT_NSF_TOTAL),nsf_fees=sum(ACCT_CONTR_TOTAL_NSF_FEES)) %>% filter(nsf_num>0)

save(nsf2014b,file='Z:/M&T Projects/OCA/nsf2014b.rdata',compress='xz')
