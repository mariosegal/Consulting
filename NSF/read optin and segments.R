library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')


a = "SELECT  EXPRESSION_8, ACCT_REG_E_FLAG_CUR FROM "
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


#get segments


a1 = "select A.EXPRESSION_8, B.HHLD_LIFE_CYCLE_SEGMENT from "
b1 = "where  (A.ACCT_PTYPE = 'DDA' AND LEFT(A.ACCT_STYPE,1)='R' AND A.ACCT_STATUS_FOR_PRIME = 'X') AND ( A.ACCT_ID = B.HHLD_ID)"



for (year in c('2013','2014')) {
  for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    table1a = paste0("IQRMT.dbo.ACCT_",year,month," as A, ")
    table2a = paste0("IQRMT.dbo.HHLD_",year,month," as B ")
    qry_a <- paste(a1,table1a,table2a,b1)
    data <- sqlQuery(mtdata,qry_a)
    assign(paste0('segm',year,month),data)
  }
}

segm <- bind_rows(lapply(ls(pattern='segm201'),get)) 
save(segm,file='segm.rdata',compress='xz')

         
         
IQRMT.dbo.ACCT_201412 as A, IQRMT.dbo.HHLD_201412 as B  
where  
(A.ACCT_PTYPE = 'DDA' AND LEFT(A.ACCT_STYPE,1)='R' AND A.ACCT_STATUS_FOR_PRIME = 'X') AND
( A.ACCT_ID = B.HHLD_ID)


segm_201412 <- sqlQuery(mtdata,"select A.EXPRESSION_8, B.HHLD_LIFE_CYCLE_SEGMENT from IQRMT.dbo.ACCT_201412 as A, IQRMT.dbo.HHLD_201412 as B where  (A.ACCT_PTYPE = 'DDA' AND LEFT(A.ACCT_STYPE,1)='R' AND A.ACCT_STATUS_FOR_PRIME <> 'X') AND ( A.ACCT_ID = B.HHLD_ID)")


segm_201212 <- sqlQuery(mtdata,"select A.EXPRESSION_8, B.HHLD_LIFE_CYCLE_SEGMENT from IQRMT.dbo.ACCT_201212 as A, IQRMT.dbo.HHLD_201212 as B where  (A.ACCT_PTYPE = 'DDA' AND LEFT(A.ACCT_STYPE,1)='R' AND A.ACCT_STATUS_FOR_PRIME <> 'X') AND ( A.ACCT_ID = B.HHLD_ID)")

save(segm_201212,segm_201412,file='Z:/M&T Projects/NSF/segm_aux.rdata')
