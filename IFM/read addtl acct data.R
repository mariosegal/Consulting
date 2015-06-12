
library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

part1 = 'SELECT  ACCT_ID, ACCT_PTYPE, ACCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME,'
part2= 'ACCT_SBU_GROUP , ACCT_STATUS_FOR_PRIME, EXPRESSION_8,'
part3 = 'ACCT_WEB_NBR_PMNT_MNT_ACCT,ACCT_WEB_NBR_PMNT_NON_MNT from '
table = 'IQRMT.dbo.ACCT_201409'
part4="where (ACCT_SBU_GROUP = 'CON' or ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) "
part5 = "and ACCT_STATUS_FOR_PRIME <> 'X'"
qry = paste(part1, part2,part3,table,part4,part5)

accts_201409 <- sqlQuery(mtdata,qry)
accts_201409$open_date <- as.Date(accts_201409$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts_201409,file='Z:/M&T Projects/accts_201409.rdata')

table = 'IQRMT.dbo.ACCT_201408'
qry = paste(part1, part2,part3,table,part4,part5)
accts_201408 <-  sqlQuery(mtdata,qry)

accts_201408$open_date <- as.Date(accts_201408$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts_201408,file='Z:/M&T Projects/accts_201408.rdata')

table = 'IQRMT.dbo.ACCT_201407'
qry = paste(part1, part2,part3,table,part4,part5)
accts_201407 <- sqlQuery(mtdata,qry)

accts_201407$open_date <- as.Date(accts_201407$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
save(accts_201407,file='Z:/M&T Projects/accts_201407.rdata')



ifm201409 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201409")
save(ifm201409,file='Z:/M&T Projects/ifm201409.rdata')

ifm201408 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201408")
save(ifm201408,file='Z:/M&T Projects/ifm201408.rdata')

ifm201407 <- sqlQuery(mtdata,"SELECT *  from IQRMT.dbo.IFM_201407")
save(ifm201407,file='Z:/M&T Projects/ifm201407.rdata')


accts_201201_all <- sqlQuery(mtdata,"select EXPRESSION_8, ACCT_DATE_OPENED_FOR_PRIME, ACCT_STYPE, ACCT_DATE_CLOSED from IQRMT.dbo.ACCT_201201 where LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA' ")
save(accts_201201_all,file='Z:/M&T Projects/NSF/accts_201201_all.rdata')

quit(save='no')