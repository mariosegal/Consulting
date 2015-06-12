library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')


#read all accts for all of 2014 to match to complaints

q1 = 'SELECT ACCT_ID, EXPRESSION_8 , ACCT_PTYPE, aCCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME, EXPRESSION_18, ACCT_SBU_GROUP from IQRMT.dbo.ACCT_2014'
#q2 = " WHERE ((ACCT_SBU_GROUP='CON') OR (ACCT_PTYPE = 'DDA' AND LEFT(ACCT_STYPE,1)='R')) "
q2 = " WHERE 1=1"
q3= " and (ACCT_STATUS_FOR_PRIME <> 'X' "
q4 = " AND LEFT(ACCT_DATE_OPENED_FOR_PRIME,2) = '"
q5 = " AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2014' )"

for (month in c('02','03','04','05','06','07','08','09','10','11','12')) {
  qry = paste0(q1,month,q2,q3,q4,month,"'",q5)
  aux = sqlQuery(mtdata,qry)
  aux$table = paste0('2014',month)
  assign(paste0("accts_2014",month,'_all'),aux)
}

accts_201401_all <- sqlQuery(mtdata,paste0(q1,'01',q2))
accts_201401_all$table = '201401'

#also read 2015
q1 = 'SELECT ACCT_ID, EXPRESSION_8 , ACCT_PTYPE, aCCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME, EXPRESSION_18, ACCT_SBU_GROUP from IQRMT.dbo.ACCT_2015'
q5 = " AND RIGHT(ACCT_DATE_OPENED_FOR_PRIME,4) = '2015' )"
for (month in c('01','02','03')) {
  qry = paste0(q1,month,q2,q3,q4,month,"'",q5)
  aux = sqlQuery(mtdata,qry)
  aux$table = paste0('2015',month)
  assign(paste0("accts_2015",month,'_all'),aux)
}


#read the closed accts per period

q1a = 'SELECT ACCT_ID, EXPRESSION_8 , ACCT_PTYPE, aCCT_STYPE, ACCT_DATE_CLOSED from IQRMT.dbo.ACCT_2014'
for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  qry = paste0(q1a,month,q2,"and (ACCT_STATUS_FOR_PRIME = 'X')")
  aux = sqlQuery(mtdata,qry)
  aux$closed_period = paste0('2014',month)
  assign(paste0("closed_2014",month,'_all'),aux)
}

q1b = 'SELECT ACCT_ID, EXPRESSION_8 , ACCT_PTYPE, aCCT_STYPE, ACCT_DATE_CLOSED from IQRMT.dbo.ACCT_2015'
for (month in c('01','02','03')) {
  qry = paste0(q1b,month,q2,"and (ACCT_STATUS_FOR_PRIME = 'X')")
  aux = sqlQuery(mtdata,qry)
  aux$closed_period = paste0('2015',month)
  assign(paste0("closed_2015",month,'_all'),aux)
}


library(tidyr)
library(dplyr)
closed_all_2014 <- bind_rows(lapply(ls(pattern='closed'),get)) 
base_all_2014 <- bind_rows(lapply(ls(pattern='accts_2014'),get)) 
 
closed_all_2015Q1 <- bind_rows(lapply(ls(pattern='closed_2015'),get)) 
base_all_2015Q1 <- bind_rows(lapply(ls(pattern='accts_2015'),get)) 

save(closed_all_2014,base_all_2014,file="Z:/M&T Projects/OCA/accts_base_OCA_2014.rdata")
save(closed_all_2015Q1,base_all_2015Q1,file="Z:/M&T Projects/OCA/accts_base_OCA_2015Q1.rdata")


for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  qry = paste0("SELECT A.HHLD_ID, A.HHLD_COMMUNITY_BANK_MARKET, A.HHLD_HH_OWN_AGE ,  A.HHLD_NUMSERVICES,A.HHLD_AMT_CONTRIB_MTD, A.HHLD_LIFE_CYCLE_SEGMENT,A.HHLD_CLV_TOTAL , A.HHLD_CLV_REMAINING, A.HHLD_CLV_REMAINING_TENURE,B.IXI_WC_TOTAL_ASSETS from IQRMT.dbo.HHLD_2014",month," AS A , IQRMT.dbo.IXI_CODE_2014",month," AS B WHERE A.HHLD_ID = B.[IX-ID]")
  aux = sqlQuery(mtdata,qry)
  aux$period = paste0('2014',month)
  assign(paste0("hhld_2014",month,'_all'),aux)
}


for (month in c('01','02','03')) {
  qry = paste0("SELECT A.HHLD_ID, A.HHLD_COMMUNITY_BANK_MARKET, A.HHLD_HH_OWN_AGE ,  A.HHLD_NUMSERVICES,A.HHLD_AMT_CONTRIB_MTD, A.HHLD_LIFE_CYCLE_SEGMENT,A.HHLD_CLV_TOTAL , A.HHLD_CLV_REMAINING, A.HHLD_CLV_REMAINING_TENURE,B.IXI_WC_TOTAL_ASSETS from IQRMT.dbo.HHLD_2015",month," AS A , IQRMT.dbo.IXI_CODE_2015",month," AS B WHERE A.HHLD_ID = B.[IX-ID]")
  aux = sqlQuery(mtdata,qry)
  aux$period = paste0('2015',month)
  assign(paste0("hhld_2015",month,'_all'),aux)
}

hhld_all_2014 <- bind_rows(lapply(ls(pattern='hhld_2014'),get)) 

hhld_all_2014 = hhld_all_2014 %>% arrange(HHLD_ID,desc(period))
hhld_all_2014$num_na <- apply(hhld_all_2014[-1],1,function(x) sum(is.na(x)))
View(head(hhld_all_2014))
save(hhld_all_2014,file='hhld_all_2014.rdata')


hhld_all_2015Q1 <- bind_rows(lapply(ls(pattern='hhld_2015'),get)) 
save(hhld_all_2015Q1,file='Z:/M&T Projects/OCA/hhld_all_2015Q1.rdata')

#what we need is to match all to the latest month available, 
#for thsoe that have NAs, if a nearby month has less NAs maybe we should take those
#this is not going to be easy and it is not worth essting time on it, maybe just mesure how many 
#get NAs and manually see if those have less

