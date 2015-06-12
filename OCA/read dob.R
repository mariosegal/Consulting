library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrusa-db2;trusted_connection=true')

#who knows when the date of birth is on V2 or not, I have to try all options to account for 
#the lack of consistency
a1= 'SELECT EXPRESSION_8, ACCT_DATE_OF_BIRTH from '
for (year in c('2014')) {
  for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    table1a = paste0("IQRMT.dbo.ACCT_",year,month,"_V2")
    qry_a <- paste(a1,table1a)
    data <- sqlQuery(mtdata,qry_a)
    if (class(data) == 'data.frame') {
      data$period = paste0(year,month)
      assign(paste0('dob_',year,month),data)
    }
  }
}

for (year in c('2014')) {
  for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    table1a = paste0("IQRMT.dbo.ACCT_",year,month)
    qry_a <- paste(a1,table1a)
    data <- sqlQuery(mtdata,qry_a)
    if (class(data) == 'data.frame') {
      data$period = paste0(year,month)
      assign(paste0('dob_',year,month),data)
    }
  }
}



dob_2014 <- bind_rows(lapply(ls(pattern='dob'),get)) 
library(dplyr)

dob_2014_clean = dob_2014[-which(duplicated(dob_2014$EXPRESSION_8)),]
sum(duplicated(dob_2014_clean$EXPRESSION_8))

save(dob_2014_clean,file="Z:/M&T Projects/OCA/dob_2014_clean.rdata")


a1= 'SELECT EXPRESSION_8, ACCT_DATE_OF_BIRTH from '
for (year in c('2015')) {
  for (month in c('01','03')) {
    table1a = paste0("IQRMT.dbo.ACCT_",year,month)
    qry_a <- paste(a1,table1a)
    data <- sqlQuery(mtdata,qry_a)
    data$period = paste0(year,month)
    assign(paste0('dob_',year,month),data)
  }
}

month = '02'
table1a = paste0("IQRMT.dbo.ACCT_",year,month,"_V2")
qry_a <- paste(a1,table1a)
data <- sqlQuery(mtdata,qry_a)
data$period = paste0(year,month)
assign(paste0('dob_',year,month),data)



dob_2014_2015Q1_clean = bind_rows(lapply(ls(pattern='dob_201'),get)) 
dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH = as.character(dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH)
dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH =str_trim(dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH)
dob_2014_2015Q1_clean = dob_2014_2015Q1_clean %>% arrange(EXPRESSION_8,ACCT_DATE_OF_BIRTH)  
#get rid of any dupes - taking the earliest one
dob_2014_2015Q1_clean = dob_2014_2015Q1_clean[-which(duplicated(dob_2014_2015Q1_clean[1:2])),]
dob_2014_2015Q1_clean = dob_2014_2015Q1_clean %>% arrange(EXPRESSION_8,desc(ACCT_DATE_OF_BIRTH))
#we want to take the one with a date if we have with and without
#given the sort jus take unique
dob_2014_2015Q1_clean = dob_2014_2015Q1_clean[-which(duplicated(dob_2014_2015Q1_clean$EXPRESSION_8)),]
sum(dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH=="")  #8% missing 
dob_2014_2015Q1_clean = dob_2014_2015Q1_clean[-3]

save(dob_2014_2015Q1_clean,file="Z:/M&T Projects/OCA/dob_2014_2015Q1_clean.rdata")


bals_201503 = sqlQuery(mtdata,"SELECT EXPRESSION_8, ACCT_AMT_BAL_FOR_PRIME from iqrmt.dbo.ACCT_201503")
save(bals_201503,file="Z:/M&T Projects/OCA/bals_201503.rdata")
