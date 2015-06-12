for (year in c('2014')) {
  for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    if (month %in% c('01','02')) {
      d = read.table(paste0('O:/MNT/MNT - Mario//ACCT_',year,month,'.txt'),sep='\t',header=T,stringsAsFactors=F)
    }
    else  {
      d = read.table(paste0('O:/MNT/MNT - Mario//ACCT_',year,month,'.txt'),sep='\t',header=F,stringsAsFactors=F)
      names(d) = names(nsf_extra_201401)[1:14]
    }
    d$period = paste0(year,month)
    assign(paste0('nsf_extra_',year,month),d)
  }
}



nsf_extra_14 <- bind_rows(lapply(ls(pattern='nsf_extra'),get))
save(nsf_extra_14,file='Z:/M&T Projects/OCA/nsf_extra_14.rdata')



#I am having issues COMBINNG the files that Hiran extracted because some have no header, and maybE OTEHR RESOSNS

library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')
a = "SELECT  EXPRESSION_8,ACCT_STYPE, ACCT_DATE_OPENED, ACCT_REG_E_FLAG_CUR,  ACCT_REG_E_DATE_CUR, ACCT_REG_E_FLAG_PRIOR1, ACCT_REG_E_DATE_PRIOR1 ,ACCT_REG_E_FLAG_PRIOR2, ACCT_REG_E_DATE_PRIOR2  from  iqrmt.dbo.ACCT_201412 where ACCT_STATUS_FOR_PRIME <>'X' and left(ACCT_STYPE,1)='R' AND right(ACCT_DATE_OPENED,4) = '2014' and ACCT_PTYPE='DDA' "

optin_data = sqlQuery(mtdata,a)
save(optin_data,file='Z:/M&T Projects/OCA/optin_data_201412.rdata')

b = "SELECT EXPRESSION_8, ACCT_NSF_TOTAL_CHECK ,ACCT_NSF_TOTAL_OTHER from IQRMT.dbo.ACCT_2014"
b1 = "where left(ACCT_STYPE,1)='R' and ACCT_PTYPE='DDA'"

for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  d = sqlQuery(mtdata,paste0(b,month,' ',b1))
  d$period = paste0('2014',month)
  assign(paste0('nsf_extra_','2014',month),d)
}


nsf_extra_14 <- bind_rows(lapply(ls(pattern='nsf_extra'),get))
save(nsf_extra_14,file='Z:/M&T Projects/OCA/nsf_extra_14.rdata')



for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  d = sqlQuery(mtdata,paste0("SELECT EXPRESSION_8, ACCT_CQI_OVERDRAFT from IQRMT.DBO.ACCT_2014",month," where ACCT_PTYPE = 'DDA' "))
  d$period = paste0('2014',month)
  assign(paste0('od_','2014',month),d)
}

od_extra_14 <- bind_rows(lapply(ls(pattern='od_'),get))
save(od_extra_14,file='Z:/M&T Projects/OCA/od_extra_14.rdata')


for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
  d = sqlQuery(mtdata,paste0("SELECT EXPRESSION_8, ACCT_REG_E_FLAG_CUR from IQRMT.DBO.ACCT_2014",month," where ACCT_PTYPE = 'DDA' "))
  d$period = paste0('2014',month)
  assign(paste0('rege_','2014',month),d)
}

rege_extra_14 <- bind_rows(lapply(ls(pattern='rege_'),get))
save(rege_extra_14,file='Z:/M&T Projects/OCA/rege_extra_14.rdata')
