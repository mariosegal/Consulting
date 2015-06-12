load("Z:/M&T Projects/OCA/dda2.rdata")
load("Z:/M&T Projects/M&T Way/dob_201412.rdata")

library(dplyr)
library(tidyr)
library(scales)

dda2 <- left_join(dda2,dob_201412[-1])
dda2$age1 <- cut(2015-as.numeric(format(dda2$ACCT_DATE_OF_BIRTH_201412,'%Y')),c(0,seq(20,45,by=5),c(55,65,75),Inf))

dda2 %>% group_by(age1,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))

dda2 %>% group_by(segment,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))


dda2 %>% group_by(tenure,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))

stype <- dda2 %>% group_by(ACCT_STYPE,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))
write.table(stype,'clipboard-128',sep='\t',row.names=F)

dda2 %>% group_by(cbr,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=((`Yes`) / (`Yes`+`No with Fee`)))

dda2 %>% group_by(ACCT_REG_E_FLAG_CUR_201412,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=((`Yes`) / (`Yes`+`No with Fee`)))

load('Z:/M&T Projects/OCA/debit_3q2014.rdata')
dda2 <- left_join(dda2,debit_3q2014,by=c('EXPRESSION_8'='dda'))
dda2$debit_num[is.na(dda2$debit_num)] <- 0
dda2$debit_num1 <- cut(dda2$debit_num,c(-0.01,.01,1,2,5,10,15,20,Inf),include.lowest = F)

  
dda2 %>% group_by(debit_num1,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))

load("Z:/M&T Projects/NSF/contrib.rdata")
waived <- contrib %>% filter(period >= '201401') %>% group_by(EXPRESSION_8) %>% summarise_each(funs(sum(.,na.rm=T)),ACCT_CONTR_TOTAL_NSF_FEES:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED) %>% filter(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED>0 )  %>% mutate(flag_waive = 1)
rm(contrib)

dda2 <- left_join(dda2,waived[-2])
dda2$flag_waive[is.na(dda2$flag_waive)] <- 0
dda2$ACCT_CONTR_TOTAL_NSF_FEES_WAIVED[is.na(dda2$ACCT_CONTR_TOTAL_NSF_FEES_WAIVED)] <- 0

dda2 %>% group_by(flag_waive,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))

dda2$waiver <- cut(dda2$ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,c(0,0.01,1:10*38.5,Inf),dig.labs=8,include.lowest = T)

dda2 %>% group_by(waiver,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(nsf_rate = percent((`Yes`+`No with Fee`) / (`Yes`+`No with Fee` + `No NSF Fee`)),compl_rate=percent((`Yes`) / (`Yes`+`No with Fee`)))
