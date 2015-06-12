load("Z:/M&T Projects/OCA/dda2_small.rdata")
load("Z:/M&T Projects/NSF/contrib.rdata")
load("Z:/M&T Projects/OCA/complaints.rdata")

library(dplyr)
library(tidyr)
library(stringr)

waived <- contrib %>% filter(period >= '201401') %>% group_by(EXPRESSION_8) %>% summarise_each(funs(sum(.,na.rm=T)),ACCT_CONTR_TOTAL_NSF_FEES:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED) %>% filter(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED>0 )  %>% mutate(flag_waive = 1)

dda2b <- left_join(dda2a,waived,by='EXPRESSION_8') %>% mutate(flag_waive=ifelse(is.na(flag_waive),0,flag_waive),
                                                              ACCT_CONTR_TOTAL_NSF_FEES_WAIVED=ifelse(is.na(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),0,ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),
                                                              waiver_rate=ACCT_CONTR_TOTAL_NSF_FEES_WAIVED/ACCT_CONTR_TOTAL_NSF_FEES.x)
dda2b

my_test <- function(a,b,c,d) prop.test(c(a,b),c(c,d),alternative='g')$p.value


#does waiver make a difference
waiver1 <- dda2b %>% group_by(nsf_fee,flag_waive) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(flag_waive) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 
                                                              
                                                              
write.table(waiver1,'clipboard-128',sep='\t',row.names=F)


#does the amount of the waiver matter
dda2b$waiver <- cut(dda2b$ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,c(0,0.01,1:10*38.5,Inf),dig.labs=8,include.lowest = T)

waiver2 <- dda2b %>% group_by(nsf_fee,waiver) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(waiver) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 


write.table(waiver2,'clipboard-128',sep='\t',row.names=F)


#look at the department
dda2b <- left_join(dda2b,complaints[c(1,18:20)],by='complaint_master_id')
dda2b$department <- as.character(dda2b$department)
dda2b$department <- str_trim(dda2b$department)
dda2b$department <- ifelse(dda2b$department=='' & !is.na(dda2b$branch),'branch',dda2b$department)


departmen1 <- dda2b  %>% group_by(flag_waive,department) %>% summarise(N=n()) %>% spread(flag_waive,N,fill=0) %>% rename(No=`0`,Yes=`1`) %>% mutate(tot=Yes+No,rate=Yes/tot,Yes1=sum(Yes),tot1=sum(tot)) %>% group_by(department) %>% mutate(p=my_test(Yes,Yes1,tot,tot1)) 
#%>% filter(nsf_fee=='Yes')
write.table(departmen1,'clipboard-128',sep='\t',row.names=F)


#validate teh waiver rate
tmp <- contrib %>% group_by(EXPRESSION_8,year=substr(period,1,4)) %>% 
  summarise_each(funs(sum(.,na.rm=T)),ACCT_CONTR_TOTAL_NSF_FEES:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED) %>% 
  filter(ACCT_CONTR_TOTAL_NSF_FEES>0 | ACCT_CONTR_TOTAL_NSF_FEES_WAIVED > 0)

tmp %>% group_by(year) %>% filter(ACCT_CONTR_TOTAL_NSF_FEES>0 ) %>%
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),wvd=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),N=n(),waived=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED>0)) %>%
  mutate(ratw1 = wvd/fees,rate2=waived/N)
