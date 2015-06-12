
#load adata
load("Z:/M&T Projects/OCA/oca_image_20150319.rdata")
load("Z:/M&T Projects/OCA/alerts.rdata")
load('Z:/M&T Projects/OCA/esvc_tables.rdata')

library(dplyr)
library(tidyr)
library(stringr)

#First I need to crucnh the alerts data for before and after so I can append it
names(alerts) <- c('acct','type','period','N')

alerts$type <- str_trim(alerts$type )
alerts_summary  <- alerts %>% filter(period %in% c(201401,201402,201501,201502)) %>% 
  group_by(acct,type, year=substr(period,1,4)) %>% summarise(count=sum(N)) %>% group_by(acct) %>%
  unite(key,type,year) %>% spread(key,count,fill=0) %>% 
  mutate(low_delta_act=LowBalance_2015-LowBalance_2014,avail_delta_act= AvailableBalanc_2015-AvailableBalanc_2014)



esvc201312$`E-SERVICE-DATE-ENROLLMENT` <- str_trim(as.character(esvc201312$`E-SERVICE-DATE-ENROLLMENT`))
esvc201412$`E-SERVICE-DATE-ENROLLMENT` <- str_trim(as.character(esvc201412$`E-SERVICE-DATE-ENROLLMENT`))

esvc201312$`E-SERVICE-DATE-CANCEL` <- str_trim(as.character(esvc201312$`E-SERVICE-DATE-CANCEL`))
esvc201412$`E-SERVICE-DATE-CANCEL` <- str_trim(as.character(esvc201412$`E-SERVICE-DATE-CANCEL`))

names(esvc201312)[5:6] <- c('enroll','cancel')
names(esvc201412)[5:6] <- c('enroll','cancel')

esvc201312$cancel[esvc201312$cancel==''] <- NA
esvc201412$cancel[esvc201412$cancel==''] <- NA

esvc201312$enroll[esvc201312$enroll==''] <- NA
esvc201412$enroll[esvc201412$enroll==''] <- NA

esvc201312$enroll <- as.Date(esvc201312$enroll,'%m/%d/%Y')
esvc201312$cancel <- as.Date(esvc201312$cancel,'%m/%d/%Y')
esvc201412$enroll <- as.Date(esvc201412$enroll,'%m/%d/%Y')
esvc201412$cancel <- as.Date(esvc201412$cancel,'%m/%d/%Y')

esvc201312$enrolled <- ifelse(!is.na(esvc201312$enroll) & (is.na(esvc201312$cancel) | esvc201312$enroll> esvc201312$cancel),1,0)
esvc201412$enrolled <- ifelse(!is.na(esvc201412$enroll) & (is.na(esvc201412$cancel) | esvc201412$enroll> esvc201412$cancel),1,0)

names(esvc201312)[8] <- 'type'
names(esvc201412)[8] <- 'type'
names(esvc201312)[12] <- 'acct'
names(esvc201412)[12] <- 'acct'

esvc201312$type <- str_trim(esvc201312$type)
esvc201412$type <- str_trim(esvc201412$type)

aux13 <- esvc201312 %>% filter(enrolled==1 & !is.na(acct)) %>% group_by(acct,type) %>% summarise(flag=1) %>% 
  spread(type,flag,fill=0) %>%
  rename(low_flag_2013 = LowBalance, available_flag_2013= AvailableBalance)

aux14 <- esvc201412 %>% filter(enrolled==1 & !is.na(acct)) %>% group_by(acct,type) %>% summarise(flag=1) %>% 
  spread(type,flag,fill=0) %>%
  rename(low_flag_2014 = LowBalance, available_flag_2014= AvailableBalance)

enrollment <- left_join(aux13,aux14,by='acct')

enrollment$available_flag_2014[is.na(enrollment$available_flag_2014)] <- 0
enrollment$low_flag_2014[is.na(enrollment$low_flag_2014)] <- 0
enrollment$low_delta_enroll <- enrollment$low_flag_2014 - enrollment$low_flag_2013
enrollment$avail_delta_enroll <- enrollment$available_flag_2014 - enrollment$available_flag_2013

alerts_summary1 <- inner_join(alerts_summary,enrollment,by='acct')


dda2 <- left_join(dda2,alerts_summary1,by=c('EXPRESSION_8'='acct'))

save(dda2,file='dda2.rdata')


#Now I want to know if the people who had NSF and such were more likely to enroll or not

low_bal_enroll <- dda2 %>% group_by(nsf_fee, low_delta_enroll) %>% summarise(N=n()) %>% group_by(nsf_fee) %>%
  mutate(P=N/sum(N),tot=sum(N)) %>% gather(var,value,N:tot) %>% unite(key,var,nsf_fee) %>% spread(key,value,fill=0)

avail_enroll <- dda2 %>% group_by(nsf_fee, avail_delta_enroll) %>% summarise(N=n()) %>% group_by(nsf_fee) %>%
  mutate(P=N/sum(N),tot=sum(N)) %>% gather(var,value,N:tot) %>% unite(key,var,nsf_fee) %>% spread(key,value,fill=0)



write.table(low_bal_enroll,'clipboard-128',sep='\t',row.names=F)
write.table(avail_enroll,'clipboard-128',sep='\t',row.names=F)


pairwise.prop.test(unlist(c(avail_enroll[3,3:5])),unlist(c(avail_enroll[3,11:13])),p.adjust.method = 'n',alternative='g')

pairwise.prop.test(unlist(c(low_bal_enroll[3,3:5])),unlist(c(low_bal_enroll[3,11:13])),p.adjust.method = 'n',alternative='g')

enrollment_rate1 <- dda2 %>% group_by(nsf_fee, low_flag_2014) %>% summarise(N=n()) %>% group_by(nsf_fee) %>%
  mutate(P=N/sum(N),tot=sum(N)) %>% gather(var,value,N:tot) %>% unite(key,var,low_flag_2014) %>% spread(key,value,fill=0)

enrollment_rate2 <- dda2 %>% group_by(nsf_fee, available_flag_2014) %>% summarise(N=n()) %>% group_by(nsf_fee) %>%
  mutate(P=N/sum(N),tot=sum(N)) %>% gather(var,value,N:tot) %>% unite(key,var,available_flag_2014) %>% spread(key,value,fill=0)

write.table(enrollment_rate1,'clipboard-128',sep='\t',row.names=F)
write.table(enrollment_rate2,'clipboard-128',sep='\t',row.names=F)

pairwise.prop.test(unlist(c(enrollment_rate1$N_1)),unlist(c(enrollment_rate1$tot_1)),p.adjust.method = 'n',alternative='g')
pairwise.prop.test(unlist(c(enrollment_rate2$N_1)),unlist(c(enrollment_rate2$tot_1)),p.adjust.method = 'n',alternative='g')


rm(alerts)
