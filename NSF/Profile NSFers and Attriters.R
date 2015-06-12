#For the base, lets profile who NSFs, and who leaves and NSFs and the others

#I need to attach the hhld data and create the segments, stypes, tenure, age, clv bands, etc.
#I also need to create the prod penet )(or load it)

load("Z:/M&T Projects/M&T Way/hhld_201412.rdata")
load("Z:/M&T Projects/OCA/ifm_income_201312.rdata")
load("Z:/M&T Projects/IFM/accts_201312.rdata")
load("Z:/M&T Projects/M&T Way/accts_201412.rdata")
load('Z:/M&T Projects/NSF/hhld_201312.rdata')
load("Z:/M&T Projects/NSF/hhld_201301.rdata")
load('Z:/M&T Projects/NSF/accts_201301.rdata')

library(dplyr)
library(tidyr)

#create the product ownership balances and contrib  for 201312 and 201412, that will allow me to measure what was lost

accts_201312c$ACCT_PTYPE <- as.character(accts_201312c$ACCT_PTYPE)
dupesc <- which(accts_201312c$EXPRESSION_8 %in% accts_201312c$EXPRESSION_8[which(duplicated(accts_201312c$EXPRESSION_8))])
dupesa <- which(accts_201312a$EXPRESSION_8 %in% accts_201312a$EXPRESSION_8[which(duplicated(accts_201312a$EXPRESSION_8))])
dupesb <- which(accts_201312b$EXPRESSION_8 %in% accts_201312b$EXPRESSION_8[which(duplicated(accts_201312b$EXPRESSION_8))])

accts_201312c <- accts_201312c[-dupesc,]
accts_201312b <- accts_201312b[-dupesb,]
accts_201312a <- accts_201312a[-dupesa,]

accts_201312c <- left_join(accts_201312c,accts_201312a[c(2:3,15)],by='EXPRESSION_8')

prods_201312 <- accts_201312c %>% 
  mutate(ACCT_PTYPE=ifelse(ACCT_PTYPE=='CCS' & ACCT_STYPE %in% c('NOR','REW','SIG'),'CRD',ACCT_PTYPE)) %>% 
  group_by(ACCT_ID,ACCT_PTYPE) %>% 
  summarise(N=n_distinct(ACCT_PTYPE),bal=sum(ACCT_AMT_BAL_FOR_PRIME),contr=sum(ACCT_CONTR_NET_CONTRIBUTION_MTD)) %>%
  gather(measure,value,N:contr) %>% unite(key,ACCT_PTYPE,measure) %>% 
  mutate(key=gsub('_N','',key)) %>% spread(key,value,fill=NA)

prods_201312[,grep('_bal|_contr|ACCT_ID',names(prods_201312),invert=T)] <- ifelse(is.na(prods_201312[,grep('_bal|_contr|ACCT_ID',names(prods_201312),invert=T)]),0,1)




dupes14 <- which(accts_201412$EXPRESSION_8 %in% accts_201412$EXPRESSION_8[which(duplicated(accts_201412$EXPRESSION_8))])

accts_201412 <- accts_201412[-dupes14,]

accts_201412$ACCT_PTYPE <- as.character(accts_201412$ACCT_PTYPE)

prods_201412 <- accts_201412 %>% 
  mutate(ACCT_PTYPE=ifelse(ACCT_PTYPE=='CCS' & ACCT_STYPE %in% c('NOR','REW','SIG'),'CRD',ACCT_PTYPE)) %>% 
  group_by(ACCT_ID,ACCT_PTYPE) %>% 
  summarise(N=n_distinct(ACCT_PTYPE),bal=sum(ACCT_AMT_BAL_FOR_PRIME),contr=sum(ACCT_CONTR_NET_CONTRIBUTION_MTD)) %>% 
  gather(measure,value,N:contr) %>% unite(key,ACCT_PTYPE,measure) %>% 
  mutate(key=gsub('_N','',key)) %>% spread(key,value,fill=NA)

prods_201412[,grep('_bal|_contr|ACCT_ID',names(prods_201412),invert=T)] <- ifelse(is.na(prods_201412[,grep('_bal|_contr|ACCT_ID',names(prods_201412),invert=T)]),0,1)


dupes201301 <- which(accts_201301$EXPRESSION_8 %in% accts_201301$EXPRESSION_8[which(duplicated(accts_201301$EXPRESSION_8))])

accts_201301 <- accts_201301[-dupes201301,]
accts_201301$ACCT_PTYPE <- as.character(accts_201301$ACCT_PTYPE)

prods_201301 <- accts_201301 %>% 
  mutate(ACCT_PTYPE=ifelse(ACCT_PTYPE=='CCS' & ACCT_STYPE %in% c('NOR','REW','SIG'),'CRD',ACCT_PTYPE)) %>% 
  group_by(ACCT_ID,ACCT_PTYPE) %>% 
  summarise(N=n_distinct(ACCT_PTYPE),bal=sum(ACCT_AMT_BAL_FOR_PRIME),contr=sum(ACCT_CONTR_NET_CONTRIBUTION_MTD)) %>%
  gather(measure,value,N:contr) %>% unite(key,ACCT_PTYPE,measure) %>% 
  mutate(key=gsub('_N','',key)) %>% spread(key,value,fill=NA)

prods_201301[,grep('_bal|_contr|ACCT_ID',names(prods_201301),invert=T)] <- ifelse(is.na(prods_201301[,grep('_bal|_contr|ACCT_ID',names(prods_201301),invert=T)]),0,1)

##########

#merge the segments and other for 201312

#first I need the hhld id from accts_201301
length(setdiff(existing_book$EXPRESSION_8,accts_201301$EXPRESSION_8))  #most are
existing_book <- left_join(existing_book,accts_201301[c(1,5)])

existing_book <- left_join(existing_book,hhld_201301[c(1,70,68,69,72,54,19,15,33)],by=c('ACCT_ID'='HHLD_ID'))



#create factors
#code needs to be modified it was copied

existing_book$cbr <- factor(existing_book$HHLD_COMMUNITY_BANK_MARKET,levels=1:17,
                      labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA',
                               'SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))


existing_book$HHLD_LIFE_CYCLE_SEGMENT[existing_book$HHLD_LIFE_CYCLE_SEGMENT==8] <- 1
existing_book$HHLD_LIFE_CYCLE_SEGMENT[existing_book$HHLD_LIFE_CYCLE_SEGMENT==9] <- 4
existing_book$HHLD_LIFE_CYCLE_SEGMENT[is.na(existing_book$HHLD_LIFE_CYCLE_SEGMENT)] <- 7

existing_book$segment <- factor(existing_book$HHLD_LIFE_CYCLE_SEGMENT,levels=c(1:7),labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

existing_book$age <- cut(existing_book$HHLD_HH_OWN_AGE,c(0,seq(25,75,by=10),Inf))

existing_book$tenure <- as.numeric(round((as.Date('2013-01-01')-
                                             as.Date(existing_book$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y'))/(365.25),1))

existing_book$tenure <- (cut(existing_book$tenure,c(0,.0001,1,2,3,4,5,7,10,15,20,Inf)))

existing_book$clv_rem <- cut(existing_book$HHLD_CLV_REMAINING,c(0,0.0001,250,500,750,1000,2000,3000,4000,5000,10000,Inf))

existing_book$clv_tot <- cut(existing_book$HHLD_CLV_TOTAL,c(0,0.0001,250,500,750,1000,2000,3000,4000,5000,10000,Inf))
existing_book$clv_rem_ten <- cut(existing_book$HHLD_CLV_REMAINING_TENURE,c(0,0.0001,1,2,3,4,5,7,10,15,20,Inf))

#I now have to define teh groups, but also add the product penetrations for 201301 and 201312 
#4 groups NSFers that stayed, NSF left, no NSF stay and no nsf left

existing_book$group <- ifelse(existing_book$tot_num_2013==0 & existing_book$lost_2013==0,'No_retained',NA)
existing_book$group <- ifelse(existing_book$tot_num_2013==0 & existing_book$lost_2013==1,'No_lost',existing_book$group)
existing_book$group <- ifelse(existing_book$tot_num_2013>0 & existing_book$lost_2013==0,'NSF_retained',existing_book$group)
existing_book$group <- ifelse(existing_book$tot_num_2013>0 & existing_book$lost_2013==1,'NSF_lost',existing_book$group)

existing_book$nsf_events <- cut(existing_book$tot_num_2013,c(0,.001,1,2,3,4,5,10,15,20,Inf))
existing_book$nsf_events[is.na(existing_book$nsf_events)] <- '(0,0.001]'
existing_book$nsf_months <- cut(existing_book$months_2013,c(0,.001,1,2,3,4,5,9,12))
existing_book$nsf_months[is.na(existing_book$nsf_months)] <- '(0,0.001]'

#define RM
existing_book$rm <- ifelse(substring(existing_book$HHLD_HH_OWNER_CODE,4)!='00',"Yes","No")


#add ifm income
load('Z:/M&T Projects/NSF/ifm_income_201212.rdata')

existing_book <- left_join(existing_book,ifm_income_201212)
existing_book$income <- cut(existing_book$INTELLIGENTSIA_EARNED_EST_ANNUAL_INCOME,c(0,0.001,10000,20000,40000,60000,80000,100000,Inf),dig.labs=12)


profile_categoric <- existing_book %>% select(c(EXPRESSION_8,cbr:rm,income,group_13)) %>% 
  gather(variable,level,-c(EXPRESSION_8,group)) %>% group_by(group,variable,level) %>% summarise(N=n()) %>%
  group_by(group,variable) %>% mutate(P=N/sum(N)) %>% gather(measure,value,N:P) %>% 
  unite(key,group,measure,sep='_') %>% spread(key,value,fill=0) 

#melt the prods datasets add a year dimension, rbind, merge groups, crunch all at once, synch column names, 
#rbin with categoric, then move to excel 

aux_numeric_jan <- existing_book[c('ACCT_ID','EXPRESSION_8','group')]
aux_numeric_jan <- left_join(aux_numeric_jan,prods_201301,by='ACCT_ID')
aux_numeric_jan <- aux_numeric_jan %>% gather(variable,value,-c(ACCT_ID:group)) %>% 
  group_by(group,variable) %>% summarise(mean=mean(value,na.rm=T)) %>%mutate(period=201301)

aux_numeric_dec <- existing_book[c('ACCT_ID','EXPRESSION_8','group')]
aux_numeric_dec <- left_join(aux_numeric_dec,prods_201312,by='ACCT_ID')
aux_numeric_dec <- aux_numeric_dec %>% gather(variable,value,-c(ACCT_ID:group))  %>%
  group_by(group,variable) %>% summarise(mean=mean(value,na.rm=T)) %>% mutate(period=201312)


aux_numeric <- rbind(aux_numeric_dec,aux_numeric_jan)


prods_201301 <- 
prods_201312 <- left_join(prods_201312,existing_book[c('ACCT_ID','group')],by='ACCT_ID')




#generate all charts in a loop?
profile_products <- 