#we also want to see if after a complait people becam einactive
#for that I want to define activity somehow, the classify before and after and compare the rate of change
#between the groups
#more analysis can also be doen later

#load the data on withdrawals and deposits
load('activity.rdata')

#analyze 201401 to understand what is active

trans_201401$ACCT_NUMBER_DEBITS_MTD[is.na(trans_201401$ACCT_NUMBER_DEBITS_MTD)] <- 0
trans_201401$ACCT_NUMBER_OF_DEPOSITS_MTD[is.na(trans_201401$ACCT_NUMBER_OF_DEPOSITS_MTD)] <- 0
trans_201412$ACCT_NUMBER_DEBITS_MTD[is.na(trans_201412$ACCT_NUMBER_DEBITS_MTD)] <- 0
trans_201412$ACCT_NUMBER_OF_DEPOSITS_MTD[is.na(trans_201412$ACCT_NUMBER_OF_DEPOSITS_MTD)] <- 0

bands <- c(-0.1,1,2,3,4,5,10,Inf)
ggplot(trans_201401,aes(x=cut(ACCT_NUMBER_DEBITS_MTD,bands,dig.lab = 10),y=cut(ACCT_NUMBER_OF_DEPOSITS_MTD,bands,dig.lab = 10),color=factor(ACCT_CQI_DIRECT_DEPOSIT)))+geom_jitter(alpha=0.1)+scale_fill_discrete(c('red',"green"))

table(cut(trans_201401$ACCT_NUMBER_DEBITS_MTD,bands,dig.lab = 10),cut(trans_201401$ACCT_NUMBER_OF_DEPOSITS_MTD,bands,dig.lab = 10),trans_201401$ACCT_CQI_DIRECT_DEPOSIT,dnn=c('debits','deposits','DD'))

table(cut(trans_201401$ACCT_NUMBER_OF_DEPOSITS_MTD,bands,dig.lab = 10),trans_201401$ACCT_CQI_DIRECT_DEPOSIT)


with(trans_201401,table( cut(ACCT_NUMBER_DEBITS_MTD[ACCT_NUMBER_OF_DEPOSITS_MTD>=1],bands)))
#most people with at least one deposit have 5 or more debits (in broader sense) (77%), 80% 3 or more
#only 9% had 0 and 17% up to 2 - lets assume 5 to be sure they are active

#I will define as active people with direct 1+ deposits anf 5+ debits
with(trans_201401,sum(ACCT_NUMBER_OF_DEPOSITS_MTD>=1 & ACCT_NUMBER_DEBITS_MTD>=3))/dim(trans_201401)[1]  #75% are activer whihc is about my guess


#define activity 
trans_201401$active_201401 <- ifelse(trans_201401$ACCT_NUMBER_OF_DEPOSITS_MTD>0 & trans_201401$ACCT_NUMBER_DEBITS_MTD>=5,1,0)
table(trans_201401$active,useNA='ifany')

trans_201412$active_201412 <- ifelse(trans_201412$ACCT_NUMBER_OF_DEPOSITS_MTD>0 & trans_201412$ACCT_NUMBER_DEBITS_MTD>=5,1,0)
table(trans_201412$active,useNA='ifany')



#Now merge it to the bid dataset

dda2 <- left_join(dda2,trans_201401[,c('EXPRESSION_8','active_201401')])
dda2 <- left_join(dda2,trans_201412[,c('EXPRESSION_8','active_201412')])

dda2$active_delta <- ifelse(!is.na(dda2$active_201412) & !is.na(dda2$active_201401),dda2$active_201412-dda2$active_201401,NA)
dda2$active_delta <- factor(dda2$active_delta,c(-1,0,1),labels=c('to_inactive','same','to_active'))

activity <- dda2 %>% filter(!is.na(active_delta)) %>% group_by(nsf_fee,active_delta) %>% summarise(N=n()) %>% 
  group_by(nsf_fee) %>% mutate(P=N/sum(N)) %>% gather(var,value,N:P) %>% 
  unite(key,var,active_delta) %>% spread(key,value) %>% mutate(tot=N_to_active+N_to_inactive+N_same)
row.names(activity) <- activity$nsf_fee

less <- pairwise.prop.test(activity$N_to_inactive,activity$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)
more <- pairwise.prop.test(activity$N_to_active,activity$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)


across <- (sapply(1:4,function(x) prop.test(c(activity$N_to_inactive[x],activity$N_to_active[x]),c(activity$tot[x],activity$tot[x]),alternative = 'g')$p.value))
across1 <- p.adjust(across,method='holm')

write.table(activity,'clipboard-128',sep='\t',row.names=F)

write.table(less$p.value,'clipboard-128',sep='\t',row.names=T)
write.table(more$p.value,'clipboard-128',sep='\t',row.names=T)
write.table(across1,'clipboard-128',sep='\t',row.names=T)


# Iwant to get data for teh activity by level
write.table(table(dda2$active_201401,dda2$nsf_fee),'clipboard-128',sep='\t',row.names=T)

#clean-up uneeded tables
rm(trans_201401,trans_201412)


#I also want to see if they decreased debit activity,likely from 1Q 2014 to 4Q 2014


load("Z:/M&T Projects/NSF/debit_all.rdata")

debit_qtr <- debit_all %>% filter(period %in% c('201310','201311','201312','201410','201411','201412')) %>% 
  group_by(dda,year=paste0('debit_4q_',substr(period,1,4))) %>% summarise(num=sum(num,na.rm=T),amt=sum(amt,na.rm=T)) %>%
  gather(var,value,num:amt) %>% unite(key,year,var) %>% spread(key,value,fill=0) 

debit_qtr <- debit_qtr %>% group_by(dda) %>% 
  mutate(change_num = (debit_4q_2014_num/debit_4q_2013_num) -1 , change_amt=(debit_4q_2014_amt/debit_4q_2013_amt)-1)

#some accts weirdlyhave trans and no amt, this is illogical so I will exclude them
debit_qtr <- debit_qtr %>% filter(!(debit_4q_2013_amt==0 & debit_4q_2013_num>0) & !(debit_4q_2014_amt==0 & debit_4q_2014_num>0) )

sum(is.na(debit_qtr$change_num))
sum(is.na(debit_qtr$change_amt))
bands1 = c(-1.000000001,-.5,-.25,-.1,.1,.25,.5,1,Inf)
names1<-c('lost_50_to_100p','lost_25_to_50p','lost_10_to_25p','within_10p','incr_10_to_25p','incr_25_to_50p','incr_50_to_100p','incr_over_100p')
debit_qtr$delta_num <- cut(debit_qtr$change_num,bands1,labels=names1,include.lowest = T)
debit_qtr$delta_amt <- cut(debit_qtr$change_amt,bands1,labels=names1,include.lowest = T)


#merge with dda2
length(intersect(dda2$EXPRESSION_8,debit_qtr$dda))
dda2 <- left_join(dda2,debit_qtr[,c('dda','delta_num','delta_amt')],by=c('EXPRESSION_8'='dda'))

rm(debit_all)

#crucnh

debit_num <- dda2 %>% filter(!is.na(delta_num)) %>% group_by(nsf_fee,delta_num) %>% summarise(N=n()) %>% 
  group_by(nsf_fee) %>% mutate(P=N/sum(N)) %>% gather(var,value,N:P) %>% 
  unite(key,var,delta_num) %>% spread(key,value,fill=0) 
debit_num$tot <- rowSums(debit_num[which(grepl('^N_',names(debit_num)))])
row.names(activity) <- activity$nsf_fee


debit_amt <- dda2 %>% filter(!is.na(delta_amt)) %>% group_by(nsf_fee,delta_amt) %>% summarise(N=n()) %>% 
  group_by(nsf_fee) %>% mutate(P=N/sum(N)) %>% gather(var,value,N:P) %>% 
  unite(key,var,delta_amt) %>% spread(key,value,fill=0) 
debit_amt$tot <- rowSums(debit_amt[which(grepl('^N_',names(debit_amt)))])


write.table(debit_num,'clipboard-128',sep='\t',row.names=F)

lost_50_to_100p <- pairwise.prop.test(debit_num$N_lost_50_to_100p,debit_num$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)
write.table(lost_50_to_100p$p.value,'clipboard-128',sep='\t',row.names=T)

lost_25_to_50p <- pairwise.prop.test(debit_num$N_lost_25_to_50p,debit_num$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)
write.table(lost_25_to_50p$p.value,'clipboard-128',sep='\t',row.names=T)

lost_10_to_25p <- pairwise.prop.test(debit_num$N_lost_10_to_25p,debit_num$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)
write.table(lost_10_to_25p$p.value,'clipboard-128',sep='\t',row.names=T)


#thr results are inconsistent with the other results, as now I see no change, likely due to the different periods
#will redo by month jan to dec


debit_mth <- debit_all %>% filter(period %in% c('201401','201412')) %>% 
  group_by(dda,year=paste0('debit_',period)) %>% summarise(num=sum(num,na.rm=T),amt=sum(amt,na.rm=T)) %>%
  gather(var,value,num:amt) %>% unite(key,year,var) %>% spread(key,value,fill=0) 

debit_mth <- debit_mth %>% group_by(dda) %>% 
  mutate(change_num = (debit_201412_num/debit_201401_num) -1 , change_amt=(debit_201412_amt/debit_201401_amt)-1)


debit_mth <- debit_mth %>% filter(!(debit_201401_amt==0 & debit_201401_num>0) & !(debit_201412_amt==0 & debit_201412_num>0) )

sum(is.na(debit_mth$change_num))
sum(is.na(debit_mth$change_amt))

debit_mth$delta_num1 <- cut(debit_mth$change_num,bands1,labels=names1,include.lowest = T)
debit_mth$delta_amt1 <- cut(debit_mth$change_amt,bands1,labels=names1,include.lowest = T)


#merge with dda2
length(intersect(dda2$EXPRESSION_8,debit_mth$dda))
dda2 <- left_join(dda2,debit_mth[,c('dda','delta_num1','delta_amt1')],by=c('EXPRESSION_8'='dda'))

rm(debit_all)


debit_num_mth <- dda2 %>% filter(!is.na(delta_num1)) %>% group_by(nsf_fee,delta_num1) %>% summarise(N=n()) %>% 
  group_by(nsf_fee) %>% mutate(P=N/sum(N)) %>% gather(var,value,N:P) %>% 
  unite(key,var,delta_num1) %>% spread(key,value,fill=0) 

debit_num_mth$tot <- rowSums(debit_num_mth[which(grepl('^N_',names(debit_num_mth)))])


write.table(debit_num_mth,'clipboard-128',sep='\t',row.names=F)


debit_num_mth1 <- dda2 %>% filter(!is.na(delta_num1) & active_delta=='same' & active_201401==1) %>% group_by(nsf_fee,delta_num1) %>% summarise(N=n()) %>% 
  group_by(nsf_fee) %>% mutate(P=N/sum(N)) %>% gather(var,value,N:P) %>% 
  unite(key,var,delta_num1) %>% spread(key,value,fill=0) 

debit_num_mth1$tot <- rowSums(debit_num_mth1[which(grepl('^N_',names(debit_num_mth)))])
write.table(debit_num_mth1,'clipboard-128',sep='\t',row.names=F)

lost_50_to_100p2 <- pairwise.prop.test(debit_num_mth1$N_lost_50_to_100p,debit_num_mth$tot,p.adjust.method = 'holm',alternative='g',conf.level=0.9)
write.table(lost_50_to_100p2$p.value,'clipboard-128',sep='\t',row.names=T)

