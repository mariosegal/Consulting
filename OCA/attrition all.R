#Analyze the selected group
#checking and NSF
data_dda <- complaints %>% filter( level_2=='Checking account')
dim(data_dda)[1]
sum(is.na(data_dda$mask_acct) & is.na(data_dda$mask_ssn))
data_dda1 <- filter(data_dda,!is.na(mask_acct) | !is.na(mask_ssn))
sum(!is.na(data_dda1$mask_acct))
sum(is.na(data_dda1$mask_acct))


#part1 - do the ones where I have an acct
part1_dda <- subset(data_dda1,!is.na(mask_acct),select=c('mask_acct','complaint_master_id'))
sum(duplicated(part1_dda$mask_acct))
length(unique(part1_dda$mask_acct))

match1_dda <- inner_join(part1_dda,accts_201312_oca,by=c('mask_acct'='EXPRESSION_8'))
length(unique(match1_dda$ACCT_ID))
sum(duplicated(match1_dda$ACCT_ID))

sum(match1_dda$mask_acct %in% accts_201312_oca$EXPRESSION_8)  #validate nothing fishy
sum(!(match1_dda$mask_acct %in% accts_201312_oca$EXPRESSION_8))  #not in dec 2013


#part2 - try the ones with ssn
part2_dda <- subset(data_dda1,is.na(mask_acct),select=c('mask_ssn','complaint_master_id'))
sum(duplicated(part2_dda$mask_ssn))
length(unique(part2_dda$mask_ssn))
sum(duplicated(part2_dda$complaint_master_id))

match2_dda <- inner_join(part2_dda,accts_201312_oca,by=c('mask_ssn'='EXPRESSION_18'))
part2_dda$mask_acct <- part2_dda$EXPRESSION_8


#how many matched multiple times by SSN - those are the ones that have duped maSTER_ID AS WE HAD ZERO BEFORE
length(unique(match2_dda$complaint_master_id)) # this are the number matched
length(setdiff(part2_dda$complaint_master_id,match2_dda$complaint_master_id))  #not matched by SSN

dupes2_dda <- match2_dda$complaint_master_id[which(duplicated(match2_dda$complaint_master_id))]
length(unique(match2_dda$complaint_master_id[!(match2_dda$complaint_master_id %in% dupes2_dda)])) #one match
length(unique(match2_dda$complaint_master_id[(match2_dda$complaint_master_id %in% dupes2_dda)]))  #multiple

keep2_dda <- match2_dda[!(match2_dda$complaint_master_id %in% dupes2_dda),]  #it is impossible to analyze the accts for multiple ones


drop2_dda <- match2_dda$EXPRESSION_8[(match2_dda$complaint_master_id %in% dupes2_dda)]

accts1_dda <- bind_rows(keep2_dda,match1_dda)  #this is the acct level analysis dataset

accts1_dda$mask_acct[is.na(accts1_dda$mask_acct)] <- accts1_dda$EXPRESSION_8[is.na(accts1_dda$mask_acct)]

#how many are duplicated on both acct and level
accts1_dda <- left_join(accts1_dda,complaints[,-which(names(complaints) %in% c("mask_acct","mask_ssn"))])
accts1_dda$level_3 <- as.factor(as.character(accts1_dda$level_3))
accts1_dda <- subset(accts1_dda, complaint_date >= '2014-04-01')
accts1_dda$period <- (format(accts1_dda$complaint_date,'%Y%m'))

sum(duplicated(accts1_dda[c("level_3","mask_acct",'period')]))
accts1_dda <- accts1_dda[order(accts1_dda$mask_acct,accts1_dda$level_3,accts1_dda$complaint_date),]
bad <- which(duplicated(accts1_dda[c("level_3","mask_acct",'period')]))

#drop the dupes
accts2_dda <- accts1_dda[-bad,]
sum(duplicated(accts2_dda[c("level_3","mask_acct",'period')]))


stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Interest','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking Account','Retail Portfolio Management Account','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
accts2_dda$ACCT_STYPE <- factor(accts2_dda$ACCT_STYPE ,levels=stypes,labels=packages)


#now merge a flag for those present in 201502
load("Z:/M&T Projects/NSF/accts_201502.rdata")
accts2_dda$left_201502 <- ifelse(accts2_dda$mask_acct %in% accts_201502$EXPRESSION_8[accts_201502$ACCT_PTYPE=='DDA'],0,1)


attr_level3 <- accts2_dda %>% group_by(period,level_3) %>% summarise(rate=mean(left_201502)) %>% spread(level_3,rate)
write.table(attr_level3,'clipboard-128',sep='\t',row.names=F)

attr_level3_N <- accts2_dda %>% group_by(period,level_3) %>% summarise(N=n()) %>% spread(level_3,N)
write.table(attr_level3_N,'clipboard-128',sep='\t',row.names=F)


# I want to get a baseline and also be able to do 6 month, 9 months
# for that I need to add the other accounts as I did before, and also
# mark the closed accts
# by being samrt about the numbering of months apr14 = 1, may14 = 2, ...), the month the left can be calculated and 
#all aggregated at once

load("Z:/M&T Projects/OCA/closed_2015_jan_feb.rdata")
load("Z:/M&T Projects/OCA/accts_201312_oca.rdata")

extra <-accts_201312_oca %>% filter(ACCT_PTYPE=='DDA' & !(EXPRESSION_8 %in% accts2_dda$mask_acct ) & !(ACCT_ID %in% accts2_dda$ACCT_ID))  #we only want accounts with no complaints, and also not on the same hhlds we took

intersect(extra$EXPRESSION_8 , accts2_dda$mask_acct)  #as expected

extra$level_3 <- 'no_complaint'   #for the merge
extra$mask_acct <- extra$EXPRESSION_8
extra$period <- '201404' # make them all present in 201404 as they were

attrition_all <- bind_rows(accts2_dda,extra)
sum(duplicated(attrition_all[c("mask_acct",'level_3','period')]))  #there shouls be no dupes at these levels

#merge the closed period
closed_201501$period <- as.character(closed_201501$period )
closed_201502$period <- as.character(closed_201502$period )

load("Z:/M&T Projects/NSF/closed_accts.rdata")

closedx <- bind_rows(closed_accts[closed_accts$period >= '201401',],closed_201501,closed_201502)
sum(duplicated(closedx$EXPRESSION_8))
closed_dupes <- subset(closedx, EXPRESSION_8 %in% closedx$EXPRESSION_8[which(duplicated(closedx$EXPRESSION_8))])
View(closed_dupes[order(closed_dupes$EXPRESSION_8,closed_dupes$period),])
#they seem continuous, take lowest one

closedx <- closedx[order(closedx$EXPRESSION_8,closedx$period),]
closedx <- closedx[-which(duplicated(closedx$EXPRESSION_8)),]
sum(duplicated(closedx$EXPRESSION_8))
names(closedx)[2] <- 'closed_period'

attrition_all <- left_join(attrition_all,closedx,by=c('mask_acct'='EXPRESSION_8'))

rm(closed_accts)

#finally add the nsf data, as we will need it to do the NSF baseline
#get it from attriton_data

attrition_all <- left_join(attrition_all,attrition_data[c('EXPRESSION_8','ACCT_CONTR_TOTAL_NSF_FEES','ACCT_NSF_TOTAL')],by=c('mask_acct'='EXPRESSION_8'))

table(attrition_all$closed_period)  #some accts were closed in 201401, 201402 or 201403, I need to take those out

attrition_all <- subset(attrition_all,is.na(closed_period) | closed_period >= '201404' )

period_aux <- data.frame(closed_period = names(table(attrition_all$closed_period)),closed_aux = 1:11,stringsAsFactors=F)
period_aux1 <- period_aux
names(period_aux1) <- c('period','period_aux')

#Now do the relabeling of periods, calc the lost interval and  and then crunch

attrition_all <- left_join(attrition_all,period_aux)
attrition_all <- left_join(attrition_all,period_aux1)


attrition_all$when <- ifelse(!is.na(attrition_all$closed_aux),attrition_all$closed_aux-attrition_all$period_aux,-99)
table(attrition_all$when,useNA='ifany')
#some accounts closed before the compalint,  221 - for simplicity lets drop them

attrition_all <- subset(attrition_all, when>=0 | when == -99)
attrition_all$lost <- ifelse(attrition_all$when==-99,0,1)



#finally the crunching

attrit_summ_all <- attrition_all %>%  group_by(level_3,period,when) %>% summarise(lost=sum(lost),N=n()) %>% 
  group_by(level_3,period) %>% mutate(base=sum(N),lost_cumm=cumsum(lost),rate=lost_cumm/base) 
write.table(attrit_summ_all,'clipboard-128',sep='\t',row.names=F)
#attrit_summ_all is by period and when, but I want to aggregate why when

attrit_summ_all1 <- attrit_summ_all %>% group_by(level_3,when) %>% 
  summarise(base=sum(base),lost_cumm=sum(lost_cumm)) %>% mutate(rate=lost_cumm/base)

write.table(attrit_summ_all1,'clipboard-128',sep='\t',row.names=F)

attrit_summ_all2 <- attrit_summ_all1 %>% select(-c(base,lost_cumm)) %>% filter(when %in% c(3,6,9)) %>% 
  group_by(level_3) %>% mutate(when1=paste0(when,'m')) %>% select(-when) %>% spread(when1,rate)

write.table(attrit_summ_all2,'clipboard-128',sep='\t',row.names=F)

attrit_summ_all2_extra <- attrit_summ_all1  %>% filter(when %in% c(3,6,9)) %>% 
  group_by(level_3) %>% mutate(when1=paste0(when,'m')) %>% select(-when) %>% 
  gather(measure,value,base:rate)  %>%  spread(when1,value)


write.table(attrit_summ_all2_extra,'clipboard-128',sep='\t',row.names=F)

#and I also want to aggregate for all at 3, 6 and 9 months (al lcomplaints)

attrit_summ_all3 <- attrit_summ_all %>% filter(level_3!='no_complaint') %>% group_by(when) %>% 
  summarise(base=sum(base),lost_cumm=sum(lost_cumm)) %>% mutate(rate=lost_cumm/base,level_3='all') %>% 
  select(-c(base,lost_cumm)) %>% filter(when %in% c(3,6,9)) %>% 
  group_by(level_3) %>% mutate(when1=paste0(when,'m')) %>% select(-when) %>% spread(when1,rate)

write.table(attrit_summ_all3,'clipboard-128',sep='\t',row.names=F)


attrit_summ_all3_extra <- attrit_summ_all %>% filter(level_3!='no_complaint') %>% group_by(when) %>% 
  summarise(base=sum(base),lost_cumm=sum(lost_cumm)) %>% mutate(rate=lost_cumm/base,level_3='all') %>% 
  filter(when %in% c(3,6,9)) %>% group_by(level_3) %>% mutate(when1=paste0(when,'m')) %>% select(-when) %>% 
  gather(measure,value,base:rate)  %>%  spread(when1,value)

write.table(attrit_summ_all3_extra,'clipboard-128',sep='\t',row.names=F)



# The last thing I need to do is calculate the baseline for NSF accts with no NSF compaint to have consistent #s
# I will also have to quickly redo the profile for the NSF attriters

#to do this I need to know when the NSFs where, to have a logical attrition number


attrition_all$nsf_flag <- ifelse(attrition_all$ACCT_CONTR_TOTAL_NSF_FEES>0,1,0 )

nsf_no_compl <- attrition_all %>%  filter(level_3=='no_complaint' & nsf_flag==1) %>% select(mask_acct,lost,nsf_flag,closed_period,closed_aux)

load("Z:/M&T Projects/NSF/contrib.rdata")
#create a set with 1 row per acct/month with NSF fees
nsf_aux <- contrib %>% filter(period >= "201404" & ACCT_CONTR_TOTAL_NSF_FEES > 0) %>% group_by(EXPRESSION_8,period) %>%
  summarise(nsf_fee= sum(ACCT_CONTR_TOTAL_NSF_FEES))


period_aux2 <- period_aux
names(period_aux2) <- c('period','nsf_period_aux')

nsf_aux <- left_join(nsf_aux,period_aux2)


nsf_no_compl1 <- left_join(nsf_no_compl,nsf_aux,by=c('mask_acct'='EXPRESSION_8'))
length(unique(nsf_no_compl1$mask_acct))

rm(contrib)

nsf_no_compl1$when <- ifelse(!is.na(nsf_no_compl1$closed_aux),nsf_no_compl1$closed_aux-nsf_no_compl1$nsf_period_aux,-99)
#some had no nsf in april december 


nsf_attr_no_copl <- nsf_no_compl1 %>%  filter(!is.na(period) & (when>=0 | when==-99)) %>% group_by(period,when) %>% 
  summarise(lost=sum(lost),N=n()) %>% group_by(period) %>% 
  mutate(base=sum(N),lost_cumm=cumsum(lost),rate=lost_cumm/base) %>% group_by(when) %>%
  summarise(base=sum(base),lost_cumm=sum(lost_cumm)) %>% mutate(rate=lost_cumm/base)

write.table(nsf_attr_no_copl,'clipboard-128',sep='\t',row.names=F)

nsf_no_compl2 <- nsf_no_compl1
nsf_no_compl2$when[nsf_no_compl2$when==-99] <- 99
nsf_no_compl2$base_9m <- ifelse(nsf_no_compl2$period <= "201405",1,0)
nsf_no_compl2$lost_9m <- ifelse(nsf_no_compl2$when <= 9,1,0)
nsf_no_compl2$lost_9m <- ifelse(is.na(nsf_no_compl2$closed_period),0,nsf_no_compl2$lost_9m)


nsf_no_compl2$base_6m <- ifelse(nsf_no_compl2$period <= "201408",1,0)
nsf_no_compl2$lost_6m <- ifelse(nsf_no_compl2$when <= 6,1,0)
nsf_no_compl2$lost_6m <- ifelse(is.na(nsf_no_compl2$closed_period),0,nsf_no_compl2$lost_6m)

nsf_no_compl2$base_3m <- ifelse(nsf_no_compl2$period <= "201411",1,0)
nsf_no_compl2$lost_3m <- ifelse(nsf_no_compl2$when <= 3,1,0)
nsf_no_compl2$lost_3m <- ifelse(is.na(nsf_no_compl2$closed_period),0,nsf_no_compl2$lost_3m)



#I want to calculate some intervals
attrition_all$base = 1

attrition_all$base_9m <- ifelse(attrition_all$period <= "201405",1,0)
attrition_all$lost_9m <- ifelse(attrition_all$when <= 9,1,0)
attrition_all$lost_9m <- ifelse(is.na(attrition_all$closed_period),0,attrition_all$lost_9m)

attrition_all$base_6m <- ifelse(attrition_all$period <= "201408",1,0)
attrition_all$lost_6m <- ifelse(attrition_all$when <= 6 ,1,0)
attrition_all$lost_6m <- ifelse(is.na(attrition_all$closed_period),0,attrition_all$lost_6m)

attrition_all$base_3m <- ifelse(attrition_all$period <= "201411",1,0)
attrition_all$lost_3m <- ifelse(attrition_all$when <= 3 ,1,0)
attrition_all$lost_3m <- ifelse(is.na(attrition_all$closed_period),0,attrition_all$lost_3m)

a<- attrition_all %>% filter(base_9m ==1 ) %>% group_by(level_3) %>% summarise(b=sum(base_9m),r=mean(lost_9m,na.rm=T),low=t.test(lost_9m)$conf.int[[1]],high=t.test(lost_9m)$conf.int[[2]]) %>% mutate(time='9m')
b<- attrition_all %>% filter(base_6m ==1 ) %>% group_by(level_3) %>% summarise(b=sum(base_6m),r=mean(lost_6m,na.rm=T),low=t.test(lost_6m)$conf.int[[1]],high=t.test(lost_6m)$conf.int[[2]]) %>% mutate(time='6m')
c <- attrition_all %>% filter(base_3m ==1 ) %>% group_by(level_3) %>% summarise(b=sum(base_3m),r=mean(lost_3m,na.rm=T),low=t.test(lost_3m)$conf.int[[1]],high=t.test(lost_3m)$conf.int[[2]]) %>% mutate(time='3m')
d<- attrition_all %>% filter(base_9m ==1 & level_3!='no_complaint') %>% summarise(b=sum(base_9m),r=mean(lost_9m,na.rm=T),low=t.test(lost_9m)$conf.int[[1]],high=t.test(lost_9m)$conf.int[[2]]) %>% mutate(level_3='all',time='9m')
e<- attrition_all %>% filter(base_6m ==1 & level_3!='no_complaint') %>% summarise(b=sum(base_6m),r=mean(lost_6m,na.rm=T),low=t.test(lost_6m)$conf.int[[1]],high=t.test(lost_6m)$conf.int[[2]])  %>% mutate(level_3='all',time='6m')
f<- attrition_all %>% filter(base_3m ==1 & level_3!='no_complaint') %>% summarise(b=sum(base_3m),r=mean(lost_3m,na.rm=T),low=t.test(lost_3m)$conf.int[[1]],high=t.test(lost_3m)$conf.int[[2]])  %>% mutate(level_3='all',time='3m')

g<- nsf_no_compl2 %>% filter(base_9m ==1) %>% summarise(b=sum(base_9m),r=mean(lost_9m,na.rm=T),low=t.test(lost_9m)$conf.int[[1]],high=t.test(lost_9m)$conf.int[[2]])  %>% mutate(level_3='nsf_no_compl',time='9m')

h<- nsf_no_compl2 %>% filter(base_6m ==1) %>% summarise(b=sum(base_6m),r=mean(lost_6m,na.rm=T),low=t.test(lost_6m)$conf.int[[1]],high=t.test(lost_6m)$conf.int[[2]])  %>% mutate(level_3='nsf_no_compl',time='6m')

i<- nsf_no_compl2 %>% filter(base_3m ==1) %>% summarise(b=sum(base_3m),r=mean(lost_3m,na.rm=T),low=t.test(lost_3m)$conf.int[[1]],high=t.test(lost_3m)$conf.int[[2]])  %>% mutate(level_3='nsf_no_compl',time='3m')

#I aLSO want no complaint and no NSF
#I can do it from d,e,f is I have a flag for NSF
attrition_all$nsf_flag <- 0
attrition_all$nsf_flag[attrition_all$mask_acct %in% nsf_aux$EXPRESSION_8] <- 1

j<- attrition_all %>% filter(base_9m ==1 & level_3!='no_complaint' & nsf_flag==0) %>% summarise(b=sum(base_9m),r=mean(lost_9m,na.rm=T),low=t.test(lost_9m)$conf.int[[1]],high=t.test(lost_9m)$conf.int[[2]]) %>% mutate(level_3='no_compl_no_nsf',time='9m')
k<- attrition_all %>% filter(base_6m ==1 & level_3!='no_complaint' & nsf_flag==0) %>% summarise(b=sum(base_6m),r=mean(lost_6m,na.rm=T),low=t.test(lost_6m)$conf.int[[1]],high=t.test(lost_6m)$conf.int[[2]])  %>% mutate(level_3='no_compl_no_nsf',time='6m')
l<- attrition_all %>% filter(base_3m ==1 & level_3!='no_complaint' & nsf_flag==0) %>% summarise(b=sum(base_3m),r=mean(lost_3m,na.rm=T),low=t.test(lost_3m)$conf.int[[1]],high=t.test(lost_3m)$conf.int[[2]])  %>% mutate(level_3='no_compl_no_nsf',time='3m')



attrition_master <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l)

write.table(attrition_master,'clipboard-128',sep='\t',row.names=F)


save.image('oca_image_20150317.rdata',compress='xz')
