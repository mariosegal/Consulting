# there were posting changes in 2013Q1 and thos elikely explain most of the decrease, but they say they stil see some
#so I will compare last 3Q of 2013 toi last 3Q of 2014 and  redefine the segments a bit to work in 9 months

#to do that I will use base_new, I will have to:
#   i. define a new existing and new flag for that, existing will be as of jan 2012 1 year tenure
#   ii. create lost flags for 2012, 2013 and 2014
#   iii. classify as accidental, ocassional, frequent and no (for existing only)
#   iv. run all  the stats to validate
#   v. then attempt the analysis of who goes to no NSFs and who does less NSFs (binary)


#load data and libraries
#I will load all the data from image but keep only what I need as it is too much already

load("Z:/M&T Projects/NSF/nsf_image_20150312.rdata")
rm(list=setdiff(ls(),c('nsf_fees_all','base_new')))
load("Z:/M&T Projects/NSF/contrib.rdata")

library(dplyr)
library(tidyr)
library(ggplot2)


#base_new doe snot have the number of months for 12 to 14, or the tot num and I need to do that from contrib
#I did all claculations even if some where there, I need to check

nsf_summary_new1 <- contrib %>% 
  filter((period >= '201304' & period <= '201312') | (period >= '201404' & period <= '201412')) %>%
  select(c(EXPRESSION_8:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,period)) %>% 
  group_by(year=substr(period,1,4),EXPRESSION_8) %>%
  summarise(fees_9m=sum(ACCT_CONTR_TOTAL_NSF_FEES),waivers_9m=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),
            months_9m=sum(ACCT_CONTR_TOTAL_NSF_FEES>0),present_9m=n_distinct(year)) %>%
  gather(measure,value,fees_9m:present_9m) %>% unite(key,measure,year) %>% spread(key,value,fill=NA)


nsf_summary_new1$present_9m_2013[is.na(nsf_summary_new1$present_9m_2013)] <- 0
nsf_summary_new1$present_9m_2014[is.na(nsf_summary_new1$present_9m_2014)] <- 0


base_new1 <- full_join(base_new[1:7],nsf_summary_new1,by='EXPRESSION_8')

rm(contrib)

colSums(base_new[8:13],na.rm=T)
colSums(base_new1[8:15],na.rm=T)

#there are many that are not present in either period - this is because the base_new has accts from 12 and also some that maybe closed on 1Q of 13
base_new1 <- base_new1 %>% filter(present_9m_2013==1 | present_9m_2014==1)

sum(duplicated(base_new1$EXPRESSION_8)) #good it is zero



#I need to add some details for those that are in nsf_summary_new and not on base_new
load("Z:/M&T Projects/NSF/closed_accts.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")

missing <- base_new1$EXPRESSION_8[is.na(base_new1$ACCT_STYPE)]

load('Z:/M&T Projects/NSF/accts_201201_all.rdata')

#logically all the missing should have been closed in 201201, check you have them in the new extract and then append the data somehow
#I think others were opened in 201201 as well and I had not counted them

extra <- subset(base_new1,EXPRESSION_8 %in% missing)
length(setdiff(missing,accts_201201_all$EXPRESSION_8))
length(intersect(missing,accts_201201_all$EXPRESSION_8))  #243
found <- subset(accts_201201_all,EXPRESSION_8 %in% intersect(missing,accts_201201_all$EXPRESSION_8))
names(found)[2:4] <- c('date','stype1','closed1')

table(is.na(found$closed1))  # those 22,242 were closed in 201201 
found$stype1 <- as.character(found$stype1 )

still_missing <- subset(extra,!(EXPRESSION_8 %in% intersect(missing,accts_201201_all$EXPRESSION_8)))
#who are the others, I will have to look them up manually on sql
#they are likely acquired, as they appear suddenly with an old open date, 
# at least I found 22K or so, lets see how much I miss after I tag those with sTYPE and open_date


base_new1 %>% group_by(a=!is.na(ACCT_STYPE)) %>% summarise_each(funs(sum(.,na.rm=T)),fees_9m_2013:waivers_9m_2014)


base_new1 <- left_join(base_new1,found)
base_new1$ACCT_DATE_OPENED_FOR_PRIME[is.na(base_new1$ACCT_DATE_OPENED_FOR_PRIME) & !is.na(base_new1$date)] <- as.Date(base_new1$date[is.na(base_new1$ACCT_DATE_OPENED_FOR_PRIM) & !is.na(base_new1$date)],'%m/%d/%Y')

base_new1$ACCT_STYPE[is.na(base_new1$ACCT_STYPE) & !is.na(base_new1$stype1)] <- base_new1$stype1[is.na(base_new1$ACCT_STYPE) & !is.na(base_new1$stype1)]

base_new1$closed[is.na(base_new1$closed) & !is.na(base_new1$closed1)] <- format(as.Date(base_new1$closed1[is.na(base_new1$closed) & !is.na(base_new1$closed1)],'%m/%d/%Y'),'%Y%m')

#some accts closed before 201304 SO EXCLUDE THEM
base_new1 <- filter(base_new1,(closed > '201303') | is.na(closed))

#base_new1$closed12 <- ifelse(base_new1$closed >= '201201' & base_new1$closed <= "201212",1,0)
base_new1$closed13 <- ifelse(base_new1$closed >= '201304' & base_new1$closed <= "201403",1,0)  #this is a new dfinition
#it means they  closed before the secoind comparison period, I do not want to rename all the rest
base_new1$closed14 <- ifelse(base_new1$closed >= '201404' & base_new1$closed <= "201412",1,0)

View(subset(base_new1,!is.na(stype1))[1:100,])
base_new1$exclude <- 0
base_new1$exclude[which(base_new1$EXPRESSION_8 %in% still_missing$EXPRESSION_8)] <- 1

#1) define front and back book flags

base_new1$book <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME < '2012-04-01','back','front')
base_new1$book[is.na(base_new1$book)] <- 'exclude'
table(format(base_new1$ACCT_DATE_OPENED_FOR_PRIME,"%Y") ,base_new1$book)  #looks good
base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),fees_9m_2013:fees_9m_2014)
base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),waivers_9m_2013:waivers_9m_2014)

base_new1 %>% group_by(exclude) %>% summarise_each(funs(sum(.,na.rm=T)),fees_9m_2013:fees_9m_2014)
base_new1 %>% group_by(exclude) %>% summarise_each(funs(sum(.,na.rm=T)),waivers_9m_2013:waivers_9m_2014)

#we are accounting for all so we are golden

#the front bookm inclusdes people who opened in 13 and 14, and those cant be compared easily - unless I define 2 non overlapping groups maybe opened in 1Q2013 and 1Q 2014 - that is to be defined later

###########
#2) classify into groups - only bask book
base_new1$group_13 <- NA
base_new1$group_13 <- ifelse(base_new1$present_9m_2013==1 & base_new1$book=='back' & base_new1$months_9m_2013==0,'No_NSF',base_new1$group_13)
base_new1$group_13 <- ifelse(base_new1$present_9m_2013==1 & base_new1$book=='back' & base_new1$months_9m_2013==1,'Accidental',base_new1$group_13)
base_new1$group_13 <- ifelse(base_new1$present_9m_2013==1 & base_new1$book=='back' & base_new1$months_9m_2013 %in% 2:3,'Occasional',base_new1$group_13)
base_new1$group_13 <- ifelse(base_new1$present_9m_2013==1 & base_new1$book=='back' & base_new1$months_9m_2013 %in% 4:12,'Frequent',base_new1$group_13)
table(base_new1$group_13,useNA='ifany')

base_new1$group_14 <- NA
base_new1$group_14 <- ifelse(base_new1$present_9m_2014==1 & base_new1$book=='back' & base_new1$months_9m_2014==0,'No_NSF',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_9m_2014==1 & base_new1$book=='back' & base_new1$months_9m_2014==1,'Accidental',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_9m_2014==1 & base_new1$book=='back' & base_new1$months_9m_2014 %in% 2:3,'Occasional',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_9m_2014==1 & base_new1$book=='back' & base_new1$months_9m_2014 %in% 4:12,'Frequent',base_new1$group_14)

base_new1$group_14 <- ifelse(base_new1$present_9m_2013==1 & !is.na(base_new1$closed) & base_new1$closed<='201312' & base_new1$book=='back' ,'Lost_2013',base_new1$group_14)

base_new1$group_14 <- ifelse(base_new1$present_9m_2013==1 & !is.na(base_new1$closed) & base_new1$closed<='201412' & base_new1$closed>='201401'  & base_new1$book=='back' ,'Lost_2014',base_new1$group_14)

#I still get some NAs

#there are some that the group is NA and are on the back book, 259 so lets remove them
table(base_new1$group_14,base_new1$book,useNA='ifany')
table(base_new1$group_13,base_new1$book,useNA='ifany')
colSums(base_new1[base_new1$book=='back' & (is.na(base_new1$group_13) | is.na(base_new1$group_14)),8:15],na.rm=T) #fees are inmaterial
weird <- which(base_new1$book=='back' & (is.na(base_new1$group_13) | is.na(base_new1$group_14)))
base_new1 <- base_new1[-weird,]



#I forgot the with, do it now
base_new1$with_2013 <- ifelse(base_new1$months_9m_2013>0,1,0)
base_new1$with_2014 <- ifelse(base_new1$months_9m_2014>0,1,0)

#the last thing is the NSF_Events, needs to be aggregated for the 9M periods
load("Z:/M&T Projects/NSF/nsf_accts.rdata")
events_summary <- nsf_accts %>% select(period,EXPRESSION_8,ACCT_NSF_TOTAL) %>%
  filter((period >= '201304' & period <= '201312') | (period >= '201404' & period <= '201412')) 
events_summary <- events_summary %>% group_by(year=paste0('9m_',substr(period,1,4)),EXPRESSION_8) %>% 
  summarise(events=sum(ACCT_NSF_TOTAL)) 
events_summary <- events_summary %>% spread(year,events)
names(events_summary)[2:3] <- paste0('events_',names(events_summary)[2:3])
events_summary$with_9m_2013a <- ifelse(events_summary$events_9m_2013,1,0)
events_summary$with_9m_2014a <- ifelse(events_summary$events_9m_2014,1,0)


#merge it and make with and events 0 when acct was present and values are NA
base_new1 <- left_join(base_new1,events_summary)
base_new1$events_9m_2013[base_new1$present_9m_2013==1 & is.na(base_new1$events_9m_2013)] <- 0
base_new1$with_9m_2013a[base_new1$present_9m_2013==1 & is.na(base_new1$with_9m_2013a)] <- 0

base_new1$events_9m_2014[base_new1$present_9m_2014==1 & is.na(base_new1$events_9m_2014)] <- 0
base_new1$with_9m_2014a[base_new1$present_9m_2014==1 & is.na(base_new1$with_9m_2014a)] <- 0


#stats for the matrix
matrix_n_new1 <- table(base_new1$group_13,base_new1$group_14)
write.table(matrix_n_new1,'clipboard-128',sep='\t',row.names=T)


#total stats for calculations of waterfalls and such
matrix_sum_a <- base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a))
write.table(matrix_sum_a,'clipboard-128',sep='\t',row.names=F)

matrix_sum_a1 <- base_new1 %>% group_by(group_13) %>% summarise(N=sum(present_9m_2013),amt=sum(fees_9m_2013,na.rm=T),wvr=sum(waivers_9m_2013,na.rm=t))
write.table(matrix_sum_a1,'clipboard-128',sep='\t',row.names=F)


matrix_sum_a2 <- base_new1 %>% mutate(n=1) %>%  group_by(group_13,group_14) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:n))
write.table(matrix_sum_a2,'clipboard-128',sep='\t',row.names=F)


save.image('nsf_image_2013Q2_on_20150320.rdata')

#EXTEND Analysis to te front book
#I think it is already on base_new1, with group_13 as NA, but I need to explore

load('nsf_image_2013Q2_on_20150320.rdata')

table(format(base_new1$ACCT_DATE_OPENED_FOR_PRIME[is.na(base_new1$group_13)],'%Y%m'))
#yes they appera to haev been opened from 201204 onwards

#lets first classify them as front book in 201304, and front Boon in 201404
base_new1$front_book <- NA
base_new1$front_book <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME >= '2012-04-01' & base_new1$ACCT_DATE_OPENED_FOR_PRIME < '2013-04-01','front_2013',base_new1$front_book)
base_new1$front_book <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME >= '2013-04-01' & base_new1$ACCT_DATE_OPENED_FOR_PRIME < '2014-04-01','front_2014',base_new1$front_book)
table(base_new1$front_book ,useNA='ifany')

#let's see ho wany made it to the back book and compare with the number of closed from front
sum(base_new1$front_book=='front_2013' & (is.na(base_new1$closed)  | base_new1$closed>='201404'),na.rm=T)

sum(!is.na(base_new1$group_13))
sum(!is.na(base_new1$group_13) & base_new1$closed <='201403',na.rm=T)
table(base_new1$closed ,!is.na(base_new1$group_13) ,useNA='ifany')


#the issue is not that much that teh front was smaller, it is that the acquistion rate for 2014 was so much lower
#and my analysis also included any new accts opened in say 201306

#But I can certainly, compare the 2 front books and indetify differences
#and I can also show the difference in the acquisiton rate by month (I had that)
#and even compare diffrences in package, segment, and even nsf for those really new ones

#lets compare the 2 front books

matrix_sum_front <- base_new1 %>% filter(!is.na(front_book)) %>% group_by(front_book) %>% summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a))
write.table(matrix_sum_front,'clipboard-128',sep='\t',row.names=F)

matrix_sum_front1 <- base_new1 %>% filter(!is.na(front_book)) %>% group_by(front_book) %>% summarise(N=sum(present_9m_2013),amt=sum(fees_9m_2013,na.rm=T),wvr=sum(waivers_9m_2013,na.rm=t))
write.table(matrix_sum_front1,'clipboard-128',sep='\t',row.names=F)


matrix_sum_front2 <- base_new1  %>% filter(!is.na(front_book)) %>% mutate(n=1) %>%  group_by(front_book) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:n))
write.table(matrix_sum_front2,'clipboard-128',sep='\t',row.names=F)




load('z:/M&T Projects/NSF/optin.rdata')
aux <- subset(base_new1,!is.na(front_book))
aux <- inner_join(aux,optin)
rm(optin)

load('z:/M&T Projects/NSF/segm_201304_201404.rdata')
#I want to get the data at the beginning of period for comsistency

segm_aux <- rbind(segm_201304[segm_201304$EXPRESSION_8 %in% aux$EXPRESSION_8[aux$front_book=='front_2013'],],
                  segm_201404[segm_201404$EXPRESSION_8 %in% aux$EXPRESSION_8[aux$front_book=='front_2014'],])
aux <- inner_join(aux,segm_aux)

#I only keep those that I found data on the month, I honestly do not know where the other ones comes from
#I validated that these counts are what i see on SQL, minus maybe 100 accts each period, meaningless


#redo the analysis from above
matrix_sum_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book) %>% summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a))
write.table(matrix_sum_front,'clipboard-128',sep='\t',row.names=F)

matrix_sum_front2 <- aux  %>% filter(!is.na(front_book)) %>% mutate(n=1) %>%  group_by(front_book) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a,n))
write.table(matrix_sum_front2,'clipboard-128',sep='\t',row.names=F)

pkg_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,stype) %>% summarise(N=n()) %>% 
  group_by(front_book) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,front_book,var) %>% spread(key,value,fill=0) %>%  arrange(desc(front_2013_N))
write.table(pkg_front,'clipboard-128',sep='\t',row.names=F)

#analyze optin segm and balances
optin_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,ACCT_REG_E_FLAG_CUR) %>% summarise(N=n()) %>% 
  group_by(front_book) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,front_book,var) %>% spread(key,value,fill=0) 

write.table(optin_front,'clipboard-128',sep='\t',row.names=F)

aux$segment = aux$HHLD_LIFE_CYCLE_SEGMENT
aux$segment <- as.character(aux$segment )
aux$segment[aux$segment==8] <- 1
aux$segment[aux$segment==9] <- 4
aux$segment[is.na(aux$segment)] <- 7
aux$segment <- factor(aux$segment,
levels=c(1:7),
labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))


segm_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,segment) %>% summarise(N=n()) %>% 
  group_by(front_book) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,front_book,var) %>% spread(key,value,fill=0) 
write.table(segm_front,'clipboard-128',sep='\t',row.names=F)

aux$CQI <- rowSums(aux[grepl('CQI',names(aux))])
bals_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,stype) %>% 
  summarise(N=n(),cqi=mean(CQI),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% 
  gather(var,value,N:bal) %>% unite(key,front_book,var) %>%  spread(key,value,fill=0) %>% arrange(desc(front_2013_N))
write.table(bals_front,'clipboard-128',sep='\t',row.names=F)



cqi_front <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,CQI) %>% summarise(N=n()) %>% 
  group_by(front_book) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,front_book,var) %>% spread(key,value,fill=0) 
write.table(cqi_front,'clipboard-128',sep='\t',row.names=F)

cqi_front1 <- aux %>% filter(!is.na(front_book)) %>% select(c(front_book,ACCT_CQI_BILL_PAY:ACCT_CQI_OVERDRAFT)) %>% 
 group_by(front_book) %>% summarise_each(funs(mean)) %>% 
  gather(var,value,-front_book) %>% spread(front_book,value,fill=0) 
write.table(cqi_front1,'clipboard-128',sep='\t',row.names=F)

prop.test(c(56911,50548),c(211620,184843),alternative = 'l',conf.level = .95)


#for the mean test, I will need the sd


aux %>% filter(front_book=='front_2013' & events_9m_2013>0) %>% group_by(front_book) %>% 
  summarise(N=n(),events=sum(events_9m_2013),avg=mean(events_9m_2013),sd=sd(events_9m_2013))

aux %>% filter(front_book=='front_2013' & events_9m_2014>0) %>% group_by(front_book) %>% 
  summarise(N=n(),events=sum(events_9m_2014),avg=mean(events_9m_2014),sd=sd(events_9m_2014))

aux %>% filter(front_book=='front_2014' & events_9m_2014>0) %>% group_by(front_book) %>% 
  summarise(N=n(),events=sum(events_9m_2014),avg=mean(events_9m_2014),sd=sd(events_9m_2014))


t.test(aux$events_9m_2013[aux$front_book=='front_2013' & aux$events_9m_2013>0],
       aux$events_9m_2014[aux$front_book=='front_2014' & aux$events_9m_2014>0],
       alternative='g')

t.test(aux$events_9m_2013[aux$front_book=='front_2013' & aux$events_9m_2013>0],
       aux$events_9m_2014[aux$front_book=='front_2013' & aux$events_9m_2014>0],
       alternative='g')

#optin test
prop.test(c(82033,62386),c(211620,184843),alternative = 'g',conf.level = .95)


t.test(aux$CQI[aux$front_book=='front_2013'],aux$CQI[aux$front_book=='front_2014'],alternative = 'g',conf.level = .95)

aux$stype <- factor(as.character(aux$stype))
sapply(as.character(levels(aux$stype)), function(x) try(t.test(aux$CQI[aux$front_book=='front_2013' & aux$stype==x],aux$CQI[aux$front_book=='front_2014' & aux$stype==x],alternative = 'g',conf.level = .95)$p.value))

sapply(as.character(levels(aux$stype)), function(x) try(t.test(aux$CQI[aux$front_book=='front_2013' & aux$stype==x],aux$CQI[aux$front_book=='front_2014' & aux$stype==x],alternative = 'l',conf.level = .95)$p.value))


rm(segm_201304,segm_201404,segm_aux)

#For completeness i should do the back book and also the new ones

#create a new group variable
base_new1$back_2013 <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME< '2012-04-01','back_2013','other')
base_new1$back_2014 <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME< '2013-04-01' & (is.na(base_new1$closed) | base_new1$closed >='201404') ,'back_2014','other')

base_new1$ntb_flag <- NA
base_new1$ntb_flag <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME >= '2013-04-01' & base_new1$ACCT_DATE_OPENED_FOR_PRIME <= '2013-12-31','ntb_2013',base_new1$ntb_flag)
base_new1$ntb_flag <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME >= '2014-04-01' & base_new1$ACCT_DATE_OPENED_FOR_PRIME <= '2014-12-31','ntb_2014',base_new1$ntb_flag)

matrix_sum_back13 <- base_new1  %>% filter(!is.na(back_2013)) %>% mutate(n=1) %>%  group_by(back_2013) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a,n))
write.table(matrix_sum_back13,'clipboard-128',sep='\t',row.names=F)

matrix_sum_back14 <- base_new1  %>% filter(!is.na(back_2014)) %>% mutate(n=1) %>%  group_by(back_2014) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a,n))
write.table(matrix_sum_back14,'clipboard-128',sep='\t',row.names=F)

matrix_sum_ntb <- base_new1  %>% filter(!is.na(ntb_flag)) %>% mutate(n=1) %>%  group_by(ntb_flag) %>% 
  summarise_each(funs(sum(.,na.rm=T)),c(fees_9m_2013:waivers_9m_2014,with_2013:with_9m_2014a,n))
write.table(matrix_sum_ntb,'clipboard-128',sep='\t',row.names=F)


#look ntb <- aux %>% filter(!is.na(front_book)) %>% group_by(front_book,ACCT_REG_E_FLAG_CUR) %>% summarise(N=n()) %>% 

#measure Opt-in for back and NTB
load("Z:/M&T Projects/NSF/optin.rdata")
aux1 <- base_new1 %>% filter(!is.na(ntb_flag)) %>% inner_join(optin)
ntb_optin <-  aux1%>% group_by(ntb_flag,ACCT_REG_E_FLAG_CUR) %>%
  summarise(N=n()) %>% group_by(ntb_flag) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,ntb_flag,var) %>% spread(key,value,fill=0) 

write.table(ntb_optin,'clipboard-128',sep='\t',row.names=F)
prop.test(c(63695,40213),c(63695+110979,40213+103375),alternative = 'g')

rm(optin)

load("Z:/M&T Projects/NSF/optin_201304.rdata")
load("Z:/M&T Projects/NSF/optin_201404.rdata")


aux2 <- base_new1 %>% filter(!is.na(back_2013)) %>% left_join(optin_201304)
ntb_back13 <-  aux2 %>% group_by(back_2013,ACCT_REG_E_FLAG_CUR) %>%
  summarise(N=n()) %>% group_by(back_2013) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,back_2013,var) %>% spread(key,value,fill=0) 
write.table(ntb_back13,'clipboard-128',sep='\t',row.names=F)

aux3 <- base_new1 %>% filter(!is.na(back_2014)) %>% left_join(optin_201404)
ntb_back14 <-  aux2 %>% group_by(back_2014,ACCT_REG_E_FLAG_CUR) %>%
  summarise(N=n()) %>% group_by(back_2014) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,back_2014,var) %>% spread(key,value,fill=0) 
write.table(ntb_back14,'clipboard-128',sep='\t',row.names=F)

prop.test(c(322481,356212),c(1558772,1599324),alternative = 'l')

rm(optin_201304,optin_201404)

#mix for NTB
mix_ntb <- aux1 %>% group_by(ntb_flag,stype) %>% summarise(N=n()) %>% 
  group_by(ntb_flag) %>% mutate(P=N/sum(N))  %>% gather(var,value,N:P) %>% 
  unite(key,ntb_flag,var) %>% spread(key,value,fill=0) %>%  arrange(desc(ntb_2013_N))
write.table(mix_ntb,'clipboard-128',sep='\t',row.names=F)
  
  
#test for average events
a = base_new1$events_9m_2013[base_new1$events_9m_2013>0 & base_new1$back_2013=='back_2013']
b = base_new1$events_9m_2014[base_new1$events_9m_2014>0 & base_new1$back_2014=='back_2014']
t.test(a,b,alternative = 'g')

a1 = base_new1$events_9m_2013[base_new1$events_9m_2013>0 & base_new1$front_book=='front_2013']
b1 = base_new1$events_9m_2014[base_new1$events_9m_2014>0 & base_new1$front_book=='front_2014']
t.test(a1,b1,alternative = 'g')

a2 = base_new1$events_9m_2013[base_new1$events_9m_2013>0 & base_new1$ntb_flag=='ntb_2013']
b2 = base_new1$events_9m_2014[base_new1$events_9m_2014>0 & base_new1$ntb_flag=='ntb_2014']
t.test(a2,b2,alternative = 'g')

# Where is the decrease coming form

  
  save.image('matrix analysis 2Q13 20150410.rdata')
