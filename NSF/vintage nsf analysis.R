library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#get the acct details from the base
load("Z:/M&T Projects/NSF/base.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")

names(closed_accts)[2] <- 'closed_period'

sum(duplicated(base$EXPRESSION_8))
sum(duplicated(closed_accts$EXPRESSION_8))

base_clean <- base[-which(duplicated(base$EXPRESSION_8)),]

#if acct is closed 2 times take the earliest close
closed_accts <- closed_accts[order(closed_accts$EXPRESSION_8,closed_accts$closed_period),]
closed_accts <- closed_accts[-which(duplicated(closed_accts$EXPRESSION_8)),]

#load the nsf  data
load("Z:/M&T Projects/NSF/nsf_accts.rdata")

#we want to have one line per acct per period, but we only have it when they had NSFs
#so we need acartesian join, to a dummy table with all periods
#I believe merge, with no by's and no all does that

periods <- nsf_accts %>% group_by(period) %>% summarise(n=n()) %>% select(period)

base_aux <- merge(base_clean,periods)  #this is the sper one
nsf <- full_join(nsf_accts,base_aux)

rm(nsf_accts)
nsf <- left_join(nsf,closed_accts)
nsf$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(nsf$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')

rm(closed_accts)
rm(base)
rm(base_clean)
rm(periods)
rm(base_aux)

nsf[2:4][is.na(nsf[2:4])] <- 0  #make the nsf detail NAs zero as they are zzeros
nsf$open <- format(nsf$ACCT_DATE_OPENED_FOR_PRIME,'%Y%m')

save(nsf,file='nsf_aux.rdata')

#lets work first on New accounts
ntb <- nsf %>% filter(ACCT_DATE_OPENED_FOR_PRIME >= '2012-01-01' & period >= open)
length(unique(ntb$EXPRESSION_8))

ntb %>% filter(period==open) %>% group_by(EXPRESSION_8,open) %>% summarise(N=1) %>% group_by(open) %>% summarise(accts=sum(N)) %>% 
  mutate(year=substr(open,1,4),month=substring(open,5)) %>% ggplot(aes(x=month,color=year,y=accts,group=year)) + geom_line() +
  coord_cartesian(ylim=c(0,30000))+theme_bw()+scale_y_continuous('New Checking Accounts Sold',labels=comma)+
  scale_x_discrete('',labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  theme(legend.position='Bottom')+scale_color_discrete('Year')
ggsave('checking sales trends.png',width=9,height = 5, units='in')

ntb_analysis <- ntb %>% group_by(EXPRESSION_8,open,period) %>% arrange(EXPRESSION_8,open,period) 
ntb_analysis <- ntb_analysis %>% group_by(EXPRESSION_8) %>%  mutate(num=dense_rank(period))

closed_early <- ntb_analysis %>% filter(!is.na(closed_period) & num==6) %>% filter(closed_period <= period)
ntb_analysis1 <- ntb_analysis %>% filter(num<=6) %>% group_by(EXPRESSION_8,open) %>% 
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),events=sum(ACCT_NSF_TOTAL)) %>% mutate(with=ifelse(events>=1,1,0))

ntb_analysis1 <- ntb_analysis[!(ntb_analysis$EXPRESSION_8 %in% closed_early$EXPRESSION_8), ]
length(unique(ntb_analysis1$EXPRESSION_8))
ntb_analysis1 <- ntb_analysis1 %>% filter(num<=6) 
length(unique(ntb_analysis1$EXPRESSION_8))

ntb_summary1 <- ntb_analysis1 %>%  filter(num<=6) %>% group_by(EXPRESSION_8,open,ACCT_STYPE) %>% 
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),events=sum(ACCT_NSF_TOTAL),m=n()) %>%
  mutate(with=ifelse(events>=1,1,0)) %>% filter(m==6) %>% mutate(year=substr(open,1,4))

t.test(ntb_summary1$events[ntb_summary1$year=='2012' & ntb_summary1$events>0],ntb_summary1$events[ntb_summary1$year=='2013' & ntb_summary1$events>0],alternative='g')
t.test(ntb_summary1$events[ntb_summary1$year=='2013'& ntb_summary1$events>0],ntb_summary1$events[ntb_summary1$year=='2014' & ntb_summary1$events>0],alternative='g')

ntb_summary2 <- ntb_summary1 %>% group_by(open) %>% summarise(events=sum(events),with=sum(with),accts=n()) %>% mutate(with1=with/accts,avg=events/with)

write.table(ntb_summary2,"clipboard",sep='\t',row.names=F)

ntb_summary1 %>% group_by(year) %>% summarise(events=sum(events),with=sum(with),N=n()) %>% mutate(with1=with/N)
prop.test(c(48583,43729),c(230743,200314),alternative = 'l')
prop.test(c(48583,43729),c(230743,200314),alternative = 'g')

prop.test(c(20242,43729),c(99795,200314),alternative = 'l')

ntb_analysis2 <- ntb_analysis1 %>%  group_by(EXPRESSION_8,open) %>% 
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),events=sum(ACCT_NSF_TOTAL),m=n()) %>%
  mutate(with=ifelse(events>=1,1,0))
ntb_analysis3  = ntb_analysis2 %>% filter(m==6) %>%group_by(open,with) %>% 
  summarise(events1=sum(events),events2=mean(events),N=n()) %>% 
  group_by(open) %>% 
  gather(var,value,events1:N) %>% unite(key,var,with) %>% spread(key,value) %>% 
  mutate(with=N_1/(N_1+N_0),year=substr(open,1,4),month=substring(open,5))


write.table(ntb_analysis3,"clipboard",sep='\t',row.names=F)


ntb_analysis3  %>% ggplot(aes(x=month,y=with,color=year,group=year))+geom_line(size=1)+coord_cartesian(ylim=c(0.1,0.25))+scale_y_continuous('% of Accounts with NSF in First 6 Months',labels=percent)+stat_smooth(method='lm',se=F,linetype=2)+geom_point(size=3,shape=19)+theme_bw()+theme(legend.position='bottom')+scale_x_discrete('',labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+scale_color_discrete("Year")

ggsave('with NSF trend.png',width=9,height = 5, units='in')

ntb_analysis3 %>% mutate(year=substr(open,1,4),month=substring(open,5)) %>% ggplot(aes(x=month,y=events_1,color=year,group=year))+geom_line(size=1)+coord_cartesian(ylim=c(2.5,7.5))+scale_y_continuous('Average NSF Events in First 6 Months',labels=comma)+stat_smooth(method='lm',se=F,linetype=2)+geom_point(size=3,shape=19)+theme_bw()+theme(legend.position='bottom')+scale_x_discrete('',labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+scale_color_discrete("Year")
ggsave('avg events trend new.png',width=9,height = 5, units='in')

#compare incidence and rate for free versus ezchoice
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Int','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking','Retail PMA','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA','Rewards Card','Signature Card','Basic Card')
packages <- gsub('Retail ','',packages)
packages <- gsub('Checking','',packages)
packages <- gsub(' +',' ',packages)
packages <- gsub(' $','',packages)
packages <- gsub('with Interest','w/Int',packages)
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA','REW','SIG','NOR')

#merge the balance data
load("Z:/M&T Projects/NSF/nsf_with_bal.rdata")
bals = nsf %>% filter(EXPRESSION_8 %in% ntb_summary1$EXPRESSION_8 & as.character(period) >= as.character(open) ) %>% select(EXPRESSION_8,ACCT_CONTR_BALANCE,period,open,closed_period) %>% arrange(EXPRESSION_8,period)
bals = bals %>% group_by(EXPRESSION_8) %>%  mutate(num=dense_rank(period)) 
bals1 = bals %>% filter(num<=6) %>% summarise(bal=mean(ACCT_CONTR_BALANCE))


#add debit data
load("Z:/M&T Projects/NSF/debit_all.rdata")

#aggregate it
debit_aux = debit_all %>% group_by(dda,period) %>% arrange(dda,period) %>% 
  summarise_each(funs(sum(.,na.rm=T)),num:amt) %>% 
  group_by(dda) %>% mutate(num1=dense_rank(period))


accts_aux = nsf %>% group_by(EXPRESSION_8,open,closed_period) %>% summarise(extra=1)
debit_aux = left_join(debit_aux,accts_aux[-4],by=c('dda'='EXPRESSION_8'))
debit_aux =  subset(debit_aux, dda %in% ntb_summary1$EXPRESSION_8)


ntb_summary1a = left_join(ntb_summary1,bals1)

ntb_summary2_stype <- ntb_summary1a %>%  group_by(open,ACCT_STYPE) %>% summarise(events=sum(events),with=sum(with),accts=n(),bal=sum(bal,na.rm=T)) %>% mutate(with1=with/accts,avg=events/with) %>%  mutate(stype=factor(ACCT_STYPE,stypes,packages)) %>% filter(ACCT_STYPE %in% c("RE5","RE7",'RC6','RE6')) %>% gather(variable,val,events:avg) %>% unite(key,stype,variable) %>% select(-ACCT_STYPE) %>% spread(key,val,fill=0)

write.table(ntb_summary2_stype,"clipboard",sep='\t',row.names=F)


t.test(ntb_summary1$events[ntb_summary1$events>0 & ntb_summary1$ACCT_STYPE %in% c('RE7') & ntb_summary1$open >='201401' & ntb_summary1$open <='201406'],
       ntb_summary1$events[ntb_summary1$events>0 & ntb_summary1$ACCT_STYPE %in% c('RE5') & ntb_summary1$open >='201401' & ntb_summary1$open <='201406'],alternative = 'g')
prop.test(c(367,2448),c(2490,14460),alternative = 'l')


#for the ntb I want to merge to ntb_analysis1 the debit and the balances
names(debit_aux)[3]  = 'deb_num'
ntb_analysis1a = left_join(ntb_analysis1,debit_aux[c(1:4)],by=c('EXPRESSION_8'='dda','period'='period'))
ntb_analysis1a = left_join(ntb_analysis1a,bals[1:3])

ntb_analysis1a[,16:17][is.na(ntb_analysis1a[,16:17])] = 0 #if no data make zero in this case

#now summarise for the first6 months, by accts, then replicate the analyses, rathe from scratch as
#a test

ntb_summary1x = ntb_analysis1a %>% filter(num<=6) %>% group_by(EXPRESSION_8,ACCT_STYPE,ACCT_DATE_OPENED_FOR_PRIME,closed_period,open) %>% summarise(N=n(),deb_num=sum(deb_num),amt=sum(amt),bal=mean(ACCT_CONTR_BALANCE),nsf=sum(ACCT_NSF_TOTAL),months=sum(ACCT_NSF_TOTAL>0),bal6=nth(ACCT_CONTR_BALANCE,6)) %>% mutate(with=ifelse(months>0,1,0))

ntb_summary1x = subset(ntb_summary1x,EXPRESSION_8 %in% ntb_summary1$EXPRESSION_8)
ntb_summary1x = subset(ntb_summary1x,!is.na(ntb_summary1x$bal))


ntb_summary2x = ntb_summary1x %>% mutate(period = paste0(substr(open,1,4),ifelse(substring(open,5) %in% c('01','02','03','04','05','06'),'1H','2H'))) %>% group_by(period) %>% summarise(accts=n(),deb_num=mean(deb_num),amt=mean(amt),bal=mean(bal),bal6=mean(bal6),months=mean(months),nsf1=mean(nsf),nsf2=mean(nsf[with==1]),with1=mean(with))
write.table(ntb_summary2x,"clipboard",sep='\t',row.names=F)

ntb_summary3x = ntb_summary1x %>% filter(ACCT_STYPE %in% c("RE5","RE7",'RC6','RE6')) %>% mutate(period = paste0(substr(open,1,4),ifelse(substring(open,5) %in% c('01','02','03','04','05','06'),'1H','2H'))) %>% group_by(period,ACCT_STYPE) %>% summarise(accts=n(),deb_num=mean(deb_num),amt=mean(amt),bal=mean(bal),bal6=mean(bal6),months=mean(months),nsf1=mean(nsf),nsf2=mean(nsf[with==1]),with1=mean(with)) %>% gather(measure,value,accts:with1) %>% unite(key,ACCT_STYPE,measure) %>% spread(key,value)
write.table(ntb_summary3x,"clipboard",sep='\t',row.names=F)


#for the existing it is a bit more difficult
#if I do it per period I think I can do it al at once
#we will defie as back nbook if perion - open +1 >12

#you subtract the years, multiply result by 12, then you add the difference in the months (can be negative) - 
#that is tenure in months

nsf$active = 0
nsf$active[(is.na(nsf$closed_period) | (!is.na(nsf$closed_period) & nsf$closed_period > nsf$period)) & nsf$open <= nsf$period] <- 1
table(nsf$active)

nsf$yeardiff <- as.numeric(substr(nsf$period,1,4)) - as.numeric(substr(nsf$open,1,4))
nsf$monthdiff <- as.numeric(substring(nsf$period,5)) - as.numeric(substring(nsf$open,5))
nsf$aux <- paste0(as.numeric(substr(nsf$period,1,4))-1,substring(nsf$period,5))


nsf$book <- NA
nsf$book[nsf$open<nsf$aux & nsf$active==1] <- 'back' 
nsf$book[nsf$open>=nsf$aux & nsf$active==1] <- 'front' 
nsf$book[nsf$active==0] <- 'closed'
nsf$book[nsf$book=='closed' & nsf$open > nsf$period] <- 'future'

table(nsf$period,nsf$book,useNA='ifany')


nsf %>% group_by(period,book) %>% summarise(N=n()) %>% filter(book %in% c('front','back')) %>% 
  ggplot(aes(x=period,y=N,group='identity'))+facet_grid(book~.,scales='free')+geom_line()


back1 <- nsf %>% filter(book=='back') %>% group_by(period) %>% 
  summarise(accts=n(),with=sum(ACCT_NSF_TOTAL>=1),events=sum(ACCT_NSF_TOTAL)) 
back2 <- back1 %>% mutate(avg=events/with,with1=with/accts) %>% mutate(year=substr(period,1,4),month=substring(period,5))
write.table(back2,"clipboard",sep='\t',row.names=F)

back2 %>% group_by(year) %>% summarise(events=sum(events),with=sum(with),accts=sum(accts)) %>%
  mutate(with1=with/accts,avg=events/with)

prop.test(c(1007290,1036595),c(18169890,18703279),alternative = 'l')
prop.test(c(1036595,1004359),c(18703279,19141349),alternative = 'g')

#lets try the prop test
t.test(nsf$ACCT_NSF_TOTAL[nsf$book=='back' & substr(nsf$period,1,4)=="2012" & nsf$ACCT_NSF_TOTAL>0],
nsf$ACCT_NSF_TOTAL[nsf$book=='back' & substr(nsf$period,1,4)=="2013" & nsf$ACCT_NSF_TOTAL>0],alternative='g')

t.test(nsf$ACCT_NSF_TOTAL[nsf$book=='back' & substr(nsf$period,1,4)=="2013"]>0,
          nsf$ACCT_NSF_TOTAL[nsf$book=='back' & substr(nsf$period,1,4)=="2014"]>0,alternative='g')


back2  %>% 
  ggplot(aes(x=month,color=year,y=avg,group=year)) + geom_line() +
  coord_cartesian(ylim=c(4,6))+theme_bw()+scale_y_continuous('Average Events per NSF Account',labels=comma)+
  scale_x_discrete('',labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  theme(legend.position='bottom')+scale_color_discrete('Year')

back2 %>% 
  ggplot(aes(x=month,color=year,y=with1,group=year)) + geom_line() +
  coord_cartesian(ylim=c(0,.05))+theme_bw()+scale_y_continuous('% Accounts with NXF',labels=percent)+
  scale_x_discrete('',labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  theme(legend.position='bottom')+scale_color_discrete('Year')

load("Z:/M&T Projects/NSF/optin.rdata")
#optin has the optin rate for all new accts in 2012 till 201412

#lets' merge it and get a sanity check

optin1 <- inner_join(optin,subset(nsf,period==open,c(book,open,ACCT_DATE_OPENED_FOR_PRIME,closed_period,EXPRESSION_8)))

optin2 <- optin1 %>% filter(!is.na(ACCT_REG_E_FLAG_CUR)) %>% group_by(open,ACCT_REG_E_FLAG_CUR) %>% 
  summarise(N=n()) %>% group_by(open) %>% mutate(P=N/sum(N)) %>% gather(var,val,N:P) %>% 
  unite(key,var,ACCT_REG_E_FLAG_CUR) %>% spread(key,val)

write.table(optin2,"clipboard",sep='\t',row.names=F)

optin2 %>% ggplot(aes(x=substring(open,5),y=P_I,color=substr(open,1,4),group=substr(open,1,4)))+geom_line()

rm(optin)

load("Z:/M&T Projects/NSF/extra.rdata")
nsf_type <- extra %>% group_by(period) %>% summarise_each(funs(sum),-EXPRESSION_8) %>%
  mutate(tot=ACCT_NSF_TOTAL_CHECK+ ACCT_NSF_TOTAL_OTHER)
write.table(nsf_type,"clipboard",sep='\t',row.names=F)
rm(extra)

#lets do the mix through time
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Int','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking','Retail PMA','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
packages <- gsub('Checking','',packages)
packages <- gsub(' +',' ',packages)
packages <- gsub(' $','',packages)
packages <- gsub('with Interest','w/Int',packages)
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')




quarter = function(x) ((x-1) %/% 3) +1
mix <- nsf %>% filter (period==open) %>% group_by(period1=paste0(substr(period,1,4),'Q',quarter(as.numeric(substring(period,5)))),ACCT_STYPE) %>% summarise(N=n()) %>% spread(period1,N,fill=0)
mix$stype <- factor(mix$ACCT_STYPE ,levels=stypes,labels=packages)


write.table(mix,"clipboard",sep='\t',row.names=F)

segm1 <- inner_join(nsf[nsf$open %in% c('201210','201211','201209') & nsf$period=='201212',],segm_201212)
segm2 <- inner_join(nsf[nsf$open %in% c('201410','201411','201409') & nsf$period=='201412',],segm_201412)

segm3 <- rbind(segm1,segm2)

segm3$segment = segm3$HHLD_LIFE_CYCLE_SEGMENT
segm3$segment <- as.character(segm3$segment )
segm3$segment[segm3$segment==8] <- 1
segm3$segment[segm3$segment==9] <- 4
segm3$segment[is.na(segm3$segment)] <- 7
segm3$segment <- factor(segm3$segment,
levels=c(1:7),
labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

segm3$stype = factor(segm3$ACCT_STYPE ,levels=stypes,labels=packages)

#for each of segm1 and segm 2, so the segment mx by package
segm4 <- segm3 %>% group_by(stype,segment,period) %>% summarise(N=n()) %>% group_by(period,stype) %>% mutate(P=N/sum(N)) %>%
  gather(var,value,N:P) %>% unite(key,var,period) %>% spread(key,value,fill=0)

write.table(segm4,"clipboard",sep='\t',row.names=F)

#finally ai want to look at the NSF inciodence by segment, is that MANK does more?

segm6 <- segm5 %>% filter(active==1) %>% group_by(EXPRESSION_8,HHLD_LIFE_CYCLE_SEGMENT) %>% 
  summarise(events=sum(ACCT_NSF_TOTAL)) %>% group_by(HHLD_LIFE_CYCLE_SEGMENT) %>%
  summarise(with=sum(events>=1),accts=n_distinct(EXPRESSION_8))
  

segm6$segment = segm6$HHLD_LIFE_CYCLE_SEGMENT
segm6$segment <- as.character(segm6$segment )
segm6$segment[segm6$segment==8] <- 1
segm6$segment[segm6$segment==9] <- 4
segm6$segment[is.na(segm6$segment)] <- 7
segm6$segment <- factor(segm6$segment,
                        levels=c(1:7),
                        labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))



segm7<- segm6 %>% group_by(segment) %>% summarise_each(funs(sum),-HHLD_LIFE_CYCLE_SEGMENT) %>% mutate(with1=with/accts)
write.table(segm7,"clipboard",sep='\t',row.names=F)
