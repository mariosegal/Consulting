#Consolidate the base into something I have the open abnd closed status to measure accts per year

base_new <- left_join(base,closed_accts)
names(base_new)[4] <- 'closed'

base_new <- subset(base_new,!(EXPRESSION_8 %in% weird))

base_new$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(base_new$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')

base_new$closed12 <- ifelse(is.na(base_new$closed) | base_new$closed >= "201301",0,1)
base_new$closed13 <- 0
base_new$closed13[!is.na(base_new$closed) & base_new$closed >= "201301" & base_new$closed <= "201312"] <- 1

accts_12 <- sum(base_new$ACCT_DATE_OPENED_FOR_PRIME < '2013-01-01')  #because I extracted startins in 2012

accts_13 <- sum(base_new$ACCT_DATE_OPENED_FOR_PRIME < '2014-01-01' & base_new$closed12 !=1) 
#because they did not close in 2012 and were open in 2013 at least for part partially

accts_14 <- sum(base_new$ACCT_DATE_OPENED_FOR_PRIME < '2015-01-01' & base_new$closed12 !=1 &  base_new$closed13 !=1) 

open <- table(format(base_new$ACCT_DATE_OPENED_FOR_PRIME[format(base_new$ACCT_DATE_OPENED_FOR_PRIME,"%Y")>="2012"],'%Y%m'))
open <- as.data.frame(open)
open$Var1 <- as.character(open$Var1)
ggplot(open,aes(x=substr(Var1,5,6),y=Freq,color=substr(Var1,1,4),group=substr(Var1,1,4)))+geom_line()

ggplot(open,aes(x=substr(Var1,5,6),y=Freq,color=substr(Var1,1,4),group=substr(Var1,1,4)))+geom_line()


base_new$stype <-  factor(base_new$ACCT_STYPE ,levels=stypes,labels=packages)
#open trend  by package
open1 <- base_new %>% filter(ACCT_DATE_OPENED_FOR_PRIME>= '2012-01-01') %>% 
  group_by(year=format(ACCT_DATE_OPENED_FOR_PRIME,'%Y'),month=format(ACCT_DATE_OPENED_FOR_PRIME,'%m'),ACCT_STYPE,stype) %>%
  summarise(N=n()) 
library(scales)
open1 %>% group_by(year,month) %>% summarise(N=sum(N))  %>% ggplot(aes(x=month,group=year,color=year,y=N))+geom_line(stat='identity')+theme_bw()+theme(legend.position='bottom')+scale_y_continuous('Number of Accounts',labels=comma)+scale_x_discrete('Month',labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+scale_color_discrete('Year')
ggsave('checking_trends_all.png',width=10,height=6,units='in')

open1  %>% filter(ACCT_STYPE %in% c('RA8','RE6','RH3','RH5','RC6','RZ2','RE7','RH2','RW2')) %>% ggplot(aes(x=month,group=year,color=year,y=N))+geom_line()+facet_wrap(~stype,scales='free_y')+theme_bw()+theme(legend.position='bottom')+scale_y_continuous('Number of Accounts',labels=comma)+scale_x_discrete('Month',labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+scale_color_discrete('Year')

ggsave('checking_trends.png',width=10,height=6,units='in')



#try the statistics again to deal with the differences in waived fees
#do it from sratch for sanity

tmp <- nsf_accts %>% group_by(year=substr(period,1,4)) %>% summarise_each(funs(sum),ACCT_NSF_TOTAL:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED)


#it is an extraction issue, I only extracted those with events, some waivers are for prior year and also the OD fees
#nsf_fee_all has the fees for all accts present at any time in the year that had NSF
load("Z:/M&T Projects/NSF/nsf_fees_all.rdata")

base_new <- left_join(base_new,nsf_fees_all,by='EXPRESSION_8')


#base new is missing accts from 2012 that closed in 201201, due to how data was extracted, this  is only messing the numbers for all sectrion for 2012, so lets add those

extra_Accts <- subset(nsf_fees_all,EXPRESSION_8 %in% closed_accts$EXPRESSION_8[closed_accts$period=='201201'])
 
aux <- bind_rows(base_new,extra_Accts)


#add flags for which year they should be counted

aux$flag12 <- ifelse(aux$ACCT_DATE_OPENED_FOR_PRIME < '2013-01-01',2012,0)
aux$flag13 <- ifelse(aux$ACCT_DATE_OPENED_FOR_PRIME < '2014-01-01' & aux$closed12 !=1,2013,0)
aux$flag14 <- ifelse(aux$ACCT_DATE_OPENED_FOR_PRIME < '2015-01-01' & aux$closed12 !=1 &  aux$closed13 !=1,2014,0)

#fix the extra one swith no open date (I know they wer eopen on 201201 for at least a day, due to how I extracted)

aux$flag12[is.na(aux$ACCT_DATE_OPENED_FOR_PRIME)] <- 2012  #they were open in 2012 for maybe a day but open
aux$flag13[is.na(aux$ACCT_DATE_OPENED_FOR_PRIME)] <- 0  #they were all closed in 201201
aux$flag14[is.na(aux$ACCT_DATE_OPENED_FOR_PRIME)] <- 0  #they were all closed in 201201

aux1 <- aux %>% gather(extra,year,flag12:flag14) %>% filter(year!=0)
aux1$with12 <- ifelse(aux1$fees_2012>0 & !is.na(aux1$fees_2012) & aux1$year==2012,1,0)
aux1$with13 <- ifelse(aux1$fees_2013>0 & !is.na(aux1$fees_2013) & aux1$year==2013,1,0)
aux1$with14 <- ifelse(aux1$fees_2014>0 & !is.na(aux1$fees_2014) & aux1$year==2014,1,0)

aux1$wv12 <- ifelse(aux1$waived_2012>0 & !is.na(aux1$waived_2012) & aux1$year==2012,1,0)
aux1$wv13 <- ifelse(aux1$waived_2013>0 & !is.na(aux1$waived_2013) & aux1$year==2013,1,0)
aux1$wv14 <- ifelse(aux1$waived_2014>0 & !is.na(aux1$waived_2014) & aux1$year==2014,1,0)

summary_all_new <- aux1 %>% group_by(year) %>%
  summarise(N=n(),fees_2012=sum(fees_2012,na.rm=T),waived_2012=sum(waived_2012,na.rm=T),
            fees_2013=sum(fees_2013,na.rm=T),waived_2013=sum(waived_2013,na.rm=T),
            fees_2014=sum(fees_2014,na.rm=T),waived_2014=sum(waived_2014,na.rm=T),
            with_2012=sum(with12),with_2013=sum(with13),with_2014=sum(with14),
            wv_2012=sum(wv12),wv_2013=sum(wv13),wv_2014=sum(wv14),
            both_2012=sum(with12=1 & wv12==1 ),both_2013=sum(with13=1 & wv13==1),
            both_2014=sum(with14=1 & wv14==1)) 

write.table(summary_all_new,'clipboard-128',sep='\t',row.names=F)
