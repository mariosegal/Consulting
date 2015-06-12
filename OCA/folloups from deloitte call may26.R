######
# on 5/26 they asked to look at level 5 reasons for NSF analysis (page 12)
#by age and otehr dimensions

#append the new level_5, just to make sure
load("Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata")
load('Z:/M&T Projects/OCA/dda_20150424.rdata')

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

data = subset(dda2,!is.na(level_3))
data = left_join(data[-c(57:61)],complaints_2014_2015Q1_matched_both[c(22:26,1)])

(prop.table(table(data$level_5)[table(data$level_5)>0]))  #the date is almost identical, and the changs are because the level_5 are a bit cleaner

#now that I validated lets do it by age, tenure, package - etc lets crunch all the data

reasons1 = data %>% select(level_5,age1,tenure,segment,cbr) %>% gather(measure,level,-level_5) %>% group_by(level_5,measure,level) %>% summarise(N=n()) %>% group_by(level_5,measure) %>% mutate(total=sum(N),P=N/total)

data$ACCT_STYPE = as.character(data$ACCT_STYPE)
stype_aux = unique(data$ACCT_STYPE)
stype_aux = gsub(' Checking',"",stype_aux)
stype_aux = gsub(' Interest|Interest',"Int",stype_aux)
stype_aux = gsub('with',"w/",stype_aux)
data$ACCT_STYPE = factor(data$ACCT_STYPE,unique(data$ACCT_STYPE)[c(1,2,14,15,16,6,17,3,10,8,9,12,5,4,7,13,11,18)],labels=stype_aux[c(1,2,14,15,16,6,17,3,10,8,9,12,5,4,7,13,11,18)])

data1= data
#they asked to changed the alleged on the alleged balance confussion, even if that is part of hierarchy
#data$level_5[data$level_5=='Alleged balance confusion'] = 'Balance confusion'
#doa factor to preserve order
data1$level_5 = factor(data1$level_5,c('Alleged balance confusion','Bounced check','Extended overdraft fee','Fee clarity','Low balance','Merchant error','Other','Other payment issue','Overdraft election', 'Chex Systems','Charge off/Unsat/CUBS','Direct deposit did not post','Right to offset','Debit card decline',''),labels=c('Balance Confusion','Bounced Check','Extended Overdraft Fee','Fee Clarity','Low Balance','Merchant Error','Other','Other Payment Issue','Overdraft Election', 'Chex Systems','Charge off/Unsat/CUBS','Direct deposit did not post','Right to offset','Debit card decline','blank'),ordered = T)

reasons2 = data1 %>% select(level_5,age1,tenure,segment,cbr,ACCT_STYPE) %>%
  filter(!(ACCT_STYPE %in% c('HSA','Brokerage Account')) & !(segment=='NC')) %>% 
  gather(measure,level,-level_5) %>% group_by(level_5,measure,level) %>% summarise(N=n()) %>% group_by(measure,level) %>% mutate(total=sum(N),P=N/total)



levels_all = c(levels(data1$tenure),levels(data1$age1),levels(data1$cbr),levels(data1$segment),stype_aux[c(1,2,14,15,16,6,17,3,10,8,9,12,5,4,7,13)])
reasons2$level = factor(reasons2$level,levels_all,labels=levels_all)
write.table(reasons2,'clipboard-128',sep='\t',row.names=F)
measures1 = c('Customer Age','Segment','Tenure','Community Bank','Checking Package')
names(measures1) = c('age1','segment','tenure','cbr','ACCT_STYPE')

age_labels = str_wrap(c('Under 20','20 to 25','26 to 30','31 to 35','36 to 40','41 to 45','46 to 55','56 to 65','66 to 75','76+'),width=3)
tenure_labels = str_wrap(c('Up to 1','1 to 2','2 to 3','3 to 5','5 to 7','7 to 10','10 to 15','Over 15'),width=3)
seg_labels=str_wrap(c('Build Future','Mass Affl n/Kids','Mainst Fam','Mass Affl Fam','Mainst Ret','Mass Affl Ret') ,width=5)        
rotate = c(F,F,F,T,T)
cbr_labels=levels(data$cbr)
stype_labels= stype_aux[c(1,2,14,15,16,6,17,3,10,8,9,12,5,4,7,13)]
labs1 = c('age_labels','seg_labels','tenure_labels','cbr_labels','stype_labels')

#I need an average line
#reasons_avg = reasons2 %>% group_by(level_5,measure) %>% summarise(N=sum(N)) %>% group_by(measure) %>%mutate(total=sum(N),avg1=N/total)

#level_5=ifelse(level_5=="",'blank',level_5),

reasons_avg = data1 %>% filter(!(ACCT_STYPE %in% c('HSA','Brokerage Account')) & !(segment=='NC')) %>% select(level_5,EXPRESSION_8) %>% mutate(N=1) %>% spread(level_5,N,fill=0) %>% gather(level_5,N,-EXPRESSION_8) %>% group_by(level_5) %>% summarise(m=mean(N),sd=sd(N),N1=n()) %>% mutate(se=sd/sqrt(N1),low=m-se*qnorm(0.975),hi=m+se*qnorm(0.975))
  


chart1 = function(measurex,namex,labelsx,rot=F) {
  rot1=ifelse(rot,90,0)
  hj=ifelse(rot,1,0.5)
  vj=ifelse(rot,0.5,1)
  dat1 = subset(reasons2,measure==measurex & !(level_5 %in% c('Chex Systems','Charge off/Unsat/CUBS','Direct deposit did not post','Right to offset','Debit card decline','blank')) & !is.na(level)   )
  dat1 = left_join(dat1,reasons_avg)
  ch = ggplot()+geom_blank(data=dat1,aes(x=level,y=P,fill=level_5,ymin=low,ymax=hi))+facet_wrap(~level_5,scales='free_y')+geom_rect(data=dat1,aes(ymax=hi,ymin=low,xmin=.4,xmax=length(unique(level))+.6),fill='wheat2',alpha=0.1)
  ch=ch+geom_bar(data=dat1,stat='identity',aes(fill=level_5,x=level,y=P))+theme_bw()+theme(legend.position='none',axis.text.x=element_text(angle=rot1,hjust=hj,vjust=vj,size=8))+scale_y_continuous('Percent of Accts Claiming',labels=percent)+scale_x_discrete(namex,labels=labelsx) +ggtitle(paste('NSF Reason by',namex))+geom_hline(data=dat1,aes(yintercept=m),color='black',linetype=2)
  ch = ch + geom_text(data=dat1,aes(label=comma(N),y=0,x=level),size=2,hjust=-0.5,color='white',angle=90)
  ch
}



lime = rgb(122,184,0,maxColorValue = 256)
green = rgb(0,120,86,maxColorValue = 256)
yellow = rgb(255,179,0,maxColorValue = 256)
purple = rgb(134,73,157,maxColorValue = 256)
green2 = rgb(195,231,111,maxColorValue = 256)
gray = rgb(128,128,128,maxColorValue = 256)
teal = rgb(35,164,145,maxColorValue = 256)
gray1 = rgb(179,179,179,maxColorValue = 256)
blue = rgb(0,112,192,maxColorValue = 256)
red = rgb(192,0,0,maxColorValue = 256)

colors_all = c(purple,green,gray,green2,teal,blue,red,yellow,lime)
colors_age = c(gray1,green,gray1,gray1,gray1,gray1,gray1,yellow,gray1)
colors_tenure = c(gray1,gray1,gray1,gray1,gray1,gray1,gray1,gray1,lime)
#they wanted to show most grayed put, except some, i will do thos eseprately
chart2 = function(measurex,namex,labelsx,rot=F,colors1) {
  rot1=ifelse(rot,90,0)
  hj=ifelse(rot,1,0.5)
  vj=ifelse(rot,0.5,1)
  dat1 = subset(reasons2,measure==measurex & !(level_5 %in% c('Chex Systems','Charge off/Unsat/CUBS','Direct deposit did not post','Right to offset','Debit card decline','blank')) & !is.na(level)   )
  dat1 = left_join(dat1,reasons_avg)
  ch = ggplot()+geom_blank(data=dat1,aes(x=level,y=P,fill=level_5,ymin=low,ymax=hi))+facet_wrap(~level_5,scales='free_y')+geom_rect(data=dat1,aes(ymax=hi,ymin=low,xmin=.4,xmax=length(unique(level))+.6),fill='wheat2',alpha=0.1)
  ch=ch+geom_bar(data=dat1,stat='identity',aes(fill=level_5,x=level,y=P))+theme_bw()+theme(legend.position='none',axis.text.x=element_text(angle=rot1,hjust=hj,vjust=vj,size=8))+scale_y_continuous('Percent of Accts Claiming',labels=percent)+scale_x_discrete(namex,labels=labelsx) +ggtitle(paste('NSF Reason by',namex))+geom_hline(data=dat1,aes(yintercept=m),color='black',linetype=2)
  ch = ch + geom_text(data=dat1,aes(label=comma(N),y=0,x=level),size=2,hjust=-0.5,color='white',angle=90)
  ch = ch + scale_fill_manual(values = colors1)
  ch
}


for (i in 1:5) {  
  chart= chart2(names(measures1[i] ),measures1[i],get(labs1[i]),rotate[i],colors_all)
  ggsave(filename=paste0('z:/M&T Projects/OCA/',measures1[i],'_v4.png'),plot=chart,width =9,height  =5.5,dpi=600)
}

i=1
ch2 = chart2(names(measures1[i] ),measures1[i],get(labs1[i]),rotate[i],colors_age)
ggsave(filename=paste0('z:/M&T Projects/OCA/',measures1[i],'_v5.png'),plot=ch2,width =9,height  =5.5,dpi=600)

i=3
ch2 = chart2(names(measures1[i] ),measures1[i],get(labs1[i]),rotate[i],colors_tenure)
ggsave(filename=paste0('z:/M&T Projects/OCA/',measures1[i],'_v5.png'),plot=ch2,width =9,height  =5.5,dpi=600)



#the second request I understood was for say the age chart, etc. what is the rate by num of NSF fees
#or the reverse, let see what the best visualization is
#they wanted to do it by the NSF feee month before


#Thisis complex,
#1 for the compaints merge the events from prior month andf aggregate by that and the measures, set aside
#2 for the base do  the same, then you can merge and crunch

#1
load("Z:/M&T Projects/NSF/nsf_with_bal.rdata")


nsf1 = nsf %>% filter(period %in% c('201403','201404','201405','201406','201407','201408','201409','201410','201411')) %>% select(c(EXPRESSION_8,ACCT_NSF_TOTAL,period))
nsf_period = c('201404','201405','201406','201407','201408','201409','201410','201411',"201412")
names(nsf_period) = c('201403','201404','201405','201406','201407','201408','201409','201410','201411')
nsf1$nsf_period = nsf_period[as.character((nsf1$period))]
names(nsf1)[2] = 'nsf_events_prior_month'

data$period = format(data$entered_date,'%Y%m')
data1 = left_join(data,nsf1[-3],by=c('EXPRESSION_8','period'='nsf_period'))

complaints = data1 %>% select(EXPRESSION_8,period,nsf_events_prior_month,segment,age1,tenure,ACCT_STYPE,cbr) %>% gather(measure,value,-c(EXPRESSION_8,period,nsf_events_prior_month))


#first I need to merge the new levels to all
aux = subset(complaints_2014_2015Q1_matched_both,type=='non escalated')
data1 = left_join(dda2[-c(57:61)],aux[c(22:26,1)])

prop.table(table(data1$nsf_events_prior_month,useNA='ifany'))   # this sucks as with 62% having no, it iwil not be what they expect


#I need to try current and past month, ie a rolling sum using zoo
library(zoo)

#nsf2 = nsf %>% filter(period %in% c('201403','201404','201405','201406','201407','201408','201409','201410','201411','201412')) %>% select(c(EXPRESSION_8,ACCT_NSF_TOTAL,period))

nsf2 = nsf %>% filter(period %in% c('201403','201404','201405','201406','201407','201408','201409','201410','201411','201412')) %>% select(c(EXPRESSION_8,ACCT_NSF_TOTAL,period)) %>% spread(period,ACCT_NSF_TOTAL,fill=0) %>% gather(period,ACCT_NSF_TOTAL,-EXPRESSION_8)


nsf2$events_curr_last = ave(nsf2$ACCT_NSF_TOTAL,nsf2$EXPRESSION_8,FUN=function(x) rollsum(x,k=2,align='right',fill=0))

#now emrge the last 2, see how it looks
data1 = left_join(data1,nsf2[-3],by=c('EXPRESSION_8','period'))
prop.table(table(data1$events_curr_last,useNA='ifany'))  
sum(data1$events_curr_last>0,na.rm=T)
5684/6208


#this ii better
#now I need to calculat the rate by age, segment, eetc and numbe rof events
data1$events_curr_last[is.na(data1$events_curr_last)] = 0
data1$events = cut(data1$events_curr_last,c(-0.01,.01,1,2,3,4,5,10,Inf),labels=c('None','1','2','3','4','5','6 to 10','11+'),ordered=T)

part1 = data1 %>% select(EXPRESSION_8, events,age1, segment, tenure, ACCT_STYPE, cbr, period) %>% gather(measure,level,-c(EXPRESSION_8,events, period)) %>% group_by(period,events,measure,level)  %>% summarise(complaints=n())


#now I need to do the same with dda2, but hereI need to be careful to define if they should be counted after the merege as I created all 2 months aggregates - all of these accounts wer epresent in 201412

#first of all add create oen record per period (if open)
dda3 = dda2 %>% select(EXPRESSION_8,age1, segment, tenure, ACCT_STYPE, cbr, ACCT_DATE_OPENED_FOR_PRIME)
aux = data.frame(EXPRESSION_8=unique(dda2$EXPRESSION_8),aux1='x')
aux1 = data.frame(aux1='x',period = c('201403','201404','201405','201406','201407','201408','201409','201410','201411','201412'))
aux = left_join(aux,aux1)

dda3 =  left_join(dda3,aux[-2])
dda3= left_join(dda3,nsf2,by=c('EXPRESSION_8','period'))

dda3$open = format(dda3$ACCT_DATE_OPENED_FOR_PRIME,'%Y%m')
dda3$flag_open = ifelse(dda3$open <= as.character(dda3$period),1,0)
table(dda3$flag_open)
dda3 = subset(dda3,flag_open==1)
sum(is.na(dda3$events_curr_last))
dda3$events_curr_last[is.na(dda3$events_curr_last)]=0

dda3$events = cut(dda3$events_curr_last,c(-0.01,.01,1,2,3,4,5,10,Inf),labels=c('None','1','2','3','4','5','6 to 10','11+'),ordered=T)

part2 = dda3 %>% select(EXPRESSION_8, events,age1, segment, tenure, ACCT_STYPE, cbr, period) %>% gather(measure,level,-c(EXPRESSION_8,events, period)) %>% group_by(period,events,measure,level)  %>% summarise(accts=n())

combo = left_join(part2,part1)
combo$complaints[is.na(combo$complaints)]=0


combo1 = combo %>% filter(period != '201403') %>% group_by(measure,events,level) %>% summarise(accts1=sum(accts),compl1=sum(complaints)) %>% mutate(rate=1000*compl1/accts1)

combo1





#################################
#################################

#abbas wants to see interaction between OD and optin  
od1 = dda2 %>% group_by(nsf_fee,ACCT_REG_E_FLAG_CUR_201401,ACCT_REG_E_FLAG_CUR_201412,ACCT_CQI_OVERDRAFT_201401,ACCT_CQI_OVERDRAFT) %>% summarise(N=n())
write.table(od1,'clipboard-128',sep='\t',row.names=F)


#he also wants tenure versus age
dda2$tenure_num = as.numeric((as.Date('2014-12-31')-dda2$ACCT_DATE_OPENED_FOR_PRIME))/365
dda2$age_num = as.numeric((as.Date('2014-12-31')-dda2$ACCT_DATE_OF_BIRTH_201412))/365

ggplot(dda2,aes(x=tenure_num,y=age_num))+geom_point(alpha=0.1,color='blue')+stat_smooth(method='lm',color='red')
cor(dda2$tenure_num,dda2$age_num,use="pairwise.complete.obs")
ggplot(dda2,aes(x=tenure,y=age1))+geom_jitter(alpha=0.1,color='blue')+stat_smooth(aes(grup=1),method='lm',color='red')


#what % of new accts opted in originally or at a later point, for 2014 only
load('Z:/M&T Projects/OCA/optin_data_201412.rdata')
optin_data$ACCT_DATE_OPENED = as.Date(optin_data$ACCT_DATE_OPENED,'%m/%d/%Y')
optin_data$ACCT_REG_E_DATE_CUR = as.Date(optin_data$ACCT_REG_E_DATE_CUR,'%m/%d/%Y')
optin_data$ACCT_REG_E_DATE_PRIOR1 = as.Date(optin_data$ACCT_REG_E_DATE_PRIOR1,'%m/%d/%Y')
optin_data$ACCT_REG_E_DATE_PRIOR2 = as.Date(optin_data$ACCT_REG_E_DATE_PRIOR2,'%m/%d/%Y')
sum(optin_data$ACCT_REG_E_DATE_CUR == optin_data$ACCT_DATE_OPENED,na.rm=T)

sum(is.na(optin_data$ACCT_REG_E_DATE_CUR))
sum(!is.na(optin_data$ACCT_REG_E_DATE_PRIOR1))
sum(!is.na(optin_data$ACCT_REG_E_DATE_PRIOR2))

changed = optin_data %>% filter(  as.numeric(format(ACCT_DATE_OPENED,'%m')) <= 3) %>% group_by(ACCT_REG_E_FLAG_PRIOR1,ACCT_REG_E_FLAG_CUR) %>% summarise(N=n())
write.table(changed,'clipboard-128',sep='\t',row.names=F)


changed1 = optin_data %>% filter(  as.numeric(format(ACCT_DATE_OPENED,'%m') ) <= 3 & ACCT_REG_E_FLAG_PRIOR1!=ACCT_REG_E_FLAG_CUR & !is.na(ACCT_REG_E_DATE_PRIOR1)) %>% group_by(ACCT_STYPE,ACCT_REG_E_FLAG_PRIOR1,ACCT_REG_E_FLAG_CUR) %>% summarise(N=n()) %>% group_by(ACCT_STYPE) %>% mutate(P=N/sum(N))
write.table(changed1,'clipboard-128',sep='\t',row.names=F)


base1 = optin_data %>% filter(  as.numeric(format(ACCT_DATE_OPENED,'%m') ) <= 3 ) %>% group_by(ACCT_STYPE,ACCT_REG_E_FLAG_PRIOR1,ACCT_REG_E_FLAG_CUR) %>% summarise(N=n()) %>% group_by(ACCT_STYPE) %>% mutate(P=N/sum(N))
write.table(base1,'clipboard-128',sep='\t',row.names=F)


opt_days = optin_data %>% filter(!is.na(ACCT_REG_E_DATE_PRIOR1) & ACCT_REG_E_FLAG_CUR!=ACCT_REG_E_FLAG_PRIOR1 & as.numeric(format(ACCT_DATE_OPENED,'%m')) <= 3) %>% group_by(days=ACCT_REG_E_DATE_CUR-ACCT_REG_E_DATE_PRIOR1) %>% summarise(N=n())
write.table(opt_days,'clipboard-128',sep='\t',row.names=F)

opt_days1 = optin_data %>% filter(!is.na(ACCT_REG_E_DATE_PRIOR1) & ACCT_REG_E_FLAG_CUR==ACCT_REG_E_FLAG_PRIOR1 & as.numeric(format(ACCT_DATE_OPENED,'%m')) <= 3) %>% group_by(days=ACCT_REG_E_DATE_CUR-ACCT_REG_E_DATE_PRIOR1) %>% summarise(N=n())
write.table(opt_days1,'clipboard-128',sep='\t',row.names=F)

optin_age = table(dda2$ACCT_REG_E_FLAG_CUR_201412,dda2$age1)
write.table(optin_age,'clipboard-128',sep='\t',row.names=T)

# the latest ask was to see if they changed whne there wa a complaint ro an NSF
# this will need to be done period by period, and I am going to do it the easy way
#no fanct dplyr just do ti and collpse later

load('Z:/M&T Projects/OCA/rege_extra_14.rdata')
load('Z:/M&T Projects/OCA/od_extra_14.rdata')
load("Z:/M&T Projects/OCA/closed_14.rdata")

dda2$open_period = format(dda2$ACCT_DATE_OPENED_FOR_PRIME,'%Y%m')
names(closed_14)[2] = 'closed_period'
dda2 = left_join(dda2,closed_14)

aux_month = c('02','03','04','05','06','07','08','09','10')
names(aux_month) = c('04','05','06','07','08','09','10','11','12')
res1 = list()
i=0
for (month in  c('04','05','06','07','08','09','10','11','12')) {
  gc()
  i = i +1  
  month1 = aux_month[month]
  nsf_aux = subset(nsf2,period == paste0('2014',month) & events_curr_last >0)
  nsf_aux1 = left_join(dda2[ dda2$open_period <= paste0('2014',month1) ,],nsf_aux[-c(2)],by='EXPRESSION_8')
  nsf_aux1$compl_period = format(nsf_aux1$entered_date,'%Y%m')
  nsf_aux1$complaint = ifelse(nsf_aux1$compl_period==paste0('2014',month),1,0)
  nsf_aux1$complaint[is.na(  nsf_aux1$complaint)] = 0
  od_aux1 = subset(od_extra_14,period ==  paste0('2014',month))
  od_aux2 = subset(od_extra_14,period ==  paste0('2014',month1))
  reg_aux1 = subset(rege_extra_14,period ==  paste0('2014',month))
  reg_aux2 = subset(rege_extra_14,period ==  paste0('2014',month1))
  names(od_aux1)[2] = paste0("ACCT_CQI_OVERDRAFT",'_after')
  names(od_aux2)[2] = paste0("ACCT_CQI_OVERDRAFT",'_before')
  names(reg_aux1)[2] = paste0("ACCT_REG_E_FLAG_CUR",'_after')
  names(reg_aux2)[2] = paste0("ACCT_REG_E_FLAG_CUR",'_before')
  nsf_aux1 = left_join(left_join(nsf_aux1,od_aux1[-3]),od_aux2[-3])
  nsf_aux1 = left_join(left_join(nsf_aux1,reg_aux1[-3]),reg_aux2[-3])
  nsf_aux1$events_curr_last[is.na(nsf_aux1$events_curr_last)] = 0
  nsf_aux1$grp1[nsf_aux1$events_curr_last >0 & nsf_aux1$complaint] = 'nsf_compl'
  nsf_aux1$grp1[nsf_aux1$events_curr_last >0 & !nsf_aux1$complaint] = 'nsf_only'
  nsf_aux1$grp1[nsf_aux1$events_curr_last ==0 & nsf_aux1$complaint] = 'complaint_only'
  nsf_aux1$grp1[nsf_aux1$events_curr_last ==0 & !nsf_aux1$complaint] = 'neither'
  gc()
  res1[[i]] = nsf_aux1 %>% group_by(grp1,ACCT_CQI_OVERDRAFT_before,ACCT_CQI_OVERDRAFT_after,ACCT_REG_E_FLAG_CUR_before,ACCT_REG_E_FLAG_CUR_after) %>% summarise(N=n()) %>% group_by(grp1) %>% mutate(P=N/sum(N),period = paste0('2014',month))
  rm(nsf_aux1,od_aux1,od_aux2,reg_aux1,reg_aux2,nsf_aux)

}

results = bind_rows(res1)
results1 = results %>% filter(!is.na(ACCT_CQI_OVERDRAFT_before) & !is.na(ACCT_CQI_OVERDRAFT_after)) %>%  group_by( grp1, ACCT_CQI_OVERDRAFT_before, ACCT_CQI_OVERDRAFT_after,ACCT_REG_E_FLAG_CUR_before,ACCT_REG_E_FLAG_CUR_after) %>%  summarise(N1=sum(N)) %>% group_by(grp1) %>%  mutate(P=N1/sum(N1),tot=sum(N1))
write.table(results1,'clipboard-128',sep='\t',row.names=F)

results2 = results %>% filter(!is.na(ACCT_CQI_OVERDRAFT_before) & !is.na(ACCT_CQI_OVERDRAFT_after)) %>%  group_by( grp1, ACCT_CQI_OVERDRAFT_before, ACCT_CQI_OVERDRAFT_after) %>% summarise(N1=sum(N)) %>% group_by(grp1)%>% mutate(P=N1/sum(N1))
write.table(results2,'clipboard-128',sep='\t',row.names=F)

results3 = results %>% filter(!is.na(ACCT_CQI_OVERDRAFT_before) & !is.na(ACCT_CQI_OVERDRAFT_after)) %>%  group_by( grp1, ACCT_REG_E_FLAG_CUR_before,ACCT_REG_E_FLAG_CUR_after) %>% summarise(N1=sum(N)) %>% group_by(grp1) %>% mutate(P=N1/sum(N1))
write.table(results3,'clipboard-128',sep='\t',row.names=F)
