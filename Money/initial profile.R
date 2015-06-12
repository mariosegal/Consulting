#load/read some data to establish a abaseline as of 201412
library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')

#read required data for 201406, latest month we have alerts data
#read directly from SQL database and create R datasets
acct_201406 <- sqlQuery(mtdata,"SELECT  EXPRESSION_8, ACCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME, ACCT_AMT_BAL_FOR_PRIME, ACCT_CONTR_NET_CONTRIBUTION_MTD, ACCT_CQI_BILL_PAY, ACCT_CQI_CHECK_CARD,ACCT_WEB_NBR_PMNT_MNT_ACCT, ACCT_WEB_NBR_PMNT_NON_MNT, ACCT_CQI_WEB, ACCT_CQI_DIRECT_DEPOSIT, ACCT_CQI_OVERDRAFT from IQRMT.dbo.ACCT_201406 where ( ( LEFT(ACCT_STYPE,1)='R' AND ACCT_PTYPE='DDA')) AND ACCT_STATUS_FOR_PRIME <> 'X' ")

web_201406 <- sqlQuery(mtdata,"SELECT EXPRESSION_8, ACCT_STYPE, ACCT_DATE_OPENED_FOR_PRIME,  ACCT_WEB_CANCEL_DATE, ACCT_WEB_NBR_SIGN_ONS, ACCT_WEB_ENROLLMENT_DATE, ACCT_WEB_BILL_PAY_DATE, ACCT_WEB_BILL_PAY_CANCEL_DATE from IQRMT.dbo.ACCT_201406 where ACCT_PTYPE='WEB' ")

acac_201406 <- sqlQuery(mtdata,"SELECT * from IQRMT.dbo.ACAC_201406")

eactive_201406 <- sqlQuery(mtdata,"SELECT  * from IQRMT.dbo.EACTIVE_201406")

eservice_201406 <- sqlQuery(mtdata,"SELECT  * from IQRMT.dbo.ESERVICE_201406")

mobile_201406 <- sqlQuery(mtdata,"SELECT  * from IQRMT.dbo.MOBILE_201406")

odbcClose(mtdata)
rm(mtdata)

save(list=ls(),file='baseline data 201406.rdata')

#load libraries
library(dplyr)
library(tidyr)
library(stringr)

#append the dda acct number from ACAC the web acct
#this is so we aggregate certain web activity at the acct level
aux_acac = subset(acac_201406,EXPRESSION_2 %in% acct_201406$EXPRESSION_8,c(EXPRESSION_2,EXPRESSION_1))
names(aux_acac) = c('linked_dda','web_acct')
length(intersect(web_201406$EXPRESSION_8,aux_acac$web_acct))
names(web_201406)[1:2] = c('web_acct','web_stype')

web_201406 = left_join(web_201406,aux_acac)

#it is possible to have 2+ web ccts per DDA, like Husband and wife and a joint DDA
#I will aggregate the web data by linked_dda, as far as enrollment data I will take earlier that is still open, same
#for bill pay
web_201406$ACCT_WEB_ENROLLMENT_DATE = as.Date(web_201406$ACCT_WEB_ENROLLMENT_DATE,'%m/%d/%Y')
web_201406$ACCT_WEB_CANCEL_DATE = as.Date(web_201406$ACCT_WEB_CANCEL_DATE,'%m/%d/%Y')
web_201406$ACCT_WEB_BILL_PAY_DATE = as.Date(web_201406$ACCT_WEB_BILL_PAY_DATE,'%m/%d/%Y')
web_201406$ACCT_WEB_BILL_PAY_CANCEL_DATE = as.Date(web_201406$ACCT_WEB_BILL_PAY_CANCEL_DATE,'%m/%d/%Y')
aux_web = web_201406 %>% filter(web_stype=='WEB') %>% group_by(linked_dda) %>%
  summarise(web_date=min(ACCT_WEB_ENROLLMENT_DATE),web_cancel=max(ACCT_WEB_CANCEL_DATE,na.rm=T),bp_date=min(ACCT_WEB_BILL_PAY_DATE,na.rm=T),bp_cancel=max(ACCT_WEB_BILL_PAY_CANCEL_DATE,na.rm=T),signons=sum(ACCT_WEB_NBR_SIGN_ONS,na.rm=T))

#I lost the web acct, I can add it by merging back by linked_dda and the 2 web dates
aux_web = left_join(aux_web,web_201406[c(9,6,1)],by=c('linked_dda'='linked_dda','web_date' ='ACCT_WEB_ENROLLMENT_DATE'))

#the dupes are when 2 web acct sopend the same day, after I merge the aactivity I will collapse this

#merge to the acct table
accts1 = left_join(acct_201406,aux_web,by=c('EXPRESSION_8'='linked_dda'))

#I have questions as to whether this matching is accurate so I need to validate it
table(accts1$ACCT_CQI_WEB,!is.na(accts1$web_date) )
table(accts1$ACCT_CQI_WEB,!is.na(accts1$web_date)  & (!is.na(accts1$web_cancel) & accts1$web_cancel >accts1$web_date))
#seems my web indicator with dates is very consistent with the web one one

table(accts1$ACCT_CQI_BILL_PAY==0,!is.na(accts1$bp_date) )
table(accts1$bp_date[accts1$ACCT_CQI_BILL_PAY==0 &!is.na(accts1$bp_date)] < 
        accts1$bp_cancel[accts1$ACCT_CQI_BILL_PAY==0 &!is.na(accts1$bp_date)])
#it seems for  bpay the dates did not work as well ,I am not sure why

#I will use the original indicators, but I can use the date for web if need be later on

#define some flags
accts1$signons[is.na(accts1$signons)] = 0
accts1$web_active = ifelse(accts1$signons>0,1,0)
accts1$bp_active = ifelse(accts1$ACCT_WEB_NBR_PMNT_MNT_ACCT >0 | accts1$ACCT_WEB_NBR_PMNT_NON_MNT > 0,1,0)

#I need to do the othr services
names(eservice_201406) = gsub('\\.','_',tolower(make.names(names(eservice_201406))))
eservice_201406$e_service_date_enrollment = as.Date(eservice_201406$e_service_date_enrollment,'%m/%d/%Y')
eservice_201406$e_service_date_cancel= as.Date(eservice_201406$e_service_date_cancel,'%m/%d/%Y')
eservice_201406$e_service_service = str_trim(eservice_201406$e_service_service)

eservice1 = eservice_201406 %>% filter(e_service_related_acct_ptype=='DDA' & !is.na(expression_2) & !is.na(e_service_date_enrollment) &(is.na(e_service_date_cancel) | (!is.na(e_service_date_cancel) & e_service_date_enrollment > e_service_date_cancel)) ) %>% group_by(expression_2,e_service_service) %>% summarise(flag=1) %>% spread(e_service_service,flag)
eservice1[,-1][is.na(eservice1[-1])] = 0

#I want activity for financeworks and fico only, alerts activity depends on the alert (I can even argue, they are 
#for emergencies and if I personally never get one that is better
names(eactive_201406) = gsub('\\.','_',tolower(make.names(names(eactive_201406))))
eactive_201406$e_activity_service = str_trim(eactive_201406$e_activity_service)
eactive_201406$e_activity_label_5 = str_trim(eactive_201406$e_activity_label_5)

eactive1 = eactive_201406 %>% filter(!is.na(expression_1)  & e_activity_service %in% c('FICO','FINANCEWORKS') & e_activity_label_5=="Num of Sign Ons") %>% group_by(expression_1,e_activity_service) %>% summarise(activity=sum(e_activity_value_5,na.rm=T)) %>% spread(e_activity_service,activity)
eactive1[,-1][is.na(eactive1[,-1])] = 0
names(eactive1)[-1] = paste0(names(eactive1)[-1],'_activity')

#merge them
accts1 = left_join(accts1,eservice1[1:5],by=c('EXPRESSION_8'='expression_2'))
accts1 = left_join(accts1,eactive1,by=c('web_acct'='expression_1'))

#add mobile at the web acct level
#mbile has sms and wap
names(mobile_201406)[26:27] = c('sms_date','wap_date')
names(mobile_201406)[2] = c('web_acct_mob')
mobile_201406$sms_date= as.Date(mobile_201406$sms_date,'%m/%d/%Y')
mobile_201406$wap_date= as.Date(mobile_201406$wap_date,'%m/%d/%Y')
mobile_201406$sms_activity = rowSums(mobile_201406[c(3,5,8)],na.rm=T)
mobile_201406$wap_activity = rowSums(mobile_201406[c(4,6,7,9,10:25)],na.rm=T)

#let's summarise SMS and WAP
sms = mobile_201406 %>% filter(!is.na(sms_date)) %>% group_by(web_acct_mob) %>% summarise(sms=1,sms_activity=sum(sms_activity)) %>% mutate(sms_active=ifelse(sms_activity>0,1,0))
wap = mobile_201406 %>% filter(!is.na(wap_date)) %>% group_by(web_acct_mob) %>% summarise(wap=1,wap_activity=sum(wap_activity))  %>% mutate(wap_active=ifelse(wap_activity>0,1,0))
accts1 = left_join(accts1,sms,by=c('web_acct'='web_acct_mob'))
accts1 = left_join(accts1,wap,by=c('web_acct'='web_acct_mob'))

#I need to collapse the dupes, they are from merging when 2 web accts were present
accts2 = accts1 %>%group_by(EXPRESSION_8,ACCT_STYPE,ACCT_DATE_OPENED_FOR_PRIME,ACCT_AMT_BAL_FOR_PRIME,ACCT_CONTR_NET_CONTRIBUTION_MTD,ACCT_CQI_BILL_PAY,ACCT_CQI_CHECK_CARD,ACCT_WEB_NBR_PMNT_MNT_ACCT,ACCT_WEB_NBR_PMNT_NON_MNT,ACCT_CQI_WEB,ACCT_CQI_DIRECT_DEPOSIT,ACCT_CQI_OVERDRAFT,web_date,web_cancel,bp_date,bp_cancel,signons,web_active,bp_active,ALERTS,EDELIVERY,FICO,FINANCEWORKS) %>% summarise(FICO_activity=sum(FICO_activity,na.rm=T),FINANCEWORKS_activity=sum(FINANCEWORKS_activity,na.rm=T),sms=sum(sms,na.rm=T),sms_activity=sum(sms_activity,na.rm=T),sms_active=sum(sms_active,na.rm=T),wap=sum(wap,na.rm=T),wap_activity=sum(wap_activity,na.rm=T),wap_active=sum(wap_active,na.rm=T))

accts2$wap = ifelse(accts2$wap,1,0)
accts2$wap_active = ifelse(accts2$wap_active,1,0)
accts2$sms = ifelse(accts2$sms,1,0)
accts2$sms_active = ifelse(accts2$sms_active,1,0)

accts2$fico_active=ifelse(accts2$FICO_activity>0,1,0)
accts2$fworks_active=ifelse(accts2$FINANCEWORKS_activity>0,1,0)

accts2[20:23][is.na(accts2[20:23])] = 0


#add debit activity 
load("Z:/M&T Projects/NSF/debit_all.rdata")
debit1 = debit_all %>% filter(period=='201406') %>% group_by(dda) %>% summarise_each(funs(sum),c(num,amt))

accts2 = left_join(accts2,debit1,by=c('EXPRESSION_8'='dda'))
accts2$num[is.na(accts2$num)]=0
accts2$amt[is.na(accts2$amt)]=0
accts2$debit_active = ifelse(accts2$num >0,1,0)
save(accts2,file='accts2.rdata')


#I have to re-run the next piece, as at least web changed
#I also need to validat ethe high mobile thing
#and then measure differences in attrition (simple 9 month)
#and then see the causality part

#do the analysis 
web = accts2 %>% group_by(ACCT_CQI_WEB,web_active) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(web,'clipboard',sep='\t',row.names=F)

bp = accts2 %>% group_by(ACCT_CQI_BILL_PAY,bp_active) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(bp,'clipboard',sep='\t',row.names=F)

deb = accts2 %>% group_by(ACCT_CQI_CHECK_CARD,debit_active) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(deb,'clipboard',sep='\t',row.names=F)

dd = accts2 %>% group_by(ACCT_CQI_DIRECT_DEPOSIT) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(dd,'clipboard',sep='\t',row.names=F)


od = accts2 %>% group_by(ACCT_CQI_OVERDRAFT) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(od,'clipboard',sep='\t',row.names=F)

fw = accts2 %>% group_by(FINANCEWORKS,fworks_active) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(fw,'clipboard',sep='\t',row.names=F)

fico = accts2 %>% group_by(FICO,fico_active) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(fico,'clipboard',sep='\t',row.names=F)

alerts = accts2 %>% group_by(ALERTS) %>% summarise(N=n(),contrib=mean(ACCT_CONTR_NET_CONTRIBUTION_MTD),bal=mean(ACCT_AMT_BAL_FOR_PRIME)) %>% ungroup() %>% mutate(P=N/sum(N))
write.table(alerts,'clipboard',sep='\t',row.names=F)

mean(accts2$ACCT_CONTR_NET_CONTRIBUTION_MTD)
mean(accts2$ACCT_AMT_BAL_FOR_PRIME)


#try to do boxplots
#to do  that create some groups per measure

accts2$debit1[accts2$ACCT_CQI_CHECK_CARD==1 & accts2$debit_active==1] = 'active'
accts2$debit1[accts2$ACCT_CQI_CHECK_CARD==1 & accts2$debit_active==0] = 'inactive'
accts2$debit1[accts2$ACCT_CQI_CHECK_CARD==0 & accts2$debit_active==0] = 'none'

accts2$web1[accts2$ACCT_CQI_WEB ==1 & accts2$web_active==1] = 'active'
accts2$web1[accts2$ACCT_CQI_WEB==1 & accts2$web_active==0] = 'inactive'
accts2$web1[accts2$ACCT_CQI_WEB==0 & accts2$web_active==0] = 'none'

accts2$dd[accts2$ACCT_CQI_DIRECT_DEPOSIT ==1 ] = 'active'
accts2$dd[accts2$ACCT_CQI_DIRECT_DEPOSIT==0 ] = 'none'

accts2$od[accts2$ACCT_CQI_OVERDRAFT ==1 ] = 'active'
accts2$od[accts2$ACCT_CQI_OVERDRAFT==0 ] = 'none'

accts2$alerts1[accts2$ALERTS ==1 ] = 'active'
accts2$alerts1[accts2$ALERTS ==0 ] = 'none'

accts2$bp1[accts2$ACCT_CQI_BILL_PAY==1 & accts2$bp_active==1] = 'active'
accts2$bp1[accts2$ACCT_CQI_BILL_PAY==1 & accts2$bp_active==0] = 'inactive'
accts2$bp1[accts2$ACCT_CQI_BILL_PAY==0 & accts2$bp_active==0] = 'none'

accts2$FW[accts2$FINANCEWORKS==1 & accts2$fworks_active==1] = 'active'
accts2$FW[accts2$FINANCEWORKS==1 & accts2$fworks_active==0] = 'inactive'
accts2$FW[accts2$FINANCEWORKS==0 & accts2$fworks_active==0] = 'none'

accts2$fico1[accts2$FICO==1 & accts2$fico_active==1] = 'active'
accts2$fico1[accts2$FICO==1 & accts2$fico_active==0] = 'inactive'
accts2$fico1[accts2$FICO==0 & accts2$fico_active==0] = 'none'

vars = which(names(accts2) %in% c("EXPRESSION_8","ACCT_AMT_BAL_FOR_PRIME","ACCT_CONTR_NET_CONTRIBUTION_MTD","ACCT_STYPE","debit1","web1","dd","od","alerts1","bp1","FW","fico1"))
aux = accts2 %>% ungroup() %>% select(vars) %>% gather(measure,level,debit1:fico1)
aux$measure = factor(aux$measure,c('debit1','web1','bp','dd','od','FW','FICO','alerts1'),labels=c('Debit Card','Web Banking','Bill Pay','Direct Deposit','Overdraft','FinanceWorks','FICO Score','Alerts (Checking)'))

aux$level = factor(aux$level,c('none','inactive','active'),labels=c('Not Enrolled','Inactive/None','Active'))

#mtb colors
lime = rgb(122,184,0,maxColorValue = 256)
green = rgb(0,120,86,maxColorValue = 256)
yellow = rgb(255,179,0,maxColorValue = 256)
purple = rgb(134,73,157,maxColorValue = 256)
green2 = rgb(195,231,111,maxColorValue = 256)
gray = rgb(128,128,128,maxColorValue = 256)
teal = rgb(35,164,145,maxColorValue = 256)

aux %>% filter(!is.na(level)) %>% ggplot(aes(x=measure,fill=level,y=ACCT_CONTR_NET_CONTRIBUTION_MTD))+geom_boxplot(notch=T)+scale_y_continuous('Average Monthly Contribution - June 2014',labels=dollar)+theme_bw()+coord_cartesian(ylim=c(-5,25))+theme(legend.position='bottom',panel.grid.major=element_blank())+scale_fill_manual("",values=c(lime,yellow,green))+geom_hline(yintercept=16.57,color='red',size=1,linetype=2)
ggsave('contrib_boxplots.png',width = 9,height=5.5,units='in')
