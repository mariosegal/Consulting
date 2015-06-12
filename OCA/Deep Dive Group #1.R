#load data
load("Z:/M&T Projects/OCA/complaints_top.rdata")
load('Z:/M&T Projects/OCA/base_20150501.rdata')


#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)


#Hypothesis !
# age is driving the spikes in MyChoice

#lest group data for group1 by package and age
#and the same for base
#if muchoice rate is higher for 26 to 35 then it is age not package
#if other packages show the same behavior

complaints_grp1 = subset(complaints_top,group_new==1)
complaints_grp1a = complaints_grp1 %>% group_by(stype,age1) %>% summarise(complaints=n()) 
complaints_grp1b = complaints_grp1 %>% group_by(stype,tenure) %>% summarise(complaints=n()) 
complaints_grp1c = complaints_grp1 %>% group_by(stype,cbr) %>% summarise(complaints=n()) 

base1 = filter(base,level_2_clean=="Checking account") %>% group_by(stype,age1) %>% summarise(accts=n())
base1b = filter(base,level_2_clean=="Checking account") %>% group_by(stype,tenure) %>% summarise(accts=n())

base1c = filter(base,level_2_clean=="Checking account") %>% group_by(stype,cbr) %>% summarise(accts=n())

combo1 = right_join(complaints_grp1a,base1) %>% mutate(complaints=ifelse(is.na(complaints),0,complaints),rate=1000*complaints/accts) %>% group_by(age1) %>% mutate(P=accts/sum(accts))

combo1b = right_join(complaints_grp1b,base1b) 
combo1c = right_join(complaints_grp1c,base1c)

combo1b$complaints =ifelse(is.na(combo1b$complaints),0,combo1b$complaints)
combo1b = combo1b%>% mutate(rate=1000*complaints/accts) %>% group_by(tenure) %>% mutate(P=accts/sum(accts))

combo1c$complaints =ifelse(is.na(combo1c$complaints),0,combo1c$complaints)
combo1c = combo1c%>% mutate(rate=1000*complaints/accts) %>% group_by(cbr) %>% mutate(P=accts/sum(accts)) 



ch1 = combo1 %>% filter(stype %in% c('My Choice','EZ Choice','Free','My Choice Plus','Direct','My Choice Plus w/int') & !is.na(age1)) %>% ggplot(aes(x=age1,y=rate,fill=stype,group=1,label=comma(round(rate,1))))+geom_bar(stat='identity')+facet_wrap(~stype,ncol = 2)+stat_smooth(method='lm',se=F,color='red',linetype=2,size=1)+geom_text(vjust=-1)+coord_cartesian(ylim=c(0,10))+scale_y_continuous('Complaint Rate per 1,000 Accounts',labels=NULL)+scale_x_discrete("Account Owner Age")+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(),panel.grid.major=element_blank())+ggtitle("Complaint Rate by Owner Age and Checking Package")

ch2 = combo1 %>% filter(stype=='My Choice' & !is.na(age1)) %>% ggplot(aes(x=age1,y=P,group=1,label=percent(round(P,2)))) + geom_bar(stat='identity',fill='lightblue') + stat_smooth(method='lm',se=F,color='red',size=1,linetype=2)+theme_bw()+geom_text(vjust=-1)+theme(legend.position='none',axis.ticks=element_blank(),panel.grid.major=element_blank())+ggtitle("My Choice Accounts by Owner Age\n(% of All Accounts)")+scale_x_discrete('Account Owner Age',labels=function(x) str_wrap(x,width=5))+scale_y_continuous('% of Accounts',labels=percent)

ch3 = combo1b %>% filter(stype=='My Choice' & !is.na(tenure)) %>% ggplot(aes(x=tenure,y=P,group=1,label=percent(round(P,2)))) + geom_bar(stat='identity',fill='lightgreen') + stat_smooth(method='lm',se=F,color='red',size=1,linetype=2)+theme_bw()+geom_text(vjust=-1)+theme(legend.position='none',axis.ticks=element_blank(),panel.grid.major=element_blank())+ggtitle("My Choice Accounts by Tenure\n(% of All Accounts)")+scale_x_discrete('Account Tenure (Years)',labels=function(x) str_wrap(x,width=5))+scale_y_continuous('% of Accounts',labels=percent)

#ggplot(combo1,aes(x=age1,y=rate,color=stype,group=stype))+geom_line()+geom_point()+theme(legend.position='bottom')

ch4 = combo1c %>% filter(  cbr %in% c('WNY','Roch','Balt','Ches A','NYC','DE') & !is.na(cbr)) %>% ggplot(aes(x=age1,y=rate,fill=cbr,group=1,label=comma(round(rate,1))))+geom_bar(stat='identity')+facet_wrap(~cbr,ncol = 2)+stat_smooth(method='lm',se=F,color='red',linetype=2,size=1)+geom_text(vjust=-1)+coord_cartesian(ylim=c(0,10))+scale_y_continuous('Complaint Rate per 1,000 Accounts',labels=NULL)+scale_x_discrete("Account Owner Age")+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(),panel.grid.major=element_blank())+ggtitle("Complaint Rate by Owner Age and Region - Selected Regions")

ch5 = combo1c %>% filter(stype=='My Choice' & !is.na(cbr)) %>% ggplot(aes(x=cbr,y=P,group=1,label=percent(round(P,2)))) + geom_bar(stat='identity',fill='orange',alpha=0.5) +theme_bw()+geom_text(vjust=-1)+theme(legend.position='none',axis.ticks=element_blank(),panel.grid.major=element_blank())+ggtitle("My Choice Accounts by Region\n(% of All Accounts)")+scale_x_discrete('Account Tenure (Years)',labels=function(x) str_wrap(x,width=5))+scale_y_continuous('% of Accounts',labels=percent)

ch6 = base %>% filter(stype %in% c('My Choice','EZ Choice','Free','My Choice Plus','Direct','My Choice Plus w/int','Select','Select w/int','Classic','Classic w/Int')) %>% ggplot(aes(x=reorder(stype,ACCT_AMT_BAL_FOR_PRIME,FUN=mean,na.rm=T),y=ACCT_AMT_BAL_FOR_PRIME,fill=stype))+geom_boxplot(notch=T,alpha=0.5)+coord_cartesian(ylim=c(0,15000))+scale_y_continuous("Average Balance (Dec 2014)",labels=dollar)+theme_bw()+theme(legend.position="none")+ggtitle('Balance Distribution by Package - Selected Packages')+scale_x_discrete('Checking Package')

ch7 = base %>% filter(stype %in% c('My Choice','EZ Choice','Free','My Choice Plus','Direct','My Choice Plus w/int','Select','Select w/int','Classic','Classic w/Int')) %>% ggplot(aes(x=age1,y=ACCT_AMT_BAL_FOR_PRIME,fill=age1))+geom_boxplot(notch=T,alpha=0.5)+coord_cartesian(ylim=c(0,15000))+scale_y_continuous("Average Balance (Dec 2014)",labels=dollar)+theme_bw()+theme(legend.position="none")+ggtitle('Balance Distribution by Owner Age - Selected Packages')+scale_x_discrete('Owner Age')


pdf('Z:/M&T Projects/OCA/Group1 Deep Dive.pdf',paper='USr',width=10.75,height = 8.25)
  ch1
  print(arrangeGrob(ch2,ch3,nrow=1))
  ch4
  ch5
  ch6
  ch7
dev.off()
