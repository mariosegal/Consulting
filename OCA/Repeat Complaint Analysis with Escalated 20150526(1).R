library(dplyr)
library(tidyr)
library(ggplot2)

load('Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')



### Analyze the repeat for escalated only

repeat_all = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) ) %>% group_by(type,account_master) %>% summarise(N=n()) %>% group_by(type,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P))
write.table(repeat_all,'clipboard-128',sep='\t',row.names=F)


repeat_all1 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) ) %>% group_by(account_master) %>% summarise(N=n()) %>% group_by(N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P))
write.table(repeat_all1,'clipboard-128',sep='\t',row.names=F)

analysis_base = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) ) 
analysis_base$level_1=factor(analysis_base$level_1)
analysis_base$level_2 =factor(analysis_base$level_2)
analysis_base$level_3 =factor(analysis_base$level_3)

analysis_base$level_3_orig = analysis_base$level_3
#we have a mnir issue with the credit reportig for overdraft, I need to exclude it, because they are in another category
analysis_base = subset(analysis_base, !( level_3=='credit reporting' ))

#synch the level_2 for mortgage
levels(analysis_base$level_2)[levels(analysis_base$level_2) %in% c('conventional arm','conventional fixed','fha','other mortgage')] = 'mortgage'
analysis_base$type = gsub(" ","_",analysis_base$type)


repeated1 = analysis_base %>% group_by(account_master, level_1,level_2,level_3,type) %>% summarise(complaints=n()) %>% spread(type,complaints,fill=0) %>% mutate(complaints=escalated+non_escalated)

repeated1$group = NA
repeated1$group[repeated1$non_escalated>0 & repeated1$escalated>0] = 'both'
repeated1$group[repeated1$non_escalated>1 & repeated1$escalated==0] = 'non_escalated'
repeated1$group[repeated1$non_escalated==0 & repeated1$escalated>1] = 'escalated'
repeated1$group[(repeated1$non_escalated==0 & repeated1$escalated==1) | (repeated1$non_escalated==1 & repeated1$escalated==0) ] = 'unique'


repeated2 = repeated1 %>% group_by(level_1,level_2,level_3,group) %>% summarise(accts=n(),complaints=sum(complaints)) %>% group_by(level_1,level_2,level_3) %>% mutate(P=accts/sum(accts)) %>% gather(measure,value,accts:P) %>% unite(key,measure,group) %>% spread(key,value,fill=0) %>% mutate(total=accts_unique+accts_both+accts_escalated+accts_non_escalated) %>% arrange(level_1,level_2,P_unique)
write.table(repeated2,'clipboard-128',sep='\t',row.names=F)

repeated3 = repeated2 %>% select(-starts_with('P')) %>% group_by(level_1,level_2) %>% summarise_each(funs(sum),-starts_with("total")) %>% mutate(total=accts_both+accts_unique+accts_escalated+accts_non_escalated,P_both=accts_both/total,P_escalated=accts_escalated/total,P_non_escalated=accts_non_escalated/total,P_unique=accts_unique/total) 
write.table(repeated3,'clipboard-128',sep='\t',row.names=F)

repeat2 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') %>% group_by(account_master,level_1) %>% summarise(N=n()) %>% group_by(level_1,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P),tot=sum(N1)) %>% arrange(level_1,desc(tot))
write.table(repeat2,'clipboard-128',sep='\t',row.names=F)


repeat3 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') %>% group_by(account_master,level_1,level_2) %>% summarise(N=n()) %>% group_by(level_1,level_2,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P),tot=sum(N1)) %>% arrange(level_1,desc(tot))
write.table(repeat3,'clipboard-128',sep='\t',row.names=F)





repeat4 = analysis_base %>% group_by(account_master,level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(level_1,level_2,level_3,complaints) %>% summarise(lines=n(),accts=n_distinct(account_master)) %>% mutate(P=accts/sum(accts),cum1=cumsum(P),tot=sum(accts))  
write.table(repeat4,'clipboard-128',sep='\t',row.names=F)

repeat4a = analysis_base %>% group_by(account_master,level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(level_1,level_2,level_3,complaints) %>% summarise(lines=n(),accts=n_distinct(account_master)) %>% mutate(P=accts/sum(accts),cum1=cumsum(P),tot=sum(accts))  %>% filter(complaints==1) %>% arrange(level_1,P)
write.table(repeat4a,'clipboard-128',sep='\t',row.names=F)


#we want to understand the distribution by numbe rof complaints
#first # per account
#then more granular by cateogry 

hist1 = analysis_base %>% group_by(account_master) %>% summarise(complaints=n()) %>% group_by(complaints) %>% summarise(N=n())
write.table(hist1,'clipboard-128',sep='\t',row.names=F)


hist2 = analysis_base %>% group_by(account_master,level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(complaints) %>% summarise(N=n())
write.table(hist2,'clipboard-128',sep='\t',row.names=F)


#can we see if the NSF recontacts are related to multiple events?
#one quick way is to see thenumber of months in 2014 that they had NSF, and perhaps the total number of fees
#then do a distribution

load('z:/M&T Projects/OCA/nsf_extra_14.rdata')
nsf = nsf_extra_14 %>% mutate(tot=(ACCT_NSF_TOTAL_CHECK+ACCT_NSF_TOTAL_OTHER)) %>% group_by(EXPRESSION_8) %>% summarise(events=sum(tot),months=sum(tot>0))

analysis_base = left_join(analysis_base,nsf)

nsf_repeat = analysis_base %>% filter(level_3 == 'nsf fees - problems caused by low funds' & level_2=='checking account') %>% group_by(account_master) %>% summarise(complaints=n())

nsf_repeat1 = nsf_repeat %>% filter(complaints>1) %>% left_join(nsf,by=c("account_master"='EXPRESSION_8'))  %>% mutate(Complaints=cut(complaints,c(0,2,3,Inf),labels=c('2','3','4+')),months1=cut(months,c(-Inf,0,1,4,6,9,12),labels=c('None','1','2 to 3','4 to 6','7 to 9','10 to 12')),events1=cut(events,c(-Inf,0,1,3,5,10,15,25,Inf)),group=ifelse(complaints>events,'more','less_same'),group1=ifelse(complaints>months,'more','less_same'),groupa=cut(events,c(-Inf,0,1,2,3,4,5,6,7,8,9,Inf),c('0','1','2','3','4','5','6','7','8','9','10+'),ordered_result = T))

ggplot(subset(nsf_repeat1,!is.na(months1)),aes(x=months1,fill=Complaints))+geom_bar()+facet_grid(Complaints~.,labeller=label_both,as.table=F)+theme_bw()+theme(legend.position='none')+scale_x_discrete('Number of Months with NSF Items - 2014')+scale_y_continuous("Number of Accounts with Repeat Complaints")
ggsave('z:/M&T Projects/OCA/repeat nsf chart.png',width = 9,height = 5,units='in')


ggplot(nsf_repeat1,aes(x=events1,fill=Complaints))+geom_bar()+facet_grid(Complaints~.,labeller=label_both,as.table=F)+theme_bw()+theme(legend.position='none')+scale_x_discrete('Number of Months with NSF Items - 2014')+scale_y_continuous("Number of Accounts with Repeat Complaints")

ch1 = ggplot(subset(nsf_repeat1,!is.na(months)),aes(x=factor(months),fill=group,group=group))+geom_bar()+theme_bw()+theme(legend.position='bottom')
ch2 = ggplot(subset(nsf_repeat1,!is.na(events)),aes(x=groupa,fill=group1,group=group1))+geom_bar()+theme_bw()+theme(legend.position='bottom')

pdf('z:/M&T Projects/OCA/complaint hist v2 20150603.pdf',width=11,height=8,paper = 'USr')
ch1
ch2
dev.off()

nsf_repeat2 = nsf_repeat1 %>% group_by(groupa,group1) %>% summarise(N=n()) %>% filter(!is.na(group1)) %>% spread(group1,N,fill='')
write.table(nsf_repeat2,'clipboard-128',sep='\t',row.names=F)

nsf_repeat3 = nsf_repeat1 %>% group_by(months,group) %>% summarise(N=n()) %>% filter(!is.na(group)) %>% spread(group,N,fill='')
write.table(nsf_repeat3,'clipboard-128',sep='\t',row.names=F)


#deloitte asked for a trend of recontacts for NSf by month - maybe if I filter those and take out the first one 
#it will work
#analysis_base$open_time = as.Date(analysis_base$open_time,'%Y-%m-%d')
analysis_base$date = as.Date(NA)
analysis_base$date[analysis_base$type=='escalated'] = as.Date(analysis_base$open_time[analysis_base$type=='escalated'],'%Y-%m-%d')
analysis_base$date[analysis_base$type!='escalated'] = as.Date(analysis_base$entered_date[analysis_base$type!='escalated'],'%Y-%m-%d')



#analysis_base$date = as.Date(analysis_base$date,origin = '1970-01-01')

trend1 = analysis_base %>% filter(level_3 == 'nsf fees - problems caused by low funds' & level_2=='checking account') %>% select(c(account_master,date,type)) %>%  group_by(account_master) %>% mutate(complaints=n()) %>% arrange(account_master,date) 

#some are messed up in the format

trend1$date[format(trend1$date,'%Y')=='0015'] = 
  as.Date(paste0('2015-',
                 format(trend1$date[format(trend1$date,'%Y')=='0015'],'%m'),'-',
                 format(trend1$date[format(trend1$date,'%Y')=='0015'],'%d')))


trend1 = trend1 %>% arrange(account_master,date) %>% mutate(seq1=1:n(),delta=date-lag(date))

trend1 %>% filter(seq1 <=4 & seq1>1)  %>% group_by(period=format(date,'%Y%m')) %>% summarise(N=n()) %>% ggplot(aes(x=period,y=N,group='identity'))+geom_line()


trend1 %>% filter(seq1 <=4 & seq1 >1 )  %>% group_by(period=format(date,'%Y-%m'),seq1) %>% summarise(N=n()) %>% ggplot(aes(x=period,y=N,color=factor(seq1),group=factor(seq1)))+geom_line()+theme_bw()+theme(legend.position='bottom')+scale_x_discrete('Month of Repeated Complaint')+scale_y_continuous('Number of Repeated Complaints')+scale_color_discrete("Complaint \nNumber")
ggsave('z:/M&T Projects/OCA/repeat nsf trend.png',width = 9,height = 5,units='in')

trend_days=trend1 %>% filter(seq1 <=4 & seq1 >1 ) %>% group_by(seq1,days=round(as.numeric(delta))) %>% summarise(N=n()) 
trend_days$days1 = cut(trend_days$days,c(-Inf,0,1,2,7,30,60,90,Inf),labels=c('Same Day','Next Day','2 Days Later','3 to 7 Days Later','2nd Week','3rd/4th Week','2nd Month','3rd Month+'),ordered_result = T)

write.table(trend_days,'clipboard-128',sep='\t',row.names=F)


trend_days1=trend1 %>% filter(seq1 <=4 & seq1 >1 ) %>% group_by(seq1,type,days=round(as.numeric(delta))) %>% summarise(N=n()) %>% spread(type,N,fill=0)

trend_days1$days1 = cut(trend_days1$days,c(-Inf,0,1,2,7,30,60,90,Inf),labels=c('Same Day','Next Day','2 Days Later','3 to 7 Days Later','2nd Week','3rd/4th Week','2nd Month','3rd Month+'),ordered_result = T)
write.table(trend_days1,'clipboard-128',sep='\t',row.names=F)
