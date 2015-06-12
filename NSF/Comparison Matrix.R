#Load Data

load("Z:/M&T Projects/NSF/base.rdata")
load("Z:/M&T Projects/NSF/nsf_accts.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")
load("Z:/M&T Projects/NSF/extra.rdata")


#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)


#1 summarize the nsf data by acct and by year
nsf_summary <- nsf_accts %>% group_by(EXPRESSION_8,year=substr(period,1,4)) %>% summarise(months=sum(ACCT_NSF_TOTAL>0),fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),waived_n=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED>0),tot_num=sum(ACCT_NSF_TOTAL),mean_num=mean(ACCT_NSF_TOTAL),mean_fees=mean(ACCT_CONTR_TOTAL_NSF_FEES),max_fees=max(ACCT_CONTR_TOTAL_NSF_FEES),max_num=max(ACCT_NSF_TOTAL),waived=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,na.rm=T))

nsf_summary <- nsf_summary %>% gather(variable,value,-c(EXPRESSION_8:year)) %>% unite(key,variable,year) %>% spread(key,value,fill=0)


#do some clean-up to solve accts closed twice and accts 2 times on base
sum(duplicated(closed_accts$EXPRESSION_8))
sum(duplicated(base$EXPRESSION_8))

weird <- union(base$EXPRESSION_8[which(duplicated(base$EXPRESSION_8))],closed_accts$EXPRESSION_8[which(duplicated(closed_accts$EXPRESSION_8))])

#we will exclude any in weird later on

#2 select only the accounts that had 1+ year tenure in 201301, because first we wantt o analye the existing book
base$open_date <- as.Date(base$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
table(format(base$open_date,"%Y"))
sum(base$open_date < '1950-01-01')

#there are some accts with suspect open dates, for simplicity let's  take only accts opened in or after 1950, this excludes 879 accts only 
existing_book <- subset(base,open_date <= '2012-01-01' & open_date >= '1950-01-01')

#some closed in 2012 of course so I need to exclude those
closed_2012 <- subset(closed_accts,period >= '201201' & period <= '201212')

existing_book <- subset(existing_book,!(EXPRESSION_8 %in% closed_2012$EXPRESSION_8))
length(intersect(existing_book$EXPRESSION_8,closed_accts$EXPRESSION_8))/dim(existing_book)[1]  #implies 14% attrtion for the book from 2013 to end 2014, this is reasonable to me

existing_book <- left_join(existing_book,closed_accts)
names(existing_book)[5] <- "closed_period"

existing_book <- subset(existing_book,!(EXPRESSION_8 %in% weird))

#merge with the nsf stats
existing_book <-left_join(existing_book,nsf_summary,by='EXPRESSION_8')

#some accts were not in 2014 at all, I want to tag them, I need to be able to group them as attrited for 2014
existing_book$lost_2013 <- ifelse(existing_book$EXPRESSION_8 %in% closed_accts$EXPRESSION_8[closed_accts$period >='201301' & closed_accts$period <='201312'],1,0)

#if the acct never had an NSF in the period studies, then when I merged with nsf_sumamry that generated NAs, but they are really zeros - so I need to convert those. This is why I wanted the flag lost_2013, for those not lost in 2013 I can safely make it zero, if lost in 2013 I want to exclude them anyway,
#in fact make all  the NAs be zero, as they did not NSf, except for 2014 fields when lost_2013 ==1

existing_book <- existing_book[c(1:5,seq(6,32,by=3),seq(7,32,by=3),seq(8,32,by=3),33)]  #reorder to deal with 2014 easier
existing_book[,6:23][is.na(existing_book[,6:23])] <- 0
existing_book[existing_book$lost_2013==0,24:32][is.na(existing_book[existing_book$lost_2013==0,24:32])] <- 0

#there are thsoe lost in 2013 who have zeros on th ensf data for 2014 from the spread operation before
#these have to be made NA
existing_book[existing_book$lost_2013==1,24:32] <- NA

#define groups
existing_book$group_2013 <- NA
existing_book$group_2013 <- ifelse(existing_book$tot_num_2013==0,'No NSF',existing_book$group_2013)  
existing_book$group_2013 <- ifelse(is.na(existing_book$group_2013) & existing_book$months_2013 %in% 1 ,'Accidental',existing_book$group_2013 )
existing_book$group_2013 <- ifelse(is.na(existing_book$group_2013) & existing_book$months_2013 %in% 2:4 ,'Ocassional',existing_book$group_2013 )
existing_book$group_2013 <- ifelse(is.na(existing_book$group_2013) & existing_book$months_2013 %in% 5:12 ,'Frequent',existing_book$group_2013 )

table(existing_book$group_2013,useNA='ifany')


existing_book$group_2014 <- NA
existing_book$group_2014 <- ifelse(existing_book$tot_num_2014==0,'No NSF',existing_book$group_2014)  
existing_book$group_2014 <- ifelse(is.na(existing_book$group_2014) & existing_book$months_2014 %in% 1 ,'Accidental',existing_book$group_2014 )
existing_book$group_2014 <- ifelse(is.na(existing_book$group_2014) & existing_book$months_2014 %in% 2:4 ,'Ocassional',existing_book$group_2014 )
existing_book$group_2014 <- ifelse(is.na(existing_book$group_2014) & existing_book$months_2014 %in% 5:12 ,'Frequent',existing_book$group_2014 )

table(existing_book$group_2014,existing_book$lost_2013,useNA='ifany')  #sanity - all NA have to be lost in 2013

#all left attrited in 2013
existing_book$group_2014 <- ifelse(is.na(existing_book$group_2014),'lost 2013',existing_book$group_2014 )
                                   
table(existing_book$group_2014,useNA='ifany')

#CREATE FACTORS ORDERED
existing_book$group_2013 <- factor(existing_book$group_2013,levels=c('No NSF','Accidental','Ocassional','Frequent'))
existing_book$group_2014 <- factor(existing_book$group_2014,levels=c('No NSF','Accidental','Ocassional','Frequent','lost 2013'))


existing_book$lost_2014 <- ifelse(existing_book$closed_period >= '201401',1,0)


existing_book$with_2013 <- ifelse(existing_book$tot_num_2013>0,1,0)
existing_book$with_2014 <- ifelse(existing_book$tot_num_2014>0 & !is.na(existing_book$tot_num_2014),1,0)
  
  
#create matrix data
prop.table(table(existing_book$group_2013,existing_book$group_2014),1)

           
matrix_data <- existing_book %>% group_by(group_2013,group_2014) %>% 
  summarise_each(funs(sum(.,na.rm=T),mean(.,na.rm=T)),-c(ACCT_STYPE:closed_period))

matrix_n <- existing_book %>% group_by(group_2013,group_2014) %>% summarise(N=n()) %>% gather(variable,value,-c(group_2013:group_2014))

matrix_data1 <- matrix_data %>% gather(variable,value,-c(group_2013:group_2014))

matrix_data2 <- rbind(matrix_n,matrix_data1)
write.table(matrix_data2,'clipboard-128',sep='\t',row.names=F)


#statisitcs that they will like
table(existing_book$months_2013,cut(existing_book$tot_num_2013,c(0,.001,1,2,3,4,5,10,15,Inf),include.lowest = T,dig.lab = 8))



#account for all NSFs in 2013
summary1 <- nsf_accts %>% group_by(year=substr(period,1,4)) %>% summarise_each(funs(sum),-c(EXPRESSION_8,period))
write.table(summary1,'clipboard-128',sep='\t',row.names=F)


#data for same pages for existing
summary2 <- existing_book %>% group_by(group_2014) %>% mutate(N=1,with=ifelse(tot_num_2013>=1,1,0)) %>% summarise_each(funs(sum),c(fees_2013,fees_2014,tot_num_2014,tot_num_2013,waived_2013,waived_2014,N,with))
write.table(summary2,'clipboard-128',sep='\t',row.names=F)


existing_book  %>% mutate(N=1,with=ifelse(tot_num_2013>=1,1,0)) %>% summarise_each(funs(sum),c(fees_2013,fees_2014,tot_num_2014,tot_num_2013,waived_2013,waived_2014,N,with))


#how many accts NSFd per year
nsf_accts %>% group_by(year=substr(period,1,4)) %>% summarise(N=n_distinct(EXPRESSION_8))


#is there a tredn in the type of NSF

extra$period <- as.character(extra$period)
extra %>% filter(period >= '201304') %>% group_by(period) %>% 
  summarise_each(funs(sum),ACCT_NSF_TOTAL_CHECK:ACCT_NSF_TOTAL_OTHER) %>% 
  gather(measure,value,ACCT_NSF_TOTAL_CHECK:ACCT_NSF_TOTAL_OTHER) %>% 
  ggplot(aes(x=(period),y=value,color=measure,group=measure))+geom_line()+stat_smooth(method='lm',se=F,linetype=2)

type_nsf <- extra %>% filter(period >= '201304') %>% group_by(period) %>% summarise_each(funs(sum),ACCT_NSF_TOTAL_CHECK:ACCT_NSF_TOTAL_OTHER) %>% gather(measure,value,ACCT_NSF_TOTAL_CHECK:ACCT_NSF_TOTAL_OTHER) %>% spread(measure,value)

lm(ACCT_NSF_TOTAL_CHECK~ as.numeric(as.factor(type_nsf$period)),data=type_nsf)
lm(ACCT_NSF_TOTAL_OTHER~ as.numeric(as.factor(type_nsf$period)),data=type_nsf)

write.table(type_nsf,'clipboard-128',sep='\t',row.names=F)





#NSF dist mature port per year 

number_year <- existing_book %>% filter(group_2014 !='lost 2013') %>%  select(contains("tot_num")) %>% 
  gather(variable,value,tot_num_2012:tot_num_2014) %>% separate(variable,c('variable1','xx','year'),sep="_") %>%
  group_by(year,value) %>% summarise(N=n()) %>% group_by(year) %>% mutate(p=N/sum(N))  %>% 
  select(-N) %>% spread(year,p,fill=0)

write.table(number_year,'clipboard-128',sep='\t',row.names=F)
