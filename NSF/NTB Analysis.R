#I will analyze changes in NTB - using 4Q 2012 accts and 4Q2013 accts (becayuse I get to folow them for 12 months)


#Assemble  data

new_accts <- subset(base,(ACCT_DATE_OPENED_FOR_PRIME >= '2012-10-01' & ACCT_DATE_OPENED_FOR_PRIME <= '2012-12-31') | (ACCT_DATE_OPENED_FOR_PRIME >= '2013-10-01' & ACCT_DATE_OPENED_FOR_PRIME <= '2013-12-31'))


new_accts <- left_join(new_accts,closed_accts,by='EXPRESSION_8')
names(new_accts)[4] <- "closed"

new_accts <- new_accts[-which(new_accts$EXPRESSION_8 %in% new_accts$EXPRESSION_8[which(duplicated(new_accts$EXPRESSION_8))]),]
sum(duplicated(new_accts$EXPRESSION_8 ))
length(unique(new_accts$EXPRESSION_8))

#I want to have up to 12 lines per acct <- one for each of its first 12 months, then append the nsf data to that
#there mayb be many ways ot do that, I will create columns with the 24 months and then melt

new_accts1 <- new_accts %>% mutate(p1=1,p2=2,p3=3,p4=4,p5=5,p6=6,p7=7,p8=8,p9=9,p10=10,p11=11,p12=12) %>% 
  gather (aux,value,p1:p12) 

new_accts1$period <- ifelse(as.numeric(format(new_accts1$ACCT_DATE_OPENED_FOR_PRIME,"%m")) + new_accts1$value - 1 <=12,
                            as.numeric(format(new_accts1$ACCT_DATE_OPENED_FOR_PRIME,"%Y%m")) + new_accts1$value - 1,
                            (as.numeric(format(new_accts1$ACCT_DATE_OPENED_FOR_PRIME,"%Y"))+1)*100 + 
                              (as.numeric(format(new_accts1$ACCT_DATE_OPENED_FOR_PRIME,"%m"))+new_accts1$value - 1)%%12)

new_accts1$period <- as.character(new_accts1$period )

length(unique(new_accts1$EXPRESSION_8))

#merge nsf data
new_accts2 <- left_join(new_accts1,nsf_accts)
length(unique(new_accts2$EXPRESSION_8))

#take out lines where acct was not present
new_accts3 <- new_accts2 %>% filter(period <= closed | is.na(closed))
length(unique(new_accts3$EXPRESSION_8))

# summarise by year and accts
new_summary <- new_accts3 %>% group_by(year=format(ACCT_DATE_OPENED_FOR_PRIME,'%Y'),closed,ACCT_STYPE,EXPRESSION_8) %>% 
  summarise(months=n(),nsf_months=sum(!is.na(ACCT_NSF_TOTAL)), fees=sum(ACCT_CONTR_TOTAL_NSF_FEES,na.rm=T),events=sum(ACCT_NSF_TOTAL,na.rm=T)) 

new_fee_months <- new_summary %>% group_by(year,nsf_months) %>% summarise(N=n()) %>% spread(year, N)
write.table(new_fee_months,'clipboard-128',sep='\t',row.names=F)

new_fee_events <- new_summary %>% group_by(year,events) %>% summarise(N=n()) %>% spread(year, N)
write.table(new_fee_events,'clipboard-128',sep='\t',row.names=F)

new_avg <- new_summary %>% group_by(year) %>% summarise(events_avg=mean(events),months_avg =mean(nsf_months),have=sum(events>=1)/n(),N=n())
write.table(new_avg,'clipboard-128',sep='\t',row.names=F)


#create attrition curves
new_accts3 <- new_accts3 %>% group_by(EXPRESSION_8) %>% mutate(events=sum(ACCT_NSF_TOTAL,na.rm=T),year=format(ACCT_DATE_OPENED_FOR_PRIME,'%Y'))

#by year
curves1 <- new_accts3 %>% group_by(year,value) %>% summarise(N=n()) %>% spread(year,N)
write.table(curves1,'clipboard-128',sep='\t',row.names=F)


#curve with NSF
new_accts3$with <- ifelse(new_accts3$events >=1 ,'nsf','no')
curves2 <- new_accts3 %>% group_by(year,with,value) %>% summarise(N=n()) %>% unite(key,with,year) %>% spread(key,N)
write.table(curves2,'clipboard-128',sep='\t',row.names=F)



#curve with NSF
new_accts3$with <- ifelse(new_accts3$events >=1 ,'nsf','no')
curves2 <- new_accts3 %>% group_by(year,with,value) %>% summarise(N=n()) %>% unite(key,with,year) %>% spread(key,N)
write.table(curves2,'clipboard-128',sep='\t',row.names=F)

#curve by number
new_accts3$events1 <- cut(new_accts3$events,c(0,0.01,1,2,3,4,5,10,20,40,60,Inf),include.lowest = T)
curves3 <- new_accts3 %>% group_by(year,events1,value) %>% summarise(N=n()) %>% unite(key,events1,year) %>% spread(key,N)
write.table(curves3,'clipboard-128',sep='\t',row.names=F)

#summary by year

new_stats <- new_accts3 %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=T),mean(.,na.rm=T)),c(ACCT_NSF_TOTAL:events))

write.table(new_stats,'clipboard-128',sep='\t',row.names=F)
new_stats1 <- new_accts3 %>% group_by(year) %>% summarise(N=n(),with1=sum(events >=1)/n(),with2=sum(events >=1))
write.table(new_stats1,'clipboard-128',sep='\t',row.names=F)


#curve by stype
new_accts3$stype <-  factor(new_accts3$ACCT_STYPE ,levels=stypes,labels=packages)

curves_stype <- new_accts3 %>% group_by(year,stype,value) %>% summarise(N=n()) %>% unite(key,stype,year) %>% spread(key,N)
write.table(curves_stype,'clipboard-128',sep='\t',row.names=F)
