

#read the data that was collapsed togetehr in excel
setwd("Z:/M&T Projects/IFM")
sales <- read.csv('sales.csv')
leads <- read.csv('leads.csv')
contrib <- read.csv('contrib.csv')


#add the attriton to contribution numbers - so I can calculate the 3 year impact
contrib$retention <- c(.916,.959,.751,.692,.824,.663,.852,.816,.87,.951,.951,.975)


#now create it into long format, in tidy format
library(tidyr)
library(dplyr)
library(reshape2)
library(stringr)

names(leads) <- gsub(".","",names(leads),fixed=T)
leads$Trigger <- str_trim(leads$Trigger)
leads1 <- gather(leads,key,value,Test_7_2013:Control_10_2014)
leads1 <- filter(leads1,!grepl('DROP',key))
leads2 <- separate(leads1,key,c('group','month','year'),'_',convert=T)
names(leads2)[6] <- 'leads'
leads2$group <- tolower(leads2$group)
leads3 <- leads2 %>% filter(!(year == 2013 & month %in% 7:9) & !(year == 2014 & month %in% 10))
sum(is.na(leads3$leads))
leads3$leads[is.na(leads3$leads)] <- 0
table(leads3$month,leads3$year)  #check
leads4 <- leads3 %>% group_by(Trigger,Channel,group) %>% summarise(leads=sum(leads))

names(sales) <- gsub(".","",names(sales),fixed=T)
sales$Trigger <- str_trim(sales$Trigger)
#sales is missing SEC and INS at TBC as they can't be sold, but that is creating problems, so I will force as zero
sales1 <- dcast(melt(sales,id.vars=c('Trigger','Channel','Product')),Trigger+Channel+Product~variable,fun.aggregate = sum,drop=F,fill=0,na.rm=T)

sales2 <- gather(sales1,key,value,Test_7_2013:Control_10_2014,na.rm=F)
sales3 <- sales2 %>% filter(!grepl('DROP',key)) %>% separate(key,c('group','month','year'),'_',convert=T)
sales3$group <- tolower(sales3$group)
#sales4 <- inner_join(sales3,contrib,by='Product')
sales3$Channel <- toupper(sales3$Channel)

names(sales3)[7] <- 'sales'
sales4 <- sales3 %>% filter(!(year == 2013 & month %in% 7:9) & !(year == 2014 & month %in% 10))

#summarise for entire 12 months
sales5 <- sales4 %>% group_by(Trigger,Channel,Product,group) %>% summarise(sales=sum(sales))



analysis <- merge(sales5,leads4,by=c('Trigger','Channel','group'),all.x=T)

analysis[which(is.na(analysis$leads) & analysis$sales > 0),]
analysis[which(analysis$leads==0 & analysis$sales > 0),]


analysis$sales[which(is.na(analysis$leads) & analysis$sales > 0)] <- NA   #this is illogical so it needs to be fixed

#Now I need to put the control and test side by side so I can calculate incremental and also the prop.test

analysis1 <- gather(analysis,measure,value,sales:leads)
#analysis1$aux <- paste(analysis1$measure,analysis1$group,sep='_')
#analysis2 <- spread(analysis1[,-c(5,7)],aux,value)
analysis2 <- dcast(analysis1,Trigger+Channel+Product~measure+group)


analysis2$response_test <- ifelse(analysis2$leads_test!=0,analysis2$sales_test/analysis2$leads_test,0)
analysis2$response_control <- ifelse(analysis2$leads_control!=0,analysis2$sales_control/analysis2$leads_control,0)
analysis2$incremental <- analysis2$response_test - analysis2$response_control


mytest <- function(x1,x2,n1,n2) {
  if(n1 >0 & n2 >0) {prop.test(c(x1,x2),c(n1,n2),alternative='g')$p.value} else NA
}


analysis3 = analysis2 
analysis3$p_value=apply(analysis3[c("sales_test","sales_control","leads_test","leads_control")],1, function(x) mytest(x[1],x[2],x[3],x[4]))


#merge the contrib py product
analysis3 <- inner_join(analysis3,contrib,by='Product')

#calc incremental accts at 95% and 90% and the incr contrib, then aggregate by trigger 
analysis3$increm95 <- ifelse(!is.na(analysis3$p_value) & analysis3$p_value <= 0.05,analysis3$incremental,0)
analysis3$increm90 <- ifelse(!is.na(analysis3$p_value) &analysis3$p_value <= 0.1,analysis3$incremental,0)


#3 year impact assumes 50% on year 1 as leads sell evenly + full or reatined on year 2 and then full*retained on year 3
# the formula for the 3 years is 1.5 + 2*retention + retention^2


analysis3 <- analysis3 %>% mutate(incr_contr_95 =floor(increm95*leads_test)*Contribution,
                                  incr_contr_90 =floor(increm90*leads_test)*Contribution,
                                  incr_contrib_iqr = floor(incremental*leads_test)*Contribution) %>% 
  mutate(incr_contr_95_3y=incr_contr_95*(1.5+2*retention+retention^2),
         incr_contr_90_3y=incr_contr_90*(1.5+2*retention+retention^2),
         incr_contrib_iqr_3y=incr_contrib_iqr*(1.5+2*retention+retention^2))

analysis3 %>% group_by(Channel,Trigger) %>% summarise(incr_contr_90=sum(incr_contr_90),incr_contr_95=sum(incr_contr_95))
analysis3 %>% group_by(Channel) %>% summarise(incr_contr_90=sum(incr_contr_90),incr_contr_95=sum(incr_contr_95),incr_contrib_iqr=sum(incr_contrib_iqr))

analysis3 %>% group_by(Channel,Trigger) %>% summarise_each(funs(sum),incr_contr_95:incr_contrib_iqr_3y)
analysis3 %>% group_by(Trigger) %>% summarise_each(funs(sum),incr_contr_95:incr_contrib_iqr_3y)
analysis3 %>% group_by(Channel) %>% summarise_each(funs(sum),incr_contr_95:incr_contrib_iqr_3y)
analysis3  %>% summarise_each(funs(sum),incr_contr_95:incr_contrib_iqr_3y)


#caclulate overall lift for what is worth

analysis4 <- analysis3 %>% group_by(Channel,Trigger) %>% summarise_each(funs(sum),sales_control:leads_test) %>%
  mutate(lift=(sales_test/leads_test)-(sales_control/leads_control))

analysis4$p_value=apply(analysis4[c("sales_test","sales_control","leads_test","leads_control")],1, function(x) mytest(x[1],x[2],x[3],x[4]))

write.table(analysis4,'clipboard',sep='\t',row.names=F)

save.image('trigger_sizing.rdata')
