

#To measure attrition I will do the matching to  201312 - then for those accts 
# see how many remained by 201412, by month of complaint
#as a comparison I will use other accts with NSF at the same point (this is imperfect, but the NSF date is not present)


load("Z:/M&T Projects/OCA/accts_201412_oca.rdata")
load('Z:/M&T Projects/OCA/accts_201312_oca.rdata')


data <- complaints %>% filter(level_3=='NSF Fees - Problems caused by low funds' & level_2=='Checking account')
dim(data)[1]
sum(is.na(data$mask_acct) & is.na(data$mask_ssn))
data1 <- filter(data,!is.na(mask_acct) | !is.na(mask_ssn))

#part1 - do the ones where I have an acct
part1 <- subset(data1,!is.na(mask_acct),select=c('mask_acct','complaint_master_id'))
sum(duplicated(part1$mask_acct))
length(unique(part1$mask_acct))

match1a <- inner_join(part1,accts_201312_oca,by=c('mask_acct'='EXPRESSION_8'))
length(unique(match1a$ACCT_ID))
sum(duplicated(match1a$ACCT_ID))

sum(part1$mask_acct %in% accts_201312_oca$EXPRESSION_8)
sum(!(part1$mask_acct %in% accts_201312_oca$EXPRESSION_8))  #not in dec 2013


#part2 - try the ones with ssn
part2 <- subset(data1,is.na(mask_acct),select=c('mask_ssn','complaint_master_id'))
sum(duplicated(part2$mask_ssn))
length(unique(part2$mask_ssn))
sum(duplicated(part2$complaint_master_id))

match2a <- inner_join(part2,accts_201312_oca,by=c('mask_ssn'='EXPRESSION_18'))
match2a$mask_acct <- match2a$EXPRESSION_8


#how many matched multiple times by SSN - those are the ones that have duped maSTER_ID AS WE HAD ZERO BEFORE
length(unique(match2a$complaint_master_id)) # this are the number matched
length(setdiff(part2$complaint_master_id,match2a$complaint_master_id))  #not matched by SSN

dupes2a <- match2a$complaint_master_id[which(duplicated(match2a$complaint_master_id))]
length(unique(match2a$complaint_master_id[!(match2a$complaint_master_id %in% dupes2a)])) #one match
length(unique(match2a$complaint_master_id[(match2a$complaint_master_id %in% dupes2a)]))  #multiple

keep2a <- match2a[!(match2a$complaint_master_id %in% dupes2a),]  #it is impossible to analyze the accts for multiple ones
#I will analyze them at the hhld level though as well
drop2a <- match2a$EXPRESSION_8[(match2a$complaint_master_id %in% dupes2a)]

accts1a <- bind_rows(keep2a,match1a)  #this is the acct level analysis dataset
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Interest','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking Account','Retail Portfolio Management Account','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
accts1a$ACCT_STYPE <- factor(accts1a$ACCT_STYPE ,levels=stypes,labels=packages)
accts1a <- inner_join(accts1a,data1,by='complaint_master_id')


#I want to also add all ddas in dec12 that are not related to HHLDs that we dropped 
dda2013 <-accts_201312_oca %>% filter(ACCT_PTYPE=='DDA')
dda2013$nsf_fee <- "Drop"
dda2013$nsf_fee <- ifelse(dda2013$EXPRESSION_8 %in% accts1a$mask_acct.x,"Yes",dda2013$nsf_fee)
dda2013$nsf_fee <- ifelse((!(dda2013$ACCT_ID %in% match1a$ACCT_ID) & !(dda2013$ACCT_ID %in% match2a$ACCT_ID))& dda2013$nsf_fee=="Drop","No",dda2013$nsf_fee)

#merge nsf data
load('nsf2014.rdata')
dda2013 <- left_join(dda2013,nsf2014,by='EXPRESSION_8')

dda2013$nsf_fee <- ifelse(dda2013$nsf_fee=="No" & dda2013$ACCT_NSF_TOTAL >0,"No with Fee",dda2013$nsf_fee)
dda2013$nsf_fee <- ifelse(is.na(dda2013$nsf_fee) ,"No NSF Fee",dda2013$nsf_fee)
dda2013$flag_2013 = 1




#now merge the accts1a data
accts1a <- accts1a[-18]
names(accts1a)[8] <- "mask_acct"

attrition_data <- left_join(dda2013,accts1a,by=c('EXPRESSION_8'='mask_acct'))

#some dupes
sum(duplicated(attrition_data$EXPRESSION_8))

attrition_data <- attrition_data[order(attrition_data$EXPRESSION_8,attrition_data$complaint_date),]

attrition_data <- attrition_data[-which(duplicated(attrition_data$EXPRESSION_8)),]


#now flag those that are still present in 201412
attrition_data$flag_2014 <- ifelse(attrition_data$EXPRESSION_8 %in% accts_201412_oca$EXPRESSION_8[accts_201412_oca$ACCT_PTYPE=='DDA'],1,0)


#I need one more thing, I need to tag the accts by month of NSF and or complaint month to do the comparison

load("Z:/M&T Projects/NSF/contrib.rdata")

nsf_months <- contrib %>% filter(ACCT_CONTR_TOTAL_NSF_FEES>0 & period > '201312') %>% group_by(EXPRESSION_8,period) %>%
  select(EXPRESSION_8,ACCT_CONTR_TOTAL_NSF_FEES,period) %>% group_by(EXPRESSION_8) %>% mutate(order=1:n()) %>% 
  arrange(EXPRESSION_8,period) %>% filter(order==1) # %>% mutate(period = substr(period,5,2))

nsf_months$month <- as.numeric(substr(nsf_months$period,5,6))

attrition_data <- left_join(attrition_data,nsf_months[c(1,5)],by='EXPRESSION_8')

#when they cmplianed we want to use the complaint month
attrition_data$month <- ifelse(attrition_data$nsf_fee=='Yes',as.numeric(format(attrition_data$complaint_date,'%m')),attrition_data$month)

attrition_data$month <- ifelse(attrition_data$nsf_fee=='No NSF Fee' ,NA,attrition_data$month)  #a few have months when of course they should have none

#Now we can measure attrition
attr <- attrition_data %>% group_by(nsf_fee,month) %>% summarise(dec13 = sum(flag_2013),dec14=sum(flag_2014)) %>%
  mutate(attr= (dec14/dec13)-1)   

#%>% filter(nsf_fee %in% c('Yes','No with Fee'))

attr  %>%  ggplot(aes(x=as.factor(month),fill=nsf_fee,y=attr))+geom_bar(position='dodge',stat='identity')


attr1 <- attrition_data %>% filter(month>=4) %>% group_by(nsf_fee) %>% 
  summarise(dec13 = sum(flag_2013),dec14=sum(flag_2014)) %>%
  mutate(attr= (dec14/dec13)-1)  %>% filter(nsf_fee %in% c('Yes','No with Fee')) 

write.table(attr,'clipboard-128',sep='\t',row.names=F)
write.table(attr1,'clipboard-128',sep='\t',row.names=F)


#####################################
#####################################
#####################################

#understand who attrited

#merge some data
load("Z:/M&T Projects/IFM/hhld_201312.rdata")
attrition_data <- left_join(attrition_data,hhlds_201312[c(1,23,68,72,54,45,19)],by=c('ACCT_ID.x'='HHLD_ID'))

load('Z:/M&T Projects/OCA/ifm_income_201312.rdata')

attrition_data <- left_join(attrition_data,ifm_income_201312,by=c('EXPRESSION_8'='EXPRESSION_1'))


attrition_data$income <- cut(attrition_data$INTELLIGENTSIA_EARNED_EST_ANNUAL_INCOME,
                             c(0,.01,10000,20000,40000,60000,80000,100000,Inf),dig.labs=8)


attrition_data$lost <- attrition_data$flag_2013 - attrition_data$flag_2014

packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Interest','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking Account','Retail Portfolio Management Account','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)

stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')

attrition_data$package <- factor(attrition_data$ACCT_STYPE.x ,levels=stypes,labels=packages)

attrition_data$HHLD_LIFE_CYCLE_SEGMENT[attrition_data$HHLD_LIFE_CYCLE_SEGMENT==8] <- 1
attrition_data$HHLD_LIFE_CYCLE_SEGMENT[attrition_data$HHLD_LIFE_CYCLE_SEGMENT==9] <- 4
attrition_data$HHLD_LIFE_CYCLE_SEGMENT[is.na(attrition_data$HHLD_LIFE_CYCLE_SEGMENT)] <- 7

attrition_data$segment <- factor(attrition_data$HHLD_LIFE_CYCLE_SEGMENT,levels=c(1:7),labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

attrition_data$age <- cut(attrition_data$HHLD_HH_OWN_AGE,c(0,seq(25,75,by=10),Inf))

attrition_data$tenure <- as.numeric(round((as.Date('2014-01-01')-
                                             as.Date(attrition_data$ACCT_DATE_OPENED_FOR_PRIME.x,'%m/%d/%Y'))/(365.25),1))

attrition_data$tenure <- (cut(attrition_data$tenure,c(0,.0001,1,2,3,4,5,7,10,15,20,Inf)))

                 
                 
                 
attrition_profile <- attrition_data %>% filter(nsf_fee %in% c('Yes','No with Fee') & month >=4) %>%
  select(c(EXPRESSION_8,lost,nsf_fee,package,segment,income,age,tenure)) %>% 
  gather(variable,level,package:tenure) %>% group_by(lost,nsf_fee,variable,level) %>% 
  summarise(N=n()) %>% spread(lost,N,fill=0)

attrition_profile <- attrition_profile %>% rename(lost=`1`,not=`0`) %>% mutate(tot=not+lost,rate=lost/tot) %>%
  group_by(nsf_fee,variable) %>% mutate(tot1=sum(tot),lost1=sum(lost)) %>% group_by(nsf_fee,variable,level) %>% 
  mutate(p=my_test(lost,lost1,tot,tot1)) 

tests_attr <- attrition_data %>% filter(nsf_fee %in% c('Yes','No with Fee') & month >=4) %>%
  select(c(EXPRESSION_8,lost,nsf_fee,package,segment,income,age,tenure)) %>% 
  gather(variable,level,package:tenure) %>% group_by(lost,nsf_fee,variable,level) %>% 
  summarise(N=n()) %>% spread(lost,N,fill=0) %>% rename(lost=`1`,not=`0`) %>%
  mutate(tot=not+lost,rate=lost/tot) %>% gather(measure,value,not:rate) %>% 
  group_by(variable,level,nsf_fee) %>% arrange(variable,level,nsf_fee) %>% 
  unite(key,nsf_fee,measure) %>% spread(key,value,fill=0) %>% group_by(variable,level) %>% 
  filter(Yes_lost>0 & `No with Fee_lost` >0 & Yes_tot >0 & `No with Fee_tot` >0 ) %>%
  mutate(p=my_test(Yes_lost,`No with Fee_lost`,Yes_tot,`No with Fee_tot`))

write.table(attrition_profile,'clipboard-128',sep='\t',row.names=F)
write.table(tests_attr,'clipboard-128',sep='\t',row.names=F)


#per nishith do no NSf but only for acct present in april or later
load("Z:/M&T Projects/OCA/closed_14.rdata")
attr_aux <- subset(attrition_data,!(EXPRESSION_8 %in% closed_14$EXPRESSION_8))


#this is attrtion for all accts present in 2014 april with NO NSF to end of year
attr_aux %>% filter(is.na(month)) %>% group_by(nsf_fee) %>% 
  summarise(dec13 = sum(flag_2013),dec14=sum(flag_2014)) %>%
  mutate(attr= (dec14/dec13)-1)  %>% filter(nsf_fee %in% c('No NSF Fee'))
| 