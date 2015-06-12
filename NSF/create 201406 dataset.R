#####CREATE A DATASET FOR 201406

#load data
#load('model_data.rdata')
load("Z:/M&T Projects/NSF/base.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")

#load libraries
library(plyr)
library(dplyr)
library(tidyr)


base$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(base$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')  
model_data_new <- base
#take out any duplicared accts, they are weird
dupes <- which(duplicated(model_data_new$EXPRESSION_8))
model_data_new <- model_data_new[-which(model_data_new$EXPRESSION_8 %in% model_data_new$EXPRESSION_8[dupes]),]

names(closed_accts)[2] <- 'closed_period'
#some acct closed twice or more, I want to take those out, as they are weird, it is only 667 so  it is not big deal
dupes1 <- which(duplicated(closed_accts$EXPRESSION_8))
model_data_new <- model_data_new[-which(model_data_new$EXPRESSION_8 %in% closed_accts$EXPRESSION_8[dupes1]),]


model_data_new <- left_join(model_data_new,closed_accts)
table(is.na(model_data_new$closed_period))

rm(base);rm(closed_accts);rm(dupes,dupes1)

#for modeling I will work only with accts present as of 201406, any accts closed on or before are not included
sum(!is.na(model_data_new$closed_period) & model_data_new$closed_period <='201406')   #536,903  exclusions
model_data_new <- subset(model_data_new,is.na(closed_period) | closed_period >='201407')

#front book are those with less 1 year of tenure, so opened on 201306 or after
model_data_new$book <- ifelse(model_data_new$ACCT_DATE_OPENED_FOR_PRIME>='2013-06-01','front','back')
table(model_data_new$book,useNA='ifany')
View(model_data_new %>% group_by(period=format(model_data_new$ACCT_DATE_OPENED_FOR_PRIME,'%Y%m'),book) %>% summarise(N=n()) %>% spread(book,N))
load("Z:/M&T Projects/NSF/hhkey.rdata")
model_data_new <- inner_join(model_data_new,hhkey[-3])

# read and process the product data as of 201406, and then append it
accts_201406$ACCT_PTYPE <- as.character(accts_201406$ACCT_PTYPE )
prods <- accts_201406 %>% mutate(ACCT_PTYPE = ifelse(ACCT_STYPE %in% c('REW','NOR','SIG'),'CRD',ACCT_PTYPE)) %>% 
  filter(ACCT_PTYPE %in% c('DDA','SAV','MMS','TDA','IRA','CRD','ILN','CCS','MTG','SEC','HEQ','INS')) %>%
  group_by(ACCT_ID,ACCT_PTYPE) %>% summarise(penet=n_distinct(ACCT_PTYPE),bal=sum(ACCT_AMT_BAL_FOR_PRIME+ACCT_MARKET_VALUE)) %>%
  gather(var,value,penet:bal) %>% arrange(ACCT_ID,var,ACCT_PTYPE) %>% unite(key,ACCT_PTYPE,var,sep='_') %>%
  spread(key,value,fill=0)

prods$prods <- rowSums(prods[,grepl('penet',names(prods))])

model_data_new <- inner_join(model_data_new,prods)
  
rm(accts_201406,prods,hhkey)

#now add the HHLD data
load("Z:/M&T Projects/NSF/hhld_201406.rdata")
model_data_new <- left_join(model_data_new,hhld_201406[c(1,15,19,48,53,67:68,71)],by=c('ACCT_ID'='HHLD_ID'))

#create the factors for cbr, segment, convert tenure to years,

model_data_new$segment = model_data_new$HHLD_LIFE_CYCLE_SEGMENT
model_data_new$segment <- as.character(model_data_new$segment )
model_data_new$segment[model_data_new$segment==8] <- 1
model_data_new$segment[model_data_new$segment==9] <- 4
model_data_new$segment[is.na(model_data_new$segment)] <- 7
model_data_new$segment <- factor(model_data_new$segment,
                             levels=c(1:7),
                             labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

model_data_new$cbr <- model_data_new$HHLD_COMMUNITY_BANK_MARKET
model_data_new$cbr[is.na(model_data_new$cbr)] <- 99
model_data_new$cbr <- factor(model_data_new$cbr,levels=c(1:17,99),
                         labels=c('WNY','Roch','Syr','South','Alb','Tarry','NYC','Phil','PA_N','C&W_PA',
                                  'SEPA','Balt','ChesA','Wash','ChesB','C_VA','DE','OOM'))


model_data_new$hh_tenure <- model_data_new$HHLD_TENURE_DAYS/365
model_data_new$dda_tenure <- as.numeric((as.Date('2014-06-30')- model_data_new$ACCT_DATE_OPENED_FOR_PRIME)/365)

model_data_new <- model_data_new[-which(names(model_data_new) %in% c('HHLD_LIFE_CYCLE_SEGMENT','HHLD_TENURE_DAYS','HHLD_COMMUNITY_BANK_MARKET'))]

rm(hhld_201406)

#add IXI
load('Z:/M&T Projects/NSF/ixi_201406.rdata')

model_data_new <- left_join(model_data_new,ixi_201406[c(1,6)],by=c('ACCT_ID'='IX-ID'))
names(model_data_new)[40] = 'ixi_assets'
rm(ixi_201406)

#add ifm
load("Z:/M&T Projects/NSF/ifm_201406.rdata")
ifm_aux <- subset(ifm_201406,,c('EXPRESSION_1','INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH','INTELLIGENTSIA_PRIMARY_BANK_INDICATOR','INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO','INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO','INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME','INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO','INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO','INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT','INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT','INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME'))
library(stringr)
ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME <- str_trim(ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME)
ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME <- str_trim(ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME)
ifm_aux$ext_bank<- ifelse(ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME != '',1,0)
ifm_aux$ext_sec <- ifelse(ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME != '',1,0)

model_data_new <- left_join(model_data_new,ifm_aux[-c(6,11)],by=c('EXPRESSION_8'='EXPRESSION_1'))
rm(ifm_201406,ifm_aux)


model_data_new$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO[is.na(model_data_new$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO)] <- 0

model_data_new$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO[is.na(model_data_new$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO)] <- 0

model_data_new$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO[is.na(model_data_new$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO)] <- 0
model_data_new$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO[is.na(model_data_new$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO)] <- 0
model_data_new$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT[is.na(model_data_new$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT)] <- 0
model_data_new$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT[is.na(model_data_new$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT)] <- 0

model_data_new$ext_bank[is.na(model_data_new$ext_bank)] <- 0
model_data_new$ext_sec[is.na(model_data_new$ext_sec)] <- 0

#Consumer demographics
load('condem_201406.rdata')
model_data_new <- left_join(model_data_new,condem_201406[-13],by=c('ACCT_ID'='con_id'))
rm(condem_201406)

model_data_new[,c("con_dem_education_code"  , "con_dem_estimated_income")][is.na(model_data_new[,c("con_dem_education_code"  , "con_dem_estimated_income")])] <-99
model_data_new$con_dem_education_code <- as.factor(model_data_new$con_dem_education_code)
model_data_new$con_dem_estimated_income<- as.factor(model_data_new$con_dem_estimated_income)

#the other factors in condem have " " and I rather make them explicit "blank"
levels(model_data_new$con_dem_home_owner_renter)[levels(model_data_new$con_dem_home_owner_renter)==" "] <- "blank"
levels(model_data_new$con_dem_income_producing_assets_cd)[levels(model_data_new$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(model_data_new$con_dem_marital_status)[levels(model_data_new$con_dem_marital_status)==" "] <- "blank"
levels(model_data_new$con_dem_income_producing_assets_cd)[levels(model_data_new$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(model_data_new$con_dem_presence_children_0_to_10)[levels(model_data_new$con_dem_presence_children_0_to_10)==" "] <- "blank"
levels(model_data_new$con_dem_presence_children_11_to_15)[levels(model_data_new$con_dem_presence_children_11_to_15)==" "] <- "blank"
levels(model_data_new$con_dem_presence_children_16_to_17)[levels(model_data_new$con_dem_presence_children_16_to_17)==" "] <- "blank"
levels(model_data_new$con_dem_presence_of_children)[levels(model_data_new$con_dem_presence_of_children)==" "] <- "blank"

#I forgot to take out people who star after 201406, obviosuly we do not need those
model_data_new <- subset(model_data_new,ACCT_DATE_OPENED_FOR_PRIME <='2014-06-30')

#add the CQi data
load('cqi_201406.rdata')

model_data_new <- left_join(model_data_new,cqi_201406)
model_data_new[,61:67][is.na(model_data_new[,61:67])] <- 0

rm(cqi_201406)

#add the NSF data 
load('contrib.rdata')

nsf_summary_new2 <- contrib %>% 
  filter((period >= '201401' & period <= '201406') ) %>%
  select(c(EXPRESSION_8:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,period)) %>% 
  group_by(period,EXPRESSION_8) %>%
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),waivers=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),
            months=sum(ACCT_CONTR_TOTAL_NSF_FEES>0)) 

#because Iintedn to do some cumsums, I want to make sure I have 6 period per, but if the accts did not have a line on the contrib for that period for wahtever reason i do not, so gather then spresd wil fill=0, then gather agiain and voila
nsf_summary_new2 <- nsf_summary_new2 %>% group_by(EXPRESSION_8,period) %>% arrange(EXPRESSION_8,period) %>%  
  gather(var,value,fees:months) %>% unite(key,period,var) %>% spread(key,value,fill=0) %>% 
  gather(var,value,-EXPRESSION_8) %>% separate(var,c('period','var'))

#create aone colum per
nsf_summary_new2 <- nsf_summary_new2 %>% spread(var,value) 
nsf_summary_new2 <- nsf_summary_new2 %>% arrange(EXPRESSION_8,desc(period))

nsf_summary_new2a <- nsf_summary_new2 %>% group_by(EXPRESSION_8) %>%
  mutate(fees_cum=cumsum(fees),waivers_cum=cumsum(waivers),name=1:6) %>% mutate(name=paste0('last',name,'m')) %>%
  select(EXPRESSION_8,name,fees_cum,waivers_cum) %>% gather(var,value,fees_cum:waivers_cum) %>% 
  unite(key,var,name,sep='_') %>% spread(key,value)
  
#I also need the total months with an NSF, just the sum of months
nsf_months <- nsf_summary_new2 %>% group_by(EXPRESSION_8) %>% summarise(months=sum(months))

length(setdiff(model_data_new$EXPRESSION_8,nsf_summary_new2a$EXPRESSION_8))  #101 accts

model_data_new <- inner_join(model_data_new,nsf_summary_new2a)
model_data_new <- inner_join(model_data_new,nsf_months)

rm(contrib,nsf_months,nsf_summary_new2,nsf_summary_new2a)

#do the same with events
load("Z:/M&T Projects/NSF/nsf_accts.rdata")
events_summary2 <- nsf_accts %>% select(period,EXPRESSION_8,ACCT_NSF_TOTAL) %>%
  filter((period >= '201401' & period <= '201406')) %>% group_by(EXPRESSION_8,period) %>% 
  summarise(events=sum(ACCT_NSF_TOTAL)) 

events_summary2 <- events_summary2 %>%  arrange(EXPRESSION_8,period) %>%  
  spread(period,events,fill=0) %>% 
  gather(period,events,-EXPRESSION_8) %>% arrange(EXPRESSION_8,desc(period))

events_summary2a <- events_summary2 %>% group_by(EXPRESSION_8) %>%
  mutate(events_cum=cumsum(events),name=1:6) %>% mutate(name=paste0('events_last',name,'m')) %>%
  select(EXPRESSION_8,name,events_cum) %>% spread(name,events_cum)

length(setdiff(model_data_new$EXPRESSION_8,events_summary2a$EXPRESSION_8))
model_data_new <- left_join(model_data_new,events_summary2a)
summary(model_data_new[81:86])
model_data_new[81:86][is.na(model_data_new[81:86])] <- 0  #because the nsf events only has those with any, so by definition the otehrs had 0

rm(nsf_accts,events_summary2,events_summary2a)


#add debit

load("Z:/M&T Projects/NSF/debit_all.rdata")


debit_summary <- debit_all %>% 
  filter((period >= '201401' & period <= '201406') ) %>%
  select(c(dda,num,amt,period)) %>% 
  group_by(dda,period) %>%
  summarise(num=sum(num),amt=sum(amt),months=sum(num>0)) 

length(unique(debit_summary$dda))
#because Iintedn to do some cumsums, I want to make sure I have 6 period per, but if the accts did not have a line on the contrib for that period for wahtever reason i do not, so gather then spresd wil fill=0, then gather agiain and voila
debit_summary <- debit_summary %>% group_by(dda,period) %>% arrange(dda,period) %>%  select(-months) %>%
  gather(var,value,num:amt) %>% unite(key,period,var) %>% spread(key,value,fill=0) %>% 
  gather(var,value,-dda) %>% separate(var,c('period','var')) %>% spread(var,value,fill=0) %>% arrange(dda,desc(period))




debit_summary1 <- debit_summary %>% group_by(dda) %>%
  mutate(debit_num_cum=cumsum(num),debit_amt_cum=cumsum(amt),name=1:6) %>% mutate(name=paste0('last',name,'m')) %>%
  select(dda,name,debit_num_cum,debit_amt_cum) %>% gather(var,value,debit_num_cum:debit_amt_cum) %>% 
  unite(key,var,name,sep='_') %>% spread(key,value)

#I also need the total months with debit, just the sum of months
debit_months <- debit_all %>% 
  filter((period >= '201401' & period <= '201406') ) %>%
  select(c(dda,num)) %>% 
  group_by(dda) %>%
  summarise(months=sum(num>0)) 


model_data_new <- left_join(model_data_new,debit_summary1,by=c('EXPRESSION_8'='dda'))
model_data_new <- left_join(model_data_new,debit_months,by=c('EXPRESSION_8'='dda'))
names(model_data_new)[99] <- 'debit_months'
names(model_data_new)[80] <- 'nsf_months'
summary(model_data_new[87:99])

#all NAs are by definition 0, due to how I extracted the debit data, only when transactions were preset in at least one month
model_data_new[87:99][is.na(model_data_new[87:99])] <- 0


rm(debit_all,debit_months,debit_summary,debit_summary1)

#Ineed one morre variable, which is the numbe rof NSF events in 2014 last 6M
#this will be used to define the modeing variable
load("Z:/M&T Projects/NSF/nsf_accts.rdata")
events_future <- nsf_accts %>% select(period,EXPRESSION_8,ACCT_NSF_TOTAL) %>%
  filter((period >= '201407' & period <= '201412')) %>% group_by(EXPRESSION_8) %>% 
  summarise(events_future=sum(ACCT_NSF_TOTAL)) 

model_data_new <- left_join(model_data_new,events_future)

#if NA the make zero
model_data_new$events_future[is.na(model_data_new$events_future)] <- 0
rm(events_future,nsf_accts)


#add activity (I need it for 201406 and for the next 6 months)
load("Z:/M&T Projects/NSF/activity1.rdata")

active_201412 <- activity1 %>% filter(period %in%  c('201412')) %>% group_by(EXPRESSION_8) %>% 
  summarise(active_201412 = ifelse(ACCT_NUMBER_DEBITS_MTD>=5 & ACCT_NUMBER_OF_DEPOSITS_MTD>0,1,0))

load('activity_201406.rdata')
active_201406 <- activity_201406 %>% group_by(EXPRESSION_8) %>% 
  summarise(active_201406 = ifelse(ACCT_NUMBER_DEBITS_MTD>=5 & ACCT_NUMBER_OF_DEPOSITS_MTD>0,1,0))

model_data_new <- left_join(model_data_new,active_201412)
model_data_new <- left_join(model_data_new,active_201406)
model_data_new[101:102][is.na(model_data_new[101:102])] <- 0
rm(active_201406,active_201412,activity_201406,activity1)

#Now define the flags
model_data_new$flag_less <- ifelse(model_data_new$events_last6m > model_data_new$events_future,1,0)
model_data_new$flag_closed <- ifelse(!is.na(model_data_new$closed) | model_data_new$active_201412==0,1,0)


#finally I need to create a nice factor for ACCT_STYPE
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Int','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking','Retail PMA','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
packages <- gsub('Checking','',packages)
packages <- gsub(' +',' ',packages)
packages <- gsub(' $','',packages)
packages <- gsub('with Interest','w/Int',packages)

stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')

model_data_new$stype <- factor(model_data_new$ACCT_STYPE ,levels=stypes,labels=packages)
model_data_new$stype <- factor(as.character(model_data_new$stype),compress='xz')
