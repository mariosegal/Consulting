setwd("Z:/M&T Projects/IFM")
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)


setClass("currency")
setAs("character","currency", function(from) as.numeric(gsub('\\$','',from)))



business <- read.table('ifmBusinessProfile_Masked.txt',stringsAsFactors=F,sep='\t',header=T)


save(business,file='business.rdata')

#########
load("Z:/M&T Projects/IFM/business.rdata")


#####   PAYROLL
business  %>% filter(PayrollProcessor!='') %>% group_by(PayrollProcessor) %>% 
  summarise(N=n(),amt=sum(PayrollProcessorAmount,na.rm=T),n1=sum(PayrollProcessorAmount>0,na.rm=T)) %>% arrange(desc(N)) %>% 
  mutate(P=N/sum(N,na.rm=T),cum=cumsum(P),avg=amt/n1) %>% filter(cum<=0.9)

sum(business$PayrollProcessor!='' & business$PayrollProcessorAmount>0 ,na.rm=T)

business$PayrollProcessorLastDate <- as.Date(business$PayrollProcessorLastDate,origin='1899-12-30')

table(business$PayrollProcessorLastDate,business$PayrollProcessorAmount>0 & business$PayrollProcessor != '')

mean(business$PayrollProcessorAmount[business$PayrollProcessorAmount>0 & business$PayrollProcessor != ''],na.rm=T)
table(cut(business$PayrollProcessorAmount[business$PayrollProcessorAmount>0 & business$PayrollProcessor != ''],
          c(0,10000,25000,50000,100000,250000,Inf)))


ggplot(business %>% filter(PayrollProcessor %in% c('INTUIT','PAYCHEX',"INTUIT DEPOSIT","ADP") & PayrollProcessorAmount>0 ),
       aes(x=PayrollProcessorAmount,fill=PayrollProcessor))+geom_histogram(aes(y=..density..),alpha=0.8,binwidth=1000)+
  facet_grid(PayrollProcessor~.,scales='free')+coord_cartesian(xlim=c(0,100000))+scale_y_continuous(labels=percent)+
  theme_bw()+theme(legend.position='none')


## MErchant

business$MerchantServicesLastDate <- as.Date(business$MerchantServicesLastDate,origin='1899-12-30')
business$MerchantServicesPrimaryProvider <- str_trim(business$MerchantServicesPrimaryProvider)

business  %>% filter(MerchantServicesPrimaryProvider!='') %>% group_by(MerchantServicesPrimaryProvider) %>% 
  summarise(N=n(),amt=sum(MerchantServicesAmount,na.rm=T),amt1=sum(MerchantServicesAvgMonthlyAmount,na.rm=T),n1=sum(MerchantServicesAmount>0,na.rm=T)) %>% arrange(desc(N)) %>% 
  mutate(P=N/sum(N,na.rm=T),cum=cumsum(P),avg=amt/n1,avg1=amt1/n1) %>% filter(cum<=0.9)

sum(business$MerchantServicesAmount>0 & business$MerchantServicesPrimaryProvider != '',na.rm=T)
mean(business$MerchantServicesAmount[business$MerchantServicesAmount>0 & business$MerchantServicesPrimaryProvider != ''],na.rm=T)
mean(business$MerchantServicesAvgMonthlyAmount[business$MerchantServicesAmount>0 & business$MerchantServicesPrimaryProvider != ''],na.rm=T)


table(cut(business$MerchantServicesAmount[business$MerchantServicesAmount>0 & 
                                            business$MerchantServicesPrimaryProvider != '' & 
                                            business$MerchantServicesPrimaryProvider != 'SQUARE INC'],
          c(0,10000,25000,50000,100000,250000,Inf)))

table(cut(business$MerchantServicesAmount[business$MerchantServicesAmount>0 & business$MerchantServicesPrimaryProvider != ''],
          c(0,10000,25000,50000,100000,250000,Inf)))

sum(business$AmexMerchantServicesAmount >0,na.rm=T)
sum(business$AmexMerchantServicesAmount >0 & business$MerchantServicesAmount>0,na.rm=T)
sum(is.na(business$AmexMerchantServicesAmount)  & business$MerchantServicesAmount>0,na.rm=T)
sum(business$AmexMerchantServicesAmount >0 & is.na(business$MerchantServicesAmount),na.rm=T)
table(business$AmexMerchantServicesAmount >0 , business$MerchantServicesAmount>0,useNA='ifany')
mean(business$AmexMerchantServicesAmoun[business$AmexMerchantServicesAmount >0],na.rm=T )
mean(business$AmexMerchantServicesAmoun[business$AmexMerchantServicesAmount >0 & is.na(business$MerchantServicesAmount)],na.rm=T )
mean(business$AmexMerchantServicesAmoun[business$AmexMerchantServicesAmount >0 & business$MerchantServicesAmount>0],na.rm=T )


#business card
sum(business$AmexCreditCardAmount | business$VisaMCCreditCardAmount,na.rm=T)

load('accts_201410_bus.rdata')
accts_201410_bus$ACCT_PTYPE <- as.character(accts_201410_bus$ACCT_PTYPE)
accts_201410_bus$ACCT_PTYPE[accts_201410_bus$ACCT_STYPE=='ELN'] <- 'BCRD'
aux_bus <- accts_201410_bus %>% group_by(ACCT_ID,ACCT_PTYPE) %>% summarise(flag=n_distinct(ACCT_PTYPE))
aux_bus$ACCT_PTYPE <- as.factor(aux_bus$ACCT_PTYPE)

aux_bus <- as.data.frame(aux_bus)
penet_bus <- spread(aux_bus,key=ACCT_PTYPE,value=flag,fill=0,drop=F) 

save(penet_bus,file='penet_bus.rdata')

bus_combo <- merge(business,penet_bus,by.x='HHKey',by.y='ACCT_ID')
bus_combo <- tbl_df(bus_combo)
save(bus_combo,file='bus_combo.rdata')

sum(bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount,na.rm=T)
sum((bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount) & bus_combo$BCRD==1,na.rm=T)
sum((bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount) & bus_combo$BCRD==0,na.rm=T)

length(unique(bus_combo$HHKey[(bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount) ]))
length(unique(bus_combo$HHKey[(bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount) & bus_combo$BCRD==1]))
length(unique(bus_combo$HHKey[(bus_combo$AmexCreditCardAmount >0 | bus_combo$VisaMCCreditCardAmount) & bus_combo$BCRD==0]))

crd_indx = ifelse(((!is.na(bus_combo$AmexCreditCardAmount) & bus_combo$AmexCreditCardAmount >0) | 
                     (!is.na(bus_combo$VisaMCCreditCardAmount) & bus_combo$VisaMCCreditCardAmount) )
                  & bus_combo$BCRD==0,1,0)
crd_amts <- gather(bus_combo[crd_indx==1,c('HHKey','AmexCreditCardAmount','VisaMCCreditCardAmount')],measure,amt,-HHKey)
crd_amts1 <- crd_amts %>% group_by(HHKey) %>% summarise(amt=sum(amt,na.rm=T))

table(cut(crd_amts1$amt,c(0,1000,2500,5000,10000,25000,50000,100000,250000,Inf)))

#business baseline

business$BankTransferCreditTrailing12Months <- as.numeric(gsub('\\$|,','',business$BankTransferCreditTrailing12Months))
business$BankTransferCreditOverallAvgAmount <- as.numeric(gsub('\\$|,','',business$BankTransferCreditOverallAvgAmount))

sum(accts_201410_bus$ACCT_PTYPE=='DDA')
dda_bus <- subset(accts_201410_bus,ACCT_PTYPE=='DDA' & substr(ACCT_STYPE,1,1)=='C' & ACCT_SBU_GROUP=='BUS')
business$flag <- 1
bus_combo1 <- inner_join(dda_bus,business,by=c('EXPRESSION_8'='Acct_Masked'))

105/191
keep = which(sapply(bus_combo1,is.numeric))


my.penet <- function(x) sum(x>1,na.rm=T)/length(x)
my.mean <- function(x) sum(as.numeric(x),na.rm=T)/sum(x>1,na.rm=T)

results <- t(bus_combo1 %>% select((keep)) %>% summarise_each(funs(my.penet,my.mean)))
results <- data.frame(what=row.names(results),value=results)
results$type <- sapply(results$what,function(x) strsplit(as.character(x),'_')[[1]][2])
results$measure <- sapply(results$what,function(x) strsplit(as.character(x),'_')[[1]][1])
results1 <- spread(results[results$type %in% c('my.mean','my.penet'),c('measure','type','value')],type,value)
results1$my.penet <- round(results1$my.penet*100,digits=1)
results1$my.mean <- round(results1$my.mean,digits=0)

with(bus_combo1,sum(DiscoverMerchantServicesAmount >0 | AmexMerchantServicesAmount >0 | MerchantServicesAmount >0,na.rm=T))/dim(bus_combo1)[1]
with(bus_combo1,sum(AmexMerchantServicesAmount >0 & MerchantServicesAmount >0,na.rm=T))/dim(bus_combo1)[1]
with(bus_combo1,sum(AmexMerchantServicesAmount >0 | MerchantServicesAmount >0,na.rm=T))/dim(bus_combo1)[1]

with(bus_combo1,sum(AmexCreditCardAmount >0 | VisaMCCreditCardAmount >0 | DiscoverCreditCardAmount >0,na.rm=T))/dim(bus_combo1)[1]

with(bus_combo1,sum(InvestmentDebitTrailing12Months >0 | InvestmentCreditTrailing12Months >0 | BankTransferDebitTrailing12Months >0 | BankTransferCreditTrailing12Months>0,na.rm=T))/dim(bus_combo1)[1]
