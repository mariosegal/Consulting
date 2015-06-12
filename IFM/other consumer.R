refund <- read.table('ifmOpportunity_Mario_HH_TaxRefund.txt',sep='\t',header=T,colClasses=c('numeric','numeric'))
load("Z:/M&T Projects/IFM/penet_201410.rdata")

refund1 <- merge(refund,penet,by.x='HHKey',by.y='ACCT_ID') 

refund2 <- subset(refund1, !(HHKey %in% c(secs_hh,retired_hhs)))
                  
dim(refund2)[1]
table(refund2$SEC)
table(refund2$IRA)
table(refund2$MMS)

table(cut(refund1$SumOfInflowAmount,c(0,1000,2500,5000,10000,20000,50000,Inf)))

accts_201410 %>% filter(ACCT_PTYPE %in% c('MMS','IRA') & ACCT_AMT_BAL_FOR_PRIME >0) %>% 
  group_by(ACCT_PTYPE) %>% summarise(mean(ACCT_AMT_BAL_FOR_PRIME,na.rm=T))

accts_201410 %>% filter(ACCT_PTYPE %in% c('SEC') & ACCT_MARKET_VALUE >0) %>% 
  group_by(ACCT_PTYPE) %>% summarise(mean(ACCT_MARKET_VALUE,na.rm=T))                                                   
                                                    
                                                 
sum(refund1$SEC==0 | refund1$MMS==0 | refund1$IRA==0)
tax_hh <- refund2$HHKey[refund2$SEC==0 | refund2$MMS==0 | refund2$IRA==0]

intersect(intersect(secs_hh,tax_hh),retired_hhs)

#######  Bonus
bonus <- read.table('ifmOpportunity_Mario_HH_Bonus.txt',sep='\t',header=T,colClasses=c('numeric','numeric'))
bonus1 <- merge(bonus,penet,by.x='HHKey',by.y='ACCT_ID') 
bonus2 <- subset(bonus1,!(HHKey %in% c(secs_hh,retired_hhs,tax_hh)))

dim(bonus2)[1]
table(bonus2$SEC)
table(bonus2$MMS)

sum(bonus1$SEC==0 | bonus1$MMS==0)

bonus_hh <- bonus2$HHKey[bonus2$SEC==0 | bonus2$MMS==0 ]
intersect(intersect(intersect(secs_hh,tax_hh),retired_hhs),tax_hh)


#large deposits
large <- read.table('ifmOpportunity_Mario_HH_LargeDeposit.txt',sep='\t',header=T,
                    colClasses=c('numeric','numeric','currency','currency','currency'))
large1 <- merge(large,penet,by.x='HHKey',by.y='ACCT_ID') 
large2 <-subset(large1,!(HHKey %in% c(secs_hh,retired_hhs,tax_hh,bonus_hh)))

dim(large1)[1]
dim(large2)[1]
table(large1$SEC)
table(large1$MMS)
sum(large1$SEC==0 | large1$MMS==0)

table(large2$SEC)
table(large2$MMS)
sum(large2$SEC==0 | large2$MMS==0)
