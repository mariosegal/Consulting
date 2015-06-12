
setwd("Z:/M&T Projects/IFM")
flow <- read.csv('ifmFlowOfFunds_Masked.txt',stringsAsFactors=F)
flow$TotalPurchases <- gsub('\\$| ','',flow$TotalPurchases)
flow$TotalPurchases <- str_trim(flow$TotalPurchases)
flow$TotalPurchases <- as.numeric(flow$TotalPurchases)

flow$NbrOfPurchaseTransactions <- gsub('\\$| ','',flow$NbrOfPurchaseTransactions)
flow$NbrOfPurchaseTransactions <- str_trim(flow$NbrOfPurchaseTransactions)
flow$NbrOfPurchaseTransactions <- as.numeric(flow$NbrOfPurchaseTransactions)


sum(flow$TotalPurchases,na.rm=T)/sum(flow$NbrOfPurchaseTransactions,na.rm=T)
sum(flow$NbrOfPurchaseTransactions>=1,na.rm=T)

curr_to_num <- function(x) as.numeric(gsub('\\$','',x))

setClass("currency")
setAs("character","currency", function(from)as.numeric(gsub('\\$','',from)))
retail <- read.table('ifmOpportunity_Mario_HH_RetailStorePaper_Card.txt',,stringsAsFactors=F,header=T,sep='\t',
                     colClasses = c('integer','integer','currency','currency','currency') )


merchant <- read.table('ifmOpportunity_Mario_HH_MERCHSERV.txt',,stringsAsFactors=F,header=T,sep='\t',
                       colClasses = c('integer','currency') )

load("Z:/M&T Projects/accts_201410.rdata")

bus <- unique(dda_201410$ACCT_ID[ substr(dda_201410$ACCT_STYPE,1,1)!='R'])

con <- unique(dda_201410$ACCT_ID[ substr(dda_201410$ACCT_STYPE,1,1)=='R'])

merchant$type <- ifelse(merchant$HHKey %in% bus,'bus',NA)
merchant$type <- ifelse(merchant$HHKey %in% con & is.na(merchant$type),'con',merchant$type)
table(merchant$type,useNA='ifany')


merchant$flow <- cut(merchant$TotalAmt,c(0,5000,10000,25000,50000,100000,250000,500000,1000000,2000000,5000000,10000000,Inf),dig.lab=8)
prop.table(table(merchant$flow))
