load('Z:/M&T Projects/M&T Way/accts_201312.rdata')
load("Z:/M&T Projects/M&T Way/accts_201412.rdata")


#classify HHLDS by whether they had checking or not at end of 2013

hhlds_2013 <- accts_201312 %>% group_by(ACCT_ID) %>% summarise(checking=sum(ACCT_PTYPE=='DDA'))

hhlds_2013$checking <- ifelse(hhlds_2013$checking >=1,1,0)


accts_201412$ACCT_PTYPE <- as.character(accts_201412$ACCT_PTYPE)
accts_201412$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(accts_201412$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
sales14 <- accts_201412 %>% filter(format(ACCT_DATE_OPENED_FOR_PRIME,'%Y')==2014) %>% 
  mutate(ACCT_PTYPE=ifelse(ACCT_PTYPE=='CCS' & ACCT_STYPE %in% c("NOR","REW","SIG"),'CRD',ACCT_PTYPE)) %>% 
  group_by(ACCT_ID,ACCT_PTYPE) %>% summarise(N=n())

sales14 %>% group_by(ACCT_PTYPE) %>% summarise(sum(N))

#append the 2013 classe

sales14 <- left_join(sales14,hhlds_2013,by='ACCT_ID') 
sales14$checking <- ifelse(is.na(sales14$checking),-1,sales14$checking)
sales14$checking <- factor(sales14$checking,levels=c(-1,0,1),labels=c('New',"No","Yes"))

summary <- sales14 %>% group_by(checking,ACCT_PTYPE) %>% summarise(sales=sum(N)) %>% 
  group_by(ACCT_PTYPE) %>% spread(checking,sales,fill=0)

write.table(summary,'clipboard-128',sep='\t',row.names=F)
