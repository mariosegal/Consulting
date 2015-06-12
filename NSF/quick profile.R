

existing_book$tenure <- as.numeric(round((as.Date('2013-06-01')-existing_book$ACCT_DATE_OPENED_FOR_PRIME)/(365.25),1))

tenure1 <- (table(existing_book$group_2013,cut(existing_book$tenure,c(0,1,2,3,4,5,7,10,15,20,Inf))))
write.table(tenure1,'clipboard-128',sep='\t',row.names=T)

packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Interest','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking Account','Retail Portfolio Management Account','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')

existing_book$stype <- factor(existing_book$ACCT_STYPE ,levels=stypes,labels=packages)


stype1 <- (table(existing_book$group_2013,existing_book$stype))
write.table(stype1,'clipboard-128',sep='\t',row.names=T)
