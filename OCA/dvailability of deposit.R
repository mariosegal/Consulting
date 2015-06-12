#load libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(ape)

#load data
load("Z:/M&T Projects/OCA/oca_data.rdata")
load('dda_201401_oca_cqi.rdata')
load('dda_extra.rdata')
load('nsf2014.rdata')
load("Z:/M&T Projects/OCA/dda2.rdata")


levels <- complaints %>% group_by(level_1,level_2,level_3,level_4) %>% summarise(N=n())
levels <- levels[order(-levels$N),]

#checking and NSF
data <- complaints %>% filter(level_4=='Availability of deposits' & level_2=='Checking account')
dim(data)[1]
sum(is.na(data$mask_acct) & is.na(data$mask_ssn))
data1 <- filter(data,!is.na(mask_acct) | !is.na(mask_ssn))
sum(!is.na(data1$mask_acct))
sum(is.na(data1$mask_acct))


#I need to merge by SSN and by acct number to get the HHLD in question, I also want to get the STYPE
#for the ones that I only get SSN it is gong to be hard to know the acct if multiple

#part1 - do the ones where I have an acct
part1 <- subset(data1,!is.na(mask_acct),select=c('mask_acct','complaint_master_id'))
sum(duplicated(part1$mask_acct))
length(unique(part1$mask_acct))

match1 <- inner_join(part1,accts_201412_oca[accts_201412_oca$ACCT_PTYPE=='DDA',],by=c('mask_acct'='EXPRESSION_8'))
length(unique(match1$ACCT_ID))
sum(duplicated(match1$ACCT_ID))

sum(part1$mask_acct %in% accts_201412_oca$EXPRESSION_8[accts_201412_oca$ACCT_PTYPE=='DDA'])
sum(!(part1$mask_acct %in% accts_201412_oca$EXPRESSION_8[accts_201412_oca$ACCT_PTYPE=='DDA']))


#part2 - try the ones with ssn
part2 <- subset(data1,is.na(mask_acct),select=c('mask_ssn','complaint_master_id'))
sum(duplicated(part2$mask_ssn))
length(unique(part2$mask_ssn))
sum(duplicated(part2$complaint_master_id))

match2 <- inner_join(part2,accts_201412_oca[accts_201412_oca$ACCT_PTYPE=='DDA',],by=c('mask_ssn'='EXPRESSION_18'))
match2$mask_acct <- match2$EXPRESSION_8


length(unique(match2$complaint_master_id)) # this are the number matched
length(setdiff(part2$complaint_master_id,match2$complaint_master_id))

dupes2 <- match2$complaint_master_id[which(duplicated(match2$complaint_master_id))]
length(unique(match2$complaint_master_id[!(match2$complaint_master_id %in% dupes2)])) #one match
length(unique(match2$complaint_master_id[(match2$complaint_master_id %in% dupes2)]))  #multiple

keep2 <- match2[!(match2$complaint_master_id %in% dupes2),]  #it is impossible to analyze the accts for multiple ones
#I will analyze them at the hhld level though as well
drop2 <- match2$EXPRESSION_8[(match2$complaint_master_id %in% dupes2)]


accts1 <- bind_rows(keep2,match1)  #this is the acct level analysis dataset
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')
packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Interest','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking Account','Retail Portfolio Management Account','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
accts1$ACCT_STYPE <- factor(accts1$ACCT_STYPE ,levels=stypes,labels=packages)
accts1 <- inner_join(accts1,data1,by='complaint_master_id')

length(unique(accts1$ACCT_ID))
length(unique(accts1$mask_acct.x))


#accts1 only has the complaint accts selected
# I want to create a set with all the keep accts as well as all other ddas not excluded so I can get both distributions at the same time
#it all ddas from accts, if they are in keep marked as 1, of they were ewxcluded mark as -1 or drop, if they are neither are 0
#then add the complaint detail and then the extra data (properly labelled)


#since I want to mark as drop or -1 those that had complaints but I coul dnot match, but by definition they were not matched. I will do this
#1 - mark all as drop
#2 - if on accts1 then it is a Yes
#if HHLD is not on match1/match2 and still drop then mark it as No : these were matched and dropped so I do mot want them
#further if No, create a No with NSf and No with no NSF

dda <-accts_201412_oca %>% filter(ACCT_PTYPE=='DDA')
dda$availability <- "Drop"
dda$availability <- ifelse(dda$EXPRESSION_8 %in% accts1$mask_acct.x,"Yes",dda$availability)
dda$availability <- ifelse((!(dda$ACCT_ID %in% match1$ACCT_ID) & !(dda$ACCT_ID %in% match2$ACCT_ID))& dda$availability=="Drop","No",dda$availability)

#merge deposit data
load('Z:/M&T Projects/OCA/dep2014.rdata')
dda <- left_join(dda,dep2014,by='EXPRESSION_8')

dda$availability <- ifelse(dda$availability=="No" & dda$ACCT_NUMBER_OF_DEPOSITS_MTD.y >0,"No with Dep",dda$availability)
dda$availability <- ifelse(is.na(dda$availability) ,"No Deposits",dda$availability)



#Now merge the extra dda data and the complaints
dda1 <- left_join(dda,dda_201401_oca[-1],by='EXPRESSION_8')
dda1 <- left_join(dda1,dda_201412_oca[-c(1,3)],by='EXPRESSION_8')
dda1 <- left_join(dda1,dda_201401_oca_cqi[-1],by='EXPRESSION_8')

dda1$ACCT_STYPE <- factor(dda1$ACCT_STYPE ,levels=stypes,labels=packages)

accts1 <- accts1[order(accts1$complaint_master_id),]  #sort to take the first complaint for acct analysis
accts2 <- accts1[!duplicated(accts1$mask_acct.x),]
dda2 <- left_join(dda1,accts2[c(2,24,26:55)],by=c('EXPRESSION_8'='mask_acct.x'))

dda2$availability <- as.factor(dda2$availability)

dda2$balance_dec <- cut(dda2$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8)  
dda2$balance_jan <- cut(dda2$ACCT_AMT_BAL_FOR_PRIME_201401,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8) 

########################
###   ANALYSIS       ###
########################

#acct abseline
dda2 %>% group_by(ACCT_STYPE) %>% summarise(N=n())  %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>%  arrange(desc(N))

stype1 <- dda2 %>% group_by(availability,ACCT_STYPE) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0) %>% arrange(desc(Yes))
write.table(stype1[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=F)


bals1 <- dda2 %>% group_by(availability,balance_dec) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0) 
bals2 <- dda2 %>% group_by(availability,balance_jan) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0)

write.table(bals1[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=T)
write.table(bals2[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=T)


cqi <- dda2 %>% group_by(availability,ACCT_CQI) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0) 
write.table(cqi[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=F)


cqi1 <- dda2 %>% group_by(availability) %>% summarise_each(funs(mean),vars=ACCT_CQI_BILL_PAY:ACCT_CQI_WEB) 
names(cqi1)[-1] <- names(dda2)[15:19]
cqi1$availability <- as.character(cqi1$availability)
cqi1b <- t(cqi1[c(4,3,2),c(3,6,2,5,4)])
#colnames(cqi1b) <- (cqi1[c(4,3,2),1])
write.table(cqi1b,'clipboard-128',sep='\t',row.names=T)


#age
dda2$ACCT_DATE_OF_BIRTH_201412 <- as.Date(dda2$ACCT_DATE_OF_BIRTH_201412,"%m/%d/%Y")
dda2$age <- cut(2015-as.numeric(format(dda2$ACCT_DATE_OF_BIRTH_201412,'%Y')),c(0,seq(25,75,by=10),Inf))

age1 <- dda2 %>% group_by(availability,age) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0) 
write.table(age1[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=F)


#tenure
dda2$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(dda2$ACCT_DATE_OPENED_FOR_PRIME,"%m/%d/%Y")
dda2$tenure <- cut(2015-as.numeric(format(dda2$ACCT_DATE_OPENED_FOR_PRIME,'%Y')),c(0.01,1,2,3,5,7,10,15,Inf))

tenure <- dda2 %>% group_by(availability,tenure) %>% summarise(N=n()) %>% group_by(availability) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(availability,N,fill=0) 
write.table(tenure[c(1,5,4,3)],'clipboard-128',sep='\t',row.names=F)


######## 
# HHLD level analysis
######## 

#Ineed to classify the HHLDs in the 3 groups and not have overlap

hh_aux <- dcast(dda2[c('ACCT_ID','availability')],ACCT_ID~availability)
names(hh_aux) <- gsub(" ","_",names(hh_aux))

hh_aux$flag <- ifelse(hh_aux$Yes>=1,"Yes",NA)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) & hh_aux$No_with_Dep>=1,"No_with_Dep",hh_aux$flag)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) & hh_aux$No_Deposits>=1,"No_Deposits",hh_aux$flag)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) ,"Drop",hh_aux$flag)

table(hh_aux$flag)


#penetration 
penet1 <- merge(penet,hh_aux,by='ACCT_ID')
penet1$prods <- rowSums(penet1[,c('DDA','MMS','SAV','TDA','IRA','CRD','CCS','ILN','MTG',"HEQ",'SEC','INS')])

prods <- penet1 %>% group_by(flag) %>% summarise_each(funs(mean),-c(ACCT_ID,Drop:Yes))
prods2 <- penet1 %>% group_by(flag,prods) %>% summarise(N=n()) %>% mutate(p=N/sum(N)) %>% select(flag,prods,p) %>% spread(prods,p)

write.table(prods[c(4,3,2),c('flag','DDA','MMS','SAV','TDA','IRA','CRD','CCS','ILN','MTG',"HEQ",'SEC','INS')],'clipboard-128',sep='\t',row.names=F)

write.table(prods2[c(4,3,2),],'clipboard-128',sep='\t',row.names=F)



#balances

bals1 <- merge(bals,hh_aux,by='ACCT_ID')
#only summarise the non NA
bals2 <- bals1 %>% group_by(flag) %>% summarise_each(funs(mean(.,na.rm=T)),-c(ACCT_ID,Drop:Yes))
write.table(bals2[c(4,3,2),c('flag','DDA_bal','MMS_bal','SAV_bal','TDA_bal','IRA_bal','CRD_bal','CCS_bal','ILN_bal','MTG_bal',"HEQ_bal",'SEC_bal','INS_bal')],'clipboard-128',sep='\t',row.names=F)


#contribution
contr1 <- merge(contr,hh_aux,by='ACCT_ID')
#only summarise the non NA
contr2 <- contr1 %>% group_by(flag) %>% summarise_each(funs(mean(.,na.rm=T)),-c(ACCT_ID,Drop:Yes))
write.table(contr2[c(4,3,2),c('flag','DDA_contr','MMS_contr','SAV_contr','TDA_contr','IRA_contr','CRD_contr','CCS_contr','ILN_contr','MTG_contr',"HEQ_contr",'SEC_contr','INS_contr')],'clipboard-128',sep='\t',row.names=F)


#segment and Managed and geography
segment <- merge(hh_aux,hhlds_201412[c('HHLD_ID','HHLD_LIFE_CYCLE_SEGMENT','HHLD_COMMUNITY_BANK_MARKET','HHLD_HH_OWNER_CODE')],by.x='ACCT_ID',by.y='HHLD_ID',all.x=T)

segment$rm <- ifelse(substring(as.character(segment$HHLD_HH_OWNER_CODE),4)!='00',"Mgd","Non")

segment$HHLD_LIFE_CYCLE_SEGMENT[segment$HHLD_LIFE_CYCLE_SEGMENT==8] <- 1
segment$HHLD_LIFE_CYCLE_SEGMENT[segment$HHLD_LIFE_CYCLE_SEGMENT==9] <- 4
segment$HHLD_LIFE_CYCLE_SEGMENT[is.na(segment$HHLD_LIFE_CYCLE_SEGMENT)] <- 7

segment$HHLD_LIFE_CYCLE_SEGMENT <- factor(segment$HHLD_LIFE_CYCLE_SEGMENT,levels=c(1:7),
                                          labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

segment1 <- segment %>% group_by(flag,HHLD_LIFE_CYCLE_SEGMENT) %>% summarise(N=n()) %>% group_by(flag) %>% 
  mutate(p=N/sum(N)) %>% select(-N) %>% spread(HHLD_LIFE_CYCLE_SEGMENT,p)

write.table(segment1[c(4,3,2),c('flag','BTF','MANK','MNF','MAF','MNR','MAR','NC')],'clipboard-128',sep='\t',row.names=F)



rm <- segment %>% group_by(flag,rm) %>% summarise(N=n()) %>% group_by(flag) %>% 
  mutate(p=N/sum(N)) %>% select(-N) %>% spread(rm,p)

write.table(rm[c(4,3,2),],'clipboard-128',sep='\t',row.names=F)


segment$cbr <- factor(segment$HHLD_COMMUNITY_BANK_MARKET,levels=1:17,
                      labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA',
                               'SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))
cbr <- segment %>% filter(!is.na(cbr)) %>%group_by(flag,cbr) %>% summarise(N=n()) %>% group_by(flag) %>% 
  mutate(p=N/sum(N)) %>% select(-N) %>% spread(cbr,p)

write.table(cbr[c(4,3,2),],'clipboard-128',sep='\t',row.names=F)

save(dda2,file='dda2_availability.rdata')
