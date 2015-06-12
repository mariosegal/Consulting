#load libraries


#library(ape)

#load data
load("Z:/M&T Projects/OCA/oca_data.rdata")
load('dda_201401_oca_cqi.rdata')
load('dda_extra.rdata')
load('nsf2014.rdata')
load("Z:/M&T Projects/OCA/dda2.rdata")

#make a tree of the levels
levels <- complaints %>% group_by(level_1,level_2,level_3,level_4,level_5) %>% summarise(N=n())
levels <- levels[order(-levels$N),]
newdata <- as.phylo(x=~level_1/level_2/level_3,data=levels)

#png('Z:/M&T Projects/OCA/hierarchy1.png',width=8,height=6,units='in')
plot.phylo(x=newdata,show.tip.label = T,show.node.label=T,no.margin=T,cex=0.5,type='p')
nodelabels()
nodelabels(c('Bank Acct','Cons Loan','Credit Reporting','MTG/HE'),node=c(31,32,33,34),col='red',bg='white',frame='rect')
dev.off()
 
table(complaints$level_2,complaints$level_1)


#Analyze the selected group
#checking and NSF
data <- complaints %>% filter(level_3=='NSF Fees - Problems caused by low funds' & level_2=='Checking account')
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

#how many matched multiple times by SSN - those are the ones that have duped maSTER_ID AS WE HAD ZERO BEFORE
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
dda$nsf_fee <- "Drop"
dda$nsf_fee <- ifelse(dda$EXPRESSION_8 %in% accts1$mask_acct.x,"Yes",dda$nsf_fee)
dda$nsf_fee <- ifelse((!(dda$ACCT_ID %in% match1$ACCT_ID) & !(dda$ACCT_ID %in% match2$ACCT_ID))& dda$nsf_fee=="Drop","No",dda$nsf_fee)

#merge nsf data
dda <- left_join(dda,nsf2014,by='EXPRESSION_8')

dda$nsf_fee <- ifelse(dda$nsf_fee=="No" & dda$ACCT_NSF_TOTAL >0,"No with Fee",dda$nsf_fee)
dda$nsf_fee <- ifelse(is.na(dda$nsf_fee) ,"No NSF Fee",dda$nsf_fee)



#Now merge the extra dda data and the complaints
dda1 <- left_join(dda,dda_201401_oca[-1],by='EXPRESSION_8')
dda1 <- left_join(dda1,dda_201412_oca[-c(1,3)],by='EXPRESSION_8')
dda1 <- left_join(dda1,dda_201401_oca_cqi[-1],by='EXPRESSION_8')

dda1$ACCT_STYPE <- factor(dda1$ACCT_STYPE ,levels=stypes,labels=packages)

accts1 <- accts1[order(accts1$complaint_master_id),]  #sort to take the first complaint for acct analysis
accts2 <- accts1[!duplicated(accts1$mask_acct.x),]
dda2 <- left_join(dda1,accts2[c(2,24,26:55)],by=c('EXPRESSION_8'='mask_acct.x'))

dda2$nsf_fee <- as.factor(dda2$nsf_fee)

dda2$balance_dec <- cut(dda2$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8)  
dda2$balance_jan <- cut(dda2$ACCT_AMT_BAL_FOR_PRIME_201401,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8) 

#stype1 <- accts1 %>% group_by(ACCT_STYPE) %>% summarise(N=n()) %>% mutate(p=N/sum(N)) %>% arrange(desc(N))
stype1 <- dda2 %>% group_by(nsf_fee,ACCT_STYPE) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0) %>% arrange(desc(Yes))
write.table(stype1,'clipboard-128',sep='\t',row.names=F)

#bals <- table(format(accts1$complaint_date,"%Y%m"),
#              cut(accts1$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8))
bals1 <- dda2 %>% group_by(nsf_fee,balance_dec) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0) 
bals2 <- dda2 %>% group_by(nsf_fee,balance_jan) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0)

write.table(bals1,'clipboard-128',sep='\t',row.names=T)
write.table(bals2,'clipboard-128',sep='\t',row.names=T)



#cqi <- accts1 %>% group_by(ACCT_CQI) %>% summarise(N=n()) %>% mutate(p=N/sum(N)) 
cqi <- dda2 %>% group_by(nsf_fee,ACCT_CQI) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0) 
write.table(cqi,'clipboard-128',sep='\t',row.names=F)


cqi1 <- dda2 %>% group_by(nsf_fee) %>% summarise_each(funs(mean),vars=ACCT_CQI_BILL_PAY:ACCT_CQI_WEB) 
names(cqi1)[-1] <- names(dda2)[14:18]
write.table(cqi1,'clipboard-128',sep='\t',row.names=F)


#age
dda2$ACCT_DATE_OF_BIRTH_201412 <- as.Date(dda2$ACCT_DATE_OF_BIRTH_201412,"%m/%d/%Y")
dda2$age <- cut(2015-as.numeric(format(dda2$ACCT_DATE_OF_BIRTH_201412,'%Y')),c(0,seq(25,75,by=10),Inf))

age1 <- dda2 %>% group_by(nsf_fee,age) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0) 
write.table(age1,'clipboard-128',sep='\t',row.names=F)


#tenure
dda2$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(dda2$ACCT_DATE_OPENED_FOR_PRIME,"%m/%d/%Y")
dda2$tenure <- cut(2015-as.numeric(format(dda2$ACCT_DATE_OPENED_FOR_PRIME,'%Y')),c(0.01,1,2,3,5,7,10,15,Inf))

tenure <- dda2 %>% group_by(nsf_fee,tenure) %>% summarise(N=n()) %>% group_by(nsf_fee) %>% mutate(p=N/sum(N)) %>% arrange(desc(N)) %>% select(-p) %>%  spread(nsf_fee,N,fill=0) 
write.table(tenure,'clipboard-128',sep='\t',row.names=F)


save(dda2,file='dda2.rdata')


######## 
# HHLD level analysis
######## 

#Ineed to classify the HHLDs in the 3 groups and not have overlap

hh_aux <- dcast(dda2[c('ACCT_ID','nsf_fee')],ACCT_ID~nsf_fee)
names(hh_aux) <- gsub(" ","_",names(hh_aux))

hh_aux$flag <- ifelse(hh_aux$Yes>=1,"Yes",NA)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) & hh_aux$No_with_Fee>=1,"No_with_Fee",hh_aux$flag)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) & hh_aux$No_NSF_Fee>=1,"No_NSF_Fee",hh_aux$flag)
hh_aux$flag <- ifelse(is.na(hh_aux$flag) ,"Drop",hh_aux$flag)

table(hh_aux$flag)

#penetration 

penet1 <- merge(penet,hh_aux,by='ACCT_ID')
penet1$prods <- rowSums(penet1[,c('DDA','MMS','SAV','TDA','IRA','CRD','CCS','ILN','MTG',"HEQ",'SEC','INS')])

prods <- penet1 %>% group_by(flag) %>% summarise_each(funs(mean),-c(ACCT_ID,Drop:Yes))
prods2 <- penet1 %>% group_by(flag,prods) %>% summarise(N=n()) %>% mutate(p=N/sum(N)) %>% select(flag,prods,p) %>% spread(prods,p)

write.table(prods[,c('flag','DDA','MMS','SAV','TDA','IRA','CRD','CCS','ILN','MTG',"HEQ",'SEC','INS')],'clipboard-128',sep='\t',row.names=F)

write.table(prods2,'clipboard-128',sep='\t',row.names=F)



#balances

bals1 <- merge(bals,hh_aux,by='ACCT_ID')
#only summarise the non NA
bals2 <- bals1 %>% group_by(flag) %>% summarise_each(funs(mean(.,na.rm=T)),-c(ACCT_ID,Drop:Yes))
write.table(bals2[,c('flag','DDA_bal','MMS_bal','SAV_bal','TDA_bal','IRA_bal','CRD_bal','CCS_bal','ILN_bal','MTG_bal',"HEQ_bal",'SEC_bal','INS_bal')],'clipboard-128',sep='\t',row.names=F)


#contribution
contr1 <- merge(contr,hh_aux,by='ACCT_ID')
#only summarise the non NA
contr2 <- contr1 %>% group_by(flag) %>% summarise_each(funs(mean(.,na.rm=T)),-c(ACCT_ID,Drop:Yes))
write.table(contr2[,c('flag','DDA_contr','MMS_contr','SAV_contr','TDA_contr','IRA_contr','CRD_contr','CCS_contr','ILN_contr','MTG_contr',"HEQ_contr",'SEC_contr','INS_contr')],'clipboard-128',sep='\t',row.names=F)



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

write.table(segment1[,c('flag','BTF','MANK','MNF','MAF','MNR','MAR','NC')],'clipboard-128',sep='\t',row.names=F)



rm <- segment %>% group_by(flag,rm) %>% summarise(N=n()) %>% group_by(flag) %>% 
  mutate(p=N/sum(N)) %>% select(-N) %>% spread(rm,p)

write.table(rm,'clipboard-128',sep='\t',row.names=F)


segment$cbr <- factor(segment$HHLD_COMMUNITY_BANK_MARKET,levels=1:17,
                      labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA',
                               'SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))
cbr <- segment %>% filter(!is.na(cbr)) %>%group_by(flag,cbr) %>% summarise(N=n()) %>% group_by(flag) %>% 
  mutate(p=N/sum(N)) %>% select(-N) %>% spread(cbr,p)

write.table(cbr,'clipboard-128',sep='\t',row.names=F)


profit <- segment %>% group_by(flag,HHLD_PROFIT_SCORE_YEAR) %>% summarise(N=n()) %>% mutate(p=N/sum(N))

#######################################

#the document was confusing, so we need to do it in terms of rates, use dda2a as that ha sonly the people who could have complaiend, really an approximation

load("Z:/M&T Projects/OCA/dda2_small.rdata")
load("Z:/M&T Projects/OCA/segment_small.rdata")
load("Z:/M&T Projects/OCA/contr1_small.rdata")
load("Z:/M&T Projects/OCA/bals1_small.rdata")
load("Z:/M&T Projects/OCA/penet1_small.rdata")

  
my_test <- function(a,b,c,d) prop.test(c(a,b),c(c,d),alternative='g')$p.value  

#rate by stype
stype_rate <- dda2a %>% group_by(ACCT_STYPE,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N) %>% mutate(rate=Yes/(`No with Fee`+Yes)) %>% arrange(desc(rate))
write.table(stype_rate,'clipboard-128',sep='\t',row.names=F)

tenure_rate <- dda2a %>% group_by(tenure,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N) %>% mutate(rate=Yes/(`No with Fee`+Yes)) 
write.table(tenure_rate,'clipboard-128',sep='\t',row.names=F)



dda2a$ACCT_DATE_OF_BIRTH_201412 <- as.Date(dda2a$ACCT_DATE_OF_BIRTH_201412,"%m/%d/%Y")
dda2a$age <- cut(2015-as.numeric(format(dda2a$ACCT_DATE_OF_BIRTH_201412,'%Y')),c(0,seq(20,45,by=5),c(55,65,75),Inf))

age_rate1 <- dda2a %>% group_by(age,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot)) %>% group_by(age) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(age_rate1,'clipboard-128',sep='\t',row.names=F)

#do region but at the acct level, we do not need to get that fancy

dda2a <- inner_join(dda2a,select(segmenta,c(ACCT_ID,cbr,HHLD_LIFE_CYCLE_SEGMENT)),by='ACCT_ID')

cbr_rate1 <- dda2a %>% group_by(cbr,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot)) %>% group_by(cbr) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(cbr_rate1,'clipboard-128',sep='\t',row.names=F)


#explore why MyChoice

tenure_stype <- dda2a %>% group_by(tenure,ACCT_STYPE) %>% summarise(N=n()) %>% group_by(tenure) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(ACCT_STYPE,p)
tenure_stype$tenure <- as.character(tenure_stype$tenure)

tenure_aux <- dda2a %>% group_by(ACCT_STYPE) %>% summarise(N=n()) 

tenure_stype <- rbind(tenure_stype,c('N',tenure_aux$N))

write.table(tenure_stype,'clipboard-128',sep='\t',row.names=F,na='')


age_stype <- dda2a %>% group_by(age,ACCT_STYPE) %>% summarise(N=n()) %>% group_by(age) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(ACCT_STYPE,p)
age_stype$age <- as.character(age_stype$age)

age_aux <- dda2a %>% group_by(ACCT_STYPE) %>% summarise(N=n()) 

age_stype <- rbind(age_stype,c('N',age_aux$N))
write.table(age_stype,'clipboard-128',sep='\t',row.names=F,na='')

#lets do analysis, but with data for the average along all months present in 2014, thisis the right way

dda2a <- left_join(dda2a,bal2014,by='EXPRESSION_8')

dda2a$avg_bal_2014 <- cut(dda2a$ACCT_AMT_BAL_AVG_MONTH,c(-Inf,0,100,250,500,1000,2000,5000,Inf),dig.lab = 8) 

balance_rate <- dda2a  %>% group_by(avg_bal_2014,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(avg_bal_2014) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(balance_rate,'clipboard-128',sep='\t',row.names=F,na='')

bal_stype <- dda2a %>% group_by(avg_bal_2014,ACCT_STYPE) %>% summarise(N=n()) %>% group_by(avg_bal_2014) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(ACCT_STYPE,p)
bal_stype$avg_bal_2014 <- as.character(bal_stype$avg_bal_2014)

bal_aux <- dda2a %>% group_by(ACCT_STYPE) %>% summarise(N=n()) 

bal_stype <- rbind(bal_stype,c('N',bal_aux$N))
write.table(bal_stype,'clipboard-128',sep='\t',row.names=F,na='')

###################
#Explore cbr

tenure_cbr <- dda2a %>% filter(!is.na(cbr)) %>% group_by(tenure,cbr) %>% summarise(N=n()) %>% group_by(tenure) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(cbr,p)
tenure_cbr$tenure <- as.character(tenure_cbr$tenure)

tenure_aux1 <- dda2a %>% group_by(cbr) %>% summarise(N=n()) 

tenure_cbr <- rbind(tenure_cbr,c('N',tenure_aux1$N))

write.table(tenure_cbr,'clipboard-128',sep='\t',row.names=F,na='')


age_cbr <- dda2a %>% filter(!is.na(cbr)) %>% group_by(age,cbr) %>% summarise(N=n()) %>% group_by(age) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(cbr,p)
age_cbr$age <- as.character(age_cbr$age)


age_cbr <- rbind(age_cbr,c('N',tenure_aux1$N))
write.table(age_cbr,'clipboard-128',sep='\t',row.names=F,na='')


bal_cbr <- dda2a %>% filter(!is.na(cbr)) %>% group_by(avg_bal_2014,cbr) %>% summarise(N=n()) %>% group_by(avg_bal_2014) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(cbr,p)
bal_cbr$avg_bal_2014 <- as.character(bal_cbr$avg_bal_2014)


bal_cbr <- rbind(bal_cbr,c('N',tenure_aux1$N))
write.table(bal_cbr,'clipboard-128',sep='\t',row.names=F,na='')


ggplot(dda2a,aes(x=ACCT_AMT_BAL_AVG_MONTH,fill=cbr))+geom_histogram(binwidth=250,alpha=0.5)+coord_cartesian(xlim=c(-1500,10000))+facet_grid(cbr~.)+theme_bw()+theme(legend.position="none")


#see if debit and opt-in maTTER

optin_rate <- dda2a  %>% group_by(ACCT_REG_E_FLAG_CUR_201401,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(ACCT_REG_E_FLAG_CUR_201401) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(optin_rate,'clipboard-128',sep='\t',row.names=F,na='')

optin_rate1 <- dda2a  %>% group_by(ACCT_REG_E_FLAG_CUR_201412,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(ACCT_REG_E_FLAG_CUR_201412) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(optin_rate1,'clipboard-128',sep='\t',row.names=F,na='')


#for debit the debit is at the HHLD level, not sure if this is perfect, 
#I could do it only for hhlds with 1 checking - let ssee

dda2a <- left_join(dda2a,card_tran1,by=c('ACCT_ID'='id'))
dda2a$VPOS_trans[is.na(dda2a$VPOS_trans)] <- 0
dda2a$vpos<- cut(dda2a$VPOS_trans,c(-0.01,.01,1,2,3,4,5,10,15,20,25,50,Inf),include.lowest = F)

vpos_rate <- dda2a  %>% group_by(vpos,nsf_fee) %>% summarise(N=n()) %>% spread(nsf_fee,N,fill=0) %>% mutate(rate=Yes/(`No with Fee`+Yes),tot=`No with Fee`+Yes,yes1=sum(Yes),tot1=sum(tot,na.rm=T)) %>% group_by(vpos) %>% mutate(p=my_test(Yes,yes1,tot,tot1)) 

write.table(vpos_rate,'clipboard-128',sep='\t',row.names=F,na='')

#I think it is the package for baltimore
stype_cbr <- dda2a %>% filter(!is.na(cbr)) %>% group_by(ACCT_STYPE,cbr) %>% summarise(N=n()) %>% 
  group_by(ACCT_STYPE) %>% mutate(p=N/sum(N)) %>%  select(-N) %>% spread(cbr,p)
stype_cbr$ACCT_STYPE <- as.character(stype_cbr$ACCT_STYPE)

tenure_aux1 <- dda2a %>% group_by(cbr) %>% summarise(N=n()) 

stype_cbr <- rbind(stype_cbr,c('N',tenure_aux1$N))
write.table(stype_cbr,'clipboard-128',sep='\t',row.names=F,na='')
write.cb(stype_cbr)

#it is not the balance, the negativer balance is the result of the NSF, it is a bit circular logic
#lets see if they NSF more in baltimore


