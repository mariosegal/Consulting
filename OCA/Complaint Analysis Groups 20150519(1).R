
#I want to find a way to analyze the complaints, quickly
#we need to be able to define a base for each type of acct 
#I also want to calculate age from owner age
#and crete a factor with all the package names

#I think we need some 4 panel pages
#one with age, tenure, package and cbr for complaint rate
#I also would liek to see concentrations of the broke_the law, dishonest, etc - there are 
#6 of those for 7 pages (potentially)


#I am going to do the analysis for Checking and card for now, as I am not sure about the mapping for HE/MTG and Consumer

load('Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')

#clean up levels
###level_2
levels(complaints_2014_2015Q1_matched_both$level_2)[22:23] = levels(complaints_2014_2015Q1_matched_both$level_2)[21]
levels(complaints_2014_2015Q1_matched_both$level_2)[c(17,23)] = levels(complaints_2014_2015Q1_matched_both$level_2)[34]
levels(complaints_2014_2015Q1_matched_both$level_2)[30] = levels(complaints_2014_2015Q1_matched_both$level_2)[31]


#level3
levels(complaints_2014_2015Q1_matched_both$level_3)[22] = levels(complaints_2014_2015Q1_matched_both$level_3)[23]
levels(complaints_2014_2015Q1_matched_both$level_3)[41] = levels(complaints_2014_2015Q1_matched_both$level_3)[42]
levels(complaints_2014_2015Q1_matched_both$level_3)[9] = levels(complaints_2014_2015Q1_matched_both$level_3)[10]
levels(complaints_2014_2015Q1_matched_both$level_3)[59] = levels(complaints_2014_2015Q1_matched_both$level_3)[60]



complaintsx <- subset(complaints_2014_2015Q1_matched_both,
                      (level_1 %in% c("bank account or service") & level_2 %in% c('checking account','(cd) certificate of deposit','other deposit (ira, passbook, holiday club, etc)','savings account')) | level_1 == 'credit card' | level_1 =="consumer loan" | level_1== 'mortgage or home equity')


#adjust the mortgage levels
complaintsx$level_2 = as.character(complaintsx$level_2)
complaintsx$level_2[complaintsx$level_2 %in% c('conventional arm','conventional fixed')] = 'conventional'
complaintsx$level_2[complaintsx$level_2 %in% c('reverse mortgage')] = 'other mortgage'

#drop the levels I can't map products to
#Secured Loan on Auto
complaintsx = subset(complaintsx,level_2!= 'secured loan')

#create an stype field, that incorporates the other for cehcking
complaintsx = complaintsx[-which(complaintsx$ACCT_PTYPE %in% c('DDA','MMS','TDA','IRA','SAV') & substr(complaintsx$aCCT_STYPE,1,1) == 'C' ),]
complaintsx$stype = NA
complaintsx$stype[!is.na(complaintsx$ACCT_PTYPE) & complaintsx$ACCT_PTYPE=='DDA' & !(complaintsx$aCCT_STYPE %in% c('RA2','RA8','RC6','RE5','RE6','RE7','RH2','RH3','RH6','RW2','RW3','RX2',"RH5"))]='Other Checking'


lookup1 = c('Classic','Classic w/Int','@College','EZ Choice','My Choice','Free','Select w/int','My Choice Plus w/int','Power','Select','My Choice Plus','Direct','My Choice Prem')
names(lookup1) = c('RA2','RA8','RC6','RE5','RE6','RE7','RH2','RH3','RH6','RW2','RW3','RX2',"RH5")

complaintsx$stype[complaintsx$ACCT_PTYPE=='DDA' & is.na(complaintsx$stype) & complaintsx$aCCT_STYPE %in% names(lookup1)] <- lookup1[complaintsx$aCCT_STYPE[complaintsx$ACCT_PTYPE=='DDA' & is.na(complaintsx$stype) & complaintsx$aCCT_STYPE %in% names(lookup1)]]

#thi smay need to be adjusted if new data i used
#for credit card
complaintsx$stype[complaintsx$aCCT_STYPE=='NOR'] = 'Standard Card'
complaintsx$stype[complaintsx$aCCT_STYPE=='REW'] = 'Rewards Card'
complaintsx$stype[complaintsx$aCCT_STYPE=='SIG'] = 'Signature Card'

#for MTG/HE
complaintsx$stype[complaintsx$aCCT_STYPE=='FHA'] = 'FHA MTG'
complaintsx$stype[complaintsx$aCCT_STYPE=='VA '] = 'VA MTG'
complaintsx$stype[complaintsx$aCCT_STYPE=='FRM'] = 'Other MTG'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('SHE','HQL')] = 'Home Equity'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('PMI','CNV')] = 'Conventional'

complaintsx$level_2[complaintsx$aCCT_STYPE=='FHA'] = 'FHA MTG'
complaintsx$level_2[complaintsx$aCCT_STYPE=='FRM'] = 'Other MTG'
complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('SHE','HQL')] = 'Home Equity'
complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('PMI','CNV')] = 'Conventional'


#for loans
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('SIA','LC3','SDA','EOL','ICA')] = 'Auto Loan'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('LOC','EBL')] = 'Overdraft Line'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('SDP','CID','SIH','CSI','CSD')] = 'Unsecured Loan'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('IMH','SCG','CGV','TD1')] = 'Recr./ Mobile Home'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('SLN','2MG')] = 'Unsecured Loan'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('SLN')] = 'Unsecured Loan'

complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('SIA','LC3','SDA','EOL','ICA')] = 'Auto Loan'
complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('LOC','EBL')] = 'Overdraft Line'
complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('SDP','CID','SIH','CSI','CSD')] = 'Unsecured Loan'
complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('IMH','SCG','CGV','TD1')] = 'Recr./ Mobile Home'
complaintsx$level_2[complaintsx$ACCT_PTYPE %in% c('SLN','2MG')] = 'Unsecured Loan'
complaintsx$level_2[complaintsx$ACCT_PTYPE %in% c('SLN')] = 'Unsecured Loan'

#other deposits
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('R9M','R9I','R9E','R9N')] = 'MMS'
complaintsx$stype[complaintsx$aCCT_STYPE %in% c('R2B','R3B','R7M')] = 'Savings'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('TDA') & complaintsx$aCCT_STYPE %in% c('RAO')] = 'Add On'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('TDA') & complaintsx$aCCT_STYPE %in% c('R6M')] = 'Up to 6M'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('TDA') & complaintsx$aCCT_STYPE %in% c('R2Y')] = '1-2 Years'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('TDA') & complaintsx$aCCT_STYPE %in% c('R2S','R2+')] = 'Over 2Yr'
complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('TDA') & complaintsx$aCCT_STYPE %in% c('R1Y')] = '6 to 12M'

complaintsx$stype[complaintsx$ACCT_PTYPE %in% c('IRA')] = 'IRA'
complaintsx$stype[complaintsx$ACCT_STYPE %in% c('R90','R91')] = 'Club Accounts'
complaintsx$stype[complaintsx$ACCT_STYPE %in% c('R6B')] = 'Passbooks'

complaintsx$level_2[complaintsx$aCCT_STYPE %in% c('R2B','R3B','R7M','R9M','R9I','R9E','R9N')] = 'Savings MMS'
complaintsx$level_2[complaintsx$ACCT_PTYPE %in% c('TDA')] = 'CD'
complaintsx$level_2[complaintsx$ACCT_PTYPE %in% c('IRA')] = 'Other Deposits'
complaintsx$level_2[complaintsx$ACCT_STYPE %in% c('R90','R91','R6B')] = 'Other Deposits'

#the complainst with no STYPE still have the old name
complaintsx$level_2[complaintsx$level_2=="other deposit (ira, passbook, holiday club, etc)"]='Other Deposits'
complaintsx$level_2[complaintsx$level_2=="(cd) certificate of deposit"]='CD'
complaintsx$level_2[complaintsx$level_2=="recreational/mobile home loan"]='Recr./ Mobile Home'
complaintsx$level_2[complaintsx$level_2=="savings account"]='Savings MMS'
complaintsx$level_2[complaintsx$level_2=="fha"]='FHA MTG'
complaintsx$level_2[complaintsx$level_2=="heloc"]='Home Equity'
complaintsx$level_2[complaintsx$level_2=="other mortgage"]='Other MTG'
complaintsx$level_2[complaintsx$level_2=="va mortgage"]='VA MTG'

complaintsx$level_2[complaintsx$level_2=="conventional arm"]='Conventional'
complaintsx$level_2[complaintsx$level_2=="conventional fixed"]='Conventional'
complaintsx$level_2[complaintsx$level_2=="conventional"]='Conventional'

complaintsx$level_2[complaintsx$level_2=="unsecured loan"]='Unsecured Loan'
complaintsx$level_2[complaintsx$level_2=="overdraft line"]='Overdraft Line'
complaintsx$level_2[complaintsx$level_2=="Overdraft Line"]='Overdraft Line'
complaintsx$level_2[complaintsx$level_2=="Overdraft line"]='Overdraft Line'
complaintsx$level_2[complaintsx$level_2=="credit card"]='Credit Card'
complaintsx$level_2[complaintsx$level_2=="checking account"]='Checking Account'
complaintsx$level_2[complaintsx$level_2=="auto loan"]='Auto Loan'

#careful running this line
complaintsx = complaintsx[!(complaintsx$aCCT_STYPE %in% c('CML','CPL','RWI','RWD')),]

complaintsx$level_4[complaintsx$level_4=="overdraft / insufficient funds fees"]=  "overdraft/insufficient funds fees"
complaintsx$level_4[complaintsx$level_4=="mortgage / home equity"]=  "mortgage/home equity"
complaintsx$level_4[complaintsx$level_4=="statements & documentation"]=  "statements and documentation"

complaintsx$level_5[complaintsx$level_5=='statement arrived late']=  "statement received late"
complaintsx$level_5[complaintsx$level_5=="rewards program"]=  "rewards"
complaintsx$level_5[complaintsx$level_5=="loan / credit card payment delay"]= 'loan/credit card payment delay'
complaintsx$level_5[complaintsx$level_5=="late charge / fees"]= 'late charge/fees'
complaintsx$level_5[complaintsx$level_5=="incorrect information on checks"]= 'incorrect info on checks'
complaintsx$level_5[complaintsx$level_5=="customer service / customer relations"]= 'customer service/customer relations'
complaintsx$level_5[complaintsx$level_5=="credit line increase / decrease"]= 'credit line increase/decrease'

complaintsx$level_5[complaintsx$level_5=="collections dispute"]= 'collection disputes'
complaintsx$level_5[complaintsx$level_5=="closing costs / fees"]= 'closing costs/fees'

complaintsx$level_5[complaintsx$level_5=="card activiation issue"]= 'card activation issue'
complaintsx$level_5[complaintsx$level_5=="card activation issue"]= 'card activation'
complaintsx$level_5[complaintsx$level_5=="bill pay / bank to bank processing time frame"]= 'bill pay/bank to bank processing time frame'


#create groups
groups1 <- complaintsx %>% group_by(level_2,level_3,level_4,level_5) %>% summarise(N=n()) %>% group_by(level_2) %>% arrange(desc(N)) 
groups2 = groups1 %>% filter(N>=100)
groups2$group_id = 1:dim(groups2)[1]


#merge back the group _id to the data

complaints_top = inner_join(complaintsx,groups2) #%>% filter(match==1)

#I need to fix back fill the acct-number sI fgot from ssn matches
complaints_top$acct= complaints_top$EXPRESSION_8
complaints_top$acct = ifelse(is.na(complaints_top$acct) & !is.na(complaints_top$mask_acct) ,complaints_top$mask_acct,complaints_top$acct )

complaints_top$age1[complaints_top$age1==0] <- NA #on original are missing

complaints_top$age1 = cut(complaints_top$age1,c(0,18,25,35,45,55,65,75,Inf),labels=c('Up to 18','19 to 25','26 to 35','36 to 45','46 to 55','56 to 65','66 to 75','Over 75'),dig.lab = 10,ordered_result = T)

complaints_top$tenure = as.numeric(as.Date('2015-03-31')-as.Date(complaints_top$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y'))/365
complaints_top$tenure = cut(complaints_top$tenure,c(0,1,2,3,4,5,10,15,Inf),labels=c('Up to 1','1 to 2','2 to 3','3 to 4','4 to 5','5 to 10','10 to 15','Over 15'),dig.labs=10,ordered_result = T)

complaints_top$cbr <- factor(complaints_top$HHLD_COMMUNITY_BANK_MARKET,1:17,labels=c('WNY','Roch','Syr','South','Albny','Tarry','NYC','Philly','PA N','C&W PA','SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))

#massge segment
complaints_top$segment <- complaints_top$HHLD_LIFE_CYCLE_SEGMENT
complaints_top$segment[complaints_top$segment==8] = 1
complaints_top$segment[complaints_top$segment==9] = 4
complaints_top$segment[is.na(complaints_top$segment)] = 7
complaints_top$segment = factor(complaints_top$segment,levels=c(1:7),
       labels=c('Build Fut','Mass Aff no Kids','Mainst Fam','Mass Affl Fam','Mainst Retired',"Mass Affl Ret",'Not Coded'),ordered=T)

#services
complaints_top$services = complaints_top$HHLD_NUMSERVICES
complaints_top$services = cut(complaints_top$services,c(-Inf,0,1,2,3,4,5,Inf),labels=c('None','1','2','3','4','5','6 or More'),ordered_result = T,include.lowest = F)

#balances
load("Z:/M&T Projects/OCA/bals_201312.rdata")
complaints_top = left_join(complaints_top,bals_201312,by=c('acct'='EXPRESSION_8'))
sum(is.na(complaints_top$ACCT_AMT_BAL_FOR_PRIME))
rm(bals_201312)
complaints_top$balance = cut(complaints_top$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,250,500,1000,2500,5000,10000,Inf),
                             labels=c('Zero or Less','Up to 250','250 to 500','500 to 1,000','1,000 to 2,500','2,500 to 5,000','5,000 to 10,000','Over 10,000'))

complaints_top$balance2 = cut(complaints_top$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,25000,50000,100000,250000,Inf),
                             labels=c('Zero or Less','Up to 25K','25K to 50K','50K to 100K','100K to 250K','Over 250K'))

#get group totals
groups3 = complaints_top %>% group_by(group_id) %>% summarise(total=n()) %>% arrange(desc(total)) 
groups3 = groups3 %>% mutate(group_new=1:dim(groups3)[1])
complaints_top = left_join(complaints_top,groups3)

#make it long
complaints_top_long <- complaints_top %>% 
  select(c(level_2:level_5,discriminated:dishonest,scra,ada,age1,tenure,cbr,services,segment,balance,balance2,stype,complaint_master_id,group_new,total)) %>% 
  gather(measure,level,age1:stype) %>% group_by(group_new,level_2,level_3,level_4,level_5,measure,level) %>% arrange(group_new,measure,level)

levels(complaints_top_long$level_4)[1] = 'blank'
levels(complaints_top_long$level_5)[1] = 'blank'

#I want to make sure we get all the empty levels, even if zero
#dplyr still does not doe that (it is an improvement pending)
#trick is to create all the levels on a side 
aux_grp = bind_rows(expand.grid(measure='balance',level=levels(complaints_top$balance)),
                    expand.grid(measure='balance2',level=levels(complaints_top$balance2)),
                    expand.grid(measure='cbr',level=levels(complaints_top$cbr)),
                    expand.grid(measure='age1',level=levels(complaints_top$age1)),
                    expand.grid(measure='tenure',level=levels(complaints_top$tenure)),
                    expand.grid(measure='segment',level=levels(complaints_top$segment)),
                    expand.grid(measure='services',level=levels(complaints_top$services)))

complaints_top_grouped <- complaints_top_long %>% 
  group_by(group_new,total,level_2,level_3,level_4,level_5,measure,level) %>% 
  summarise(complaints=n()) 

save(complaints_top,file='Z:/M&T Projects/OCA/complaints_top_20150521.rdata')



#now I can append the bases for cbr, stype, etc. by measure
#after I create them

load('Z:/M&T Projects/OCA/accts_201503_oca.rdata')
base = subset(accts_201503_oca,(ACCT_PTYPE == "DDA" & substr(ACCT_STYPE,1,1)=='R') | 
                (ACCT_PTYPE %in% c('ILN','CCS','MTG','HEQ','SLN')) | 
                (ACCT_PTYPE %in% c('MMS','SAV','TDA','IRA') & substr(ACCT_STYPE,1,1)=='R'),
              c(ACCT_ID,EXPRESSION_8,ACCT_PTYPE,ACCT_STYPE,ACCT_DATE_OPENED_FOR_PRIME))


#adjust the level_2 for base
#adjust the mortgage levels
base$level_2 = ""
base$level_2[base$ACCT_PTYPE == 'MTG' & base$ACCT_STYPE %in% c('CNV','PMI')] = 'Conventional'
base$level_2[base$ACCT_PTYPE == 'MTG' & base$ACCT_STYPE %in% c('FHA')] = 'FHA MTG'
base$level_2[base$ACCT_PTYPE == 'MTG' & base$ACCT_STYPE %in% c('VA','VA ')] = 'VA'
base$level_2[base$ACCT_PTYPE == 'HEQ'] = 'Home Equity'
base$level_2[base$ACCT_PTYPE == 'DDA' & substr(base$ACCT_STYPE,1,1)=='R'] = 'Checking account'
base$level_2[base$ACCT_PTYPE %in% c('CCS') & base$ACCT_STYPE %in% c('REW','NOR','SIG')] = 'Credit Card'
base$level_2[base$ACCT_PTYPE %in% c('MMS') & substr(base$ACCT_STYPE,1,1)=='R']  = 'Savings MMS'
base$level_2[base$ACCT_PTYPE %in% c('SAV') & substr(base$ACCT_STYPE,1,1)=='R']  = 'Savings MMS'
base$level_2[base$ACCT_PTYPE %in% c('TDA') & substr(base$ACCT_STYPE,1,1)=='R']  = 'CD'
base$level_2[base$ACCT_PTYPE %in% c('IRA') & substr(base$ACCT_STYPE,1,1)=='R']  = 'Other Deposits'
base$level_2[base$ACCT_PTYPE == 'ILN' & base$ACCT_STYPE %in% c('DAL','DAV','CAV','LCC','LC1','LC3','LCR','SDA','BXX','SIA')] = 'Auto Loan'
base$level_2[base$ACCT_PTYPE == 'ILN' & base$ACCT_STYPE %in% c('REV','SDP')] = 'Unsecured Loan'
base$level_2[base$ACCT_PTYPE == 'SLN' ] = 'Unsecured Loan'
base$level_2[base$ACCT_PTYPE %in% c('CCS') & base$ACCT_STYPE %in% c('LOC','EBL')] = 'Overdraft Line'


#create an stype field, that incorporates the other for cehcking
base$stype = NA
base$stype[!is.na(base$ACCT_PTYPE) & base$ACCT_PTYPE=='DDA' & !(base$ACCT_STYPE %in% c('RA2','RA8','RC6','RE5','RE6','RE7','RH2','RH3','RH6','RW2','RW3','RX2',"RH5"))]='Other Checking'

lookup1 = c('Classic','Classic w/Int','@College','EZ Choice','My Choice','Free','Select w/int','My Choice Plus w/int','Power','Select','My Choice Plus','Direct','My Choice Prem')
names(lookup1) = c('RA2','RA8','RC6','RE5','RE6','RE7','RH2','RH3','RH6','RW2','RW3','RX2',"RH5")

base$stype[base$ACCT_PTYPE=='DDA' & is.na(base$stype) & base$ACCT_STYPE %in% names(lookup1)] <- lookup1[as.character(base$ACCT_STYPE[base$ACCT_PTYPE=='DDA' & is.na(base$stype) & base$ACCT_STYPE %in% names(lookup1)])]

#for credit card
base$stype[base$ACCT_STYPE=='NOR'] = 'Standard Card'
base$stype[base$ACCT_STYPE=='REW'] = 'Rewards Card'
base$stype[base$ACCT_STYPE=='SIG'] = 'Signature Card'

#for MTG/HE
base$stype[base$ACCT_STYPE=='FHA'] = 'FHA MTG'
base$stype[base$ACCT_STYPE=='VA '] = 'VA MTG'
base$stype[base$ACCT_STYPE=='FRM'] = 'Other MTG'
base$stype[base$ACCT_STYPE=='HUD'] = 'Other MTG'
base$stype[base$ACCT_STYPE %in% c('SHE','HQL','MHE','HEV','RWH')] = 'Home Equity'
base$stype[base$ACCT_STYPE %in% c('PMI','CNV')] = 'Conventional'

#for loans
base$stype[base$ACCT_STYPE %in% c('SIA','LC3','SDA','EOL')] = 'Auto Loan'
base$stype[base$ACCT_STYPE %in% c('LOC','EBL')] = 'Overdraft Line'
base$stype[base$ACCT_STYPE %in% c('SDP','CID','SIH','CSI','CSD')] = 'Unsecured Loan'
base$stype[base$ACCT_STYPE %in% c('IMH','SCG','CGV','TD1')] = 'Recr./ Mobile Home'
base$stype[base$ACCT_STYPE %in% c('RWI','RWD')] = 'Other'
base$stype[base$ACCT_STYPE %in% c('SLN','2MG')] = 'Unsecured Loan'
base$stype[base$ACCT_PTYPE %in% c('SLN')] = 'Unsecured Loan'

#other deposits
#base$stype[base$ACCT_STYPE %in% c('R2B','R3B','R7M','R9M','R9I','R9E','R9N')] = 'Savings MMS'
#base$stype[base$ACCT_PTYPE %in% c('TDA')] = 'CD'
base$stype[base$ACCT_PTYPE %in% c('IRA')] = 'Other Deposits'
base$stype[base$ACCT_STYPE %in% c('R90','R91','R6B')] = 'Other Deposits'

base$stype[base$ACCT_STYPE %in% c('R9M','R9I','R9E','R9N')] = 'MMS'
base$stype[base$ACCT_STYPE %in% c('R2B','R3B','R7M')] = 'Savings'
base$stype[base$ACCT_PTYPE %in% c('TDA') & base$ACCT_STYPE %in% c('RAO')] = 'Add On'
base$stype[base$ACCT_PTYPE %in% c('TDA') & base$ACCT_STYPE %in% c('R6M')] = 'Up to 6M'
base$stype[base$ACCT_PTYPE %in% c('TDA') & base$ACCT_STYPE %in% c('R2Y')] = '1-2 Years'
base$stype[base$ACCT_PTYPE %in% c('TDA') & base$ACCT_STYPE %in% c('R2S','R2+')] = 'Over 2Yr'
base$stype[base$ACCT_PTYPE %in% c('TDA') & base$ACCT_STYPE %in% c('R1Y')] = '6 to 12M'

base = base[!(base$ACCT_STYPE %in% c('CML','CPL','CRA','RWD','RWI')),]  #I do not know if we workouts are auto or 

#I need to check that level_2 and stype and in synch, in partivulr the base neeeds t have what is on the complaints
setdiff(unique(complaints_top_grouped$stype),unique(base$stype))
setdiff(unique(complaints_top_grouped$level_2),unique(base$level_2))
setdiff(unique(base$level_2),unique(complaints_top_grouped$level_2))

base$level_2[base$level_2=="Checking account"] = 'Checking Account'

# some level_2 wwre mising, adjust that
table(base$stype[base$level_2==""])
base$level_2[base$stype=="Auto Loan"] = 'Auto Loan'
base$level_2[base$stype=="Unsecured Loan"] = 'Unsecured Loan'
base$level_2[base$stype=="Other MTG"] = 'Other MTG'
base$level_2[base$stype=="Recr./ Mobile Home"] = 'Recr./ Mobile Home'
table(base$stype[base$level_2==""])

base$tenure = as.numeric(as.Date('2015-03-31')-as.Date(base$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y'))/365
base$tenure = cut(base$tenure,c(0,1,2,3,4,5,10,15,Inf),labels=c('Up to 1','1 to 2','2 to 3','3 to 4','4 to 5','5 to 10','10 to 15','Over 15'),dig.labs=10,ordered_result = T)


#merge DOB
load("Z:/M&T Projects/OCA/dob_2014_2015Q1_clean.rdata")
#dob = subset(dob_2014,dob_2014$EXPRESSION_8 %in% base$EXPRESSION_8)
dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH <- as.Date(dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH,'%m/%d/%Y')
dob_2014_2015Q1_clean$age1 = as.numeric(as.Date('2015-03-31') - (dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH))/365
base= merge(base,dob_2014_2015Q1_clean[c(1,3)],all.x=T)
rm(dob_2014_2015Q1_clean)
save(base,file='z:/M&T Projects/OCA/base_20150521.rdata')

#merge cbr and hOH-age
load("Z:/M&T Projects/OCA/hhld_all_2015Q1.rdata")
#load("Z:/M&T Projects/base_20150501.rdata")
hhld_all_2015Q1 = hhld_all_2015Q1 %>% arrange(HHLD_ID,desc(period))
hhld_aux = unique(hhld_all_2015Q1[-11])
hhld_aux = hhld_all_2015Q1[-which(duplicated(hhld_all_2015Q1$HHLD_ID)),]
base = left_join(base,hhld_aux[c( 'HHLD_ID',"HHLD_COMMUNITY_BANK_MARKET","HHLD_HH_OWN_AGE", "HHLD_NUMSERVICES"  , "HHLD_LIFE_CYCLE_SEGMENT"   )],by=c('ACCT_ID'='HHLD_ID'))
base$age1[is.na(base$age1)] = base$HHLD_HH_OWN_AGE[is.na(base$age1)]
base$age1= cut(base$age1,c(0,18,25,35,45,55,65,75,Inf),dig.lab = 10,labels=c('Up to 18','19 to 25','26 to 35','36 to 45','46 to 55','56 to 65','66 to 75','Over 75'),ordered_result = T)
base$cbr = factor(base$HHLD_COMMUNITY_BANK_MARKET,1:17,labels=c('WNY','Roch','Syr','South','Albny','Tarry','NYC','Philly','PA N','C&W PA','SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))

#add segment
base$segment <- base$HHLD_LIFE_CYCLE_SEGMENT
base$segment[base$segment==8] = 1
base$segment[base$segment==9] = 4
base$segment[is.na(base$segment)] = 7
base$segment = factor(base$segment,levels=c(1:7),
                      labels=c('Build Fut','Mass Aff no Kids','Mainst Fam','Mass Affl Fam','Mainst Retired',"Mass Affl Ret",'Not Coded'),ordered=T)

#services
base$services = base$HHLD_NUMSERVICES
base$services = cut(base$services,c(-Inf,0,1,2,3,4,5,Inf),labels=c('None','1','2','3','4','5','6 or More'),ordered_result = T,include.lowest = F)

#balances
load("Z:/M&T Projects/OCA/bals_201312.rdata")
base = left_join(base,bals_201312)
sum(is.na(base$ACCT_AMT_BAL_FOR_PRIME))
rm(bals_201312)
base$balance = cut(base$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,250,500,1000,2500,5000,10000,Inf),
                   labels=c('Zero or Less','Up to 250','250 to 500','500 to 1,000','1,000 to 2,500','2,500 to 5,000','5,000 to 10,000','Over 10,000'))

base$balance2 = cut(base$ACCT_AMT_BAL_FOR_PRIME,c(-Inf,0,25000,50000,100000,250000,Inf),
                    labels=c('Zero or Less','Up to 25K','25K to 50K','50K to 100K','100K to 250K','Over 250K'))


save(base,file='Z:/M&T Projects/OCA/base_20150521.rdata')

#now make long and group
base_groups <- base %>% select(level_2,cbr,stype,age1,tenure,segment,balance,balance2,services,EXPRESSION_8) %>%
  gather(measure,level,c(cbr,tenure,age1,segment,services,balance,balance2,stype)) %>% 
  group_by(level_2,measure,level) %>% summarise(accts=n())

#level_2 base counts
base %>% group_by(level_2) %>% summarise(N=n())




###############################
#merge the 2 long sets, then you can just crunch it all

#check they are consistent
setdiff(unique(base_groups$level_2),unique(complaints_top_grouped$level_2))   #should be something, as not all made it to top
setdiff(unique(complaints_top_grouped$level_2),unique(base_groups$level_2))  #has to be NULL


setdiff(unique(complaints_top_grouped$measure),unique(base_groups$measure))
setdiff(unique(complaints_top_grouped$level),unique(base_groups$level))


combined = left_join(complaints_top_grouped,base_groups,by=c('level_2','measure','level'))
sum(is.na(combined$accts))
sum(is.na(combined$total))

#which are NAs
table(combined$level[is.na(combined$accts) ],useNA='ifany')
table(combined$level_2[is.na(combined$accts) & !is.na(combined$level)],useNA='ifany')
View(combined[which(is.na(combined$accts) & !is.na(combined$level)),])

#I am going to add a flag for the ones that have NA accts ans are not level==NA (because those get excluded later from charts), I will use the flag later to make the rate be NA or something

combined$flag = 0
combined$flag[(is.na(combined$accts) & !is.na(combined$level))] = 1

#View(subset(combined,is.na(accts)))
combined$accts[is.na(combined$accts)] = 0
results = combined %>% mutate(rate=1000*complaints/accts)
results$rate[is.infinite(results$rate)] = NA

#I also need an average complaint rate by group, and confidence interval
complaints_aux = complaints_top %>% group_by(group_new,level_2) %>% summarise(complaints=n())
accts_aux = base %>% group_by(level_2) %>% summarise(N=n())
#[accts_aux$level_2=='Checking account'] = 'Checking Account'

group_rate = left_join(complaints_aux,accts_aux) %>% mutate(p=complaints/N,group_rate=1000*complaints/N,lower=1000*(p -qnorm(0.95)*sqrt((p)*(1-p)*(1/N))),higher=1000*(p +qnorm(0.95)*sqrt((p)*(1-p)*(1/N))))

#results is givign a very high  rate for HHlds with no services and I think ineed to make those rates NA,it is because you get a single acct here an dthere, but that make no sense (have no servide son base)

results$rate[results$measure=="services" & results$level=='None'] = NA


#create a super factor, concatyenate the labels in the order from original factors, the create as ordered=T
#for stype we need to define order manually
#if we do more groups thi sneedss to be revised
stype_order = c("MMS","Savings","@College","Free",'EZ Choice','My Choice','My Choice Plus','My Choice Plus w/int','My Choice Prem','Direct','Select','Select w/int','Power','Classic','Classic w/Int','Other Checking','Auto Loan','Conventional','Home Equity','FHA MTG',"Recr./ Mobile Home",'Standard Card','Rewards Card','Signature Card')
levelsx = c(levels(complaints_top$age1),levels(complaints_top$tenure),levels(complaints_top$cbr),levels(complaints_top$services),levels(complaints_top$segment),levels(complaints_top$balance),levels(complaints_top$balance2),stype_order)
#THERE ARE 2 'Zero or Less', from the 2 balance factors, this is fine, take one out
levelsx= levelsx[-which(duplicated(levelsx))]
results$level = factor(results$level,levelsx,ordered=T)

results = results %>% arrange(group_new,measure,level)
write.csv(results,'z:/M&T Projects/OCA/results_20150521.csv',row.names=F)

View(results[results$group_new==1,])

save(results,file='z:/M&T Projects/OCA/results_20150521.rdata')

#I need to assign the doc number
complaints_2014_2015Q1_matched_both$doc = 1:dim(complaints_2014_2015Q1_matched_both)[1]
complaints_top = left_join(complaints_top,complaints_2014_2015Q1_matched_both[c('complaint_master_id','doc')])

#mtb colors
lime = rgb(122,184,0,maxColorValue = 256)
green = rgb(0,120,86,maxColorValue = 256)
yellow = rgb(255,179,0,maxColorValue = 256)
purple = rgb(134,73,157,maxColorValue = 256)
green2 = rgb(195,231,111,maxColorValue = 256)
gray = rgb(128,128,128,maxColorValue = 256)
teal = rgb(35,164,145,maxColorValue = 256)
colors = c(green2,green,purple, teal,lime,yellow,yellow,gray)  

titles = c('Community Bank','Product Subtype','Account Owner Age','Account Tenure (Years)','Customer Segment','Account Balance','Account Balance','Number of HHLD Services')
names(titles) = c('cbr','stype','age1','tenure','segment','balance','balance2','services')
#ready for the page development
library(ggplot2)
library(stringr)
library(scales)
library(gridExtra)
library(wordcloud)

load('z:/M&T Projects/OCA/ngrams3_1Q2015.rdata')
load('z:/M&T Projects/OCA/ngrams4_1Q_2015.rdata')

mychart =  function(i,measurex,data=results) {
  ch_data = data %>% filter(group_new==i & !is.na(level) & measure ==measurex &!is.na(rate) & level!='Not Coded' & level!='None')
  aux_label = ifelse(ch_data$level_2[1] %in% c('Conventional','FHA MTG','Home Equity','Recr./ Mobile Home'),'balance','balance2')
  ymax = ceiling(max(data$rate[data$group_new==i & !is.na(data$level) & data$level != 'Not Coded' & data$level != 'None' & data$measure != aux_label ],na.rm=T)+2)
  #get stats for mean and interval
  avg1 = group_rate$group_rate[group_rate$group_new==i]
  top = group_rate$higher[group_rate$group_new==i]
  bottom = group_rate$lower[group_rate$group_new==i]
  right1 = dim(ch_data)[1]
  j=which(names(titles) == measurex)
  
  ch = ggplot(ch_data,aes(x=level,y=rate,label=comma(round(rate,1))))
  ch = ch + geom_bar(stat='identity',fill=colors[j])
  ch = ch + theme_bw() +scale_x_discrete('',labels=function(x) str_wrap(x,width=5))
  ch = ch + geom_rect(xmin=0.4,xmax=right1+0.6,ymin=bottom,ymax=top,fill='wheat',alpha=.2)
  ch = ch + geom_hline(yintercept=avg1,color='firebrick2',linetype=2)
  
  ch = ch +scale_y_continuous('',breaks=NULL) 
  ch = ch +theme(legend.position='none',panel.grid.major=element_blank(),plot.title=element_text(face='bold',color='steelblue4',size=14),panel.border=element_blank(),axis.line=element_line(color='black'),axis.ticks=element_blank(),axis.text.x=element_text(angle=0,vjust=0.5,size=12))
  ch = ch + ggtitle(paste('Complaint Rate by',titles[measurex],'\n(per 1,000 Accts) '))
  if(!(measurex %in% c('balance','balance2'))) {
    ch = ch+theme(axis.line.y=element_blank())+geom_text(vjust=-1,size=4)+ coord_cartesian(ylim=c(0,ymax))
  }
  if((measurex %in% c('balance','balance2'))) {
    ch = ch+coord_flip(ylim=c(0,ymax))+theme(axis.line.x=element_blank())+geom_text(hjust=-1,size=6)
  }
  if (measurex %in% c('stype','cbr')){
    ch = ch+theme(axis.text.x=element_text(angle=90,vjust=0.5,size=4))
  }
  return(ch)
}

make_page1 = function(grp1,page=NA,results1=results,group_rate1=group_rate) {
  aux = results1[results1$group_new==grp1,][1,]
  title = str_wrap(paste0('Group #',grp1,': ',aux$level_2,'~',aux$level_3,'~',aux$level_4,'~',aux$level_5," (N=",comma(group_rate1$complaints[group_rate1$group_new==grp1]),")"),width=70)
  page1 = arrangeGrob(mychart(grp1,names(titles)[1]),
                       mychart(grp1,names(titles)[2]),
                       mychart(grp1,names(titles)[3]),
                       mychart(grp1,names(titles)[4]),
                       ncol=2,main=textGrob(title,x=0.5,y=unit(0.90,'npc'),hjust=0.5,vjust=1,gp=gpar(fontsize=16,col='steelblue4',fontface='bold')),
                      sub=textGrob(paste('Page',page),x=0.5,y=unit(4,'mm'),hjust=0.5,vjust=0,gp=gpar(fontsize=10)))
  return(page1)
}

make_page2 = function(grp1,page=NA,results1=results,group_rate1=group_rate) {
  aux = results1[results1$group_new==grp1,][1,]
  bal_ch=ifelse(aux$level_2 %in% c('Conventional','FHA MTG','Home Equity','Recr./ Mobile Home'),7,6)
  title = str_wrap(paste0('Group #',grp1,': ',aux$level_2,'~',aux$level_3,'~',aux$level_4,'~',aux$level_5," (N=",comma(group_rate1$complaints[group_rate1$group_new==grp1]),")"),width=70)
  page2 = arrangeGrob(mychart(grp1,names(titles)[bal_ch]),
                       arrangeGrob(mychart(grp1,names(titles)[5]),
                                   mychart(grp1,names(titles)[8]),
                                   nrow=2),
                       ncol=2,main=textGrob(title,x=0.5,y=unit(0.90,'npc'),hjust=0.5,vjust=1,gp=gpar(fontsize=16,col='steelblue4',fontface='bold')),
                       sub=textGrob(paste('Page',page),x=0.5,y=unit(4,'mm'),hjust=0.5,vjust=0,gp=gpar(fontsize=10)))
  return(page2)
}


# nf <- layout(matrix(c(3,4,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2),10,2,byrow=T))
# par(mar=c(0,0,0,0))
# plot.new()
# 
# text(x=0.5,y=0.5,'3 word sentences')
# 
# text(x=0.5,y=0.5,'4 word sentences')
# layout.show(nf)




wordcloud_page = function(grpwc,pagewc) {
  aux = results[results$group_new==grpwc,][1,]
  title = str_wrap(paste0('Group #',grpwc,': ',aux$level_2,'~',aux$level_3,'~',aux$level_4,'~',aux$level_5," (N=",comma(group_rate$complaints[group_rate$group_new==grpwc]),")"),width=70)
  docs = complaints_top$doc[complaints_top$group_new==grpwc]
  doc_num = comma(sum(complaints_top$comment[complaints_top$group_new==grpwc]!=""))
  aux4 = ngrams4 %>% filter(doc %in% docs) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% ungroup() %>% arrange(desc(freq)) %>% filter(!is.na(freq))
  aux3 = ngrams3 %>% filter(doc %in% docs) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% ungroup() %>% arrange(desc(freq)) %>% filter(!is.na(freq))
  nf <- layout(matrix(c(5,5,3,4,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,6,6),12,2,byrow=T))
  
    par(mar=c(0,0,0,0))
    wordcloud(aux3$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,rot.per=0,scale=c(1.25,.1),max.words=min(ceiling(.5*dim(aux4)[1]),50))
 
    par(mar=c(0,0,0,0))
    wordcloud(aux4$ngram,aux4$freq,color=brewer.pal(8,"Dark2"),random.order=F,rot.per=0,scale=c(1.25,.1),max.words=min(ceiling(.5*dim(aux4)[1]),50))

  
  plot.new()
  text(x=0.5,y=0.5,paste0('Top 3 Word Sentences','\n(Number of Comments = ',doc_num,')'),cex=1.5,col='steelblue4')
  plot.new()
  text(x=0.5,y=0.5,paste0('Top 4 Word Sentences','\n(Number of Comments = ',doc_num,')'),cex=1.5,col='steelblue4')
  plot.new()
  text(0.5,0.5,title,cex=2,,col='steelblue4')
  plot.new()
  text(0.5,0.5,paste('Page',pagewc))
}

#dim(group_rate)[1]

page_num=1
pdf('Z:/M&T Projects/OCA/Analysis of top complaits with text 20150521.pdf',height = 8.25, width=10.75,paper='USr')
cat('Pages: ')
for (grp in 1:dim(group_rate)[1]) {
  print(make_page1(grp,page_num))
  cat(grp,'..',sep="")
  page_num = page_num+1
  print(make_page2(grp,page_num))
  #cat(page_num,'..',sep="")
  page_num = page_num+1
  #sum(complaints_top$comment[complaints_top$group_new==grp]!="")>0
  if ( dim(ngrams4 %>% filter(doc %in% complaints_top$doc[complaints_top$group_new==grp]  & !is.na(freq)))[1]>0 ) {
    wordcloud_page(grp,page_num)
    #cat(page_num,'..',sep="")
    page_num = page_num+1
  }
}
dev.off()


save(results,complaints_top,base,file='z:/M&T Projects/OCA/results_analysis_image_20150521.rdata')


ngrams4 %>% filter(doc %in% complaints_top$doc[complaints_top$group_new==grp])
