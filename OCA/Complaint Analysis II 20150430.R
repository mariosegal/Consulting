
#I want to find a way to analyze the complaints, quickly
#we need to be able to define a base for each type of acct 
#I also want to calculate age from owner age
#and crete a factor with all the package names

#I think we need some 4 panel pages
#one with age, tenure, package and cbr for complaint rate
#I also would liek to see concentrations of the broke_the law, dishonest, etc - there are 
#6 of those for 7 pages (potentially)


#I am going to do the analysis for Checking and card for now, as I am not sure about the mapping for HE/MTG and Consumer

dda_card <- subset(complaints_matched_full_new,(level_1_clean %in% c("Bank Account or Service") & level_2_clean=='Checking account') | level_1_clean == 'Credit Card')

packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Int','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking','Retail PMA','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA','Rewards Card','Signature Card','Basic Card')
packages <- gsub('Retail ','',packages)
packages <- gsub('Checking','',packages)
packages <- gsub(' +',' ',packages)
packages <- gsub(' $','',packages)
packages <- gsub('with Interest','w/Int',packages)
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA','REW','SIG','NOR')

dda_card$stype = factor(dda_card$aCCT_STYPE,stypes,labels=packages)

#create groups
groups1 <- dda_card %>% group_by(level_2_clean,level_3_clean,level_4_clean,level_5_clean) %>% summarise(N=n()) %>% group_by(level_2_clean) %>% arrange(desc(N)) 
groups1 = groups1 %>% mutate(group_id = 1:length(level_2_clean))

top_groups = groups1 %>% filter(group_id<=10)

#merge back the group _id to the data

dda_card_top = inner_join(dda_card,top_groups) %>% filter(match==1)
sum(top_groups$N)

#add the age from dob, at least to those with acctm, then fill with the HH_OWN age
load("Z:/M&T Projects/OCA/dob_2014_clean.rdata")
dob_2014_clean$ACCT_DATE_OF_BIRTH <- as.Date(dob_2014_clean$ACCT_DATE_OF_BIRTH,'%m/%d/%Y')
dob_2014_clean$age1 = as.numeric(as.Date('2014-12-31') - (dob_2014_clean$ACCT_DATE_OF_BIRTH))/365

dda_card_top = left_join(dda_card_top,dob_2014_clean[c('EXPRESSION_8','age1')])
dda_card_top$age1[is.na(dda_card_top$age1)] <- dda_card_top$HHLD_HH_OWN_AGE[is.na(dda_card_top$age1)]
sum(is.na(dda_card_top$age1))
dda_card_top$age1[dda_card_top$age1==0] <- NA #on original are missing
dda_card_top$age1 = cut(dda_card_top$age1,c(0,18,25,35,45,55,65,75,Inf),dig.lab = 10,ordered_result = T)

dda_card_top$tenure = as.numeric(as.Date('2014-12-31')-as.Date(dda_card_top$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y'))/365
dda_card_top$tenure = cut(dda_card_top$tenure,c(0,1,2,3,4,5,10,15,Inf),dig.labs=10,ordered_result = T)

dda_card_top$cbr <- factor(dda_card_top$HHLD_COMMUNITY_BANK_MARKET,1:17,labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA','SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))

#make it long
dda_card_top_long <- dda_card_top %>% 
  select(c(level_2_clean:level_5_clean,discriminated:dishonest,scra,ada,group_id:cbr,stype,complaint_master_id)) %>% 
  gather(measure,level,age1:stype)

dda_card_top_grouped <- dda_card_top_long %>% group_by(level_2_clean,levels=level_3_clean:level_4_clean:level_5_clean,group_id,measure,level) %>% summarise(complaints=n())
#now I can append the bases for cbr, stype, etc. by measure
#after I create them

load('accts_201412_oca.rdata')
base = subset(accts_201412_oca,(ACCT_PTYPE == "DDA" & substr(ACCT_STYPE,1,1)=='R')| (ACCT_PTYPE=='CCS' & ACCT_STYPE %in% c('REW','NOR','SIG')),c(ACCT_ID,EXPRESSION_8,ACCT_PTYPE,ACCT_STYPE,ACCT_DATE_OPENED_FOR_PRIME))
base$stype = factor(base$ACCT_STYPE,stypes,labels=packages)
base$tenure = as.numeric(as.Date('2014-12-31')-as.Date(base$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y'))/365
base$tenure = cut(base$tenure,c(0,1,2,3,4,5,10,15,Inf),dig.labs=10,ordered_result = T)

#merge DOB
base = left_join(base,dob_2014_clean[c(1,4)])
#merge cbr and hOH-age
load("Z:/M&T Projects/OCA/hhlds_201412.rdata")
base = left_join(base,hhlds_201412[c(1,15,19)],by=c('ACCT_ID'='HHLD_ID'))
base$age1[is.na(base$age1)] = base$HHLD_HH_OWN_AGE[is.na(base$age1)]
base$age1= cut(base$age1,c(0,18,25,35,45,55,65,75,Inf),dig.lab = 10,ordered_result = T)
base$cbr = factor(base$HHLD_COMMUNITY_BANK_MARKET,1:17,labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA','SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE'))

base$level_2_clean = ""
base$level_2_clean[base$ACCT_PTYPE=='DDA'] = 'Checking account'
base$level_2_clean[base$ACCT_PTYPE=='CCS'] = 'Credit Card'

#now make long and group
base_groups <- base %>% select(level_2_clean,cbr,stype,age1,tenure,EXPRESSION_8) %>%
  gather(measure,level,cbr:tenure) %>% group_by(level_2_clean,measure,level) %>% summarise(N=n())

#merge the 2 long sets, then you can just crunch it all
combined = left_join(dda_card_top_grouped,base_groups,by=c('level_2_clean','measure','level'))
combined$N[is.na(combined$N)] = 0
results = combined %>% mutate(rate=1000*complaints/N)
order <- data.frame(unique(results[c('measure','level')])) %>% group_by(measure) %>% mutate(order=1:length(measure))
#the order from cbr and from tenure is wrong, the correct one is on the original factor
tenure_order = 1:8
names(tenure_order) = levels(dda_card_top$tenure)  #this is a dictionary, a la python style
order$order[order$measure=='tenure'] <- tenure_order[order$level[order$measure=='tenure']]

cbr_order = 1:17
names(cbr_order) = levels(dda_card_top$cbr)
order$order[order$measure=='cbr'] <- cbr_order[order$level[order$measure=='cbr']]

results = left_join(results,order) %>% arrange(level_2_clean,group_id,measure,order)
results$rate[is.infinite(results$rate)] = 0
write.table(results,'clipboard-128',sep='\t',row.names=F)

#we want the average complaint rate by group_id 
#let summarise here and calculate in excel, it wil be fatser
complaints_group = inner_join(dda_card,top_groups) %>%  filter(group_id<=10) %>% group_by(level_2_clean,group_id) %>% summarise(complaints=n())
write.table(complaints_group,'clipboard-128',sep='\t',row.names=F)
N_group = base_groups %>% group_by(level_2_clean) %>% summarise(N=sum(N)/4)
write.table(N_group,'clipboard-128',sep='\t',row.names=F)
