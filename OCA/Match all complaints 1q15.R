load("Z:/M&T Projects/OCA/complaints_2014_2015Q1.rdata")

sum(is.na(complaints_2014_2015Q1$mask_ssn) & is.na(complaints_2014_2015Q1$mask_acct))
sum(! is.na(complaints_2014_2015Q1$mask_acct))
sum(! is.na(complaints_2014_2015Q1$mask_ssn))
sum(! is.na(complaints_2014_2015Q1$mask_acct) & ! is.na(complaints_2014_2015Q1$mask_ssn))

#lets match the accounts first - how many do I match
load("Z:/M&T Projects/OCA/accts_base_OCA_2014.rdata")
load("Z:/M&T Projects/OCA/accts_base_OCA_2015Q1.rdata")

length(intersect(base_all_2014$EXPRESSION_8,base_all_2015Q1$EXPRESSION_8))
#there are duplicates, and a quick view shows the same acct number but all esle different, very weird - luckilly only 1042

closed_all = bind_rows(closed_all_2014,closed_all_2015Q1)

sum(duplicated(closed_all$EXPRESSION_8))

closed_all <- closed_all %>% arrange(EXPRESSION_8,ACCT_DATE_CLOSED,closed_period)
closed1 <- closed_all[-which(duplicated(closed_all$EXPRESSION_8)),]  #take earliest close
sum(duplicated(closed1$EXPRESSION_8))


base_all = bind_rows(base_all_2014,base_all_2015Q1)
sum(duplicated(base_all$EXPRESSION_8))
base_all = base_all %>% arrange(EXPRESSION_8,ACCT_DATE_OPENED_FOR_PRIME,table)
all1 <- base_all[-which(duplicated(base_all)),]

all1 <- all1 %>% arrange(EXPRESSION_8,table,ACCT_DATE_OPENED_FOR_PRIME)
all1 <- all1[-which(duplicated(all1$EXPRESSION_8)),]
sum(duplicated(all1$EXPRESSION_8))
base <- left_join(all1,closed1[c(2,5)],by='EXPRESSION_8')
length(unique(base$EXPRESSION_8))


rm(all1,base_all_2014,closed_all_2014,closed1,base_all_2015Q1,closed_all_2015Q1,base_all,closed_all)

#fsplit into pieces
part1 <- subset(complaints_2014_2015Q1,!is.na(mask_acct))
part2 <- subset(complaints_2014_2015Q1,!is.na(mask_ssn) & is.na(mask_acct))
part3 <- subset(complaints_2014_2015Q1,is.na(mask_ssn) & is.na(mask_acct))

#work on those with  accts #s (part1)
base$ACCT_PTYPE[base$ACCT_PTYPE=='CCS' & base$aCCT_STYPE %in% c('SIG','REW','NOR')] <- 'CRD'
base$category = ""
base$category[base$ACCT_PTYPE=='CRD'] <- 'CRD'
base$category[base$ACCT_PTYPE %in% c('MTG') & base$aCCT_STYPE=='FHA'] <- 'FHA'
base$category[base$ACCT_PTYPE %in% c('MTG') & base$aCCT_STYPE=='VA'] <- 'VA'
base$category[base$ACCT_PTYPE %in% c('MTG') & base$aCCT_STYPE %in% c('CNV','PMI')] <- 'CNV'
base$category[base$ACCT_PTYPE %in% c('MTG') & base$aCCT_STYPE %in% c('OTH')] <- 'MTG OTH'
base$category[base$ACCT_PTYPE %in% c('HEQ')] <- 'HEQ'

base$category[base$ACCT_PTYPE %in% c('ILN') & base$aCCT_STYPE %in% c('BXX','CAV','DAL','DAV','ICA','LCC','LC1','LC3','LCR','SDA','SIA')] <- 'Auto'
base$category[base$ACCT_PTYPE %in% c('CCS') & base$aCCT_STYPE %in% c('LOC')] <- 'OD'
base$category[base$ACCT_PTYPE %in% c('ILN') & base$aCCT_STYPE %in% c('CGS','CGV','IMH','SCG','TD1')] <- 'CGS'
base$category[base$ACCT_PTYPE %in% c('ILN') & base$aCCT_STYPE %in% c('DPL','DPV','REV','SDP')] <- 'Unsecured'

base$category[base$ACCT_PTYPE %in% c('DDA')] <- 'DDA'
base$category[base$ACCT_PTYPE %in% c('TDA')] <- 'TDA'
base$category[base$ACCT_PTYPE %in% c('SAV','MMS') & !(base$aCCT_STYPE %in% c('R6B','R90','R91'))] <- 'SAV'
base$category[base$ACCT_PTYPE %in% c('SAV','MMS') & (base$aCCT_STYPE %in% c('R6B','R90','R91'))] <- 'OTH DEP'
base$category[base$ACCT_PTYPE %in% c('IRA')] <- 'OTH DEP'

levels(part1$level_2)=  gsub('\"',"",levels(part1$level_2),fixed=T)
part1$category = ""
part1$category[part1$level_2=='Credit Card'] <- "CRD"
part1$category[part1$level_2 %in% c('Home Equity Loan or HELOC','Home equity loan','HELOC')] <- 'HEQ'
part1$category[part1$level_2 %in% c('Conventional ARM','Conventional Fixed','Conventional fixed')] = 'CNV'
part1$category[part1$level_2 %in% c('FHA')] = 'FHA'
part1$category[part1$level_2 %in% c('VA Mortgage')] = 'VA'
part1$category[part1$level_1=='Mortgage or Home Equity' & part1$level_2 %in% c('Other Mortgage','Other')] = 'MTG OTH'

part1$category[part1$level_2=='Auto Loan'] <- 'Auto'
part1$category[part1$level_2=='Overdraft Line'] <- 'OD'
part1$category[part1$level_2=='Recreational/Mobile Home Loan'] <- 'OGS'
part1$category[part1$level_2 %in% c('Unsecured Loan','Unsecured loan')] <- 'Unsecured'

part1$category[part1$level_2=='Checking account'] <- 'DDA'
part1$category[part1$level_2 %in% c('(CD) Certificate of Deposit','(CD) Certificate of deposit')] <- 'TDA'
part1$category[part1$level_2=='Savings account'] <- 'SAV'
part1$category[part1$level_2 %in% c('Other deposit (IRA, Passbook, Holiday Club, etc)','Other deposit (IRA, Passbook, Holiday Club, etc.)','Other deposit (IRA')] <- 'OTH DEP'



length(unique(part1$mask_acct))
length(intersect(part1$mask_acct,base$EXPRESSION_8))
missing1 <- setdiff(part1$mask_acct,base$EXPRESSION_8)

part1a <- left_join(part1,base,by=c('mask_acct'='EXPRESSION_8','category'='category'))

levels(part1a$level_1) = tolower(levels(part1a$level_1))

table(part1a$level_1,part1a$ACCT_PTYPE)  #some weird matches to be fixed
part1a[part1a$level_1=='bank account or service' & !(part1a$ACCT_PTYPE %in% c('DDA','SAV','TDA',"IRA",'MMS')),35:42] = NA
part1a[part1a$level_1=='credit reporting' & (part1a$ACCT_PTYPE %in% c('DDA')),35:42] = NA

#CHECK IF LEVEL 2 MATCHES TO ptype
levels(part1a$level_2) = tolower(levels(part1a$level_2))
table(part1a$level_2,part1a$ACCT_PTYPE,useNA='ifany')

sum(is.na(part1a$ACCT_PTYPE) & !is.na(part1a$mask_ssn))  #can be tried on ssn side

extra <- part1a[is.na(part1a$ACCT_PTYPE) & !is.na(part1a$mask_ssn),]
part1a_matched = part1a[!is.na(part1a$ACCT_PTYPE) | (is.na(part1a$ACCT_PTYPE) & is.na(part1a$mask_ssn)),]
part1a_matched$source = ''
part1a_matched$source[!is.na(part1a_matched$ACCT_ID) & !is.na(part1a_matched$ACCT_PTYPE)] = 'acct_matched'
part1a_matched$source[is.na(part1a_matched$ACCT_ID) & is.na(part1a_matched$ACCT_PTYPE)] = 'acct_not_matched'

length(unique(part1a_matched$mask_acct))
intersect(extra$complaint_master_id,part1a_matched$complaint_master_id)
sum(!is.na(part1a_matched$ACCT_ID))
sum(!is.na(part1a_matched$ACCT_ID) & !is.na(part1a_matched$ACCT_PTYPE))
sum(!is.na(part1a_matched$ACCT_ID) & is.na(part1a_matched$ACCT_PTYPE))
sum(is.na(part1a_matched$ACCT_ID) & is.na(part1a_matched$ACCT_PTYPE))

#do the ssn piece
part2 <- rbind(part2,extra[names(part2)])  #add extra
length(intersect(part2$mask_ssn,base$EXPRESSION_18))


#I think I need to add a field for what to merge with, I do not want all the sav and web for the DDAs
part2$category = ""
levels(part2$level_2)=  gsub('\"',"",levels(part2$level_2),fixed=T)


part2$category[part2$level_2=='Credit Card'] <- "CRD"
part2$category[part2$level_2 %in% c('Home Equity Loan or HELOC','Home equity loan','HELOC')] <- 'HEQ'
part2$category[part2$level_2 %in% c('Conventional ARM','Conventional Fixed','Conventional fixed')] = 'CNV'
part2$category[part2$level_2 %in% c('FHA')] = 'FHA'
part2$category[part2$level_2 %in% c('VA Mortgage')] = 'VA'
part2$category[part2$level_1=='Mortgage or Home Equity' & part2$level_2 %in% c('Other Mortgage','Other')] = 'MTG OTH'

part2$category[part2$level_2=='Auto Loan'] <- 'Auto'
part2$category[part2$level_2=='Overdraft Line'] <- 'OD'
part2$category[part2$level_2=='Recreational/Mobile Home Loan'] <- 'OGS'
part2$category[part2$level_2 %in% c('Unsecured Loan','Unsecured loan')] <- 'Unsecured'

part2$category[part2$level_2=='Checking account'] <- 'DDA'
part2$category[part2$level_2 %in% c('(CD) Certificate of Deposit','(CD) Certificate of deposit')] <- 'TDA'
part2$category[part2$level_2=='Savings account'] <- 'SAV'
part2$category[part2$level_2 %in% c('Other deposit (IRA, Passbook, Holiday Club, etc)','Other deposit (IRA, Passbook, Holiday Club, etc.)','Other deposit (IRA')] <- 'OTH DEP'

levels(part2$level_2) = tolower(levels(part2$level_2))
levels(part2$level_1) = tolower(levels(part2$level_1))

#fix weird SSNs
base$EXPRESSION_18[base$EXPRESSION_18==0] <- NA

#now try to match by SSN and by category
part2a <- subset(part2,category != "")
part2b <- subset(part2,category == "")

#match the part2b ones to anything to get a HHLD ID only
hhlds <- unique(base[c('ACCT_ID','EXPRESSION_18')])
aux <- base %>% group_by(ACCT_ID) %>% summarise(N=n())
length(unique(base$EXPRESSION_18))
sum(duplicated(hhlds$EXPRESSION_18))  #there are 112515 SSNs in 2 or more HHLDS
hhlds = left_join(hhlds,aux)
hhlds = hhlds %>% arrange(EXPRESSION_18,desc(N))  #if I have to choose I will choose largest HHLD
hhlds = hhlds[-which(duplicated(hhlds$EXPRESSION_18)),]

part2b1 = left_join(part2b,hhlds[-3],by=c('mask_ssn' = 'EXPRESSION_18'))
sum(is.na(part2b1$ACCT_ID))
sum(!is.na(part2b1$ACCT_ID))

part2b1$source = ''
part2b1$source[is.na(part2b1$ACCT_ID)] = 'ssn_hhld_no'
part2b1$source[!is.na(part2b1$ACCT_ID)] = 'ssn_hhld_only'

#match the ones where we can match the category
part2a1 = left_join(part2a,base,by=c('mask_ssn'='EXPRESSION_18',"category"='category'))
sum(duplicated(part2a1$complaint_master_id))
length(unique(part2a1$complaint_master_id[(is.na(part2a1$ACCT_PTYPE))]))
length(unique(part2a1$complaint_master_id[(!is.na(part2a1$ACCT_PTYPE))]))

sum(duplicated(part2a1$complaint_master_id))  #7,670 matched multiple accounts

dupes = part2a1$complaint_master_id[which(duplicated(part2a1$complaint_master_id))]

part2a_no = subset(part2a1,!(complaint_master_id %in% dupes) & is.na(part2a1$ACCT_PTYPE))
part2a_solo = subset(part2a1,!(complaint_master_id %in% dupes) & !is.na(part2a1$ACCT_PTYPE))
part2a_mult = subset(part2a1,(complaint_master_id %in% dupes))
intersect(part2a_mult$complaint_master_id,part2a_solo$complaint_master_id)
intersect(part2a_mult$complaint_master_id,part2a_no$complaint_master_id)
intersect(part2a_no$complaint_master_id,part2a_solo$complaint_master_id)

#for multiple, I need to somehow choose
#I can choose older, or I can choose higher balance, 
#it is not obvious what to do without analyzing each one, and there are
#too mnay  cases so I can't do it by hand
#if we get hits for multiple, like NTG/HE who knoww what I will do
#or I can do the following, not choose an acct, just keep the HHLD_ID and I can do acct level analysis for those
#is not going to skew things
part2a_mult[36:42] <- NA

#some macth to multiple HHLDS, I will sort so that we can choose the older HHLD assuming that is  smallest number
part2a_mult = part2a_mult %>% group_by(complaint_master_id,ACCT_ID)  %>% arrange(complaint_master_id,ACCT_ID)

#take dupes out
part2a_mult <- part2a_mult[-which(duplicated(part2a_mult$complaint_master_id)),]
part2a_mult$source = 'ssn_multiple'

part2a_no$source = 'ssn_no'
part2a_solo$source = 'ssn_solo'

part3$source = 'neither'

#now lets assemble this 
complaints_2014_2015Q1_matched <- bind_rows(part1a_matched,part2a_no,part2a_solo,part2a_mult,part3,part2b1)

#final stats
complaints_2014_2015Q1_matched$match = 1
complaints_2014_2015Q1_matched$match[is.na(complaints_2014_2015Q1_matched$ACCT_PTYPE) & is.na(complaints_2014_2015Q1_matched$ACCT_ID)] <- 0
table(complaints_2014_2015Q1_matched$match)

save(complaints_2014_2015Q1_matched,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched.rdata')
save(base,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_base.rdata')


#I want to add the HHLD level details to it
#first i will take the latest one and match it, then i will see if the ones wth NAs can be improved and how many have
load("Z:/M&T Projects/OCA/hhld_all_2014.rdata")
load("Z:/M&T Projects/OCA/hhld_all_2015Q1.rdata")


hhld_aux <- bind_rows(hhld_all_2014,hhld_all_2015Q1) %>% arrange(HHLD_ID,desc(period)) %>% group_by(HHLD_ID) %>%  top_n(1,period)

#there are dupes, I do not know why exactly, it is only on some month and I think it is because I erged to IXI
#I think the iXI table in M&T has dupes soemhow,
#in any case, some browsing shows they are identical, so I will take one

hhld_aux = hhld_aux[-which(duplicated(hhld_aux$HHLD_ID)),]
tmp = left_join(complaints_2014_2015Q1_matched,hhld_aux,by=c('ACCT_ID'='HHLD_ID'))
prop.table(table(tmp$num_na))  #87% have no NAs, and 91% have 1
#the 1s are mstly CBR and that is fixable, or has a meaning
#the otehr large group is 3, and most are the CLV fields and that happens
#I am happy to move forward


complaints_2014_2015Q1_matched_full = left_join(complaints_2014_2015Q1_matched,hhld_aux[-12],by=c('ACCT_ID'='HHLD_ID'))

#add the dob
load("Z:/M&T Projects/OCA/dob_2014_2015Q1_clean.rdata")

dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH <- as.Date(dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH,'%m/%d/%Y')
dob_2014_2015Q1_clean$age1 = as.numeric(as.Date('2015-03-31') - (dob_2014_2015Q1_clean$ACCT_DATE_OF_BIRTH))/365

complaints_2014_2015Q1_matched_full = left_join(complaints_2014_2015Q1_matched_full,dob_2014_2015Q1_clean[c('EXPRESSION_8','age1')],by=c('mask_acct'='EXPRESSION_8'))
complaints_2014_2015Q1_matched_full$age1[is.na(complaints_2014_2015Q1_matched_full$age1)] <- complaints_2014_2015Q1_matched_full$HHLD_HH_OWN_AGE[is.na(complaints_2014_2015Q1_matched_full$age1)]
sum(is.na(complaints_2014_2015Q1_matched_full$age1))


save(complaints_2014_2015Q1_matched_full,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_full.rdata')
rm(tmp,hhld_aux,hhld_all_2014,hhld_all_2015Q1)


#################################
#match the escalated

#they said the sent me acct numbers
load("Z:/M&T Projects/OCA/escalated1.rdata")
sum(!is.na(escalated1$Masked_Number))
sum(is.na(escalated1$Masked_Number))


length(intersect(escalated1$Masked_Number,base$EXPRESSION_8))
escalated2 = left_join(escalated1,base,by=c('Masked_Number'='EXPRESSION_8'))


#check quality
table(escalated2$category.x,escalated2$ACCT_PTYPE)

#I am not sure how ron matched, but there are things that we get DDAs that the catogory is loan and so and that is weird. It aslo appears SAV/MMS are in cateogry DDA and that is possible, I asked M&T to confirm


#I am going to err on the side of keeping it, except when a DDA points to a loan complaint, the category must be assumed right, so I have to go with an error when Ron matched
escalated2[!is.na(escalated2$ACCT_PTYPE) & escalated2$ACCT_PTYPE=='DDA' & escalated2$category.x %in% c('time accounts','rvl','iln'),12:20] = NA 
escalated2[!is.na(escalated2$ACCT_PTYPE) & escalated2$ACCT_PTYPE=='DDA' & grepl('heloc|mortgage',escalated2$category.x ),12:20] = NA 
escalated2[!is.na(escalated2$ACCT_PTYPE) & escalated2$ACCT_PTYPE=='SAV' & escalated2$category.x %in% c('iln'),12:20] = NA 


#match the hhld data
escalated_matched_full = left_join(escalated2,hhld_aux[-12],by=c('ACCT_ID'='HHLD_ID'))
escalated_matched_full$match = 1
escalated_matched_full$match[is.na(escalated_matched_full$ACCT_PTYPE) & is.na(escalated_matched_full$ACCT_ID)] <- 0
table(escalated_matched_full$match)




escalated_matched_full = left_join(escalated_matched_full,dob_2014_2015Q1_clean[c('EXPRESSION_8','age1')],by=c('Masked_Number'='EXPRESSION_8'))
escalated_matched_full$age1[is.na(escalated_matched_full$age1)] <- escalated_matched_full$HHLD_HH_OWN_AGE[is.na(escalated_matched_full$age1)]

#you also need to prevent the match for the age
escalated_matched_full$age1[!is.na(escalated_matched_full$age1) & is.na(escalated_matched_full$ACCT_ID)] = NA 


sum(is.na(escalated_matched_full$age1))
sum(!is.na(escalated_matched_full$age1))



save(escalated_matched_full,file='Z:/M&T Projects/OCA/escalated_matched_full.rdata')


#create a super set for the repeat analysis
complaints_2014_2015Q1_matched_both = bind_rows(complaints_2014_2015Q1_matched_full,escalated_matched_full)
complaints_2014_2015Q1_matched_both$type = ""
complaints_2014_2015Q1_matched_both$type[is.na(complaints_2014_2015Q1_matched_both$complaint_master_id)] = 'escalated'
complaints_2014_2015Q1_matched_both$type[!is.na(complaints_2014_2015Q1_matched_both$complaint_master_id)] = 'non escalated'

complaints_2014_2015Q1_matched_both$type = factor(complaints_2014_2015Q1_matched_both$type)

#joint acct field
complaints_2014_2015Q1_matched_both$account_master = NA

complaints_2014_2015Q1_matched_both$source[is.na(complaints_2014_2015Q1_matched_both$source)] = "escalated"
#take mask acct if we have it and matched
complaints_2014_2015Q1_matched_both$account_master[complaints_2014_2015Q1_matched_both$source=='acct_matched'] = complaints_2014_2015Q1_matched_both$mask_acct[complaints_2014_2015Q1_matched_both$source=='acct_matched']


#take EXPRESSION_ 8 if we matched by ssn once
complaints_2014_2015Q1_matched_both$account_master[complaints_2014_2015Q1_matched_both$source=='ssn_solo'] = complaints_2014_2015Q1_matched_both$EXPRESSION_8[complaints_2014_2015Q1_matched_both$source=='ssn_solo']

#for escalated take the accts fielf, it is an acct or NA
complaints_2014_2015Q1_matched_both$account_master[complaints_2014_2015Q1_matched_both$type=='escalated'] = complaints_2014_2015Q1_matched_both$Masked_Number[complaints_2014_2015Q1_matched_both$type=='escalated']


save(complaints_2014_2015Q1_matched_both,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')
