load("Z:/M&T Projects/OCA/complaints.rdata")

sum(is.na(complaints$mask_ssn) & is.na(complaints$mask_acct))
sum(! is.na(complaints$mask_acct))
sum(! is.na(complaints$mask_ssn))
sum(! is.na(complaints$mask_acct) & ! is.na(complaints$mask_ssn))

#lets match the accounts first - how many do I match
load("Z:/M&T Projects/OCA/accts_base_OCA_2014.rdata")

sum(duplicated(closed_all_2014$EXPRESSION_8))
closed_all_2014 <- closed_all_2014 %>% arrange(EXPRESSION_8,ACCT_DATE_CLOSED,closed_period)
closed1 <- closed_all_2014[-which(duplicated(closed_all_2014$EXPRESSION_8)),]  #take earliest close
sum(duplicated(closed1$EXPRESSION_8))

sum(duplicated(base_all_2014$EXPRESSION_8))
all1 <- base_all_2014[-which(duplicated(base_all_2014)),]
#all1$table[is.na(all1$table)] <- '201401'
all1 <- all1 %>% arrange(EXPRESSION_8,table,ACCT_DATE_OPENED_FOR_PRIME)
all1 <- all1[-which(duplicated(all1$EXPRESSION_8)),]
sum(duplicated(all1$EXPRESSION_8))
base <- left_join(all1,closed1[c(2,5)],by='EXPRESSION_8')
length(unique(base$EXPRESSION_8))


rm(all1,base_all_2014,closed_all_2014,closed1)

#fsplit into pieces
part1 <- subset(complaints,!is.na(mask_acct))
part2 <- subset(complaints,!is.na(mask_ssn) & is.na(mask_acct))
part3 <- subset(complaints,is.na(mask_ssn) & is.na(mask_acct))

#work on those with  accts #s (part1)
base$ACCT_PTYPE[base$ACCT_PTYPE=='CCS' & base$aCCT_STYPE %in% c('SIG','REW','NOR')] <- 'CRD'
base$category = ""
base$category[base$ACCT_PTYPE=='CRD'] <- 'CRD'
base$category[base$ACCT_PTYPE %in% c('HEQ','MTG')] <- 'MTG'
base$category[base$ACCT_PTYPE %in% c('CCS','ILN','SLN')] <- 'Loan'
base$category[base$ACCT_PTYPE %in% c('DDA')] <- 'DDA'
base$category[base$ACCT_PTYPE %in% c('TDA')] <- 'TDA'
base$category[base$ACCT_PTYPE %in% c('SAV','MMS')] <- 'SAV'
base$category[base$ACCT_PTYPE %in% c('IRA')] <- 'IRA'

part1$category = ""
part1$category[part1$category=='Credit Card'] <- "CRD"
part1$category[part1$level_1=='Mortgage or Home Equity'] <- 'MTG'
part1$category[part1$level_1=='Consumer Loan'] <- 'Loan'
part1$category[part1$level_2=='Checking account'] <- 'DDA'
part1$category[part1$level_2=='(CD) Certificate of Deposit'] <- 'TDA'
part1$category[part1$level_2=='Savings account'] <- 'SAV'
part1$category[part1$level_2=='Other deposit (IRA'] <- 'IRA'



length(unique(part1$mask_acct))
length(intersect(part1$mask_acct,base$EXPRESSION_8))
missing1 <- setdiff(part1$mask_acct,base$EXPRESSION_8)

part1a <- left_join(part1,base,by=c('mask_acct'='EXPRESSION_8','category'='category'))
table(part1a$level_1,part1a$ACCT_PTYPE)  #some weird matches to be fixed

part1a[part1a$level_1=='Bank Account or Service' & !(part1a$ACCT_PTYPE %in% c('DDA','SAV','TDA',"IRA",'MMS')),33:38] = NA
part1a[part1a$level_1=='Credit Card' & !(part1a$ACCT_PTYPE  == 'CCS' & part1a$aCCT_STYPE %in% c('SIG','NOR','REW')),33:38] = NA
part1a[part1a$level_1=='Mortgage or Home Equity' & !(part1a$ACCT_PTYPE %in% c('MTG','HEQ')),33:38] = NA
part1a[part1a$level_1=='Consumer Loan' & (part1a$ACCT_PTYPE %in% c('DDA','HEQ','SAV',"WEB") | 
                                            (part1a$ACCT_PTYPE %in% c('CCS') & part1a$aCCT_STYPE %in% c('SIG','NOR','REW'))),33:38] = NA
part1a[part1a$level_1=='Credit Reporting' & (part1a$ACCT_PTYPE %in% c('DDA')),33:38] = NA

#CHECK IF LEVEL 2 MATCHES TO ptype
table(part1a$level_2,part1a$ACCT_PTYPE,useNA='ifany')

sum(is.na(part1a$ACCT_PTYPE) & !is.na(part1a$mask_ssn))  #can be tried on ssn side

extra <- part1a[is.na(part1a$ACCT_PTYPE) & !is.na(part1a$mask_ssn),]
part1a_matched = part1a[!is.na(part1a$ACCT_PTYPE) | (is.na(part1a$ACCT_PTYPE) & is.na(part1a$mask_ssn)),]
length(unique(part1a_matched$mask_acct))
intersect(extra$complaint_master_id,part1a_matched$complaint_master_id)
sum(!is.na(part1a_matched$ACCT_ID))
sum(!is.na(part1a_matched$ACCT_ID) & !is.na(part1a_matched$ACCT_PTYPE))
sum(!is.na(part1a_matched$ACCT_ID) & is.na(part1a_matched$ACCT_PTYPE))
sum(is.na(part1a_matched$ACCT_ID) & is.na(part1a_matched$ACCT_PTYPE))

#do the ssn piece
part2 <- rbind(part2,extra[names(part2)])  #add extra
length(intersect(part2$mask_ssn,base$EXPRESSION_18))

#if I merge them I am going to have a mess, as sometimes you will get multiple matches
test <- left_join(part2,base,by=c('mask_ssn'='EXPRESSION_18'))
length(duplicated(test$complaint_master_id))
sum(duplicated(test$complaint_master_id))

#I think I need to add a field for what to merge with, I do not want all the sav and web for the DDAs
part2$category = ""
part2$category[part2$category=='Credit Card'] <- "CRD"
part2$category[part2$level_1=='Mortgage or Home Equity'] <- 'MTG'
part2$category[part2$level_1=='Consumer Loan'] <- 'Loan'
part2$category[part2$level_2=='Checking account'] <- 'DDA'
part2$category[part2$level_2=='(CD) Certificate of Deposit'] <- 'TDA'
part2$category[part2$level_2=='Savings account'] <- 'SAV'
part2$category[part2$level_2=='Other deposit (IRA'] <- 'IRA'



#fix weird SSNs
base$EXPRESSION_18[base$EXPRESSION_18==0] <- NA

#now try to match by SSN and by category
part2a <- subset(part2,category != "")
part2b <- subset(part2,category == "")

#match the part2b ones to anything to get a HHLD ID only
hhlds <- unique(base[c('ACCT_ID','EXPRESSION_18')])
aux <- base %>% group_by(ACCT_ID) %>% summarise(N=n())
length(unique(base$EXPRESSION_18))
sum(duplicated(hhlds$EXPRESSION_18))  #there are 104809 SSNs in 2 or more HHLDS
hhlds = left_join(hhlds,aux)
hhlds = hhlds %>% arrange(EXPRESSION_18,desc(N))  #if I have to choose I will choose largest HHLD
hhlds = hhlds[-which(duplicated(hhlds$EXPRESSION_18)),]

part2b1 = left_join(part2b,hhlds[-3],by=c('mask_ssn' = 'EXPRESSION_18'))
sum(is.na(part2b1$ACCT_ID))
sum(!is.na(part2b1$ACCT_ID))



#match the ones where we can match the category
part2a1 = left_join(part2a,base,by=c('mask_ssn'='EXPRESSION_18',"category"='category'))
sum(duplicated(part2a1$complaint_master_id))
length(unique(part2a1$complaint_master_id[(is.na(part2a1$ACCT_PTYPE))]))
length(unique(part2a1$complaint_master_id[(!is.na(part2a1$ACCT_PTYPE))]))
table(test1$level_1,test1$ACCT_PTYPE)
sum(duplicated(part2a1$complaint_master_id))  #5,545 matched multiple accounts

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
#4,542 cases so I can't do it by hand
#if we get hits for multiple, like NTG/HE who knoww what I will do
#or I can do the following, not choose an acct, just keep the HHLD_ID and I can do acct level analysis for those
#I like this better - it is 4,500 out of 63,000 - 7% is not going to skew things
part2a_mult[34:39] <- NA
#take dupes out
part2a_mult <- part2a_mult[-which(duplicated(part2a_mult$complaint_master_id)),]

#now lets assemble this 
complaints_matched <- bind_rows(part1a_matched,part2a_no,part2a_solo,part2a_mult,part3,part2b1)

#final stats
complaints_matched$match = 1
complaints_matched$match[is.na(complaints_matched$ACCT_PTYPE) & is.na(complaints_matched$ACCT_ID)] <- 0
table(complaints_matched$match)

save(complaints_matched,file='complaints_matched.rdata')
save(base,file='complaints_base.rdata')


#I want to add the HHLD level details to it
#first i will take the latest one and match it, then i will see if the ones wth NAs can be improved and how many have
load("Z:/M&T Projects/OCA/hhld_all_2014.rdata")
hhld_aux <- hhld_all_2014 %>% arrange(HHLD_ID,desc(period)) %>% group_by(HHLD_ID) %>%  top_n(1,period)
#there are dupes, I do not know why exactly, it is only on some month and I think it is because I erged to IXI
#I think the iXI table in M&T has dupes soemhow,
#in any case, some browsing shows they are identical, so I will take one

hhld_aux = hhld_aux[-which(duplicated(hhld_aux$HHLD_ID)),]
tmp = left_join(complaints_matched,hhld_aux,by=c('ACCT_ID'='HHLD_ID'))
prop.table(table(tmp$num_na))  #87% have no NAs, and 91% have 1
#the 1s are mstly CBR and that is fixable, or has a meaning
#the otehr large group is 3, and most are the CLV fields and that happens
#I am happy to move forward


complaints_matched_full = left_join(complaints_matched,hhld_aux[-12],by=c('ACCT_ID'='HHLD_ID'))
save(complaints_matched_full,file='complaints_matched_full.rdata')
rm(tmp,hhld_aux,hhld_all_2014)

