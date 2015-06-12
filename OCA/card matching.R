#credit card match rate is very low
#I want to ensure there is no error

cards <- subset(complaints_matched_full_new,level_2_clean=='Credit Card')
sum(!is.na(cards$mask_acct))
sum(is.na(cards$mask_acct) & !is.na(cards$mask_ssn))
sum(is.na(cards$mask_acct) & is.na(cards$mask_ssn))

#get the card base
load("Z:/M&T Projects/OCA/accts_base_OCA_2014.rdata")
card_base <- subset(base_all_2014,ACCT_PTYPE=='CCS' & aCCT_STYPE %in% c('REW',"NOR",'SIG'))
card_base = card_base[-which(duplicated(card_base$EXPRESSION_8)),]

intersect(cards$mask_acct,card_base$EXPRESSION_8)

head(cards$mask_acct[!is.na(cards$mask_acct)])


length(intersect(cards$mask_ssn,card_base$EXPRESSION_18))

cards1 <- subset(cards,mask_ssn %in% card_base$EXPRESSION_18)

cards2 <- inner_join(cards1,card_base,by=c('mask_ssn'='EXPRESSION_18'))
which(duplicated(cards2$complaint_master_id))[1:10]
View(subset(cards2,complaint_master_id==4576))


table(table(cards2$complaint_master_id))

dupes = cards2$complaint_master_id[which(duplicated(cards2$complaint_master_id))]
cards2_solo = subset(cards2,!(complaint_master_id %in% dupes))
cards2_mult= subset(cards2,(complaint_master_id %in% dupes))

cards2_mult2 = unique(cards2_mult[c(1,52)])
#one matched 2 HH, pick one
cards2_mult2 = cards2_mult2[-which(duplicated(cards2_mult2$complaint_master_id)),]

#all I want is to get by complaint_master_id the new data, then I will figure how to match it
cards_matched_new = bind_rows(cards2_solo[c(1,52:56)],cards2_mult2)
#adjust names
names(cards_matched_new) <- gsub('.y','',names(cards_matched_new),fixed=T)

#add also the CBR, and the DOB 
load("Z:/M&T Projects/OCA/dob_2014_clean.rdata")
load("Z:/M&T Projects/OCA/hhld_all_2014.rdata")
cards_matched_new = left_join(cards_matched_new,dob_2014_clean[-3])
sum(is.na(cards_matched_new$ACCT_DATE_OF_BIRTH ))
hhld_aux <- hhld_all_2014 %>% arrange(HHLD_ID,desc(period)) %>% group_by(HHLD_ID) %>%  top_n(1,period)
hhld_aux = hhld_aux[-which(duplicated(hhld_aux$HHLD_ID)),]
cards_matched_new = left_join(cards_matched_new,hhld_aux[1:10],by=c('ACCT_ID'='HHLD_ID'))
rm(hhld_aux,hhld_all_2014)
#the question now is what is the best way to add this data
#I like the dictionary idea, as I can then only affect the 3,500 or so card rows
row.names(cards_matched_new) = as.numeric(as.character(cards_matched_new$complaint_master_id))

#cards_matched_new = cards_matched_new[c("complaint_master_id" ,"ACCT_ID","ACCT_PTYPE","aCCT_STYPE","ACCT_DATE_OPENED_FOR_PRIME","EXPRESSION_8")]  #reorder to match

#I want to replace columns 23:26 from the master for those rows tat match by complaint_master_id, with 
#columns 2:5



cols1=c("ACCT_PTYPE","aCCT_STYPE","ACCT_DATE_OPENED_FOR_PRIME","EXPRESSION_8","HHLD_COMMUNITY_BANK_MARKET","HHLD_HH_OWN_AGE","HHLD_NUMSERVICES","HHLD_AMT_CONTRIB_MTD","HHLD_LIFE_CYCLE_SEGMENT","HHLD_CLV_TOTAL","HHLD_CLV_REMAINING","HHLD_CLV_REMAINING_TENURE","IXI_WC_TOTAL_ASSETS")

#make a copy to test
test = complaints_matched_full_new

#I need to get the list of all the complaint matser id to match
aux = test$complaint_master_id[test$complaint_master_id %in% cards_matched_new$complaint_master_id]
#I need a set with the replcements
replace = cards_matched_new[match(aux,cards_matched_new$complaint_master_id),]

#only do it if all match
if(sum(test[test$complaint_master_id %in% cards_matched_new$complaint_master_id,'complaint_master_id']==replace$complaint_master_id)!=0 ) {
  test[test$complaint_master_id %in% cards_matched_new$complaint_master_id,cols1] = replace[,cols1]
}
complaints_matched_full_new1 = test

save(complaints_matched_full_new1,file='Z:/M&T Projects/OCA/complaints_matched_full_new1.rdata')
