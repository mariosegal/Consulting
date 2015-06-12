#Nishith requested I do an updated matrix from 2012 to 2014
#to match numbers from front


#to do that I will use base_new, I will have to:
#   i. define a new existing and new flag for that, existing will be as of jan 2012 1 year tenure
#   ii. create lost flags for 2012, 2013 and 2014
#   iii. classify as accidental, ocassional, frequent and no (for existing only)
#   iv. run all  the stats to validate
#   v. then attempt the analysis of who goes to no NSFs and who does less NSFs (binary)


#load data and libarries
#I will load all the data from image but keep only what I need as it is too much already

load("Z:/M&T Projects/NSF/nsf_image_20150311.rdata")
rm(list=setdiff(ls(),c('nsf_fees_all','base_new')))
load("Z:/M&T Projects/NSF/contrib.rdata")

library(dplyr)
library(tidyr)
library(ggplot2)


#base_new doe snot have the number of months for 12 to 14, or the tot num and I need to do that from contrib
#I did all claculations even if some where there, I need to check

nsf_summary_new <- contrib %>% select(c(EXPRESSION_8:ACCT_CONTR_TOTAL_NSF_FEES_WAIVED,period)) %>% 
  group_by(year=substr(period,1,4),EXPRESSION_8) %>%
  summarise(fees=sum(ACCT_CONTR_TOTAL_NSF_FEES),waivers=sum(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED),months=sum(ACCT_CONTR_TOTAL_NSF_FEES>0),present=n_distinct(year)) %>%
  gather(measure,value,fees:present) %>% unite(key,measure,year) %>% spread(key,value,fill=NA)


nsf_summary_new$present_2012[is.na(nsf_summary_new$present_2012)] <- 0
nsf_summary_new$present_2013[is.na(nsf_summary_new$present_2013)] <- 0
nsf_summary_new$present_2014[is.na(nsf_summary_new$present_2014)] <- 0

tmp <- left_join(base_new,nsf_summary_new,by='EXPRESSION_8')
sum(tmp$fees_2012.x!=tmp$fees_2012.y,na.rm=T)
sum(tmp$fees_2013.x!=tmp$fees_2013.y,na.rm=T) 
sum(tmp$waivers_2012!=tmp$waived_2012,na.rm=T)
sum(tmp$waivers_2013!=tmp$waived_2013,na.rm=T)
sum(tmp$waivers_2014!=tmp$waived_2014,na.rm=T)
View(head(tmp,100))
#tmp showed that the data was the same, so i can drop it and do a merge
rm(tmp)

base_new1 <- full_join(base_new[1:7],nsf_summary_new,by='EXPRESSION_8')

rm(contrib)

colSums(base_new[8:13],na.rm=T)
colSums(base_new1[8:19],na.rm=T)

#I need to add some details for those that are in nsf_summary_new and not on base_new
load("Z:/M&T Projects/NSF/closed_accts.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")

missing <- base_new1$EXPRESSION_8[is.na(base_new1$ACCT_STYPE)]

load('Z:/M&T Projects/NSF/accts_201201_all.rdata')

#logically all the missing should have been closed in 201201, check you have them in the new extract and then append the data somehow
#I think others were opened in 201201 as well and I had not counted them

extra <- subset(base_new1,EXPRESSION_8 %in% missing)
length(setdiff(missing,accts_201201_all$EXPRESSION_8))
length(intersect(missing,accts_201201_all$EXPRESSION_8))  #22,242 are on the set who are they
found <- subset(accts_201201_all,EXPRESSION_8 %in% intersect(missing,accts_201201_all$EXPRESSION_8))
names(found)[2:4] <- c('date','stype1','closed1')

table(is.na(found$ACCT_DATE_CLOSED))  # those 22,242 were closed in 201201 
found$stype1 <- as.character(found$stype1 )

still_missing <- subset(extra,!(EXPRESSION_8 %in% intersect(missing,accts_201201_all$EXPRESSION_8)))
#who are the others, I will have to look them up manually on sql
#they are likely acquired, as they appear suddenly with an old open date, 
# at least I found 22K or so, lets see how much I miss after I tag those with sTYPE and open_date


base_new1 %>% group_by(a=!is.na(ACCT_STYPE)) %>% summarise_each(funs(sum(.,na.rm=T)),fees_2012:waivers_2014)


base_new1 <- left_join(base_new1,found)
base_new1$ACCT_DATE_OPENED_FOR_PRIME[is.na(base_new1$ACCT_DATE_OPENED_FOR_PRIM) & !is.na(base_new1$date)] <- as.Date(base_new1$date[is.na(base_new1$ACCT_DATE_OPENED_FOR_PRIM) & !is.na(base_new1$date)],'%m/%d/%Y')

base_new1$ACCT_STYPE[is.na(base_new1$ACCT_STYPE) & !is.na(base_new1$stype1)] <- base_new1$stype1[is.na(base_new1$ACCT_STYPE) & !is.na(base_new1$stype1)]

base_new1$closed[is.na(base_new1$closed) & !is.na(base_new1$closed1)] <- format(as.Date(base_new1$closed1[is.na(base_new1$closed) & !is.na(base_new1$closed1)],'%m/%d/%Y'),'%Y%m')

base_new1$closed12 <- ifelse(base_new1$closed >= '201201' & base_new1$closed <= "201212",1,0)
base_new1$closed13 <- ifelse(base_new1$closed >= '201301' & base_new1$closed <= "201312",1,0)
base_new1$closed14 <- ifelse(base_new1$closed >= '201401' & base_new1$closed <= "201412",1,0)

View(subset(base_new1,!is.na(stype1))[1:100,])
base_new1$exclude <- 0
base_new1$exclude[which(base_new1$EXPRESSION_8 %in% still_missing$EXPRESSION_8)] <- 1

#1) define front and back book flags

base_new1$book <- ifelse(base_new1$ACCT_DATE_OPENED_FOR_PRIME < '2011-01-01','back','front')
base_new1$book[is.na(base_new1$book)] <- 'exclude'
table(format(base_new1$ACCT_DATE_OPENED_FOR_PRIME,"%Y") ,base_new1$book)  #looks good
base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),fees_2012:fees_2014)
base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),waivers_2012:waivers_2014)

base_new1 %>% group_by(exclude) %>% summarise_each(funs(sum(.,na.rm=T)),fees_2012:fees_2014)
base_new1 %>% group_by(exclude) %>% summarise_each(funs(sum(.,na.rm=T)),waivers_2012:waivers_2014)

#we are accounting for all so we are golden

###########
#2) classify into groups - only existing
base_new1$group_12 <- NA
base_new1$group_12 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2012==0,'No_NSF',base_new1$group_12)
base_new1$group_12 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2012==1,'Accidental',base_new1$group_12)
base_new1$group_12 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2012 %in% 2:4,'Occasional',base_new1$group_12)
base_new1$group_12 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2012 %in% 5:12,'Frequent',base_new1$group_12)
table(base_new1$group_12,useNA='ifany')

base_new1$group_14 <- NA
base_new1$group_14 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2014==0,'No_NSF',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2014==1,'Accidental',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2014 %in% 2:4,'Occasional',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_2012==1 & base_new1$book=='back' & base_new1$months_2014 %in% 5:12,'Frequent',base_new1$group_14)
base_new1$group_14 <- ifelse(base_new1$present_2012==1 & !is.na(base_new1$closed) & base_new1$closed<='201212' & base_new1$book=='back' ,'Lost_2012',base_new1$group_14)

base_new1$group_14 <- ifelse(base_new1$present_2012==1 & !is.na(base_new1$closed) & base_new1$closed<='201312' & base_new1$closed>='201301'  & base_new1$book=='back' ,'Lost_2013',base_new1$group_14)

base_new1$group_14 <- ifelse(base_new1$present_2012==1 & !is.na(base_new1$closed) & base_new1$closed<='201412' & base_new1$closed>='201401'  & base_new1$book=='back' ,'Lost_2014',base_new1$group_14)

#some remain that look to have ben opened in 2014 but I see NA months, I will call them NO_NSF
base_new1$group_14 <- ifelse(is.na(base_new1$group_14) & base_new1$book=='back','No_NSF',base_new1$group_14)
table(base_new1$group_14,useNA='ifany')


#I forgot the with, do it now
base_new1$with_2012 <- ifelse(base_new1$months_2012>0,1,0)
base_new1$with_2013 <- ifelse(base_new1$months_2013>0,1,0)
base_new1$with_2014 <- ifelse(base_new1$months_2014>0,1,0)

#stats for the matrix
matrix_n_new <- table(base_new1$group_12,base_new1$group_14)
write.table(matrix_n_new,'clipboard-128',sep='\t',row.names=T)


#total stats for calculations of waterfalls and such
matrix_sum <- base_new1 %>% group_by(exclude,book) %>% summarise_each(funs(sum(.,na.rm=T)),c(fees_2012:waivers_2014,with_2012:with_2014))
write.table(matrix_sum,'clipboard-128',sep='\t',row.names=F)

matrix_sum1 <- base_new1 %>% group_by(group_12) %>% summarise(N=sum(present_2012),amt=sum(fees_2012,na.rm=T),wvr=sum(waivers_2012,na.rm=t))
write.table(matrix_sum1,'clipboard-128',sep='\t',row.names=F)


matrix_sum2 <- base_new1 %>% group_by(group_12,group_14) %>% summarise(N=sum(present_2012),amt12=sum(fees_2012,na.rm=T),wvr12=sum(waivers_2012,na.rm=t),amt14=sum(fees_2014,na.rm=T),wvr14=sum(waivers_2014,na.rm=t))
write.table(matrix_sum2,'clipboard-128',sep='\t',row.names=F)


save.image('nsf_image_20150312.rdata')
