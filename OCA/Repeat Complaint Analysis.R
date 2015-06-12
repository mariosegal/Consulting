library(dplyr)
library(tidyr)
library(ggplot2)

load('Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')

#I need to attach the new data that steve sent me with clean , some code for levels1 is onComplaint Analysis

complaints_2014_2015Q1_matched_both <- left_join(complaints_2014_2015Q1_matched_both,levels1)
complaints_2014_2015Q1_matched_both$level_3 = as.character(complaints_2014_2015Q1_matched_both$level_3)
complaints_2014_2015Q1_matched_both$level_4 = as.character(complaints_2014_2015Q1_matched_both$level_4)
complaints_2014_2015Q1_matched_both$level_5 = as.character(complaints_2014_2015Q1_matched_both$level_5)

complaints_2014_2015Q1_matched_both$level_1_clean = as.character(complaints_2014_2015Q1_matched_both$level_1_clean)
complaints_2014_2015Q1_matched_both$level_2_clean = as.character(complaints_2014_2015Q1_matched_both$level_2_clean)
complaints_2014_2015Q1_matched_both$level_3_clean = as.character(complaints_2014_2015Q1_matched_both$level_3_clean)
complaints_2014_2015Q1_matched_both$level_4_clean = as.character(complaints_2014_2015Q1_matched_both$level_4_clean)
complaints_2014_2015Q1_matched_both$level_5_clean = as.character(complaints_2014_2015Q1_matched_both$level_5_clean)


complaints_2014_2015Q1_matched_both$level_1[!is.na(complaints_2014_2015Q1_matched_both$level_1_clean)] = complaints_2014_2015Q1_matched_both$level_1_clean[!is.na(complaints_2014_2015Q1_matched_both$level_1_clean)]
complaints_2014_2015Q1_matched_both$level_2[!is.na(complaints_2014_2015Q1_matched_both$level_2_clean)] = complaints_2014_2015Q1_matched_both$level_2_clean[!is.na(complaints_2014_2015Q1_matched_both$level_2_clean)]
complaints_2014_2015Q1_matched_both$level_3[!is.na(complaints_2014_2015Q1_matched_both$level_3_clean)] = complaints_2014_2015Q1_matched_both$level_3_clean[!is.na(complaints_2014_2015Q1_matched_both$level_3_clean)]
complaints_2014_2015Q1_matched_both$level_4[!is.na(complaints_2014_2015Q1_matched_both$level_4_clean)] = complaints_2014_2015Q1_matched_both$level_4_clean[!is.na(complaints_2014_2015Q1_matched_both$level_4_clean)]
complaints_2014_2015Q1_matched_both$level_5[!is.na(complaints_2014_2015Q1_matched_both$level_5_clean)] = complaints_2014_2015Q1_matched_both$level_5_clean[!is.na(complaints_2014_2015Q1_matched_both$level_5_clean)]




#Deloitte did a shity job in my estimation
#they worked on an idealiaed hierarchy, whihc doe snot match the data I received
#this is the difference betweenworking with data and workign with specs
complaints_2014_2015Q1_matched_both$level_1 = factor(tolower(complaints_2014_2015Q1_matched_both$level_1))
levels(complaints_2014_2015Q1_matched_both$level_1) = gsub(".","",levels(complaints_2014_2015Q1_matched_both$level_1),fixed=T)

complaints_2014_2015Q1_matched_both$level_2 = factor(tolower(complaints_2014_2015Q1_matched_both$level_2))
levels(complaints_2014_2015Q1_matched_both$level_2) = gsub(".","",levels(complaints_2014_2015Q1_matched_both$level_2),fixed=T)
levels(complaints_2014_2015Q1_matched_both$level_2) = gsub('\"',"",levels(complaints_2014_2015Q1_matched_both$level_2),fixed=T)


complaints_2014_2015Q1_matched_both$level_3 = factor(tolower(complaints_2014_2015Q1_matched_both$level_3))
levels(complaints_2014_2015Q1_matched_both$level_3) = gsub('\"',"",levels(complaints_2014_2015Q1_matched_both$level_3),fixed=T)
levels(complaints_2014_2015Q1_matched_both$level_3)[51] = levels(complaints_2014_2015Q1_matched_both$level_3)[53]
levels(complaints_2014_2015Q1_matched_both$level_3)[52] = levels(complaints_2014_2015Q1_matched_both$level_3)[53]
levels(complaints_2014_2015Q1_matched_both$level_3)[64] = levels(complaints_2014_2015Q1_matched_both$level_3)[64]
levels(complaints_2014_2015Q1_matched_both$level_3)[38] = levels(complaints_2014_2015Q1_matched_both$level_3)[40]

complaints_2014_2015Q1_matched_both$level_4 = factor(tolower(complaints_2014_2015Q1_matched_both$level_4))
levels(complaints_2014_2015Q1_matched_both$level_4) = gsub(".","",levels(complaints_2014_2015Q1_matched_both$level_4),fixed=T)
levels(complaints_2014_2015Q1_matched_both$level_4) = gsub('\"',"",levels(complaints_2014_2015Q1_matched_both$level_4),fixed=T)


complaints_2014_2015Q1_matched_both$level_5 = factor(tolower(complaints_2014_2015Q1_matched_both$level_5))
levels(complaints_2014_2015Q1_matched_both$level_5) = gsub(".","",levels(complaints_2014_2015Q1_matched_both$level_5),fixed=T)

complaints_2014_2015Q1_matched_both = complaints_2014_2015Q1_matched_both[1:70]


#add comments, aome code on complaint analysis
names(comments1)[-1] = paste0(names(comments1)[-1],"_clean")
comments1$discriminated_clean = as.character(comments1$discriminated_clean)
comments1$unfair_clean = as.character(comments1$unfair_clean)
comments1$broke_the_law_clean = as.character(comments1$broke_the_law_clean)
comments1$dishonest_clean = as.character(comments1$dishonest_clean)
comments1$comment_clean = as.character(comments1$comment_clean)

comments1$discriminated_clean[comments1$discriminated_clean==""] = NA
comments1$unfair_clean[comments1$unfair_clean==""] = NA
comments1$broke_the_law_clean[comments1$broke_the_law_clean==""] = NA
comments1$dishonest_clean[comments1$dishonest_clean==""] = NA
comments1$comment_clean[comments1$comment_clean==""] = NA


complaints_2014_2015Q1_matched_both <- left_join(complaints_2014_2015Q1_matched_both,comments1[-7],by='complaint_master_id')

complaints_2014_2015Q1_matched_both$discriminated[!is.na(complaints_2014_2015Q1_matched_both$discriminated_clean)] = complaints_2014_2015Q1_matched_both$discriminated_clean[!is.na(complaints_2014_2015Q1_matched_both$discriminated_clean)]

complaints_2014_2015Q1_matched_both$dishonest[!is.na(complaints_2014_2015Q1_matched_both$dishonest_clean)] = complaints_2014_2015Q1_matched_both$dishonest_clean[!is.na(complaints_2014_2015Q1_matched_both$dishonest_clean)]


complaints_2014_2015Q1_matched_both$broke_the_law[!is.na(complaints_2014_2015Q1_matched_both$broke_the_law_clean)] = complaints_2014_2015Q1_matched_both$broke_the_law_clean[!is.na(complaints_2014_2015Q1_matched_both$broke_the_law_clean)]

complaints_2014_2015Q1_matched_both$unfair[!is.na(complaints_2014_2015Q1_matched_both$unfair_clean)] = complaints_2014_2015Q1_matched_both$unfair_clean[!is.na(complaints_2014_2015Q1_matched_both$unfair_clean)]

complaints_2014_2015Q1_matched_both$comment[!is.na(complaints_2014_2015Q1_matched_both$comment_clean)] = complaints_2014_2015Q1_matched_both$comment_clean[!is.na(complaints_2014_2015Q1_matched_both$comment_clean)]

complaints_2014_2015Q1_matched_both = complaints_2014_2015Q1_matched_both[1:70]
save(complaints_2014_2015Q1_matched_both,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')

#create hierrachy for mapping
levels= complaints_2014_2015Q1_matched_both %>% group_by(level_1,level_2,level_3,level_4,level_5) %>% summarise(N=n())
write.csv(levels,'Z:/M&T Projects/OCA/hierarchy ccts 20150515',row.names=F)

escalatedh = complaints_2014_2015Q1_matched_both %>% group_by(category.x,area,subarea) %>% summarise(N=n())
write.table(escalatedh,'clipboard-128',sep='\t',row.names=F)


complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master)) %>% group_by(type,account_master) %>% summarise(N=n()) %>% group_by(type,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P))



#########
### Analyze the repeat for escalated only

repeat1 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') %>% group_by(type,account_master) %>% summarise(N=n()) %>% group_by(type,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P))
write.table(repeat1,'clipboard-128',sep='\t',row.names=F)


analysis_base = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') 
analysis_base$level_3_orig = analysis_base$level_3
levels(analysis_base$level_2)[levels(analysis_base$level_2) %in% c('home equity loan','home equity loan or heloc')] = 'heloc'
levels(analysis_base$level_2)[levels(analysis_base$level_2) %in% c('conventional arm','conventional fixed','fha')] = 'mortgage'
save(analysis_base,file='Z:/M&T Projects/OCA/analysis_base.rdata')

repeat2 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') %>% group_by(account_master,level_1) %>% summarise(N=n()) %>% group_by(level_1,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P),tot=sum(N1)) %>% arrange(level_1,desc(tot))
write.table(repeat2,'clipboard-128',sep='\t',row.names=F)


repeat3 = complaints_2014_2015Q1_matched_both %>% filter(!is.na(account_master) & type == 'non escalated') %>% group_by(account_master,level_1,level_2) %>% summarise(N=n()) %>% group_by(level_1,level_2,N) %>% summarise(N1=n()) %>% mutate(P=N1/sum(N1),cum1=cumsum(P),tot=sum(N1)) %>% arrange(level_1,desc(tot))
write.table(repeat3,'clipboard-128',sep='\t',row.names=F)



repeat4 = analysis_base %>% group_by(account_master,level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(level_1,level_2,level_3,complaints) %>% summarise(lines=n(),accts=n_distinct(account_master)) %>% mutate(P=accts/sum(accts),cum1=cumsum(P),tot=sum(accts))  
write.table(repeat4,'clipboard-128',sep='\t',row.names=F)

repeat4a = analysis_base %>% group_by(account_master,level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(level_1,level_2,level_3,complaints) %>% summarise(lines=n(),accts=n_distinct(account_master)) %>% mutate(P=accts/sum(accts),cum1=cumsum(P),tot=sum(accts))  %>% filter(complaints==1) %>% arrange(level_1,P)
write.table(repeat4a,'clipboard-128',sep='\t',row.names=F)
