#load data
load('text_analysis_datasets.rdata')  #top word flags per complaints, ngrams of 3,4 and 5 words
load('complaints_matched_full.rdata')  #complaints data set matched to datamart, with some HHLD and acct details
load("Z:/M&T Projects/OCA/accts_201412_oca.rdata")   #acct base in 201412, with ome details


#load libraries
library(wordcloud)
library(dplyr)
library(tidyr)

#Run only once, no needto run again
##########################################
#the levels were wrong, an issue with importing ,
#mathc new ones
      library(stringr)
      levels1 =read.table('CCTS_PROD_0414_1231 ID and 5 Levels.txt',sep='\t',quote='"',header = T,stringsAsFactors = F)
      names(levels1) <- tolower(gsub('\\.','_',names(levels1)))
      levels1 = levels1[-7]
      levels1[,2] <- str_trim(levels1[,2])
      levels1[,3] <- str_trim(levels1[,3])
      levels1[,4] <- str_trim(levels1[,4])
      levels1[,5] <- str_trim(levels1[,5])
      levels1[,6] <- str_trim(levels1[,6])
      names(levels1)[-1] = paste0(names(levels1)[-1],'_clean')
      levels1[,2] <- factor(levels1[,2])
      levels1[,3] <- factor(levels1[,3])
      levels1[,4] <- factor(levels1[,4])
      levels1[,5] <- factor(levels1[,5])
      levels1[,6] <- factor(levels1[,6])
      complaints_matched_full <- left_join(complaints_matched_full,levels1)
      complaints_matched_full = complaints_matched_full[-c(22:26)]
      #save it 
      save(complaints_matched_full,file='complaints_matched_full.rdata')
##########################################

##########################################
#add new comments and other fields
#one time only
comments1 <- read.table('CCTS_PROD_0414_1231_Discrimination_and%09Comments.txt',sep='\t',quote='"',header=T,fill=T)
names(comments1) <- tolower(make.names(names(comments1)))
names(comments1) <- gsub("\\.+","_",names(comments1))
names(comments1) <- gsub("_$","",names(comments1))
complaints_matched_full_new <- complaints_matched_full[-c(22:26)]
complaints_matched_full_new <- left_join(complaints_matched_full_new,comments1[-7])



save(complaints_matched_full_new,file='complaints_matched_full_new.rdata')
##########################################

#rename the CCS that are cards as CRD
levels(accts_201412_oca$ACCT_PTYPE) <- c(levels(accts_201412_oca$ACCT_PTYPE),'CRD')  #add level to factor
accts_201412_oca$ACCT_PTYPE[accts_201412_oca$ACCT_STYPE %in% c('REW','NOR','SIG')] <- 'CRD'

#First let's got to level_1 and develop the counts of complaints by category
level1_counts <- complaints_matched_full %>% group_by(level_1) %>% summarise(N=n())
write.table(level1_counts,'clipboard-128',sep='\t',row.names=F)
acct_base <- accts_201412_oca %>% group_by(ACCT_PTYPE) %>% summarise(N=n())
write.table(acct_base,'clipboard-128',sep='\t',row.names=F)


#Lets do level_2 for all 
level2_all <- complaints_matched_full %>% group_by(level_1,level_2) %>% summarise(N=n())
write.table(level2_all,'clipboard-128',sep='\t',row.names=F)


#lets do level_3 for checking
level3_dda <- complaints_matched_full %>% filter(level_2=='Checking account') %>% group_by(level_2,level_3) %>% summarise(N=n())
write.table(level3_dda,'clipboard-128',sep='\t',row.names=F)

#lets do level_4 for checking
level4_dda <- complaints_matched_full %>% filter(level_2=='Checking account') %>% group_by(level_2,level_3,level_4) %>% summarise(N=n())
write.table(level4_dda,'clipboard-128',sep='\t',row.names=F)


#now let's do the wordclouds, for the drill down levels we care about
#the data is on files ngrams3, ngrams4, ngrams5, I think this is better than single words
#the colum doc in those refers to the row number on compaints_matched_full
#one way to do a word cliur for a subset of complaint is to take the docs that match
#the result of which(condition1 & condition 2 ...)

which_docs = which(complaints_matched_full$level_4=='Overdraft/Insufficient funds fees')
aux2 <- ngrams2 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux3 <- ngrams3 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux4 <- ngrams4 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux5 <- ngrams5 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq)) 

#for the wordsm we need to process the data more
word_data$doc = 1:dim(word_data)[1]
words = word_data %>% select(-c(level_1:level_3)) %>% gather(word,freq,abl:wrong) %>% filter(freq>0) 
#words have counts of words by document

aux_words = words %>% filter(doc %in% which_docs) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))


#lets doe teh wordclouds
aux_words = aux_words %>% filter(!(word %in% c('cust','account') ))
png('wordcloud_nsf1.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux_words$word,aux_words$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=100,scale=c(2,0.25),rot.per=0)
dev.off()


#cust relating to customer and account add little to this, they refer to the person in question, so I filteretd them out
png('wordcloud_nsf2.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux2$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=6,scale=c(1.5,0.25),rot.per=0)
dev.off()

png('wordcloud_nsf3.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux3$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=6,scale=c(1.2,0.25),rot.per=0)
dev.off()

png('wordcloud_nsf4.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux4$ngram,aux4$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=3,scale=c(1.2,0.25),rot.per=0)
dev.off()

wordcloud(aux5$ngram,aux5$freq,color=brewer.pal(9,"Purples")[4:9],random.order=F,min.freq=2,scale=c(2,0.5))

#I manually tweaked parameters until it looked nice




#now lets do the charts for atm
which_docs = which(complaints_matched_full$level_4==' ATM')
aux2 <- ngrams2 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux3 <- ngrams3 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux4 <- ngrams4 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux5 <- ngrams5 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq)) 
aux_words = words %>% filter(doc %in% which_docs) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux_words = aux_words %>% filter(!(word %in% c('cust','account') ))

png('wordcloud_atm1.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux_words$word,aux_words$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=90,scale=c(2.5,0.25),rot.per=0)
dev.off()


#cust relating to customer and account add little to this, they refer to the person in question, so I filteretd them out
png('wordcloud_atm2.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux2$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=6,scale=c(1.5,0.25),rot.per=0)
dev.off()

png('wordcloud_atm3.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux3$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=6,scale=c(1.4,0.25),rot.per=0)
dev.off()

png('wordcloud_atm4.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux4$ngram,aux4$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=3,scale=c(1.2,0.25),rot.per=0)
dev.off()
