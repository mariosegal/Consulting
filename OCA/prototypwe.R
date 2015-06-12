
#top category is level 5 = 'Low balance' for checking
load("Z:/M&T Projects/OCA/complaints_matched_full_new.rdata")

data <- filter(complaints_matched_full_new,level_2_clean=='Checking account' & level_5_clean== 'Low balance')

age <- data %>% group_by(age=cut(HHLD_HH_OWN_AGE,c(0,18,25,35,45,55,65,75,Inf),dig.lab = 8,ordered_result = T)) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N))


cbr <- data %>% group_by(cbr=HHLD_COMMUNITY_BANK_MARKET) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N))

packages <- c('Retail Classic Checking','Retail M&T Classic Checking with Interest','Retail Pay As You Go','Retail Student Checking','Retail @College Checking','Retail Worry Free Checking','Retail Worry Free (Dir Dep) Checking','Retail EZChoice Checking','Retail MyChoice Checking','Retail Free Checking','Retail Interest Checking (First)','Retail Interest Checking','Retail Premium Checking','Retail Select Checking with Interest','Retail MyChoice Plus Checking w/Int','Retail MyChoice Premium Checking','Retail Power Checking with Interest','Retail Brokerage Checking','Retail PMA','Retail First Checking','Retail Relationship Checking','Retail First Checking with Interest','Retail Alliance Checking','Retail Relationship Checking with Interest','Retail Select Checking','Retail MyChoice Plus Checking','Retail Direct Checking','Retail M&T At Work Checking','Retail Direct Deposit Checking','Retail Basic Checking','HSA')
packages <- gsub('Retail ','',packages)
packages <- gsub('Checking','',packages)
packages <- gsub(' +',' ',packages)
packages <- gsub(' $','',packages)
packages <- gsub('with Interest','w/Int',packages)
stypes <- c('RA2','RA8','RB2','RC2','RC6','RD2','RE2','RE5','RE6','RE7','RF2','RG2','RG6','RH2','RH3','RH5','RH6','RI1','RI2','RJ2','RJ7','RK2','RK6','RK7','RW2','RW3','RX2','RX7','RX6','RZ2','HSA')
data$stype <- factor(data$aCCT_STYPE ,levels=stypes,labels=packages)

stype <- data %>% group_by(stype) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N)) %>% arrange(desc(N))

write.table(stype,'clipboard',sep='\t',row.names=F)
write.table(cbr,'clipboard',sep='\t',row.names=F)
write.table(age,'clipboard',sep='\t',row.names=F)

#I need to get a baseline for age, cbr and segment
load("Z:/M&T Projects/OCA/hhlds_201412.rdata")
load("Z:/M&T Projects/OCA/accts_201412_oca.rdata") 
base <- left_join(accts_201412_oca,hhlds_201412,by=c('ACCT_ID'='HHLD_ID'))
dda <- subset(base,ACCT_PTYPE=='DDA')

base_age <- dda %>% group_by(age=cut(HHLD_HH_OWN_AGE,c(0,18,25,35,45,55,65,75,Inf),dig.lab = 8,ordered_result = T)) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N))
write.table(base_age,'clipboard',sep='\t',row.names=F)

base_cbr <- dda %>% group_by(HHLD_COMMUNITY_BANK_MARKET) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N))
write.table(base_cbr,'clipboard',sep='\t',row.names=F)

dda$stype = factor(dda$ACCT_STYPE ,levels=stypes,labels=packages)
base_stype <- dda %>% group_by(stype) %>% 
  summarise(N=n()) %>% mutate(P=N/sum(N))
write.table(base_stype,'clipboard',sep='\t',row.names=F)


#ONLY 4 have coments
#########
which_docs = which(complaints_matched_full_new$level_2_clean=='Checking account' & complaints_matched_full_new$level_5_clean=='Low balance')
aux2 <- ngrams2 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux3 <- ngrams3 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux4 <- ngrams4 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))
aux5 <- ngrams5 %>% filter(doc %in% which_docs & !is.na(ngram)) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq)) 

#for the wordsm we need to process the data more
word_data$doc = 1:dim(word_data)[1]
words = word_data %>% select(-c(level_1_clean:level_3_clean)) %>% gather(word,freq,abl:wrong) %>% filter(freq>0) 
aux_words = words %>% filter(doc %in% which_docs) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% arrange(desc(freq))


aux_words = aux_words %>% filter(!(word %in% c('cust','account') ))
png('wordcloud_nsf1.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux_words$word,aux_words$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=100,scale=c(2,0.25),rot.per=0)
dev.off()
#only 4 have any comments

#cust relating to customer and account add little to this, they refer to the person in question, so I filteretd them out
png('wordcloud_nsf2.png',width = 4,height=2.8, units='in', res=600)
wordcloud(aux2$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=6,scale=c(1.5,0.25),rot.per=0)
dev.off()

png('wordcloud_nsf3.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux3$ngram,aux3$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=1,scale=c(1.2,0.25),rot.per=0)
dev.off()

png('wordcloud_nsf4.png',width = 4, height = 2.8, units='in',res=600)
wordcloud(aux4$ngram,aux4$freq,color=brewer.pal(8,"Dark2"),random.order=F,min.freq=1,scale=c(1.2,0.25),rot.per=0)
dev.off()

wordcloud(aux5$ngram,aux5$freq,color=brewer.pal(9,"Purples")[4:9],random.order=F,min.freq=2,scale=c(2,0.5))
