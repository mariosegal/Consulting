load('complaints.rdata')

library(tm)
library(wordcloud)
library(dplyr)
library(tidyr)

comments = tolower(complaints_matched_full_new$comment)
#I want to standardize most of the references to M&T as mtb, so that they hopefully remain
comments <- gsub("m&t","mtb",comments)  #any M&T to mtb
comments <- gsub("m+t","mtb",comments,fixed=T) #m+t to mtb
comments <- gsub(" mt "," mtb ",comments)
comments <- gsub("mtb bank","mtb",comments) #we do not need the bank after mtb anymore

#cust shows up al ot, as an abbreviation of cust, i want to standardize customer to cust, this is because it gets stemmed as custom, and I like cust better
#it also mixes custom rom add-ons or cxard with cusotmer
comments <- gsub("customer","cust",comments)  

comments <- gsub("acct","account",comments)  #use account
comments <- gsub("accts","accounts",comments)  #use account

corpus <- Corpus(VectorSource(comments))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation)
#extra <- c("customer",'cust','m&t','acct','account','upset')
#c('customer',"cust","customers","customer's","m t bank","m&t bank","mtb",stopwords('english'))
corpus <- tm_map(corpus,removeWords,stopwords('english'))
corpus <-tm_map(corpus,stripWhitespace)
corpus1 <- tm_map(corpus,stemDocument)

save(corpus1,file='corpus1.rdata')
dtm <- DocumentTermMatrix(corpus1)
#dtm
dtm1 <- removeSparseTerms(dtm,0.995)
dtm1

data <- as.data.frame(as.matrix(dtm1))
row.names(data) <- NULL
data$complaint_master_id <- complaints_matched_full_new$complaint_master_id
data$level_1_clean = complaints_matched_full_new$level_1_clean
data$level_2_clean = complaints_matched_full_new$level_2_clean
data$level_3_clean = complaints_matched_full_new$level_3_clean

extra <- c("customer",'cust','mtb','account','upset','bank','branch')
word_data <- data
words <- data %>% group_by(complaint_master_id) %>% gather(word,freq,-c(complaint_master_id:level_3_clean)) %>% group_by(word) %>% summarise(freq=sum(freq))
words1 <- words %>% filter(! (word %in% extra))
wordcloud(words$word,words$freq,color=brewer.pal(9,"Blues")[4:9],random.order=F)
wordcloud(words1$word,words1$freq,color=brewer.pal(9,"Blues")[4:9],random.order=F)

words2 <- data %>% filter(level_2=='Checking account' ) %>% group_by(id) %>% gather(word,freq,-c(id:level_3)) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% filter( ! (word %in% extra))
png('checking_cloud.png',height = 3,width = 3,units = 'in',res=600)
wordcloud(words2$word,words2$freq,color=brewer.pal(9,"Blues")[4:9],random.order=F)
dev.off()

words3 <- data %>% filter(level_1=='Mortgage or Home Equity' ) %>% group_by(id) %>% gather(word,freq,-c(id:level_3)) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% filter( ! (word %in% extra))
png('mtg_cloud.png',height = 3,width = 3,units = 'in',res=600)
wordcloud(words3$word,words3$freq,color=brewer.pal(9,"Reds")[4:9],random.order=F)
dev.off()

words4 <- data %>% filter(level_1=='Consumer Loan' ) %>% group_by(id) %>% gather(word,freq,-c(id:level_3)) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% filter( ! (word %in% extra))
png('loan.png',height = 3,width = 3,units = 'in',res=600)
wordcloud(words4$word,words4$freq,color=brewer.pal(9,"Greens")[4:9],random.order=F)
dev.off()


pdf("wordcloud example.pdf")
dev.off()
getwd()


words5 <- data %>% filter(level_3=='Deposits or withdrawals' ) %>% group_by(id) %>% gather(word,freq,-c(id:level_3)) %>% group_by(word) %>% summarise(freq=sum(freq)) %>% filter( ! (word %in% c(extra,'card')))
wordcloud(words5$word,words5$freq,color=brewer.pal(9,"Greens")[4:9],random.order=F,max.words=50)

#I want to try the ngrams
library(tau)
a <- sapply(1:length(corpus1),function(x) corpus1[[x]]$content)
a1 = data.frame(id=complaints_matched_full_new$complaint_master_id,text=a,stringsAsFactors = F)

b0 <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=2,method='string'))
c0 <- lapply(1:length(b0), function(y) data.frame(ngram=names(b0[[y]]),freq=as.numeric(b0[[y]]),stringsAsFactors = F))
for (i in 1:length(c0)) {
  if (dim(c0[[i]])[1]==0 ) {  c0[[i]][1,1] = NA}
  c0[[i]]$doc = i
}
ngrams2 <- do.call('rbind',c0)
ngrams2 %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% slice(1:20)


b <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=3,method='string'))
c <- lapply(1:length(b), function(y) data.frame(ngram=names(b[[y]]),freq=as.numeric(b[[y]]),stringsAsFactors = F))
for (i in 1:length(c)) {
  if (dim(c[[i]])[1]==0 ) {  c[[i]][1,1] = NA}
  c[[i]]$doc = i
}
ngrams3 <- do.call('rbind',c)
ngrams3 %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% slice(1:20)

e <- d %>% filter(!is.na(ngram) & !(ngram %in% c("m t bank"))) %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% filter(N>=5)
wordcloud(e$ngram,e$N,color=brewer.pal(9,"Greens")[4:9],random.order=F,max.words=50)

b1 <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=4,method='string'))
c1 <- lapply(1:length(b1), function(y) data.frame(ngram=names(b1[[y]]),freq=as.numeric(b1[[y]]),stringsAsFactors = F))
for (i in 1:length(c1)) {
  if (dim(c1[[i]])[1]==0 ) {  c1[[i]][1,1] = NA}
  c1[[i]]$doc = i
}
ngrams4 <- do.call('rbind',c1)

b2 <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=5,method='string'))
c2 <- lapply(1:length(b2), function(y) data.frame(ngram=names(b2[[y]]),freq=as.numeric(b2[[y]]),stringsAsFactors = F))
for (i in 1:length(c2)) {
  if (dim(c2[[i]])[1]==0 ) {  c2[[i]][1,1] = NA}
  c2[[i]]$doc = i
}
ngrams5 <- do.call('rbind',c2)

save(ngrams2,ngrams3,ngrams4,ngrams5,word_data,file='text_analysis_datasets.rdata')


e1 <- d1 %>% filter(!is.na(ngram) & !(ngram %in% c("m t bank"))) %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% filter(N>=5)
wordcloud(e1$ngram,e1$N,color=brewer.pal(9,"Blues")[4:9],random.order=F,max.words=50)

aux_docs <- data.frame(id=complaints$complaint_master_id,doc=1:63057)
d1 <- left_join(d1,aux_docs,by='doc')
d1 <- left_join(d1,complaints[c(1,22:26)],by=c('id'='complaint_master_id'))


e2 <- d1 %>% filter(!is.na(ngram) & !(ngram %in% c("m t bank")) & level_2=='Checking account') %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% filter(N>=5)
wordcloud(e2$ngram,e2$N,color=brewer.pal(9,"Blues")[4:9],random.order=F,max.words=50,scale=c(3,1))

# quick analysis of complaints by category
complaints %>% filter(level_1 == 'Account maintenance') %>% group_by(level_2) %>% summarise(N=n())
load("Z:/M&T Projects/OCA/accts_201412_oca.rdata")
accts_201412_oca %>% 
  filter(is.na(ACCT_STATUS_FOR_PRIME)  & (ACCT_SBU_GROUP=='CON' | (ACCT_PTYPE=='DDA' & substr(ACCT_STYPE,1,1)=='R'))) %>%
  group_by(ACCT_PTYPE) %>% summarise(N=n())