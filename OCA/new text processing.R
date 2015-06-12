library(tm)
library(wordcloud)
library(dplyr)
library(tidyr)

comments = tolower(complaints_matched_full_new1$comment)
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

save(corpus1,file='corpus1_20150504.rdata')
dtm <- DocumentTermMatrix(corpus1)
#dtm
dtm1 <- removeSparseTerms(dtm,0.995)
dtm1


#NGRAMS
library(tau)
a <- sapply(1:length(corpus1),function(x) corpus1[[x]]$content)
a1 = data.frame(id=complaints_matched_full_new1$complaint_master_id,text=a,stringsAsFactors = F)

b0 <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=2,method='string'))
c0 <- lapply(1:length(b0), function(y) data.frame(ngram=names(b0[[y]]),freq=as.numeric(b0[[y]]),stringsAsFactors = F))
for (i in 1:length(c0)) {
  if (dim(c0[[i]])[1]==0 ) {  c0[[i]][1,1] = NA}
  c0[[i]]$doc = i
}
ngrams2 <- do.call('rbind',c0)
ngrams2 %>% group_by(ngram) %>% summarise(N=n()) %>% arrange(desc(N)) %>% slice(1:20)
save(ngrams2,file='Z:/M&T Projects/OCA/ngrams2_20150504.rdata')

b <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=3,method='string'))
c <- lapply(1:length(b), function(y) data.frame(ngram=names(b[[y]]),freq=as.numeric(b[[y]]),stringsAsFactors = F))
for (i in 1:length(c)) {
  if (dim(c[[i]])[1]==0 ) {  c[[i]][1,1] = NA}
  c[[i]]$doc = i
}
ngrams3 <- do.call('rbind',c)
save(ngrams3,file='Z:/M&T Projects/OCA/ngrams3_20150504.rdata')

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
save(ngrams4,file='Z:/M&T Projects/OCA/ngrams4_20150504.rdata')

b2 <- sapply(1:length(a1$text),function(x) textcnt(a1$text[x],n=5,method='string'))
c2 <- lapply(1:length(b2), function(y) data.frame(ngram=names(b2[[y]]),freq=as.numeric(b2[[y]]),stringsAsFactors = F))
for (i in 1:length(c2)) {
  if (dim(c2[[i]])[1]==0 ) {  c2[[i]][1,1] = NA}
  c2[[i]]$doc = i
}
ngrams5 <- do.call('rbind',c2)
save(ngrams5,file='Z:/M&T Projects/OCA/ngrams5_20150504.rdata')

save(ngrams2,ngrams3,ngrams4,ngrams5,word_data,file='Z:/M&T Projects/OCA/text_analysis_datasets_20150504.rdata')
