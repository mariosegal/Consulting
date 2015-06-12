#I want to try the text analytics
library(dplyr)
library(tidyr)
library(wordcloud)

load("Z:/M&T Projects/OCA/text_analysis_datasets.rdata")
load("Z:/M&T Projects/OCA/complaints_top.rdata")

grp = 2
docs = complaints_top$complaint_master_id[complaints_top$group_new==grp]

ngrams4 %>% filter(doc %in% docs) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% ungroup() %>% arrange(desc(freq))

aux = ngrams4 %>% filter(doc %in% docs) %>% group_by(ngram) %>% summarise(freq=sum(freq)) %>% ungroup() %>% arrange(desc(freq)) %>% filter(!is.na(freq))

wordcloud(aux$ngram,aux$freq)
View(subset(complaints_top,group_new==1 & ACCT_AMT_BAL_FOR_PRIME<=0))


mywordcloud = function(docs) {
  require(wordcloud)
  require(RColorBrewer)
  
  color=brewer.pal(8,"Dark2")
}