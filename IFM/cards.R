
setClass("currency")
setAs("character","currency", function(from)as.numeric(gsub('\\$','',from)))

class1 <- c('character','integer','integer','currency','')
cards <- read.csv('ifmCCProfile_Masked.csv',stringsAsFactors=F)
save(cards,file='cards.rdata')

issuers <- gather(cards[c('Acct_Masked','Major01Company','Major02Company','Major03Company','Major05Company','Major04Company')],variable,name,-Acct_Masked)

issuers$name <- str_trim(issuers$name )
issuers %>% filter(name != '') %>% group_by(name) %>% summarise(N=n()) %>% mutate(P=N/sum(N)) %>% arrange(desc(N)) %>% slice(1:15)


sum(penet2q$DDA==1 & penet2q$CRD==1)/sum(penet2q$DDA==1)



store <- gather(cards[c('Acct_Masked','Store01Company','Store02Company','Store03Company','Store05Company','Store04Company')],variable,name,-Acct_Masked)

store$name <- str_trim(store$name )
store %>% filter(name != '') %>% group_by(name) %>% summarise(N=n()) %>% mutate(P=N/sum(N)) %>% arrange(desc(N)) %>% slice(1:25)

