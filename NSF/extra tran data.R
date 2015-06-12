# add more data to model_+data_new

library(dplyr)
library(tidyr)


load("Z:/M&T Projects/NSF/bpay.rdata")


#crucnh the bpay by 6m, 5m, ...

bpay_summary <- bpay %>% 
  group_by(EXPRESSION_8,period) %>%
  summarise_each(funs(sum)) 
bpay_summary <- bpay_summary %>% gather(measure,value,-c(EXPRESSION_8,period)) %>% arrange(EXPRESSION_8,measure,desc(period))

bpay_summary$measure <- gsub('ACCT_WEB_','',bpay_summary$measure)
bpay_summary1 <- bpay_summary %>% group_by(EXPRESSION_8,measure) %>% 
  mutate(value_cum=cumsum(value),name=paste0(measure,'_last',as.numeric(substring(period,5)),'m'))

save(bpay_summary1,file='bpay_sumamry1.rdata')
 
bpay_summary$measure <- gsub('ACCT_WEB_','',bpay_summary$measure)
bpay_summary1 <-bpay_summary$measure <- gsub('ACCT_WEB_','',bpay_summary$measure)

#if I am running out of memory, fiter out all the zeros, they are most of it ayway
bpay_summaryx <- subset(bpay_summary1,value != 0)
rm(bpay_summary1)
gc()
bpay_summary2 <- bpay_summaryx %>% ungroup() %>%   select(EXPRESSION_8,value_cum,name) 
bpay_summary2 <- bpay_summary2 %>% spread(name,value_cum,fill=0)

rm(bpay_summaryx)

#crunch the depsiits and debits gor last 6m, 5m ...
load("Z:/M&T Projects/NSF/tran_extra.rdata")
tran1 <- tran_extra %>% gather(measure,value,-c(EXPRESSION_1,period)) %>% arrange(EXPRESSION_1,measure,desc(period))
rm(tran_extra)
tran2 <- tran1 %>% group_by(EXPRESSION_1,measure) %>% 
  mutate(value_cum=cumsum(value),name=paste0(measure,'_last',as.numeric(substring(period,5)),'m'))
tran3 <- tran2 %>%  ungroup() %>% select(EXPRESSION_1,value_cum,name) %>% spread(name,value_cum,fill=0)
rm(tran2,tran1,tran_extra)
names(tran3)[1] <- 'EXPRESSION_8'



#do the sums odf feps and with

act1 <- active_new_20141h %>% gather(measure,value,-c(EXPRESSION_8,period)) %>% arrange(EXPRESSION_8,measure,desc(period))
act2 <- act1 %>% group_by(EXPRESSION_8,measure) %>% 
  mutate(value_cum=cumsum(value),name=paste0(measure,'_last',as.numeric(substring(period,5)),'m'))
act3 <- act2 %>%  ungroup() %>% select(EXPRESSION_8,value_cum,name) %>% spread(name,value_cum,fill=0)

rm(active_new_20141h,act1,act2)

load("Z:/M&T Projects/NSF/model_data_new1.rdata")
model_data_new2 <- left_join(model_data_new,act3)
model_data_new2 <- left_join(model_data_new2,bpay_summary2)
model_data_new2 <- left_join(model_data_new2,tran3)

which(sapply(model_data_new2[116:205],function(x) sum(is.na(x))>0))
model_data_new2[116:205][is.na(model_data_new2[116:205])] <- 0

save(model_data_new2,file='model_data_new2.rdata')
