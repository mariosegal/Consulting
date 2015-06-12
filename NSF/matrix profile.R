load("Z:/M&T Projects/NSF/models.rdata")
load('Z:/M&T Projects/NSF/cqi_201403.rdata')
rm(list=ls()[!(ls() %in%  c('model_data','cqi_201304'))])

library(dplyr)
library(tidyr)
model_data <- left_join(model_data,cqi_201304)
rm(cqi_201304)


drop = c('ACCT_STYPE','stype1','book','group_14','date','closed','closed14',"present_9m_2013",'present_9m_2014',
         'with_2013','with_2014','with_9m_2013a','with_9m_2014a','flag_stopped_nsf','ACCT_ID','EXPRESSION_8',
         'exclude','closed13','closed12','ACCT_DATE_OPENED_FOR_PRIME')

profile1 <- model_data %>% filter(book=='back') %>% 
  select(c(which(sapply(model_data,is.factor)),which(sapply(model_data,is.character)))) %>%
  select(-one_of(drop)) %>% gather(measure,level,-group_13) %>% group_by(group_13,measure,level) %>%
  summarise(N=n()) %>% group_by(group_13,measure) %>% mutate(P=N/sum(N)) %>% gather(aux,value,N:P) %>%
  unite(key,aux,group_13) %>% spread(key,value,fill=0)

keep = intersect(which(!sapply(model_data,is.factor)),which(!sapply(model_data,is.character)))
profile2 <- model_data %>% filter(book=='back') %>% select(c(group_13,keep)) %>% select(-one_of(drop)) %>% 
  gather(measure,aux,-group_13) %>% group_by(group_13,measure) %>% summarise(value=mean(aux,na.rm=T)) %>%
  ungroup() %>% mutate(group_13=paste0('N_',group_13)) %>% spread(group_13,value) %>% mutate(level='mean')
  
profile3 <- model_data %>% filter(book=='back') %>% select(group_13)  %>% 
   group_by(group_13) %>% summarise(N=n()) %>% ungroup() %>% mutate(group_13=paste0('N_',group_13)) %>% 
  group_by(group_13) %>% mutate(measure="Accts",level='N') %>% spread(group_13,N) 

profile <- bind_rows(profile3,profile1,profile2)
write.table(profile,'clipboard-128',sep='\t',row.names=F)


tenure <- model_data %>% filter(book=='back') %>% select(group_13,hh_tenure) %>%  
  group_by(group_13,tenure=cut(hh_tenure,c(0,.0001,1,2,3,4,5,10,15,Inf),dig.labs=10)) %>%
  summarise(N=n()) %>% group_by(group_13) %>% mutate(P=N/sum(N)) %>% gather(aux,value,N:P) %>%
  unite(key,aux,group_13) %>% spread(key,value,fill=0)

write.table(tenure,'clipboard-128',sep='\t',row.names=F)

dda_tenure <- model_data %>% filter(book=='back') %>% select(group_13,dda_tenure) %>%  
  group_by(group_13,tenure=cut(dda_tenure,c(0,.0001,1,2,3,4,5,10,15,Inf),dig.labs=10)) %>%
  summarise(N=n()) %>% group_by(group_13) %>% mutate(P=N/sum(N)) %>% gather(aux,value,N:P) %>%
  unite(key,aux,group_13) %>% spread(key,value,fill=0)


load("Z:/M&T Projects/NSF/con201304.rdata")
model_data <- left_join(model_data,con201304)


cont_aux <- con201304 %>% filter(!(ACCT_PTYPE %in% c('DEB','WEB','HBK','WEB','TRS',"CLN"))) %>% 
  group_by(ACCT_ID) %>% summarise(con=sum(ACCT_CONTR_NET_CONTRIBUTION_MTD))

model_data <- left_join(model_data,cont_aux)

contr <- model_data %>% filter(book=='back') %>% select(c(group_13,con)) %>% 
  group_by(group_13) %>% summarise(value=mean(con,na.rm=T)) %>%
  mutate(group_13=paste0('N_',group_13)) %>% mutate(level='mean') %>% spread(group_13,value) 

write.table(contr,'clipboard-128',sep='\t',row.names=F)
