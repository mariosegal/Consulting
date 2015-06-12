
#The comment was that I was normalizing by accts, not events, 
#I think they mean they want a rate not per accoint but per event
#my only concern is the fact we cant match the waiver to the fee, but that was 
#also the case before

#lets try that

load('z:/M&T Projects/OCA/dda_20150424.rdata')

my_test <- function(a,b,c,d) prop.test(c(a,b),c(c,d),alternative='g')$p.value 
my_test1 <- function(a,b,c,d) prop.test(c(a,b),c(c,d),alternative='l')$p.value 

age_event <- dda2 %>% group_by(age1,nsf_fee) %>% summarise(events=sum(ACCT_NSF_TOTAL,na.rm=T),N=n()) %>% 
  gather(variable,value,events:N) %>% mutate(nsf_fee=gsub(" ","_",nsf_fee)) %>% 
  filter(nsf_fee != 'Drop') %>% unite(key,variable,nsf_fee) %>% spread(key,value,fill=0) %>% 
  mutate(N=select(.,contains('N_')) %>% rowSums,events=select(.,contains('events_')) %>% rowSums) %>% ungroup() %>%
  mutate(compl_rate_acct = N_Yes/(N_Yes+N_No_with_Fee),compl_rate_event=N_Yes/(events_Yes+events_No_with_Fee)) %>%
  mutate(a=N_Yes,b=sum(N_Yes),c=N_Yes+N_No_with_Fee,d=sum(N_Yes)+sum(N_No_with_Fee),a1=N_Yes,b1=sum(N_Yes),c1=events_Yes+events_No_with_Fee,d1=sum(events_Yes)+sum(events_No_with_Fee)) %>%
  group_by(age1) %>% mutate(p_N=my_test(a,b,c,d),p_events=my_test(a1,b1,c1,d1)) %>% select(-c(a:d1))

write.table(age_event,'clipboard',sep='\t',row.names=F)

dda2$segment[is.na(dda2$segment)] <- 'NC'
segment_event <- dda2 %>% group_by(segment,nsf_fee) %>% summarise(events=sum(ACCT_NSF_TOTAL,na.rm=T),N=n()) %>% 
  gather(variable,value,events:N) %>% mutate(nsf_fee=gsub(" ","_",nsf_fee)) %>% 
  filter(nsf_fee != 'Drop') %>% unite(key,variable,nsf_fee) %>% spread(key,value,fill=0) %>% 
  mutate(N=select(.,contains('N_')) %>% rowSums,events=select(.,contains('events_')) %>% rowSums) %>% ungroup() %>%
  mutate(compl_rate_acct = N_Yes/(N_Yes+N_No_with_Fee),compl_rate_event=N_Yes/(events_Yes+events_No_with_Fee)) %>%
  mutate(a=N_Yes,b=sum(N_Yes),c=N_Yes+N_No_with_Fee,d=sum(N_Yes)+sum(N_No_with_Fee),a1=N_Yes,b1=sum(N_Yes),c1=events_Yes+events_No_with_Fee,d1=sum(events_Yes)+sum(events_No_with_Fee)) %>%
  group_by(segment) %>% mutate(p_N=my_test(a,b,c,d),p_events=my_test(a1,b1,c1,d1)) %>% select(-c(a:d1))

write.table(segment_event,'clipboard',sep='\t',row.names=F)


tenure_event <- dda2 %>% group_by(tenure,nsf_fee) %>% summarise(events=sum(ACCT_NSF_TOTAL,na.rm=T),N=n()) %>% 
  gather(variable,value,events:N) %>% mutate(nsf_fee=gsub(" ","_",nsf_fee)) %>% 
  filter(nsf_fee != 'Drop') %>% unite(key,variable,nsf_fee) %>% spread(key,value,fill=0) %>% 
  mutate(N=select(.,contains('N_')) %>% rowSums,events=select(.,contains('events_')) %>% rowSums) %>% ungroup() %>%
  mutate(compl_rate_acct = N_Yes/(N_Yes+N_No_with_Fee),compl_rate_event=N_Yes/(events_Yes+events_No_with_Fee)) %>%
  mutate(a=N_Yes,b=sum(N_Yes),c=N_Yes+N_No_with_Fee,d=sum(N_Yes)+sum(N_No_with_Fee),a1=N_Yes,b1=sum(N_Yes),c1=events_Yes+events_No_with_Fee,d1=sum(events_Yes)+sum(events_No_with_Fee)) %>%
  group_by(tenure) %>% mutate(p_N=my_test(a,b,c,d),p_events=my_test(a1,b1,c1,d1)) %>% select(-c(a:d1))

write.table(tenure_event,'clipboard',sep='\t',row.names=F)


stype_events <- dda2 %>% group_by(ACCT_STYPE,nsf_fee) %>% summarise(events=sum(ACCT_NSF_TOTAL,na.rm=T),N=n()) %>% 
  gather(variable,value,events:N) %>% mutate(nsf_fee=gsub(" ","_",nsf_fee)) %>% 
  filter(nsf_fee != 'Drop') %>% unite(key,variable,nsf_fee) %>% spread(key,value,fill=0) %>% 
  mutate(N=select(.,contains('N_')) %>% rowSums,events=select(.,contains('events_')) %>% rowSums) %>% ungroup() %>%
  mutate(compl_rate_acct = N_Yes/(N_Yes+N_No_with_Fee),compl_rate_event=N_Yes/(events_Yes+events_No_with_Fee)) %>%
  mutate(compl_rate_acct=ifelse(is.na(compl_rate_acct) | is.infinite(compl_rate_acct),0,compl_rate_acct),
         compl_rate_event=ifelse(is.na(compl_rate_event) | is.infinite(compl_rate_event),0,compl_rate_event)) %>%
  mutate(a=N_Yes,b=sum(N_Yes),c=N_Yes+N_No_with_Fee,d=sum(N_Yes)+sum(N_No_with_Fee),a1=N_Yes,b1=sum(N_Yes),c1=events_Yes+events_No_with_Fee,d1=sum(events_Yes)+sum(events_No_with_Fee)) %>%
  group_by(ACCT_STYPE) %>% filter(a > 0 &  c>0 & a1>0 & c1>0) %>% 
  mutate(p_N=my_test(a,b,c,d),p_events=my_test(a1,b1,c1,d1)) %>% select(-c(a:d1))

write.table(stype_events,'clipboard',sep='\t',row.names=F)

prop.test(c(312,6205),c(30487,359953),alternative = 'g')$p.value 
prop.test(c(312,6205),c(175671,2761638),alternative = 'g')$p.value 
