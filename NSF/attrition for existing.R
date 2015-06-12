
attr_13 <-existing_book[c(3,5,21,38)]
attr_13$with <- ifelse(attr_13$tot_num_2013 >=1, 'Y','N')


#like for new, create 12 rows for each, then only keep the ones when open

attr_13a <- attr_13 %>% mutate(p1=1,p2=2,p3=3,p4=4,p5=5,p6=6,p7=7,p8=8,p9=9,p10=10,p11=11,p12=12) %>% 
  gather (aux,value,p1:p12) %>% mutate(period=as.character(2013*100+value)) %>% 
  filter(is.na(closed_period) | period <= closed_period) %>% group_by(EXPRESSION_8) %>% mutate(events=mean(tot_num_2013,na.rm=T))


curves1_ex <- attr_13a %>% group_by(value,with) %>% summarise(N=n()) %>% spread(with,N)
write.table(curves1_ex,'clipboard-128',sep='\t',row.names=F)

attr_13a$events1 <- cut(attr_13a$events,c(0,0.01,1,2,3,4,5,10,20,Inf),include.lowest = T)
curves3_ex <- attr_13a %>% group_by(events1,value) %>% summarise(N=n())  %>% spread(events1,N)
write.table(curves3_ex,'clipboard-128',sep='\t',row.names=F)


#################

attr_14 <-existing14[c(3,5,30,1)]
names(attr_14)[2] <- 'closed_period'
attr_14$with <- ifelse(attr_14$tot_num_2014 >=1, 'Y','N')


#like for new, create 12 rows for each, then only keep the ones when open

attr_14a <- attr_14 %>% mutate(p1=1,p2=2,p3=3,p4=4,p5=5,p6=6,p7=7,p8=8,p9=9,p10=10,p11=11,p12=12) %>% 
  gather (aux,value,p1:p12) %>% mutate(period=as.character(2014*100+value)) %>% 
  filter(is.na(closed_period) | period <= closed_period) %>% group_by(EXPRESSION_8) %>% mutate(events=mean(tot_num_2014,na.rm=T))


curves2_ex <- attr_14a %>% group_by(value,with) %>% summarise(N=n()) %>% spread(with,N)
write.table(curves2_ex,'clipboard-128',sep='\t',row.names=F)


attr_14a$events1 <- cut(attr_14a$events,c(0,0.01,1,2,3,4,5,10,20,Inf),include.lowest = T)
curves4_ex <- attr_14a %>% group_by(events1,value) %>% summarise(N=n())  %>% spread(events1,N)
write.table(curves4_ex,'clipboard-128',sep='\t',row.names=F)
