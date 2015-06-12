
load("Z:/M&T Projects/NSF/hhld_201312.rdata")

clv <- hhld_201312 %>% select(c(contains("CLV"),HHLD_ID))

dda2 <- left_join(dda2,clv,by=c('ACCT_ID'='HHLD_ID'))

dda2 %>% group_by(nsf_fee) %>% ggplot(aes(x=nsf_fee,y=HHLD_CLV_REMAINING,fill=nsf_fee)) +
  geom_boxplot()+coord_cartesian(ylim=c(-1000,5000))+geom_jitter(alpha=0.1,color='white')

boxplot_data <- dda2 %>% group_by(nsf_fee) %>% 
  summarise_each(funs(bottom=quantile(.,c(0.25),na.rm=T),mid=quantile(.,c(0.5),na.rm=T),
            top=quantile(.,c(0.75),na.rm=T),avg=mean(.,na.rm=T)),
            vars=c(HHLD_CLV_REMAINING_TENURE,HHLD_CLV_REMAINING,HHLD_CLV_TOTAL)) %>% 
  gather(var,value,-nsf_fee) %>% separate(var,c("variable","measure")) %>% ungroup() %>%
  mutate(variable=ifelse(variable=='vars1','tenure',variable)) %>% 
  mutate(variable=ifelse(variable=='vars2','remaining',variable)) %>% 
  mutate(variable=ifelse(variable=='vars3','total',variable)) %>% spread(nsf_fee,value)
  

write.table(boxplot_data,'clipboard-128',sep='\t',row.names=F)

save.image('image_20150323.rdata')
