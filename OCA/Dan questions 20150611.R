repeats = repeated1$account_master[repeated1$group != 'unique']

length(unique(repeats))

aux = table(table(repeats))
aux


repeated1 %>% group_by(account_master,level_1,level_2, level_3) %>% summarise(N=n()) %>% filter(N>1) %>% group_by(N) %>% summarise(N1=n())


 combo = analysis_base %>% group_by(account_master, level_1,level_2,level_3) %>% summarise(complaints=n()) %>% group_by(complaints) %>% summarise(N=n())
write.table(combo,'clipboard',sep='\t',row.names=F)

acct1 = analysis_base %>% group_by(account_master, level_1,level_2,level_3) %>% summarise(complaints=n()) %>% filter(complaints>=2) %>%  group_by(account_master) %>% summarise(N=n()) %>% group_by(N) %>% summarise(n())
write.table(acct1,'clipboard',sep='\t',row.names=F)
