#do the baisc stats ofr the existing 2014 portfolio

existing14 <- subset(base,open_date <= '2013-01-01' & open_date >= '1950-01-01')
closed_2012a <- subset(closed_accts,period >= '201201' & period <= '201312')

existing14 <- subset(existing14,!(EXPRESSION_8 %in% closed_2012a$EXPRESSION_8))
length(intersect(existing14$EXPRESSION_8,closed_accts$EXPRESSION_8))/dim(existing14)[1]  #implies 7.9% attrtion for the book during 2014, this is reasonable to 

existing14 <- left_join(existing14,closed_accts)
names(existing_book)[5] <- "closed_period"

existing14 <- subset(existing14,!(EXPRESSION_8 %in% weird))  

#merge with the nsf stats
existing14 <-left_join(existing14,nsf_summary,by='EXPRESSION_8')


#make 2014 stats = 0 if NA
existing14 <- existing14[c(1:5,seq(6,32,by=3),seq(7,32,by=3),seq(8,32,by=3))]  #reorder to deal with 2014 easier

existing14[,24:32][is.na(existing14[,24:32])] <- 0


summary14 <- existing14  %>% mutate(N=1,with=ifelse(tot_num_2014>=1,1,0)) %>% summarise_each(funs(sum),c(fees_2014,tot_num_2014,waived_2014,N,with))
write.table(summary14,'clipboard-128',sep='\t',row.names=F)


#####
 dist14 <- prop.table(table(cut(existing14$tot_num_2014,c(0,.01,1,2,3,4,5,10,20,Inf),include.lowest=T)))
write.table(dist14,'clipboard-128',sep='\t',row.names=F)

dist13 <- prop.table(table(cut(existing_book$tot_num_2013,c(0,.01,1,2,3,4,5,10,20,Inf),include.lowest=T)))
write.table(dist13,'clipboard-128',sep='\t',row.names=F)
