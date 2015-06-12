
nsf_accts$year <- substr(nsf_accts$period,1,4)

library(dplyr)

nsf_accts %>% group_by(year) %>% summarise(N=sum(ACCT_NSF_TOTAL),amt=sum(ACCT_CONTR_TOTAL_NSF_FEES))


#average is not 38.5, this is a worry

nsf_accts$implied <- nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES/38.5

sum(nsf_accts$implied==nsf_accts$ACCT_NSF_TOTAL)
sum(nsf_accts$implied>nsf_accts$ACCT_NSF_TOTAL)
sum(nsf_accts$implied<nsf_accts$ACCT_NSF_TOTAL)

nsf_accts[which(nsf_accts$implied<nsf_accts$ACCT_NSF_TOTAL)[1:10],]


table(nsf_accts$period[nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES==37.5])   #201301 is the last month with 37.5

table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201301'])

(as.numeric(names(table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201301'])))/38.5) %% 1

weird <- which((as.numeric(names(table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201301'])))/38.5) %% 1!=0)

table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201301'])[weird][order(-table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201301'])[weird])]


weird1 <- which((as.numeric(names(table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201305'])))/38.5) %% 1!=0)

table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201305'])[weird1][order(-table(nsf_accts$ACCT_CONTR_TOTAL_NSF_FEES[nsf_accts$period=='201305'])[weird1])]


check <- nsf_accts %>% group_by(period) %>% summarise(N=n(),consistent=sum(ACCT_NSF_TOTAL==implied),higher=sum(ACCT_NSF_TOTAL<implied)) %>% mutate(consistent1=consistent/N,higher1=higher/N)

#January 2013 is weird, as it was the transition - for other months It looks better in terms of the total NSF Fees, the count is not correct

#implied event sis higher in 1/3 of cases

x <- which(nsf_accts$implied > nsf_accts$ACCT_NSF_TOTAL  & nsf_accts$period != '201301')
nsf_accts[x[1:10],]

cumsum(prop.table(table(nsf_accts$implied[x] - nsf_accts$ACCT_NSF_TOTAL[x])[order(-table(nsf_accts$implied[x] - nsf_accts$ACCT_NSF_TOTAL[x]))]))


#99% are 1 or 2

#look at some of accts
subset(nsf_accts,EXPRESSION_8==26155883)
subset(nsf_accts,EXPRESSION_8==11766670)
subset(nsf_accts,EXPRESSION_8==17638004)


