#The questionw as if waivers seemt oimpact attrition

#fI will compare people present in ja-feb-mar 2014 and measure their attrition
#by march 2015
#I will split the ones with NSF in jan/feb and for waives I will  use 1Q 2014 to allow for some rxtra time

load("Z:/M&T Projects/NSF/base.rdata")
load("Z:/M&T Projects/NSF/closed_accts.rdata")
load('Z:/M&T Projects/OCA/dda_201503.rdata')
load('Z:/M&T Projects/NSF/contrib.rdata')

library(dplyr)
library(tidyr)

base$ACCT_DATE_OPENED_FOR_PRIME <- as.Date(base$ACCT_DATE_OPENED_FOR_PRIME,'%m/%d/%Y')
base <- base %>% filter(ACCT_DATE_OPENED_FOR_PRIME < '2014-03-01')
aux <- filter(closed_accts,period <= '201403')

base <- filter(base, !(EXPRESSION_8 %in% aux$EXPRESSION_8))
sum(duplicated(base$EXPRESSION_8))

base$lost = 1
base$lost[base$EXPRESSION_8 %in% dda_201503$EXPRESSION_8]  <- 0
prop.table(table(base$lost)) #9.97% attrition over 1 year, sounds logical and consistent

contrib_1q14 <- filter(contrib,period %in% c('201401','201402','201403'))
nsf <- contrib_1q14 %>% filter(ACCT_CONTR_TOTAL_NSF_FEES >0 & period %in% c('201401','201402') )
length(unique(nsf$EXPRESSION_8))
waived <- contrib_1q14 %>% filter(ACCT_CONTR_TOTAL_NSF_FEES_WAIVED >0 & period %in% c('201401','201402','201403') )
length(unique(waived$EXPRESSION_8))

length(intersect(nsf$EXPRESSION_8,waived$EXPRESSION_8)) #like 15% fot a waiver

base$nsf <- 0
base$nsf[base$EXPRESSION_8 %in% nsf$EXPRESSION_8] <- 1
base$waived <- 0
base$waived[base$EXPRESSION_8 %in% waived$EXPRESSION_8] <- 1

attr <- base %>% group_by(nsf,waived) %>% summarise(N=n(),nsf1=sum(nsf),waived1=sum(waived),lost1=sum(lost),attrition=mean(lost))
prop.test(c(23901,4237),c(129161,21411),alternative = 'l')
write.table(attr,'clipboard',sep='\t',row.names=F)

save(dda2,file='dda_20150424.rdata')
