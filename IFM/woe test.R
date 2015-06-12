data <- data.frame(response= rbinom(200,1,p=0.5),a=rnorm(200),b=rnorm(200),c=rnorm(200))
data$b <- data$b  +rnorm(20,0.5,.5)*data$response

data$a <- quantcut(data$a,q=seq(0,1,by=0.1))
data$b <- quantcut(data$b,q=seq(0,1,by=0.1))
data$c <- quantcut(data$c,q=seq(0,1,by=0.1))

data$response <- as.factor(data$response)

woe1 <- woe(response~.,data=data)

woe1
plot(woe1,type='IV')

plot(woe1,type='woes')

#manual calc

library(dplyr)
library(reshape2)

data1 <- melt(data[c(53,81)],id.vars=c('target'))
data2 <- data1[!is.na(data1$value),]
res1 <- data2 %>% group_by(variable,value,target) %>% summarise(N=n()) %>% group_by(variable,target) %>% mutate(P=N/sum(N)) 
res2 <- dcast(res1,variable+value~target,value.var='N',fill=0.5)
names(res2)[-c(1:2)] <- paste0('n_',names(res2)[-c(1,2)])
res2 <- res2 %>%  group_by(variable) %>%mutate(p_0=n_0/sum(n_0),p_1=n_1/sum(n_1))
#names(res2)[-c(1:2)] <- paste0('p_',names(res2)[-c(1,2)])
res2$woe = log(res2$p_1/res2$p_0)
res2$IV = (res2$p_1-res2$p_0)*res2$woe

res2 %>% group_by(variable) %>% summarise(IV=sum(IV)) %>% arrange(desc(IV))


my_IV(data[c(31,81)])
iv