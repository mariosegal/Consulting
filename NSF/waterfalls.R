#waterfall charts


wf1 <- data.frame(label=c('Total Accts Analyzed',"Front Book",'Non NSF in 2013 (9m)','Closed by Dec 2014','Inactive in 2Q2013','Inactive in 4Q2014','Accounts For Modeling'),value=c(1950670,-391944,-1269810,-52911,-11507,-12382,212116),order=1:7)

wf1$balance=cumsum(c(0,wf1$value[1:6]))
wf1$balance[7]=0
wf1$sign <- (sign(wf1$value))
wf1$sign[1] <- 0
wf1$sign <- as.factor(wf1$sign)
wf1

library(scales)
plot1 <- ggplot(wf1)+geom_rect(aes(xmin=order-0.45,xmax=order+0.45,ymin=balance,ymax=balance+value,fill=sign))+
  geom_text(aes(x=order,y=pmax(balance,balance+value)+50,label=comma(abs(value)),hjust=0.5,vjust=-1))+
  scale_y_continuous("",breaks=NULL)+theme_bw()+scale_x_continuous("",breaks=wf1$order,labels=wf1$label)+
  theme(legend.position='none')+ggtitle("Waterfall for Less/More Model")

ggsave('wf1.pdf',width = 11,height=8)

wf2 <- data.frame(label=c('Total Accts Analyzed',"Front Book",'Non NSF in 2013 (9m)','Inactive in 2Q2013','Accounts For Modeling'),value=c(1950670,-391944,-1269810,-19153,269763),order=1:5)

wf2$balance=cumsum(c(0,wf2$value[1:4]))
wf2$balance[5]=0
wf2$sign <- (sign(wf2$value))
wf2$sign[1] <- 0
wf2$sign <- as.factor(wf2$sign)
wf2


plot2 <- ggplot(wf2)+geom_rect(aes(xmin=order-0.45,xmax=order+0.45,ymin=balance,ymax=balance+value,fill=sign))+
  geom_text(aes(x=order,y=pmax(balance,balance+value)+50,label=comma(abs(value)),hjust=0.5,vjust=-1))+
  scale_y_continuous("",breaks=NULL)+theme_bw()+scale_x_continuous("",breaks=wf1$order,labels=wf1$label)+
  theme(legend.position='none')+ggtitle("Waterfall for Lost/Inactive Model")
ggsave('wf2.pdf',width = 11,height=8)
