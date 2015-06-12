#############################################################
### We want to analyze why per Phil question
### is it more money in accts, is it less spencing
### so forth
#############################################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)


#nedd the balances - this is too much data I may eed to use data.table, let's see
load('nsf_aux.rdata')
load("Z:/M&T Projects/NSF/contrib.rdata")

length(unique(nsf$EXPRESSION_8)) #2424723
length(intersect(nsf$EXPRESSION_8,contrib$EXPRESSION_8))  #2424723, all are on contrib at least once
length(setdiff(nsf$EXPRESSION_8,contrib$EXPRESSION_8))   #0
length(setdiff(contrib$EXPRESSION_8,nsf$EXPRESSION_8))  #30,563, very few so who cares


nsf <- left_join(nsf,contrib[c(1,8,9)])

nsf$period = factor(nsf$period)
nsf$book = factor(nsf$book)
nsf$aux = factor(nsf$aux)
nsf$ACCT_STYPE = factor(nsf$ACCT_STYPE)
nsf$closed_period = factor(nsf$closed_period)
nsf$ACCT_STYPE = factor(nsf$ACCT_STYPE)


load('nsf_with_bal.rdata')


### Hypothesis #1 , are balances increasing?
#the nsf set does not have balances, and I will need that

#are balances increasing or what
bals = nsf %>% group_by(period) %>% summarise(balance=mean(ACCT_CONTR_BALANCE,na.rm=T)) 
  
ggplot(bals,aes(x=period,y=balance,group=1))+geom_line()+coord_cartesian(ylim=c(0,10000))

nsf$nsf = ifelse(nsf$ACCT_NSF_TOTAL>0,1,0)
bals1 = nsf %>% group_by(period,book,nsf) %>% summarise(balance=mean(ACCT_CONTR_BALANCE,na.rm=T),N=n()) 
bals1 %>% filter(book %in% c('back','front')) %>% ggplot(aes(x=period,y=balance,color=book,group=book))+geom_line()+facet_grid(nsf~.,scales='free_y')+theme(axis.text.x=element_text(angle=90))+stat_smooth(method='lm',se=F)

bals2 =nsf %>% filter(book %in% c('back','front'))%>% group_by(period,book,ACCT_NSF_TOTAL) %>% summarise(balance=mean(ACCT_CONTR_BALANCE,na.rm=T),N=n())


#I think that the balance for that month is a bit messedup becauyse of the NSFs
# I want to try to calculate teh average of the past 3 months, but I can only do it then for acct of 3 m or more
nsf = nsf %>% arrange(EXPRESSION_8,period)
 #calculate a rolling3 months avg. balance 
library(zoo)
#nsf = nsf %>% group_by(EXPRESSION_8) %>% mutate(bal_past_3m=ifelse(length(ACCT_CONTR_BALANCE)>3,rollapply(data=ACCT_CONTR_BALANCE,width=rep(list(-(3:1)),length(ACCT_CONTR_BALANCE)),FUN=(mean),fill=NA,partial=F,align='right'),rep(NA,length(ACCT_CONTR_BALANCE))))


nsf$bal_past_3m = ave(nsf$ACCT_CONTR_BALANCE,nsf$EXPRESSION_8,FUN=function(x) ifelse(length(x)>3,rollapply(data=x,width=rep(list(-(3:1)),length(x)),FUN=(mean),fill=NA,partial=F,align='right'),rep(NA,length(x))))

#this is not workin, i am going to use data.table, but I only need it a data table with the balances and the expression_8
library(data.table)
tmp = data.table(nsf[c('EXPRESSION_8','ACCT_CONTR_BALANCE'),by=EXPRESSION_8])
setkey(tmp,EXPRESSION_8)

getmv = function(x) {
  if(length(x)<4) return(rep(NA,length(x)))
  rollapply(data=x,width=rep(list(-(3:1)),length(x)),FUN=(mean),align='right',partial=F,fill=NA)
}
tmp[,bal_past_3m:= as.numeric(getmv(ACCT_CONTR_BALANCE)),by=EXPRESSION_8]

#merge it back to nsf
nsf = cbind(nsf,tmp$bal_past_3m)
names(nsf)[17] = 'bal_past_3m'
save(nsf,file='nsf_with_bal.rdata')
rm(tmp)

#lets see if the balance for the past 3 m varied by book and such

bals1a = nsf %>% group_by(period,book,nsf) %>% summarise(balance=mean(bal_past_3m,na.rm=T),N=n()) 
bals1a %>% filter(book %in% c('back','front')) %>% ggplot(aes(x=period,y=balance,color=book,group=book))+geom_line()+facet_grid(nsf~.,scales='free_y')+theme(axis.text.x=element_text(angle=90))+stat_smooth(method='lm',se=F)


#finally I want to show that 1) if NSf is related to avg balance and , show if the balance has been increasing or what
nsf$nsf_grp = cut(nsf$ACCT_NSF_TOTAL,c(-Inf,0,1,2,3,4,5,10,15,Inf),labels=c('None','1','2','3','4','5','6 to 10','11 to 15','16+'),ordered=T)

tmp = data.table(nsf[c('nsf_grp','bal_past_3m')])
bal_grp = nsf  %>% group_by(nsf_grp) %>% summarise(bal=mean(bal_past_3m,na.rm=T)) 

ggplot(bal_grp,aes(x=nsf_grp,y=bal,fill=nsf_grp))+geom_bar(stat='identity',alpha=0.5)


bal_grp1 = nsf %>%  filter(book %in% c('back','front')) %>% group_by(nsf_grp,period,book) %>% summarise(bal=mean(bal_past_3m,na.rm=T))
my_labs = rep("",36)
my_labs[c(1,13,25,36)] = levels(bal_grp1$period)[c(1,13,25,36)]
bal_grp1 %>%  ggplot(aes(x=period,y=bal,color=nsf_grp,group=nsf_grp))+geom_line()+stat_smooth(method='lm',se=F,linetype=2,color='black')+facet_wrap(~nsf_grp)+theme_bw()+theme(legend.position='none',axis.text.x=element_text(angle=90),title=element_text(size=16,face='bold'))+scale_x_discrete("",labels=my_labs)+scale_y_continuous("Average Balance 3 Months Prior to NSF",labels=dollar)+ggtitle('Average Balance Trends - By Number of NSF Events in Period')+coord_cartesian(ylim=c(0,6500))
ggsave('balance trend by NSF events.png',width = 9,height = 5.5,units='in')

#use grid to customize the scalefor each
for (grp in 1:length(levels(bal_grp1$nsf_grp))) {
    ymax= (ceiling(max(bal_grp1$bal[bal_grp1$nsf_grp==levels(bal_grp1$nsf_grp)[grp]],na.rm=T)/1000)+1)*1000
    aux = bal_grp1 %>% filter(nsf_grp == levels(bal_grp1$nsf_grp)[grp] & book=='back') %>%  
      ggplot(aes(x=period,y=bal,group=nsf_grp))+
      geom_line(color=grp+1,size=1)+stat_smooth(method='lm',se=F,size=0.5,linetype=2,color='black')+theme_bw()+
      theme(legend.position='none',axis.text.x=element_text(angle=0,size=4),title=element_text(size=6,face='bold'),panel.grid.major=element_blank(),axis.text.y=element_text(size=4),plot.margin=unit(c(0,0,0,0),'cm'),axis.ticks=element_blank())+
      ggtitle(paste("NSF Events =",levels(bal_grp1$nsf_grp)[grp]))+
      coord_cartesian(ylim=c(0,ymax))+scale_y_continuous("",labels=dollar)+
      scale_x_discrete("",labels=my_labs)
    assign(paste0('panel_',grp,'_back'),aux)
}

panel_chart_back = arrangeGrob(panel_1_back,panel_2_back,panel_3_back,panel_4_back,panel_5_back,panel_6_back,panel_7_back,panel_8_back,panel_9_back,ncol=3)
ggsave('balance trend by NSF events back book.png',plot=panel_chart_back,width = 9,height = 5.5,units='in')


for (grp in 1:length(levels(bal_grp1$nsf_grp))) {
  ymax= (ceiling(max(bal_grp1$bal[bal_grp1$nsf_grp==levels(bal_grp1$nsf_grp)[grp]],na.rm=T)/1000)+1)*1000
  aux = bal_grp1 %>% filter(nsf_grp == levels(bal_grp1$nsf_grp)[grp] & book=='front') %>%  
    ggplot(aes(x=period,y=bal,group=nsf_grp))+
    geom_line(color=grp+1,size=1)+stat_smooth(method='lm',se=F,size=0.5,linetype=2,color='black')+theme_bw()+
    theme(legend.position='none',axis.text.x=element_text(angle=0,size=4),title=element_text(size=6,face='bold'),panel.grid.major=element_blank(),axis.text.y=element_text(size=4),plot.margin=unit(c(0,0,0,0),'cm'),axis.ticks=element_blank())+
    ggtitle(paste("NSF Events =",levels(bal_grp1$nsf_grp)[grp]))+
    coord_cartesian(ylim=c(0,ymax))+scale_y_continuous("",labels=dollar)+
    scale_x_discrete("",labels=my_labs)
  assign(paste0('panel_',grp,'_front'),aux)
}

panel_chart_front = arrangeGrob(panel_1_front,panel_2_front,panel_3_front,panel_4_front,panel_5_front,panel_6_front,panel_7_front,panel_8_front,panel_9_front,ncol=3)
ggsave('balance trend by NSF events front book.png',plot=panel_chart_front,width = 9,height = 5.5,units='in')


write.table(bal_grp1,'clipboard',sep='\t',row.names=F)
write.table(bal_grp,'clipboard',sep='\t',row.names=F)


#it is possible that the NSf acct sare related to less, to account for that, i am going to look at acct sthat were on 3 years and went from yes to no, no to yes, ... and compare those balances

#mtb colors
lime = rgb(122,184,0,maxColorValue = 256)
green = rgb(0,120,86,maxColorValue = 256)
yellow = rgb(255,179,0,maxColorValue = 256)
purple = rgb(134,73,157,maxColorValue = 256)
green2 = rgb(195,231,111,maxColorValue = 256)
gray = rgb(128,128,128,maxColorValue = 256)
teal = rgb(35,164,145,maxColorValue = 256)

nsf$year = factor(substr(nsf$period,1,4))

nsf_aux = nsf %>% group_by(EXPRESSION_8,year,book) %>% summarise(nsf=sum(ACCT_NSF_TOTAL),bal=mean(ACCT_CONTR_BALANCE,na.rm=T),N=n(),events=sum(ACCT_NSF_TOTAL>0))
nsf_aux = nsf_aux %>% gather(measure,value,nsf:events) %>% unite(key,measure,year) %>% spread(key,value)
nsf_aux$group = NA
nsf_aux$group = ifelse(nsf_aux$book=='back' & nsf_aux$N_2013==12 & nsf_aux$N_2014==12 & nsf_aux$nsf_2013>0 & nsf_aux$nsf_2014 ==0,'yes_no',nsf_aux$group)
nsf_aux$group = ifelse(nsf_aux$book=='back' & nsf_aux$N_2013==12 & nsf_aux$N_2014==12 & nsf_aux$nsf_2013>0 & nsf_aux$nsf_2014 >0,'yes_yes',nsf_aux$group)
nsf_aux$group = ifelse(nsf_aux$book=='back' & nsf_aux$N_2013==12 & nsf_aux$N_2014==12 & nsf_aux$nsf_2013==0 & nsf_aux$nsf_2014 ==0,'no_no',nsf_aux$group)
nsf_aux$group = ifelse(nsf_aux$book=='back' & nsf_aux$N_2013==12 & nsf_aux$N_2014==12 & nsf_aux$nsf_2013==0 & nsf_aux$nsf_2014 >0,'no_yes',nsf_aux$group)

 nsf_aux %>% filter(!is.na(group)) %>% group_by(book,group) %>% gather(var,value,bal_2012:bal_2014) %>% ggplot(aes(x=group,y=value,fill=var))+geom_boxplot()+coord_cartesian(ylim=c(0,10000))+stat_summary(fun.y=mean,geom='point',position=position_dodge(0.75),shape=18,size=4,color="red",show_guide=F)+theme_bw()+scale_y_continuous('Average Balance',labels=dollar)+theme(panel.grid.major=element_blank(),legend.position="bottom")+scale_x_discrete("NSF Incidence 2013 & 2014",labels=c('Neither Period','2014 Only','2013 Only','Both Periods'))+scale_fill_manual('Year',labels=c('2012','2013',"2014"),values=c(lime,yellow,green))
ggsave('balance trend by NSF events 2.png',width = 9,height = 5.5,units='in')
#rm(nsf_aux)



#lets look the reverse, what is the incidence and average for those who increased or decreased
nsf_aux$delta = (nsf_aux$bal_2014/nsf_aux$bal_2013)-1
nsf_aux$delta1 = cut(nsf_aux$delta,c(-Inf,-1,-0.75,-0.5,-0.25,-0.1,0,.1,.25,.5,.75,1,Inf),dig.lab =10)

bal1 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12) %>% group_by(delta1) %>% summarise(inc_2013=mean(nsf_2013>0),inc_2014=mean(nsf_2014>0),N=n(),with2013=sum(nsf_2013>0),with2014=sum(nsf_2014>0),events13=sum(events_2013),events14=sum(events_2014)) %>% group_by(delta1) %>% mutate(test_g=prop.test(c(with2013,with2014),c(N,N),alternative='g')$p.value,test_l=prop.test(c(with2013,with2014),c(N,N),alternative='l')$p.value)
write.table(bal1,'clipboard-128',sep='\t',row.names=F)

#do the tests for average events for those who have them
bal2 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12) %>% group_by(delta1) %>%
  summarise(t2013=mean(events_2013[events_2013>0]),t2014=mean(events_2014[events_2014>0]),
            test_g = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='g')$p.value,
            test_l = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='l')$p.value)
write.table(bal2,'clipboard-128',sep='\t',row.names=F)


# find the debit data
load("Z:/M&T Projects/NSF/debit_all.rdata")

#aggregate it
debit_aux = debit_all %>% group_by(dda,year=substr(period,1,4)) %>% summarise_each(funs(sum(.,na.rm=T)),num:amt)
debit_aux = debit_aux %>% gather(var,value,num:amt) %>% unite(key,var,year) %>% spread(key,value,fill=0)

#merge it
nsf_aux = left_join(nsf_aux,debit_aux,by=c('EXPRESSION_8'='dda'))
nsf_aux[18:21][is.na(nsf_aux[18:21])] = 0

nsf_aux %>% filter(!is.na(group)) %>% group_by(book,group) %>% gather(var,value,amt_2013:amt_2014) %>% ggplot(aes(x=group,y=value/12,fill=var))+geom_boxplot()+coord_cartesian(ylim=c(0,2000))+stat_summary(fun.y=mean,geom='point',position=position_dodge(0.75),shape=18,size=4,color="red",show_guide=F)+theme_bw()+scale_y_continuous('Average Debit Spend',labels=dollar)+theme(panel.grid.major=element_blank(),legend.position="bottom")+scale_x_discrete("NSF Incidence 2013 & 2014",labels=c('Neither Period','2014 Only','2013 Only','Both Periods'))+scale_fill_manual('Year',labels=c('2013',"2014"),values=c(yellow,green))


nsf_aux$delta2 = (nsf_aux$amt_2014/nsf_aux$amt_2013)-1
nsf_aux$delta3 = cut(nsf_aux$delta2,c(-Inf,-1,-0.75,-0.5,-0.25,-0.1,0,.1,.25,.5,.75,1,Inf),dig.lab =10)

deb1 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12) %>% group_by(delta3) %>% summarise(inc_2013=mean(nsf_2013>0),inc_2014=mean(nsf_2014>0),N=n(),with2013=sum(nsf_2013>0),with2014=sum(nsf_2014>0),events13=sum(events_2013),events14=sum(events_2014)) %>% group_by(delta3) %>% mutate(test_g=prop.test(c(with2013,with2014),c(N,N),alternative='g')$p.value,test_l=prop.test(c(with2013,with2014),c(N,N),alternative='l')$p.value)
write.table(deb1,'clipboard-128',sep='\t',row.names=F)


deb2 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12) %>% group_by(delta3) %>%
  summarise(t2013=mean(events_2013[events_2013>0]),t2014=mean(events_2014[events_2014>0]),
            test_g = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='g')$p.value,
            test_l = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='l')$p.value)
write.table(deb2,'clipboard-128',sep='\t',row.names=F)


rm(debit_aux,debit_all,activity1,cqi_201412,hhld_201412,accts_201412)
ggsave('debit trend by NSF events 2.png',width = 9,height = 5.5,units='in')


#Nishith asked for opt-in,
load("Z:/M&T Projects/NSF/optin_2012_2014.rdata")
optin1 = optin_2012_2014 %>% filter(period %in% c('201301','201412')) %>% group_by(EXPRESSION_8) 
optin1$ACCT_REG_E_FLAG_CUR[optin1$optin$ACCT_REG_E_FLAG_CUR  !="I"] = "O"
optin1 = optin1 %>% spread(period,ACCT_REG_E_FLAG_CUR )
optin1$grp = ""
optin1$grp[ optin1$`201301`=="I" & optin1$`201412`=="O" ] = 'IO'
optin1$grp[ optin1$`201301`=="I" & optin1$`201412`=="I" ] = 'II'
optin1$grp[ optin1$`201301`=="O" & optin1$`201412`=="O" ] = 'OO'
optin1$grp[ optin1$`201301`=="O" & optin1$`201412`=="I" ] = 'OI'

nsf_aux = left_join(nsf_aux, optin1[c(1,4)])  

opt1 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12) %>% group_by(grp) %>% summarise(inc_2013=mean(nsf_2013>0),inc_2014=mean(nsf_2014>0),N=n(),with2013=sum(nsf_2013>0),with2014=sum(nsf_2014>0),events13=sum(events_2013),events14=sum(events_2014)) %>% group_by(grp) %>% mutate(test_g=prop.test(c(with2013,with2014),c(N,N),alternative='g')$p.value,test_l=prop.test(c(with2013,with2014),c(N,N),alternative='l')$p.value)
write.table(opt1,'clipboard-128',sep='\t',row.names=F)

opt2 = nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12 & grp %in% c('II','IO','OO','OI')) %>% group_by(grp) %>% summarise(t2013=mean(events_2013[events_2013>0]),t2014=mean(events_2014[events_2014>0]),test_g = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='g')$p.value,test_l = t.test(events_2013[events_2013>0],events_2014[events_2014>0],alternative='l')$p.value)

write.table(opt2,'clipboard-128',sep='\t',row.names=F)
nsf_aux %>% filter(book=='back' & N_2014==12 & N_2013==12 & grp %in% c('II','IO','OO','OI')) %>% gather(measure,value,nsf_2013:nsf_2014) %>% filter(value > 0) %>% ggplot(aes(y=value,x=grp,fill=measure))+ geom_boxplot()+stat_summary(fun.y=mean,clor='red',shape=18,geom='point',position=position_dodge(0.75),color='red',size=4)+theme_bw()+coord_cartesian(ylim=c(0,100))

#I need to do the same or simialr for front book 
#I want to compare 2013 and 2014, any non back book accts that were present all year for simpkicity

new1 = nsf %>% filter(open %in% c('201301','201302','201303','201304','201305','201306','201401','201402','201403','201404','201405','201406') & period %in% c('201312','201412') & is.na(closed_period)) %>% select(EXPRESSION_8,open,ACCT_CONTR_BALANCE,period) 
new1$grp[new1$open  %in% c('201301','201302','201303','201304','201305','201306')] = 'n2013'
new1$grp[new1$open  %in% c('201401','201402','201403','201404','201405','201406')] = 'n2014'

f1 = new1 %>% group_by(grp,period,bal=cut(ACCT_CONTR_BALANCE,c(-Inf,0,250,500,750,1000,2500,5000,10000,Inf),dig.lab = 10)) %>% summarise(N=n()) %>% spread(period,N)

new1 %>% group_by(grp,period) %>% summarise(bal=mean(ACCT_CONTR_BALANCE,na.rm=T)) %>% spread(period,bal)

write.table(f1,'clipboard-128',sep='\t',row.names=F)

new1 %>% filter((grp=='n2013' & period=='201312') | (grp=='n2014' & period=='201412')) %>% ggplot(aes(x=grp,y=ACCT_CONTR_BALANCE,fill=period))+geom_boxplot()+coord_cartesian(ylim=c(0,10000))+stat_summary(fun.y=mean,geom='point',position=position_dodge(0.75),shape=18,size=4,color="red",show_guide=F)+theme_bw()+scale_y_continuous('Average Balance in December',labels=dollar)+theme(panel.grid.major=element_blank(),legend.position="bottom")+scale_x_discrete("Accounts Opened in First Half of Year",labels=c('2013','2014'))+scale_fill_manual('Year',labels=c('2013',"2014"),values=c(yellow,green))

new_aux =nsf %>% filter(period %in% c('201307','201308','201309','201310','201311','201312','201407','201408','201409','201410','201411','201412')) %>% group_by(EXPRESSION_8, year) %>% summarise(N=n(),events=sum(ACCT_NSF_TOTAL),with=sum(ACCT_NSF_TOTAL>0)) %>% gather(measure,value,N:with) %>% unite(key,measure,year) %>% spread(key,value)

new2 = new1 %>% spread(period,ACCT_CONTR_BALANCE)
names(new2)[4:5] = paste0("bal_",names(new2)[4:5])
new2 = left_join(new2,new_aux)

new3 = new2 %>% mutate(nsf_2013=ifelse(with_2013,1,0),nsf_2014=ifelse(with_2014,1,0)) %>% group_by(grp) %>% summarise_each(funs(mean(.,na.rm=T),sum(.,na.rm=T)),c(bal_201312:with_2014))
write.table(new3,'clipboard-128',sep='\t',row.names=F)

new4= new2 %>% mutate(nsf_2013=ifelse(with_2013,1,0),nsf_2014=ifelse(with_2014,1,0)) %>% group_by(grp) %>% summarise(n=n(),e2013=mean(events_2013[with_2013>0],na.rm=T),e2014=mean(events_2014[with_2014>0],na.rm=T))
write.table(new4,'clipboard-128',sep='\t',row.names=F)
