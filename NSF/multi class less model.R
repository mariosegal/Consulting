

#First explore the distribution of less

delta <- model_data_new$events_last6m - model_data_new$events_future
hist(delta[delta<0],breaks=100)
prop.table(table(cut(delta[delta>0 & model_data_new$flag_less1],c(seq(0,10,1),Inf))))

delta1 <- delta[delta>0 & model_data_new$flag_less1]
write.table(prop.table(table(cut(delta1,c(seq(0,10,1),Inf)))),'clipboard',sep='\t',row.names=F)

#I want to try to do a kmeans, it did nto work as I expected
km1 <- kmeans(delta1,5)
km1$size
tapply(delta1,km1$cluster,summary)
plot(delta1,col=km1$cluster)


delta2 <- cut(delta1,c(1,2,3,6,Inf),labels = c('1','2','3 to 5','6+'))
table(delta2)

table(cut(delta,c(-Inf,-6.01,-3.01,-2.01,-1.01,-0.001,0,.001,1,2,3,6,Inf)))

#add optin and alerts
load("Z:/M&T Projects/NSF/esvc_201406_full.rdata")
names(esvc_201406_full) <- tolower(gsub('-','_',names(esvc_201406_full)))
names(esvc_201406_full) <- (gsub('e_service_','',names(esvc_201406_full)))
library(stringr)
esvc_201406_full$service <- str_trim(esvc_201406_full$service )
esvc_201406_full$date_cancel <- as.Date(esvc_201406_full$date_cancel,'%m/%d/%Y')
esvc_201406_full$date_enrollment <- as.Date(esvc_201406_full$date_enrollment,'%m/%d/%Y')
esvc_201406_full$value_1 <- str_trim(esvc_201406_full$value_1)

alerts <- esvc_201406_full %>% filter(service=='ALERTS' & value_1 %in% c('AvailableBalance','DepositReceived','LargeWithdrawal','LowBalance','ReturnedDepositedItem') & (is.na(date_cancel) | date_enrollment > date_cancel )) %>% group_by(expression_2,value_1) %>% summarise(N=n()) %>% spread(value_1,N,fill=0)

model_data_new <- left_join(model_data_new,alerts,by=c('EXPRESSION_8'='expression_2'))
model_data_new[108:112][is.na(model_data_new[108:112])] <-0

rm(esvc_201406_full,alerts)

load("Z:/M&T Projects/NSF/optin_201406.rdata")
model_data_new <- left_join(model_data_new,optin_201406)
table(model_data_new$ACCT_REG_E_FLAG_CUR)
model_data_new$ACCT_REG_E_FLAG_CUR[model_data_new$ACCT_REG_E_FLAG_CUR %in%  c(' ','F')] <- 'O'
model_data_new$ACCT_REG_E_FLAG_CUR <- factor(as.character(model_data_new$ACCT_REG_E_FLAG_CUR))

save(model_data_new,file='model_data_new1.rdata')
rm(optin_201406)

#try a model 

model_data_new$delta2 <- cut(delta,c(-Inf,1,2,3,6,Inf),labels = c('More','1','2','3 to 5','6+'))
model_data_new$delta2[model_data_new$flag_less1==0] <- 'More'
dropx = c('ACCT_DATE_OPENED_FOR_PRIME','flag_closed','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less1','flag_closed1','flag_less')

set.seed(457)
intrain <- createDataPartition(model_data_new$delta2,p=0.1,list=F)
train1 <- model_data_new[intrain,-which(names(model_data_new) %in% dropx)]
test1 <- model_data_new[-intrain,-which(names(model_data_new) %in% dropx)]

source('measures2.R')

#baseline
delta0 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=1)
measures2(delta0,train1$delta2,type='class')
write.table(measures2(delta0,train1$delta2,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

delta1 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'))
measures2(delta1,train1$delta2,type='class')
write.table(measures2(delta1,train1$delta2,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)


delta2 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=0.001)
measures2(delta2,train1$delta2,type='class')
write.table(measures2(delta2,train1$delta2,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta2)

#optimzie  the cp from 0.001 to 0.01

cp_grid2 <- expand.grid(.cp=seq(0.001,0.01,by=0.001))
delta3cv<- train(delta2 ~ ., data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2)
delta3cv
delta3cv$finalModel
fancyRpartPlot(delta3cv$finalModel)
measures2(delta3cv$finalModel,train1$delta2,type='class')
write.table(measures2(delta2,train1$delta2,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

delta3 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=0.002)
measures2(delta3,train1$delta2,type='class')
write.table(measures2(delta3,train1$delta2,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta3)


penaltyx <- matrix(c(0,1,1,1,2,1,0,1,1,1,2,1,0,1,1,4,2,1,0,1,8,4,2,1,0),byrow=T,nrow=5)


#not a great model, lets do active with NSF
idx = train1$events_last6m>0 & train1$active_201406==1
delta4 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=1,subset=idx)
measures2(delta4,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta4,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)


delta5 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),subset=idx)
measures2(delta5,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta5,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)


delta6 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=0.002,subset=idx)
dim(delta6$frame)
measures2(delta6,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta6,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta6,split.cex=4,facsep='\t')
varImp(delta6)

delta7 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini'),cp=0.0025,subset=idx)
dim(delta7$frame)
measures2(delta7,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta7,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta7,split.cex=2,facsep='\t')
confusionMatrix(predict(delta7,type='class'),train1$delta2[idx])

#i will now add the penalty
delta8 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini',loss=penaltyx),subset=idx)
dim(delta8$frame)
measures2(delta8,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta8,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta8,split.cex=2,facsep='\t')
confusionMatrix(predict(delta8,type='class'),train1$delta2[idx])

delta9 <- rpart(delta2 ~.,data=train1,method='class',parms=list(split='gini',loss=penaltyx),subset=idx,cp=0.001)
dim(delta9$frame)
measures2(delta9,train1$delta2[idx],type='class',newdata=train1[idx,])
write.table(measures2(delta9,train1$delta2[idx],type='class',newdata=train1[idx,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta9,split.cex=2,facsep='\t')
confusionMatrix(predict(delta9,type='class'),train1$delta2[idx])


#redefine the flag to be 3 levels: 'More', '3+' ,'1-2'
model_data_new$delta3 <- ifelse(model_data_new$delta2=='More','More',NA)
model_data_new$delta3 <- ifelse(model_data_new$delta2 %in% c('1','2'),'Up to 2',model_data_new$delta3)
model_data_new$delta3 <- ifelse(model_data_new$delta2 %in% c('3 to 5','6+'),'3+',model_data_new$delta3)

table(model_data_new$delta2,model_data_new$delta3)


dropx1 = c('ACCT_DATE_OPENED_FOR_PRIME','flag_closed','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less1','flag_closed1','flag_less','delta2')

set.seed(4578)
intrain2 <- createDataPartition(model_data_new$delta3,p=0.1,list=F)
train2 <- model_data_new[intrain2,-which(names(model_data_new) %in% dropx1)]
test2 <- model_data_new[-intrain2,-which(names(model_data_new) %in% dropx1)]


idx2 = train2$events_last6m>0 & train2$active_201406==1

delta10 <- rpart(delta3 ~.,data=train2,method='class',parms=list(split='gini'),subset=idx2,cp=1)
dim(delta10$frame)
measures_multi(train2$delta3[idx2],predict(delta10,type='class'))
write.table(measures_multi(train2$delta3[idx2],predict(delta10,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta10,split.cex=2,facsep='\t')

delta11 <- rpart(delta3 ~.,data=train2,method='class',parms=list(split='gini'),subset=idx2,cp=.005)
dim(delta11$frame)
measures_multi(train2$delta3[idx2],predict(delta11,type='class'))
write.table(measures_multi(train2$delta3[idx2],predict(delta11,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta11,split.cex=2,facsep='\t')

delta12 <- rpart(delta3 ~.,data=train2,method='class',parms=list(split='gini'),subset=idx2,cp=.002)
dim(delta12$frame)
measures_multi(train2$delta3[idx2],predict(delta12,type='class'))
write.table(measures_multi(train2$delta3[idx2],predict(delta12,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta12,split.cex=2,facsep='\t')

penaltyx2 <- matrix(c(0,2,2,1,0,1,1,1,0),byrow=T,nrow=3)

delta13 <- rpart(delta3 ~.,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx2)
dim(delta13$frame)
measures_multi(train2$delta3[idx2],predict(delta13,type='class'))
write.table(measures_multi(train2$delta3[idx2],predict(delta13,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta13,split.cex=2,facsep='\t')


delta14 <- rpart(delta3 ~.,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx2,cp=0.005)
dim(delta14$frame)
measures_multi(train2$delta3[idx2],predict(delta14,type='class'))
write.table(measures_multi(train2$delta3[idx2],predict(delta14,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta14,split.cex=1,facsep='\t')


#all of this is nice, but delta3 depends on events_last6m, so it is kind of weird to have thme
#it makes sense because it can separate between levels of events, but then again it may not
#lets taje it out
idx3 = train2$events_last6m>=3 & train2$active_201406==1 

delta15 <- rpart(delta3 ~.  ,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx3)
dim(delta15$frame)
measures_multi(train2$delta3[idx3],predict(delta15,type='class'))
write.table(measures_multi(train2$delta3[idx3],predict(delta15,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta15,split.cex=1,facsep='\t')

delta16 <- rpart(delta3 ~.  ,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx3,cp=0.005)
dim(delta16$frame)
measures_multi(train2$delta3[idx3],predict(delta16,type='class'))
write.table(measures_multi(train2$delta3[idx3],predict(delta16,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta16,split.cex=1,facsep='\t')

#I do not like that evenst keeps coming up
#I will try to see what  happens if I take all of those plus the feew (as they are equivalent almost) out
out = names(train2)[grepl('events|fees',names(train2))]
formula1 <- as.formula(paste('delta3 ~ ','. -',paste(out,collapse='-')))

delta17 <- rpart(formula1  ,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx3)
dim(delta17$frame)
measures_multi(train2$delta3[idx3],predict(delta17,type='class'))
write.table(measures_multi(train2$delta3[idx3],predict(delta17,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta17,split.cex=1,facsep='\n')


delta18 <- rpart(formula1  ,data=train2,method='class',parms=list(split='gini',loss=penaltyx2),subset=idx3,cp=.004)
dim(delta18$frame)
measures_multi(train2$delta3[idx3],predict(delta18,type='class'))
write.table(measures_multi(train2$delta3[idx3],predict(delta18,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(delta18,split.cex=1,facsep='\n')

measures_multi(test2$delta3[test2$events_last6m>=3 & test2$active_201406==1],
               predict(delta18,newdata=test2[test2$events_last6m>=3 & test2$active_201406==1,],type='class'))


png('multi_model.png',width = 9, height = 5, units='in', res=600)
my_fancy(delta17,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,compress=F,round=0,tweak=1.0,sub='delta17_20150409',facsep='\n')
dev.off()


#some stats for  the break even analysis

#what is the average decraese for thgose classidfied as 3+

delta_test <- model_data_new$events_last6m - model_data_new$events_future
delta_test <- delta_test[-intrain2]
tapply(delta_test[test2$events_last6m>=3 & test2$active_201406==1],predict(delta18,newdata=test2[test2$events_last6m>=3 & test2$active_201406==1,],type='class'),mean)

#how many actually had any
aux <- model_data_new[-intrain2,]
aux$pred = predict(delta17,newdata=aux,type='class')

tapply(aux$events_future[aux$events_last6m>=3 & aux$active_201406==1]>0,
       aux$pred[aux$events_last6m>=3 & aux$active_201406==1],mean)


tapply(aux$events_future[aux$events_last6m>=3 & aux$active_201406==1 & aux$events_future>0],
       aux$pred[aux$events_last6m>=3 & aux$active_201406==1 & aux$events_future>0],mean)


tapply(aux$events_future[aux$events_last6m>=3 & aux$active_201406==1 ],
       aux$pred[aux$events_last6m>=3 & aux$active_201406==1 ],sum)

aux$delta <- aux$events_last6m - aux$events_future



tapply(aux$delta[aux$events_last6m>=3 & aux$active_201406==1 ],
       aux$pred[aux$events_last6m>=3 & aux$active_201406==1 ],mean)

#I also need the variance
tapply(aux$delta[aux$events_last6m>=3 & aux$active_201406==1 ],
       aux$pred[aux$events_last6m>=3 & aux$active_201406==1 ],sd)

# how many will I predict 
predq = predict(delta17,newdata=model_data_new,type='class')
table(predq[model_data_new$active_201406==1 & model_data_new$events_last6m>2])

#calculate required N
#did it on mac as I needed a new library

save(list=ls(pattern='delta'),file='delta_models.rdata')
save(model_data_new,file='model_data_new1.rdata')

#can I see an impact between waivers and doing less or more
library(ggplot2)
data <- model_data_new[intrain2,]
data$delta = data$events_last6m- data$events_future
ggplot(data,aes(y=waivers_cum_last6m,x=delta,color=delta3))+geom_point(alpha=0.1)+coord_cartesian(ylim=c(0,500),xlim=c(-50,50))

data %>% filter(events_last6m >=3) %>% group_by(delta3) %>% summarise(cor=cor(waivers_cum_last6m,delta))
#correlation is very low
