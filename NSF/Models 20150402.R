#load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)

source('measures2.R')

#load data
load('model_data_new.rdata')

#create training set
drop = c('ACCT_DATE_OPENED_FOR_PRIME','flag_closed','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less1')

set.seed(6767)
intrain <- createDataPartition(model_data_new$flag_less,p=0.1,list=F)
train1 <- model_data_new[intrain,-which(names(model_data_new) %in% drop)]
test1 <- model_data_new[-intrain,-which(names(model_data_new) %in% drop)]

train1$flag_less <- factor(train1$flag_less)
penalty <- matrix(c(0,1,2,0),byrow=T,nrow=2,ncol=2)
penalty1 <- matrix(c(0,2,1,0),byrow=T,nrow=2,ncol=2)

#baseline model
less0 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini'),cp=1)
less0
measures2(less0,train1$flag_less,type='class')
write.table(measures2(less0,train1$flag_less,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)


               
#initial model with no penalty, default params
less1 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini'))
less1
measures2(less1,train1$flag_less,type='class')
write.table(measures2(less1,train1$flag_less,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less1)
table(train1$flag_less,predict(less1,type='class'))
#athe model first guesses all with zweo will be zero or more (logical and predicted), then simply guesse all else wthose who has 1+
#will do less, this is 61% right, but given the large number of no NSF onbase it yields anexcellent model


#I will now add the penalty, to tell it we do not want to predict 0 when it is 1
less2 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini',loss=penalty))
less2
measures2(less2,train1$flag_less,type='class')
write.table(measures2(less2,train1$flag_less,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less2)
table(train1$flag_less,predict(less2,type='class'))
#we get the same model, asthe penalty is not improving, I do not believe anything else will work

#I will do a 2 stage model, first model is to guess same or more more for tose with 0 on base, as it is logical and expected
#then I will build a new model only for those that have base >=1 
#(which was my initial intuition anyway)
less3 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini'),subset=events_last6m>0)
less3
measures2(less3,train1$flag_less[train1$events_last6m>0],type='class')
write.table(measures2(less3,train1$flag_less[train1$events_last6m>0],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

#now forcr a maximal tree for this
less4 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini'),subset=events_last6m>0,cp=0.0)
less4
measures2(less4,train1$flag_less[train1$events_last6m>0],type='class')
write.table(measures2(less4,train1$flag_less[train1$events_last6m>0],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
plot(less4)
#this does shows some promise, with some optimal Cp

#i should add the penalty to the second model
less5 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini',loss=penalty),subset=events_last6m>0)
less5
measures2(less5,train1$flag_less[train1$events_last6m>0],type='class')
write.table(measures2(less5,train1$flag_less[train1$events_last6m>0],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less5)
#penalty is not working

#optimize with no penalty
cp_grid1 <- expand.grid(.cp=seq(0.01,0.05,by=0.01))
cp_grid2 <- expand.grid(.cp=seq(0.001,0.01,by=0.001))
less6cv<- train(flag_less ~ ., data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2,subset=events_last6m>0)
less6cv
less6cv$finalModel

less6 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini'),subset=events_last6m>0,cp=0.005)
less6
measures2(less6,train1$flag_less[train1$events_last6m>0],type='class')
write.table(measures2(less6,train1$flag_less[train1$events_last6m>0],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less6)
table(train1$flag_less[train1$events_last6m>0],predict(less6,type='class'))
#this model is quite nice, it has some nice simmetry

#I also want to try the reverse penalty, I really do not know which is right, but my intutition says penalty is right
less7 <- rpart(flag_less~.,data=train1,method='class',parms=list(split='gini',loss=penalty1),subset=events_last6m>0)
less7
measures2(less7,train1$flag_less[train1$events_last6m>0],type='class')
write.table(measures2(less5,train1$flag_less[train1$events_last6m>0],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less7)
table(train1$flag_less[train1$events_last6m>0],predict(less7,type='class'))
#this model is worse. it prefers to guess 0 when 1, and we wnt the reverse

#the best model is less6, create chart
source('myfancy.R')

png('less_model_with_NSF.png',width = 9, height = 5, units='in', res=600)
  my_fancy(less6,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,compress=F,round=0,tweak=1.0,sub='less6_20150403')
dev.off()

png('less_model_no_NSF.png' ,width=3, height=2, units='in', res=600)
my_fancy(less1,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,compress=F,round=0,tweak=0.8,sub='')
dev.off()

#quick stats for table
model_data_new %>% group_by(fut_nsf=ifelse(events_future==0,'none','some'),
                            future=ifelse(events_future<events_last6m,'less','more'),
                            base=ifelse(events_last6m==0,'None','Some')) %>%
  summarise(N=n())


model_data_new %>% group_by(closed=ifelse(is.na(closed_period),"N","Y"),active_201412) %>% summarise(N=n())


#I need to calaculate the accuracy and othe rmeasures for the join model on test set

test1$flag_less <- factor(test1$flag_less,c('0','1'))
#divide test into 2 parts
testa <- subset(test1,events_last6m==0)
testb <- subset(test1,events_last6m>0)

#for a. we simply apply less1; for b apply less6
#then run measureson rbind versions

predicta <- predict(less1,newdata=testa,type='class')
predictb <- predict(less6,newdata=testb,type='class')

validate_t <- table(act,pred)
pred <- c(as.character(predicta),as.character(predictb))
act <- c(as.character(testa$flag_les),as.character(testb$flag_les))
confusionMatrix(pred,act)

#dump variable names
write.table(names(train1),'clipboard',sep='\t')

save(list=ls(pattern='less|train1|test1'),file='less_models_20150403.rdata')

############################################################################
############################################################################
#####################
####  STOP MODEL  ###
#####################

#create training set
drop1 = c('ACCT_DATE_OPENED_FOR_PRIME','flag_less','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less1')

set.seed(78545)
intrain1 <- createDataPartition(model_data_new$flag_less,p=0.1,list=F)
train2 <- model_data_new[intrain1,-which(names(model_data_new) %in% drop1)]
test2 <- model_data_new[-intrain1,-which(names(model_data_new) %in% drop1)]

train2$flag_closed <- factor(train2$flag_closed)
penalty <- matrix(c(0,1,2,0),byrow=T,nrow=2,ncol=2)

#baseline model
lost0 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini'),cp=1)
lost0
measures2(lost0,train2$flag_closed,type='class')
write.table(measures2(lost0,train2$flag_closed,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

#model with basic params
lost1 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini'))
lost1
measures2(lost1,train2$flag_closed,type='class')
write.table(measures2(lost1,train2$flag_closed,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost1)
#as I expected those that were inactive likely remain inactive


#try the penalty to see if I can fix this with it, if not I will do a 2 stage again
#with std params it was the same as 1. sp I tried cp=0.0 to force a amximal tree
lost2 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini',loss=penalty),cp=0.0)
lost2
measures2(lost2,train2$flag_closed,type='class')
write.table(measures2(lost2,train2$flag_closed,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
plot(lost2)

#try to narrow cp via cv
lost3cv<- train(flag_closed ~ ., data=train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2)
lost3cv
lost3cv$finalModel

lost3cv1<- train(flag_closed ~ ., data=train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2, parms=list(loss=penalty))
lost3cv1
lost3cv1$finalModel
#this one did not work so well

#the cv was not with penalty
lost3 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini'),cp=0.001)
lost3
measures2(lost3,train2$flag_closed,type='class')
write.table(measures2(lost3,train2$flag_closed,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

fancyRpartPlot(lost3)

#lost 3 is maybe ok,
#but I want to try one model for the active ones
lost4 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini'),subset=active_201406==1)
lost4
measures2(lost4,train2$flag_closed[train2$active_201406==1],type='class')
write.table(measures2(lost4,train2$flag_closed[train2$active_201406==1],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)


lost5 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini',loss=penalty),subset=active_201406==1)
lost5
measures2(lost5,train2$flag_closed[train2$active_201406==1],type='class')
write.table(measures2(lost5,train2$flag_closed[train2$active_201406==1],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost5)

lost6cv<- train(flag_closed ~ ., data=train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2,subset=active_201406==1,parms=(list(loss=penalty)))
lost6cv
lost6cv$finalModel

#cv active no penalty
lost6cv1<- train(flag_closed ~ ., data=train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2,subset=active_201406==1)
lost6cv1
lost6cv1$finalModel


#this is really not working, lets try the active with NSF
lost7 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini',loss=penalty),subset=active_201406==1 & events_last6m >=1)
lost7
measures2(lost7,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class')
write.table(measures2(lost7,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost7)

penalty2 = penalty
penalty2[2,1] <- penalty2[2,1]*2
lost8 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini',loss=penalty2),subset=active_201406==1 & events_last6m >=1)
lost8
measures2(lost8,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class')
write.table(measures2(lost8,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost8)


penalty3 = penalty
penalty3[2,1] <- penalty3[2,1]*4
lost9 <- rpart(flag_closed~.,data=train2,method='class',parms=list(split='gini',loss=penalty3),subset=active_201406==1 & events_last6m >=1)
lost9
measures2(lost9,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class')
write.table(measures2(lost9,train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost9)
confusionMatrix(predict(lost9,newdata=train2[train2$active_201406==1 & train2$events_last6m>=1,],
                        type='class'),train2$flag_closed[train2$active_201406==1 & train2$events_last6m>=1],positive='1')


png('loss9.png' ,width=9, height=5, units='in', res=600)
my_fancy(lost9,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,compress=F,round=0,tweak=1.2,sub='lost9')
dev.off()

#I want to revisit the loss models and do one only for those that did not become inactive or closed
model_data_new$flag_less1 <- ifelse(model_data_new$active_201406==1 & 
                                      model_data_new$flag_closed==0 & 
                                      model_data_new$active_201412==1 &
                                      model_data_new$events_future<model_data_new$events_last6m,1,0)

drop2 = c('ACCT_DATE_OPENED_FOR_PRIME','flag_closed','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less','flag_closed1')


set.seed(6767)
intrain2 <- createDataPartition(model_data_new$flag_less1,p=0.1,list=F)
train3 <- model_data_new[intrain2,-which(names(model_data_new) %in% drop2)]
test3 <- model_data_new[-intrain2,-which(names(model_data_new) %in% drop2)]

train3$flag_less1 <- factor(train3$flag_less)



#baseline model
less0a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini'),cp=1)
less0a
measures2(less0a,train3$flag_less1,type='class')
write.table(measures2(less0a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)


#initial model with no penalty, default params
less1a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini'))
less1a
measures2(less1a,train3$flag_less1,type='class')
write.table(measures2(less1a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less1a)
table(train3$flag_less1,predict(less1a,type='class'))
confusionMatrix(predict(less1a,type='class'),train3$flag_less1,positive="1")
#thsi model splits immediately by those that had NSF as the other one, if I explicitely take them out will the model be the same on the right side

less2a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini'),subset=events_last6m>0 & active_201406 ==1)
less2a
measures2(less2a,train3$flag_less1[model_data_new$active_201406==1 & model_data_new$events_last6m>0],type='class',
          newdata=train3[model_data_new$active_201406==1 & model_data_new$events_last6m>0,])
write.table(measures2(less2a,train3$flag_less1[model_data_new$active_201406==1 & model_data_new$events_last6m>0],type='class',
                      newdata=train3[model_data_new$active_201406==1 & model_data_new$events_last6m>0,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less2a)
table(train3$flag_less1,predict(less1a,type='class'))

#less1a, sees nice, lets see if we can optimize using cv
less3cva<- train(flag_less1 ~ ., data=train3,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2)
less3cva
less3cva$finalModel

#trid cp grid between 0.1 to 0.5
#tried .01 to .09 amd .01 eas chosen
we ended up in cp=0.002

less3a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini'),cp=0.001)
less3a
measures2(less3a,train3$flag_less1,type='class')
write.table(measures2(less3a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less3a)
confusionMatrix(predict(less3a,type='class'),train3$flag_less1,positive="1")

#try oenalty
less4a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini',loss=penalty))
less4a
measures2(less4a,train3$flag_less1,type='class')
write.table(measures2(less4a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less4a)

less5cva<- train(flag_less1 ~ ., data=train3,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2,parms=list(split='gini',loss=penalty))
less5cva
less5cva$finalModel
fancyRpartPlot(less5cva$finalModel)

less5a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini',loss=penalty),cp=0.001)
less5a
measures2(less5a,train3$flag_less1,type='class')
write.table(measures2(less5a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less5a)

#can I make it a bit simple
less6a <- rpart(flag_less1~.,data=train3,method='class',parms=list(split='gini',loss=penalty),cp=0.00125)
less6a
measures2(less6a,train3$flag_less1,type='class')
write.table(measures2(less6a,train3$flag_less1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(less6a)

confusionMatrix(predict(less6a,newdata=test3,type='class'),test3$flag_less1,positive="1")

save(list=ls(pattern='less'),file='less_models_a.rdata')
#rm(list=ls(pattern='less'))

save(list=ls(pattern='lost'),file='lost_models.rdata')
#rm(list=ls(pattern='lost'))

#dump some params for flag definiton
table1 <- model_data_new %>% group_by(active_201412,closed_period,active_201406,
                                      before=ifelse(events_last6m==0,'none','some'),
                                      after=ifelse(events_future==0,'none','some'),
                                      less=ifelse(events_last6m>events_future,'less','not less')) %>% summarise(N=n())
write.table(table1,'clipboard-128',sep='\t',row.names=F)

png('less_model_2.png',width = 10, height = 6, units='in', res=600)
my_fancy(less5a,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,ycompress=T,round=0,tweak=1.7,sub='less5a_20150406',facsep='\n',split.border.col='gray',nn.cex=.6)
dev.off()

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=15), collapse="\n")
  }
  labs
}


png('less_model_6a.png',width = 9.5, height = 6, units='in', res=600)
my_fancy(less6a,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,ycompress=T,round=0,tweak=1.7,sub='less6a_20150406',facsep='\n',split.border.col='gray',nn.cex=.6,split.fun=split.fun)
dev.off()


measures2(less6a,test3$flag_less1,type='class',newdata=test3)


# I want to try the lost models again, this time with anew flag, I think tje otehr one was not so good as I confused the flag
model_data_new$flag_closed1 <- ifelse(model_data_new$active_201406==1 & model_data_new$active_201412==0,1,0)
table(model_data_new$flag_closed1)

#I will stil predict for all HHLDs, the target is the one that changed
drop3 = c('ACCT_DATE_OPENED_FOR_PRIME','flag_closed','active_201412','events_future','ACCT_ID','closed_period','EXPRESSION_8','ACCT_STYPE','flag_less','flag_less1')


set.seed(6767)
intrain3 <- createDataPartition(model_data_new$flag_closed1,p=0.1,list=F)
train4 <- model_data_new[intrain3,-which(names(model_data_new) %in% drop3)]
test4 <- model_data_new[-intrain3,-which(names(model_data_new) %in% drop3)]

train4$flag_closed1 <- factor(train4$flag_closed1)


lost0a <- rpart(flag_closed1~.,data=train4,method='class',parms=list(split='gini'),cp=1)
lost0a
measures2(lost0a,train4$flag_closed1,type='class')
write.table(measures2(lost0a,train4$flag_closed1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost0a)


lost1a <- rpart(flag_closed1~.,data=train4,method='class',parms=list(split='gini'))
lost1a
measures2(lost1a,train4$flag_closed1,type='class')
write.table(measures2(lost1a,train4$flag_closed1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost1a)



lost2a <- rpart(flag_closed1~.,data=train4,method='class',parms=list(split='gini',loss=penalty))
lost2a
measures2(lost2a,train4$flag_closed1,type='class')
write.table(measures2(lost2a,train4$flag_closed1,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost2a)

lost3cva<- train(flag_closed1 ~ ., data=train4,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2,parms=list(split='gini',loss=penalty))
lost3cva
lost3cva$finalModel
fancyRpartPlot(lost3cva$finalModel)


#model does not move beyond, lets do it only for active
lost4a <- rpart(flag_closed1~.,data=train4,method='class',parms=list(split='gini',loss=penalty2),subset=active_201406==1 )
lost4a
measures2(lost4a,train4$flag_closed1[train4$active_201406==1],type='class',newdata=train4[train4$active_201406==1,])
write.table(measures2(lost4a,train4$flag_closed1[train4$active_201406==1],type='class',newdata=train4[train4$active_201406==1,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost4a)


lost5a <- rpart(flag_closed1~.,data=train4,method='class',parms=list(split='gini',loss=penalty3),subset=active_201406==1 )
lost5a
measures2(lost5a,train4$flag_closed1[train4$active_201406==1],type='class',newdata=train4[train4$active_201406==1,])
write.table(measures2(lost5a,train4$flag_closed1[train4$active_201406==1],type='class',newdata=train4[train4$active_201406==1,]),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(lost5a)


png('lost_model_5a.png',width = 9, height = 5, units='in', res=600)
my_fancy(lost5a,main='',under=F,split.cex=1,extra1=104,branch.lty1=1,branch.col='black',type1=2,compress=F,round=0,tweak=1.0,sub='lost5a_20150406')
dev.off()

measures2(lost5a,train4$flag_closed1[test4$active_201406==1],type='class',newdata=test4[test4$active_201406==1,])
measures2(less5a,test3$flag_less1,type='class',newdata=test3)

save(list=ls(pattern='lost'),file='lost_models_a.rdata')
