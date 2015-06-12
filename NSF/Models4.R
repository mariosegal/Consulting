#3/31 Nishith wanted only 3 models
#1) Less and zero but active
#2) closed/inactive
#3) more
#4) an attrtion odel (TBD)

#The first thing is to create a small data_set with all my data, this is likely model_data
#and save it for simplicity
#the start with the first model
#I will do a waterfall as well to show all the process and I will not 
#exclude NAs per his request

#load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

#load data
rm(list=ls())
load('model_data.rdata')
#this file has the 2 definitions now, andf it hasd all processed variables`


###########################
####  MODEL for LESS   ####
###########################

#define a smaller data set, with the variables that make sense - we have many variables that we will not use
#for example expression_8, or ACCT_ID, and also 2014 variables that can be sued to define the groups but not for modeling

variables2 <- c(7,8,10,14,22,26,32:65,67,70:77,79:84,89:99,100:103,105:108,110,111,112:113,115:118,119,120)
data_small <- subset(model_data,!is.na(flag_less_new),variables2)

#dump for list of variables
write.table(names(data_small),'clipboard',sep='\t',row.names=F)

set.seed(6767)
intrain <- createDataPartition(data_small$flag_less_new,p=0.5,list=F)
train1 <- data_small[intrain,-which(names(data_small) %in% c('flag_closed_inactive_new'))]
test1 <- data_small[-intrain,-which(names(data_small) %in% c('flag_closed_inactive_new'))]
train1$flag_less_new <- as.factor(train1$flag_less_new)

measures2 <- function(model,actual,...) {
  pred = predict(model,...)
  t = table(actual,pred)
  acc = sum(diag(t))/sum(t)
  sen = prop.table(t,1)[2,2]
  spec = prop.table(t,1)[1,1]
  f1 = 2*t[2,2]/(2*t[2,2]+t[1,2]+t[2,1])
  return(data.frame(accuracy=acc,sensitivity=sen,specificity=spec,F1_score=f1))
}

#Start with a basic rpart model, forcing it to use gini with the default parameters
#which are minsplit=20
less0 <- rpart(flag_less_new ~ ., data=train1,method='class', parms=list(split='gini'),cp=1)
less0
measures2(less0,train1$flag_less_new,type='class')

less1 <- rpart(flag_less_new ~ ., data=train1,method='class', parms=list(split='gini'))
less1
measures2(less1,train1$flag_less_new,type='class')
write.table(measures2(less1,train1$flag_less_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

#this model does not split at all, that means we can't at all do better than guessing that you will do less

less2 <- rpart(flag_less_new ~ ., data=train1,method='class', parms=list(split='gini'),cp=0.0)
 #force a maximum tree
less2
measures2(less2,train1$flag_less_new,type='class')
write.table(measures2(less2,train1$flag_less_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
dim(less2$frame)[1]

#try optizing cp using CV
cp_grid1 <- expand.grid(.cp=seq(0.001,0.1,by=0.001))
less3<- train(flag_less_new ~ ., data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid1)

#build cp otimal model
less4 <- rpart(flag_less_new ~ ., data=train1,method='class', parms=list(split='gini'),cp=0.001)
less4
measures2(less4,train1$flag_less_new,type='class')
write.table(measures2(less4,train1$flag_less_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
dim(less4$frame)[1]

#if chose the minimum value it could after 0, so I want to try more granularity still
cp_grid2 <- expand.grid(.cp=seq(0.0001,0.001,by=0.0001))
less5<- train(flag_less_new ~ ., data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid2)
less5

build cp otimal model with more granular cp
less6 <- rpart(flag_less_new ~ ., data=train1,method='class', parms=list(split='gini'),cp=0.0005)
less6
measures2(less6,train1$flag_less_new,type='class')
write.table(measures2(less6,train1$flag_less_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
dim(less6$frame)[1]
my_fancy(less6,under=T,tweak=2,extra1=108,branch.lty1=1,branch.col='black',type=1)
pdf('less6.pdf',width=14,height=8.5)
  my_fancy(less6,under=T,tweak=2,extra1=108,branch.lty1=1,branch.col='black',type=1)
dev.off()


save(list=ls(pattern='less|train1|test1'),file='less_models_20150401.rdata')
rm(list=ls(pattern='less|train1|test1'))
rm(data_small)

#this is the best I can do with this variables and using CART
#not a great model to find the less, and it is too complex at that

####################################
####  MODEL for LOST/INACTIVE   ####
####################################

data_small_lost <- subset(model_data,!is.na(flag_closed_inactive_new),variables2)
set.seed(7556)
intrain2 <- createDataPartition(data_small_lost$flag_closed_inactive_new,p=0.5,list=F)
train2 <- data_small_lost[intrain2,-which(names(data_small_lost) %in% c('flag_less_new'))]
test2 <- data_small_lost[-intrain2,-which(names(data_small_lost) %in% c('flag_less_new'))]
train2$flag_closed_inactive_new <- as.factor(train2$flag_closed_inactive_new)


#Start with a basic rpart model, forcing it to use gini with the default parameters
#which are minsplit=20
lost0 <- prop.table(table(train2$flag_closed_inactive_new))[1]
lost0 <- rpart(flag_closed_inactive_new ~ ., data=train2,method='class', parms=list(split='gini'),cp=1)
measures2(lost0,train2$flag_closed_inactive_new,type='class')

lost1 <- rpart(flag_closed_inactive_new ~ ., data=train2,method='class', parms=list(split='gini'))
lost1
measures2(lost1,train2$flag_closed_inactive_new,type='class')
write.table(measures2(lost1,train2$flag_closed_inactive_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)
pdf('lost1.pdf',width=14,height=8.5)
  my_fancy(lost1,main='Lost/Inactive Model 1',under=T,split.cex=1,extra1=108,branch.lty1=1,branch.col='black',type1=2,compress=F,under.cex=1.0,round=0,nn.cex=0.7,cex=.7)
dev.off()

#quite simple model, try a maximal model 


#force maximal
lost2 <- rpart(flag_closed_inactive_new ~ ., data=train2,method='class', parms=list(split='gini'),cp=0.0)
dim(lost2$frame)[1]
lost2
measures2(lost2,train2$flag_closed_inactive_new,type='class')
write.table(measures2(lost2,train2$flag_closed_inactive_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

#do cv
cp_grid3 <- expand.grid(.cp=c(seq(0.0001,0.001,by=0.0001),seq(.002,0.01,.001)))
lost3<- train(flag_closed_inactive_new ~ ., data=train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid3)
lost3
#value of cp was 0.01

lost4 <- rpart(flag_closed_inactive_new ~ ., data=train2,method='class', parms=list(split='gini'),cp=0.001)
dim(lost4$frame)[1]
lost4
measures2(lost4,train2$flag_closed_inactive_new,type='class')
write.table(measures2(lost4,train2$flag_closed_inactive_new,type='class'),'clipboard',sep='\t',row.names=F,col.names=F)

pdf('lost4.pdf',width=14,height=8.5)
my_fancy(lost4,main='Lost/Inactive Model 4',under=T,split.cex=1,extra1=108,branch.lty1=1,branch.col='black',type1=2,compress=F,under.cex=1.0,round=0,nn.cex=0.7,cex=.7)
dev.off()


#top vars 
top4 <- data.frame(var=row.names(varImp(lost4)),Overall=varImp(lost4))
top4 <- top4[top4$Overall >0,]
top4<- top4[order(-top4$Overall),]


top_less6 <- data.frame(var=row.names(varImp(less6)),Overall=varImp(less6))
top_less6 <- top_less6[top_less6$Overall >0,]
top_less6<- top_less6[order(-top_less6$Overall),]


save(list=ls(pattern='lost|train2|test2'),file='lost_models_20150401.rdata')
save(data_small_lost,file='data_small_lost.rdata')
rm(data_small_lost)
rm(list=ls(pattern='lost|train2|test2'))