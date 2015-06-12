
#continued from models

#the multi class model is either too complex, ort missed some classes no matter what i tried
#so I will go back to individual models




#take out the fees_9m_2013 and months_9m_2013less1 <- rpart(flag_less~.,train1[-56],method='class')
less2 <- rpart(flag_less~.,train1[-c(56,2,3)],method='class')
measures(train1$flag_less,predict(less2,type='class'))
prop.table(table(train1$flag_less,predict(less2,type='class')),1)
write.table(measures(train1$flag_less,predict(less2,type='class')),'clipboard',sep='\t',row.names=F)
dim(less2$frame)[1]
fancyRpartPlot(less2)

#force maximal tree
less3 <- rpart(flag_less~.,train1[-c(56,2,3)],method='class',cp=0.0)
measures(train1$flag_less,predict(less3,type='class'))
prop.table(table(train1$flag_less,predict(less3,type='class')),1)
write.table(measures(train1$flag_less,predict(less3,type='class')),'clipboard',sep='\t',row.names=F)
dim(less3$frame)[1]



less3cv <- train(flag_less~.,train1[-c(56,2,3)],method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid)
less3cv 

less3f <- rpart(flag_less~.,train1[-c(56,2,3)],method='class',cp=0.01)
measures(train1$flag_less,predict(less3f,type='class'))
prop.table(table(train1$flag_less,predict(less3f,type='class')),1)
write.table(measures(train1$flag_less,predict(less3f,type='class')),'clipboard',sep='\t',row.names=F)
dim(less3f$frame)[1]


less4 <- rpart(flag_less~.,train1[-c(56)],method='class',minbucket=1)
measures(train1$flag_less,predict(less4,type='class'))
prop.table(table(train1$flag_less,predict(less3f,type='class')),1)
write.table(measures(train1$flag_less,predict(less4,type='class')),'clipboard',sep='\t',row.names=F)
dim(less4$frame)[1]
fancyRpartPlot(less4)


#increse train size
set.seed(6971)
intrain2 <- createDataPartition(data_small_clean2$flag_delta,p=0.5,list=F)
train2 <- data_small_clean2[intrain2,]
test2 <- data_small_clean2[-intrain2,]


#define new vars for less or not less

train2$flag_less <- ifelse(train2$flag_delta=="less",'less',"not_less")
train2$flag_less <- factor(train2$flag_less,c('not_less','less'),labels=c('not_less','less'), ordered = T)
prop.table(table(train2$flag_less,useNA='ifany'))


less1a <- rpart(flag_less~.,train2[-56],method='class')
measures(train2$flag_less,predict(less1a,type='class'))
prop.table(table(train2$flag_less,predict(less1a,type='class')),1)
write.table(measures(train2$flag_less,predict(less1a,type='class')),'clipboard',sep='\t',row.names=F)
dim(less1a$frame)[1]
fancyRpartPlot(less1a)




cp_grid1 <- expand.grid(.cp=seq(0.001,0.1,by=0.001))
less2a_cv <- train(flag_less~.,train2[-c(56)],method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid1)
less2a_cv


less2a <- rpart(flag_less~.,train2[-56],method='class',cp=0.001)
dim(less2a$frame)[1]
measures(train2$flag_less,predict(less2a,type='class'))
prop.table(table(train2$flag_less,predict(less2a,type='class')),1)
write.table(measures(train2$flag_less,predict(less2a,type='class')),'clipboard',sep='\t',row.names=F)
fancyRpartPlot(less2a,split.cex=2,node.cex=1.2)
png('less2a.png',width=9,height=5,units='in',res=600)
  fancyRpartPlot(less2a,tweak=1.2,branch.col='black',branch.lwd=6)
dev.off()
                 
save.image('models.rdata')
