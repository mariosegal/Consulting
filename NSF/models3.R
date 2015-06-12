#contimued from models2,
#I added more variables to train2
#I will try the multi model from scratch again

multi1a <- rpart(flag_delta ~ .,data=train2,method='class')
measures1(train2$flag_delta,predict(multi1a,type='class'))
x <- (unlist(measures1(train2$flag_delta,predict(multi1a,type='class'))))
write.table(x,'clipboard',sep='\t',row.names=T)
fancyRpartPlot(multi1a,branch.lwd=4)

multi2a <- rpart(flag_delta ~ . -months_9m_2013,data=train2,method='class')
measures1(train2$flag_delta,predict(multi2a,type='class'))
write.table(measures1(train2$flag_delta,predict(multi2a,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(multi2a,branch.lwd=4)


multi3a <- rpart(flag_delta ~ . -months_9m_2013 - fees_9m_2013,data=train2,method='class')
measures1(train2$flag_delta,predict(multi3a,type='class'))
write.table(measures1(train2$flag_delta,predict(multi3a,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
fancyRpartPlot(multi3a,branch.lwd=4)


multi4a <- rpart(flag_delta ~ . -months_9m_2013 - fees_9m_2013,data=train2,method='class',cp=0.0)
measures1(train2$flag_delta,predict(multi4a,type='class'))
write.table(measures1(train2$flag_delta,predict(multi4a,type='class')),'clipboard',sep='\t',row.names=F,col.names=F)
dim(multi4a$frame)[1]
fancyRpartPlot(multi4a,branch.lwd=4)

cp_grid1 <- expand.grid(.cp=seq(0.001,0.1,by=0.001))
multi4acv <- train(flag_delta~. -months_9m_2013 - fees_9m_2013,train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid1)
multi4acv
fancyRpartPlot(multi4acv$finalModel,branch.lwd=4,branch.col='black')
measures1(train2$flag_delta,predict(multi4acv$finalModel,type='class'))
write.table(measures1(train2$flag_delta,predict(multi4acv$finalModel,type='class')),'clipboard',row.names=F,col.names=F,sep='\t')
save_png <- function(model,name,...) {
  png(paste0(name,'.png'),width=9,height=5,units='in', res=600)
    fancyRpartPlot(model,branch.lwd=4,branch.col='black',...)
  dev.off()
}
save_png(multi4acv$finalModel,'multi_class_model4cv')

multi5acv <- train(flag_delta~. ,train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid1)
multi5acv
measures1(train2$flag_delta,predict(multi5acv$finalModel,type='class'))
fancyRpartPlot(multi5acv$finalModel,branch.lwd=4,branch.col='black')

multi6acv <- train(flag_delta~ . -months_9m_2013 - fees_9m_2013 -group_13,train2,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid1)
multi6acv
multi6acv$finalModel
measures1(train2$flag_delta,predict(multi6acv$finalModel,type='class'))
fancyRpartPlot(multi6acv$finalModel,branch.lwd=4,branch.col='black',split.cex=3,ycompress=T)
png('multi_class_model6cv.png',width=11,height=8.5,units="in",res=600)
  my_fancy(multi6acv$finalModel,branch.lwd=4,branch.col='black',ycompress=F,extra1=8,branch.lty1=1,tweak=1.4,xflip=T)
dev.off()
write.table(measures1(train2$flag_delta,predict(multi6acv$finalModel,type='class')),'clipboard',row.names=F,col.names=F,sep='\t')


save.image('models.rdata')
