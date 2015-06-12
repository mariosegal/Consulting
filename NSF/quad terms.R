
#what if I can improve the closure model by using quad terms

train4x <- train4[-97]
numeric_vars <- which(sapply(train4[-97],is.numeric))
train4x <- sapply(train4[,numeric_vars], function(x) x^2)
train4x <- as.data.frame(train4x)
same <- which(sapply(names(train4x),function(x) sum(train4[,x]!=train4x[,x])!=0))

train4x <- train4x[,same]
names(train4x) <- paste0(names(train4x),'_quad')

train4x1 <- cbind(train4,train4x)

test <- rpart(flag_closed1~ .,data=train4x1,method='class',parms=list(split='gini',loss=penalty3),subset=active_201406==1)
test
measures2(test,train4x1$flag_closed1[train4x1$active_201406==1],type='class',newdata=train4x1[train4x1$active_201406==1,])


#quad terms did not do anything, they did not enter the model at all

