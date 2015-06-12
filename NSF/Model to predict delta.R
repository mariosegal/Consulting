#after talking to Phil he wants a better model to predict those that wil decrease the most

#I will hence work on a regression model to predict the actual decrease,  it is a continuous variable that can be predicted

load("Z:/M&T Projects/NSF/model_data_new2.rdata")

drop <- which(names(model_data_new2) %in% c('active_201412','events_future','flag_less','flag_less1','flag_closed1','flag_closed','delta2','delta3','closed_period','EXPRESSION_8','ACCT_STYPE','ACCT_DATE_OPENED_FOR_PRIME','book','ACCT_ID'))

#create the target flag
model_data_new2$nsf_delta <- model_data_new2$events_future - model_data_new2$events_last6m

#the NAs in the data create issues (of course it is regression)
#rather than exclude data, i will try first to exclude the variables with NAs,  they are all appended data (like Clv and ixi and demog)
drop2 <- which(sapply(model_data_new2,function(x) sum(is.na(x))>0))

levels(model_data_new2$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)[1] <- 'blank'

#load libraries
library(caret)

#I will use caret, specifically stepAIC as it does all the work for you

#no need to predict for accts that are inactive to begin with, or that had no NSF (what does a decrease mean?)
#despite what Nishith said, you can certainly apply the model to only active with NSF, you can observe such behavior, the re is no ruls I know that say that you have to apply the model to all people
index1 = model_data_new2$active_201406==1 & model_data_new2$events_last6m>0
sum(index1)

set.seed(1234)
split1 = createDataPartition(model_data_new2$nsf_delta[index1],p=0.5,list=F)
train <- model_data_new2[index1,-c(drop,drop2)][split1,]
test <- model_data_new2[index1,-c(drop,drop2)][-split1,]



change1 <- train(nsf_delta ~ . , data=train,method='lmStepAIC',direction='forward')
summary(change1)
plot(train$nsf_delta,predict(change1,train))
abline(a=0,b=1,col='red')

#This is not a good model R2 = 0.092

ctrl1 = trainControl(method='cv',number=10)
change2 <- train(nsf_delta ~ . , data=train,method='lmStepAIC',direction='forward',trControl = ctrl1)
change2$results
#the cv improved only marginally to 0.099

#I am going to go back to predict into bands, we have more data now, and I am also going to use better techniques than simpel CART

model_data_new2$flag_decrease <- NA
model_data_new2$flag_decrease <- ifelse(model_data_new2$nsf_delta %in% 1:3,'1 to 3',model_data_new2$flag_decrease)
model_data_new2$flag_decrease <- ifelse(model_data_new2$nsf_delta %in% 4:6,'4 to 6',model_data_new2$flag_decrease)
model_data_new2$flag_decrease <- ifelse(model_data_new2$nsf_delta %in% 7:10,'7 to 10',model_data_new2$flag_decrease)
model_data_new2$flag_decrease <- ifelse(model_data_new2$nsf_delta >10 ,'11+',model_data_new2$flag_decrease)
model_data_new2$flag_decrease <- ifelse(model_data_new2$nsf_delta <1,'Same or More',model_data_new2$flag_decrease)

#I thin on the resampling we get a situation where we get 0 of one class, lets try with aa simple predict 34 and not 4+
model_data_new2$flag4plus = ifelse(model_data_new2$nsf_delta >= 4,'Y',"N")

drop3 <- which(names(model_data_new2) %in% c('active_201412','events_future','flag_less','flag_less1','flag_closed1','flag_closed','delta2','delta3','closed_period','EXPRESSION_8','ACCT_STYPE','ACCT_DATE_OPENED_FOR_PRIME','book','ACCT_ID','active_201406','nsf_delta','flag_decrease'))

train1 <- model_data_new2[index1,-c(drop3,drop2)][split1,]
test1 <- model_data_new2[index1,-c(drop3,drop2)][-split1,]
train1$flag4plus <- as.factor(train1$flag4plus)



#I will try a parallel model (why not) using a random forest
k=10
repeats=1
tune = 5
set.seed(1)
seeds=vector(mode='list',length=k*repeats + 1)
for (i in 1:(k*repeats)) seeds[[i]] = sample.int(1000,k+tune)
seeds[[k*repeats + 1]] <- sample.int(1000,1)

cl <- makeCluster(4,type='PSOCK',outfile=" ")
registerDoParallel(cl)
getDoParWorkers()
set.seed(1)
ctrl2 = trainControl(method='cv',number=k,repeats=repeats,classProbs=T, verboseIter = T,
                     summaryFunction = twoClassSummary,seeds=seeds,
                     index=createMultiFolds(train1$flag4plus,k=k,times=repeats))

decrease1 <- train(flag4plus ~ . , data=train1,method='gbm',distribution='bernoulli',
                   tuneLength = tune,trControl = ctrl2,metric="ROC")
registerDoSEQ()

save(decrease1,file='decrease1.rdata')

#how goood is this model on test
confusionMatrix(predict(decrease1,newdata=test1),test1$flag4plus,positive='Y')
#quite bad it only predicts like 50% of the Yes

set.seed(345)
decrease2 <- train(flag4plus ~ . , data=train1,method='rf',
                   tuneLength = tune,trControl = ctrl2,metric="ROC")

save(decrease2,file='decrease2.rdata')
