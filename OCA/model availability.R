library(rpart)
library(rpart.plot)
library(rattle)


#prepare data
load("Z:/M&T Projects/OCA/dda2_availability.rdata")
load('Z:/M&T Projects/OCA/hhlds_201412.rdata')
load("penet_bals.rdata")
load('names_modeling.rdata')

hhlds_201412$rm <- ifelse(substring(as.character(hhlds_201412$HHLD_HH_OWNER_CODE),4)!='00',"Mgd","Non")

modeling <- inner_join(dda2,hhlds_201412[c("HHLD_ID","rm",'HHLD_LIFE_CYCLE_SEGMENT','HHLD_COMMUNITY_BANK_MARKET')],by=c('ACCT_ID'='HHLD_ID'))

modeling <- inner_join(modeling,penet,by='ACCT_ID')

modeling <- inner_join(modeling,bals,by='ACCT_ID')

names2 <- c(names1,'availability')
modeling <- modeling[,which(names(modeling) %in% names2)]

modeling[49:66][is.na(modeling[49:66])] <- 0
save(modeling,file='modeling_data_availability.rdata')

#####################################################


other <- subset(modeling, !(availability %in% c('Drop','Yes')),-c(1))
yes <- subset(modeling, (availability %in% c('Yes')),-c(1))

set.seed(12769)
#I will need to oversample, targeting 50% of the Yes
index <- sample(1:dim(yes)[1],ceiling(dim(yes)[1]/2))
index1 <- sample(1:dim(other)[1],ceiling(dim(yes)[1]/2))

train <- rbind(other[index1,],yes[index,])
test <- rbind(other[-index1,],yes[-index,])
intersect(train$EXPRESSION_8,test$EXPRESSION_8)  #has to be zero



train$availability <- as.factor(as.character(train$availability))
tree1 <- rpart(availability~.,train)


#train performance
m = prop.table(table(train$availability,predict(tree1,type='class'),dnn=c('Actual','Predicted')))
sum(diag(m))
m[1,3]+m[2,3]  #false positive
m[3,1]+m[3,2] #false negative


#validate performance
test$availability <- as.factor(as.character(test$availability))
m1 = prop.table(table(test$availability,predict(tree1,test,type='class'),dnn=c('Actual','Predicted')))
sum(diag(m1))
m1[1,3]+m1[2,3]  #false positive
m1[3,1]+m1[3,2] #false negative


fancyRpartPlot(tree1)
