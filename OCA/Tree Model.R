library(rpart)
library(rpart.plot)
library(rattle)


#prepare data
load("Z:/M&T Projects/OCA/dda2.rdata")
load('Z:/M&T Projects/OCA/hhlds_201412.rdata')

drop <- c(2,4,6,7,8,9,10,27,28,29,36:66)
hhlds_201412$rm <- ifelse(substring(as.character(hhlds_201412$HHLD_HH_OWNER_CODE),4)!='00',"Mgd","Non")

modeling <- inner_join(dda2[,-drop],hhlds_201412[c("HHLD_ID","rm",'HHLD_LIFE_CYCLE_SEGMENT','HHLD_COMMUNITY_BANK_MARKET')],by=c('ACCT_ID'='HHLD_ID'))

modeling <- inner_join(modeling,penet,by='ACCT_ID')
                  
modeling <- inner_join(modeling,bals,by='ACCT_ID')

modeling[52:70][is.na(modeling[52:70])] <- 0
save(modeling,file='modeling_data.rdata')

#####################################################
load('modeling_data.rdata')

other <- subset(modeling, !(nsf_fee %in% c('Drop','Yes')),-c(1))
yes <- subset(modeling, (nsf_fee %in% c('Yes')),-c(1))

set.seed(12769)
#I will need to oversample, targeting 50% of the Yes
index <- sample(1:dim(yes)[1],ceiling(dim(yes)[1]/2))
index1 <- sample(1:dim(other)[1],ceiling(dim(yes)[1]/2))

train <- rbind(other[index1,],yes[index,])
test <- rbind(other[-index1,],yes[-index,])
intersect(train$EXPRESSION_8,test$EXPRESSION_8)  #has to be zero



train$nsf_fee <- as.factor(as.character(train$nsf_fee))
tree1 <- rpart(nsf_fee~.,train)


#train performance
m = prop.table(table(train$nsf_fee,predict(tree1,type='class'),dnn=c('Actual','Predicted')))
sum(diag(m))
m[1,3]+m[2,3]  #false positive
m[3,1]+m[3,2] #false negative

#plot(tree1);text(tree1,cex=1)
fancyRpartPlot(tree1)

#validate performance
test$nsf_fee <- as.factor(as.character(test$nsf_fee))
m1 = prop.table(table(test$nsf_fee,predict(tree1,test,type='class'),dnn=c('Actual','Predicted')))
sum(diag(m1))
m1[1,3]+m1[2,3]  #false positive
m1[3,1]+m1[3,2] #false negative



