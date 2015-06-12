#Nishith wants to do one model for people who had NSF and then no
# and one for those who did less, while having

#for that first I will need to append data to base_new1
#since we wantg to predict sort of lets use data from 201304 the beginning of the analysis
#I think the data is is on existing book

#load some data 
load('nsf_image_2013Q2_on_20150320.rdata')
load('existing_book.rdata')
load('accts_201304new.rdata')
load("Z:/M&T Projects/OCA/ifm_income_201312.rdata")
load('Z:/M&T Projects/NSF/hhkey.rdata')

#load libraries
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

#crunch the product penetrationa sd create flags and bals for sec, deposits, loans, other dep, etc.
accts_201304new$ACCT_PTYPE <- as.character(accts_201304new$ACCT_PTYPE)
prods_aux <- accts_201304new %>% filter(!(ACCT_PTYPE %in% c("CLN","DEB","HBK","SLN","TRS","WEB"))) %>% 
  mutate(ACCT_PTYPE=ifelse(ACCT_STYPE %in% c('REW','NOR','SIG'),'CRD',ACCT_PTYPE)) %>%
  group_by(ACCT_ID,ACCT_PTYPE) %>% 
  summarise(penet=n_distinct(ACCT_PTYPE),bal=sum(ACCT_AMT_BAL_FOR_PRIME+ACCT_MARKET_VALUE)) %>%
  gather(measure,value,penet:bal) %>% unite(key,ACCT_PTYPE,measure) %>% spread(key,value) 

#make the NAs for penet be zero, the balances remain as NA
prods_aux[grepl('penet',names(prods_aux))][is.na(prods_aux[grepl('penet',names(prods_aux))])] <- 0
prods_aux$prods <- rowSums(prods_aux[grepl('penet',names(prods_aux))])
prods_aux$oth_dep <- ifelse(prods_aux$MMS_penet | prods_aux$SAV_penet | prods_aux$IRA_penet | prods_aux$TDA_penet,1,0)
prods_aux$loans <- ifelse(prods_aux$CRD_penet | prods_aux$CCS_penet | prods_aux$HEQ_penet | prods_aux$MTG_penet | prods_aux$ILN_penet,1,0)
prods_aux$re_loans <- ifelse(prods_aux$HEQ_penet | prods_aux$MTG_penet ,1,0)
prods_aux$mtb_sec <- ifelse(prods_aux$SEC_penet | prods_aux$INS_penet ,1,0)
prods_aux$dep_bal <- rowSums(prods_aux[c('DDA_bal','MMS_bal','TDA_bal','IRA_bal','SAV_bal')],na.rm=T)
prods_aux$oth_dep_bal <- rowSums(prods_aux[c('MMS_bal','TDA_bal','IRA_bal','SAV_bal')],na.rm=T)
prods_aux$oth_dep_bal[prods_aux$oth_dep==0] <- NA
prods_aux$dep_bal[prods_aux$oth_dep==0 & prods_aux$DDA_penet==0] <- NA


#start assembling a modeling set
model_data <- subset(base_new1,present_9m_2013==1)

#append the acct_ID
length(setdiff(model_data$EXPRESSION_8,hhkey$EXPRESSION_8))   #only 267 are missing, who knows why, not worth worrying about 267 out of 2MM
model_data <- inner_join(model_data,hhkey[c('EXPRESSION_8','ACCT_ID')])
rm(hhkey)

#create the flag for no more NSF
#1 if it was present in both periods and with went from 0 to 1, o if it it had in both periods, there wil be NAs to deal with others

model_data$flag_stopped_nsf <- NA 
model_data$flag_stopped_nsf[model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & 
                              model_data$with_9m_2014a==0 & model_data$with_9m_2013a==1] <- 1
model_data$flag_stopped_nsf[model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & 
                              model_data$with_9m_2014a==1 & model_data$with_9m_2013a==1] <- 0

#append the ownership data and ifm income
model_data <- left_join(model_data,prods_aux)
model_data <- left_join(model_data,ifm_income_201312,by=c('EXPRESSION_8'='EXPRESSION_1'))

rm(prods_aux)
rm(ifm_income_201312)

#append the hhld demographics and such
load("Z:/M&T Projects/NSF/hhld_201301.rdata")
load("Z:/M&T Projects/NSF/hhld_201312.rdata")
load("Z:/M&T Projects/NSF/hhld_201304.rdata")

hhld_aux <- bind_rows(hhld_201301,hhld_201312[!(hhld_201312$HHLD_ID %in% hhld_201301$HHLD_ID),])
hhld_aux <- bind_rows(hhld_aux,hhld_201304[!(hhld_201304$HHLD_ID %in% hhld_aux$HHLD_ID),])
length(setdiff(model_data$ACCT_ID,hhld_aux$HHLD_ID))  #all but 4,907 were found

# append hhld data
model_data <- left_join(model_data,hhld_aux[c(1,15,19,49,54,68,69,72)],by=c('ACCT_ID'='HHLD_ID'))

#create the factors for cbr, segment, convert tenure to years,

model_data$segment = model_data$HHLD_LIFE_CYCLE_SEGMENT
model_data$segment <- as.character(model_data$segment )
model_data$segment[model_data$segment==8] <- 1
model_data$segment[model_data$segment==9] <- 4
model_data$segment[is.na(model_data$segment)] <- 7
model_data$segment <- factor(model_data$segment,
                              levels=c(1:7),
                              labels=c('BTF','MANK','MNF','MAF','MNR',"MAR",'NC'))

model_data$cbr <- model_data$HHLD_COMMUNITY_BANK_MARKET
model_data$cbr[is.na(model_data$cbr)] <- 99
model_data$cbr <- factor(model_data$cbr,levels=c(1:17,99),
                          labels=c('WNY','Roch','Syr','Southern','Albany','Tarry','NYC','Philly','PA N','C&W PA',
                                   'SEPA','Balt','Ches A','Wash','Ches B','C. VA','DE','OOM'))


model_data$hh_tenure <- model_data$HHLD_TENURE_DAYS/365
model_data$dda_tenure <- as.numeric((as.Date('2013-06-01')- model_data$ACCT_DATE_OPENED_FOR_PRIME)/365)

rm(hhld_aux,hhld_201301,hhld_201304,hhld_201312)

#add ixi data
load('Z:/M&T Projects/NSF/ixi_201304.rdata')

model_data <- left_join(model_data,ixi_201304[1:2],by=c('ACCT_ID'='IX-ID'))
names(model_data)[77] = 'ixi_assets'


#add cqi and opt-in
load('cqi_201403.rdata')
model_data <- left_join(model_data,cqi_201304)
rm(cqi_201304)

#do all the other flags as well as well as a multi flag, but also do inactivity per Nishit's request

with(model_data[model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & model_data$with_9m_2014a==1 & model_data$with_9m_2013a==1 & model_data$book=='back',],table(cut(events_9m_2013-events_9m_2014,c(-Inf,seq(-10,10,1),Inf),dig.labs=10)))

model_data$flag_delta <- NA 
model_data$flag_delta[ model_data$book=='back' & model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & is.na(model_data$closed) & model_data$with_9m_2013a==1 & model_data$with_9m_2014a==1 & model_data$events_9m_2014 < model_data$events_9m_2013] <- 'less'

model_data$flag_delta[ model_data$book=='back' & model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & is.na(model_data$closed) & model_data$with_9m_2013a==1 & model_data$with_9m_2014a==1 & model_data$events_9m_2014 >= model_data$events_9m_2013] <- 'same_or_more'

model_data$flag_delta[ model_data$book=='back' & model_data$present_9m_2013==1 & model_data$present_9m_2014==1 & is.na(model_data$closed) & model_data$with_9m_2013a==1 & model_data$with_9m_2014a==0 ] <- 'stopped'


load("Z:/M&T Projects/NSF/activity1.rdata")
active_aux <- activity1 %>% mutate(active=ifelse(ACCT_NUMBER_DEBITS_MTD>=5 & ACCT_NUMBER_OF_DEPOSITS_MTD>=1,1,0)) %>%
  group_by(EXPRESSION_8,year=(substr(period,1,4))) %>% summarise(active=sum(active)) %>% spread(year,active) 

names(active_aux)[2:3] <- paste0("active_",names(active_aux)[2:3])

model_data <- left_join(model_data,active_aux)

rm(activity1);rm(active_aux)
#inactives 
model_data$flag_delta[model_data$present_9m_2013==1  & model_data$present_9m_2014==1 & model_data$book=='back' & model_data$with_9m_2013a >0 & model_data$with_9m_2014a==0 & model_data$active_2013>0 & model_data$active_2014==0 & is.na(model_data$closed)] <- 'closed_inactive'
#closed
model_data$flag_delta[model_data$present_9m_2013==1  & model_data$book=='back' & model_data$with_9m_2013a==1 & (!is.na(model_data$closed))] <- 'closed_inactive'

model_data$flag_delta<- as.factor(model_data$flag_delta)


#now we are ready to start the models
#but I want a smaller set of only those people in front book, that are present in 201304 and present in all of 2014 too

model_data$model_flag1 = 0

model_data$model_flag1 = ifelse(model_data$book=='back' & is.na(model_data$closed) & !is.na(model_data$flag_stopped_nsf),1,0)

#define amodel_flag for the multi class model
model_data$model_flag2 = 0
model_data$model_flag2 <- ifelse(!is.na(model_data$flag_delta),1,0)

#add ifm data and consumer demographics and debit trans
load("Z:/M&T Projects/NSF/condem_201304.rdata")
model_data <- left_join(model_data,condem_201304[-13],by=c('ACCT_ID'='con_id'))
rm(condem_201304)

load("Z:/M&T Projects/NSF/ifm_201304.rdata")
ifm_aux <- subset(ifm_201304,,c('EXPRESSION_1','INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH','INTELLIGENTSIA_PRIMARY_BANK_INDICATOR','INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO','INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO','INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME','INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO','INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO','INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT','INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT','INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME'))
library(stringr)
ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME <- str_trim(ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME)
ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME <- str_trim(ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME)
ifm_aux$ext_bank<- ifelse(ifm_aux$INTELLIGENTSIA_PRIMARY_BANK_TRANS_REL_NAME != '',1,0)
ifm_aux$ext_sec <- ifelse(ifm_aux$INTELLIGENTSIA_PRIMARY_INVEST_REL_NAME != '',1,0)

model_data <- left_join(model_data,ifm_aux,by=c('EXPRESSION_8'='EXPRESSION_1'))
rm(ifm_201304,ifm_aux)

#add activity
load("Z:/M&T Projects/NSF/activity1.rdata")
model_data <- left_join(model_data,subset(activity1,period == '201304'))
rm(activity1)

load("Z:/M&T Projects/NSF/debit_all.rdata")
names(debit_all)[5:6] <- paste0('debit_card_',names(debit_all)[5:6])
debit_aux <- debit_all %>% filter(period=='201304') %>% group_by(dda) %>% 
  summarise_each(funs(sum),c(debit_card_num,debit_card_amt))
                 
model_data <- left_join(model_data,debit_aux,by=c('EXPRESSION_8'='dda'))
rm(debit_all,debit_aux)



#I also feel like adding 3m of debit

debit_aux1 <- debit_all %>% filter(period %in% c('201304','201305','201306')) %>% group_by(dda) %>% 
  summarise(debit_card_num_3m=sum(debit_card_num),debit_card_amt_3m=sum(debit_card_amt))

model_data <- left_join(model_data,debit_aux1,by=c('EXPRESSION_8'='dda'))
rm(debit_all,debit_aux1)

#here I moved to the tab addtl_dModel_ata_processing

#define vars
variables <- c(31,7,8,10,14,22,28,32:65,67,70:77,79:84)

data_small <- subset(model_data,model_flag1==1,variables)
data_small$flag_stopped_nsf <- as.factor(data_small$flag_stopped_nsf)
sum(duplicated(data_small))  #run iteratively until 0
data_small <- data_small[-which(duplicated(data_small)),]  #run iteratively

data_small <- data_small[-is.na(data_small$stype),]

#if NAs on penet make 0
data_small[grepl('penet',names(data_small))][is.na(data_small[grepl('penet',names(data_small))])] <- 0
#if NAs on bals make 0
data_small[grepl('bal',names(data_small))][is.na(data_small[grepl('bal',names(data_small))])] <- 0




#all characters need to be factors or random forest dies
which(sapply(data_small,is.character))
data_small$group_13 <- as.factor(data_small$group_13)

#for rest omit na
data_small_clean <- na.omit(data_small)

 library(rpart); library(caret); library(rpart.plot); library(rattle)

set.seed(1969)
intrain <- createDataPartition(data_small_clean$flag_stopped_nsf,p=0.1,list=F)
train <- data_small_clean[intrain,]
test <- data_small_clean[-intrain,]
dim(intersect(train,test))[1]
prop.table(table(data_small_clean$flag_stopped_nsf))
prop.table(table(train$flag_stopped_nsf))

#I will define my own function to do measures, because I do not undertsand caret's to be honest
measures <- function(actual,pred) {
  t = table(actual,pred)
  acc = sum(diag(t))/sum(t)
  sen = prop.table(t,1)[2,2]
  spec = prop.table(t,1)[1,1]
  f1 = 2*t[2,2]/(2*t[2,2]+t[1,2]+t[2,1])
  return(data.frame(accuracy=acc,sensitivity=sen,specificity=spec,F1_score=f1))
}


#baseline accuracy is to guess 0
baseline_acc = mean(train$flag_stopped_nsf==0)
baseline_sens = 0
baseline_spec = 1

tree1 <- train(flag_stopped_nsf~.,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree1_final <- tree1$finalModel
fancyRpartPlot(tree1$finalModel)
confusionMatrix(tree1)
sum(diag(confusionMatrix(tree1)$table))
sensitivity(confusionMatrix(tree1)$table,positive="1")
specificity(confusionMatrix(tree1)$table,positive='0')

#after I added the CQI and such I ran 1 again
tree1x <- train(flag_stopped_nsf~.,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree1x_final <- tree1x$finalModel
sum(diag(confusionMatrix(tree1x)$table))
fancyRpartPlot(tree1x$finalModel)
confusionMatrix(tree1x)
sensitivity(confusionMatrix(tree1x)$table)
specificity(confusionMatrix(tree1x)$table)

#very simplistic
tree2 <- train(flag_stopped_nsf~. - months_9m_2013,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree2_final <- tree2$finalModel
fancyRpartPlot(tree2_final)
confusionMatrix(tree2)
sum(diag(confusionMatrix(tree2)$table))
sensitivity(confusionMatrix(tree2)$table)
specificity(confusionMatrix(tree2)$table)


tree2x <- train(flag_stopped_nsf~. - months_9m_2013,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree2x_final <- tree2x$finalModel
fancyRpartPlot(tree2x_final)

measures(train$flag_stopped_nsf,predict(tree2x,newdata=train))

#take the groups and the fees, they are related
tree3 <- train(flag_stopped_nsf~. - months_9m_2013 - fees_9m_2013 -group_13,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree3_final <- tree3$finalModel
fancyRpartPlot(tree3_final)
confusionMatrix(tree3)

measures(train$flag_stopped_nsf,predict(tree3,newdata=train))

#after adding cqi and opt in
tree4 <- train(flag_stopped_nsf~. - months_9m_2013 - fees_9m_2013 -group_13,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"))
tree4_final <- tree4$finalModel
fancyRpartPlot(tree4_final)
confusionMatrix(tree4)

measures(train$flag_stopped_nsf,predict(tree4,newdata=train))


#do a full model cp=0.0 with all
tree5 <- train(flag_stopped_nsf~.,data=train,method='rpart',trControl = trainControl(method = "repeatedcv"),cp=0.0)
measures(train$flag_stopped_nsf,predict(tree5,newdata=train))

drop <- which(names(train) %in% c('months_9m_2013','fees_9m_2013','group_13'))
tree4a <- rpart(flag_stopped_nsf~. ,data=train[,-drop],cp=0.003610108)

mean(predict(tree4a,type='class')==train$flag_stopped_nsf)
table(train$flag_stopped_nsf,predict(tree4a,type='class'),dnn=c('actual','pred'))
3657/(3657+5484)
fancyRpartPlot(tree4a)
measures(train$flag_stopped_nsf,predict(tree4a,newdata=train,type='class'))

tree4b<- rpart(flag_stopped_nsf~. ,data=train[,-drop],minbucket=25)
fancyRpartPlot(tree4b)
measures(train$flag_stopped_nsf,predict(tree4b,newdata=train,type='class'))


#The models are quite bad at predicting who will stop
#I will try a random forest
rf1 <-  train(flag_stopped_nsf~. ,data=train,method='rf',trControl = trainControl(method = "repeatedcv"))
rf1
varImp(rf1)
varImpPlot(rf1)
measures(train$flag_stopped_nsf,predict(rf1,newdata=train,type='response'))




#generate PNGs
png('tree1.png',width=9,height=5.5,units='in',res=300)
fancyRpartPlot(tree1_final,main='Model with All Variables')
dev.off()

png('tree2.png',width=9,height=5.5,units='in',res=300)
fancyRpartPlot(tree2_final,main='Model without Months')
dev.off()

png('tree3.png',width=9,height=5.5,units='in',res=300)
fancyRpartPlot(tree3_final,main='Model without NSf Information')
dev.off()

png('tree4.png',width=9,height=5.5,units='in',res=300)
fancyRpartPlot(tree4_final,main='Model with Addtl Vars')
dev.off()

png('tree5.png',width=9,height=5.5,units='in',res=300)
fancyRpartPlot(tree5$finalModel,main='Largest Model')
dev.off()

save.image('models.rdata')



#do a multi class model


variables2 <- c(7,8,10,14,22,28,32:65,67,70:77,79:84,85,89:97,100:103,105:108,110,111,112:113,115:116)
data_small2 <- subset(model_data,model_flag2==1,variables2)

data_small2 <- data_small2[-is.na(data_small2$stype),]

#if NAs on penet make 0
data_small2[grepl('penet',names(data_small2))][is.na(data_small2[grepl('penet',names(data_small2))])] <- 0
#if NAs on bals make 0
data_small2[grepl('bal',names(data_small2))][is.na(data_small2[grepl('bal',names(data_small2))])] <- 0

#all characters need to be factors or random forest dies
which(sapply(data_small2,is.character))
data_small2$group_13 <- as.factor(data_small2$group_13)

#for the ifm and the debit variables NA means zero, also condem are factors
data_small2[,57:58][is.na(data_small2[,57:58])] <-99
data_small2$con_dem_education_code <- as.factor(data_small2$con_dem_education_code)
data_small2$con_dem_estimated_income<- as.factor(data_small2$con_dem_estimated_income)

#the other factors in condem have " " and I rather make them explicit "blank"
levels(data_small2$con_dem_home_owner_renter)[levels(data_small2$con_dem_home_owner_renter)==" "] <- "blank"
levels(data_small2$con_dem_income_producing_assets_cd)[levels(data_small2$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(data_small2$con_dem_marital_status)[levels(data_small2$con_dem_marital_status)==" "] <- "blank"
levels(data_small2$con_dem_income_producing_assets_cd)[levels(data_small2$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(data_small2$con_dem_presence_children_0_to_10)[levels(data_small2$con_dem_presence_children_0_to_10)==" "] <- "blank"
levels(data_small2$con_dem_presence_children_11_to_15)[levels(data_small2$con_dem_presence_children_11_to_15)==" "] <- "blank"
levels(data_small2$con_dem_presence_children_16_to_17)[levels(data_small2$con_dem_presence_children_16_to_17)==" "] <- "blank"
levels(data_small2$con_dem_presence_of_children)[levels(data_small2$con_dem_presence_of_children)==" "] <- "blank"

data_small2$debit_card_num[is.na(data_small2$debit_card_num)] <- 0
data_small2$debit_card_amt[is.na(data_small2$debit_card_amt)] <- 0
levels(data_small2$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)[levels(data_small2$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)==" "] <- "blank"
data_small2$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH[is.na(data_small2$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)] <- 'blank'
data_small2$INTELLIGENTSIA_PRIMARY_BANK_INDICATOR[is.na(data_small2$INTELLIGENTSIA_PRIMARY_BANK_INDICATOR)] <- 1 #if no IFM data I am defining as we are the primary bank

data_small2$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO[is.na(data_small2$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO)] <- 0

data_small2$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO[is.na(data_small2$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO)] <- 0

data_small2$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO[is.na(data_small2$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO)] <- 0
data_small2$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO[is.na(data_small2$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO)] <- 0
data_small2$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT[is.na(data_small2$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT)] <- 0
data_small2$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT[is.na(data_small2$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT)] <- 0

data_small2$ext_bank[is.na(data_small2$ext_bank)] <- 0
data_small2$ext_sec[is.na(data_small2$ext_sec)] <- 0

#for rest omit na
data_small_clean2 <- na.omit(data_small2)

set.seed(6971)
intrain <- createDataPartition(data_small_clean2$flag_delta,p=0.1,list=F)
train1 <- data_small_clean2[intrain,]
test1 <- data_small_clean2[-intrain,]

#I need to define my own measures to understand this,
#sensitivity is going tobe the diagonal/total
#I need to claculate a sensitivity per row

measures1 <- function(actual,pred) {
  acc = mean(actual==pred)
  t = table(actual,pred)
  sens = diag(t)/rowSums(t)
  spec=numeric(dim(t)[1])
  prec=diag(t)/colSums(t)
  for (i in 1:dim(t)[1]) {
      tn = sum(t[-i,-i])
      spec[i] = tn/(tn+sum(t[-i,i]))   
  }
  f1 = (2*prec*sens)/(prec+sens)
  out = data.frame(class=names(sens),accuracy=NA,sensitivity=sens,specificity=spec,precision=prec,f1=f1)
  out1=  data.frame(class='all',accuracy=acc,sensitivity=NA,specificity=NA,precision=NA,f1=NA)

  #return(list(accuracy=acc,sensitivity=sens,specificity=spec,precision=prec,f1=f1))
  return(rbind(out1,out))
}

#initial model, rpart, std settings, all vars
multi1 <- rpart(flag_delta ~ .,data=train1,method='class')
measures1(train1$flag_delta,predict(multi1,newdata=train1,type="class"))
fancyRpartPlot(multi1)

multi2 <- rpart(flag_delta ~ .,data=train1,method='class',cp=0)
measures1(train1$flag_delta,predict(multi2,newdata=train1,type="class"))
dim(multi2$splits)


cp_grid <- expand.grid(.cp=seq(0.01,0.5,by=0.01))
multi3 <- train(flag_delta ~ .,data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid)

multi3f <- rpart(flag_delta ~ .,data=train1,method='class',cp=0.16)
measures1(train1$flag_delta,predict(multi3f,newdata=train1,type="class"))
dim(multi3f$frame)
fancyRpartPlot(multi3f)

#let's take out the variables on fees
multi4 <- rpart(flag_delta ~ . -fees_9m_2013 ,data=train1,method='class')
measures1(train1$flag_delta,predict(multi4,newdata=train1,type="class"))
dim(multi4$frame)
fancyRpartPlot(multi4)


#let's take out talso months_9m_2013
multi5 <- rpart(flag_delta ~ . -fees_9m_2013 -months_9m_2013,data=train1,method='class')
measures1(train1$flag_delta,predict(multi5,newdata=train1,type="class"))
dim(multi5$frame)
fancyRpartPlot(multi5)

#let's take out also group_13
multi6 <- rpart(flag_delta ~ .,data=train1[-c(2,3,5)],method='class')
measures1(train1$flag_delta,predict(multi6,newdata=train1,type="class"))
dim(multi6$frame)
fancyRpartPlot(multi6)

multi6b <- rpart(flag_delta ~ .,data=train1[-c(2,3,5)],method='class',cp=0.0)
measures1(train1$flag_delta,predict(multi6b,newdata=train1,type="class"))
dim(multi6b$frame)
fancyRpartPlot(multi6b)

multi6_cv <- train(flag_delta ~ .,data=train1[-c(2,3,5)],method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid)
multi6_cv 

multi6f <- rpart(flag_delta ~ .,data=train1[-c(2,3,5)],method='class',cp=0.01)
measures1(train1$flag_delta,predict(multi6f,newdata=train1,type="class"))
dim(multi6f$frame)
fancyRpartPlot(multi6f)


#as a last resort try a RF to select the vars, then try  that as a tree
multirf <- randomForest(flag_delta ~ .,train1)
multirf
varImpPlot(multirf)

multirf1 <- randomForest(flag_delta ~ .,train1[-c(2,3,5,6)])
multirf1
vars <- varImp(multirf1)
vars$var <- row.names(vars)
vars[order(-vars$Overall),][1:10,]
varImpPlot(multirf1,n.var=10)


multi7 <- rpart(flag_delta ~ cbr + HHLD_CLV_REMAINING  + HHLD_CLV_TOTAL + dep_bal + dda_tenure + DDA_bal + hh_tenure + HHLD_CLV_REMAINING_TENURE + ixi_assets +  HHLD_HH_OWN_AGE,data=train1,method='class',cp=0.0) 
measures1(train1$flag_delta,predict(multi7,newdata=train1,type="class"))
dim(multi7$frame)
fancyRpartPlot(multi7)


multi7_cv <- train(flag_delta ~ cbr + HHLD_CLV_REMAINING  + HHLD_CLV_TOTAL + dep_bal + dda_tenure + DDA_bal + hh_tenure + HHLD_CLV_REMAINING_TENURE + ixi_assets +  HHLD_HH_OWN_AGE,data=train1,method='rpart',trControl = trainControl(method='cv',number=10),tuneGrid = cp_grid)
multi7_cv 

multi7f <- rpart(flag_delta ~ cbr + HHLD_CLV_REMAINING  + HHLD_CLV_TOTAL + dep_bal + dda_tenure + DDA_bal + hh_tenure + HHLD_CLV_REMAINING_TENURE + ixi_assets +  HHLD_HH_OWN_AGE,data=train1,method='class',cp=0.01)
measures1(train1$flag_delta,predict(multi7,newdata=train1,type="class"))
dim(multi7$frame)
fancyRpartPlot(multi7)

#continue on models2