

save(model_data,file='model_data')

#if NAs on penet make 0
model_data[grepl('penet',names(model_data))][is.na(model_data[grepl('penet',names(model_data))])] <- 0
#if NAs on bals make 0
model_data[grepl('bal',names(model_data))][is.na(model_data[grepl('bal',names(model_data))])] <- 0

#all characters need to be factors or random forest dies
model_data$group_13 <- as.factor(model_data$group_13)

#for the ifm and the debit variables NA means zero, also make condem as factors and make other adjustments to them
model_data[,c("con_dem_education_code"  , "con_dem_estimated_income")][is.na(model_data[,c("con_dem_education_code"  , "con_dem_estimated_income")])] <-99
model_data$con_dem_education_code <- as.factor(model_data$con_dem_education_code)
model_data$con_dem_estimated_income<- as.factor(model_data$con_dem_estimated_income)

#the other factors in condem have " " and I rather make them explicit "blank"
levels(model_data$con_dem_home_owner_renter)[levels(model_data$con_dem_home_owner_renter)==" "] <- "blank"
levels(model_data$con_dem_income_producing_assets_cd)[levels(model_data$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(model_data$con_dem_marital_status)[levels(model_data$con_dem_marital_status)==" "] <- "blank"
levels(model_data$con_dem_income_producing_assets_cd)[levels(model_data$con_dem_income_producing_assets_cd)==" "] <- "blank"
levels(model_data$con_dem_presence_children_0_to_10)[levels(model_data$con_dem_presence_children_0_to_10)==" "] <- "blank"
levels(model_data$con_dem_presence_children_11_to_15)[levels(model_data$con_dem_presence_children_11_to_15)==" "] <- "blank"
levels(model_data$con_dem_presence_children_16_to_17)[levels(model_data$con_dem_presence_children_16_to_17)==" "] <- "blank"
levels(model_data$con_dem_presence_of_children)[levels(model_data$con_dem_presence_of_children)==" "] <- "blank"

model_data$debit_card_num[is.na(model_data$debit_card_num)] <- 0
model_data$debit_card_amt[is.na(model_data$debit_card_amt)] <- 0
model_data$debit_card_num_3m[is.na(model_data$debit_card_num_3m)] <- 0
model_data$debit_card_amt_3m[is.na(model_data$debit_card_amt_3m)] <- 0

levels(model_data$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)[levels(model_data$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)==" "] <- "blank"
model_data$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH[is.na(model_data$INTELLIGENTSIA_LOYALTY_GRADE_PRIOR_MONTH)] <- 'blank'
#model_data$INTELLIGENTSIA_PRIMARY_BANK_INDICATOR[is.na(model_data$INTELLIGENTSIA_PRIMARY_BANK_INDICATOR)] <- 1 #if no IFM data I am defining as we are the primary bank, this is a logical guess, plus it makes the variable be neatly 0 to 1

model_data$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO[is.na(model_data$INTELLIGENTSIA_INVEST_CREDIT_TRAILING_12_MO)] <- 0

model_data$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO[is.na(model_data$INTELLIGENTSIA_INVEST_DEBIT_TRAILING_12_MO)] <- 0

model_data$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO[is.na(model_data$INTELLIGENTSIA_BANK_TRANS_DEBIT_TRAIL_12MO)] <- 0
model_data$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO[is.na(model_data$INTELLIGENTSIA_BANK_TRANS_CREDIT_TRAIL_12MO)] <- 0
model_data$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT[is.na(model_data$INTELLIGENTSIA_BANK_TRANS_CREDIT_COUNT)] <- 0
model_data$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT[is.na(model_data$INTELLIGENTSIA_BANK_TRANS_DEBIT_COUNT)] <- 0

model_data$ext_bank[is.na(model_data$ext_bank)] <- 0
model_data$ext_sec[is.na(model_data$ext_sec)] <- 0

#let's define the falgs i will need and the waterfalls

#model for less inclusing zero
sum(model_data$book!="exclude")
sum(model_data$book=='back')
sum(model_data$book=='front')

sum(model_data$book=='back' & model_data$events_9m_2013==0)
sum(model_data$book=='back' & model_data$events_9m_2013>0)

sum(model_data$book=='back' & model_data$events_9m_2013>0 & !is.na(model_data$closed))
sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed))

sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
      (model_data$active_2013==0 | is.na(model_data$active_2013)))

sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
      (model_data$active_2014==0 | is.na(model_data$active_2014)))

sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
      (model_data$active_2014>0 & !is.na(model_data$active_2014)))sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
      (model_data$active_2014==0 | is.na(model_data$active_2014)) &
      !is.na(model_data$closed) )

sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
      (model_data$active_2014>0 & !is.na(model_data$active_2014)) & 
      model_data$events_9m_2013>model_data$events_9m_2014)

sum(model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
      (model_data$active_2014>0 & !is.na(model_data$active_2014)) & 
      model_data$events_9m_2013<=model_data$events_9m_2014)



model_data$flag_less_new <- NA
model_data$flag_less_new[model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
                           (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
                           (model_data$active_2014>0 & !is.na(model_data$active_2014)) & 
                           model_data$events_9m_2013>model_data$events_9m_2014] <- "less"

model_data$flag_less_new[model_data$book=='back' & model_data$events_9m_2013>0 & is.na(model_data$closed) & 
                           (model_data$active_2013>0 & !is.na(model_data$active_2013)) & 
                           (model_data$active_2014>0 & !is.na(model_data$active_2014)) & 
                           model_data$events_9m_2013<=model_data$events_9m_2014] <- "same_or_more"

table(model_data$flag_less_new,useNA='ifany')

#Now define the flag for closed or inactive
#the first few levels are identical, we only want back book and they had tohad an NSF in 2013
#and active in 2013
#the order ia a bit different though

sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013==0 | is.na(model_data$active_2013)))

sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)))

sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
      (model_data$active_2014>0 & !is.na(model_data$active_2014) & is.na(model_data$closed)))   #active  2014and not closed

sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed)))


#now count how many of the remainder closed, and how many are open but inactive
sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
    & !is.na(model_data$closed))

sum(model_data$book=='back' & model_data$events_9m_2013>0 &
      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
    & is.na(model_data$closed))

#I am not convinced of the last one so I need to validate that
#for that define some temporray flags


model_data$flag_closed_inactive_new <- NA
model_data$flag_closed_inactive_new[model_data$book=='back' & model_data$events_9m_2013>0 &
                                      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
                                      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
                                    & !is.na(model_data$closed)] <- "closed"

model_data$flag_closed_inactive_new[model_data$book=='back' & model_data$events_9m_2013>0 &
                                      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
                                      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
                                    & is.na(model_data$closed)] <- "inactive"

table(model_data$flag_closed_inactive_new,useNA='ifany')

table(model_data$active_2014,!is.na(model_data$closed),model_data$flag_closed_inactive_new,useNA='ifany')
#works like a charm
#define the flags now
model_data$flag_closed_inactive_new <- NA
model_data$flag_closed_inactive_new[model_data$book=='back' & model_data$events_9m_2013>0 &
                                      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
                                      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
                                    & !is.na(model_data$closed)] <- "closed_inactive"

model_data$flag_closed_inactive_new[model_data$book=='back' & model_data$events_9m_2013>0 &
                                      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
                                      (model_data$active_2014==0 | is.na(model_data$active_2014) | !is.na(model_data$closed))
                                    & is.na(model_data$closed)] <- "closed_inactive"

model_data$flag_closed_inactive_new[model_data$book=='back' & model_data$events_9m_2013>0 &
                                      (model_data$active_2013>0 & !is.na(model_data$active_2013)) &
                                      (model_data$active_2014>0 & !is.na(model_data$active_2014) & is.na(model_data$closed))] <- "active"

table(model_data$flag_closed_inactive_new,useNA='ifany')


#check to see if there is overlap, they have to as the active are the first group by definition
table(model_data$flag_closed_inactive_new,model_data$flag_less_new,useNA='ifany')


#there are records with stype ==NA
model_data$stype[model_data$EXPRESSION_8==14069407]  = 'Classic Checking'




save(model_data,file='model_data.rdata')



