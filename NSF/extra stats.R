labs1 = c('none','1','2','3','4','5','6-20','11-20','21+')

table(cut(model_data_new$events_last6m,c(0,.01,1,2,3,4,5,10,20,Inf),labels=labs1,ordered=T),
      cut(model_data_new$events_future,c(0,.01,1,2,3,4,5,10,20,Inf),labels=labs1,ordered=T),
      dnn=c('Jan to June','Jul to Dec'))