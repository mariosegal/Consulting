library(dplyr)
library(tidyr)

#create some dummy data like what we will get
test <- data_frame(acct=1:12,open=c(201401,201402,201403,201404,201405,201406,201407,201408,201409,201410,201411,201412))
test$closed <- NA
test$closed[c(1,5,7)] <- c(201412,201406,201412)
test$aux <- 1

#create the wide
test1 <- test %>% spread(open,aux)
test2 <- inner_join(test[1:2],test1,by='acct')

aux <- as.numeric(names(test2[-c(1:3)]))
aux >= test2$open[1] & aux <test2$closed[1]

#this line assigns the 1s when there is a closed date
test2[1,-c(1:3)]<- (aux >= test2$open[1] & aux <test2$closed[1])*1
#this line assigns it when there is no closed date
test2[2,-c(1:3)]<- (aux >= test2$open[2] )*1

closed1 <- which(!is.na(test2$closed))
not_closed <- which(is.na(test2$closed))
intersect(closed1,not_closed) #has to be empty

test2[closed1,-c(2:3)] <- apply(test2[1,-c(2:3)],1,function(x) (aux >= x[2] & aux <x[3])*1)
test2[not_closed,-c(2:3)] <- apply(test2[not_closed,-c(2:3)],1,function(x) (aux >= x[2] )*1)

#I am not getting a row, I need to deal with that

#first, make all 1's when it was opened before earlier day
apply(test1[is.na(test1$closed)],1,function(x) 


My_fill <-   function(data) {
  if (!is.na(data$closed )) {
    
  }
}

as.numeric(names(test1)[-c(1:2)])  <rep(test1[5,'closed'],12)
as.numeric(names(test1)[-c(1:2)])  >=rep(test1[5,'closed'],12)
