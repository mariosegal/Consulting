measures2 <- function(model,actual,...) {
  pred = predict(model,...)
  t = table(actual,pred)
  acc = sum(diag(t))/sum(t)
  sen = prop.table(t,1)[2,2]
  spec = prop.table(t,1)[1,1]
  f1 = 2*t[2,2]/(2*t[2,2]+t[1,2]+t[2,1])
  return(data.frame(accuracy=acc,sensitivity=sen,specificity=spec,F1_score=f1))
}