measures_multi <- function(actual,pred) {
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