n1 = seq(10000,130000,10000)
d = c(0.1,0.2,0.3,0.4,0.5,.6,.7,.8,.9,1)/11.9

n_eq = sapply(d,function(x) pwr.t.test(power=0.8,d=x,alternative='greater',sig.level=0.1)$n)
n1 = c(seq(10000,130000,10000))

results <- sapply(d, function(y) sapply(n1, function(x) pwr.t2n.test(n1=x,n2=1750000-x,sig.level=0.9,alternative='g',d=y)$power))


#if 2% call and that is likely
# then to get 5,000 calls we need 250,000 on test, and 1500000 on control


#we only precict 47500 people  for 3+
pwr.t.test(n=47500/2,sig.level=0.1,d=d[1],alternative='g')$power

results1 <- sapply(d,function(x) pwr.t.test(n=47500/2,sig.level=0.1,d=x,alternative='g')$power)
names(results1) = c(0.1,0.2,0.3,0.4,0.5,.6,.7,.8,.9,1)
results1
