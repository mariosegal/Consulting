#skinny dda2  file


dda2a <- dda2[dda2$nsf_fee %in% c('No with Fee','Yes'),c(1,8,3,11,12,13:18,25,67:68,70,69,52,28,27,23,21,26,29,36,57:66)]

save(dda2a,file='dda2_small.rdata',compress='xz')

penet1a <- penet1[penet1$flag %in% c('No_with_Fee','Yes'),]
save(penet1a,file='penet1_small.rdata',compress='xz')


bals1a <- bals1[bals1$flag %in% c('No_with_Fee','Yes'),]
save(bals1a,file='bals1_small.rdata',compress='xz')

contr1a <- contr1[contr1$flag %in% c('No_with_Fee','Yes'),]
save(contr1a,file='contr1_small.rdata',compress='xz')


segmenta <- segment[segment$flag %in% c('No_with_Fee','Yes'),]
save(segmenta,file='segment_small.rdata',compress='xz')
