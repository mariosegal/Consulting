

library(ggplot2)
library(scales)
library(stringr)

results1 = results[results$group_new==1,]
results1$measure = factor(results1$measure,c('age1','cbr','stype','tenure'),labels=c('Owner Age','Community Bank','Product Subtype','Account Tenure'))

levels1 = unique(as.character(results1$level))
levels1 = levels1[-c(9,32,39,51,52)]
results1 = results1 %>% filter(level %in% levels1)
results1$level = factor(results1$level,levels1,labels=levels1,ordered = T)

results1 %>% filter(!is.na(level) ) %>% ggplot(aes(x=level,y=rate,fill=measure,label=comma(round(rate,1))))+geom_bar(position='dodge',stat='identity')+facet_wrap(~measure,scales='free')+theme_bw()+theme(legend.position='none',panel.grid.major=element_blank(),axis.text.x=element_text(angle=0))+geom_text(vjust=-1)+scale_y_continuous("Complaint Rate per 1,000 Accounts",breaks=NULL)+scale_x_discrete('',labels=function(x) str_wrap(x,width=5))+coord_cartesian(ylim=c(0,10))+geom_hline(yintercept=3,color='red',linetype=2)+ggtitle('Checking Complaints - NSF Related due to Low Balance ')

ggsave('sample pdf page.pdf',width = 11,height = 8)
