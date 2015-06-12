setwd("Z:/M&T Projects/OCA")

#the file has commas ont he last fieldand no queotes, and it is comma separated - so it wont work
#I erased that on excel and saved as tab, lets read that first
main <- read.csv("CCTS_Masked1.csv",stringsAsFactors=T)  
names(main) <- tolower(gsub("\\.+","_",names(main)))
names(main) <- (gsub("_+$","",names(main)))

#READ THE OTHR PIECE THEY SENT ME
comments <- read.csv("Complaint_Only.csv",stringsAsFactors=F)
names(comments) <- tolower(gsub("\\.+","_",names(comments)))
names(comments) <- (gsub("_+$","",names(comments)))

complaints <- inner_join(main,comments)
complaints$entered_date <- as.Date(complaints$entered_date,format='%m/%d/%Y')


complaints$complaint_date <- as.Date(complaints$complaint_date,format='%m/%d/%Y')

save(complaints,file='complaints.rdata')

#check against dashboard
table(format(complaints$complaint_date,'%m%Y'))
table(format(complaints$entered_date,'%m%Y'))
table(format(complaints$complaint_date,'%m%Y'),format(complaints$entered_date,'%m%Y'))
table(complaints$level_1)


comments1 <- read.table('CCTS_PROD_0414_1231_Discrimination_and%09Comments.txt',sep='\t',quote="",header=T,fill=T,stringsAsFactors = F)

#20150514
#read the escalated and the non escalatred for 1Q2015

escalated = read.table('Z:/M&T Projects/OCA/test.txt',sep='\t',header=T,fill=T,quote='"',stringsAsFactors = F)
table(escalated$Category)
escalated1 = escalated[1:10]
names(escalated1) = tolower(gsub("\\.+","_",names(escalated1)))

extra_esc = read.table('Z:/M&T Projects/OCA/Customer_Care_Complaint_Data_Closed_Jan_2014_Mar_2015_Match.txt',sep='\t',header=T,fill=T,quote="",stringsAsFactors = F)

escalated1 = left_join(escalated1,extra_esc,by=c('incident_id'='Incident.ID'))

save(escalated1,file='Z:/M&T Projects/OCA/escalated1.rdata')
#the escalated is failing, but I believe before solution it wrokds, lets take that mereg it to teh accts and create amatser set so I can match



complaints_1q15 = read.table('Z:/M&T Projects/OCA/CCTS_PROD_010115_033115_no_cust.txt',sep='\t',header=T,fill=T,quote="",stringsAsFactors = F)
table(complaints_1q15$Level_1)
extra_1q2015 = read.table('Z:/M&T Projects/OCA/CCTS_PROD_Match.txt',sep='\t',header=T,fill=T,quote="",stringsAsFactors = F)
names(extra_1q2015) = c("complaint_master_id",'mask_acct','mask_ssn')
extra_1q2015$complaint_master_id = gsub('"','',extra_1q2015$complaint_master_id)
extra_1q2015$complaint_master_id = as.numeric(extra_1q2015$complaint_master_id)

names(complaints_1q15) = tolower(names(complaints_1q15))
complaints_1q15 = left_join(complaints_1q15,extra_1q2015)
save(complaints_1q15,file='Z:/M&T Projects/OCA/complaints_1q15.rdata')


save(escalated,file='Z:/M&T Projects/OCA/escalated.rdata')

length(intersect(complaints$complaint_master_id,complaints_1q15$complaint_master_id))
length(intersect(complaints_1q15$complaint_master_id,complaints$complaint_master_id))

length(intersect(complaints$complaint_master_id,escalated1$incident_id))
length(intersect(escalated1$incident_id,complaints$complaint_master_id))

length(intersect(complaints_1q15$complaint_master_id,escalated1$incident_id))
length(intersect(escalated1$incident_id,complaints_1q15$complaint_master_id))



#As of now I do not have any common fields, so putting them togetehr seems silly to me
intersect(names(escalated1),names(complaints_1q15))
intersect(names(escalated1),names(complaints))


#combine the full complaints non-escalated

#convert dates
complaints_1q15$entered_date <- as.Date(complaints_1q15$entered_date,format='%m/%d/%Y')
complaints_1q15$complaint_date <- as.Date(complaints_1q15$complaint_date,format='%m/%d/%Y')
complaints_1q15$lan_id = NA

complaints_2014_2015Q1 = bind_rows(complaints,complaints_1q15)

#create factors
complaints_2014_2015Q1$level_1 = factor(complaints_2014_2015Q1$level_1)
complaints_2014_2015Q1$level_2 = factor(complaints_2014_2015Q1$level_2)
complaints_2014_2015Q1$level_3 = factor(complaints_2014_2015Q1$level_3)
complaints_2014_2015Q1$level_4 = factor(complaints_2014_2015Q1$level_4)
complaints_2014_2015Q1$level_5 = factor(complaints_2014_2015Q1$level_5)
complaints_2014_2015Q1$discriminated = factor(complaints_2014_2015Q1$discriminated)
complaints_2014_2015Q1$dishonest = factor(complaints_2014_2015Q1$dishonest)
complaints_2014_2015Q1$unfair = factor(complaints_2014_2015Q1$unfair)
complaints_2014_2015Q1$broke_the_law = factor(complaints_2014_2015Q1$broke_the_law)

save(complaints_2014_2015Q1,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1.rdata')
