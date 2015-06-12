

head(complaints)
table(complaints$level_1,useNA='ifany')

table(complaints$level_2,complaints$level_1,useNA='ifany')


table(complaints$level_3[complaints$level_1=='Bank Account or Service' & complaints$level_2=='Checking account'])

dim(complaints)
