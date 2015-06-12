####
#I want to ensure the levels for escalated an dnon are similar, 
#since I ghad modified the a bit for dupes on the non escalated 
#and deloitte may hav edone some other modisfications (or not)
#the goal is not to have dupes just due to spaces, periods, caps, etc.

load('Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')

####Level 1
table((complaints_2014_2015Q1_matched_both$level_1))
complaints_2014_2015Q1_matched_both$level_1 = tolower(complaints_2014_2015Q1_matched_both$level_1)
complaints_2014_2015Q1_matched_both$level_1[complaints_2014_2015Q1_matched_both$level_1=='mortgage'] = 'mortgage or home equity'
complaints_2014_2015Q1_matched_both$level_1[complaints_2014_2015Q1_matched_both$level_1=='service channels'] = 'service channel'
complaints_2014_2015Q1_matched_both$level_1[complaints_2014_2015Q1_matched_both$level_1=='service interaction'] = 'service channel'

# level_2
table((complaints_2014_2015Q1_matched_both$level_2))
complaints_2014_2015Q1_matched_both$level_2 = tolower(complaints_2014_2015Q1_matched_both$level_2)
'marketing/advertising'
complaints_2014_2015Q1_matched_both$level_2 = gsub('\\"',"",complaints_2014_2015Q1_matched_both$level_2)

complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='certificate of deposit (cd)'] = '(cd) certificate of deposit'


complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='home equity loan or heloc'] = 'home equity loan'
complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='heloc'] = 'home equity loan'
complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='other services (money order'] = 'other services (money order, bank check, wire)'

complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='marketing / advertising'] = 'marketing/advertising'
complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='other deposit (ira'] = 'other deposit (ira, passbook, holiday club, etc)'

complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='other deposit (ira'] = 'other deposit (ira, passbook, holiday club, etc)'

complaints_2014_2015Q1_matched_both$level_2[complaints_2014_2015Q1_matched_both$level_2=='other' & complaints_2014_2015Q1_matched_both$level_1=='mortgage or home equity'] = 'other mortgage'

#level_3
names(table((complaints_2014_2015Q1_matched_both$level_3)))
complaints_2014_2015Q1_matched_both$level_3 = tolower(complaints_2014_2015Q1_matched_both$level_3)
complaints_2014_2015Q1_matched_both$level_3 = gsub('\\"',"",complaints_2014_2015Q1_matched_both$level_3)
complaints_2014_2015Q1_matched_both$level_3 = gsub('^ ',"",complaints_2014_2015Q1_matched_both$level_3)
complaints_2014_2015Q1_matched_both$level_3 = gsub(' / ',"/",complaints_2014_2015Q1_matched_both$level_3)

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='making payments'] = 'making or receiving payments, sending money'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='using a debit or atm card'] = 'using a debit, atm, or custom card'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='payments & collections'] = 'payments and collections'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='payments & escrow'] = 'payments and escrow'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='using a debit'] = 'using a debit or atm card'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='shopping for a loan'] = 'shopping for a loan, lease, or line of credit'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='taking out the loan / account terms and changes'] = 'taking out the loan or lease / account terms and changes'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3 %in% c('fees and interest','fees / charges','fees & interest','fees')] = 'fees, charges and interest'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3 %in% c('loan servicing / fees / statements')] = 'loan servicing/fees/statements'
complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='collections / repossession / workout'] = 'collections/repossession/workout'


complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='payments and external transfers'] = 'making or receiving payments, sending money'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='employee interaction'] = 'service with bank personnel'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='advances & balance transfers'] = 'advances and balance transfers'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='making or receiving payments'] = 'making or receiving payments, sending money'


complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='application'] = 'application and underwriting'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='account maintenance'] = 'account maintenance, opening, or closing'

complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='managing the loan'] = 'managing the loan, lease, or line of credit'


complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='application and underwriting'] = 'applying for the loan'
complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='mods/mitigation/collection/foreclosure'] = 'collection/loss mitigation/foreclosure'
complaints_2014_2015Q1_matched_both$level_3[complaints_2014_2015Q1_matched_both$level_3=='collections/repossession/workout'] = 'collection/loss mitigation/foreclosure'

save(complaints_2014_2015Q1_matched_both,file='Z:/M&T Projects/OCA/complaints_2014_2015Q1_matched_both.rdata')
