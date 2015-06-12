#extract addtl trans data
library(RODBC)
mtdata <- odbcDriverConnect('driver={SQL Server};server=iqrus-db1;trusted_connection=true')


q1 <- "SELECT  EXPRESSION_1, sum(cast([TRANS_NBR_CHECKS_NEGOTIATED] as numeric(14,2)) ) as checks, sum(cast([TRANS_NBR_VRU_TRANSFER_FROM] as numeric(14,2))+cast([TRANS_NBR_vru_transfer_to] as numeric(14,2))+cast([TRANS_NBR_CSW_TRANSFER_FROM] as numeric(14,2))+cast([TRANS_NBR_CSW_TRANSFER_TO] as numeric(14,2))) as transfers,  sum(cast([TRANS_NBR_DRV_IN_DEPPMT_NO_CSH_BK] as numeric(14,2)) +cast([TRANS_NBR_DRV_IN_DEP_PMT_WITH_CSH_BK] as numeric(14,2)) +cast([lobby_dep_pmt_no_csh_bk] as numeric(14,2)) +cast([TRANS_NBR_LOBBY_DEP_PMT_WITH_CSH_BK] as numeric(14,2))) as deposits from iqrmt.dbo.TRAN_2014"
q2 = " where TRANS_PTYPE = 'DDA' group by EXPRESSION_1"

for (period in c('01','02','03','04','05','06')) {
  aux <- sqlQuery(mtdata,paste0(q1,period,q2))
  aux$period = paste0('2014',period)
  assign(paste0("tran_2104",period),aux)
}

tran_extra <- bind_rows(lapply(ls(pattern='tran'),get)) 
save(tran_extra,file='Z:/M&T Projects/NSF/tran_extra.rdata')



#for bill pay, lets do it by HHLD as I need ACAC and it isnot loaded (yet)

q3 = "SELECT EXPRESSION_8,  ACCT_WEB_NBR_XFER_FROM_CKG, ACCT_WEB_AMT_XFER_FROM_CKG,ACCT_WEB_NBR_PMNT_MNT_ACCT,ACCT_WEB_NBR_PMNT_NON_MNT,ACCT_WEB_AMT_PMNT_MNT_ACCT,ACCT_WEB_AMT_PMNT_NON_MNT,ACCT_WEB_AMT_BNK2BNK_TRNS_DEB,ACCT_WEB_AMT_BNK2BNK_TRNS_CRED,ACCT_WEB_NBR_BNK2BNK_TRNS_DEB,ACCT_WEB_NBR_BNK2BNK_TRNS_CRED from IQRMT.dbo.ACCT_2014"
q4 = " WHERE ACCT_PTYPE='DDA'  "

test <- sqlQuery(mtdata,paste0(q3,period,q4))


