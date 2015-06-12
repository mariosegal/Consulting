* The profile macro, uses 2 standard extracts from datamart.;
*These extracts can be used ofr many otehr analyses as well;
*The should be run periodically (quarterly is one suggestion);

*The names of the 2 datamar exports are:
EXP – SAS FILE – BALANCES AND DEMOGRAPHICS   (still being identified)
EXP – SAS FILE – CONTRIBUTION

*after exporitng the data the text files need to be moved ot a location accessible by SAS;
*the following 2 macros need to be run
* %read_monthly_data()
* read_contr_dat()

% the macro %SQUEEZE is optional, what it does is create a smaller evrion of the dataset, byt using te minimul field length for each field
* for example if a text field has a maximum length in the dataset of $17 it stores is as $17 and not say $25

* the options compress=yes will further have SAS compress the dataset, this is useful because these 2 datasets can be very big;

* one needs to specify the source file name (source), the identifier to use (is 201406)
*the path and directory where the source file is located ie., C:\\My Documents\....
* th enames and paths do not need to be quoted;




* Code to read and compress data;

	%read_monthly_data (source=MS_EXP`2.TXT, identifier=201506, directory=/folders/myfolders/, first=2) 

	options compress=yes;
	%SQUEEZE(data.main_201303, data.main_201303_new) 



	%read_contr_data (source=contrib.txt, identifier=201303, directory=C:\Documents and Settings\ewnym5s\My Documents\) ;
	%SQUEEZE(data.contrib_201303, data.contrib_201303_new) %replace(dir=data, source=contrib_201303_new, dest=contrib_201303);

	options compress= no; *Turn it off so as not to compress other datasets;
	
	
* ###########################################################;
*MACRO CODE ;
options mcompilenote=all;

    %macro read_monthly_data (source=, identifier=, directory=, first=) / store 
            source des='get read_monthly_data macro';
        filename mydata "&directory&source";

    data data.Main_&identifier;
        length HHID $ 9 STATE $ 2 ZIP $ 5 RM $ 1 clv_flag $ 1 clv_steady $ 1;
        infile mydata DLM='09'x firstobs=&first lrecl=4096 dsd missover;
        INPUT hhID $                                                         
         STATE $ 
         ZIP $                                                  
         BRANCH $                                           
         CBR MARKET dda mms sav tda ira sec trs mtg heq card ILN sln 
            sdb ins bus com DDA_Amt MMS_amt sav_amt TDA_Amt IRA_amt sec_Amt 
            trs_amt MTG_amt HEQ_Amt ccs_Amt iln_amt sln_amt IXI_tot IXi_Annuity 
            ixi_Bonds ixi_Funds ixi_Stocks ixi_Other ixi_Non_Int_Chk 
            ixi_int_chk ixi_savings ixi_MMS ixi_tda 
            source $                                                   
         WAS WFO segment clv_total clv_rem clv_rem_ten cqi_bp cqi_DD 
            cqi_deb cqi_odl cqi_web web VPOS_AMT vpos_num mpos_amt mpos_num 
            ATMO_AMT ATMO_NUM ATMT_AMT ATMT_NUM web_signon BP_NUM BP_AMT 
            BR_TR_NUM BR_TR_amt VRU_NUM SMS_NUM WAP_NUM fico_num bp WAP SMS 
            edeliv estat fico FWorks fworks_num band $
		 band_yr $
		 IND IND_AMT chk_num dd_amt distance RM tenure tran_code $
		 clv_flag $
		 clv_steady $
		 svcs age;

        if tenure lt 0 then
            tenure=.;
        tenure_yr=divide(tenure, 365);
        hh=1;
        grp=1;
    run;

%mend;


%macro read_contr_data (source=, identifier=, directory=) / source
        des='get read_monthly_data macro';
        
    filename mydata "&directory&source";

    data data.Contrib_&identifier;
        length HHID $ 9 STATE $ 2 ZIP $ 5;
        infile mydata DLM='09'x firstobs=2 lrecl=4096 dsd;
        INPUT hhID $
		STATE $ 
         ZIP $                                                  
         BRANCH $                                           
         CBR MARKET DDA_CON MMS_CON SAV_CON TDA_CON IRA_CON SEC_CON 
            TRS_CON mtg_con heq_con card_con ILN_CON SLN_CON band $
		 band_yr $
		 IND_con;
    run;

%mend;

