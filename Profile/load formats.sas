* The formats on this file need to be loaded, some are in a SAS file called formts_new, and are loded by the first proc format call;
* at present they are loaded into the work directory, if you load them into a permanent one using the option library=<dir> 
then they do not need to be loaded again , as ong as the fmtsearch global option is set to search for formats there



*#############################;
* PARAMTERS;
libname data '/folders/myfolders/data/';
option mprint nosymbolgen nomlogic mcompilenote=all minoperator;
options mstored sasmstore=data;
options fmtsearch= (data);
*#############################;

proc format cntlin=data.formats_new;
run;


Proc format ;
picture pctdoll (round)
	-999999999999 -<0 = '00,000,000,000,009.99' (mult=1 prefix='-$' )
	0 - 1000000000000 = '00,000,000,000,009.99' (mult=1 prefix='$');
picture pctcomma (round)
	-999999999999 -<0 = '00,000,000,000,009.9' (mult=.1 prefix='-' )
	0 - 1000000000000 = '00,000,000,000,009.9' (mult=.1)  ;
run;


proc format ;
value $ svcfmt  'web' = 'Web Banking'
             'bp'  = 'Bill Pay'
			 'wap' = 'Mobile Banking'
			 'sms' = 'Text Banking'
			 'fico' = 'Credit Score'
			 'fworks' = 'FinanceWorks'
			 'edeliv' = 'e-Delivery'
			 'estat' = 'e-Statements';
value $ tranfmt 'vpos' = 'Debit Signature'
                'mpos' = 'Debit PIN'
				'atmo' = 'M&T ATM'
				'atmt' = 'Other ATM'
				'bp' = 'Bill Pay'
				'chk' = 'Checks' 
				'dd' = 'Direct Deposit';
value wealthband 0<-25000 = 'Up to $25K' 25000<-50000 = '$25 to 50K'  50000<-75000 = '$50 to 75K'   
				75000<-100000 = '$75 to 100K' 100000<-250000 = '$100 to 250K'      250000<-500000 = '$250 to 500K'
				500000<-1000000 = '$500K to 1M' 1000000<-high = 'Over $1M'  ;				
run;

proc format ;
value binary_flag 0 = 'No'
                  1 = 'Yes'
				  -1 = 'All';
run;

proc format ;
value cqifmt 0='0'
             1='1'
			 2='2'
			 3='3'
			 4='4'
			 5='5'
			 -1='All';
run;

proc format ;
value mkt2012fmt 	1='WNY'      
					2='Ctl NY/E PA'    
					3='Estrn/Metro'     
					4='Ctl PA/W MD'    
					6='G Balt'      
					7='G Wash'   
					8='G Del'    
					98='NATL'      
					99='Out of Mkt'
 					-1='All';
run;

proc format ;
value $ rmfmt 'Y' = 'Managed'
			'N' = 'Not Managed'
			'A' = 'All';
run;



proc format ;
value clvband low-<0 = 'Below Zero'
			  0 = 'Zero'
			  0<-<250 = 'Up to $250'
			  250-<500 = '$250 to $500'
			  500-<750 = '$500 to $750'
			  750-<1000 = '$750 to $1,000'
			  1000-<1500 = '$1,000 to $1,500'
			  1500-<2500 = '$1,500 to $2,500'
			  2500-<5000 = '$2,500 to $5,000'
			  5000-high = '$5,000+'
			  . = 'All';
run;



