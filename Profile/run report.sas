*to run a profile report one needs to:
*;
* 1) load the macros from master code for profile macros 20150610
* 2) run the line below, you need topass several parameters
*     class1 = a variable (categorical) the analysis will be done for each value of this variable.
      it could be  somethign ike segment, or some defined groups
      fmt1 = a format for the class variable. if it does not need one you can pass a generic SAS format such as commaw.d or $w.
      condition = a condition to filter the data by, for example dda eq 1 will filter hhlds with dda, 
      if nothing is needed it can be left blank
      main_source is the name of the main dataset
      contr_souce is the name of the contribution dataset
      out_file is the name for the report a .pdf wil be appended, quotes are not needed
      out_dit is the path to the directory where sas should save the report, quotes are not needed
      logo_file if present  inserts  a  jpg logo (M&T) on the bottom right footer, however this feature may not work properly anymore, 
      if left empty then no right footer is generated
;


%create_report(class1=segment,fmt1=segfmt , condition = , main_source = data.main_201503, contrib_source = data.contrib_201503, OUT_FILE=REPORT_NAME,out_dir = /folders/myfolders/, logo_file=);


