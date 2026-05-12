/***************************************************************************
 * 003-append-determinants-other-controls.sas
 *
 * Purpose:
 *   Append determinants and control variables to the firm-quarter
 *   dataset built by 002. Builds seven intermediate control datasets
 *   (Compustat fundamentals, IBES dispersion, management guidance,
 *   institutional ownership, CRSP CARs and return volatility, future
 *   GAAP performance, future street earnings) and then merges them
 *   into all_five1 to produce all_five2.
 *
 * Inputs (from RAW_DATA_DIR):
 *   actu_epsus_07182025.sas7bdat            (IBES; downloaded by 002)
 *   det_guidance_06242025.sas7bdat          (IBES; downloaded by this script if absent)
 *   dsf_07172025.sas7bdat                   (CRSP daily stock file)
 *   dsix_07242025.sas7bdat                  (CRSP daily index; downloaded by this script if absent)
 *   funda_07242025.sas7bdat                 (Compustat annual; downloaded by this script if absent)
 *   fundq_07172025.sas7bdat                 (Compustat quarterly)
 *   wrds_io_timeseries_20251213.sas7bdat    (WRDS-built institutional ownership timeseries;
 *                                            built by the WRDS macro at the bottom of this script
 *                                            if absent)
 *
 * Inputs (from DATA_DIR):
 *   all_five1.sas7bdat                      (output of 002)
 *
 * Outputs (to DATA_DIR):
 *   compustat_controls.sas7bdat             (intermediate, persistent)
 *   ibes_controls.sas7bdat                  (intermediate, persistent)
 *   guidance_vars.sas7bdat                  (intermediate, persistent)
 *   io_vars.sas7bdat                        (intermediate, persistent)
 *   crsp_controls.sas7bdat                  (intermediate, persistent)
 *   future_performance_vars.sas7bdat        (intermediate, persistent)
 *   future_street_vars.sas7bdat             (intermediate, persistent)
 *   all_five2.sas7bdat                      (final firm-quarter dataset with all controls)
 *
 * Notes:
 *   - Reads RAW_DATA_DIR and DATA_DIR from .env via %load_env (defined in MACROS.sas).
 *   - Each PART is wrapped in a macro with an existence-check on its
 *     persistent output, so re-running the script skips already-built
 *     intermediates. To rebuild a specific intermediate, delete it from
 *     DATA_DIR first.
 *   - WRDS downloads are also wrapped with existence-checks so a re-run
 *     with the .sas7bdat already on disk skips the download.
 *   - SIGNON is conditional on whether any WRDS-derived raw file is
 *     missing -- if everything is cached locally, no credential prompt.
 ***************************************************************************/

*Setup*********************************************************************;

/* Resolve the path to this script. SYSIN is set in batch mode (sas -SYSIN);
   SAS_EXECFILEPATH is set by Enhanced Editor / Enterprise Guide interactively. */
%let codepath = %sysfunc(getoption(sysin));
%if %length(&codepath) = 0 %then %do;
    %let codepath = %sysfunc(sysget(SAS_EXECFILEPATH));
%end;
%include "&codepath\..\MACROS.sas";
%load_env;

libname raw  "&RAW_DATA_DIR";
libname data "&DATA_DIR";

*Decide whether any WRDS downloads are needed -- only signon if so;
%let need_wrds = 0;
%macro check_need(target=);
  %if not %sysfunc(exist(&target)) %then %let need_wrds = 1;
%mend;
%check_need(target=raw.det_guidance_06242025);
%check_need(target=raw.dsix_07242025);
%check_need(target=raw.funda_07242025);
%check_need(target=raw.WRDS_IO_TimeSeries_20251213);

%if &need_wrds = 1 %then %do;
  %let wrds = wrds-cloud.wharton.upenn.edu 4016;
  options comamid=TCP remote=WRDS;
  signon username=_prompt_;
%end;
%else %put NOTE: All WRDS-downloaded raw files already exist -- skipping signon.;

*Create calendar if needed;
%macro create_calendar;
%if not %sysfunc(exist(work.crspdates)) %then %do;
	proc sql; create table work.dates as select distinct date from raw.dsf_07172025 order by date; quit;
	data work.crspdates; set work.dates; n = _n_; run;
%end;
%mend;
%create_calendar;

/***************************************************************************
PART 1: COMPUSTAT CONTROLS
***************************************************************************/
%macro create_compustat_controls;
%if not %sysfunc(exist(data.compustat_controls)) %then %do;

data work.fdp; set data.all_five1; run;

proc sql;
create table work.fdp2 as select distinct 
	a.gvkey, a.datadate, a.rdq, a.fyearq, a.fqtr, a.yearqtr, a.CSHOQ, a.PRCCQ, a.ceqq, a.CSHFDQ, a.IBQ, a.SPIQ, a.atq,
	b.STKCOQ, b.RCPQ, b.AQPQ, b.NCOQ, b.NRTXTQ, b.SETPQ, b.WDPQ, b.GDWLIPQ
from work.fdp a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.datadate=b.datadate and a.rdq=b.rdq 
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C' 
	and not missing(b.fqtr) and not missing(b.ibq);
quit;

proc sql;
create table work.fdp3 as select distinct a.*, b.atq as atq_lagyear, b.ibq as ibq_lagyear
from work.fdp2 a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.FQTR=b.FQTR and year(a.datadate)-1=year(b.datadate) 
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C' 
	and not missing(b.fqtr) and not missing(b.ibq);
quit;
proc sort data=work.fdp3; by gvkey datadate descending atq_lagyear; run;
proc sort data=work.fdp3 nodupkey; by gvkey datadate; run;

proc sql;
create table work.fdp4 as select distinct a.*, b.atq as atq_lagquarter, b.datadate as datadate_lagquarter, b.cshfdq as cshfdq_lagquarter
from work.fdp3 a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.datadate-120<=b.datadate<a.datadate 
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C' and not missing(b.fqtr);
quit;
proc sort data=work.fdp4; by gvkey datadate descending atq_lagquarter; run;
proc sort data=work.fdp4 nodupkey; by gvkey datadate; run;

data work.fdp5; set work.fdp4;
	lnmve=log(CSHOQ*PRCCQ); btm=ceqq/(CSHOQ*PRCCQ);
	percent_change_cshfdq=abs((CSHFDQ-CSHFDQ_lagquarter)/CSHFDQ_lagquarter);
	percent_change_ibq=abs((IBQ-IBQ_lagyear)/IBQ_lagyear);
	q4=(fqtr=4); if spiq=. then spiq=0; if ibq ne 0 then abs_spiq_ibq=abs(spiq)/abs(ibq);
	if RCPQ=. then RCPQ=0; if AQPQ=. then AQPQ=0; if NCOQ=. then NCOQ=0; if NRTXTQ=. then NRTXTQ=0;
	if SETPQ=. then SETPQ=0; if WDPQ=. then WDPQ=0; if GDWLIPQ=. then GDWLIPQ=0;
	if ibq ne 0 then do; abs_rcpq_ibq=abs(rcpq)/abs(ibq); abs_aqpq_ibq=abs(aqpq)/abs(ibq); end;
	dum_ncoq=(ncoq ne 0); dum_nrtxtq=(nrtxtq ne 0); dum_setpq=(setpq ne 0); dum_wdpq=(wdpq ne 0); dum_gdwlipq=(gdwlipq ne 0);
run;

proc sort data=work.fdp5; by yearqtr; run;
proc rank data=work.fdp5 out=work.fdp6 groups=10;
	var abs_rcpq_ibq abs_aqpq_ibq abs_spiq_ibq;
	ranks allqtr_dec_rcpq_ibq allqtr_dec_aqpq_ibq allqtr_dec_spiq_ibq;
	by yearqtr;
run;

data work.fdp7; set work.fdp6;
	unexpected_item=((allqtr_dec_rcpq_ibq=9)+(allqtr_dec_aqpq_ibq=9)+(allqtr_dec_spiq_ibq=9)+dum_setpq+dum_gdwlipq+dum_ncoq+dum_nrtxtq+dum_wdpq)>0;
run;

proc sql;
create table work.busyness as select distinct rdq, count(*) as ea_count
from raw.fundq_07172025
where 1998<=year(datadate)<=2024 and indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
	and not missing(cusip) and not missing(epsfiq) and not missing(rdq) and 0<(rdq-datadate)<120 and compstq ne "DB"
group by rdq;
quit;

proc sql;
create table data.compustat_controls as select distinct
	a.gvkey, a.datadate, a.lnmve, a.btm, a.abs_spiq_ibq, a.percent_change_cshfdq, a.percent_change_ibq, a.q4,
	a.datadate_lagquarter, a.unexpected_item, coalesce(b.ea_count,0) as ea_count, log(1+coalesce(b.ea_count,0)) as log_ea_count
from work.fdp7 a left join work.busyness b on a.rdq=b.rdq;
quit;

%end;
%mend;
%create_compustat_controls;

/***************************************************************************
PART 2: IBES CONTROLS
***************************************************************************/
%macro create_ibes_controls;
%if not %sysfunc(exist(data.ibes_controls)) %then %do;

data work.ibes_actuepsus; set raw.Actu_epsus_07182025; where MEASURE='EPS' and PDICITY='QTR'; run;
proc sort data=work.ibes_actuepsus; by ticker PENDS descending VALUE; run;
proc sort data=work.ibes_actuepsus nodupkey; by ticker PENDS; run;

data work.ibes_actuepsus2; set work.ibes_actuepsus;
	lagmins=INTCK('minute',dhms(anndats,0,0,anntims),dhms(actdats,0,0,acttims));
	log_lagmins=log(1+lagmins); pends_month=intnx('month',PENDS,0,'end');
	keep ticker pends_month lagmins log_lagmins;
run;

proc sql;
create table data.ibes_controls as select distinct a.gvkey, a.datadate,
	a.ibes_stdev_u/a.prcn2 as dispersion,
	case when a.ibes_a_u_ratio=. then . when a.ibes_a_u_ratio ne 1 then 1 else 0 end as stock_split,
	b.lagmins, b.log_lagmins
from data.all_five1 a left join work.ibes_actuepsus2 b
	on a.ibes_ticker=b.ticker and intnx('month',a.datadate,0,'end')=b.pends_month;
quit;

%end;
%mend;
%create_ibes_controls;

/***************************************************************************
PART 3: GUIDANCE VARIABLE
***************************************************************************/
%macro create_guidance_vars;
%if not %sysfunc(exist(data.guidance_vars)) %then %do;

%if not %sysfunc(exist(raw.det_guidance_06242025)) %then %do;
	rsubmit; proc download data=ibes.det_guidance out=raw.det_guidance_06242025; run; endrsubmit;
%end;

proc sql;
create table work.guide1 as select distinct ticker, prd_yr, prd_mon, 1 as has_guidance
from raw.det_guidance_06242025 where measure="EPS" and pdicity="QTR";
quit;

proc sql;
create table data.guidance_vars as select distinct a.gvkey, a.datadate, coalesce(b.has_guidance,0) as guidance
from data.all_five1 a left join work.guide1 b
	on a.ibes_ticker=b.ticker and year(a.datadate)=b.prd_yr and month(a.datadate)=b.prd_mon;
quit;

%end;
%mend;
%create_guidance_vars;

/***************************************************************************
PART 4: INSTITUTIONAL OWNERSHIP
***************************************************************************/
%macro create_io_vars;
%if not %sysfunc(exist(data.io_vars)) %then %do;

%if not %sysfunc(exist(raw.WRDS_IO_TimeSeries_20251213)) %then %do;
	%put ERROR: raw.WRDS_IO_TimeSeries_20251213 does not exist. Please run the IO download code separately .;
	%return;
%end;

proc sql;
create table work.valid_rdates as select a.permno, a.datadate, max(b.rdate) as rdate format date9.
from data.all_five1 a left join raw.WRDS_IO_TimeSeries_20251213 b
	on a.permno=b.permno and b.rdate<=a.datadate and b.rdate>=intnx('day',a.datadate,-180)
group by a.permno, a.datadate;
quit;

proc sql;
create table data.io_vars as select distinct a.gvkey, a.datadate, min(coalesce(c.ior,0),1) as io
from data.all_five1 a
left join work.valid_rdates b on a.permno=b.permno and a.datadate=b.datadate
left join raw.WRDS_IO_TimeSeries_20251213 c on b.permno=c.permno and b.rdate=c.rdate;
quit;

%end;
%mend;
%create_io_vars;

/***************************************************************************
PART 5: CRSP CONTROLS
***************************************************************************/
%macro create_crsp_controls;
%if not %sysfunc(exist(data.crsp_controls)) %then %do;

%if not %sysfunc(exist(raw.dsix_07242025)) %then %do;
	rsubmit; proc download data=crsp.dsix out=raw.dsix_07242025; run; endrsubmit;
%end;

%tddays(dsetin=data.all_five1 (keep=gvkey permno datadate best_anndats_adj), 
		dsetout=work.temp1, datevar=best_anndats_adj, beginwin=0, endwin=1, calendarname=work.crspdates);

data work.day0; set work.temp1; where td_days=0; day0=date; keep gvkey datadate day0; run;
data work.day1; set work.temp1; where td_days=1; day1=date; keep gvkey datadate day1; run;

proc sql;
create table work.car1 as select distinct a.gvkey, a.datadate, a.permno, b.day0, c.day1
from data.all_five1 a
left join work.day0 b on a.gvkey=b.gvkey and a.datadate=b.datadate
left join work.day1 c on a.gvkey=c.gvkey and a.datadate=c.datadate;
quit;

proc sql;
create table work.car2 as select distinct a.gvkey, a.datadate, a.permno, a.day0, a.day1,
	exp(sum(log(1+b.ret)))-1 as ret_2day
from work.car1 a left join raw.Dsf_07172025 b on a.permno=b.permno and a.day0<=b.date<=a.day1
group by a.gvkey, a.datadate;
quit;

proc sql;
create table work.car3 as select distinct a.*, exp(sum(log(1+b.vwretd)))-1 as vwretd_2day
from work.car2 a left join raw.Dsix_07242025 b on a.day0<=b.caldt<=a.day1
group by a.gvkey, a.datadate;
quit;

proc sql;
create table work.car4 as select distinct a.*, b.datadate_lagquarter
from work.car3 a left join data.compustat_controls b on a.gvkey=b.gvkey and a.datadate=b.datadate;
quit;

proc sql;
create table data.crsp_controls as select distinct a.gvkey, a.datadate,
	a.ret_2day-a.vwretd_2day as car_0top1, std(b.ret) as ret_vol
from work.car4 a left join raw.dsf_07172025 b on a.permno=b.permno and a.datadate_lagquarter<b.date<=a.datadate
group by a.gvkey, a.datadate;
quit;

%end;
%mend;
%create_crsp_controls;

/***************************************************************************
PART 6: FUTURE PERFORMANCE VARIABLES
***************************************************************************/
%macro create_future_performance_vars;
%if not %sysfunc(exist(data.future_performance_vars)) %then %do;

%if not %sysfunc(exist(raw.funda_07242025)) %then %do;
	rsubmit; proc download data=comp.funda out=raw.funda_07242025; run; endrsubmit;
%end;

data work.fut; set data.all_five1 (keep=gvkey datadate rdq fyearq fqtr atq cshfdq epsfiq oancfy ibes_actual_u fset_actual_u zacks_actual_u ciq_actual_u bb_actual_u);
	lead1_fqtr=mod(FQTR,4)+1; lead2_fqtr=mod(FQTR+1,4)+1; lead3_fqtr=mod(FQTR+2,4)+1; lead4_fqtr=FQTR;
	lead1_fyearq=fyearq+(FQTR=4); lead2_fyearq=fyearq+(FQTR>=3); lead3_fyearq=fyearq+(FQTR>=2); lead4_fyearq=fyearq+1;
run;

%macro get_lead(inds=, outds=, lead=);
proc sql;
create table &outds as select a.*, b.oepsxq as lead&lead.oepsxq, b.cshfdq as lead&lead.cshfdq, b.oancfy as lead&lead.oancfy
from &inds a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.lead&lead._fqtr=b.fqtr and a.lead&lead._fyearq=b.fyearq
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C' and not missing(b.ibq) and not missing(b.fqtr);
quit;
proc sort data=&outds; by gvkey datadate descending lead&lead.oepsxq; run;
proc sort data=&outds nodupkey; by gvkey datadate; run;
%mend;

%get_lead(inds=work.fut, outds=work.fut2, lead=1);
%get_lead(inds=work.fut2, outds=work.fut3, lead=2);
%get_lead(inds=work.fut3, outds=work.fut4, lead=3);
%get_lead(inds=work.fut4, outds=work.fut5, lead=4);

data work.fut6; set work.fut5;
	if fyearq=lead1_fyearq then lead1oancfq=lead1oancfy-oancfy; else lead1oancfq=lead1oancfy;
	if lead2_fyearq=lead1_fyearq then lead2oancfq=lead2oancfy-lead1oancfy; else lead2oancfq=lead2oancfy;
	if lead3_fyearq=lead2_fyearq then lead3oancfq=lead3oancfy-lead2oancfy; else lead3oancfq=lead3oancfy;
	if lead4_fyearq=lead3_fyearq then lead4oancfq=lead4oancfy-lead3oancfy; else lead4oancfq=lead4oancfy;
	if atq>0 then do;
		future_op_earn=(lead1oepsxq*lead1cshfdq+lead2oepsxq*lead2cshfdq+lead3oepsxq*lead3cshfdq+lead4oepsxq*lead4cshfdq)/atq;
		future_op_cf=(lead1oancfq+lead2oancfq+lead3oancfq+lead4oancfq)/atq;
		ibes_exclusions=(epsfiq-ibes_actual_u)*cshfdq/atq; fset_exclusions=(epsfiq-fset_actual_u)*cshfdq/atq;
		zacks_exclusions=(epsfiq-zacks_actual_u)*cshfdq/atq; ciq_exclusions=(epsfiq-ciq_actual_u)*cshfdq/atq; bb_exclusions=(epsfiq-bb_actual_u)*cshfdq/atq;
		ibes_earnings=ibes_actual_u*cshfdq/atq; fset_earnings=fset_actual_u*cshfdq/atq;
		zacks_earnings=zacks_actual_u*cshfdq/atq; ciq_earnings=ciq_actual_u*cshfdq/atq; bb_earnings=bb_actual_u*cshfdq/atq;
	end;
run;

proc sql;
create table work.fut7 as select distinct a.*, b.stkcoq, b.aqdq, b.gldq, b.gdwlidq, b.setdq, b.rcdq, b.wddq, b.dtedq, b.rdipdq, b.spidq
from work.fut6 a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.datadate=b.datadate and a.rdq=b.rdq 
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C' and not missing(b.ibq) and not missing(b.fqtr);
quit;

data work.funda; set raw.funda_07242025; where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C' and compst ne "DB"; run;

proc sql;
create table work.fut8 as select distinct a.*, b.am from work.fut7 a left join work.funda b on a.gvkey=b.gvkey and a.fyearq=b.fyear;
quit;
proc sort data=work.fut8; by gvkey datadate descending am; run;
proc sort data=work.fut8 nodupkey; by gvkey datadate; run;

data data.future_performance_vars; set work.fut8;
	if stkcoq=. then stock_compensation=0/atq; else stock_compensation=stkcoq/atq;
	if am=. then amortization=0/atq; else amortization=(am/4)/atq;

	tansitory_pos_value=sum(
	    ifn(aqdq>0 and aqdq ne .,aqdq*cshfdq/atq,0),
	    ifn(gldq>0 and gldq ne .,gldq*cshfdq/atq,0),
	    ifn(gdwlidq>0 and gdwlidq ne .,gdwlidq*cshfdq/atq,0),
	    ifn(setdq>0 and setdq ne .,setdq*cshfdq/atq,0),
	    ifn(rcdq>0 and rcdq ne .,rcdq*cshfdq/atq,0),
	    ifn(wddq>0 and wddq ne .,wddq*cshfdq/atq,0),
	    ifn(dtedq>0 and dtedq ne .,dtedq*cshfdq/atq,0),
	    ifn(rdipdq>0 and rdipdq ne .,rdipdq*cshfdq/atq,0),
	    ifn(spidq>0 and spidq ne .,spidq*cshfdq/atq,0)
	);

tansitory_neg_value=sum(
    ifn(aqdq<0 and aqdq ne .,aqdq*cshfdq/atq,0),
    ifn(gldq<0 and gldq ne .,gldq*cshfdq/atq,0),
    ifn(gdwlidq<0 and gdwlidq ne .,gdwlidq*cshfdq/atq,0),
    ifn(setdq<0 and setdq ne .,setdq*cshfdq/atq,0),
    ifn(rcdq<0 and rcdq ne .,rcdq*cshfdq/atq,0),
    ifn(wddq<0 and wddq ne .,wddq*cshfdq/atq,0),
    ifn(dtedq<0 and dtedq ne .,dtedq*cshfdq/atq,0),
    ifn(rdipdq<0 and rdipdq ne .,rdipdq*cshfdq/atq,0),
    ifn(spidq<0 and spidq ne .,spidq*cshfdq/atq,0)
);	

	keep gvkey datadate future_op_earn future_op_cf ibes_exclusions fset_exclusions zacks_exclusions ciq_exclusions bb_exclusions
	ibes_earnings fset_earnings zacks_earnings ciq_earnings bb_earnings stkcoq stock_compensation amortization tansitory_pos_value tansitory_neg_value;
run;

%end;
%mend;
%create_future_performance_vars;

/***************************************************************************
PART 7: FUTURE STREET EPS VARIABLES
***************************************************************************/
%macro create_future_street_vars;
%if not %sysfunc(exist(data.future_street_vars)) %then %do;

data work.fstreet; set data.all_five1 (keep=gvkey datadate rdq fyearq fqtr atq);
	lead1_fqtr=mod(FQTR,4)+1; lead2_fqtr=mod(FQTR+1,4)+1; lead3_fqtr=mod(FQTR+2,4)+1; lead4_fqtr=FQTR;
	lead1_fyearq=fyearq+(FQTR=4); lead2_fyearq=fyearq+(FQTR>=3); lead3_fyearq=fyearq+(FQTR>=2); lead4_fyearq=fyearq+1;
run;

%macro get_lead_cshfdq(inds=, outds=, lead=);
proc sql;
create table &outds as select a.*, b.cshfdq as lead&lead.cshfdq from &inds a left join raw.fundq_07172025 b
	on a.gvkey=b.gvkey and a.lead&lead._fqtr=b.fqtr and a.lead&lead._fyearq=b.fyearq
	and b.compstq ne "DB" and b.indfmt='INDL' and b.datafmt='STD' and b.popsrc='D' and b.consol='C';
quit;
proc sort data=&outds nodupkey; by gvkey datadate; run;
%mend;

%get_lead_cshfdq(inds=work.fstreet, outds=work.fstreet1a, lead=1);
%get_lead_cshfdq(inds=work.fstreet1a, outds=work.fstreet1b, lead=2);
%get_lead_cshfdq(inds=work.fstreet1b, outds=work.fstreet1c, lead=3);
%get_lead_cshfdq(inds=work.fstreet1c, outds=work.fstreet1d, lead=4);

%macro get_lead_actuals(inds=, outds=, lead=);
proc sql;
create table &outds as select a.*, b.ibes_actual_u as lead&lead._ibes_actual_u, b.fset_actual_u as lead&lead._fset_actual_u,
	b.zacks_actual_u as lead&lead._zacks_actual_u, b.ciq_actual_u as lead&lead._ciq_actual_u, b.bb_actual_u as lead&lead._bb_actual_u
from &inds a left join data.all_five1 b on a.gvkey=b.gvkey and a.lead&lead._fqtr=b.fqtr and a.lead&lead._fyearq=b.fyearq;
quit;
proc sort data=&outds; by gvkey datadate descending lead&lead._ibes_actual_u; run;
proc sort data=&outds nodupkey; by gvkey datadate; run;
%mend;

%get_lead_actuals(inds=work.fstreet1d, outds=work.fstreet2, lead=1);
%get_lead_actuals(inds=work.fstreet2, outds=work.fstreet3, lead=2);
%get_lead_actuals(inds=work.fstreet3, outds=work.fstreet4, lead=3);
%get_lead_actuals(inds=work.fstreet4, outds=work.fstreet5, lead=4);

data data.future_street_vars; set work.fstreet5;
	ibes_future_earnings=(lead1_ibes_actual_u*lead1cshfdq+lead2_ibes_actual_u*lead2cshfdq+lead3_ibes_actual_u*lead3cshfdq+lead4_ibes_actual_u*lead4cshfdq)/atq;
	fset_future_earnings=(lead1_fset_actual_u*lead1cshfdq+lead2_fset_actual_u*lead2cshfdq+lead3_fset_actual_u*lead3cshfdq+lead4_fset_actual_u*lead4cshfdq)/atq;
	zacks_future_earnings=(lead1_zacks_actual_u*lead1cshfdq+lead2_zacks_actual_u*lead2cshfdq+lead3_zacks_actual_u*lead3cshfdq+lead4_zacks_actual_u*lead4cshfdq)/atq;
	ciq_future_earnings=(lead1_ciq_actual_u*lead1cshfdq+lead2_ciq_actual_u*lead2cshfdq+lead3_ciq_actual_u*lead3cshfdq+lead4_ciq_actual_u*lead4cshfdq)/atq;
	bb_future_earnings=(lead1_bb_actual_u*lead1cshfdq+lead2_bb_actual_u*lead2cshfdq+lead3_bb_actual_u*lead3cshfdq+lead4_bb_actual_u*lead4cshfdq)/atq;
	keep gvkey datadate ibes_future_earnings fset_future_earnings zacks_future_earnings ciq_future_earnings bb_future_earnings;
run;

%end;
%mend;
%create_future_street_vars;

/***************************************************************************
PART 8: MERGE ALL INTERMEDIATE DATASETS INTO ALL_FIVE2
***************************************************************************/
proc sql;
create table data.all_five2 as select distinct
	a.*, b.lnmve, b.btm, b.abs_spiq_ibq, b.percent_change_cshfdq, b.percent_change_ibq, b.q4, b.unexpected_item, b.ea_count, b.log_ea_count,
	c.dispersion, c.stock_split, c.lagmins, c.log_lagmins,
	d.guidance, e.io, f.car_0top1, f.ret_vol,
	g.future_op_earn, g.future_op_cf, g.ibes_exclusions, g.fset_exclusions, g.zacks_exclusions, g.ciq_exclusions, g.bb_exclusions,
	g.ibes_earnings, g.fset_earnings, g.zacks_earnings, g.ciq_earnings, g.bb_earnings,
	g.stkcoq, g.stock_compensation, g.amortization, g.tansitory_pos_value, g.tansitory_neg_value,
	h.ibes_future_earnings, h.fset_future_earnings, h.zacks_future_earnings, h.ciq_future_earnings, h.bb_future_earnings
from data.all_five1 a
left join data.compustat_controls b on a.gvkey=b.gvkey and a.datadate=b.datadate
left join data.ibes_controls c on a.gvkey=c.gvkey and a.datadate=c.datadate
left join data.guidance_vars d on a.gvkey=d.gvkey and a.datadate=d.datadate
left join data.io_vars e on a.gvkey=e.gvkey and a.datadate=e.datadate
left join data.crsp_controls f on a.gvkey=f.gvkey and a.datadate=f.datadate
left join data.future_performance_vars g on a.gvkey=g.gvkey and a.datadate=g.datadate
left join data.future_street_vars h on a.gvkey=h.gvkey and a.datadate=h.datadate;
quit;



* BELOW IS THE WRDS CODE USED TO DOWNLOAD Inst Ownership DATA;


/* ********************************************************************************* */
/* ************** W R D S   R E S E A R C H   A P P L I C A T I O N S ************** */
/* ********************************************************************************* */
/* Summary   : Calculate Institutional Ownership, Concentration, and Breadth Ratios  */
/* Author    : Luis Palacios, Rabih Moussawi, and Denys Glushkov                     */
/* Date      : May 18, 2009                                                          */
/* Update    : November 2024 by Freda Drechsler for CRSP CIZ data format             */
/* Variables : - INPUT : Thomson-Reuters 13F Data (TR-13F) S34TYPE3 Holdings data    */
/*                       S34TYPE1 data for FDATE and RDATE variables                 */
/*             - OUTPUT: IO_TimeSeries dataset with IO variables for common stocks   */
/* ********************************************************************************* */

*Wrap the WRDS IO-timeseries build in a macro so we only run it if the
 cached output is not already on disk;
%macro maybe_io_timeseries;
%if not %sysfunc(exist(raw.WRDS_IO_TimeSeries_20251213)) %then %do;
rsubmit;
/* Step1. Specifying Options */
/* Select Date Ranges for CRSP and Thomson Data                   */
%let begdate = 01MAR2000;
%let enddate = 31DEC2024;

/* Create a CRSP Subsample with Monthly Data */
/* Restriction to include common stocks only */
data crsp_m;
set crsp.msf_v2;
where (ShareType='NS' 
and SecurityType='EQTY' 
and SecuritySubType='COM' 
and USIncFlg='Y' 
and issuertype in ('ACOR','CORP')) 
and (%sysfunc(putn("&begdate"d,5.))<=MthCalDt<=%sysfunc(putn("&enddate"d, 5.)));
run;

* Add Cumulative Adjustment Factors;
proc sql;
 create table crsp_m as select distinct
 a.*, b.*
 from crsp_m as a
 left join 
 crsp.stkMthCumulativeAdjFactor as b
 on a.permno = b.permno
 and a.mthcaldt = b.mthcaldt;
quit;
 
/* Adjust Share and Price in Monthly Data              */
/* and Since Thomson 13-F is Quarterly (FDATE & RDATE) */
/* Align CRSP month-end Dates and keep Quarter Ends    */
data crsp_m; format QDATE date9.;
set crsp_m;
QDATE = INTNX('QTR',MthCalDt,0,'E');
DATE = INTNX("MONTH",MthCalDt,0,"E");
P = MthPrc/MthCumFacPr;
TSO=shrout*MthCumFacShr*1000;
if TSO<=0 then TSO=.;
ME = P*TSO/1000000;
label P = "Price at Period End, Adjusted";
label TSO = "Total Shares Outstanding, Adjusted";
label ME = "Market Capitalization, x$1m";
format date date9. MthRet percentn8.4 ME P dollar12.3 TSO comma12.;
run;
 
/* Keep Last Monthly Observation for Each quarter */
data crsp_m;
set crsp_m;
by permno qdate date;
if last.qdate;
drop date;
run;
 
/* Step2. Merge TR-13f S34type1 and S34type3 Sets */
/* First, Keep First Vintage with Holdings Data for Each RDATE-MGRNO Combinations */
proc sql;
create table First_Vint
as select distinct rdate, fdate, mgrno, mgrname
from tfn.s34type1
group by mgrno, rdate
having fdate=min(fdate)
order by mgrno, rdate;
quit;
 
/* Marker for First and Last Quarters of Reporting & Reporting Gaps                        */
/* Exercise Helpful Mostly For Clean Time-Series Analysis                                  */
data First_Vint;
set First_Vint;
by mgrno rdate;
length First_Report 3;
First_Report = (first.mgrno or intck("QTR",lag(rdate),rdate)>1);
run;
 
/* Last Report by Institutional Manager, or Missing 13F Reports in the Next Quarter(s) */
proc sort data=First_Vint nodupkey; by mgrno descending rdate; run;
data First_Vint;
set First_Vint;
by mgrno descending rdate;
length Last_Report 3;
Last_Report = (first.mgrno or intck("QTR",rdate,lag(rdate))>1);
if ("&begdate"d <= rdate <="&enddate"d);
run;
 
/* Add Total Number of 13F Filers During Each Quarter       */
/* undo_policy=none is used to suppress the warning message */
proc sql undo_policy=none;
create table First_Vint
as select distinct *, count(mgrno) as NumInst
from First_Vint
group by rdate
order by fdate, mgrno;
quit;
 
/* Step3. Extract Holdings and Adjust Shares Held */
/* FDATE -Vintage Date- is used in Shares' Adjustment */
data Holdings_v1 / view=Holdings_v1;
merge First_Vint(in=a drop=mgrname)
  tfn.s34type3(in=b drop=type sole shared no);
by fdate mgrno;
if a and b and shares>0;
run;
 
/* Map TR-13F's Historical CUSIP to CRSP Unique Identifier PERMNO        */
/* Keep Securities in CRSP Common Stock Universe                         */
/* Note in CRSP CIZ format, historical CUSIP is named CUSIP (not NCUSIP) */
proc sql;
create view Holdings_v2 as
select distinct a.rdate, a.fdate, a.mgrno, a.NumInst,
        a.first_report, a.last_report, b.permno, a.shares
from Holdings_v1 as a,
   (select distinct cusip, permno from crsp.StkSecurityInfoHist
    where not missing(cusip)) as b
    where a.cusip=b.cusip;
quit;
 
/* Step4. Adjust Shares using CRSP Adjustment Factors aligned at Vintage Dates */
proc sql;
create table Holdings as
select distinct a.rdate, a.mgrno, a.NumInst, a.first_report, a.last_report,
      a.permno, a.shares*b.MthCumFacShr as shares_adj label = "Adjusted Shares Held"
from Holdings_v2 as a, crsp_m as b
where a.permno=b.permno and a.fdate = b.qdate;
quit;
 
/* Sanity Checks for Duplicates - Ultimately, Should be 0 Duplicates */
/* If No Errors, then Duplicates can be due to 2 historical CUSIPs   */
/*    (Separate Holdings by Same Manager) mapping to the same permno */
proc sort data=Holdings nodupkey; by permno rdate mgrno; run;
proc sort data=crsp_m   nodupkey; by permno qdate;       run;
 
/* Step5. Calculate Institutional Measures at the Security Level */
proc means data=Holdings noprint;
where shares_adj>0;
by permno rdate;
var shares_adj first_report;
output out=IO_Metrics (drop=_freq_ _type_)
       n=NumOwners max(NumInst)=NumInst
       sum(first_report)=NewInst sum(last_report)=OldInst
       sum(shares_adj)=IO_TOTAL USS(shares_adj)=IO_SS;
run;
 
/* Changes in Institutional Breadth: Lehavy and Sloan (2008) Calculation               */
/* DBREADTH Condition: institutions should exist in Q(t) & Q(t-1)                      */
/* Objective: Mitigate Bias due to Universe Changes - $100M AUM Filing Threshold       */
/* DBREADTH=((NumInst(t)-NewInst(t))-(Numinst(t-1)-OldInst(t-1)) divided by            */
/*                  Total Number of 13F filers in quarter (t-1))                       */
/*  where,                                                                             */
/*  . NewInst(t): Number of 13F filers that reported in t, but did not report in (t-1) */
/*  . OldInst(t): Number of 13F filers that reported in (t-1), but did not report in t */
/*  . (NumOwners(t)-NewInst(t)): Number of 13F filers holding security in quarter t,   */
/*                  that have reported in both quarters t and t-1                      */
/*  . (NumOwners(t-1)-OldInst(t-1)): number of 13F filers that held the security       */
/*                  in quarter (t-1), and have reported in both quarters t and t-1     */
/*                                                                                     */
/* Calculate IO DBreadth and Concentration Metrics                                     */
data IO_Metrics;
set IO_Metrics;
by permno rdate;
IOC_HHI = IO_SS/(IO_TOTAL**2);
DBREADTH = ( (NumOwners - NewInst) - lag(NumOwners-OldInst) ) / lag(NumInst);
if first.permno then DBREADTH=.;
label NumOwners  = "Breadth - # of 13-F Institutional Owners";
label IO_TOTAL = "Institutional Ownership, Total - Adjusted";
label IOC_HHI   = "IO Concentration - Herfindahl- Hirschman Index";
label DBREADTH = "Change in IO Breadth, Percent";
drop NumInst IO_SS NewInst OldInst;
run;
 
/* Step6. Add CRSP Market Data to Holdings at Calendar Quarter Ends */
/* Note: a Right Join is Necessary to Identify Common Stock with no 13F Data */
data IO_TimeSeries;
merge IO_Metrics(in=a) crsp_m (in=b rename=(qdate=rdate));
by permno rdate;
if b and TSO>0;
IOR = IO_TOTAL/TSO;
if missing(IOR) then IOR=0;
IO_MISSING = (not a);
IO_G1      = (IOR>1);
label IOR = "Institutional Ownership Ratio";
label IO_MISSING = "Missing (or NA) 13-F Data";
label IO_G1 = "IOR % > 1";
format IO_TOTAL NumOwners comma16. IOR DBREADTH IOC_HHI percentn8.2;
run;
 

proc download data = IO_TimeSeries out=raw.WRDS_IO_TimeSeries_20251213; run;
endrsubmit;
%end;
%mend;
%maybe_io_timeseries;

*Signoff only if we signed on at the top;
%macro maybe_signoff;
  %if &need_wrds = 1 %then %do;
    signoff;
  %end;
%mend;
%maybe_signoff;

