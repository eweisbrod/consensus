/***************************************************************************
 * 005-compute-fdp-quality-salience-variables.sas
 *
 * Purpose:
 *   Compute FDP-quality and salience variables (agreement, experience,
 *   accuracy, persistence) per forecast-data provider (IBES, FactSet,
 *   Zacks, CIQ, Bloomberg) and append them to the firm-quarter dataset
 *   built by 004.
 *
 * Inputs (from DATA_DIR):
 *   all_five1.sas7bdat                      (output of 002, used as the
 *                                            broader sample for accuracy /
 *                                            experience history)
 *   all_five3.sas7bdat                      (output of 004)
 *
 * Outputs (to DATA_DIR):
 *   fdp_agreement_vars.sas7bdat             (intermediate, persistent)
 *   fdp_experience_vars.sas7bdat            (intermediate, persistent)
 *   fdp_accuracy_vars.sas7bdat              (intermediate, persistent)
 *   fdp_accuracy_vars_no_bb.sas7bdat        (intermediate variant, persistent)
 *   fdp_accuracy_vars_no_zacks.sas7bdat     (intermediate variant, persistent)
 *   fdp_persistence_vars.sas7bdat           (intermediate, persistent)
 *   all_five4.sas7bdat                      (final firm-quarter dataset
 *                                            with FDP-quality variables)
 *
 * Notes:
 *   - Reads RAW_DATA_DIR and DATA_DIR from .env via %load_env (defined in MACROS.sas).
 *   - Pure compute -- no WRDS downloads, no SIGNON.
 *   - Each PART is wrapped in a macro with an existence-check on its
 *     persistent output, so re-running the script skips already-built
 *     intermediates. To rebuild a specific intermediate, delete it from
 *     DATA_DIR first.
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

/***************************************************************************
PART 1: FDP AGREEMENT
Count how many other FDPs each FDP agrees with on actual EPS (within 1.5 cents)
Creates: data.fdp_agreement_vars
***************************************************************************/
%macro create_fdp_agreement_vars;
%if not %sysfunc(exist(data.fdp_agreement_vars)) %then %do;

data work.agreement; set data.all_five3;
	*Pairwise agreement indicators (within 1.5 cents);
	ibes_fset_diff=abs(ibes_actual_u-fset_actual_u);
	if ibes_fset_diff ne . and ibes_fset_diff<0.015 then ibes_fset_act_agree=1; else if ibes_fset_diff>=0.015 then ibes_fset_act_agree=0;
	
	ibes_zacks_diff=abs(ibes_actual_u-zacks_actual_u);
	if ibes_zacks_diff ne . and ibes_zacks_diff<0.015 then ibes_zacks_act_agree=1; else if ibes_zacks_diff>=0.015 then ibes_zacks_act_agree=0;
	
	ibes_ciq_diff=abs(ibes_actual_u-ciq_actual_u);
	if ibes_ciq_diff ne . and ibes_ciq_diff<0.015 then ibes_ciq_act_agree=1; else if ibes_ciq_diff>=0.015 then ibes_ciq_act_agree=0;
	
	ibes_bb_diff=abs(ibes_actual_u-bb_actual_u);
	if ibes_bb_diff ne . and ibes_bb_diff<0.015 then ibes_bb_act_agree=1; else if ibes_bb_diff>=0.015 then ibes_bb_act_agree=0;
	
	fset_zacks_diff=abs(fset_actual_u-zacks_actual_u);
	if fset_zacks_diff ne . and fset_zacks_diff<0.015 then fset_zacks_act_agree=1; else if fset_zacks_diff>=0.015 then fset_zacks_act_agree=0;
	
	fset_ciq_diff=abs(fset_actual_u-ciq_actual_u);
	if fset_ciq_diff ne . and fset_ciq_diff<0.015 then fset_ciq_act_agree=1; else if fset_ciq_diff>=0.015 then fset_ciq_act_agree=0;
	
	fset_bb_diff=abs(fset_actual_u-bb_actual_u);
	if fset_bb_diff ne . and fset_bb_diff<0.015 then fset_bb_act_agree=1; else if fset_bb_diff>=0.015 then fset_bb_act_agree=0;
	
	zacks_ciq_diff=abs(zacks_actual_u-ciq_actual_u);
	if zacks_ciq_diff ne . and zacks_ciq_diff<0.015 then zacks_ciq_act_agree=1; else if zacks_ciq_diff>=0.015 then zacks_ciq_act_agree=0;
	
	zacks_bb_diff=abs(zacks_actual_u-bb_actual_u);
	if zacks_bb_diff ne . and zacks_bb_diff<0.015 then zacks_bb_act_agree=1; else if zacks_bb_diff>=0.015 then zacks_bb_act_agree=0;
	
	ciq_bb_diff=abs(ciq_actual_u-bb_actual_u);
	if ciq_bb_diff ne . and ciq_bb_diff<0.015 then ciq_bb_act_agree=1; else if ciq_bb_diff>=0.015 then ciq_bb_act_agree=0;

	*Count of other FDPs each FDP agrees with (0-4, missing if any pairwise comparison is missing);
	ibes_act_u_fdpagree = ibes_fset_act_agree + ibes_zacks_act_agree + ibes_ciq_act_agree + ibes_bb_act_agree;
	fset_act_u_fdpagree = ibes_fset_act_agree + fset_zacks_act_agree + fset_ciq_act_agree + fset_bb_act_agree;
	zacks_act_u_fdpagree = ibes_zacks_act_agree + fset_zacks_act_agree + zacks_ciq_act_agree + zacks_bb_act_agree;
	ciq_act_u_fdpagree = ibes_ciq_act_agree + fset_ciq_act_agree + zacks_ciq_act_agree + ciq_bb_act_agree;
	bb_act_u_fdpagree = ibes_bb_act_agree + fset_bb_act_agree + zacks_bb_act_agree + ciq_bb_act_agree;
	ibes_act_u_fdpagree_no_bb = ibes_fset_act_agree + ibes_zacks_act_agree + ibes_ciq_act_agree;
	fset_act_u_fdpagree_no_bb = ibes_fset_act_agree + fset_zacks_act_agree + fset_ciq_act_agree;
	zacks_act_u_fdpagree_no_bb = ibes_zacks_act_agree + fset_zacks_act_agree + zacks_ciq_act_agree;
	ciq_act_u_fdpagree_no_bb = ibes_ciq_act_agree + fset_ciq_act_agree + zacks_ciq_act_agree;

	ibes_act_u_fdpagree_no_zacks = ibes_fset_act_agree + ibes_ciq_act_agree + ibes_bb_act_agree;
	fset_act_u_fdpagree_no_zacks = ibes_fset_act_agree + fset_ciq_act_agree + fset_bb_act_agree;
	bb_act_u_fdpagree_no_zacks = ibes_bb_act_agree + fset_bb_act_agree + ciq_bb_act_agree;
	ciq_act_u_fdpagree_no_zacks = ibes_ciq_act_agree + fset_ciq_act_agree + ciq_bb_act_agree;

	keep gvkey datadate ibes_act_u_fdpagree fset_act_u_fdpagree zacks_act_u_fdpagree ciq_act_u_fdpagree bb_act_u_fdpagree
				ibes_act_u_fdpagree_no_bb  fset_act_u_fdpagree_no_bb zacks_act_u_fdpagree_no_bb ciq_act_u_fdpagree_no_bb
				ibes_act_u_fdpagree_no_zacks fset_act_u_fdpagree_no_zacks bb_act_u_fdpagree_no_zacks ciq_act_u_fdpagree_no_zacks;
run;

data data.fdp_agreement_vars; set work.agreement; run;

%end;
%mend;
%create_fdp_agreement_vars;

/***************************************************************************
PART 2: FDP EXPERIENCE
Count quarters each FDP has followed the firm prior to current quarter
Creates: data.fdp_experience_vars
***************************************************************************/
%macro create_fdp_experience_vars;
%if not %sysfunc(exist(data.fdp_experience_vars)) %then %do;

data work.experience; set data.all_five1; 
	keep gvkey datadate ibes_covered fset_covered zacks_covered ciq_covered bb_covered;
run;

proc sql;
create table work.experience2 as select distinct 
	a.gvkey, a.datadate,
	sum(b.ibes_covered) as ibes_quarters_followed, 
	sum(b.fset_covered) as fset_quarters_followed,
	sum(b.zacks_covered) as zacks_quarters_followed, 
	sum(b.ciq_covered) as ciq_quarters_followed,
	sum(b.bb_covered) as bb_quarters_followed
from work.experience a left join work.experience b
	on a.gvkey=b.gvkey and a.datadate>b.datadate
group by a.gvkey, a.datadate;
quit;

data data.fdp_experience_vars; set work.experience2;
	if ibes_quarters_followed=. then ibes_quarters_followed=0;
	if fset_quarters_followed=. then fset_quarters_followed=0;
	if zacks_quarters_followed=. then zacks_quarters_followed=0;
	if ciq_quarters_followed=. then ciq_quarters_followed=0;
	if bb_quarters_followed=. then bb_quarters_followed=0;
run;

%end;
%mend;
%create_fdp_experience_vars;

/***************************************************************************
PART 3: HISTORICAL ACCURACY
Rank FDPs by average accuracy over prior 20 quarters
Creates: data.fdp_accuracy_vars
***************************************************************************/
%macro create_fdp_accuracy_vars;
%if not %sysfunc(exist(data.fdp_accuracy_vars)) %then %do;

*Calculate unsigned forecast error scaled by price;
data work.accuracy; set data.all_five3;
	ibes_accuracy=(abs(ibes_surp_u)/prcn2)*-1;
	zacks_accuracy=(abs(zacks_surp_u)/prcn2)*-1;
	ciq_accuracy=(abs(ciq_surp_u)/prcn2)*-1;
	bb_accuracy=(abs(bb_surp_u)/prcn2)*-1;
	fset_accuracy=(abs(fset_surp_u)/prcn2)*-1;
run;

*Limit to firm-quarters where all FDPs have accuracy;
data work.accuracy2; set work.accuracy;
	where ibes_accuracy ne . and zacks_accuracy ne . and ciq_accuracy ne . and bb_accuracy ne . and fset_accuracy ne .;
run;

*Reshape to FDP-firm-quarter level;
data work.accuracy_long; set work.accuracy2;
	fdp_num=1; accuracy=ibes_accuracy; output;
	fdp_num=2; accuracy=zacks_accuracy; output;
	fdp_num=3; accuracy=ciq_accuracy; output;
	fdp_num=4; accuracy=bb_accuracy; output;
	fdp_num=5; accuracy=fset_accuracy; output;
	keep gvkey datadate fdp_num accuracy;
run;

*Rank FDPs within each firm-quarter by current accuracy;
proc sort data=work.accuracy_long; by gvkey datadate; run;
proc rank data=work.accuracy_long out=work.accuracy_ranked ties=high;
	var accuracy; ranks rank_accuracy; by gvkey datadate;
run;
data work.accuracy_ranked; set work.accuracy_ranked; rank_accuracy=(rank_accuracy-1)/4; run;

*Get all prior quarter accuracy ranks (within 20 quarters);
proc sql;
create table work.accuracy_lagged as select distinct 
	a.gvkey, a.datadate, a.fdp_num, b.rank_accuracy as lag_rank_accuracy, b.datadate as datadate_lag
from work.accuracy_ranked a left join work.accuracy_ranked b
	on a.gvkey=b.gvkey and a.fdp_num=b.fdp_num and a.datadate>b.datadate;
quit;

proc rank data=work.accuracy_lagged descending out=work.accuracy_lagged2;
	var datadate_lag; ranks lag_rank; by gvkey datadate fdp_num;
run;

*Keep only prior 20 quarters;
data work.accuracy_lagged3; set work.accuracy_lagged2; where 0<lag_rank<=20; run;

*Calculate mean accuracy rank over prior quarters;
proc sort data=work.accuracy_lagged3; by gvkey datadate fdp_num; run;
proc means data=work.accuracy_lagged3 noprint;
	var lag_rank_accuracy; by gvkey datadate fdp_num;
	output out=work.mean_lag_accuracy mean=avg_lag_accuracy_rank;
run;

*Rank FDPs within firm-quarter by average historical accuracy;
proc rank data=work.mean_lag_accuracy out=work.mean_lag_accuracy2 ties=high;
	var avg_lag_accuracy_rank; ranks rank_lag_accuracy; by gvkey datadate;
run;
data work.mean_lag_accuracy2; set work.mean_lag_accuracy2; rank_lag_accuracy=(rank_lag_accuracy-1)/4; run;

*Reshape back to wide format;
proc sql;
create table data.fdp_accuracy_vars as select distinct
	coalesce(a.gvkey,b.gvkey,c.gvkey,d.gvkey,e.gvkey) as gvkey,
	coalesce(a.datadate,b.datadate,c.datadate,d.datadate,e.datadate) as datadate format date9.,
	a.rank_lag_accuracy as ibes_rank_lag_accuracy,
	b.rank_lag_accuracy as zacks_rank_lag_accuracy,
	c.rank_lag_accuracy as ciq_rank_lag_accuracy,
	d.rank_lag_accuracy as bb_rank_lag_accuracy,
	e.rank_lag_accuracy as fset_rank_lag_accuracy
from work.mean_lag_accuracy2 (where=(fdp_num=1)) a
full join work.mean_lag_accuracy2 (where=(fdp_num=2)) b on a.gvkey=b.gvkey and a.datadate=b.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=3)) c on a.gvkey=c.gvkey and a.datadate=c.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=4)) d on a.gvkey=d.gvkey and a.datadate=d.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=5)) e on a.gvkey=e.gvkey and a.datadate=e.datadate;
quit;

%end;
%mend;
%create_fdp_accuracy_vars;

/***************************************************************************
PART 3: HISTORICAL ACCURACY NO BB
Rank FDPs by average accuracy over prior 20 quarters
Creates: data.fdp_accuracy_vars_no_bb
***************************************************************************/

%macro create_fdp_accuracy_vars_no_bb;
%if not %sysfunc(exist(data.fdp_accuracy_vars_no_bb)) %then %do;

*Calculate unsigned forecast error scaled by price;
data work.accuracy; set data.all_five3;
	ibes_accuracy=(abs(ibes_surp_u)/prcn2)*-1;
	zacks_accuracy=(abs(zacks_surp_u)/prcn2)*-1;
	ciq_accuracy=(abs(ciq_surp_u)/prcn2)*-1;
	fset_accuracy=(abs(fset_surp_u)/prcn2)*-1;
run;

*Limit to firm-quarters where all FDPs have accuracy;
data work.accuracy2; set work.accuracy;
	where ibes_accuracy ne . and zacks_accuracy ne . and ciq_accuracy ne . and fset_accuracy ne .;
run;

*Reshape to FDP-firm-quarter level;
data work.accuracy_long; set work.accuracy2;
	fdp_num=1; accuracy=ibes_accuracy; output;
	fdp_num=2; accuracy=zacks_accuracy; output;
	fdp_num=3; accuracy=ciq_accuracy; output;
	fdp_num=4; accuracy=fset_accuracy; output;
	keep gvkey datadate fdp_num accuracy;
run;

*Rank FDPs within each firm-quarter by current accuracy;
proc sort data=work.accuracy_long; by gvkey datadate; run;
proc rank data=work.accuracy_long out=work.accuracy_ranked ties=high;
	var accuracy; ranks rank_accuracy; by gvkey datadate;
run;
data work.accuracy_ranked; set work.accuracy_ranked; rank_accuracy=(rank_accuracy-1)/3; run;

*Get all prior quarter accuracy ranks (within 20 quarters);
proc sql;
create table work.accuracy_lagged as select distinct 
	a.gvkey, a.datadate, a.fdp_num, b.rank_accuracy as lag_rank_accuracy, b.datadate as datadate_lag
from work.accuracy_ranked a left join work.accuracy_ranked b
	on a.gvkey=b.gvkey and a.fdp_num=b.fdp_num and a.datadate>b.datadate;
quit;

proc rank data=work.accuracy_lagged descending out=work.accuracy_lagged2;
	var datadate_lag; ranks lag_rank; by gvkey datadate fdp_num;
run;

*Keep only prior 20 quarters;
data work.accuracy_lagged3; set work.accuracy_lagged2; where 0<lag_rank<=20; run;

*Calculate mean accuracy rank over prior quarters;
proc sort data=work.accuracy_lagged3; by gvkey datadate fdp_num; run;
proc means data=work.accuracy_lagged3 noprint;
	var lag_rank_accuracy; by gvkey datadate fdp_num;
	output out=work.mean_lag_accuracy mean=avg_lag_accuracy_rank;
run;

*Rank FDPs within firm-quarter by average historical accuracy;
proc rank data=work.mean_lag_accuracy out=work.mean_lag_accuracy2 ties=high;
	var avg_lag_accuracy_rank; ranks rank_lag_accuracy; by gvkey datadate;
run;
data work.mean_lag_accuracy2; set work.mean_lag_accuracy2; rank_lag_accuracy=(rank_lag_accuracy-1)/3; run;

*Reshape back to wide format;
proc sql;
create table data.fdp_accuracy_vars_no_bb as select distinct
	coalesce(a.gvkey,b.gvkey,c.gvkey,d.gvkey) as gvkey,
	coalesce(a.datadate,b.datadate,c.datadate,d.datadate) as datadate format date9.,
	a.rank_lag_accuracy as ibes_rank_lag_accuracy_no_bb,
	b.rank_lag_accuracy as zacks_rank_lag_accuracy_no_bb,
	c.rank_lag_accuracy as ciq_rank_lag_accuracy_no_bb,
	d.rank_lag_accuracy as fset_rank_lag_accuracy_no_bb
from work.mean_lag_accuracy2 (where=(fdp_num=1)) a
full join work.mean_lag_accuracy2 (where=(fdp_num=2)) b on a.gvkey=b.gvkey and a.datadate=b.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=3)) c on a.gvkey=c.gvkey and a.datadate=c.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=4)) d on a.gvkey=d.gvkey and a.datadate=d.datadate;
quit;

%end;
%mend;
%create_fdp_accuracy_vars_no_bb;

/***************************************************************************
PART 3B: HISTORICAL ACCURACY NO ZACKS
Rank FDPs by average accuracy over prior 20 quarters
Creates: data.fdp_accuracy_vars_no_zacks
***************************************************************************/

%macro create_fdp_accuracy_nozacks;
%if not %sysfunc(exist(data.fdp_accuracy_vars_no_zacks)) %then %do;

*Calculate unsigned forecast error scaled by price;
data work.accuracy; set data.all_five3;
	ibes_accuracy=(abs(ibes_surp_u)/prcn2)*-1;
	bb_accuracy=(abs(bb_surp_u)/prcn2)*-1;
	ciq_accuracy=(abs(ciq_surp_u)/prcn2)*-1;
	fset_accuracy=(abs(fset_surp_u)/prcn2)*-1;
run;

*Limit to firm-quarters where all FDPs have accuracy;
data work.accuracy2; set work.accuracy;
	where ibes_accuracy ne . and bb_accuracy ne . and ciq_accuracy ne . and fset_accuracy ne .;
run;

*Reshape to FDP-firm-quarter level;
data work.accuracy_long; set work.accuracy2;
	fdp_num=1; accuracy=ibes_accuracy; output;
	fdp_num=3; accuracy=ciq_accuracy; output;
	fdp_num=4; accuracy=bb_accuracy; output;
	fdp_num=5; accuracy=fset_accuracy; output;
	keep gvkey datadate fdp_num accuracy;
run;

*Rank FDPs within each firm-quarter by current accuracy;
proc sort data=work.accuracy_long; by gvkey datadate; run;
proc rank data=work.accuracy_long out=work.accuracy_ranked ties=high;
	var accuracy; ranks rank_accuracy; by gvkey datadate;
run;
data work.accuracy_ranked; set work.accuracy_ranked; rank_accuracy=(rank_accuracy-1)/3; run;

*Get all prior quarter accuracy ranks (within 20 quarters);
proc sql;
create table work.accuracy_lagged as select distinct
	a.gvkey, a.datadate, a.fdp_num, b.rank_accuracy as lag_rank_accuracy, b.datadate as datadate_lag
from work.accuracy_ranked a left join work.accuracy_ranked b
	on a.gvkey=b.gvkey and a.fdp_num=b.fdp_num and a.datadate>b.datadate;
quit;

proc rank data=work.accuracy_lagged descending out=work.accuracy_lagged2;
	var datadate_lag; ranks lag_rank; by gvkey datadate fdp_num;
run;

*Keep only prior 20 quarters;
data work.accuracy_lagged3; set work.accuracy_lagged2; where 0<lag_rank<=20; run;

*Calculate mean accuracy rank over prior quarters;
proc sort data=work.accuracy_lagged3; by gvkey datadate fdp_num; run;
proc means data=work.accuracy_lagged3 noprint;
	var lag_rank_accuracy; by gvkey datadate fdp_num;
	output out=work.mean_lag_accuracy mean=avg_lag_accuracy_rank;
run;

*Rank FDPs within firm-quarter by average historical accuracy;
proc rank data=work.mean_lag_accuracy out=work.mean_lag_accuracy2 ties=high;
	var avg_lag_accuracy_rank; ranks rank_lag_accuracy; by gvkey datadate;
run;
data work.mean_lag_accuracy2; set work.mean_lag_accuracy2; rank_lag_accuracy=(rank_lag_accuracy-1)/3; run;

*Reshape back to wide format;
proc sql;
create table data.fdp_accuracy_vars_no_zacks as select distinct
	coalesce(a.gvkey,b.gvkey,c.gvkey,d.gvkey) as gvkey,
	coalesce(a.datadate,b.datadate,c.datadate,d.datadate) as datadate format date9.,
	a.rank_lag_accuracy as ibes_rank_lag_accuracy_no_zacks,
	b.rank_lag_accuracy as ciq_rank_lag_accuracy_no_zacks,
	c.rank_lag_accuracy as bb_rank_lag_accuracy_no_zacks,
	d.rank_lag_accuracy as fset_rank_lag_accuracy_no_zacks
from work.mean_lag_accuracy2 (where=(fdp_num=1)) a
full join work.mean_lag_accuracy2 (where=(fdp_num=3)) b
	on a.gvkey=b.gvkey and a.datadate=b.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=4)) c
	on a.gvkey=c.gvkey and a.datadate=c.datadate
full join work.mean_lag_accuracy2 (where=(fdp_num=5)) d
	on a.gvkey=d.gvkey and a.datadate=d.datadate;
quit;

%end;
%mend;
%create_fdp_accuracy_nozacks;


/***************************************************************************
PART 4: HISTORICAL PERSISTENCE
Rolling regressions of future earnings/CF on FDP earnings over prior 20 quarters
Creates: data.fdp_persistence_vars
***************************************************************************/
%macro create_fdp_persistence_vars;
%if not %sysfunc(exist(data.fdp_persistence_vars)) %then %do;

*Prepare regression data;
data work.regdata; set data.all_five3;
	array myvars(*) ibes_earnings zacks_earnings ciq_earnings bb_earnings fset_earnings future_op_earn future_op_cf;
	do i=1 to dim(myvars); if missing(myvars(i)) then delete; end;
	keep gvkey datadate fqtr fyearq yearqtr ibes_earnings zacks_earnings ciq_earnings bb_earnings fset_earnings future_op_earn future_op_cf;
run;

*Winsorize regression variables by year-quarter;
%winsor(dsetin=work.regdata, dsetout=work.regdataw, byvar=yearqtr, 
	vars=ibes_earnings zacks_earnings ciq_earnings bb_earnings fset_earnings future_op_earn future_op_cf, type=W, pctl=1 99);

*Run rolling regressions over 20-quarter windows;
%macro rolling_regs;
%do i=0 %to 84;
	data work.regsample; set work.regdataw;
		where yearqtr>%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i-20),end)) 
		  and yearqtr<=%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i),end));
	run;
	proc sort data=work.regsample; by gvkey datadate; run;

	*Earnings persistence regressions;
	options nonotes nomprint msglevel=N;
	proc reg data=work.regsample outest=work.ibes_coefs noprint edf; by gvkey; model future_op_earn=ibes_earnings; quit;
	proc reg data=work.regsample outest=work.zacks_coefs noprint edf; by gvkey; model future_op_earn=zacks_earnings; quit;
	proc reg data=work.regsample outest=work.ciq_coefs noprint edf; by gvkey; model future_op_earn=ciq_earnings; quit;
	proc reg data=work.regsample outest=work.bb_coefs noprint edf; by gvkey; model future_op_earn=bb_earnings; quit;
	proc reg data=work.regsample outest=work.fset_coefs noprint edf; by gvkey; model future_op_earn=fset_earnings; quit;
	options notes mprint msglevel=I;

	proc sql;
	create table work.earn_coefs&i as select distinct a.gvkey, 
		%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i),end)) as yearqtr format date9.,
		%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i+4),end)) as linkqtr format date9.,
		a._edf_+2 as nobs, a.ibes_earnings as ibes_coef_earn, b.zacks_earnings as zacks_coef_earn,
		c.ciq_earnings as ciq_coef_earn, d.bb_earnings as bb_coef_earn, e.fset_earnings as fset_coef_earn
	from work.ibes_coefs a
	inner join work.zacks_coefs b on a.gvkey=b.gvkey
	inner join work.ciq_coefs c on a.gvkey=c.gvkey
	inner join work.bb_coefs d on a.gvkey=d.gvkey
	inner join work.fset_coefs e on a.gvkey=e.gvkey
	where a._edf_>=2;
	quit;

	*Cash flow persistence regressions;
	options nonotes nomprint msglevel=N;
	proc reg data=work.regsample outest=work.ibes_coefs noprint edf; by gvkey; model future_op_cf=ibes_earnings; quit;
	proc reg data=work.regsample outest=work.zacks_coefs noprint edf; by gvkey; model future_op_cf=zacks_earnings; quit;
	proc reg data=work.regsample outest=work.ciq_coefs noprint edf; by gvkey; model future_op_cf=ciq_earnings; quit;
	proc reg data=work.regsample outest=work.bb_coefs noprint edf; by gvkey; model future_op_cf=bb_earnings; quit;
	proc reg data=work.regsample outest=work.fset_coefs noprint edf; by gvkey; model future_op_cf=fset_earnings; quit;
	options notes mprint msglevel=I;

	proc sql;
	create table work.cf_coefs&i as select distinct a.gvkey, 
		%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i),end)) as yearqtr format date9.,
		%sysfunc(intnx(QTR,"31DEC2002"d,%eval(&i+4),end)) as linkqtr format date9.,
		a._edf_+2 as nobs, a.ibes_earnings as ibes_coef_cf, b.zacks_earnings as zacks_coef_cf,
		c.ciq_earnings as ciq_coef_cf, d.bb_earnings as bb_coef_cf, e.fset_earnings as fset_coef_cf
	from work.ibes_coefs a
	inner join work.zacks_coefs b on a.gvkey=b.gvkey
	inner join work.ciq_coefs c on a.gvkey=c.gvkey
	inner join work.bb_coefs d on a.gvkey=d.gvkey
	inner join work.fset_coefs e on a.gvkey=e.gvkey
	where a._edf_>=2;
	quit;
%end;
%mend;
%rolling_regs;

*Combine regression outputs;
data work.earn_coefs; set work.earn_coefs0-work.earn_coefs84; run;
data work.cf_coefs; set work.cf_coefs0-work.cf_coefs84; run;

*Merge earnings and CF coefficients;
proc sql;
create table data.fdp_persistence_vars as select distinct
	coalesce(a.gvkey,b.gvkey) as gvkey, coalesce(a.linkqtr,b.linkqtr) as linkqtr format date9.,
	a.ibes_coef_earn, a.zacks_coef_earn, a.ciq_coef_earn, a.bb_coef_earn, a.fset_coef_earn,
	b.ibes_coef_cf, b.zacks_coef_cf, b.ciq_coef_cf, b.bb_coef_cf, b.fset_coef_cf
from work.earn_coefs a
full join work.cf_coefs b on a.gvkey=b.gvkey and a.linkqtr=b.linkqtr;
quit;

%end;
%mend;
%create_fdp_persistence_vars;

/***************************************************************************
PART 5: MERGE ALL INTERMEDIATE DATASETS INTO ALL_FIVE4
***************************************************************************/
proc sql;
create table data.all_five4 as select distinct
	a.*,
	b.ibes_act_u_fdpagree, b.fset_act_u_fdpagree, b.zacks_act_u_fdpagree, b.ciq_act_u_fdpagree, b.bb_act_u_fdpagree,
	b.ibes_act_u_fdpagree_no_bb, b.fset_act_u_fdpagree_no_bb, b.zacks_act_u_fdpagree_no_bb, b.ciq_act_u_fdpagree_no_bb,
	b.ibes_act_u_fdpagree_no_zacks, b.fset_act_u_fdpagree_no_zacks, b.bb_act_u_fdpagree_no_zacks, b.ciq_act_u_fdpagree_no_zacks,
	c.ibes_quarters_followed, c.fset_quarters_followed, c.zacks_quarters_followed, c.ciq_quarters_followed, c.bb_quarters_followed,
	d.ibes_rank_lag_accuracy, d.zacks_rank_lag_accuracy, d.ciq_rank_lag_accuracy, d.bb_rank_lag_accuracy, d.fset_rank_lag_accuracy,
	d2.ibes_rank_lag_accuracy_no_bb, d2.zacks_rank_lag_accuracy_no_bb, d2.ciq_rank_lag_accuracy_no_bb, d2.fset_rank_lag_accuracy_no_bb,
	d3.ibes_rank_lag_accuracy_no_zacks, d3.ciq_rank_lag_accuracy_no_zacks, d3.bb_rank_lag_accuracy_no_zacks, d3.fset_rank_lag_accuracy_no_zacks,
	e.ibes_coef_earn, e.zacks_coef_earn, e.ciq_coef_earn, e.bb_coef_earn, e.fset_coef_earn,
	e.ibes_coef_cf, e.zacks_coef_cf, e.ciq_coef_cf, e.bb_coef_cf, e.fset_coef_cf
from data.all_five3 a
left join data.fdp_agreement_vars b on a.gvkey=b.gvkey and a.datadate=b.datadate
left join data.fdp_experience_vars c on a.gvkey=c.gvkey and a.datadate=c.datadate
left join data.fdp_accuracy_vars d on a.gvkey=d.gvkey and a.datadate=d.datadate
left join data.fdp_accuracy_vars_no_bb d2 on a.gvkey=d2.gvkey and a.datadate=d2.datadate
left join data.fdp_accuracy_vars_no_zacks d3 on a.gvkey=d3.gvkey and a.datadate=d3.datadate
left join data.fdp_persistence_vars e on a.gvkey=e.gvkey and a.yearqtr=e.linkqtr;
quit;

