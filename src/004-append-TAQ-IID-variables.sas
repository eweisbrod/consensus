/***************************************************************************
 * 004-append-TAQ-IID-variables.sas
 *
 * Purpose:
 *   Append TAQ-IID liquidity variables (effective spread, depth, price
 *   impact, retail volume) to the firm-quarter dataset built by 003.
 *   Reads pre-downloaded annual TAQ-IID millisecond files (one per year
 *   2003-2025), combines them, and computes event-window vs.
 *   non-event-window averages around each earnings announcement to
 *   derive abnormal-liquidity measures.
 *
 * Inputs (from RAW_DATA_DIR):
 *   dsf_07172025.sas7bdat                   (CRSP daily stock file, used to
 *                                            build a trading-day calendar)
 *   taqmclink_07242025.sas7bdat             (WRDS TAQ-CRSP link;
 *                                            downloaded by this script if absent)
 *   iid_msec_<YYYY>.sas7bdat                (TAQ-IID millisecond file per
 *                                            year 2003-2025; downloaded
 *                                            by this script if absent)
 *
 * Inputs (from DATA_DIR):
 *   all_five2.sas7bdat                      (output of 003)
 *
 * Outputs (to DATA_DIR):
 *   iid_2003to2025_millisecond.sas7bdat     (intermediate: combined TAQ-IID
 *                                            data restricted to firm-days in
 *                                            the sample; rebuilt every run)
 *   taq_liquidity_vars.sas7bdat             (intermediate: per-firm-quarter
 *                                            event/non-event averages)
 *   all_five3.sas7bdat                      (final firm-quarter dataset
 *                                            with TAQ-IID variables)
 *
 * Notes:
 *   - Reads RAW_DATA_DIR and DATA_DIR from .env via %load_env (defined in MACROS.sas).
 *   - WRDS downloads are wrapped with %if not %sysfunc(exist(...)) checks
 *     so a re-run with the .sas7bdat already on disk skips the download.
 *   - SIGNON is conditional on whether any WRDS-derived raw file is missing.
 *   - The IID-millisecond annual files are large (each ~3 GB), so all 23
 *     annual files together are ~70 GB. They live in raw_data alongside
 *     other raw inputs.
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
%check_need(target=raw.taqmclink_07242025);
%macro check_iid_years;
  %do i = 2003 %to 2025;
    %check_need(target=raw.iid_msec_&i);
  %end;
%mend;
%check_iid_years;

%if &need_wrds = 1 %then %do;
  %let wrds = wrds-cloud.wharton.upenn.edu 4016;
  options comamid=TCP remote=WRDS;
  signon username=_prompt_;
%end;
%else %put NOTE: All WRDS-downloaded raw files already exist -- skipping signon.;

/***************************************************************************
PART 1: CREATE TAQ SAMPLE AND MERGE WITH IID DATA
***************************************************************************/

*Start with the all_five2 dataset;
data work.fdp;
	set data.all_five2;
run;


proc sql;
create table work.checkdups as select distinct
gvkey, datadate, count(permno) as n
from work.fdp
group by gvkey, datadate
order by n desc;
quit;




*Create calendar of trading days;
proc sql;
create table work.dates as select distinct
	date
from raw.dsf_07172025
order by date;
quit;

data work.crspdates;
	set work.dates;
	n=_n_;
run;

*Create event-time sample around announcement dates;
%tddays(dsetin=work.fdp (keep = gvkey permno datadate rdq best_anndats best_anndats_adj best_anntims), 
		dsetout=work.taqsamp1, 
		datevar=best_anndats_adj,
		beginwin=-42,
		endwin=2,
		calendarname = work.crspdates);

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, date, count(permno) as n
from work.taqsamp1
group by gvkey, datadate, date
order by n desc;
quit;

*Download the TAQ-CRSP link file if needed;
%macro download_taqlink;
%if not %sysfunc(exist(raw.taqmclink_07242025)) %then %do;
	rsubmit; 
	proc download data = wrdsapps.taqmclink out=raw.taqmclink_07242025; 
	run; 
	endrsubmit;
%end;
%mend;
%download_taqlink;

*Obtain the TAQ sym_root and sym_suffix based on the firm/day;
proc sql;
create table work.taqsamp2 as select distinct
	b.sym_root, b.sym_suffix, a.*, 
	b.ticker, b.match_lvl
from work.taqsamp1 a, raw.taqmclink_07242025 b
where a.permno=b.permno and a.date=b.date
order by best_anntims, sym_root;
quit;

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, sym_root, sym_suffix, date, count(permno) as n
from work.taqsamp2
group by gvkey, datadate, date, sym_root, sym_suffix
order by n desc;
quit;

*Drop duplicates - keep best match level;
proc sort data=work.taqsamp2;
	by gvkey datadate date match_lvl;
run;

proc sort data=work.taqsamp2 out=work.taqsamp3 nodupkey;
	by gvkey datadate date;
run;

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, sym_root, sym_suffix, date, count(permno) as n
from work.taqsamp3
group by gvkey, datadate, date, sym_root, sym_suffix
order by n desc;
quit;

*Download the TAQ MS IID Files if needed;
%macro download_iid;
%do i=2003 %to 2025;
	%if not %sysfunc(exist(raw.iid_msec_&i)) %then %do;
		rsubmit;
		data work.iid_data_&i;
			set TAQMSEC.WRDS_IID_&i;
		run;
		proc download data=work.iid_data_&i out=raw.iid_msec_&i; 
		run;
		endrsubmit;
	%end;
%end;
%mend;
%download_iid;

*Merge sample with TAQ IID data - match on sym_root, sym_suffix, and date;
%macro collect;
%do i=2003 %to 2025;
	proc sql;
	create table work.iid_data_&i as select distinct
		a.*, 
		b.EffectiveSpread_Percent_Ave, b.total_n_trades_m, b.BestBidDepth_Dollar_tw, b.BestOfrDepth_Dollar_tw, b.PercentPriceImpact_LR_Ave,
		b.BuyNumTrades_Retail, b.BuyVol_Retail, b.buy_dv_Retail, b.SellNumTrades_Retail, b.SellVol_Retail, b.sell_dv_Retail, b.total_trade_retail,
		b.total_vol_retail, b.total_dv_retail, b.avg_buy_price_Retail, b.bs_ratio_retail_num, b.bs_ratio_retail_vol, b.avg_sell_price_Retail,
		b.BuyNumTrades_Inst20k, b.BuyVol_Inst20k, b.buy_dv_Inst20k, b.SellNumTrades_Inst20k, b.SellVol_Inst20k, b.sell_dv_Inst20k, b.total_trade_Inst20k, 
		b.total_vol_Inst20k, b.total_dv_Inst20k, b.avg_buy_price_Inst20k, b.avg_sell_price_Inst20k, b.bs_ratio_Inst20k_num, b.bs_ratio_Inst20k_vol, 
		b.BuyNumTrades_Inst50k, b.BuyVol_Inst50k, b.buy_dv_Inst50k, b.SellNumTrades_Inst50k, b.SellVol_Inst50k, b.sell_dv_Inst50k, b.total_trade_Inst50k, 			
		b.total_vol_Inst50k, b.total_dv_Inst50k, b.avg_buy_price_Inst50k, b.avg_sell_price_Inst50k, b.bs_ratio_Inst50k_num, b.bs_ratio_Inst50k_vol,
		b.ivol_q, b.ivol_t, b.var_ratio1, b.var_ratio2, b.var_ratio3, b.var_ratio4, b.var_ratio5, b.bs_ratio_num, b.bs_ratio_vol, 
		b.BuyNumTrades_LR, b.SellNumTrades_LR, b.total_trade,
		b.BuyVol_LR, b.SellVol_LR, b.total_vol, b.buy_dv_LR, b.sell_dv_LR, b.total_dv_LR, b.total_vol_m, b.total_dollar_m,
		b.BuyNumTrades_wrds, b.BuyVol_wrds, b.buy_dv_wrds, b.SellNumTrades_wrds, b.SellVol_wrds, b.sell_dv_wrds, b.total_trade_wrds, b.total_vol_wrds, b.total_dv_wrds,
		b.TSignSqrtDVol1, b.TSignSqrtDVol2
	from work.taqsamp3 a 
	inner join raw.iid_msec_&i b
		on a.sym_root=b.sym_root 
		and a.date=b.date
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
	quit;
%end;
%mend;
%collect;

data work.taqmssdata;	
	set work.iid_data_2003 - work.iid_data_2025;
run;

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, sym_root, sym_suffix, date, count(permno) as n
from work.taqmssdata
group by gvkey, datadate, date, sym_root, sym_suffix
order by n desc;
quit;

*Keep only variables of interest;
proc sql;
	create table work.taqmssdata2 as select distinct
		permno, gvkey, datadate, sym_root, sym_suffix, best_anndats, best_anntims, best_anndats_adj, date, td_days, n_evtdate, 
		EffectiveSpread_Percent_Ave, total_n_trades_m, BestBidDepth_Dollar_tw, BestOfrDepth_Dollar_tw, PercentPriceImpact_LR_Ave
	from work.taqmssdata;
quit;

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, sym_root,sym_suffix,date, count(total_n_trades_m) as n
from work.taqmssdata2
group by gvkey, datadate,date, sym_root, sym_suffix
order by n desc;
quit;
*no dups; 

*Save merged IID data;
data data.iid_2003to2025_millisecond;
	set work.taqmssdata2;
run;

/***************************************************************************
PART 2: CREATE LIQUIDITY VARIABLES
***************************************************************************/

/*****ABNORMAL SPREAD*****/
*Event Window [0,+1];
data work.event;
	set data.iid_2003to2025_millisecond;
	where 0<=td_days<=1;
run;

proc sort data=work.event;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.event noprint;
	var total_n_trades_m;
	output out=work.event_total_n_trades_m sum=event_total_n_trades_m n=days_event;
	by gvkey datadate sym_root sym_suffix;
run;

proc sql;
	create table work.event2
	as select distinct a.*, b.event_total_n_trades_m 'Total Number of Trades during market hours (Open to Close) days [0,+1]',
		b.days_event 'Number of days with trading data in [0,+1]'
	from work.event as a left join work.event_total_n_trades_m as b
	on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

data work.event3;
	set work.event2;
	event_weighted_spread=EffectiveSpread_Percent_Ave*(total_n_trades_m/event_total_n_trades_m);
run;

proc sort data=work.event3;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.event3 noprint;
	var event_weighted_spread;
	output out=work.event_weighted_avg_spread sum=event_weighted_avg_spread;
	by gvkey datadate sym_root sym_suffix;
run;

*Non-Event Window [-41,-11];
data work.nonevent;
	set data.iid_2003to2025_millisecond;
	where -41<=td_days<=-11;
run;

proc sort data=work.nonevent;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.nonevent noprint;
	var total_n_trades_m;
	output out=work.nonevent_total_n_trades_m sum=nonevent_total_n_trades_m n=days_nonevent;
	by gvkey datadate sym_root sym_suffix;
run;

proc sql;
	create table work.nonevent2
	as select distinct a.*, b.nonevent_total_n_trades_m 'Total Number of Trades during market hours (Open to Close) days [-41,-11]',
		b.days_nonevent 'Number of days with trading data in [-41,-11]'
	from work.nonevent as a left join work.nonevent_total_n_trades_m as b
	on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

data work.nonevent3;
	set work.nonevent2;
	nonevent_weighted_spread=EffectiveSpread_Percent_Ave*(total_n_trades_m/nonevent_total_n_trades_m);
run;

proc sort data=work.nonevent3;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.nonevent3 noprint;
	var nonevent_weighted_spread;
	output out=work.nonevent3_weighted_avg_spread sum=nonevent_weighted_avg_spread;
	by gvkey datadate sym_root sym_suffix;
run;

*Combine Event and Non-Event Windows;
proc sql;
	create table work.event_weighted_avg_spread2
	as select distinct 
		a.gvkey, a.datadate, a.sym_root, a.sym_suffix,
		a._FREQ_ as days_event, b._FREQ_ as days_nonevent,
		a.event_weighted_avg_spread, b.nonevent_weighted_avg_spread,
		log(a.event_weighted_avg_spread/b.nonevent_weighted_avg_spread) as abnormal_spread
	from work.event_weighted_avg_spread as a 
	left join work.nonevent3_weighted_avg_spread as b
		on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

/*****ABNORMAL DEPTH*****/
*Event Window [0,+1];
data work.event_depth;
	set data.iid_2003to2025_millisecond;
	where 0<=td_days<=1;
run;

data work.event_depth2;
	set work.event_depth;
	depth=(BestBidDepth_Dollar_tw+BestOfrDepth_Dollar_tw)/2;
run;

proc sort data=work.event_depth2;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.event_depth2 noprint;
	var depth;
	output out=work.event_avg_depth mean=event_avg_depth;
	by gvkey datadate sym_root sym_suffix;
run;

*Non-Event Window [-41,-11];
data work.nonevent_depth;
	set data.iid_2003to2025_millisecond;
	where -41<=td_days<=-11;
run;

data work.nonevent_depth2;
	set work.nonevent_depth;
	depth=(BestBidDepth_Dollar_tw+BestOfrDepth_Dollar_tw)/2;
run;

proc sort data=work.nonevent_depth2;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.nonevent_depth2 noprint;
	var depth;
	output out=work.nonevent_avg_depth mean=nonevent_avg_depth;
	by gvkey datadate sym_root sym_suffix;
run;

*Combine Event and Non-Event Windows;
proc sql;
	create table work.event_avg_depth2
	as select distinct 
		a.gvkey, a.datadate, a.sym_root, a.sym_suffix,
		a._FREQ_ as days_event, b._FREQ_ as days_nonevent,
		a.event_avg_depth, b.nonevent_avg_depth,
		log(a.event_avg_depth/b.nonevent_avg_depth) as abnormal_depth
	from work.event_avg_depth as a 
	left join work.nonevent_avg_depth as b
		on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

/*****ABNORMAL PRICE IMPACT*****/
*Event Window [0,+1];
data work.event_price_impact;
	set data.iid_2003to2025_millisecond;
	where 0<=td_days<=1;
run;

proc sort data=work.event_price_impact;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.event_price_impact noprint;
	var total_n_trades_m;
	output out=work.event_total_n_trades_m_pi sum=event_total_n_trades_m n=days_event;
	by gvkey datadate sym_root sym_suffix;
run;

proc sql;
	create table work.event_price_impact2
	as select distinct a.*, b.event_total_n_trades_m 'Total Number of Trades during market hours (Open to Close) days [0,+1]',
		b.days_event 'Number of days with trading data in [0,+1]'
	from work.event_price_impact as a left join work.event_total_n_trades_m_pi as b
	on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

data work.event_price_impact3;
	set work.event_price_impact2;
	event_weighted_price_impact=PercentPriceImpact_LR_Ave*(total_n_trades_m/event_total_n_trades_m);
run;

proc sort data=work.event_price_impact3;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.event_price_impact3 noprint;
	var event_weighted_price_impact;
	output out=work.event_weighted_avg_price_impact sum=event_weighted_avg_price_impact;
	by gvkey datadate sym_root sym_suffix;
run;

*Non-Event Window [-41,-11];
data work.nonevent_price_impact;
	set data.iid_2003to2025_millisecond;
	where -41<=td_days<=-11;
run;

proc sort data=work.nonevent_price_impact;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.nonevent_price_impact noprint;
	var total_n_trades_m;
	output out=work.nonevent_total_n_trades_m_pi sum=nonevent_total_n_trades_m n=days_nonevent;
	by gvkey datadate sym_root sym_suffix;
run;

proc sql;
	create table work.nonevent_price_impact2
	as select distinct a.*, b.nonevent_total_n_trades_m 'Total Number of Trades during market hours (Open to Close) days [-41,-11]',
		b.days_nonevent 'Number of days with trading data in [-41,-11]'
	from work.nonevent_price_impact as a left join work.nonevent_total_n_trades_m_pi as b
	on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;

data work.nonevent_price_impact3;
	set work.nonevent_price_impact2;
	nonevent_wtd_price_impact=PercentPriceImpact_LR_Ave*(total_n_trades_m/nonevent_total_n_trades_m);
run;

proc sort data=work.nonevent_price_impact3;
	by gvkey datadate sym_root sym_suffix date;
run;
	
proc means data=work.nonevent_price_impact3 noprint;
	var nonevent_wtd_price_impact;
	output out=work.nonevent_wtd_avg_price_impact sum=nonevent_wtd_avg_price_impact;
	by gvkey datadate sym_root sym_suffix;
run;

*Combine Event and Non-Event Windows;
proc sql;
	create table work.event_weighted_avg_price_impact2
	as select distinct 
		a.gvkey, a.datadate, a.sym_root, a.sym_suffix,
		a._FREQ_ as days_event, b._FREQ_ as days_nonevent,
		a.event_weighted_avg_price_impact, b.nonevent_wtd_avg_price_impact,
		(a.event_weighted_avg_price_impact/b.nonevent_wtd_avg_price_impact) as abnormal_price_impact
	from work.event_weighted_avg_price_impact as a 
	left join work.nonevent_wtd_avg_price_impact as b
		on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)));
quit;





*Combine all liquidity measures into one dataset;
proc sql;
	create table data.taq_liquidity_vars as select distinct
		a.gvkey,
		a.datadate,
		a.sym_root,
		a.sym_suffix,
		a.days_event as spread_days_event, 
		a.days_nonevent as spread_days_nonevent, 
		a.abnormal_spread,
		a.event_weighted_avg_spread, 
		a.nonevent_weighted_avg_spread,
		b.abnormal_depth,
		b.event_avg_depth, 
		b.nonevent_avg_depth,
		c.abnormal_price_impact,
		c.event_weighted_avg_price_impact, 
		c.nonevent_wtd_avg_price_impact
	from work.event_weighted_avg_spread2 as a
	inner join work.event_avg_depth2 as b
		on a.gvkey=b.gvkey and a.datadate=b.datadate 
		and a.sym_root=b.sym_root 
		and (a.sym_suffix=b.sym_suffix or (missing(a.sym_suffix) and missing(b.sym_suffix)))
	inner join work.event_weighted_avg_price_impact2 as c
		on a.gvkey=c.gvkey and a.datadate=c.datadate 
		and a.sym_root=c.sym_root 
		and (a.sym_suffix=c.sym_suffix or (missing(a.sym_suffix) and missing(c.sym_suffix)));
quit;


*Drop duplicates;
proc sort data=data.taq_liquidity_vars;
	by gvkey datadate descending nonevent_avg_depth sym_suffix;
run;

proc sort data=data.taq_liquidity_vars nodupkey;
	by gvkey datadate;
run;

proc sql;
create table work.checkdups as select distinct
gvkey, datadate, count(sym_root) as n
from data.taq_liquidity_vars
group by gvkey, datadate
order by n desc;
quit;

/***************************************************************************
PART 3: MERGE TAQ LIQUIDITY VARIABLES INTO MAIN DATASET
***************************************************************************/

proc sql;
	create table data.all_five3 as select distinct 
		a.*, 
		b.sym_root, 
		b.sym_suffix,
		b.spread_days_event, 
		b.spread_days_nonevent, 
		b.abnormal_spread,
		b.event_weighted_avg_spread, 
		b.nonevent_weighted_avg_spread,
		b.abnormal_depth, 
		b.event_avg_depth, 
		b.nonevent_avg_depth,
		b.abnormal_price_impact, 
		b.event_weighted_avg_price_impact, 
		b.nonevent_wtd_avg_price_impact
	from data.all_five2 as a
	left join data.taq_liquidity_vars as b
		on a.gvkey=b.gvkey and a.datadate=b.datadate;
quit;

*Signoff only if we signed on at the top;
%macro maybe_signoff;
  %if &need_wrds = 1 %then %do;
    signoff;
  %end;
%mend;
%maybe_signoff;
