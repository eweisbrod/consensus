/* ==============================================================================
 * 000-collect-master-ccm-dataset.sas
 *
 * Purpose:
 *   Download raw Compustat and CRSP datasets from WRDS and merge them into the
 *   master CRSP/Compustat (CCM) dataset used as the analysis spine.
 *
 * Inputs:
 *   WRDS datasets (via SAS rsubmit signon):
 *     comp.fundq
 *     comp.company
 *     crsp.ccmxpf_linktable
 *     crsp.stocknames
 *     crsp.dsf
 *
 * Outputs:
 *   fundq_07172025.sas7bdat
 *   company_12092025.sas7bdat
 *   ccmxpf_linktable_07172025.sas7bdat
 *   stocknames_07172025.sas7bdat
 *   dsf_07172025.sas7bdat
 *   master_ccm_07182025.sas7bdat
 *
 * Notes:
 *   - Run by Jessie Watkins.
 *   - Last executed: 2025-12-10. The output filename master_ccm_07182025 retains
 *     the original 2025-07-18 datestamp from an earlier iteration; the file
 *     content reflects the December re-build (which incorporates the
 *     2025-12-09 company table refresh).
 *   - Requires MACROS.sas in the same directory (provides the %tddays helper).
 *   - Requires WRDS access (signon prompts for credentials at runtime).
 *   - Skipped on replication runs; outputs are pre-built.
 * ============================================================================== */

*Setup*********************************************************************;

*Define dropbox library;
libname fdpbox "D:\Dropbox\Dropbox\FDP Comparison\Data";



*macro variable for the filepath to this code;
*this is useful for creating relative file paths;
%let codepath = %qsysfunc(sysget(SAS_EXECFILEPATH));

*using the codepath, we can make path to the MACROS file;
%let macrofile = &codepath\..\MACROS.sas;

*check;
%put &codepath;

*include the code file for the macros; 
%include "&macrofile";

*Log into WRDS;
%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;


*Step 1: Start with Compustat Fundq****************************************;
rsubmit; proc download data=comp.fundq out=fdpbox.fundq_07172025; run; endrsubmit;
rsubmit; proc download data=comp.company out=fdpbox.company_12092025; run; endrsubmit;

proc sql;
create table comp1 as select distinct
gvkey, datadate, cusip, fyearq, fqtr, conm, rdq, ibq, atq, 
CEQQ, PRCCQ, CSHOQ, IBCY, OANCFY, XIDOCY, XRDQ, SPIQ, SALEQ, EPSPXQ, EPSFXQ, AJEXQ,
oepsxq, niq, oibdpq,epspiq,epsfiq,opepsq,cshprq,cshfdq,epsx12,oeps12,
epsf12,oepf12

from fdpbox.fundq_07172025
where 1998 le year(datadate) le 2023
and indfmt='INDL' and datafmt='STD' and popsrc='D'
and consol='C'
and missing(cusip)=0 and missing(epsfiq)=0 and missing(rdq)=0
and 0 < (rdq - datadate) < 120 
and compstq ne "DB";
quit;



*check for duplicates;
proc sql;
create table checkdups as select distinct
gvkey, datadate,rdq, count(rdq) as n
from comp1
group by gvkey, datadate,rdq
order by n desc, gvkey, datadate;
quit;

proc freq data=checkdups;
tables n;
run;

*check some of the duplicates;
data check;
set comp1;
where gvkey = "001681" and datadate='31MAR2008'd;
run;
*if there are two observations for the same rdq with different data
these are likely fiscal year changes or some other irregularity that 
makes it too hard to identify which observation is correct so delete both;


proc sql;
create table comp2 as select distinct
a.*
from comp1 a, checkdups b
where a.gvkey=b.gvkey and a.datadate=b.datadate and a.rdq=b.rdq
and b.n=1;
quit;

*check for duplicates;
proc sql;
create table checkdups as select distinct
gvkey, datadate, count(rdq) as n
from comp2
group by gvkey, datadate
order by n desc, gvkey, datadate;
quit;
*only 4 dups, not bad;

*Add FIC and SIC codes from Compustat;
proc sql;
create table comp3 as select distinct
a.*, b.fic, b.sic
from comp2 a, fdpbox.company_12092025 b
where a.gvkey=b.gvkey;
quit;

*check for duplicates;
proc sql;
create table checkdups as select distinct
gvkey, datadate, count(rdq) as n
from comp3
group by gvkey, datadate
order by n desc, gvkey, datadate;
quit;
*only 4 dups, not bad;




*Step 2: Link Compustat and CRSP******************************************;
rsubmit; proc download data=crsp.ccmxpf_linktable out=fdpbox.ccmxpf_linktable_07172025; run; endrsubmit;
rsubmit; proc download data=crsp.stocknames out=fdpbox.stocknames_07172025; run; endrsubmit;
rsubmit; proc download data=crsp.dsf out=fdpbox.dsf_07172025; run; endrsubmit;



*GVKEY to PERMNO Link;
proc sql;
create table ccm1 as select distinct
b.lpermno as permno, a.*
from comp3 a , fdpbox.ccmxpf_linktable_07172025 (where=(usedflag=1 and linkprim in ('P','C'))) as b
where  a.gvkey=b.gvkey and
((b.linkdt <= a.rdq) or b.linkdt=.B) and ((a.rdq <= b.linkenddt) or b.linkenddt=.E);
quit;

*check for dups;
proc sql;
create table checkdups as select distinct
gvkey, datadate, count(rdq) as n
from ccm1
group by gvkey, datadate
order by n desc, gvkey, datadate;
quit;
*only one dup;



*Step 3: Collect CRSP data around rdq******************************************;

*find the trading days around the rdq using the td days macro;
proc sql;
create table dates as select distinct
date
from fdpbox.dsf_07172025
order by date;
quit;


data crspdates;
set dates;
n=_n_;
run;

%tddays(dsetin=ccm1 (keep = permno rdq), 
		dsetout=temp1, 
		datevar=rdq,
		beginwin=0,
		endwin=1,
		calendarname = crspdates);

*Check that returns dates go from 1998 to 2024;
proc freq data=temp1;
	tables rdq;
	run;

*merge the trading days with the crsp daily returns;
proc sql;
create table temp2 as select distinct
a.*, b.ret, b.hexcd
from temp1 a, fdpbox.dsf_07172025 b
where a.date=b.date and a.permno=b.permno
and not missing(b.ret);
quit;

*aggregate and count; 
proc sql;
create table temp3 as select distinct
permno,rdq, count(ret) as n
from temp2
group by permno, rdq;
quit;

proc freq data=temp3; tables n; run;

*keep only observations with valid return data on days [0,+1] relative to rdq;
proc sql;
create table ccm2 as select distinct
a.*
from ccm1 a, temp3 b
where a.permno=b.permno and 
a.rdq=b.rdq
and b.n=2;
quit;



*check for dups;
proc sql;
create table checkdups as select distinct
gvkey, datadate, count(rdq) as n
from ccm2
group by gvkey, datadate
order by n desc, gvkey, datadate;
quit;
*still only 1 dup;

*let's throw it out.

*dropping gvkey,datadate dups;
proc sql;
create table ccm3 as select distinct
a.*
from ccm2 a, checkdups b
where a.gvkey=b.gvkey and 
a.datadate=b.datadate
and b.n=1;
quit;


*check for dups by permno rdq;
proc sql;
create table checkdups as select distinct
permno, rdq, count(rdq) as n
from ccm3
group by permno, rdq
order by n desc, permno, rdq;
quit;
*17 dups here out of  563,319, very few so let's just delete both to avoid problems later;

*dropping permno,rdq dups;
proc sql;
create table ccm4 as select distinct
a.*
from ccm3 a, checkdups b
where a.permno=b.permno and 
a.rdq=b.rdq
and b.n=1;
quit;


*check for dups;
proc sql;
create table checkdups as select distinct
gvkey, datadate, count(rdq) as n
from ccm4
group by gvkey, datadate
order by n desc, gvkey, datadate;
quit;
*no dups;

*check for dups by permno rdq;
proc sql;
create table checkdups as select distinct
permno, rdq, count(rdq) as n
from ccm4
group by permno, rdq
order by n desc, permno, rdq;
quit;
*no dups;

*Add info from the crsp stocknames file;
proc sql;
create table ccm5 as select distinct
b.ticker as crsp_ticker, b.comnam as crsp_comnam, a.*,b.ncusip, b.hexcd, b.shrcd
from ccm4 as a, fdpbox.stocknames_07172025 as b
where a.permno=b.permno and (b.namedt le a.rdq le b.nameenddt);
quit;


*use this as the master ccm dataset on dropbox;
data fdpbox.master_ccm_07182025;
set ccm5;
run;


data check;
	set fdpbox.master_ccm_07182025;
	year=year(datadate);
	run;

*Observations from 2002 to 2023 US firms:   381,496;
data check;
	set check;
	where 2002<=year<=2023 and fic='USA';
	run;


*Observations from 2002 to 2023, US, not missing assets, sales, and common equity amd sales>$25MM, assets>$100M, and price>$1: 242,259;
data check2;
	set check;
	where atq ne . and saleq ne . and CEQQ ne . and saleq>25 and atq>100 and PRCCQ>1;
	run;

*Observations from 2002 to 2023, US, not missing assets, sales, and common equity amd sales>$25MM, assets>$100M, and price>$1, header and historical cusip agree:  201,654;
proc sql;
	create table check3
	as select * from check2
	where substr(cusip,1,8)=ncusip;
	quit;
