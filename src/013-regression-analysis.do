/******************************************************************************
 * 013-regression-analysis.do
 *
 * Purpose:
 *   Run all regressions and produce paper tables / output files. Reads
 *   the firm-quarter and FDP-firm-quarter regression-input datasets
 *   built by 011 and 012 and writes one CSV per table into OUTPUT_DIR.
 *
 * Inputs (from DATA_DIR):
 *   2025_12_16_firm_qtr_regdata.dta                  (output of 011)
 *   2025_12_16_fdp_firm_qtr_regdata.dta              (output of 012)
 *
 * Outputs (to OUTPUT_DIR):
 *   descrip_firm_qtr_sample.csv                      (Table 2)
 *   determinants.csv                                 (Table 3)
 *   mkt_outcomes.csv                                 (Table 4)
 *   descrip_fdp_firm_qtr_sample_ibes.csv             (Table 5 Panel A IBES)
 *   descrip_fdp_firm_qtr_sample_zacks.csv            (Table 5 Panel A Zacks)
 *   descrip_fdp_firm_qtr_sample_ciq.csv              (Table 5 Panel A CIQ)
 *   descrip_fdp_firm_qtr_sample_bb.csv               (Table 5 Panel A BB)
 *   descrip_fdp_firm_qtr_sample_fset.csv             (Table 5 Panel A FSET)
 *   rank_correlation.csv                             (Table 5 Panel B)
 *   mbe_test.csv                                     (Table 6)
 *
 * FIRST-TIME SETUP:
 *   Install required Stata packages (uncomment and run once per machine):
 *     ssc install estout
 *     ssc install reghdfe
 *     net install projectpaths, from("https://raw.githubusercontent.com/eweisbrod/projectpaths/main/src/") replace
 *     net install doenv, from("https://github.com/vikjam/doenv/raw/master/")
 *
 *   Register this project with projectpaths (replace path with the path
 *   to your local clone of the consensus repo):
 *     project_paths_list, add project(consensus) path("C:/_git/consensus")
 ******************************************************************************/


* Setup ----------------------------------------------------------------------

// Navigate to the project root and load .env
project_paths_list, project(consensus) cd
doenv using ".env"
local data_dir   "`r(DATA_DIR)'"
local output_dir "`r(OUTPUT_DIR)'"

display "Using data directory: `data_dir'"
display "Using output directory: `output_dir'"


/********************************Firm-Qtr Analysis********************************/

 /***************TABLE 2 - DESCRIPTIVES*******************/
 use "`data_dir'/2025_12_16_firm_qtr_regdata.dta", clear

global controls unique_following high_stkcomp unexpected_item abs_spiq_ibq_w1 percent_change_cshfdq_w1 stock_split ///
	dispersion_w1 log_lagmins_w1 abs_ibes_surp_u_price_w1 pos_surprise ///
	lnmve_w1 btm_w1 io_w1 guidance percent_change_ibq_w1 ret_vol_w1  q4 log_ea_count_w1

global fdp_method sunique_following high_stkcomp unexpected_item sabs_spiq_ibq_w1 spercent_change_cshfdq_w1 stock_split
global fdp_uncertain sdispersion_w1 slog_lagmins_w1 sabs_ibes_surp_u_price_w1 pos_surprise pos_sabs_ibes_surp_u_price_w1
global firm_qtr slnmve_w1 sbtm_w1 sio_w1 guidance spercent_change_ibq_w1 sret_vol_w1 q4 slog_ea_count_w1

* The paper reports descriptives for each market-consequences variable on
* its corresponding Table 4 regression sample (post-singleton drop), so the
* Table 2 N ties to Table 4. Run the four panel regressions silently to
* identify each TAQ variable's regression sample, then null out values
* outside that sample. A single estpost summarize then produces the right
* N per variable (estpost summarize skips missings per variable).

quietly reghdfe mrt_32hr_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
	, cluster(permno best_anndats_adj) absorb(permno yearqtr)
replace mrt_32hr_w1 = . if !e(sample)

quietly reghdfe abnormal_volatility_rh_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
	, cluster(permno best_anndats_adj) absorb(permno yearqtr)
replace abnormal_volatility_rh_w1 = . if !e(sample)

quietly reghdfe abnormal_depth_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
	, cluster(permno best_anndats_adj) absorb(permno yearqtr)
replace abnormal_depth_w1 = . if !e(sample)

quietly reghdfe abnormal_price_impact_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
	, cluster(permno best_anndats_adj) absorb(permno yearqtr)
replace abnormal_price_impact_w1 = . if !e(sample)

eststo clear
estpost summarize  max_min_surp_price_w1_100 std_dev_surp_price_w1_100 miss_and_beat_mbe_2 mrt_32hr_w1 abnormal_volatility_rh_w1 abnormal_spread_w1 abnormal_depth_w1 abnormal_price_impact_w1 car_0top1_100 ///
		$controls ///
		, detail

	//preview the output
esttab . , replace noobs label ///
cells("count(fmt(%9.0fc)) mean(fmt(%9.3fc)) p50(fmt(%9.3fc)) sd(fmt(%9.3fc)) p25(fmt(%9.3fc)) p75(fmt(%9.3fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_firm_qtr_sample.csv", replace ///
cells("count(fmt(%9.0f)) mean(fmt(%9.3fc)) p50(fmt(%9.3fc)) sd(fmt(%9.3fc)) p25(fmt(%9.3fc)) p75(fmt(%9.3fc))") compress ///
 title("Descriptive Statistics") ///
 nomtitles noobs label


 /***************TABLE 3 - DETERMINANTS*******************/
 use "`data_dir'/2025_12_16_firm_qtr_regdata.dta", clear

//Define Controls

global fdp_method sunique_following high_stkcomp unexpected_item sabs_spiq_ibq_w1 spercent_change_cshfdq_w1 stock_split
global fdp_uncertain sdispersion_w1 slog_lagmins_w1 sabs_ibes_surp_u_price_w1 pos_surprise pos_sabs_ibes_surp_u_price_w1
global firm_qtr slnmve_w1 sbtm_w1 sio_w1 guidance spercent_change_ibq_w1 sret_vol_w1 q4 slog_ea_count_w1


eststo clear

eststo, title("Max less Min of Surprise Scaled By Price Winsorized"): reghdfe max_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr , cluster(permno best_anndats_adj)  absorb(yearqtr)

eststo, title("Standard Deviation of Surprise Scaled By Price Winsorized"): reghdfe std_dev_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr , cluster(permno best_anndats_adj)  absorb(yearqtr)

eststo, title("Miss and Beat"): reghdfe miss_and_beat_mbe_2 $fdp_method $fdp_uncertain $firm_qtr , cluster(permno best_anndats_adj)  absorb(yearqtr)



esttab using "`output_dir'/determinants.csv", replace  ///
 title("Determinants of Differences") ///
 mtitles label ///
 b(3) t(2) ///
 star(* 0.10 ** 0.05 *** 0.01) ///
 stats(N r2_a, fmt (%20.0g 3))


 /***************TABLE 4 - CONSEQUENCES of DIFFERENCES*******************/
 use "`data_dir'/2025_12_16_firm_qtr_regdata.dta", clear


//Define Controls

global fdp_method sunique_following high_stkcomp unexpected_item sabs_spiq_ibq_w1 spercent_change_cshfdq_w1 stock_split
global fdp_uncertain sdispersion_w1 slog_lagmins_w1 sabs_ibes_surp_u_price_w1  pos_surprise pos_sabs_ibes_surp_u_price_w1
global firm_qtr slnmve_w1 sbtm_w1 sio_w1 guidance spercent_change_ibq_w1 sret_vol_w1 q4 slog_ea_count_w1


eststo clear

/***************Panel A MRT*******************/
eststo, title("MRT 32 Hour - Winsorized"): reghdfe mrt_32hr_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("MRT 32 Hour - Winsorized"): reghdfe mrt_32hr_w1 sstd_dev_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("MRT 32 Hour - Winsorized"): reghdfe mrt_32hr_w1 miss_and_beat_mbe_2 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)



/***************Panel A Abnormal Volatility*******************/
eststo, title("Abnormal Volatility - Winsorized"): reghdfe abnormal_volatility_rh_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Abnormal Volatility - Winsorized"): reghdfe abnormal_volatility_rh_w1 sstd_dev_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Abnormal Volatility - Winsorized"): reghdfe abnormal_volatility_rh_w1 miss_and_beat_mbe_2 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)


/***************Panel B Abnormal Liquidity*******************/

eststo, title("Depths - Winsorized"): reghdfe abnormal_depth_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Depths - Winsorized"): reghdfe abnormal_depth_w1 sstd_dev_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Depths - Winsorized"): reghdfe abnormal_depth_w1 miss_and_beat_mbe_2 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)


eststo, title("Price Impact - Winsorized"): reghdfe abnormal_price_impact_w1 smax_min_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Price Impact - Winsorized"): reghdfe abnormal_price_impact_w1 sstd_dev_surp_price_w1_100 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)

eststo, title("Price Impact - Winsorized"): reghdfe abnormal_price_impact_w1 miss_and_beat_mbe_2 $fdp_method $fdp_uncertain $firm_qtr ///
		,  cluster(permno best_anndats_adj)  absorb(permno yearqtr)


 esttab using "`output_dir'/mkt_outcomes.csv", replace  ///
 title("Consequences of Differences") ///
 mtitles label ///
 b(3) t(2) ///
 star(* 0.10 ** 0.05 *** 0.01) ///
 stats(N r2_a, fmt (%20.0g 3))




 /********************************FDP-Firm-Qtr Analysis********************************/

 /***************TABLE 5 - DESCRIPTIVES*******************/

use "`data_dir'/2025_12_16_fdp_firm_qtr_regdata.dta", clear
 /*Table 5 Panel A- IBES*/
eststo clear

estpost summarize rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following rank_avg_quality if fdp_num==1, detail

//preview the output
esttab . , replace noobs label ///
cells("mean(fmt(%9.2fc))sd(fmt(%9.2fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_fdp_firm_qtr_sample_ibes.csv", replace ///
cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress ///
 title("IBES Statistics") ///
 nomtitles noobs label


/*Table 5 Panel A- Zacks*/
eststo clear

estpost summarize rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following rank_avg_quality if fdp_num==2, detail

//preview the output
esttab . , replace noobs label ///
cells("mean(fmt(%9.2fc))sd(fmt(%9.2fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_fdp_firm_qtr_sample_zacks.csv", replace ///
cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress ///
 title("Zacks Statistics") ///
 nomtitles noobs label


 /*Table 5 Panel A- CIQ*/
eststo clear

estpost summarize rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following rank_avg_quality if fdp_num==3, detail

//preview the output
esttab . , replace noobs label ///
cells("mean(fmt(%9.2fc))sd(fmt(%9.2fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_fdp_firm_qtr_sample_ciq.csv", replace ///
cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress ///
 title("CIQ Statistics") ///
 nomtitles noobs label


  /*Table 5 Panel A- BB*/
eststo clear

estpost summarize rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following rank_avg_quality if fdp_num==4, detail

//preview the output
esttab . , replace noobs label ///
cells("mean(fmt(%9.2fc))sd(fmt(%9.2fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_fdp_firm_qtr_sample_bb.csv", replace ///
cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress ///
 title("BB Statistics") ///
 nomtitles noobs label


  /*Table 5 Panel A- FSET*/
eststo clear

estpost summarize rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following rank_avg_quality if fdp_num==5, detail

//preview the output
esttab . , replace noobs label ///
cells("mean(fmt(%9.2fc))sd(fmt(%9.2fc))") compress

//output the table to excel
esttab using "`output_dir'/descrip_fdp_firm_qtr_sample_fset.csv", replace ///
cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress ///
 title("FSET Statistics") ///
 nomtitles noobs label


/*TABLE 5 PANEL B*/

 summarize rank_avg_quality
generate srank_avg_quality=(rank_avg_quality-r(mean))/r(sd)

 eststo clear
estpost correlate rank_lag_accuracy rank_earn_persist rank_cf_persist rank_exp actual_agree rank_media rank_following srank_avg_quality, matrix  listwise
esttab using "`output_dir'/rank_correlation.csv", replace  title("Rank Correlations") unstack not noobs nonote b(3) label




/***************TABLE 6 - FDP FIRM QTR ERCs*******************/
use "`data_dir'/2025_12_16_fdp_firm_qtr_regdata.dta", clear

global scontrols slnmve_w1 sbtm_w1 sio_w1 sunique_following ///
guidance sdispersion_w1 spercent_change_ibq_w1  ///
spercent_change_cshfdq_w1 stock_split sabs_spiq_ibq_w1 high_stkcomp unexpected_item ///
sret_vol_w1 slog_lagmins_w1 q4 slog_ea_count_w1

 summarize rank_avg_quality
generate srank_avg_quality=(rank_avg_quality-r(mean))/r(sd)

eststo clear

eststo, title("Signed Return"): reghdfe returns_pos ///
		mbe_2 i.fdp_num c.mbe_2#i.fdp_num ///
		, nocons cluster(permno best_anndats_adj) noabsorb

 eststo, title("Signed Return Total Effects"): reghdfe returns_pos ///
		mbe_2 i.fdp_num c.mbe_2#i.fdp_num ///
		srank_avg_quality ///
		$scontrols sln_n_articles_w1 missing_persist_earn missing_cites2 ///
		c.mbe_2#c.slnmve_w1 c.mbe_2#c.sbtm_w1 c.mbe_2#c.sio_w1 ///
		c.mbe_2#c.sunique_following c.mbe_2#c.guidance c.mbe_2#c.sdispersion_w1 c.mbe_2#c.spercent_change_ibq_w1  ///
		c.mbe_2#c.spercent_change_cshfdq_w1 c.mbe_2#c.stock_split c.mbe_2#c.sabs_spiq_ibq_w1 ///
		c.mbe_2#c.high_stkcomp c.mbe_2#c.unexpected_item ///
		c.mbe_2#c.sret_vol_w1 ///
		c.mbe_2#c.slog_lagmins_w1 c.mbe_2#c.q4 c.mbe_2#c.slog_ea_count_w1 c.mbe_2#c.sln_n_articles_w1 ///
		c.mbe_2#c.missing_persist_earn c.mbe_2#c.missing_cites2 ///
		, nocons cluster(permno best_anndats_adj) noabsorb


eststo, title("Signed Return Quality and Salience Components - Interactions"): reghdfe returns_pos mbe_2 c.mbe_2#i.fdp_num  c.mbe_2#c.srank_avg_quality  ///
		sabs_spiq_ibq_w1  sbtm_w1 spercent_change_cshfdq_w1 spercent_change_ibq_w1 sdispersion_w1 i.fdp_num ///
		guidance high_stkcomp  c.mbe_2#c.guidance c.mbe_2#c.sdispersion_w1 c.mbe_2#c.spercent_change_ibq_w1 ///
		c.mbe_2#c.spercent_change_cshfdq_w1 c.mbe_2#c.stock_split c.mbe_2#c.sabs_spiq_ibq_w1 c.mbe_2#c.high_stkcomp c.mbe_2#c.unexpected_item ///
		c.mbe_2#c.sret_vol_w1 c.mbe_2#c.slog_lagmins_w1  c.mbe_2#c.q4 c.mbe_2#c.slog_ea_count_w1 ///
		c.mbe_2#c.sln_n_articles_w1 c.mbe_2#c.missing_cites2 c.mbe_2#c.missing_persist_earn ///
		c.mbe_2#c.slnmve_w1 c.mbe_2#c.sbtm_w1 c.mbe_2#c.sio_w1 ///
		c.mbe_2#c.sunique_following sio_w1 slog_lagmins_w1 slnmve_w1 missing_cites2 missing_persist_earn sln_n_articles_w1 ///
		q4 slog_ea_count_w1 sret_vol_w1     stock_split  unexpected_item sunique_following srank_avg_quality  ///
		, nocons cluster(permno best_anndats_adj) noabsorb


	 esttab using "`output_dir'/mbe_test.csv", replace  ///
 title("ERC FDP Traits") ///
  drop(1.fdp_num*) /// drops the baseline empty reference category
 mtitles label ///
 b(3) t(2) ///
 star(* 0.10 ** 0.05 *** 0.01) ///
 stats(N r2_a, fmt (%20.0g 3))
