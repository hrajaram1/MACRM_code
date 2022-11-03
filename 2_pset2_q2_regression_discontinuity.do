/*******************************************************************************
Purpose: 
This do file executes the empirical analysis section of problem set 2 for Applied Econometrics III.

Created on: 4/14/2022
By: Hersheena Rajaram

Last modified on: 4/19/2022
By: Hersheena Rajaram
    
*******************************************************************************/

clear all
set more off
macro drop _all
cap log close
program drop _all
matrix drop _all
*set trace on
*set tracedepth 1

global date = c(current_date)
global username = c(username)

** File paths
global base "C:\Users\hraja\OneDrive - The University of Chicago\Desktop\Academic\2_UChicago_MACRM\4_econometrics\Metrics III\1_pset"
global data "$base\1_data"
global log "$base\3_log"
global results "$base\4_results"
global temp "$base\5_temp"

log using "$log\2_pset2_q2_metricsIII${username}_${date}.log", replace 

*Load data
use "$data\2miledata_2022.dta", clear
count		//415

*Recommended variables
*Housing characteristics for 1(a):
global hous_char firestoveheat80 noaircond80 nofullkitchen80 zerofullbath80 bedrms* blt* detach80occ mobile80occ

*Economic and demographic variables for 1(a):
global demo_var pop_den8 shrblk8 shrhsp8 child8 old8 shrfor8 ffh8 smhse8 hsdrop8 no_hs_dipl* ba_or_b* unemprt8 povrat8 welfare8 avhhin8 tothsun8 ownocc8 occupied80

* control variables for 3a (and again in 4):
global control_var firestoveheat80_nbr noaircond80_nbr nofullkitchen80_nbr zerofullbath80_nbr bedrms* blt* detach80occ_nbr mobile80occ_nbr pop_den8_nbr shrblk8_nbr shrhsp8_nbr child8_nbr shrfor8_nbr ffh8_nbr smhse8_nbr hsdrop8_nbr no_hs_dipl* ba_or_b* unemprt8_nbr povrat8_nbr welfare8_nbr avhhin8_nbr tothsun8_nbr ownocc8_nbr occupied80_nbr

* Hypothesis: In the case of an environmental "bad", such as living in close proximity to a hazardous waste site, one would expect that the market price of nearby housing would increase if the hazardous waste site were cleaned up (all else equal).

/* Q2.A - is HRS score a good running variable? */
sum hrs_82
*hrs_82 is the running variable(X_i)

ssc install rdrobust
rdrobust lnmdvalhs0 hrs_82 , c(28.5) p(2) bwselect(mserd)
rdrobust lnmdvalhs0 hrs_82 , c(28.5) bwselect(mserd)
* Divide hrs_82 into equally sized bins of width 10

* How to choose bins. 
rdbwselect lnmdvalhs0 hrs_82, c(28.5)
* Use a bandwith of 8.157 ~ 8
global h = e(h_mserd)

sum hrs_82
hist hrs_82
hist hrs_82, width(8) discrete addplot(pci 0 28.5 .025 28.5) title(Distribution of 1982 HRS Scores) color(ebblue%45) legend(off) xtitle(HRS score 1982)
graph save "$results\q2b_hist_b8.jpg", replace


* Also create one with bin width of 4
hist hrs_82, width(4) discrete addplot(pci 0 28.5 .025 28.5) title(Distribution of 1982 HRS Scores) color(ebblue%45) legend(off) xtitle(HRS score 1982)
graph save "$results\q2b_hist_b4.jpg", replace

* Also create one with bin width of 12
hist hrs_82, width(12) discrete addplot(pci 0 28.5 .025 28.5) title(Distribution of 1982 HRS Scores) color(ebblue%45) legend(off) xtitle(HRS score 1982)
graph save "$results\q2b_hist_b12.jpg", replace

* Also create one with bin width of 16
hist hrs_82, width(16) discrete addplot(pci 0 28.5 .025 28.5) title(Distribution of 1982 HRS Scores) color(ebblue%45) legend(off) xtitle(HRS score 1982)
graph save "$results\q2b_hist_b16.jpg", replace

/* Local linear regressions
Run two local linear regressions, one to the right and one to the left of the
cut-point. In these regressions the midpoint rating values of each of the bins
are the regressors, and the frequency counts of each bin constitute the outcomes
*/
egen bins = cut(hrs_82), at(0(8)100)
sum bins
sum hrs_82
bys bins:egen max_bin = max(hrs_82)
bys bins:egen min_bin = min(hrs_82)
bys bins:gen mid_bin = (max_bin - min_bin)/2
bys bins: egen freq_bin = count(hrs_82)


* Regression to the left of 28.5
reg freq_bin mid_bin if hrs_82<28.5
eststo q2_left

* Regression to the right of 28.5
reg freq_bin mid_bin if hrs_82>=28.5
eststo q2_right

/*
Test whether or not the log difference in height just to the right and just to the
left of the cut-point (or the log difference of the intercepts of the two regressions) is statistically different from zero. 
*/

*Export results
esttab q2_left q2_right using "$temp\pset2_q2_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q2_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}

export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_2_b_test", modify)
restore

* Now, normalize x_i
gen hrs_norm = hrs_82 - 28.5

*Let h=5, 10, 15, 20. Flag the sample 28.5-h < hrs_norm < h+28.5. 
foreach i in 8 12 16 20 {
	gen flag_h`i' = (hrs_82>(28.5-`i') & hrs_82<(28.5+`i')) if !missing(hrs_82)
	sum hrs_norm hrs_82 if flag_h`i'==1
}

* Create interaction term between hrs_82 and NPL tract
gen hrs_npl = hrs_norm*npl2000

* Now run the regressions for each bin 
foreach i in 8 12 16 20 {
reg lnmdvalhs0 npl2000 hrs_norm hrs_npl if flag_h`i'==1
eststo q2_h`i'_norm
}

*Export results
esttab q2_h8_norm q2_h12_norm q2_h16_norm q2_h20_norm using "$temp\pset2_q2hnorm_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q2hnorm_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}
export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_2b_hnorm", modify)
restore

* Now run the regressions but X_i is not normalized
gen int_hrs_npl = hrs_82*npl2000

foreach i in 8 12 16 20 {
reg lnmdvalhs0 npl2000 hrs_82 int_hrs_npl if flag_h`i'==1
eststo q2_h`i'
}

*Export results
esttab q2_h8 q2_h12 q2_h16 q2_h20 using "$temp\pset2_q2h_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q2h_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}

export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_2b_h", modify)
restore

log close