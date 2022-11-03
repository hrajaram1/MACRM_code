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

log using "$log\3_pset2_q3_metricsIII_${username}_${date}.log", replace 

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

* Create a variable taking the value of 1 if the HRS score is at least 28.5
gen hrs_threshold = (hrs_82>=28.5) if !missing(hrs_82)

*Flag sites with score between 16.5 and 40.5
gen flag_sites = (hrs_82>16.5 & hrs_82<40.5) if !missing(hrs_82)

/*Q3: First stage regression*/
*Use whether or not a census tract has a site scoring above/below 28.5 as the instrument.
reg npl2000 hrs_threshold $control_var
eststo q3_fs

* Sample are sites with scores between 16.5 and 40.5
reg npl2000 hrs_threshold $control_var if flag_sites==1
eststo q3_fs_sample

*graph plotting the the 1982 HRS score against whether a site is listed on the NPL by year 2000 (NPL on the y-axis, HRS on the x -axis).
graph twoway scatter npl2000 hrs_82, xline(28.5) symbol(o) mcolor(ebblue) ylabel(, angle(0)) legend(off) xtitle("HRS score 1982") ytitle("NPL status 2000")
graph export "$results/q3_b_scatter.png", as(png) name("Graph") replace

*graph that plots the 1982 HRS score against 1980 property values (property values on the y-axis, HRS on the x -axis).
graph twoway (lfit lnmeanhs8_nbr hrs_82) scatter lnmeanhs8_nbr hrs_82, xline(28.5) symbol(o) mcolor(ebblue) ylabel(, angle(0)) legend(off) xtitle("HRS score 1982") ytitle("Log mean house prices 1980")
graph export "$results/q3_c_scatter.png", as(png) name("Graph") replace

/*Q4: 2SLS*/
*Use whether or not a census tract has a site scoring above/below 28.5 as the instrument.
ivregress 2sls lnmdvalhs0 (npl2000 = hrs_threshold) $control_var
eststo q4_2sls

ivregress 2sls lnmdvalhs0 (npl2000 = hrs_threshold) $control_var if flag_sites==1
eststo q4_2sls_sample

*Export results
esttab q3_fs q4_2sls q3_fs_sample q4_2sls_sample using "$temp\pset2_q2_4_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q2_4_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}
export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_3", modify)
restore


*** An alternative

/*Q3: First stage regression*/
*Use whether or not a census tract has a site scoring above/below 28.5 as the instrument.
reg npl2000 hrs_threshold $control_var
eststo q3_fs2

*predict npl_hat
predict npl_hat

* 2SLS 
reg lnmdvalhs0 npl_hat $control_var
eststo q4_2sls2

* Sample are sites with scores between 16.5 and 40.5
reg npl2000 hrs_threshold $control_var if flag_sites==1
eststo q3_fs2_sample

*predict hrs_threshold_hat
predict npl_hat_sample

* 2SLS 
reg lnmdvalhs0 npl_hat_sample $control_var
eststo q4_2sls2_sample


*Export results
esttab q3_fs2 q4_2sls2 q3_fs2_sample q4_2sls2_sample using "$temp\pset2_q2_4b_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q2_4b_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}
export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_3b", modify)
restore

log close