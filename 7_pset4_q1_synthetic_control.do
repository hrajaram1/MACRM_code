/*******************************************************************************
Purpose: 
This do file executes the empirical analysis section of problem set 3 Question 2 for Applied Econometrics III.

Created on: 5/22/2022
By: Hersheena Rajaram

Last modified on: 5/22/2022
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
global synth "C:\Users\hraja\Downloads\synth_state"

log using "$log\pset4_q1_metricsIII_${username}_${date}.log", replace 

*Load data
use "$data\traffic_safety_ps4_2022", clear
count		//1104

tab state
* There are 23 obs in the treatment state. The treatment satte combines population 
* weighted data from the first 4 states to have a primary seatbelt law (CT, IA, NM, TX).

/*
Qu. 1A.i - Compare the average pre-period log traffic fatalities per capita of the TU site to
that of the average of all the "control" states. Next, graph the pre-period log traffic
fatalities by year for the pre-period for both the TU and the average of the control
group. Interpret.
*/ 

* What is the Treatment year
tab year primary if state==99
* Primary seatbelt law starts in 1986. Pre-treatment period is 1981-1985, Post-treatment is 1986-2003.
gen treatment_years = (year>=1986)
la var treatment_years "Indicator if year is a treatment year"

* Calculate average pre-period log traffic fatalities per capita
* of TU siteujbv
gen fatalities_per_capita = fatalities/population
gen log_fatalities_per_capita = log(fatalities_per_capita)
sum log_fatalities_per_capita if state==99
sum log_fatalities_per_capita if state!=99
sum log_fatalities_per_capita

* Pre-treatment
sum log_fatalities_per_capita if state==99 & treatment_years==1
sum log_fatalities_per_capita if state!=99 & treatment_years==1
sum log_fatalities_per_capita if treatment_years==1

* Quick test of the difference between control and treatment state
gen treatment_state = (state==99)
reg log_fatalities_per_capita treatment_state

* Graph 
grstyle clear
grstyle init
grstyle color background white
grstyle anglestyle vertical_tick horizontal
*grstyle color ci_area gs12%50
*grstyle color ci_arealine gs12%0
two (scatter log_fatalities_per_capita year if treatment_years==0 & state==99, mcolor(edkblue)) (scatter log_fatalities_per_capita year if treatment_years==0 & state!=99, mcolor(eltblue)), ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in pre-period") legend(off) 
graph save "$results/pset4_graph1.png", replace

graph twoway (lfitci log_fatalities_per_capita year if treatment_years==0 & state==99, clc(edkblue)) (scatter log_fatalities_per_capita year if treatment_years==0 & state==99,mcolor(edkblue)) , ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in treatment state in pre-period") legend(off)
graph save "$results/pset4_graph2.png", replace

graph twoway (lfitci log_fatalities_per_capita year if treatment_years==0 & state!=99, clc(eltblue)) (scatter log_fatalities_per_capita year if treatment_years==0 & state!=99,mcolor(eltblue)) , ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in control states in pre-period") legend(off)
graph save "$results/pset4_graph3.png", replace

* Across all years
two (scatter log_fatalities_per_capita year if state==99, mcolor(edkblue)) (scatter log_fatalities_per_capita year if state!=99, mcolor(eltblue)), ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in pre-period") legend(off) xline(1986.5)
graph save "$results/pset4_graph4_all.png", replace

graph twoway (lfitci log_fatalities_per_capita year if state==99, clc(edkblue)) (scatter log_fatalities_per_capita year if state==99,mcolor(edkblue)) , ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in treatment state in pre-period") legend(off) xline(1986.5)
graph save "$results/pset4_graph5_all.png", replace

graph twoway (lfitci log_fatalities_per_capita year if state!=99, clc(eltblue)) (scatter log_fatalities_per_capita year if state!=99,mcolor(eltblue)) , ytitle("Log of fatalities per capita") xtitle("Year") title("Log of fatalities per capita in control states in pre-period") legend(off) xline(1986.5)
graph save "$results/pset4_graph6_all.png", replace

/*
Compare the dependent variable between the TU site and each control state for the
years before the treatment (Note: you can calculate MSE for the years). Which
control state best matches the TU? Now compare this state's covariates with the TU
covariates. Do they appear similar? What might this imply for in terms of using this
state as the counterfactual state?
*/

bys state: sum log_fatalities_per_capita if treatment_years==0
decode state, g(state_str)
replace state_str="Treatment state" if state==99

/*
Q1.B - SYNTHETIC CONTROL METHOD

*/
la val state
tsset state year
*synth log_fatalities_per_capita college beer unemploy totalvmt precip snow32, trunit(99) trperiod(1986) fig nested
matrix define A = e(X_balance)
matrix define B = e(V_matrix)
matrix define C = e(Y_synthetic)
matrix define D = e(Y_treated)
matrix define E = e(RMSPE)

* Output results. Use putexcel bc I want to customize the output.
putexcel set "$results/pset4_results.xlsx", sh("synth") modify
putexcel B2 = matrix(A)
putexcel B15 = matrix(B)
putexcel H2 = matrix(C)
putexcel I2 = matrix(D)
putexcel A1 = matrix(E)
*/

/*
Q1.C - Graphical interpretation and treatment signficance.

i. Generate graphs plotting the gap between the TU and the synthetic control group
under both your preferred specfication and a few other specfications you tried.
*/
*synth log_fatalities_per_capita college beer unemploy totalvmt precip snow32, trunit(99) trperiod(1986) fig nested

* Create different specifications
* sample 1
tab year state  if primary ==1
*The following states have treatment - primary seat belt law in the following years. Recall that treatment starts in 1986
* AL - 2000 to 2003 || CA - 1993 to 2003 || CT - 1986 to 2003 || DE - 2003 || IA - 1986 to 2003 || IN - 1998 to 2003 || LA - 1996 to 2003 || MD - 1998 to 2003 || MI - 2000 to 2003 ||NC - 1987 to 2003 || NJ - 2000 to 2003 || NM - 1986 to 2003 || NY - 1984 to 2003 ||OK - 1998 to 2003 || OR - 1991 to 2003 || TX - 1986 to 2003 || WA - 2002-2003
* I want to compare fatality in treatment state because it had a primary seatbelt in 1986. I want to compare it against states that do not have primary seatbelt 
* Most of the states have secondary law

* Now, for controls we consider different combinationa
global cov1 college beer unemploy totalvmt precip snow32
global cov2 beer totalvmt precip snow32

* Sample 1 is all - preferred specification
gen sample_1=1

* Sample 2 excludes states that had a primary seatbelt law at any point in time
bys state: egen sample_2 = max(primary) 
replace sample_2=1 if state==99

* Sample 3 - only exclude NY since it started primary seatbelt in pre period
gen sample_3=1
replace sample_3=0 if state==32
*
forv i = 1/3 {
preserve
keep if sample_`i'==1
tsset state year
	synth log_fatalities_per_capita $cov1, trunit(99) trperiod(1986) fig
		graph export "$results/ps4_synth_cov1_s`i'.jpg", replace
		
		*output results
		matrix define A = e(X_balance)
		matrix define B = e(V_matrix)
		matrix define C = e(Y_synthetic)
		matrix define D = e(Y_treated)
		matrix define E = e(RMSPE)

		* Output results. Use putexcel bc I want to customize the output.
		putexcel set "$results/pset4_results.xlsx", sh("synth_cov1_s`i'") modify
		putexcel B2 = matrix(A)
		putexcel B15 = matrix(B)
		putexcel H2 = matrix(C)
		putexcel I2 = matrix(D)
		putexcel A1 = matrix(E)

	synth log_fatalities_per_capita $cov2, trunit(99) trperiod(1986) fig 
		graph export "$results/ps4_synth_cov2_s`i'.jpg", replace
		*output results
		matrix define A = e(X_balance)
		matrix define B = e(V_matrix)
		matrix define C = e(Y_synthetic)
		matrix define D = e(Y_treated)
		matrix define E = e(RMSPE)

		* Output results. Use putexcel bc I want to customize the output.
		putexcel set "$results/pset4_results.xlsx", sh("synth_cov2_s`i'") modify
		putexcel B2 = matrix(A)
		putexcel B15 = matrix(B)
		putexcel H2 = matrix(C)
		putexcel I2 = matrix(D)
		putexcel A1 = matrix(E)
restore
}
*/
/*
ii. Compare the graph plotting the gap between the TU and the synthetic control group
under your preferred specification with the graphs plotting the gap between each
control state and its "placebo" treatment. Do you conclude that the treatment was
significant? Why or why not?
*/

** loop through units
global state 1 2 3 4 5 6 7 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 99
foreach s in $state {
	qui synth log_fatalities_per_capita college beer unemploy totalvmt precip snow32, trunit(`s') trperiod(1986) fig keep($synth\synth_`s', replace)
}

*Now loop through all saved datasets and create the relevant variables (years and treatment effect). 
foreach s in $state {
	use "$synth\synth_`s'", clear
	rename _time years
	gen tr_effect_`s' = _Y_treated - _Y_synthetic
	keep years tr_effect_`s'
	drop if missing(years)
	
	save "$synth\synth_`s'", replace
}

* load the first dataset and merge all the remaining datasets.
use "$synth\synth_1", clear
foreach s in 2 3 4 5 6 7 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 99 {
qui merge 1:1 years using "$synth\synth_`s'", nogenerate
}

* GRAPH PLOTS
local lp
foreach s in $state {
	local lp `lp' line tr_effect_`s' years, lcolor(gs12) ||
}

* create plot
twoway `lp' || line tr_effect_99 years, ///
lcolor(edkblue) legend(off) xline(1986, lpattern(dash)) xtitle(Years) ytitle(Avergae log traffic fatalities per capita)

/* Output results. Use putexcel bc I want to customize the output.
putexcel set "$results/pset4_results.xlsx", sh("q1.ii") modify

local row = 3
local ncol=1
local col: word `ncol' of `c(ALPHA)'

levelsof state_str
foreach l in `r(levels)' {
	local state="`l'"
putexcel `col'`row' = "`l'"

	local ++ncol
	local col: word `ncol' of `c(ALPHA)'
	
	sum log_fatalities_per_capita if state_str=="`l'" & treatment_years==0
	putexcel `col'`row'=`r(mean)'
		local row = `row'+1
	putexcel `col'`row'=`r(sd)'
		local row = `row'-1
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
		
	foreach var in college beer primary secondary population unemploy fatalities totalvmt precip snow32 rural_speed urban_speed{
		sum `var' if state_str=="`l'" & treatment_years==0
			putexcel `col'`row' = `r(mean)'
				local row = `row'+1
			putexcel `col'`row' = `r(sd)'
				local row = `row'-1
				local ++ncol
				local col:word `ncol' of `c(ALPHA)'
	}
local row=`row'+2
local ncol=1
local col:word `ncol' of `c(ALPHA)'	
}

* Output results. Use putexcel bc I want to customize the output.
putexcel set "$results/pset4_results.xlsx", sh("q1.i") modify

local row = 3
local ncol=2
local col: word `ncol' of `c(ALPHA)'
foreach h in "state==99" "state!=99" "state!=." {
	sum log_fatalities_per_capita if `h'
	
	putexcel `col'`row' = `r(mean)'
		local row =`row'+1
	putexcel `col'`row' = `r(sd)'
		local row=`row'+1
}		
local row = 3
local ncol=3
local col: word `ncol' of `c(ALPHA)'
foreach r in 1 0 {
	foreach h in "state==99" "state!=99" "state!=." {
	sum log_fatalities_per_capita if `h' & treatment_years==`r'
	
	putexcel `col'`row' = `r(mean)'	
		local row =`row'+1
	putexcel `col'`row' = `r(sd)'
		local row=`row'+1
	}
local row = 3
local ++ncol
local col: word `ncol' of `c(ALPHA)'	
}	




log close
