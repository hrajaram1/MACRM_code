/*******************************************************************************
Purpose: 
This do file executes the empirical analysis section of problem set 3 Question 2 for Applied Econometrics III.

Created on: 5/7/2022
By: Hersheena Rajaram

Last modified on: 5/7/2022
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

log using "$log\pset3_q2_metricsIII_${username}_${date}.log", replace 

*Load data
use "$data\ps3_2022_analysis.dta", clear
count		//86,741

/*
Goal: what is the causal effect of maternal smoking during pregnancy on infant 
birthweight and other infant health outcomes.

Unit of observation - infant-mother
*/

/* Question 2: Mean difference in APGAR scores */
sum 
drop all
gen all=1

* Output summary stats for all variables for observations flagged above
putexcel set "$results/pset3_results.xlsx", sh("q2_a_mean_diff") modify

local row = 3
local ncol=2
local col: word `ncol' of `c(ALPHA)'

forv i = 1(1)2 {
    foreach var in omaps fmaps dbrwt {
    	sum `var' if tobacco==`i'
		putexcel `col'`row' = `r(mean)'
			local row = `row'+1
	}	
	local ++ncol
	local col:word `ncol' of `c(ALPHA)'
	local row = 3
}		
/*
Under what circumstances can one identify the average treatment effect of maternal
smoking by comparing the unadjusted difference in mean birth weight of infants of smoking
and non-smoking mothers? Estimate its impact under this assumption.
*/

reg dbrwt mom_smoke
eststo m1 
esttab m1 using "$results\pset3_q2b_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

* Pre-determined factors
sum dmage ormoth mrace3 dmeduc dmar nlbnl dlivord dfage orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm 
global covariates dmage i.ormoth i.mrace3 dmeduc dmar nlbnl dlivord dfage i.orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm

/*
Use a basic linear regression model, in conjunction with your answer to part (c), to
estimate the impact of smoking and report your estimates.
*/

reg dbrwt mom_smoke $covariates 
eststo m1 
esttab m1 using "$results\pset3_q2d_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

drop flag_missing flag_missing_all flag_miss_cigar flag_miss_drink flag_miss_wgain flag_miss_tobacco flag_miss_herpes

* Output a balance table. Let's call mother's who smoke to be the treatment.
putexcel set "$results/pset3_results.xlsx", sh("q2_balance_table") modify

local row = 3
local ncol=1
local col: word `ncol' of `c(ALPHA)'

foreach var of varlist _all {
	putexcel `col'`row' = "`var'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	local var_label: variable label `var'
	putexcel `col'`row' = "`var_label'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	*Overall mean and standard error
	foreach flag in all mom_smoke mom_no_smoke {
		sum `var' if `flag'==1
			foreach m in N mean sd {
				local `m' = `r(`m')'
			}
		putexcel `col'`row' = `N'
			local ++ncol
			local col:word `ncol' of `c(ALPHA)'
		putexcel `col'`row' = `mean'
			local row = `row'+1
		putexcel `col'`row' = -`sd'
			local row = `row'-1
			local ++ncol
			local col:word `ncol' of `c(ALPHA)'
	}

	local row = `row'+2
	local ncol=1
	local col:word `ncol' of `c(ALPHA)'
}

* Check if difference is statistically significant

local row = 3
local ncol=9
local col: word `ncol' of `c(ALPHA)'

foreach var of varlist _all {
    reg `var' mom_smoke
		 matrix tmp = r(table)
			local coef = tmp[1,2]
			local coef_t = round(`coef', 0.001)
			local ser = tmp[2,2]
			local ser_t = round(`ser', 0.001)
			local p = tmp[4,2]
		if `p'<0.01 {
			local coef_t = "`coef_t'"+"***"		// Set up signifcance stars for 1%(***), 5%(**) and 10%(*) significance levels
		}
		else if `p'<0.05 {
			local coef_t = "`coef_t'"+"**"
		}
		else if `p'<0.10 {
			local coef_t = "`coef_t'"+"*"
		}
	putexcel `col'`row' = "`coef_t'"
			local row=`row'+1
	putexcel `col'`row' = "(`ser_t')"
			local row = `row'+1  
}
*/
log close
