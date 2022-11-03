/*******************************************************************************
Purpose: 
This do file executes the empirical analysis section of problem set 3 Question 1 for Applied Econometrics III.

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

log using "$log\pset3_q1_metricsIII_${username}_${date}.log", replace 

*Load data
use "$data\ps3_2022.dta", clear
count		//86,741

/*
Goal: what is the causal effect of maternal smoking during pregnancy on infant 
birthweight and other infant health outcomes.

Unit of observation - infant-mother
*/


/* Question 1: Clean data and missing values */

/*
From the codebook we know that for the following variables, Unknown or not stated is denoted by:
Tobacco - 9
Cigar - 99
Alcohol during pregnancy - 9
Drink - 99
Wtgain - 99
*/
* Sum all variables to easily detect missing values
sum 

*Focus on the variables above 
sum tobacco cigar cigar6 alcohol drink drink5 wgain

* Looks like tobacco, cigar, drink, and wgain have missing values.
foreach var in cigar drink wgain {
count if `var'==99
}

* First flag all the observations that have missing values
gen flag_missing=.
gen flag_missing_all=.
foreach var in cigar drink wgain {
	gen flag_miss_`var' = (`var'==99) if !missing(`var')
}
gen flag_miss_tobacco = (tobacco==9) if !missing(tobacco)
gen flag_miss_herpes = (herpes==8) if !missing(herpes)

* I want flag_missing_all to take the value of 1 if all of the above variables are missing and 
* flag_missing to take the value of 1 if tobacco, cigar, drink, wgain are all missing
replace flag_missing=1 if (flag_miss_tobacco==1 & flag_miss_cigar==1 & flag_miss_drink==1 & flag_miss_wgain==1)
replace flag_missing_all=1 if (flag_missing==1 & flag_miss_herpes==1)
gen all=1

/* Output summary stats for all variables for observations flagged above
putexcel set "$results\pset3_results.xlsx", sh("Summary_missing") modify

local row = 3
local ncol=1
local col: word `ncol' of `c(ALPHA)'

foreach var of varlist _all {
	putexcel `col'`row' = "`var'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
		
		foreach flag in all flag_missing_all flag_missing flag_miss_tobacco flag_miss_cigar flag_miss_drink flag_miss_wgain flag_miss_herpes {
			sum `var' if `flag'==1
			if `r(N)'==0 local mean = .
				else local mean = `r(mean)'
			if `r(N)'==0 local sd = .
				else local sd = `r(sd)'	
			putexcel `col'`row' = `mean'
				local ++ncol
				local col:word `ncol' of `c(ALPHA)'
			putexcel `col'`row' = `sd'
				local ++ncol
				local col:word `ncol' of `c(ALPHA)'
		}
	local row = `row'+1
	local ncol=1
	local col: word `ncol' of `c(ALPHA)'
}
*/
*Out of all of them, wgain has the most missing values (2222). Start by droping observations with missing wgain.
drop if wgain==99
	
*Check the rest of the variables
sum tobacco cigar cigar6 alcohol drink drink5 wgain

*There are more missing values in tobacco, cigar and drink. Drop these observations all at once
drop if tobacco==9|cigar==99|drink==99

* Do a final check of all variables
*sum 

* There are 5 missing observations in herpes (coded as 8). Drop them
drop if herpes==8

* Export summary statistics for all variables.
outreg2 using "$results/pset3_summary_all.xls", replace sum(log)

* Create treatment variable here
gen mom_smoke = (tobacco==1) if !missing(tobacco)
gen mom_no_smoke = (tobacco==2) if !missing(tobacco)

* After dropping the variables, we now save a data ready for analysis.
save "$data\ps3_2022_analysis.dta", replace

* Output codebook
*codebookout "$results\pset3_codebook.xlsx", replace

log close