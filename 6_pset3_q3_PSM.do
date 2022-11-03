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

log using "$log\pset3_q3_metricsIII_${username}_${date}.log", replace 

*Load data
use "$data\ps3_2022_analysis.dta", clear
count		//86,741

/*
Goal: what is the causal effect of maternal smoking during pregnancy on infant 
birthweight and other infant health outcomes.

Unit of observation - infant-mother
*/

* Pre-determined factors
sum dmage ormoth mrace3 dmeduc dmar nlbnl dlivord nprevist dfage orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm cigar6 alcohol drink5

* Final list
sum dmage ormoth mrace3 dmeduc dmar nlbnl dlivord dfage orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm
global predet dmage ormoth mrace3 dmeduc dmar nlbnl dlivord dfage orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm

global covariates dmage i.ormoth i.mrace3 dmeduc dmar nlbnl dlivord dfage i.orfath dfeduc anemia cardiac lung diabetes herpes chyper pre4000 preterm

foreach var in anemia cardiac lung diabetes herpes chyper alcohol preterm pre4000 dmar {
	replace `var'=0 if `var'==2
}

* What is the reference group
foreach var in $predet {
	tab `var',m
}
* Reference group is ormoth=0, mrace3=1, orfath=0 - Non-hispanic, white mom and non-hispanic dad
* 
/* Q3a - Logit model for propensity score */
logit mom_smoke $covariates
eststo m1

predict mom_smoke_hat_ps

* Second model with significant covariates
global significant dmage i.ormoth i.mrace3 dmeduc dmar dlivord dfage i.orfath dfeduc pre4000 preterm
logit mom_smoke $significant
eststo m2

predict mom_smoke_hat_ps2

sum mom_smoke_hat_ps mom_smoke_hat_ps2 //prop score is the same.

/* Q3b - Including propensity score as covariate */
rename mom_smoke_hat_ps ps
reg dbrwt mom_smoke ps
eststo m3

* Export regression results
esttab m1 m2 m3 using "$results\pset3_q3_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 )

/* Q3c - Weighting with propensity score */
* Inverse probability weight is 1/propensity score if treatment and 1/1-ps if control
gen ipw=.
replace ipw = 1/ps if mom_smoke ==1
replace ipw = (1/1-ps) if mom_smoke ==0

* teffects is a nifty command to calculate and use ipw. Run two models: one 
*with all pre-determined covariates and one with "significant pre-determined" covariates

teffects ipw (dbrwt) (mom_smoke), ate
teffects ipw (dbrwt) (mom_smoke), atet

teffects ipw (dbrwt) (mom_smoke $covariates), ate
teffects ipw (dbrwt) (mom_smoke $covariates), atet

teffects ipw (dbrwt) (mom_smoke $significant), ate
teffects ipw (dbrwt) (mom_smoke $significant), atet

/* Output results. Use putexcel bc I want to customize the output.
putexcel set "$results/pset3_results.xlsx", sh("weighting") modify

local row = 4
local ncol=2
local col: word `ncol' of `c(ALPHA)'

foreach cov in ")" " $covariates)" " $significant)" {
foreach effect in ate atet {
    teffects ipw (dbrwt) (mom_smoke`cov', `effect'
		ereturn list
		local N = `e(N)'
		matrix tmp = r(table)
			forv i = 1(1)2 {
				local coef = tmp[1,`i']
				local coef_t = round(`coef', 0.001)
				local ser = tmp[2,`i']
				local ser_t = round(`ser', 0.001)
				local p = tmp[4,`i']
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
				local row = `row'+2
			}
		putexcel `col'`row' = `N'
		
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
		local row=4
	}
	
}
*/
/* Q4 - Blocking on propensity */
/*
A potentially more informative way to describe how birth
weight affects smoking is to estimate the "non-parametric" conditional mean of birth weight
as a function of the estimated probability of smoking, separately for smokers and non-smokers
on the same graph. To do so, divide the data from smokers into 100 approximately equally
spaced bins based on the estimated propensity score. Do the same for nonsmokers.
*/

sum ps
* What is the width of each block
disp (`r(max)'-`r(min)')/100

* Creates 100 equally sized bins for smokers
egen ps_block_smoke=cut(ps) if mom_smoke==1, group(100)
tab ps_block_smoke

* Create 100 equally sized bins for non smokers
egen ps_block_nonsmokers=cut(ps) if mom_smoke==0, group(100)
tab ps_block_nonsmokers

*Create a flag for each block
gen ps_block=.
forv i = 0(1)99 {
	replace ps_block=`i' if (ps_block_smoke==`i' | ps_block_nonsmokers==`i')
}
tab ps_block,m

* Plot a graph pscore on x axis, mean birth weight on y axis if smoker
* First calculate mean pscore and birth weight by block
bys ps_block mom_smoke: egen mean_weight=mean(dbrwt)
bys ps_block mom_smoke: egen mean_ps=mean(ps)
* Plot a graph pscore on x axis, mean birth weight on y axis if non-smoker
graph twoway (lfitci mean_weight mean_ps if mom_smoke==1) (lfitci mean_weight mean_ps if mom_smoke==0) (scatter mean_weight mean_ps)
*graph export using "$results/pset3_q4.png"

* Then within each block, we compute the treatment effect.
* In a regression framework, run reg dbrwt mom_smoke in each block

/* Then treatment effect is sum_k coeff_k*(# in treatment in block K+# in control in block K)/#in sample

local sum_r1=0
local sum_r2=0
local sum_r3=0

forv i = 0(1)99{
	reg dbrwt mom_smoke if ps_block==`i'
		local coeff_r1_`i'= r(table)[1,1]
	
	reg dbrwt mom_smoke $covariates if ps_block==`i'
		local coeff_r2_`i'= r(table)[1,1]
	
	reg dbrwt mom_smoke $significant if ps_block==`i'
	local coeff_r3_`i'= r(table)[1,1]
	
	count if mom_smoke==1 & ps_block==`i'
		local N1_`i'=`r(N)'
	count if mom_smoke==0 & ps_block==`i'
		local N0_`i'=`r(N)'
	count
	* Calculate (# in treatment in block K+# in control in block K)/#in sample
	local N = (`N1_`i''+`N0_`i'')/`r(N)'
	
	local t_r1_`i'= `coeff_r1_`i''*`N'
	local t_r2_`i'= `coeff_r2_`i''*`N'
	local t_r3_`i'= `coeff_r3_`i''*`N'

local sum_r1=`sum_r1'+`t_r1_`i''
disp "Treatment effect is `sum_r1'"

local sum_r2=`sum_r2'+`t_r2_`i''
disp "Treatment effect is `sum_r2'"

local sum_r2=`sum_r2'+`t_r2_`i''
disp "Treatment effect is `sum_r2'"
}
*/

/* Q5 - low birth weight */
gen low_dbrwt = (dbrwt<2500) if !missing(dbrwt)

*Plot graph
* First calculate mean pscore and birth weight by block
bys ps_block mom_smoke: egen mean_low_weight=mean(low_dbrwt)

* Plot a graph pscore on x axis, mean birth weight on y axis if non-smoker
graph twoway (lfitci mean_low_weight mean_ps if mom_smoke==1) (lfitci mean_low_weight mean_ps if mom_smoke==0) (scatter mean_low_weight mean_ps) 
*graph export using "$results/pset3_q5.jpg"

/* Run regression

local sum_r1=0
local sum_r2=0
local sum_r3=0

forv i = 0(1)99{
	reg low_dbrwt mom_smoke if ps_block==`i'
		local coeff_r1_`i'= r(table)[1,1]
	
	reg low_dbrwt mom_smoke $covariates if ps_block==`i'
		local coeff_r2_`i'= r(table)[1,1]
	
	reg low_dbrwt mom_smoke $significant if ps_block==`i'
	local coeff_r3_`i'= r(table)[1,1]
	
	count if mom_smoke==1 & ps_block==`i'
		local N1_`i'=`r(N)'
	count if mom_smoke==0 & ps_block==`i'
		local N0_`i'=`r(N)'
	count
	* Calculate (# in treatment in block K+# in control in block K)/#in sample
	local N = (`N1_`i''+`N0_`i'')/`r(N)'
	
	local t_r1_`i'= `coeff_r1_`i''*`N'
	local t_r2_`i'= `coeff_r2_`i''*`N'
	local t_r3_`i'= `coeff_r3_`i''*`N'

local sum_r1=`sum_r1'+`t_r1_`i''
disp "Treatment effect is `sum_r1'"

local sum_r2=`sum_r2'+`t_r2_`i''
disp "Treatment effect is `sum_r2'"

local sum_r2=`sum_r2'+`t_r2_`i''
disp "Treatment effect is `sum_r2'"
}
*/
********************************
*OR TRY ANOTHER WAY USING PSREG
psreg dbrwt mom_smoke $covariates, groups(100) quietly
psreg dbrwt mom_smoke $covariates, groups(100) ate quietly
psreg low_dbrwt mom_smoke $covariates, groups(100) quietly
psreg low_dbrwt mom_smoke $covariates, groups(100) ate quietly

/* Output results. Use putexcel bc I want to customize the output.
putexcel set "$results/pset3_results.xlsx", sh("blocking") modify

local row = 4
local ncol=2
local col: word `ncol' of `c(ALPHA)'

foreach y in dbrwt low_dbrwt {
foreach cov in "$covariates" "$significant" {
    psreg `y' mom_smoke `cov', groups(100) ate quietly
		local coef = `r(ate)'
			local coef_t = round(`coef', 0.001)
		local ser = `r(seate)'
			local ser_t = round(`ser', 0.001)
				
		putexcel `col'`row' = "`coef_t'"
				local row=`row'+1
		putexcel `col'`row' = "(`ser_t')"
				local row = `row'+2
		putexcel `col'`row' = `r(n_t)'
			local row=`row'+1
		putexcel `col'`row'	= `r(n_c)'
			local row = `row'+1
		putexcel `col'`row' = `r(blocks)'	
		
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
		local row=4
	psreg `y' mom_smoke `cov', groups(100) quietly
		local coef = `r(att)'
			local coef_t = round(`coef', 0.001)
		local ser = `r(seatt)'
			local ser_t = round(`ser', 0.001)
				
		putexcel `col'`row' = "`coef_t'"
				local row=`row'+1
		putexcel `col'`row' = "(`ser_t')"
				local row = `row'+2
		putexcel `col'`row' = `r(n_t)'
			local row=`row'+1
		putexcel `col'`row'	= `r(n_c)'
			local row = `row'+1
		putexcel `col'`row' = `r(blocks)'	
		
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
		local row=4	
	}
}
*/

log close

