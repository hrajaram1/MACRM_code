/*******************************************************************************
Purpose: 
This do file executes the empirical analysis section of problem set 2 for Applied Econometrics III.

Created on: 4/13/2022
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

log using "$log\1_pset2_q1_metricsIII_${username}_${date}.log", replace 

*Load data
use "$data\allsites_2022.dta", clear
count		//40,248

*Recommended variables
*Housing characteristics for 1(a):
global hous_char firestoveheat80 noaircond80 nofullkitchen80 zerofullbath80 bedrms* blt* detach80occ mobile80occ

*Economic and demographic variables for 1(a):
global demo_var pop_den8 shrblk8 shrhsp8 child8 old8 shrfor8 ffh8 smhse8 hsdrop8 no_hs_dipl* ba_or_b* unemprt8 povrat8 welfare8 avhhin8 tothsun8 ownocc8 occupied80

* control variables for 3a (and again in 4):
global control_var firestoveheat80_nbr noaircond80_nbr nofullkitchen80_nbr zerofullbath80_nbr bedrms* blt* detach80occ_nbr mobile80occ_nbr pop_den8_nbr shrblk8_nbr shrhsp8_nbr child8_nbr shrfor8_nbr ffh8_nbr smhse8_nbr hsdrop8_nbr no_hs_dipl* ba_or_b* unemprt8_nbr povrat8_nbr welfare8_nbr avhhin8_nbr tothsun8_nbr ownocc8_nbr occupied80_nbr

* Hypothesis: In the case of an environmental "bad", such as living in close proximity to a hazardous waste site, one would expect that the market price of nearby housing would increase if the hazardous waste site were cleaned up (all else equal).



*Check if data is unique at US Census tract level
*isid fips
*isid fips state
*isid fips statefips
duplicates tag fips, gen(tag)
/*
. tab tag

        tag |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     40,272       99.61       99.61
          1 |        128        0.32       99.93
          2 |         15        0.04       99.97
          3 |          8        0.02       99.99
          4 |          5        0.01      100.00
------------+-----------------------------------
      Total |     40,428      100.00
*/
tab tag
br if tag!=0
sort fips
*Claire Fan said to ignore duplicates and run analysis as is
*duplicates drop fips, force
*isid fips

/* #1A: Four Regressions, Y = 2000 housing prices */
* Did a census tract have an NPL site in 2000?
desc npl2000
tab npl2000			//925 had NPL before 2000

* Outcome - 2000 housing prices
desc lnmdvalhs0 lnmeanhs8
sum lnmdvalhs0 lnmeanhs8

* First regress 2000 housing prices on whether the census tract had an NPL site in 2000. Include 1980 housing values as a control. 
reg lnmdvalhs0 npl2000 lnmeanhs8
eststo m1_a

* Next add housing characteristics as controls. 
reg lnmdvalhs0 npl2000 lnmeanhs8 $hous_char
eststo m1_b

* Run a third regression adding economic and demographic variables as controls. 
reg lnmdvalhs0 npl2000 lnmeanhs8 $hous_char $demo_var
eststo m1_c

* Finally run a 4th regression that also includes state fixed effects. 
xtset statefips
xtreg lnmdvalhs0 npl2000 lnmeanhs8 $hous_char $demo_var, fe 
eststo m1_d

*Output results
esttab m1_a m1_b m1_c m1_d using "$temp\pset2_q1_tables_metrics_III.csv", se replace stats(r2 N) star(* 0.10 ** 0.05 *** 0.01 ) 

*Add table as new sheet in main results spreadsheet
preserve
import delimited "$temp\pset2_q1_tables_metrics_III.csv", stripquote(yes) clear

foreach v of varlist _all {
	replace `v' = subinstr(`v', "=", "", 1)
}

export excel using "$results\psets_tables_metrics_III.xlsx", sheet("pset2_1_a", modify)
restore
********************************************************************************************************************
/* #1B: Four Regressions, Y = 2000 housing prices */

*Load data
use "$data\allcovariates_2022.dta", clear
count		//45,378

*This dataset is not unique at fips level. Should we deduplicate the data by fips before running the regressions? 
*What is the unique identifier in this data? Is it still census tract? If so, what var corresponds to census tract?
*Housing characteristics for 1(a):
global hous_char firestoveheat80 noaircond80 nofullkitchen80 zerofullbath80 bedrms02_80 bedrms34_80 bedrms0_80occ bedrms1_80occ bedrms2_80occ bedrms3_80occ bedrms4_80occ bedrms5_80occ bltlast5yrs80 bltlast10yrs80 blt0_1yrs80occ blt2_5yrs80occ blt6_10yrs80occ blt10_20yrs80occ blt20_30yrs80occ blt30_40yrs80occ blt40_yrs80occ bltmore30_80 detach80occ mobile80occ

*Economic and demographic variables for 1(a):
global demo_var pop_den8 shrblk8 shrhsp8 child8 shrfor8 ffh8 smhse8 hsdrop8 no_hs_diploma8 ba_or_better8 unemprt8 povrat8 welfare8 avhhin8 tothsun8 ownocc8 occupied80

* Check for balance of covariates
global covariates meanhs8 $hous_char $demo_var

local var_label: variable label meanhs8
disp "`var_label'"

putexcel set "$results/psets_tables_metrics_III.xlsx", sheet("pset2_1_b") modify

local row = 3
local ncol = 1
local col: word `ncol' of `c(ALPHA)'

foreach var in $covariates {
	putexcel `col'`row' = "`var'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	local var_label: variable label `var'
	putexcel `col'`row' = "`var_label'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	*Overall mean and standard error
	sum `var',d
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
	
	*Treatment mean and standard error
	sum `var' if nbr_dummy ==1
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
	*Control mean and standard error
	sum `var' if nbr_dummy ==0
		foreach m in N mean sd {
			local `m' = `r(`m')'
		}
	putexcel `col'`row' = `N'
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	putexcel `col'`row' = `mean'
		local row = `row'+1
	putexcel `col'`row' = -`sd'
		
		local row = `row'+1
		
		local ncol=1
		local col:word `ncol' of `c(ALPHA)'
}

*Load data
use "$data\sitecovariates_2022.dta", clear
count		//418

* Check for balance of covariates
global covariates meanhs8 $hous_char $demo_var

* Flag different samples. Sample 1: all
gen all = 1

*Sample 2: above 28.5 vs below 28.5
gen sample_2= (hrs_82<28.5) if !missing(hrs_82)

* Sample 3: sites between 16.5 and 28.5 vs 28.5 and 40.5
gen sample_3=.
replace sample_3=1 if hrs_82>16.5 & hrs_82<28.5
replace sample_3=0 if hrs_82>28.5 & hrs_82<40.5

*Check if the samples were correctly flagged
bys sample_3:sum hrs_82  
bys sample_2:sum hrs_82

*Output results
putexcel set "$results/psets_tables_metrics_III.xlsx", sheet("pset2_1_b_HRS_all") modify

local row = 3
local ncol = 1
local col: word `ncol' of `c(ALPHA)'

foreach var in $covariates {
	putexcel `col'`row' = "`var'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	local var_label: variable label `var'
	putexcel `col'`row' = "`var_label'"
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	
	*Overall mean and standard error
	sum `var',d
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
	
	*Treatment mean and standard error
	sum `var' if nbr_dummy ==1
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
	*Control mean and standard error
	sum `var' if nbr_dummy ==0
		foreach m in N mean sd {
			local `m' = `r(`m')'
		}
	putexcel `col'`row' = `N'
		local ++ncol
		local col:word `ncol' of `c(ALPHA)'
	putexcel `col'`row' = `mean'
		local row = `row'+1
	putexcel `col'`row' = -`sd'
		
		local row = `row'+1
		
		local ncol=1
		local col:word `ncol' of `c(ALPHA)'
}

* Output results for sample 2 and 3
foreach i in 2 3 {
	putexcel set "$results/psets_tables_metrics_III.xlsx", sheet("pset2_1_b_HRS_sample_`i'") modify

	local row = 4
	local ncol = 1
	local col: word `ncol' of `c(ALPHA)'

	foreach var in $covariates {
		putexcel `col'`row' = "`var'"
			local ++ncol
			local col:word `ncol' of `c(ALPHA)'
	
		local var_label: variable label `var'
		putexcel `col'`row' = "`var_label'"
			local ++ncol
			local col:word `ncol' of `c(ALPHA)'
	
		*Overall mean and standard error
		sum `var',d
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
	
		*Treatment mean and standard error
		foreach v in 1 0 {
		sum `var' if nbr_dummy ==1 & sample_`i'==`v'
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
		
		*Control mean and standard error
		foreach v in 1 0 {
		sum `var' if nbr_dummy ==0 & sample_`i'==`v'
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
		
			local ncol=1
			local col:word `ncol' of `c(ALPHA)'
			local row = `row'+2
	}
}

log close