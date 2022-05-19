/* Calepa Carbon Neutrality

Impute Rystad-asset level cost variables
Because Rystad data records NA or 0 cost if cost was not estimated or projected to be incurred

Ruiwen Lee
Created:  17 Aug 2020
Modified: 13 Oct 2020
Modified (for paper): 12 May 2021

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

global workDir "//Users/rui/Dropbox/Research/CarbonNeutrality/STATA/"

global codeDir  "codeSTATA"
global dataDir  "dataSTATA"
global dataRawDir "dataRAW"
global tempDir	"temp"
global resultsDir "resultsSTATA"
global resultsTextDir "resultsText"
global logDir     "logs"
global docDir	  "docs"
global texDir	  "tex"

global resultsGoogleDriveDir "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost"

cd $workDir
log using $logDir/impute.log, replace

********** Start **********

***** IMPUTE capex_per_bbl_nom and opex_per_bbl_nom at Rystad asset level

insheet using $dataRawDir/rystad_entry_variables.csv, comma names clear

***** Prepare variables *****
encode original_asset_name, gen(asset)
order asset
sort asset year
destring capex_per_bbl_nom opex_per_bbl_nom, force replace

gen capex_per_bbl_nom_orig = capex_per_bbl_nom
gen opex_per_bbl_nom_orig = opex_per_bbl_nom

// Set largest p% to missing: opex 0.5%, capex 5%
egen opex_per_bbl_nom_hi = pctile(opex_per_bbl_nom), p(99.5)
replace opex_per_bbl_nom=. if opex_per_bbl_nom>opex_per_bbl_nom_hi
egen capex_per_bbl_nom_hi = pctile(capex_per_bbl_nom), p(95)
replace capex_per_bbl_nom=. if capex_per_bbl_nom>capex_per_bbl_nom_hi
drop opex_per_bbl_nom_hi capex_per_bbl_nom_hi

// Assume that market price cannot really be zero. 
// Zero in data means firm didn't spend, not that market price is zero
replace capex_per_bbl_nom=. if capex_per_bbl_nom==0
replace opex_per_bbl_nom=. if opex_per_bbl_nom==0
sum capex_per_bbl_nom opex_per_bbl_nom, det

***** Prediction model *****

*** Interpolate to impute missing capex and opex
bysort asset: ipolate opex_per_bbl_nom year, gen(opex_impute)
sum opex_per_bbl_nom opex_impute
by asset: ipolate capex_per_bbl_nom year, gen(capex_impute) 
sum capex_per_bbl_nom capex_impute

* Expand dataset to 2045
bysort asset (year): gen last = _n == _N
expand 27 if last
drop last
bysort asset (year): replace year = year[_n-1] + 1 if _n >42
foreach var of varlist capex_impute opex_impute capex_per_bbl_nom opex_per_bbl_nom capex_per_bbl_nom_orig opex_per_bbl_nom_orig {
	replace `var'=. if year>2019
}
gen capex_forecast=capex_impute if year<=2019
gen opex_forecast=opex_impute if year<=2019
gen capex_impute_hat=.
gen opex_impute_hat=.

bysort asset: egen capex_count=count(capex_per_bbl_nom)
bysort asset: egen opex_count=count(opex_per_bbl_nom)
gen capex_pool=capex_count<20
gen opex_pool=opex_count<20

sum asset
local a_start=r(min)
local a_end=r(max)

forval a=`a_start'/`a_end' {
	
	capture {
		reg capex_per_bbl_nom year if asset==`a' & capex_pool==0
		replace capex_impute_hat=_b[year] if asset==`a' & capex_pool==0
		predict capex_forecast_temp if asset==`a' & capex_pool==0
		replace capex_forecast=capex_forecast_temp if asset==`a' & capex_pool==0 & capex_forecast==.
		drop capex_forecast_temp
	}
	
	capture {
		reg opex_per_bbl_nom year if asset==`a' & opex_pool==0
		replace opex_impute_hat=_b[year] if asset==`a'
		predict opex_forecast_temp if asset==`a'
		replace opex_forecast=opex_forecast_temp if asset==`a' & opex_forecast==.
		drop opex_forecast_temp
	}	
	
}

* Pool assets with less than 21 years of cost data
xtset asset
xtreg capex_per_bbl_nom year if capex_pool==1, fe
replace capex_impute_hat=_b[year] if capex_pool==1
predict capex_forecast_temp_xb if capex_pool==1, xb
predict capex_forecast_temp_u if capex_pool==1, u // FE
bysort asset: egen capex_forecast_temp_fe=max(capex_forecast_temp_u)
replace capex_forecast=capex_forecast_temp_xb+capex_forecast_temp_fe if capex_pool==1 & capex_forecast==.
drop capex_forecast_temp

xtreg opex_per_bbl_nom year if opex_pool==1, fe
replace opex_impute_hat=_b[year] if opex_pool==1
predict opex_forecast_temp_xb if opex_pool==1, xb
predict opex_forecast_temp_u if opex_pool==1, u // FE
bysort asset: egen opex_forecast_temp_fe=max(opex_forecast_temp_u)
replace opex_forecast=opex_forecast_temp_xb+opex_forecast_temp_fe if opex_pool==1 & opex_forecast==.
drop opex_forecast_temp*

sum capex_forecast opex_forecast

***** Replace negative cost years with minimum non-neg cost *****
foreach costtype in capex opex { //
	bysort asset: egen `costtype'_min1=min(`costtype'_forecast) if `costtype'_forecast>0
	bysort asset: egen `costtype'_min=max(`costtype'_min1)
	replace `costtype'_forecast=`costtype'_min if `costtype'_forecast<0
	drop `costtype'_min* 
}

save $dataDir/cost_imputed_all_assets, replace
outsheet original_asset_name year capex_forecast opex_forecast using $resultsTextDir/Rystad_cost_imputed_all_assets.csv if year>1977, comma replace
outsheet original_asset_name year capex_forecast opex_forecast using "$resultsGoogleDriveDir/Rystad_cost_imputed_all_assets.csv" if year>1977, comma replace
 

*** Plot imputed values against original by asset

* Opex
sum asset
local a_start=r(min)
local a_end=r(max)
forval a=`a_start'/`a_end' {
	twoway (scatter opex_forecast year) (scatter opex_per_bbl_nom year)	if asset==`a' & year>1977 ///
	, title(Asset no. `a') name(Opex`a', replace)
}

* Capex
sum asset
local a_start=r(min)
local a_end=r(max)
forval a=`a_start'/`a_end' { //`a_start'/`a_end'
	twoway (scatter capex_forecast year) ///
	(scatter capex_per_bbl_nom year) if asset==`a' & year>1977 ///
	, title(Asset no. `a') name(Capex`a', replace)
} 
