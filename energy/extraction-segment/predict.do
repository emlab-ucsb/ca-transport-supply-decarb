/* Calepa Carbon Neutrality

Predict entry

Ruiwen Lee
Created:  29 Apr 2021
Modified:  8 Jun 2021

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

global workDir "/Dropbox/Research/CarbonNeutrality/STATA/"

global codeDir "codeSTATA"
global dataDir "dataSTATA"
global dataRawDir "dataRAW"
global tempDir	"temp"
global resultsDir "resultsSTATA"
global resultsTextDir "resultsText"
global logDir   "logs"
global docDir	 "docs"
global texDir	 "tex"

global resultsGoogleDriveDir "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/entry-model-results"

cd $workDir
*log using $logDir/predict.log, replace

********** Start **********


******************* Field level model with topfield FEs ************************
* Jointly estimate all coefficients except for Brent;
* Brent coefficient topfield-specific

********** Build model using full historical series (none held out) ************
use $dataDir/entry_revised, replace // from entry.do

*** calepa corrected version (version revised 210512)
poisson new_wells i.topfield##c.brent capex_imputed opex_imputed depl, robust
predict new_wells_pred

* Save coefficients
gen brent_hat=_b[brent]
gen cons_hat=_b[_cons]

forval tf=1/10 {

	replace brent_hat = brent_hat + _b[`tf'.topfield#c.brent] if topfield==`tf'
	replace cons_hat = cons_hat + _b[`tf'.topfield] if topfield==`tf'

}
gen capex_hat=_b[capex_imputed]
gen opex_hat=_b[opex_imputed]
gen depl_hat=_b[depl]

*** break up non-top fields into 10 quantiles (version revised 210607) ***
xtset doc_field_code year
xtpoisson new_wells i.field_categ##c.brent capex_imputed opex_imputed depl, fe robust nolog
eststo xtpoisson
predict new_wells_poisson_fitted, xb
gen double new_wells_poisson = exp(new_wells_poisson_fitted)
egen mean_new_wells=mean(new_wells), by(doc_field_code)
egen mean_new_wells_poisson=mean(new_wells_poisson), by(doc_field_code)
gen double new_wells_exp_alpha=mean_new_wells/mean_new_wells_poisson if mean_new_wells_poisson>0
replace new_wells_poisson=new_wells_poisson*new_wells_exp_alpha if mean_new_wells_poisson>0
rename new_wells_poisson new_wells_pred
drop new_wells_poisson_fitted mean_new_wells mean_new_wells_poisson

* Save coefficients
gen brent_hat=_b[brent]

forval fc=1/20 {
	replace brent_hat = brent_hat + _b[`fc'.field_categ#c.brent] if field_categ==`fc'
}

gen capex_hat=_b[capex_imputed]
gen opex_hat=_b[opex_imputed]
gen depl_hat=_b[depl]
rename new_wells_exp_alpha fixed_effect 

*** Save output to Stata folder
outsheet doc_field_code year new_wells new_wells_pred ///
	using $resultsTextDir/new_wells_pred_revised.csv, comma replace
outsheet doc_fieldname doc_field_code brent_hat capex_hat opex_hat depl_hat fixed_effect ///
	if year==2000 using $resultsTextDir/poisson_regression_coefficients_revised.csv, comma replace
* Save output to Google Drive folder
outsheet doc_field_code year new_wells new_wells_pred ///
	using "$resultsGoogleDriveDir/new_wells_pred_revised.csv", comma replace
outsheet doc_fieldname doc_field_code brent_hat capex_hat opex_hat depl_hat fixed_effect ///
	if year==2000 using "$resultsGoogleDriveDir/poisson_regression_coefficients_revised.csv", comma replace

	
* Plot State-level prediction
preserve
collapse (sum) new_wells new_wells_pred (mean) brent capex_imputed opex_imputed depl, by(year)
tw(line new_wells year)(line new_wells_pred year, lcolor(red)) ///
	, name(predState, replace) ///
	ti(California) legend(off)
*graph export $docDir/predictions/final_revised/state.pdf, replace
/*
tw (sc new_wells_pred new_wells) (line new_wells new_wells) ///
	, name(state_sc, replace) ti(State-level prediction) 
graph export $docDir/predictions/final_revised/state_sc.pdf, replace
*/
*tw(line brent year)(line `costVar' year, yaxis(2)), name(drivers, replace)
restore

local graphallname "predState"

* Plot Field-level predictions
local fieldlab: value label topfield
forval tf=0/10 {
	local fieldname: label `fieldlab' `tf'
	di "`fieldname'"

	preserve
	keep if topfield==`tf'
	collapse (sum) new_wells new_wells_pred, by(year)

	tw(line new_wells year)(line new_wells_pred year, lcolor(red)) ///
		, name(pred`tf', replace) ///
		ti(`fieldname') legend(off)
	*graph export $docDir/predictions/final_revised/field`tf'.pdf, replace
	/*
	tw (sc new_wells_pred new_wells) (line new_wells new_wells) ///
		, name(field`tf'_sc, replace) ///
		ti(Topfield-level prediction: `fieldname') 
	graph export $docDir/predictions/final_revised/field`tf'_sc.pdf, replace
	*/
	restore
	local graphallname "`graphallname' pred`tf'"
}

graph combine `graphallname'
graph export $docDir/predictions/final_revised/pred_full.pdf, replace

********** Build model using partial historical series (some later years held out)
use $dataDir/entry_revised, replace // from entry.do
poisson new_wells i.topfield##c.brent capex_imputed opex_imputed depl if year<2010, robust
predict new_wells_pred

* Plot State-level prediction
preserve
collapse (sum) new_wells new_wells_pred (mean) brent capex_imputed opex_imputed depl, by(year)
tw(line new_wells year)(line new_wells_pred year, lcolor(red)) ///
	, name(predState, replace) ti(California) ///
	tline(2010, lp(dash) lc(black)) legend(off)
*graph export $docDir/predictions/final_revised/state_2010.pdf, replace
/*
tw (sc new_wells_pred new_wells) (line new_wells new_wells) ///
	, name(state_sc, replace) ti(State-level prediction)
graph export $docDir/predictions/final_revised/state_sc_2010.pdf, replace
*/
restore

local graphallname "predState"

* Plot Field-level predictions
local fieldlab: value label topfield
forval tf=0/10 {
	local fieldname: label `fieldlab' `tf'
	di "`fieldname'"

	preserve
	keep if topfield==`tf'
	collapse (sum) new_wells new_wells_pred, by(year)

	tw(line new_wells year)(line new_wells_pred year, lcolor(red)) ///
		, name(pred`tf', replace) ///
		ti(`fieldname') ///
		tline(2010, lp(dash) lc(black)) legend(off)
	*graph export $docDir/predictions/final_revised/field`tf'_2010.pdf, replace
	/*
	tw (sc new_wells_pred new_wells) (line new_wells new_wells) ///
		, name(field`tf'_sc, replace) ///
		ti(Topfield-level prediction: `fieldname') 
	graph export $docDir/predictions/final_revised/field`tf'_sc_2010.pdf, replace
	*/
	restore
	local graphallname "`graphallname' pred`tf'"
}

graph combine `graphallname'
graph export $docDir/predictions/final_revised/pred_2010.pdf, replace


