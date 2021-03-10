/* Calepa Carbon Neutrality

Predict entry

Ruiwen Lee
Created:   5 Sep 2020
Modified: 13 Oct 2020

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

global workDir "//Users/rui/Dropbox/Research/CarbonNeutrality/STATA/"

global codeDir "codeSTATA"
global dataDir "dataSTATA"
global dataRawDir "dataRAW"
global tempDir	"temp"
global resultsDir "resultsSTATA"
global resultsTextDir "resultsText"
global logDir   "logs"
global docDir	 "docs"
global texDir	 "tex"

cd $workDir
*log using $logDir/predict.log, replace

********** Start **********

use $dataDir/entry_10132020_v3, replace // from entry.do

*** Field level model predictions for top10 fields ****************************

****** With topfield FEs ******
* Jointly estimate all coefficients except for Brent;
* Brent coefficient topfield-specific

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


outsheet doc_field_code year new_wells new_wells_pred ///
	using $resultsTextDir/new_wells_pred_10132020_v3.csv, comma replace
outsheet doc_fieldname doc_field_code brent_hat capex_hat opex_hat depl_hat cons_hat ///
	if year==2000 using $resultsTextDir/poisson_regression_coefficients_10132020_v3.csv, comma replace
	
/****** With field FEs ***** Not used eventually
xtset doc_field_code year
xtpoisson new_wells i.topfield##c.brent capex_imputed opex_imputed depl, fe robust
predict new_wells_poisson_fitted, xb
gen double new_wells_poisson = exp(new_wells_poisson_fitted)
egen mean_new_wells=mean(new_wells), by(doc_field_code)
egen mean_new_wells_poisson=mean(new_wells_poisson), by(doc_field_code)
gen double new_wells_exp_alpha=mean_new_wells/mean_new_wells_poisson if mean_new_wells_poisson>0
replace new_wells_poisson=new_wells_poisson*new_wells_exp_alpha if mean_new_wells_poisson>0
rename new_wells_poisson new_wells_pred

* Save coefficients
gen brent_hat=_b[brent]

forval tf=1/10 {
	replace brent_hat = brent_hat + _b[`tf'.topfield#c.brent] if topfield==`tf'
}

gen capex_hat=_b[capex_imputed]
gen opex_hat=_b[opex_imputed]
gen depl_hat=_b[depl]
rename new_wells_exp_alpha cons_hat 

outsheet doc_fieldname doc_field_code brent_hat capex_hat opex_hat depl_hat cons_hat ///
	if year==2000 using $resultsTextDir/poisson_regression_coefficients_11062020.csv, comma replace
*/


/****** With topfield FEs, hold out data from 2010-2019 ******
poisson new_wells i.topfield##c.brent capex_imputed opex_imputed depl if year<2010, robust
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


outsheet doc_field_code year new_wells new_wells_pred ///
	using $resultsTextDir/new_wells_pred_10132020_v3_oos.csv, comma replace
outsheet doc_fieldname doc_field_code brent_hat capex_hat opex_hat depl_hat cons_hat ///
	if year==2000 using $resultsTextDir/poisson_regression_coefficients_10132020_v3_oos.csv, comma replace
