/* Calepa Carbon Neutrality

Well exit model

Ruiwen Lee
Created:   8 Jun 2021
Modified:  6 Jul 2021

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

//global workDir "/Dropbox/Research/CarbonNeutrality/STATA/" // RL's macbook
global workDir "/Users/emlab/Dropbox/Research/CarbonNeutrality/STATA/" // emLab macbook

global codeDir  "codeSTATA"
global dataDir  "dataSTATA"
global dataRawDir "dataRAW"
global tempDir	"temp"
global resultsDir "resultsSTATA"
global resultsTextDir "resultsText"
global logDir     "logs"
global docDir	  "docs"
global texDir	  "tex"

global exitGoogleDriveDir "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit"

cd $workDir
*log using $logDir/exit.log, replace

********** Start **********
set maxvar 15000

* No. of exits from each field in each year under threshold rule
insheet using "$exitGoogleDriveDir/well_exits_under_rule.csv", comma names clear
rename n_exits_field field_well_exits_rule
rename exit_year year
drop if year==1977
drop if year>2010

destring field_well_exits_rule, force replace
//drop if field_well_exits_rule==. 

save $dataDir/exit_rule, replace

* No. of exits using idle years as definition
insheet using "$exitGoogleDriveDir/well_exits.csv", comma names clear

***** Prepare variables *****
//drop if start_year==1977

rename exit_year year

gen well_age = year-start_year
drop if well_age<0
label var well_age "Well Age"

* Merge with entry df to obtain predictor variables
merge m:1 doc_field_code year using $dataDir/entry_revised
// check why some didn't merge
// _merge==1: fields that are not in entry df; for exit_scen==10y, fields 0 154
drop if _merge==1
// _merge==2: fields that don't have wells that exit; for exit_scen==10y, fields 100 322
replace well_exits=0 if _merge==2

levelsof exit_scen, local(levels) 
foreach wellset of local levels {

	expand 2 if _merge==2 & exit_scen==""
	bysort doc_field_code year exit_scen: replace exit_scen="`wellset'" if _merge==2 & exit_scen=="" & _n==1
}
drop if exit_scen==""
drop _merge

* Create panel id
egen field_vintage = group(doc_field_code start_year)

save $dataDir/exit, replace

***** Field-vintage level model *****
use $dataDir/exit, replace
drop if start_year==. // fields that don't have wells that exit; for exit_scen==10y, fields 100 322

/* did not converge
poisson well_exits i.doc_field_code i.field_categ##c.brent opex_imputed depl well_age if exit_scen=="plugged_wells", cluster(doc_field_code) //nolog
*/

levelsof exit_scen, local(levels) 
foreach wellset of local levels {
	preserve
	keep if exit_scen == "`wellset'"
	
	xtset field_vintage year
	xtpoisson well_exits depl well_age, fe robust //nolog
	eststo v_noecon_`wellset'
	xtpoisson well_exits brent opex_imputed depl well_age, fe robust //nolog
	eststo v_brentavg_`wellset'
	xtpoisson well_exits i.field_categ##c.brent opex_imputed depl well_age, fe robust //nolog
	eststo v_brentflex_`wellset'
	restore	
}

***** Field-level model *****
/* Collapse data to field level
bysort doc_field_code year exit_scen: egen field_well_exits = sum(well_exits)
drop start_year well_exits well_age field_vintage
duplicates drop
save $dataDir/exit_fields, replace
*/
use $dataDir/exit_fields, replace

levelsof exit_scen, local(levels) 
foreach wellset of local levels {
	preserve
	keep if exit_scen == "`wellset'"
	
	xtset doc_field_code year
	xtpoisson field_well_exits depl, fe robust //nolog
	eststo f_noecon_`wellset'
	xtpoisson field_well_exits brent opex_imputed depl, fe robust //nolog
	eststo f_brentavg_`wellset'
	xtpoisson field_well_exits i.field_categ##c.brent opex_imputed depl, fe robust //nolog
	eststo f_brentflex_`wellset'
	restore
}

***** Results *****
quietly {
/*** Field-vintage model
* Including all plugged wells
esttab v_noecon_all_plug v_brentavg_all_plug v_brentflex_all_plug v_noecon_5y_all_plug v_brentavg_5y_all_plug v_brentflex_5y_all_plug v_noecon_10y_all_plug v_brentavg_10y_all_plug v_brentflex_10y_all_plug ///
	using $texDir/exit_field_vintage_all_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl well_age) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field-vintage level.) ///
	nogaps noomit mtitles("Plugged" "Plugged" "Plugged" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 10y Idle}" "\shortstack{Plugged\\+ 10y Idle}" "\shortstack{Plugged\\+ 10y Idle}") tex replace
	
* 5y and 10y idle including plugged wells that were idle for that period
esttab v_noecon_5y v_brentavg_5y v_brentflex_5y v_noecon_10y v_brentavg_10y v_brentflex_10y ///
	using $texDir/exit_field_vintage_incl_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl well_age) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field-vintage level.) ///
	nogaps noomit mtitles("\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}") tex replace
	
* 5y and 10y idle excluding plugged wells that were idle for that period
esttab v_noecon_5y_no_plug v_brentavg_5y_no_plug v_brentflex_5y_no_plug v_noecon_10y_no_plug v_brentavg_10y_no_plug v_brentflex_10y_no_plug ///
	using $texDir/exit_field_vintage_no_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl well_age) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field-vintage level.) ///
	nogaps noomit mtitles("\shortstack{5y Idle \\ no Plugged}" "\shortstack{5y Idle \\ no Plugged}" "\shortstack{5y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}") tex replace
*/

*** Field-level model
* Including all plugged wells
esttab f_noecon_all_plug f_brentavg_all_plug f_brentflex_all_plug f_noecon_5y_all_plug f_brentavg_5y_all_plug f_brentflex_5y_all_plug f_noecon_10y_all_plug f_brentavg_10y_all_plug f_brentflex_10y_all_plug ///
	using $texDir/exit_field_all_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level.) ///
	nogaps noomit mtitles("Plugged" "Plugged" "Plugged" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 5y Idle}" "\shortstack{Plugged\\+ 10y Idle}" "\shortstack{Plugged\\+ 10y Idle}" "\shortstack{Plugged\\+ 10y Idle}") tex replace
	
* 5y and 10y idle including plugged wells that were idle for that period
esttab f_noecon_5y f_brentavg_5y f_brentflex_5y f_noecon_10y f_brentavg_10y f_brentflex_10y ///
	using $texDir/exit_field_incl_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level.) ///
	nogaps noomit mtitles("\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{5y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}" "\shortstack{10y Idle \\ incl. Plugged}") tex replace
	
* 5y and 10y idle excluding plugged wells that were idle for that period
esttab f_noecon_5y_no_plug f_brentavg_5y_no_plug f_brentflex_5y_no_plug f_noecon_10y_no_plug f_brentavg_10y_no_plug f_brentflex_10y_no_plug ///
	using $texDir/exit_field_no_plug, b(%6.3f) not ///
	order(depl well_age opex_imputed brent *field_categ*brent) ///
	keep(brent *field_categ*brent opex_imputed depl) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level.) ///
	nogaps noomit mtitles("\shortstack{5y Idle \\ no Plugged}" "\shortstack{5y Idle \\ no Plugged}" "\shortstack{5y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}" "\shortstack{10y Idle \\ no Plugged}") tex replace
} // end results


***** Prediction *****
* For now, use field-level model, 10 year idle wells incl. plugged wells
use $dataDir/exit_fields, replace
keep if exit_scen=="10y"
drop if year>2010

* Merge with exits under threshold rule
merge 1:1 doc_field_code year using $dataDir/exit_rule
replace field_well_exits_rule=0 if _merge==1 // no entry in exit_rule dataset because no exits in that year
drop if _merge==2 // no entry in idle exit dataset because zero exits under 10y definition
drop _merge

xtset doc_field_code year
xtpoisson field_well_exits i.field_categ##c.brent opex_imputed depl, fe robust //nolog

predict well_exits_poisson_fitted, xb
gen double well_exits_poisson = exp(well_exits_poisson_fitted)
egen mean_well_exits=mean(field_well_exits), by(doc_field_code)
egen mean_well_exits_poisson=mean(well_exits_poisson), by(doc_field_code)
gen double well_exits_exp_alpha=mean_well_exits/mean_well_exits_poisson if mean_well_exits_poisson>0
replace well_exits_poisson=well_exits_poisson*well_exits_exp_alpha if mean_well_exits_poisson>0
rename well_exits_poisson well_exits_pred
drop well_exits_poisson_fitted mean_well_exits mean_well_exits_poisson

capture label drop field_categ_label
label define field_categ_label 1 "Belridge  South" 2 "Midway-Sunset" 3 "Kern River" 4 "Cymric" 5 "Wilmington" 6 "Lost Hills" 7 "San Ardo" 8 "Elk Hills" 9 "Coalinga" 10 "Poso Creek" 11 "Non-top Q10" 12 "Non-top Q9" 13 "Non-top Q8" 14 "Non-top Q7" 15 "Non-top Q6" 16 "Non-top Q5" 17 "Non-top Q4" 18 "Non-top Q3" 19 "Non-top Q2" 20 "Non-top Q1" 
label values field_categ field_categ_label

* Save coefficients
gen brent_hat=_b[brent]

forval fc=1/20 {
	replace brent_hat = brent_hat + _b[`fc'.field_categ#c.brent] if field_categ==`fc'
}

gen opex_hat=_b[opex_imputed]
gen depl_hat=_b[depl]
rename well_exits_exp_alpha fixed_effect 

rename field_well_exits well_exits

*** Save output to Stata folder
outsheet doc_field_code year well_exits well_exits_pred ///
	using $resultsTextDir/well_exits_pred.csv, comma replace
outsheet doc_fieldname doc_field_code brent_hat opex_hat depl_hat fixed_effect ///
	if year==2000 using $resultsTextDir/exit_regression_coefficients.csv, comma replace
* Save output to Google Drive folder
outsheet doc_field_code year well_exits well_exits_pred ///
	using "$exitGoogleDriveDir/well_exits_pred.csv", comma replace
outsheet doc_fieldname doc_field_code brent_hat opex_hat depl_hat fixed_effect ///
	if year==2000 using "$exitGoogleDriveDir/exit_regression_coefficients.csv", comma replace

*** Plots

* Plot State-level prediction
preserve
collapse (sum) field_well_exits well_exits_pred field_well_exits_rule, by(year)
tw(line field_well_exits year)(line well_exits_pred year, lcolor(red)) ///
	if year<=2010, ///
	name(predState, replace) ylabel(#2) xlabel(1980 1990 2000 2010) ///
	ti(California) legend(off)
restore
	
local graphallname "predState"

* Plot Field-level predictions

local fieldlab: value label field_categ
forval fc=1/20 {
	local fieldname: label `fieldlab' `fc'
	di "`fieldname'"

	preserve
	keep if field_categ==`fc'
	collapse (sum) field_well_exits well_exits_pred field_well_exits_rule, by(year)

	tw(line field_well_exits year)(line well_exits_pred year, lcolor(red)) ///
		if year<=2010 ///
		, name(pred`fc', replace) ylabel(#3) ///
		ti(`fieldname') legend(off)
		
	*graph export $docDir/predictions/final_revised/field`tf'.pdf, replace

	restore
	local graphallname "`graphallname' pred`fc'"
}

graph combine `graphallname'

graph export $docDir/predictions/exit/pred_210701.pdf, replace
