/* 
Oil well entry/exit regression model
*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800


global startDir "/Users/kylemeng/Dropbox/work/research/CA_oil_equity/repo/energy/extraction-segment"
//global workDir "/Users/rui/Dropbox/ca-transport-supply-decarb/STATA/" // RL's macbook

sysdir set PLUS "$startDir/../../scripts/STATA_toolbox"

global dataDir "$startDir/model-prep/stata-inputs"
global resultsDir "$startDir/figs-and-results/si"



********** Start **********

* Refer to predict.doc

***** Entry model *****
eststo clear
use $dataDir/entry_revised_real, replace // from entry.do
label var field_categ "Field Category"
xtset doc_field_code year

* (1) 1978-2019 with a single brent coefficient
xtpoisson new_wells brent capex_imputed opex_imputed depl , fe robust nolog
estadd scalar N_cluster = e(N_g)
eststo entry1

* (2) benchmark model
xtpoisson new_wells i.field_categ##c.brent capex_imputed opex_imputed depl, fe robust nolog
estadd scalar N_cluster = e(N_g)
eststo entry2


***** Exit model *****
use $dataDir/exit_fields_10y, replace
keep if exit_scen=="10y"
drop if year>2010


xtset doc_field_code year

* (1) 1978-2019 with a single brent coefficient
xtpoisson field_well_exits brent opex_imputed depl, fe robust 
estadd scalar N_cluster = e(N_g)
eststo exit1

* (2) benchmark model
xtpoisson field_well_exits i.field_categ##c.brent opex_imputed depl, fe robust 
estadd scalar N_cluster = e(N_g)
eststo exit2

esttab entry* exit* using "$resultsDir/entryexit", ///
	b(%6.3f) p(%6.3f) ///
 	order(opex_imputed capex_imputed depl brent *brent) ///
	label varlabels(`e(labels)') varwidth(40) ///
 	keep(brent capex_imputed opex_imputed depl *brent) ///
	stats(N_cluster N, labels("Number of fields" "Number of observations") ///
		fmt(%5.0g %9.0gc %5.0g)) ///
	substitute(main  ) /// 		
	nogaps nostar nomtitles nonotes noomit tex replace
