/* Calepa Carbon Neutrality

Estimate implied technically recoverable resource in each field

Ruiwen Lee
Created:  10 Sep 2020
Modified: 21 Oct 2020
Modified (for paper): 12 May 2021

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

global workDir "/Dropbox/Research/CarbonNeutrality/STATA/"

global codeDir  "codeSTATA"
global dataDir  "dataSTATA"
global dataRawDir "dataRAW"
global tempDir	"temp"
global resultsDir "resultsSTATA"
global resultsTextDir "resultsText"
global logDir     "logs"
global docDir	  "docs"
global texDir	  "tex"

global resultsGoogleDriveDir "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/entry-model-results"

cd $workDir
*log using $logDir/depl.log, replace

********** Start **********

use $dataDir/entry_revised, replace // from entry.do

*************** Estimate total resource by field *******************************

* Calculate cumulative prod by field
bysort doc_field_code (year): gen cum_doc_prod = sum(doc_prod)
by doc_field_code: egen sum_doc_prod=max(cum_doc_prod)

gen resource=. // estimated total resource
gen cum_prod_1977=.
gen cum_prod_all=.
gen check=.    // flag if resource is larger than latest cumulative prod

gen brent_inverse=1/brent

*** Original method
sum doc_field_code
local f_start=r(min)
local f_end=r(max)

forval f=`f_start'/`f_end' {
	capture {
		reg depl brent_inverse cum_doc_prod if doc_field_code==`f'
		replace resource=1/_b[cum_doc_prod] if doc_field_code==`f'
		replace cum_prod_1977=_b[_cons]*resource if doc_field_code==`f'
		replace cum_prod_all=cum_prod_1977+sum_doc_prod if doc_field_code==`f'
		replace check=resource>cum_prod_all if doc_field_code==`f'
	}
}
count if year==2019 & cum_prod_1977<0
count if year==2019 & check==0
gen resource_left=resource-cum_prod_all
sum resource_left if year==2019 & check==1

keep if year==2019
keep doc_field_code resource depl
rename depl depl2019
//duplicates drop
save $dataDir/forecast_depl_revised, replace
outsheet doc_field_code resource using $resultsTextDir/field_resource_revised.csv, comma replace
outsheet doc_field_code resource using "$resultsGoogleDriveDir/field_resource_revised.csv", comma replace

/*
merge 1:m doc_field_code using $dataDir/forecast_cost
drop _merge
sort doc_field_code year
order doc_field_code year

save $dataDir/forecast_vars, replace
outsheet using $resultsTextDir/forecast_vars.csv, comma replace
*/

* Plot for topfields
use $dataDir/entry, replace
keep doc_field_code doc_fieldname topfield
duplicates drop
merge 1:m doc_field_code using $dataDir/forecast_vars
drop _merge

preserve
collapse (mean) capex_forecast opex_forecast, by(year topfield)
separate capex_forecast, by(topfield)
separate opex_forecast, by(topfield)

sort topfield year  
tw (line capex_forecast?* year) ///
	, legend(size(small))name(capex_forecast, replace)
graph export $docDir/timeseries/topfield_forecast_capex.pdf, replace
tw (line opex_forecast?* year) ///
	, legend(size(small)) name(opex_forecast, replace)
graph export $docDir/timeseries/topfield_forecast_opex.pdf, replace
restore

********** Check estimated total resource against projected total production ***
insheet using $dataRawDir/predicted-production_2020-2100_field.csv, comma names clear
rename fieldcode doc_field_code
drop fieldname
bysort doc_field_code: egen prod_future_sum=sum(production_bbl)
keep if year==2020
keep doc_field_code prod_future_sum
save $tempDir/temp, replace

merge 1:1 doc_field_code using $dataDir/forecast_depl
// _merge==2 : fields that are in entry.dta but not in Meas' predicted prod

gen cum_prod_2019=depl2019*resource
gen total_prod=cum_prod_2019+prod_future_sum
gen d_res_prod=resource-total_prod
sum d_res_prod, det
hist d_res_prod
count if d_res_prod<0 // 82 
// - for most fields, estimated resource > production from existing wells till 2100

********** Check estimated total resource against DOC 2009 report ***
insheet using $dataRawDir/oil_production_reserves_2009.csv, comma names clear
gen doc_field_code_orig=doc_field_code
destring doc_field_code, force replace
destring cumulative_oil_and_condensatembb estimated_oil_reservesmbbl, ///
	force gen(cum_prod reserves_left)
replace cum_prod=0 if cum_prod==.
replace reserves_left=0 if reserves_left==.

drop if _n>=501 // drop county subtotals
drop if doc_field_code==. // fields that don't exist in main dataset, or counties

gen resource_doc=cum_prod+reserves_left

keep doc_field_code doc_fieldname field_name cum_prod reserves_left resource_doc

merge 1:1 doc_field_code using $dataDir/forecast_depl_revised
drop if _merge==1 // fields not in our main dataset
drop _merge

order doc_field_code doc_fieldname resource depl2019 field_name resource_doc
replace resource=resource/1000 // convert from bbl to Mbbl

tw (scatter resource resource_doc, mlab(doc_fieldname)) (lfit resource resource_doc) ///
	(function y=x, range(resource_doc)), ///
	legend(label(1 "Resource") label(2 "Linear fit") label(3 "y=x"))
