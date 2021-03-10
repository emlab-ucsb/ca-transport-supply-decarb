/* Calepa Carbon Neutrality

Estimate entry model parameters

Ruiwen Lee
Created:   7 Aug 2020
Modified: 12 Sep 2020

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

cd $workDir
*log using $logDir/entry.log, replace

********** Start **********

// Field-asset matching uses wells then nearest asset: entry_df.csv
// Field-asset matching uses wells then asset of nearest neighbor fields: entry_df_v2
insheet using $dataRawDir/entry_df_v2_10132020_v3.csv, comma names clear

*drop if strpos(doc_fieldname,"Gas")>0
drop if year==1977

* Add new capex variable called wellcost_imputed
merge 1:1 doc_field_code year using $dataDir/wellcost_imputed
drop _merge

***** Prepare variables *****

* Outcome variables: new_prod new_wells doc_prod
rename n_new_wells new_wells

* Cost variables
destring capex_per_bbl_nom opex_per_bbl_nom, force replace
destring capex_imputed opex_imputed wm_capex_imputed wm_opex_imputed, force replace

//gen totex_per_bbl_nom = capex_per_bbl_nom + opex_per_bbl_nom
gen totex_capex = capex_imputed + opex_imputed
*gen wm_totex_capex = wm_capex_imputed + wm_opex_imputed
*gen totex_wellcost = wellcost_imputed + opex_imputed

* Field depletion variables
rename m_cumsum_div_my_prod depl
rename wm_cumsum_div_my_prod wm_depl
rename m_cumsum_div_max_res cumsum_div_max_res

* API variable
destring wm_api_gravity, force replace

***** Define top fields and low-activity fields *****
gen topfield=0 
local topfieldcount=1
local topfieldnames `" "Belridge  South" "Midway-Sunset" "Kern River" "Cymric" "Wilmington" "Lost Hills" "San Ardo" "Elk Hills" "Coalinga" "Poso Creek" "'
foreach f of local topfieldnames {
	di "`f'"
   replace topfield = `topfieldcount' if doc_fieldname=="`f'"
   local topfieldcount=`topfieldcount'+1

}
*label define topfieldlabel 0 "Non-top fields" 1 "Belridge South" 2 "Midway-Sunset" 3 "Kern River" 4 "Cymric" 5 "Wilmington" 6 "Lost Hills" 7 "San Ardo" 8 "Elk Hills" 9 "Coalinga" 10 "Poso Creek"
label values topfield topfieldlabel

save $dataDir/entry_10132020_v3, replace
