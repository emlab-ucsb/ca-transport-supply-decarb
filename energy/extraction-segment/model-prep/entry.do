/* Calepa Carbon Neutrality

Set up data to estimate entry model parameters

Ruiwen Lee
Created:   7 Aug 2020
Modified: 12 Sep 2020
Modified (for paper): 8 Jun 2021

*/

***** Initialization *****
clear all
capture log close
set more off

set linesize 90
set matsize 800

global workDir "/Users/rui/Dropbox/ca-transport-supply-decarb/STATA/" // RL's macbook
//global workDir "/Users/emlab/Dropbox/Research/CarbonNeutrality/STATA/" // emLab macbook
global googleDriveDir "/Volumes/GoogleDrive-107971834908030063679/Shared drives/emlab/projects/current-projects/calepa-cn" // on RL's macbook


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
//insheet using $dataRawDir/entry_df_final_revised.csv, comma names clear
insheet using "$googleDriveDir/outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv", comma names clear

*drop if strpos(doc_fieldname,"Gas")>0
drop if year==1977

***** Prepare variables *****

* Outcome variables: new_prod new_wells doc_prod
rename n_new_wells new_wells

* Cost variables
destring capex_per_bbl_nom opex_per_bbl_nom, force replace
destring capex_imputed opex_imputed, force replace

//gen totex_per_bbl_nom = capex_per_bbl_nom + opex_per_bbl_nom
//gen totex_imputed = capex_imputed + opex_imputed

* Field depletion variables
rename m_cumsum_div_my_prod depl
//rename m_cumsum_div_max_res cumsum_div_max_res

* Top 10 producing fields in 2019
rename top_field topfield


capture label define topfieldlabel 0 "Non-top fields" 1 "Belridge  South" 2 "Midway-Sunset" 3 "Kern River" 4 "Cymric" 5 "Wilmington" 6 "Lost Hills" 7 "San Ardo" 8 "Elk Hills" 9 "Coalinga" 10 "Poso Creek"

label values topfield topfieldlabel

* Rank all fields by 2019 production
egen rank = rank(doc_prod) if year==2019, unique
bysort doc_field_code: egen field_rank=max(rank)

* Create new categories for low-producing fields
// by doc_prod or field_rank
xtile nontop_field_categ = field_rank if topfield==0 & year==2019, nq(10) // 1: lowest quantile
replace nontop_field_categ = -nontop_field_categ + 10 + 10 + 1

bysort doc_field_code: egen field_categ=max(nontop_field_categ)
replace field_categ=topfield if topfield>0
sum field_categ

do $codeDir/label.do
save $dataDir/entry_revised, replace

***** Convert vars to real price (for revisions to paper 11/29/2022) ***** 
* real prices weren't eventually used but these changes to entry.do were updated anyway

// Import CPI series
import excel "$googleDriveDir/data/stocks-flows/raw/BLS-CPI-U.xlsx", sheet("Annual") firstrow case(lower) clear
*save $tempDir/cpi, replace

*use $tempDir/cpi, replace
merge 1:n year using $dataDir/entry_revised
drop if _merge!=3
drop _merge
gen brent_2019 = brent/cpi*cpi2019
gen capex_imputed_2019 = capex_imputed/cpi*cpi2019
gen opex_imputed_2019 = opex_imputed/cpi*cpi2019

save $dataDir/entry_revised_real

/***** Plots to look at data *****
use $dataDir/entry, replace

*** Histograms
hist new_wells, discrete frac
graph export $docDir/histograms/new_well.pdf, replace
hist new_prod, frac
graph export $docDir/histograms/new_prod.pdf, replace
hist opex_imputed, frac
graph export $docDir/histograms/opex_imputed.pdf, replace
hist capex_imputed, frac
graph export $docDir/histograms/capex_imputed.pdf, replace
hist wellcost_imputed, frac
graph export $docDir/histograms/wellcost_imputed.pdf, replace

*** Time series

sort year
* Year level variables
bysort year: gen yearfirst=_n==1

bysort year: egen new_well_yearsum=sum(new_wells)
bysort year: egen new_prod_yearsum=sum(new_prod)
bysort year: egen doc_prod_yearsum=sum(doc_prod)

bysort year: egen capex_yearmean=mean(capex_imputed)
bysort year: egen opex_yearmean=mean(opex_imputed)
bysort year: egen totex_capex_yearmean=mean(totex_capex)
bysort year: egen wellcost_yearmean=mean(wellcost_imputed)

bysort year: egen wm_capex_yearmean=mean(wm_capex_imputed)
bysort year: egen wm_opex_yearmean=mean(wm_opex_imputed)
bysort year: egen wm_totex_yearmean=mean(wm_totex)

* Outcome variables: New prod and new wells and Brent over time	

twoway (line new_well_yearsum year) ///
	(line brent year, yaxis(2) lc(black) lp(dash)) if yearfirst==1, ///
	title(No. New Wells (sum of fields))
graph export $docDir/timeseries/new_well.pdf, replace

twoway (line new_prod_yearsum year) ///
	(line brent year, yaxis(2) lc(black) lp(dash)) if yearfirst==1, ///
	title(New Production (sum of fields))
graph export $docDir/timeseries/new_prod.pdf, replace


* Cost variables: 
twoway (line wm_opex_yearmean wm_capex_yearmean wm_totex_yearmean year) ///
	(line brent year, yaxis(2) lc(black) lp(dash)) if yearfirst==1, ///
	title(Capex/Opex/Totex per bbl (mean of fields))
graph export $docDir/timeseries/cost_imputed.pdf, replace

twoway (line wm_opex_yearmean wm_capex_yearmean year) ///
	(line brent year, yaxis(2) lc(black) lp(dash)) if yearfirst==1, ///
	title(Capex/Opex/Totex per bbl (mean of fields))

twoway (line wellcost_yearmean year) ///
	(line brent year, yaxis(2) lc(black) lp(dash)) if yearfirst==1, ///
	title(Wellcost per EUR bbl (mean of fields))
	
***** Poisson model *****

*** Without field FEs ***

** Regressions **
eststo clear

foreach capvar in capex { // wellcost
foreach wgt in "" { // "wm_"  fields matched to Rystad variables using simple or weighted mean 
foreach var of varlist new_wells new_prod { //  doc_prod 

	/* CapEx only
	poisson `var' brent `wgt'`capvar'_imputed, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "N" , replace
	eststo `wgt'`capvar'_`var'_cap

	* OpEx only
	poisson `var' brent `wgt'opex_imputed, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "N" , replace
	eststo `wgt'`capvar'_`var'_op
	*/
	
	/* Separate cost vars
	poisson `var' brent `wgt'`capvar'_imputed `wgt'opex_imputed, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "N" , replace
	eststo `wgt'`capvar'_`var'_capop
	*/
	
	* Add depletion, separate cost vars
	poisson `var' brent `wgt'`capvar'_imputed `wgt'opex_imputed `wgt'depl, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd local fieldFE "N" , replace
	estadd scalar N_cluster = e(N_clust)
	eststo `wgt'`capvar'_`var'
	
	/* Add depletion interaction, separate cost vars
	poisson `var' c.brent##c.`wgt'depl c.`wgt'`capvar'_imputed##c.`wgt'depl c.`wgt'opex_imputed##c.`wgt'depl, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd local fieldFE "N" , replace
	estadd scalar N_cluster = e(N_clust)
	eststo `wgt'`capvar'_`var'_depX
*/
	
	/*
	* Sum cost vars
	poisson `var' brent `wgt'totex_`capvar', cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd local fieldFE "N" , replace
	estadd scalar N_cluster = e(N_clust)
	eststo `wgt'`capvar'_`var'_tot
	*/
	
	* Add depletion, sum cost vars
	poisson `var' brent `wgt'totex_`capvar' `wgt'depl, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd local fieldFE "N" , replace
	estadd scalar N_cluster = e(N_clust)
	eststo `wgt'`capvar'_`var'_tot
	
	/* Add depletion interaction, sum cost vars
	poisson `var' c.brent##c.`wgt'depl c.`wgt'totex_`capvar'##c.`wgt'depl, cluster(doc_field_code) nolog
	estat gof
	estadd scalar chi2_d = r(chi2_d) // deviance goodness of fit 
	estadd local fieldFE "N" , replace
	estadd scalar N_cluster = e(N_clust)
	eststo `wgt'`capvar'_`var'_depX_tot
	*/
}
}
}

** Output results **
	
* Rystad variables matching to fields using simple mean of assets
* Capex
foreach var of varlist new_wells new_prod { // doc_prod
	esttab capex_`var'* using $texDir/poisson_`var', b(%6.3f) se(%6.3f) ///
	order(brent capex_imputed opex_imputed totex_capex depl *wm_api_gravity) ///
	keep(brent capex_imputed opex_imputed totex_capex depl *wm_api_gravity) ///
	stats(fieldFE N N_cluster, labels("Field FEs" "No. field-years" "No. fields") ///
		fmt(%5.0gc %9.0gc %5.0gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level are in parentheses.) ///
	nogaps nomtitles noomit tex replace
}

/* Wellcost
foreach var of varlist new_wells new_prod { // doc_prod
	esttab wellcost_`var'* using $texDir/poisson_wellcost_`var', b(%6.3f) se(%6.3f) ///
	order(brent wellcost_imputed opex_imputed totex_wellcost cumsum_div_my_prod *brent#* *wellcost*#* *opex*#* *totex*#* _cons) ///
	keep(brent wellcost_imputed opex_imputed totex_wellcost cumsum_div_my_prod *brent#* *wellcost*#* *opex*#* *totex*#* _cons) ///
	stats(fieldFE N chi2_d, labels("Field FEs" "No. field-years" "Deviance GOF") ///
		fmt(%5.0gc %9.0gc %5.0gc %14.6gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Coefficients reported are unexponentiated. Standard errors clustered at field level are in parentheses.) ///
	nogaps nomtitles noomit tex replace
}

* Rystad variables match to fields using mean weighted by no. of wells in asset
foreach var of varlist new_wells new_prod { // doc_prod
	esttab wm_`var'* using $texDir/poisson_wm_`var', b(%6.3f) se(%6.3f) ///
	order(brent wm_capex_imputed wm_opex_imputed wm_totex wm_cumsum_div_my_prod *brent#* *wm_capex*#* *wm_opex*#* *wm_totex*#* _cons) ///
	keep(brent wm_capex_imputed wm_opex_imputed wm_totex wm_cumsum_div_my_prod *brent#* *wm_capex*#* *wm_opex*#* *wm_totex*#* _cons) ///
	stats(fieldFE N chi2_d, labels("Field FEs" "No. field-years" "Deviance GOF") ///
		fmt(%5.0gc %9.0gc %5.0gc %14.1gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Coefficients reported are unexponentiated. Standard errors clustered at field level are in parentheses.) ///
	nogaps nomtitles noomit tex replace
}
*/

*** With field FEs ***
eststo clear

xtset doc_field_code year

** Regressions **
foreach wgt in "" { // "wm_" 
foreach var of varlist new_wells new_prod { // doc_prod 

	/* CapEx only
	xtpoisson `var' brent `wgt'capex_imputed, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_cap
	
	* OpEx only
	xtpoisson `var' brent `wgt'opex_imputed, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_op

	* Separate cost vars
	xtpoisson `var' brent `wgt'capex_imputed `wgt'opex_imputed, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_capop
	*/
	
	* Add depletion, separate cost vars
	xtpoisson `var' brent `wgt'capex_imputed `wgt'opex_imputed `wgt'depl, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'
	
	
	* Add API, separate cost vars
	xtpoisson `var' brent wm_api_gravity `wgt'capex_imputed `wgt'opex_imputed `wgt'depl , robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_api
	
	* Add API with brent interaction, separate cost vars
	xtpoisson `var' c.brent##c.wm_api_gravity `wgt'capex_imputed `wgt'opex_imputed `wgt'depl, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_apiX
	
	
	/* Add depletion interaction, separate cost vars
	xtpoisson `var' c.brent##c.`wgt'depl c.`wgt'capex_imputed##c.`wgt'depl c.`wgt'opex_imputed##c.`wgt'depl, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_depX
	*/
	
	/* Sum cost vars
	xtpoisson `var' brent `wgt'totex_capex, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_tot
	*/
	
	* Add depletion, sum cost vars
	xtpoisson `var' brent `wgt'totex_capex `wgt'depl, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_tot
	
	/* Add depletion interaction, sum cost vars
	xtpoisson `var' c.brent##c.`wgt'depl c.`wgt'totex_capex##c.`wgt'depl, robust nolog fe
	estadd local fieldFE "Y" , replace
	estadd scalar N_cluster = e(N_g)
	eststo fe`wgt'`var'_depX_tot
	*/
	
	* Add API, separate cost vars
	xtpoisson `var' brent wm_api_gravity `wgt'totex_capex `wgt'depl, robust nolog fe
	estadd scalar N_cluster = e(N_g)
	estadd local fieldFE "Y" , replace
	eststo fe`wgt'`var'_tot_api
	
	* Add API, with Brent interaction
	xtpoisson `var' c.brent##c.wm_api_gravity `wgt'totex_capex `wgt'depl, robust nolog fe
	estadd scalar N_cluster = e(N_g)
	estadd local fieldFE "Y" , replace
	eststo fe`wgt'`var'_tot_apiX
	
}
}

** Output results **

* Rystad variables matching to fields using simple mean of assets
foreach var of varlist new_prod new_wells { // doc_prod
	esttab fe`var'* using $texDir/poisson_fe_`var', b(%6.3f) se(%6.3f) ///
	order(brent capex_imputed opex_imputed totex_capex depl *wm_api_gravity) ///
	keep(brent capex_imputed opex_imputed totex_capex depl *wm_api_gravity) ///
	stats(fieldFE N N_cluster, labels("Field FEs" "No. field-years" "No. fields") ///
		fmt(%5.0gc %9.0gc %5.0gc %14.1gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level are in parentheses.) ///
	noconstant nogaps nomtitles noomit tex replace
}

* Rystad variables match to fields using mean weighted by no. of wells in asset
foreach var of varlist new_prod new_wells doc_prod { 
	esttab fewm_`var'* using $texDir/poisson_fe_wm_`var', b(%6.3f) se(%6.3f) ///
	order(brent wm_capex_imputed wm_opex_imputed wm_totex wm_cumsum_div_my_prod *brent#* *wm_capex*#* *wm_opex*#* *wm_totex*#* _cons) ///
	keep(brent wm_capex_imputed wm_opex_imputed wm_totex wm_cumsum_div_my_prod *brent#* *wm_capex*#* *wm_opex*#* *wm_totex*#* _cons) ///
	stats(fieldFE N N_cluster, labels("Field FEs" "No. field-years" "No. fields") ///
		fmt(%5.0gc %9.0gc %5.0gc %14.1gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Coefficients reported are unexponentiated. Standard errors clustered at field level are in parentheses.) ///
	nogaps nomtitles noomit tex replace
}

//	estadd scalar r2_p = e(r2_p) // Pseudo R2 only available for non-FE poisson and when SEs not clustered

/*** Linear model ***
eststo clear

** Without FEs
* Without year trend
reg new_prod brent capex_bbl_rp opex_bbl_rpbrent capex_bbl_rp opex_bbl_rp
reg new_wells brent capex_bbl_rp opex_bbl_rp, cluster(doc_field_code)
reg doc_prod brent capex_bbl_rp opex_bbl_rp, cluster(doc_field_code)


* Only Opex
reg new_prod brent opex_bbl_rp, cluster(doc_field_code)
reg doc_prod brent opex_bbl_rp, cluster(doc_field_code)


reg new_prod brent capex_bbl_rp opex_bbl_rp year, cluster(doc_field_code)

foreach var of varlist new_prod  { // lg_doc_prod d_doc_prod ihs_doc_prod
	
	* Brent without year trend
	areg `var' brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod, absorb(doc_field_code) cluster(doc_field_code)
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "Y" , replace
	estadd local yearTrend "" , replace
	estadd local fieldyearTrend "" , replace
	eststo `var'_noyear

	* Brent with linear year trend
	areg `var' brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year, absorb(doc_field_code) cluster(doc_field_code)
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "Y" , replace
	estadd local yearTrend "Y" , replace
	estadd local fieldyearTrend "" , replace
	eststo `var'_brent

	* Lagged Brent
	areg `var' brent_1 capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year, absorb(doc_field_code) cluster(doc_field_code)
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "Y" , replace
	estadd local yearTrend "Y" , replace
	estadd local fieldyearTrend "" , replace
	eststo `var'_brent1

	* Log(Brent)
	areg `var' lg_brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year, absorb(doc_field_code) cluster(doc_field_code)
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "Y" , replace
	estadd local yearTrend "Y" , replace
	estadd local fieldyearTrend "" , replace
	eststo `var'_lgbrent

	* Brent with field*year FEs
	areg `var' c.year#i.doc_field_code brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year, absorb(doc_field_code) cluster(doc_field_code)
	estadd scalar N_cluster = e(N_clust)
	estadd local fieldFE "Y" , replace
	estadd local yearTrend "Y" , replace
	estadd local fieldyearTrend "Y" , replace
	eststo `var'_fieldyear
}


/*
xtreg new_prod lg_brent capex opex year, fe cluster(doc_field_code)
estadd local fieldFE "Yes" , replace
estadd local yearTrend "Yes" , replace
estadd local fieldyearTrend "No" , replace
eststo lgbrent

xtreg new_prod lg_brent* capex opex year*, fe cluster(doc_field_code)
estadd local fieldFE "Yes" , replace
estadd local fieldyearTrend "No" , replace
eststo lgbrentlag
*/


***** Output results *****

foreach var of varlist lg_doc_prod  { // new_prod d_doc_prod ihs_doc_prod
	esttab `var'* using $docDir/results_`var', b(%6.3f) se(%6.3f) ///
	order(brent brent_1 lg_brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year) ///
	keep(brent brent_1 lg_brent capex_per_bbl_reserves opex_bbl_rp m_cumsum_div_my_prod year) ///
	stats(fieldFE yearTrend fieldyearTrend N N_cluster r2, labels("Field FEs" "Linear year trend" "Field by year trends" "No. field-years" "No. fields" "R\textsuperscript{2}") ///
		fmt(%5.0gc %5.0gc %5.0gc %9.0gc %5.0gc %6.2gc)) ///
	star (* 0.1 ** 0.05 *** 0.01) ///
	label varlabels(`e(labels)') varwidth(40) ///
	note(Standard errors clustered at field level are in parentheses.) ///
	noconstant nogaps tex replace
}

