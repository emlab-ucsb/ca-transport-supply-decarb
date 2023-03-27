*******************************************************
*Code explores mechanisms
********************************************************

/***************************   HEADER **********************************/
	clear all
	set more off
	set trace off
	set tracedepth 2
	set matsize 11000 
	set maxvar 32000
	set scheme plotplain 
	
	//set local directory (need to change manually)
	global startDir "/Users/kylemeng/Dropbox/work/research/CA_oil_equity/repo/mechanism"

	cd $startDir

	//set subfolders
	sysdir set PLUS "$startDir/scripts/STATA_toolbox"
	global rawData "$startDir/rawData"
	global processedData "$startDir/processedData"
	global tempDir "$startDir/temp"
	global tablesDir "$startDir/tables"
	global figuresDir "$startDir/figures"

	//Parameters
	local setback_distance "setback_2500" 


/******************************* ANALYSIS *************************************************/

//Load field-level data 
	insheet using $rawData/extraction_field_cluster_xwalk.csv, comma names clear
	save $tempDir/temp, replace

	insheet using $rawData/extraction_cluster_affectedpop.csv, comma names clear
	merge id using $tempDir/temp, uniqmaster sort 
	tab _merge
	drop _merge
	drop input_fid
	save $tempDir/temp, replace

	insheet using $rawData/field_characteristics.csv, comma names clear

	merge doc_field_code using $tempDir/temp, uniqusing sort
	tab _merge //1: 1, 3: 786
	drop if _merge!=3 
	drop _merge

	rename area_coverage setback_coverage
	rename upstream_kgco2e_bbl ghg_intensity
	rename mean_prod oil_prod
	gen cost=capex_2020+opex_2020

//Aggregate up to cluster level analysis
	keep if setback_scenario=="`setback_distance'"
	collapse (sum) oil_prod (mean) affected_pop share_dac_weighted ghg_intensity cost setback_coverage, by(id)

	gen l_affected_pop=log(affected_pop)
	gen l_ghg_intensity=log(ghg_intensity)
	drop if ghg_intensity>180
	drop if affected_pop>7000

	//discretize oil production in integer unit of barrels to get frequency weights
	replace oil_prod=oil_prod/1e6 //put into million barrels
	replace oil_prod=ceil(oil_prod)
	replace oil_prod=1 if oil_prod==0


//Appendix figure: Cost vs GHG intensity
	reg ghg_intensity cost 
	local r2: display %4.3f e(r2)
	lincom cost

	local beta: display %4.3f r(estimate)
	local pvalue: display %4.3f r(p)

	tw(lfitci ghg_intensity cost, ///
	text(21 30 "b=`beta', p-value=`pvalue', R2=`r2'") ///
	lwidth(.5) lcolor(gs7) fcolor(gs7) alwidth(0) ///
	), ///
	ytitle("GHG intensity (kg CO2e/bbl)", size(large)) ///
	xtitle("Cost of extraction ($/bbl)", size(large)) ///
	xlabel(20(20)80, nogrid labsize(medium)) ///
	ylabel(,labsize(medium)) ///
	legend(off)

	graph export $figuresDir/cluster_ghg_intensity_cost.jpg, replace


//Main text figure:
//Top panel: total affected population

	keep id affected_pop setback_coverage cost ghg_intensity

	//Nature Energy Source data
	outsheet using $processedData/fig5abc_source_data.csv, comma names replace

	
//Setback	
	tw(lfitci affected_pop setback_coverage, ///
	lwidth(.5) lcolor("74 108 111") fcolor("74 108 111") alwidth(0) ///
	), ///
	xtitle("") ///
	ytitle("Population affected by pollution", size(large)) ///
	xlabel(0(.2)1, nogrid labsize(medium)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/health_affected_setback.gph, replace) ///
	legend(off)


//Excise tax	
	tw(lfitci affected_pop cost, ///
	lwidth(.5) lcolor("255 94 91") fcolor("255 94 91") alwidth(0) ///
	), ///
	xtitle("") ///
	ytitle("") ///
	xlabel(20(20)80, nogrid labsize(medium)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/health_affected_cost.gph, replace) ///
	legend(off)


//Carbon tax	
	tw(lfitci affected_pop ghg_intensity, ///
	lwidth(.5) lcolor("252 185 125") fcolor("252 185 125") alwidth(0) ///
	), ///
	xtitle("") ///
	ytitle("") ///
	xlabel(20(20)80, nogrid labsize(medium)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/health_affected_ghg.gph, replace) ///
	legend(off)

//Bottom panel: employment

	insheet using $rawData/county_characteristics.csv, comma names clear

	rename wm_area_coverage setback_coverage
	rename wm_upstream_kgco2e_bbl ghg_intensity
	rename mean_prod oil_prod
	rename total_emp emp
	gen emp_dac_share=dac_share*emp
	gen cost=wm_capex_2020+wm_opex_2020
	gen l_emp=log(emp)

	keep if setback_scenario=="`setback_distance'"

	//discretize oil production in integer unit of barrels to get frequency weights
	gen oil_prod_w=oil_prod/1e6 //put into million barrels
	replace oil_prod_w=ceil(oil_prod_w)
	replace oil_prod_w=1 if oil_prod_w==0

	gen emp_per_bbl=emp/oil_prod

	keep county emp_per_bbl setback_coverage cost ghg_intensity

	//Nature Energy Source data
	outsheet using $processedData/fig5def_source_data.csv, comma names replace


//Setback
	tw(lfitci emp_per_bbl setback_coverage, ///
	lwidth(.5) lcolor("74 108 111") fcolor("74 108 111") alwidth(0) ///
	), ///
	xtitle("Share of area affected by setback", size(large)) ///
	ytitle("Employment intensity (jobs/mil. bbl)", size(large)) ///
	xlabel(0(.2)1, nogrid labsize(medlarge)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/emp_setback.gph, replace) ///
	legend(off)


//Excise tax	
	tw(lfitci emp_per_bbl cost, ///
	lwidth(.5) lcolor("255 94 91") fcolor("255 94 91") alwidth(0) ///	
	), ///
	ytitle("") ///
	xtitle("Cost of extraction ($/bbl)", size(large)) ///
	xlabel(18(4)38, nogrid labsize(medlarge)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/emp_cost.gph, replace) ///
	legend(off)


//Carbon tax
	tw(lfitci emp_per_bbl ghg_intensity, ///
	lwidth(.5) lcolor("252 185 125") fcolor("252 185 125") alwidth(0) ///
	), ///
	ytitle("") ///
	xtitle("GHG intensity (kg CO2e/bbl)", size(large)) ///
	xlabel(20(20)60, nogrid labsize(medlarge)) ///
	ylabel(,labsize(medium)) ///
	xscale(reverse) ///
	saving($tempDir/emp_ghg.gph, replace) ///
	legend(off)

	graph combine ///
 	$tempDir/health_affected_setback.gph ///
 	$tempDir/health_affected_cost.gph ///
	$tempDir/health_affected_ghg.gph ///
 	$tempDir/emp_setback.gph ///
 	$tempDir/emp_cost.gph ///
	$tempDir/emp_ghg.gph ///
 	, ///
 	row(2) col(3) altshrink

	local titles "a. b. c. d. e. f."
	forval i=1/6{
   gr_edit .plotregion1.graph`i'.title.text.Arrpush "{bf:`:word `i' of `titles''}"
   gr_edit .plotregion1.graph`i'.title.style.editstyle size(large)
   gr_edit .plotregion1.graph`i'.title.DragBy 1.5 -54
	}

	graph export $figuresDir/health_emp_mechanism_`setback_distance'.jpg, replace

	rm $tempDir/health_affected_setback.gph 
	rm $tempDir/health_affected_cost.gph 
	rm $tempDir/health_affected_ghg.gph 
 	rm $tempDir/emp_setback.gph 
 	rm $tempDir/emp_cost.gph 
	rm $tempDir/emp_ghg.gph 
	rm $tempDir/temp.dta


