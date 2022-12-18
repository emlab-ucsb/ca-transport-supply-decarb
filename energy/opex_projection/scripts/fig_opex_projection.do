/*******************************************************
Code shows different model fits for opex projection

Author: Kyle Meng
********************************************************/

/***************************   HEADER **********************************/
	clear all
	set more off
	set trace off
	set tracedepth 2
	set matsize 11000 
	set maxvar 32000
	set scheme plotplain 
	
	//set local directory
	if regexm("`c(pwd)'","/Users/kylemeng")==1  { //Kyle's machine
		global startDir "/Users/kylemeng/Dropbox/work/research/CA_oil_equity/repo/energy/opex_projection"
		sysdir set PLUS "$startDir/scripts/STATA_toolbox"
	}
	cd $startDir
	if "$startDir"=="" exit 
	disp "local path is $startDir"

	//set subfolders
	global rawData "$startDir/rawData"
	global processedData "$startDir/processedData"
	global tempDir "$startDir/temp"
	global tablesDir "$startDir/tables"
	global figuresDir "$startDir/figures"

	//Parameters


/******************************* ANALYSIS *************************************************/

//Load data 
	insheet using $rawData/hist_capex_opex_vals.csv, comma names clear

//Clean data	
	drop indicator
	keep if indicator_name=="OpEx"

	collapse (mean) cost, by(year)
	tsset year
	drop if year==1977

	//expand year to 2045
	local n=_N+1
	set obs `n'
	replace year=2045 if missing(year)	
	tsfill

	gen year2=year^2

//Estimation

	//linear full sample
	reg cost year
	predict cost_h_l_fs

	//quad half sample
	reg cost year year2 if year>=2000
	predict cost_h_q_hs	
	replace cost_h_q_hs=. if cost_h_q_hs<0
	replace cost_h_q_hs=. if cost_h_q_hs>50

//Figure
	tw(line cost year, lc(black) lw(.55)) ///
	(line cost_h_l_fs year, lw(.25) lp(solid) lc(orange_red)) ///
	(line cost_h_q_hs year if year>=2000, lw(.25) lp(dash) lc(gs10)) ///	
	, ///
	ytitle("OpEx cost (USD per barrel)") ///
	ylabel(0(5)35, nogrid) ///
	yscale(range(0 30)) ///
	xtitle("Year") ///
	xlabel(1980(10)2040, nogrid) ///
	legend( order( ///
	1 "Observed" ///
	2 "Prediction Model (1): linear time trend, 1978-2019 estimating sample" ///
	3 "Prediction Model (2): quadratic time trend, 2000-2019 estimating sample" ///
	) ///
	col(1) ring(0) position(10) size(vsmall) ///
	region(style(none)) bmargin(vsmall)) 


	graph export $figuresDir/fig_opex_prediction.jpg, replace




