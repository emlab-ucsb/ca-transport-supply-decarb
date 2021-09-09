global outputFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\academic_output"
global tempFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\temp"



*X-WALK
*EXTRACTION


import delimited using $outputFiles/09_07_2021/subset_field_results.csv, clear

merge m:1 doc_field_code using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_field_clusters_xxwalk

keep if _merge==3

collapse (sum) total_prod_bbl, by(id year scen_id oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario setback_scenario prod_quota_scenario excise_tax_scenario)
egen scenid2=group(oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario setback_scenario prod_quota_scenario excise_tax_scenario)

su scenid2 if scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"
/*

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
     scenid2 |      7,074         101           0        101        101

*/
*SCENID2 101 IS THE BAU SCENARIO
tostring scenid2, gen(scen_id2)
drop scen_id

gen scen_id="E-"+scen_id2
/*
extraction	pm25	0.00165
extraction	nox	0.04611
extraction	sox	0.01344
extraction	voc	0.02614
extraction	nh3	0.00061

*/
gen nh3=total_prod_bbl*0.0061
gen nox=total_prod_bbl*0.04611
gen pm25=total_prod_bbl*0.00165
gen sox=total_prod_bbl*0.01344
gen voc=total_prod_bbl*0.02614



saveold $tempFiles/ext_intermediate, replace

forvalues x=1/109{

use $tempFiles/ext_intermediate, clear

keep if scen_id==`"E-`x'"'

forvalues y=2019(1)2045{
	preserve
		keep if year==`y'
		merge 1:m id using  "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_extraction_all_pollutants"
		keep if _merge==3
		local poll "nh3 nox pm25 sox voc"
		foreach p in `poll'{
			gen tot_`p'=weighted_totalpm25`p'*`p'
		}
		
		gen total_pm25=tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc
		
		rename tot_pm25 prim_pm25
		
		collapse (mean) total_pm25 prim_pm25, by(GEOID year)
		
		saveold $tempFiles/extraction_`y', replace
	restore
	}

	use $tempFiles/extraction_2019, replace
		forvalues y=2020/2045{
			append using $tempFiles/extraction_`y'
		}
		
export delimited $outputFiles/E-`x'.csv, replace
}
	
*Deltas	
forvalues y=1(1)109{
	import delimited using $outputFiles/E-`y'.csv, clear
		saveold $outputFiles/E-`y', replace

}	
	
use $outputFiles/E-101, clear
rename total_pm25 BAUtotal_pm25
rename prim_pm25 BAUprim_pm25
forvalues y=1(1)100{
preserve
	merge 1:1 geoid year using $outputFiles/E-`y'
	gen delta_totalpm25=total_pm25-BAUtotal_pm25
	gen delta_primpm25=prim_pm25-BAUprim_pm25
		export delimited using $outputFiles/deltas_extraction/scenario_E-`y'.csv, replace
restore	
}		
*REFINING

forvalues x=909(1)2592{
import delimited using $outputFiles/09_07_2021/site_refining_outputs.csv, clear
replace scen_id="R-0" if scen_id=="R-BAU"
keep if scen_id==`"R-`x'"'

replace site_id="800" if site_id=="t-800"
replace site_id="34222" if site_id=="342-2"

/*
refining	pm25	0.00402
refining	nox	0.01495
refining	sox	0.00851
refining	voc	0.01247
refining	nh3	0.00056
*/

rename value total_prod_bbl
gen nh3=total_prod_bbl*0.00056
gen nox=total_prod_bbl*0.01495
gen pm25=total_prod_bbl*0.00402
gen sox=total_prod_bbl*0.00851
gen voc=total_prod_bbl*0.01247


forvalues y=2019(1)2045{
	preserve
		keep if year==`y'
		rename site_id id
		merge 1:m id using  "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_refining_all_pollutants"
		keep if _merge==3
		local poll "nh3 nox pm25 sox voc"
		foreach p in `poll'{
			gen tot_`p'=weighted_totalpm25`p'*`p'
		}
		gen total_pm25=tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc

		rename tot_pm25 prim_pm25
		
		collapse (mean) total_pm25 prim_pm25, by(GEOID year)

		
		saveold $tempFiles/refining_`y', replace
	restore
	}

	use $tempFiles/refining_2019, replace
		forvalues y=2020/2045{
			append using $tempFiles/refining_`y'
		}
		
	export delimited  $outputFiles/R-`x'.csv, replace	
}


*Deltas
forvalues y=1(1)420{
	import delimited using $outputFiles/R-`y'.csv, clear
		saveold $outputFiles/R-`y', replace

}

use $outputFiles/R-0, clear
rename total_pm25 BAUtotal_pm25
rename prim_pm25 BAUprim_pm25
forvalues y=1(1)420{
preserve
	merge 1:1 geoid year using $outputFiles/R-`y'
	gen delta_totalpm25=total_pm25-BAUtotal_pm25
	gen delta_primpm25=prim_pm25-BAUprim_pm25
		export delimited using $outputFiles/deltas_refining/scenario_R-`y'.csv, replace
restore	
}	