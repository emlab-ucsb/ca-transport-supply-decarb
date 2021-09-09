global outputFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\academic_output"
global tempFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\temp"




*X-WALKS BETWEEN FIELDS AND CLUSTERS

import delimited D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km.csv, clear
	drop objectid 
	rename output_fid id
	
	saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km, replace
	

import dbase D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_xwalk_id.dbf, clear
	rename id input_fid
	rename dc_fld_ doc_field_code
	
	saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_xwalk, replace
	
	
	use D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km, clear
	
	merge m:1 input_fid using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_xwalk
	
	destring doc_field_code, replace
	
	drop _merge
	
	saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_field_clusters_xxwalk, replace
	
*EXTRACTION
import delimited using $outputFiles/site_extraction_outputs.csv, clear

merge m:1 doc_field_code using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_field_clusters_xxwalk

keep if _merge==3

collapse (sum) total_prod_bbl, by(year id oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario prod_quota_scenario excise_tax_scenario)

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


sort oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario prod_quota_scenario excise_tax_scenario id year

keep if oil_price_scenario=="reference case"
keep if innovation_scenario=="low innovation"
keep if ccs_scenario=="medium CCS cost"
keep if carbon_price=="price floor"
keep if prod_quota_scenario=="no quota"
keep if excise_tax_scenario=="no tax"

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
		
	export delimited $outputFiles/health_extraction_base_scenario.csv, replace
	
*REFINING
import delimited using $outputFiles/site_refining_outputs.csv, clear

keep if scen_id=="R-1"
collapse (sum) value, by(year site_id oil_price_scenario demand_scenario refining_scenario innovation_scenario carbon_price_scenario ccs_scenario)

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

/*
sort oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario prod_quota_scenario excise_tax_scenario id year

keep if oil_price_scenario=="reference case"
keep if innovation_scenario=="low innovation"
keep if ccs_scenario=="medium CCS cost"
keep if carbon_price=="price floor"
keep if prod_quota_scenario=="no quota"
keep if excise_tax_scenario=="no tax"
*/
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
		
	export delimited $outputFiles/health_refining_base_scenario.csv, replace	