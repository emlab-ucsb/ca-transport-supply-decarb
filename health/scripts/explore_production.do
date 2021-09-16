global outputFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\academic_output"
global tempFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\temp"

import delimited using $outputFiles/09_07_2021/subset_field_results.csv, clear

merge m:1 doc_field_code using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_field_clusters_xxwalk

keep if _merge==3



egen scenid2=group(oil_price_scenario innovation_scenario carbon_price_scenario ccs_scenario setback_scenario prod_quota_scenario excise_tax_scenario)

su scenid2 if scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"


*SCENID2 101 IS THE BAU SCENARIO
tostring scenid2, gen(scen_id2)
drop scen_id

gen scen_id="E-"+scen_id2


preserve
	keep if scen_id=="E-101"
	keep doc_field_code year total_prod_bbl id
	rename total_prod_bbl bau_total_prod_bbl
	saveold $tempFiles/extraction_bau, replace
	
restore

drop _merge
merge m:1 doc_field_code id year using $tempFiles/extraction_bau

preserve
collapse (sum) total_prod_bbl bau_total_prod_bbl, by(id year scen_id)

gen delta_production=total_prod_bbl-bau_total_prod_bbl
	gen scn_no=substr(scen_id,3,.)
	destring scn_no, replace
		graph bar (mean) delta_production, over(scn_no)

restore

preserve
gen delta_production=total_prod_bbl-bau_total_prod_bbl
	gen scn_no=substr(scen_id,3,.)
	destring scn_no, replace
		graph bar (mean) delta_production, over(scn_no)
restore