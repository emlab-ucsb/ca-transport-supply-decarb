
*REFINING + EXTRACTION

local activity "refining extraction"
local polls "nh3 nox voc pm25 sox"
foreach a in `activity'{
foreach p in `polls'{
	cd "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix/inmap_processed_srm/`a'/`p'"
clear all
local files : dir "`filepath'" files "*.csv" // Save name of all files in folder ending with .csv in a local
di `"`files'"' // Display list of files to import data from

tempfile master // Generate temporary save file to store data in
save `master', replace empty

foreach x of local files {
    di "`x'" // Display file name

	* 2A) Import each file
	qui: import delimited "`x'", delimiter(",")  case(preserve) clear varnames(1) // Import csv file

	qui: gen id = subinstr("`x'", ".csv", "", .)	// Generate id variable (same as file name but without .csv)
	* 2B) Append each file to masterfile
	append using `master'
	save `master', replace


	}
gen poll="`p'"
gen site=substr(id,9,.)

drop id 

saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_`p', replace
}


use D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_pm25, clear
	append using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_sox
	append using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_nox
	append using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_voc
	append using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_nh3
	
	
	saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_`a'_all_pollutants_pre, replace

}	
	

use D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_refining_all_pollutants_pre, clear
	gen site2=subinstr(site,"_","",.)
	gen id=substr(site2,5,.)
	keep id GEOID totalpm25_aw poll
	rename totalpm25_aw weighted_totalpm25
	reshape wide weighted_totalpm25, i(GEOID id) j(poll) string
	
	local polls "nh3 nox voc pm25 sox"
	foreach p in `polls'{
		replace weighted_totalpm25`p'=0 if weighted_totalpm25`p'==.
	}
	
	sort id GEOID
	
	saveold "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_refining_all_pollutants", replace
	
	export delimited using "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_refining_all_pollutants.csv", replace
	
use D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_extraction_all_pollutants_pre, clear
	gen site2=subinstr(site,"_","",.)
	gen id=substr(site2,6,.)
	keep id GEOID totalpm25_aw poll
	rename totalpm25_aw weighted_totalpm25
	reshape wide weighted_totalpm25, i(GEOID id) j(poll) string
	
	local polls "nh3 nox voc pm25 sox"
	foreach p in `polls'{
		replace weighted_totalpm25`p'=0 if weighted_totalpm25`p'==.
	}
	
	sort id GEOID
	
	destring id, replace
	
*	merge 1:m id using D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km
	
	saveold "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_extraction_all_pollutants", replace	

	export delimited using "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\inmap_processed_srm/srm_extraction_all_pollutants.csv", replace	


*X-WALKS BETWEEN FIELDS AND CLUSTERS

import delimited D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km.csv, clear
	drop objectid 
	rename output_fid id
	
	saveold D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\extraction_fields_clusters_10km, replace
