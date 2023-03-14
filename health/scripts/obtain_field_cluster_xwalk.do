
import delimited calepa-cn/data/health/source_receptor_matrix/extraction_fields_clusters_10km.csv, clear
	drop objectid 
	rename output_fid id
	
	saveold calepa-cn/data/health/source_receptor_matrix/extraction_fields_clusters_10km, replace

import dbase calepa-cn/data/health/source_receptor_matrix/extraction_fields_xwalk_id.dbf, clear
	rename id input_fid
	rename dc_fld_ doc_field_code
	
	saveold calepa-cn/data/health/source_receptor_matrix/extraction_fields_xwalk, replace
	
	
	use calepa-cn/data/health/source_receptor_matrix/extraction_fields_clusters_10km, clear
	
	merge m:1 input_fid using calepa-cn/data/health/source_receptor_matrix/extraction_fields_xwalk
	
	destring doc_field_code, replace
	
	drop _merge
	
	saveold calepa-cn/data/health/source_receptor_matrix/extraction_field_clusters_xwalk, replace