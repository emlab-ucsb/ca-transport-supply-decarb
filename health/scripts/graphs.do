global outputFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\academic_output"
global tempFiles "D:\Dropbox\UCSB-PhD\emLab\CALEPA\data\source_receptor_matrix\temp"

forvalues y=2000(1)2592{
	import delimited using $outputFiles/deltas_refining/scenario_R-`y'.csv, clear
		collapse (mean) delta_totalpm25 delta_primpm25
		gen scenario="R-`y'"
		saveold $tempFiles/delta_scen_R`y', replace
}

use $tempFiles/delta_scen_R1, clear
	forvalues y=422(1)780{
			append using $tempFiles/delta_scen_R`y'
	}
	
	graph bar (mean) delta_totalpm25, over(scenario)
	
	
use $tempFiles/delta_scen_R1, clear
	forvalues y=422(1)780{
			append using $tempFiles/delta_scen_R`y'
	}
	forvalues y=2000(1)2045{
			append using $tempFiles/delta_scen_R`y'
	}
	
	
*EXTRACTION
forvalues y=1(1)100{
	import delimited using $outputFiles/deltas_extraction/scenario_E-`y'.csv, clear
		collapse (max) delta_totalpm25 delta_primpm25
		gen scenario="E-`y'"
		saveold $tempFiles/delta_scen_E`y', replace
}	

use $tempFiles/delta_scen_E1, clear
	forvalues y=2(1)100{
			append using $tempFiles/delta_scen_E`y'
	}
	
	forvalues y=102(1)109{
			append using $tempFiles/delta_scen_E`y'
	}
	
	
	gen scn_no=substr(scenario,3,.)
	destring scn_no, replace
		graph bar (mean) delta_totalpm25, over(scn_no)
		
