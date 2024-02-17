## Tracey Mangin
## October 22, 2021
## add infinity price to ccs
# revised: feb 16 2024 by Haejin 

library(tidyverse)
library(data.table)

# paths -----
scen_path         = '/capstone/freshcair/meds-freshcair-capstone/data/inputs/scenarios'
file_path         = '/capstone/freshcair/meds-freshcair-capstone/data/processed' # added file path, b/c read and store at the same place

## files
ccs_ext_file      = 'ccs_extraction_scenarios.csv'
ccs_ref_file      = 'ccs_refining_scenarios.csv'

## load ccs scenarios
ccs_scens_ext = fread(file.path(scen_path, ccs_ext_file), header = T)

## ccs infinity
ccs_infin <- unique(ccs_scens_ext[, .(year, units)])
ccs_infin[, ccs_scenario := "no ccs"]
ccs_infin[, ccs_price := Inf]

setorder(ccs_infin, "year", "ccs_scenario", "ccs_price", "units")

## bind
ccs_ext_revised <- rbind(ccs_scens_ext, ccs_infin)

fwrite(ccs_ext_revised, file.path(file_path , "ccs_extraction_scenarios_revised.csv")) # revised file path 


## refining - load ccs scenarios
ccs_scens_ref = fread(file.path(scen_path, ccs_ref_file), header = T)


## ccs infinity
ccs_infin_r <- unique(ccs_scens_ref[, .(year, units)])
ccs_infin_r[, ccs_scenario := "no ccs"]
ccs_infin_r[, ccs_price := Inf]

setorder(ccs_infin_r, "year", "ccs_scenario", "ccs_price", "units")

## bind
ccs_ref_revised <- rbind(ccs_scens_ref, ccs_infin_r)

fwrite(ccs_ref_revised, file.path(file_path, "ccs_refining_scenarios_revised.csv")) # revised the file path 



