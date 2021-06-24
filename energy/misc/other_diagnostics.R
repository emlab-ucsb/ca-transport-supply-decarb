## Tracey Mangin
## June 24, 2021
## random diagnostics 

library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(cowplot)

## paths
proj_dir          = "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'


## files
ghg_file        = 'ghg_emissions_x_field_2015_revised.csv'
entry_file      = 'stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
prod_file       = "well_prod_m_processed.csv"

## read in info
ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]


## for opex and capex
entry_dt = fread(file.path(outputs_path, entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))

## 2019 well prod
prod_2019 <- well_prod[, .(prod = sum(OilorCondensateProduced, na.rm = T)), by = .(year, doc_field_code)]
prod_2019 <- prod_2019[year == 2019]

## opex x emissions factor by field



## new well entry by field, by 2045