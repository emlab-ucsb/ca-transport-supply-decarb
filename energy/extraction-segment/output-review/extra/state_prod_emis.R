## Tracey Mangin
## August 25, 2021
## annual production and GHG emissions for extraction segement

## libraries
library(tidyverse)
library(data.table)

## paths
data_directory  = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/misc/'
main_path       = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"

## read in data
hist_ghg <- fread(paste0(data_directory, 'historic_ghg_emissions_og_ng_adjusted.csv'))

## monthly well production
prod_file  <- "well_prod_m_processed.csv"

well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))


## ghg factors
ghg_file <- 'ghg_emissions_x_field_2018-2045.csv'

ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_hist = ghg_factors[year < 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]


## annual production
hist_prod <- well_prod[, .(bbls = sum(OilorCondensateProduced, na.rm = T)), by = .(year, doc_field_code, doc_fieldname, county_name)]
hist_prod <- hist_prod[year > 2014 & year < 2019]

hist_vals <- merge(hist_prod, ghg_factors_hist[, .(doc_field_code, upstream_kgCO2e_bbl)],
                   by = "doc_field_code",
                   all.x = T)

hist_vals <- hist_vals[!is.na(upstream_kgCO2e_bbl)]

hist_vals[, county := str_remove(county_name, " Offshore")]


## county information
## -------------------------------

hist_vals <- hist_vals[, total_kgCO2e := bbls * upstream_kgCO2e_bbl]

county_hist_vals <- hist_vals[, .(total_bbls = sum(bbls),
                                  total_kgCO2e = sum(total_kgCO2e)), by = .(county, year)]

county_hist_vals[, total_MtCO2e := total_kgCO2e / (1e6 * 1000)]

## save
fwrite(county_hist_vals, paste0(save_dir, 'hist_extraction_prod_emis_x_county.csv'), row.names = F)
