## Tracey Mangin
## August 25, 2021
## annual production and GHG emissions for extraction segement

## libraries
library(tidyverse)
library(data.table)

## paths
data_directory  = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/misc/'


## read in data
hist_ghg <- fread(paste0(data_directory, 'historic_ghg_emissions_og_ng_adjusted.csv'))
hist_prod <- fread(paste0(data_directory, 'well_prod_m_processed.csv'))

## annual production
hist_prod <- hist_prod[, .(bbls = sum(OilorCondensateProduced, na.rm = T)), by = .(year)]

hist_vals <- merge(hist_prod, hist_ghg[emissions_type == "mtco2e", .(year, mtco2e)],
                   by = "year")

setnames(hist_vals, "mtco2e", "Mtco2e")

fwrite(hist_vals, paste0(save_dir, 'hist_extraction_prod_emis.csv'), row.names = F)
