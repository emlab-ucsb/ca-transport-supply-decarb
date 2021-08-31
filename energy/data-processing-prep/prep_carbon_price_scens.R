## Tracey Mangin
## August 26, 2021
## prep carbon price inputs

## libraries 
library(data.table)
library(tidyverse)

##  paths
scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'

## files
carbon_file       = 'carbon_prices_revised.csv'

## read
carbon_px_scens <- fread(file.path(scen_path, carbon_file))

## filter for:
## price floor, 2020
## central, 2020, 2025, 2030, 2035, 2040, 2045
## ceiling: 2020, 2021

carbon_px_scens_filt <- carbon_px_scens[(carbon_price_scenario == 'price floor' &
                                           year %in% c(2020)) |
                                          (carbon_price_scenario == 'central SCC' &
                                           year %in% c(seq(2020, 2045, by = 5))) |
                                          (carbon_price_scenario == 'price ceiling' &
                                             year %in% c(2020, 2021))]
