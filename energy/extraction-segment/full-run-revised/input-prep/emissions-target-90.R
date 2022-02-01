## Tracey Mangin
## January 31, 2022
## 90% emissions reduction target

## libraries
library(tidyverse)
library(data.table)

## paths
proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path <- 'data/stocks-flows/processed/'
scen_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'

## file
ghg_file <- 'indust_emissions_2000-2019.csv'

## 2019 GHG emissions
## --------------------------
hist_ghg <- fread(paste0(proj_dir, data_path, ghg_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])
ghg_target_90 <- 0.1 * ghg_2019

ghg_target_df <- tibble(emission_reduction = "90perc_reduction",
                        ghg_emission_MtCO2e = ghg_target_90)

## save
fwrite(ghg_target_df, paste0(scen_path, 'emission_reduction_90.csv'))


