## Tracey Mangin
## May 28, 2022
## Calcualate production and GHG annual reduction (2019 vs 2045)

## libraries
library(data.table)
library(tidyverse)

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/'

## csv names
levels_name <- 'state_levels_all_oil.csv'

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## filter out carbon + setback
levels_dt <- levels_dt[!policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')]
levels_dt <- levels_dt[, target_label := fifelse(target_label == "no_target", "BAU", target_label)]

## filter for 2019 and 2045, production and ghg, ref case 
levels_dt <- levels_dt[year %in% c(2019, 2045)]
levels_dt <- levels_dt[metric %in% c('total_state_bbl', 'total_state_ghg_MtCO2')]
levels_dt <- levels_dt[oil_price_scenario == 'reference case']
levels_dt[, year := paste0('x', year)]

## select relevant columns
levels_dt <- levels_dt[, .(scen_id, policy_intervention, target_label, metric, year, value)]

## pivot wider
levels_dt <- levels_dt %>%
  pivot_wider(names_from = 'year', values_from = 'value') %>%
  mutate(diff = x2045 - x2019,
         perc_diff = (diff / x2019) * 100)
  
  
  


