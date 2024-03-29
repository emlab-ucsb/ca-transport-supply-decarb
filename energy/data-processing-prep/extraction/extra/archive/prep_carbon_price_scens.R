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

## -----------------------------------------------------------------------
## for creating carbon tax pathways for targets/grid search
## -----------------------------------------------------------------------

carbon_px_scens_filt <- carbon_px_scens[year == 2020]

## these values work for main results (setback scens and 90% decrease)
# start_vals <- c(21.1, 24.18, 25.61, 239.80,
#                 28.7, 83.36, 283.25)


start_vals <- c(253.22, seq(213.6, 213.7, 0.01),
                seq(211.1, 211.2, 0.01), seq(176.8, 176.9, 0.01),
                seq(107.25, 107.3, 0.01), 73.01)

## carbon px setback/ 90 perc scens

carbon_px_scens_search <- data.table(year = rep(2020, length(start_vals)),
                                     carbon_price = start_vals,
                                     units = rep('dollars per metric ton CO2'))

carbon_px_scens_search[, carbon_price_scenario := paste0("px", carbon_price)]
setcolorder(carbon_px_scens_search, c("year", "carbon_price_scenario", "carbon_price", "units"))

##
carbon_px_scens_search2 <- expand.grid(year = 2020:2045,
                                       carbon_price_scenario = unique(carbon_px_scens_search[, carbon_price_scenario]),
                                       units = 'dollars per metric ton CO2')

## add years 2021:2045
carbon_px_scens_search <- merge(carbon_px_scens_search2, carbon_px_scens_search,
                                by = c("year", "carbon_price_scenario", "units"),
                                all.x = T)

setorder(carbon_px_scens_search, "carbon_price_scenario", "year")

## create stream of prices
## -------------------------

perc_inc <- 0.07

calculate_val <- function(x0, r, t) {

  xt <- x0 * (1 + r) ^ t

}


carbon_px_scens_search <- carbon_px_scens_search %>%
  mutate(carbon_price_scenario = as.character(carbon_price_scenario),
         units = as.character(units)) %>%
  group_by(carbon_price_scenario) %>%
  mutate(tval = row_number() - 1) %>%
  fill(carbon_price) %>%
  ungroup() %>%
  mutate(carbon_price = ifelse(tval == 0, carbon_price, calculate_val(x0 = carbon_price, r = perc_inc, t = tval))) %>%
  select(year, carbon_price_scenario, carbon_price, units)


carbon_px_scens_search <- rbind(carbon_px_scens_search, carbon_px_scens)


fwrite(carbon_px_scens_search, paste0(scen_path, '/carbon_px_scens_search.csv'))
 
 
## ----------------------------------------------
## final set
## ----------------------------------------------

setback_carbon_out <- fread(paste0(scen_path, "/setback_carbon_values.csv"))

start_vals <- setback_carbon_out[, .(carbon_price_scenario, setback_scenario, target_scen, ccs_scenario)]
start_vals[, carbon_price_scenario := as.numeric(sub("px", "", carbon_price_scenario))]
 
## carbon px setback/ 90 perc scens
carbon_px_scens_search <- copy(start_vals)
carbon_px_scens_search[, year := 2020]
carbon_px_scens_search[, units := 'dollars per metric ton CO2']
setnames(carbon_px_scens_search, 'carbon_price_scenario', 'carbon_price')

carbon_px_scens_search[, carbon_price_scenario := paste0("carbon_", target_scen)]
setcolorder(carbon_px_scens_search, c("year", "ccs_scenario", "setback_scenario", "carbon_price_scenario", "carbon_price", "units"))

carbon_px_scens_search[, target_scen := NULL]

## change carbon price scenario for setback-carbon px scens
carbon_px_scens_search[, carbon_price_scenario := fifelse(carbon_price_scenario == 'carbon_90_perc_reduction' &
                                                            setback_scenario != 'no_setback', 'carbon_sb_90_perc_reduction', carbon_price_scenario)]

## add id
carbon_px_scens_search[, id := paste0(carbon_price_scenario, "-", setback_scenario, '-', ccs_scenario)]

## create annual values
## --------------------------------

carbon_px_scens_search2 <- expand.grid(year = 2020:2045,
                                       id = unique(carbon_px_scens_search[, id]))

## add years 2021:2045
carbon_px_scens_search <- merge(carbon_px_scens_search2, carbon_px_scens_search,
                                by = c("year", "id"),
                                all.x = T)

setorder(carbon_px_scens_search, "id", "year")

## fill
carbon_px_scens_search <- carbon_px_scens_search %>%
  fill(ccs_scenario, carbon_price_scenario, units) %>%
  select(-id)


## create stream of prices
## -------------------------

perc_inc <- 0.07

calculate_val <- function(x0, r, t) {

  xt <- x0 * (1 + r) ^ t

}


carbon_px_scens_search <- carbon_px_scens_search %>%
  mutate(carbon_price_scenario = as.character(carbon_price_scenario),
         units = as.character(units)) %>%
  fill(setback_scenario) %>%
  group_by(ccs_scenario, setback_scenario, carbon_price_scenario) %>%
  mutate(tval = row_number() - 1) %>%
  fill(carbon_price) %>%
  ungroup() %>%
  mutate(carbon_price = ifelse(tval == 0, carbon_price, calculate_val(x0 = carbon_price, r = perc_inc, t = tval))) %>%
  select(year, ccs_scenario, setback_scenario, carbon_price_scenario, carbon_price, units)

## cobmine ccs_scenario with carbon price scenario
carbon_px_scens_search <- carbon_px_scens_search %>%
  mutate(carbon_price_scenario = paste(carbon_price_scenario, setback_scenario, ccs_scenario, sep = "-")) %>%
  select(-ccs_scenario, -setback_scenario)

## bind with other scenarios
carbon_px_scens_search <- rbind(carbon_px_scens_search, carbon_px_scens)

## final set
fwrite(carbon_px_scens_search, paste0(scen_path, '/final_carbon_tax_scenarios.csv'))

# ## now just include "new" scens
# 
# excise_tax_df3 <- excise_tax_df2 %>%
#   filter(excise_tax_scenario %in% c("no tax", paste("tax", setback_tax_out$match_scen, sep = "_")))
# 
# fwrite(excise_tax_df3, paste0(scen_path, 'comparison_excise_tax_scenarios.csv'))









