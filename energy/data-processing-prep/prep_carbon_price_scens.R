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
## price floor, central price, & price ceiling

carbon_px_scens_filt <- carbon_px_scens[year == 2020]

min_val <- min(carbon_px_scens_filt[, carbon_price])
max_val <- 100

start_vals <- seq(min_val, max_val, by = 0.50)

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
  










