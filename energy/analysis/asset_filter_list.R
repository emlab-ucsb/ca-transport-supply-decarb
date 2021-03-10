## Tracey Mangin
## August 26, 2020
## Asset list 

library(tidyverse)
library(lubridate)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"


## 
rystad_econ <- read_csv(paste0(rystad_path, "processed/oil_asset_opex_capex_govtt_clean.csv"))
field_assets <- read_csv(paste0(rystad_path, "raw/asset_latlon.csv"))
rystad_prod <- read_csv(paste0(rystad_path, "processed/ca_oil_production.csv"))


## econ info
## change rystad econ fields
rystad_econ_adj <- rystad_econ %>%
  filter(economics_group %in% c("Capex", "Opex")) %>%
  select(original_asset_name, year, economics_group, usd_nom) %>%
  pivot_wider(names_from = economics_group, values_from = usd_nom) %>%
  rename(capex = Capex, opex = Opex) %>%
  filter(year >= 1977 & year <= 2019)

## which assets do not have economic information?
asset_econ_info <- rystad_econ_adj %>%
  mutate(capex_val = ifelse(is.na(capex) == TRUE, 0, 1),
         opex_val = ifelse(is.na(opex) == TRUE, 0, 1)) %>%
  group_by(original_asset_name) %>%
  summarise(capex_n = sum(capex_val, na.rm = T),
            opex_n = sum(opex_val, na.rm = T)) %>%
  ungroup()

## add production

rystad_info <- rystad_prod %>%
  group_by(original_asset_name) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  left_join(asset_econ_info)


## filter bbls  == 0
asset_zero_bbl <- rystad_info %>%
  filter(bbls == 0)

asset_zero_econ <- rystad_info %>%
  filter(bbls > 0,
         opex_n == 0 | capex_n == 0)

## which assets do not have production 1977 and on? (ie in our time horizon)
rystad_prod_th <- rystad_prod %>%
  filter(year >= 1977 & year < 2020) %>%
  group_by(original_asset_name) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  filter(bbls > 0)

## save this to investigate which fields get these assets with production but no capex or opex info
write.csv(asset_zero_econ, paste0(rystad_path, "processed/asset_zero_econ.csv"))

## save this to investigate which fields get these assets with production but no capex or opex info
write.csv(asset_zero_bbl, paste0(rystad_path, "processed/asset_zero_bbl.csv"))

## filter field asset to remove asset zero bbl

field_assets_adj <- field_assets %>%
  filter(Asset %in% rystad_prod_th$original_asset_name,
         !Asset %in% asset_zero_bbl$original_asset_name)

write.csv(field_assets_adj, paste0(rystad_path, "processed/asset_latlon_adj.csv"))

