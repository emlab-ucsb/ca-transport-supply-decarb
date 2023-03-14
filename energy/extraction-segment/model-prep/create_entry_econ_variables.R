## Tracey Mangin
## August 30, 2020
## create 

library(tidyverse)


## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## read in the data
rystad_econ <- read_csv(paste0(rystad_path, "processed/oil_asset_opex_capex_govtt_clean.csv"))
rystad_prod <- read_csv(paste0(rystad_path, "processed/ca_oil_production.csv"))
rystad_capex_bbl <- read_csv(paste0(rystad_path, "processed/capex_bbl_reserves.csv"))
rystad_opex_bbl_nom_df <- read_csv(paste0(rystad_path, "processed/rystad_opex_bbl_nom_clean.csv"))
rystad_capex_bbl_nom_df <- read_csv(paste0(rystad_path, "processed/rystad_capex_bbl_nom_clean.csv"))


## meas's dfs
asset_sum_my_production <- read_csv(paste0(rystad_path, "processed/asset_sum_my-production.csv"))
asset_max_resources <- read_csv(paste0(rystad_path, "processed/asset_max_resources.csv"))
asset_yr_cumsum_production <- read_csv(paste0(rystad_path, "processed/asset-year_cumulative_sum_production.csv"))
ratio_df <- read_csv(paste0(rystad_path, "processed/asset-year_production_my-production_resources.csv"))


## Rystad production for opex per bbl
rystad_prod_adj <- rystad_prod %>%
  filter(year <= 2019) %>%
  group_by(original_asset_name, year) %>%
  summarise(total_prod_bbls = sum(bbls)) %>%
  ungroup()

## change rystad econ fields
rystad_econ_adj <- rystad_econ %>%
  mutate(adj_location = ifelse(location == "Belridge South (Linn Energy acquisition)", "Belridge South",
                               ifelse(location == "Brea-Olinda (Linn Energy acquisition)", "Brea-Olinda", # RL
                                      ifelse(location == "Caliente", "Caliente Offshore Gas (ABD)", # RL
                                             ifelse(location == "Cymric, McKittrick", "Cymric",
                                                    ifelse(location == "East Coyote", "Coyote East",
                                                           ifelse(location == "Gaviota", "Gaviota Offshore Gas (ABD)", # RL
                                                                  ifelse(location == "Midway-Sunset Diatomite", "Midway-Sunset",
                                                                         ifelse(location == "Naples/Capitan", "Capitan (ABD)", # RL
                                                                                ifelse(location == "North San Ardo", "San Ardo",
                                                                                       ifelse(location == "Rincon Offshore", "Rincon",
                                                                                              ifelse(location == "San Joaquin (Elk Hills)", "Elk Hills", # RL
                                                                                                     ifelse(location == "Santa Maria", "Santa Maria Valley",
                                                                                                            ifelse(location == "South Ellwood West", "Elwood South Offshore", # RL
                                                                                                                   ifelse(location == "Summerland", "Summerland Offshore (ABD)",
                                                                                                                          ifelse(location == "Venice Beach", "Venice Beach (ABD)",
                                                                                                                                 ifelse(location == "Ventura (Yuma Energy acquisition)", "Ventura",
                                                                                                                                        ifelse(location == "West Montalvo Offshore", "Montalvo West", location)))))))))))))))))) %>%
  filter(economics_group %in% c("Capex", "Opex")) %>%
  select(original_asset_name, adj_location, year, economics_group, usd_nom) %>%
  pivot_wider(names_from = economics_group, values_from = usd_nom) %>%
  rename(FieldName = adj_location, capex = Capex, opex = Opex) %>%
  filter(year <= 2019)



## prep capex per bbl reserves
rystad_capex_bbl2 <- rystad_capex_bbl %>%
  select(original_asset_name, year, capex_per_bbl_reserves = usd_per_bbl)

## join with production, calculate capex and opex per 
econ_per_bbl_df <- left_join(rystad_econ_adj, rystad_prod_adj) %>%
  mutate(capex_bbl_rp = capex / total_prod_bbls,
         opex_bbl_rp = opex / total_prod_bbls) %>%
  left_join(rystad_capex_bbl2) %>%
  left_join(rystad_capex_bbl_nom_df) %>%
  left_join(rystad_opex_bbl_nom_df) %>%
  select(original_asset_name, FieldName, year, rystad_prod_bbl = total_prod_bbls, capex, capex_bbl_rp, capex_per_bbl_reserves, capex_per_bbl_nom, opex, opex_bbl_rp, opex_per_bbl_nom)


## add sum my prod, two versions of depletion, max resource
## prep meas's df
ratio_df2 <- ratio_df %>%
  select(asset, year, cumsum_div_my_prod, cumsum_div_max_resources) %>%
  filter(year <= 2019) %>%
  rename(original_asset_name = asset)



econ_per_bbl_df2 <- econ_per_bbl_df %>%
  rename(asset = original_asset_name) %>%
  left_join(asset_sum_my_production) %>%
  mutate(sum_err_my_production_bbl = sum_err_my_production * 1e6) %>%
  select(-units) %>%
  left_join(asset_max_resources) %>%
  mutate(max_err_resources_bbl = max_err_resources * 1e6) %>%
  select(-units) %>%
  left_join(asset_yr_cumsum_production) %>%
  mutate(cumsum_err_production_bbl = cumsum_err_production * 1e6) %>%
  select(-production, - units) %>%
  rename(original_asset_name = asset) %>%
  left_join(ratio_df2)

# ## save to use later
write_csv(econ_per_bbl_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad_entry_variables.csv")
