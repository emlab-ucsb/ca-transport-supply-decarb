## Tracey Mangin
## September 15, 2020
## Create entry model input df
## Step 2: create the input file for the entry df

library(tidyverse)
library(lubridate)

## source items
## --------------------------------------------------
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## define data folders
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## read in the files needed
## --------------------------------------------------

## field asset matches based on step 1
# field_asset_matches <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_asset_matches.csv") %>%
#   mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode),
#          FieldName = ifelse(FieldCode == "849", "Wilmington", FieldName))

## v2
field_asset_matches_v2 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_asset_matches_v2.csv") %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode))


## load rystad econ information (asset, year, econ group)
rystad_econ <- read_csv(paste0(rystad_path, "processed/oil_asset_opex_capex_govtt_clean.csv"))

## load well_cost_euruds_per_bbl
cost_per_eur <- read.csv(paste0(rystad_path, "processed/well_cost_per_eur_clean.csv")) %>%
  select(-field) %>%
  unique()

## entry varibales (now made in create_entry_econ_variables.R )
rystad_entry_variables <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad_entry_variables.csv") %>%
  select(-FieldName)

## inputed cost values
# rystad_cost_imputed <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed.csv") 
# rystad_cost_imputed <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_10122020.csv") 
# rystad_cost_imputed <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_10132020_v1.csv")
# rystad_cost_imputed <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_10132020_v2.csv")
rystad_cost_imputed <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_10132020_v3.csv")

## well production
well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode))

## wells
wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10))

## make table for matching field id with field name
fieldcodes <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique()

## number of wells
nwells <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_type_x_field_no_plugged.csv") 

## for initial year production
# init_yr_prod <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10_x_field.csv")
init_yr_prod <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10.csv") %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode),
         api_field = paste0(api_ten_digit, "-", FieldCode),
         month_year = as.Date(month_year),
         start_date = as.Date(start_date))


## add api over time
api_time <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/annual_wm_api_x_field.csv") %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2, -FieldName) 

## prices
prices <- read_csv(paste0(rystad_path, "raw/wti_brent.csv"))


## start with the matches, add all variables
## ----------------------------------------------------------------------

## productive fields
productive_fields <- well_prod %>%
  group_by(FieldCode) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(total_prod > 0) %>%
  left_join(fieldcodes)

## productive fields x year
field_year <- expand.grid(FieldCode = unique(productive_fields$FieldCode),
                          year = unique(well_prod$year))

## annual production for all productive fields
field_prod_df_all <- well_prod %>%
  group_by(FieldCode, year) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  full_join(field_year) %>%
  mutate(total_prod = ifelse(is.na(total_prod), 0, total_prod)) %>% 
  select(FieldCode, year, total_prod) %>%
  filter(FieldCode %in% productive_fields$FieldCode) 


## join field asset match with production
# prod_df <- full_join(field_prod_df_all, field_asset_matches) 
prod_df <- full_join(field_prod_df_all, field_asset_matches_v2) 

# ## check ABD fields
# abd_fields <- field_asset_matches %>%
#   filter(match_method == "well_match") %>%
#   mutate(ABD = str_detect(FieldName, "(ABD)"))


## are there multipe assets in a field?
n_assets_x_field <- prod_df %>%
  select(FieldName, FieldCode, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name)) %>%
  group_by(FieldName, FieldCode) %>%
  summarise(n = n()) %>%
  ungroup()
## yes

## are there any doc fields without assets?

na_asset <- prod_df %>%
  select(FieldName, FieldCode, original_asset_name) %>%
  unique() %>%
  filter(is.na(original_asset_name))

nrow(na_asset)
## 000 and 154
## 154 has not produced since 1978

## add the economic information

all_combos <- prod_df %>%
  left_join(rystad_cost_imputed) %>%
  filter(!FieldCode %in% na_asset$FieldCode) %>%
  select(FieldName, FieldCode, original_asset_name, year, total_prod, n_wells_asset, match_method, dist_m, capex_impute = capex_forecast, opex_impute = opex_forecast)

## which field code is missing?
dfa <- all_combos %>%
  select(FieldCode) %>%
  unique()

dfb <- prod_df %>%
  select(FieldCode) %>%
  unique()

anti_join(dfb, dfa)
## 154 - Coal Oil Point Offshore (ABD)

## which asset is missing from well match?
dfc <- all_combos %>%
  select(original_asset_name) %>%
  unique()

dfd <- prod_df %>%
  select(original_asset_name) %>%
  unique()

dfe <- rystad_cost_imputed %>%
  select(original_asset_name) %>%
  unique()

anti_join(dfc, dfd)
# not relevant....?

anti_join(dfc, dfe)
## this should be 0 




# full_prod <- expand_grid(FieldCode = unique(all_combos$FieldCode),
#                          year = unique(field_prod_df_all$year))

prod_econ_df2 <- all_combos %>%
  # left_join(field_prod) %>%
  left_join(api_time) %>%
  select(FieldName, FieldCode, year, total_prod, original_asset_name, n_wells_asset, capex_impute, opex_impute, wm_api, match_method, dist_m) %>%
  mutate(total_prod = ifelse(is.na(total_prod), 0, total_prod)) %>%
  # mutate(capex = ifelse(is.na(capex), 0, capex),
  #        opex = ifelse(is.na(opex), 0, opex),
  #        capex_bbl_rp = ifelse(is.na(capex_bbl_rp), 0, capex_bbl_rp),
  #        capex_per_bbl_reserves = ifelse(is.na(capex_per_bbl_reserves), 0, capex_per_bbl_reserves),
  #        capex_per_bbl_nom = ifelse(is.na(capex_per_bbl_nom), 0, capex_per_bbl_nom),
  #        opex_per_bbl_nom = ifelse(is.na(opex_per_bbl_nom), 0, opex_per_bbl_nom),
  #        opex_bbl_rp = ifelse(is.na(opex_bbl_rp), 0, opex_bbl_rp)) %>%
  arrange(FieldCode, FieldName, original_asset_name, year) 

# ## how many assets per field?
# ## are there multipe assets in a field?
# View(prod_econ_df2 %>%
#        select(FieldName, FieldCode, original_asset_name) %>%
#        unique() %>%
#        group_by(FieldName, FieldCode) %>%
#        summarise(n = n()) %>%
#        ungroup())
# 
# ## mostly 1 asset per field, but some with more
# 
# ## how many fields per asset?
# View(prod_econ_df2 %>%
#        select(FieldCode, original_asset_name) %>%
#        unique() %>%
#        group_by(original_asset_name) %>%
#        summarise(n = n()) %>%
#        ungroup())
# ## about a third associated with multiple fields
# 
# ## SUMMARISE AT THE FIELD LEVEL
# ## ------------------------------------------------
# 
# ### should we remove 000, 154, 848?
# ### remove these for now
# 
# 
# 


econ_na <- prod_econ_df2 %>%
  filter(year > 1977) %>%
  filter(is.na(capex_impute) | is.na(opex_impute)) 

na_asset_names <- econ_na$original_asset_name %>% unique()

## removing
print(na_asset_names) 


prod_econ_df3 <- prod_econ_df2 %>%
  filter(!is.na(year),
         !original_asset_name %in% na_asset_names) %>%
  left_join(rystad_entry_variables) %>%
  # filter(!FieldCode %in% c("000", "154", "848")) %>%
  group_by(FieldName, FieldCode, year) %>%
  summarise(doc_prod = unique(total_prod),
            wm_api_gravity = unique(wm_api),
            capex = mean(capex, na.rm = T),
            # capex_wm = weighted.mean(capex, n_wells_asset),
            capex_bbl_rp = mean(capex_bbl_rp, na.rm = T),
            # capex_bbl_rp_wm = weighted.mean(capex_bbl_rp, n_wells_asset, na.rm = T),
            capex_per_bbl_reserves =  mean(capex_per_bbl_reserves, na.rm = T),
            # capex_per_bbl_reserve_wm = weighted.mean(capex_per_bbl_reserves, n_wells_asset, na.rm = T),
            capex_per_bbl_nom = mean(capex_per_bbl_nom, na.rm = T),
            # capex_per_bbl_nom_wm = weighted.mean(capex_per_bbl_nom, n_wells_asset, na.rm = T),
            opex = mean(opex, na.rm = T),
            # oppex_wm = weighted.mean(opex, n_wells_asset, na.rm = T),
            opex_bbl_rp = mean(opex_bbl_rp, na.rm = T),
            # opex_bbl_rp_wm = weighted.mean(opex_bbl_rp, n_wells_asset, na.rm = T),
            opex_per_bbl_nom = mean(opex_per_bbl_nom, na.rm = T),
            # opex_per_bbl_nom_wm = weighted.mean(opex_per_bbl_nom, n_wells_asset, na.rm = T),
            m_cumsum_div_my_prod = mean(cumsum_div_my_prod, na.rm = T),
            # wm_cumsum_div_my_prod = weighted.mean(cumsum_div_my_prod, n_wells_asset, na.rm = T),
            m_cumsum_div_max_res = mean(cumsum_div_max_resources, na.rm = T),
            # wm_cumsum_div_max_res = weighted.mean(m_cumsum_div_max_res, n_wells_asset, na.rm = T)
            capex_imputed = mean(capex_impute, na.rm = T),
            # wm_capex_imputed = weighted.mean(capex_imputed, n_wells_asset, na.rm = T),
            opex_imputed = mean(opex_impute, na.rm = T)
            # wm_opex_imputed = weighted.mean(capex_imputed, n_wells_asset)
  ) %>%
  ungroup() %>%
  rename(doc_fieldname = FieldName,
         doc_field_code = FieldCode)


wm_prod_econ_df <- prod_econ_df2 %>%
  left_join(rystad_entry_variables) %>%
  group_by(FieldName, FieldCode, year) %>%
  summarise(wm_cumsum_div_my_prod = weighted.mean(cumsum_div_my_prod, n_wells_asset),
            wm_cumsum_div_max_res = weighted.mean(cumsum_div_max_resources, n_wells_asset),
            wm_cumsum_eer_prod_bbl = weighted.mean(cumsum_err_production_bbl, n_wells_asset),
            wm_opex_imputed = weighted.mean(opex_impute, n_wells_asset),
            wm_capex_imputed = weighted.mean(capex_impute, n_wells_asset)) %>%
  ungroup() %>%
  rename(doc_fieldname = FieldName,
         doc_field_code = FieldCode)


## add number of wells

## n wells 2019
nwells2 <- nwells %>%
  select(FieldCode, total_wells_field) %>%
  unique() %>%
  rename(doc_field_code = FieldCode)

prod_econ_df4 <- left_join(prod_econ_df3, wm_prod_econ_df) %>%
  left_join(nwells2) %>%
  select(doc_fieldname:capex_imputed, wm_capex_imputed, opex_imputed, wm_opex_imputed, wm_cumsum_div_my_prod, wm_cumsum_div_max_res, wm_cumsum_eer_prod_bbl, total_wells_field)

## which fields don't have wells?
no_wells <- prod_econ_df4 %>%
  select(doc_fieldname, doc_field_code, total_wells_field) %>%
  unique() %>%
  filter(is.na(total_wells_field))


## using the same matching method, fined the forecasting capex and opex values
## weighted based on n_wells_asset
## ---------------------------------------------------------------------------

rystad_cost_forecast <- rystad_cost_imputed %>%
  filter(year >= 2020)


econ_forecast <- prod_econ_df2 %>%
  select(FieldCode, original_asset_name, n_wells_asset) %>%
  unique() %>%
  left_join(rystad_cost_forecast) %>%
  group_by(FieldCode, year) %>%
  summarise(m_opex_imputed = mean(opex_forecast),
            m_capex_imputed = mean(capex_forecast),
            wm_opex_imputed = weighted.mean(opex_forecast, n_wells_asset),
            wm_capex_imputed = weighted.mean(capex_forecast, n_wells_asset)) %>%
  ungroup() %>%
  rename(doc_field_code = FieldCode)

## save file
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast.csv")
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_10122020.csv")
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_10132020_v2.csv")
write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_final.csv")



## final set
final_set <- prod_econ_df2 %>%
  filter(!original_asset_name %in% na_asset_names) %>%
  select(doc_fieldname = FieldName, doc_field_code = FieldCode, original_asset_name, n_wells_asset, match_method, dist_m) %>%
  left_join(nwells2) %>%
  unique()

# ## save file for now
# write_csv(final_set, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/docfield_asset_crosswalk_entrydf_v2.csv")
write_csv(final_set, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/docfield_asset_crosswalk_entrydf_final.csv")


## add price
price_names <- c("year", "wti", "brent")
colnames(prices) <- price_names

prices2 <- prices %>%
  filter(year != "Year",
         year >= 1977 & year <= 2019) %>%
  mutate(year = as.numeric(year))

prod_econ_prices_df <- prod_econ_df4 %>%
  left_join(prices2)


# ## save file for now
# write_csv(prod_econ_prices_df, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_inputs.csv")

## well, field,  year
## 0400100001

options(scipen = 999) 

cost_per_eur2 <- cost_per_eur %>%
  mutate(APInumber = as.character(APInumber),
         nchar = nchar(APInumber),
         APINumber = substr(APInumber, 1, 12),
         api_ten_digit = substr(APInumber, 1, 10)) %>%
  select(APINumber, api_ten_digit, well_cost_eurusd_per_bbl)


## 10 digit API
## -----------------------------------------

# init_yr_prod2 <- init_yr_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   select(-orig_fc, -FieldCode2) %>%
#   mutate(api_ten_digit = as.character(paste0("0", api_ten_digit)))


cost_per_eur2_api10 <- cost_per_eur2 %>%
  select(api_ten_digit, well_cost_eurusd_per_bbl) %>%
  group_by(api_ten_digit) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(api_ten_digit) %>%
  summarise(well_cost_eurusd_per_bbl = mean(well_cost_eurusd_per_bbl, na.rm = T)) %>%
  ungroup()

new_prod_df <- init_yr_prod %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode) %>%
  rename(FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  mutate(api_ten_digit = as.character(paste0("0", api_ten_digit))) %>%
  # ## is right?
  # filter(FieldCode == "848") %>%
  # ####
mutate(start_year = year(start_date)) %>%
  filter(year == start_year) %>%
  unique() %>%
  left_join(fieldcodes) %>%
  group_by(api_ten_digit, FieldName, FieldCode, year) %>%
  summarise(new_well_prod = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  left_join(cost_per_eur2_api10) %>%
  group_by(FieldName, FieldCode, year) %>%
  summarise(new_prod = sum(new_well_prod),
            n_new_wells = n(),
            m_wc_eur_usd_per_bbl = mean(well_cost_eurusd_per_bbl, na.rm = T)) %>%
  ungroup() %>%
  rename(doc_field_code = FieldCode) %>%
  select(-FieldName)



## 12 digit API
## -----------------------------------------

# init_yr_prod2 <- init_yr_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   select(-orig_fc, -FieldCode2) %>%
#   mutate(APINumber = as.character(paste0("0", APINumber))) 
# 
# 
# 
# new_prod_df <- init_yr_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   select(-orig_fc, -FieldCode2) %>%
#   mutate(APINumber = as.character(paste0("0", APINumber))) %>%
#   # ## is kern river right?
#   # filter(FieldCode == "340",
#   #        year == 2018) %>%
#   # ####
#   mutate(start_year = year(start_date)) %>%
#   filter(year == start_year) %>%
#   left_join(fieldcodes) %>%
#   group_by(APINumber, FieldName, FieldCode, api_field, year) %>%
#   summarise(new_well_prod = sum(api_prod)) %>%
#   ungroup() %>%
#   left_join(cost_per_eur2) %>% 
#   group_by(FieldName, FieldCode, year) %>%
#   summarise(new_prod = sum(new_well_prod),
#             n_new_wells = n(),
#             m_wc_eur_usd_per_bbl = mean(well_cost_eurusd_per_bbl, na.rm = T)) %>%
#   ungroup() %>%
#   rename(doc_field_code = FieldCode) %>%
#   select(-FieldName)



# 
# ## test with 582
# # test_df <- init_yr_prod2 %>%
# #   filter(year == start_year) %>%
# #   select(api_ten_digit, year, api_prod) %>%
# #   group_by(api_ten_digit, year) %>%
# #   summarise(new_prod = sum(api_prod, na.rm = T)) %>%
# #   ungroup() %>%
# #   left_join(api_field) %>%
# #   filter(FieldCode == 582,
# #          year == 2011)
# 
# prod_test <- well_prod %>%
#   filter(api_ten_digit == "0403043474",
#          year == 2011)

prod_econ_prices_df2 <- prod_econ_prices_df %>%
  left_join(new_prod_df) %>%
  mutate(new_prod = ifelse(is.na(new_prod), 0, new_prod),
         n_new_wells = ifelse(is.na(n_new_wells), 0, n_new_wells),
         capex_div_new_prod = capex / new_prod)

test <- prod_econ_prices_df2 %>%
  mutate(div= new_prod / doc_prod)


# ## save file
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10122020.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10132020_v1.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10132020_v2.csv")
write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_final.csv")

agg_new_wells <- prod_econ_prices_df2 %>%
  group_by(year) %>%
  summarise(sum_new_wells = sum(n_new_wells, na.rm = T)) %>%
  ungroup()

## n new wells
ggplot(prod_econ_prices_df2 %>% filter(doc_field_code == "849"), aes(x = year, y = n_new_wells)) +
  geom_line(size = 1)

ggplot(agg_new_wells %>% filter(year > 1992), aes(x = year, y = sum_new_wells)) +
  geom_line(size = 1)

ggplot(prod_econ_prices_df2 %>% filter(year > 1977,
                                       doc_field_code == "228"), aes(x = year, y = n_new_wells)) +
  geom_line(size = 1)
