## Tracey Mangin
## April 20, 2021
## entry input step 2: create the input file for the entry df

# ------------------------------------------- set up and inputs -----------------------------------
proj_dir           <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
data_directory     <- "data/stocks-flows/processed/"
output_dir         <- "outputs/stocks-flows/"
rystad_path        <- "data/Rystad/data/"
save_directory     <- "outputs/"

## files
field_asset_match_file  <- "field_asset_matches_v2_revised.csv"
rystad_econ_file        <- "oil_asset_opex_capex_govtt_clean.csv"
rystad_entry_file       <- "rystad_entry_variables.csv"
rystad_imputed_file     <- "Rystad_cost_imputed_revised.csv"
prod_file               <- "well_prod_m_processed.csv"
well_start_file         <- "well_start_prod_api10_revised.csv"
brent_file              <- "wti_brent.csv"


## input file needs the following info:
# doc_fieldname, doc_field_code, n_new_wells, capex_imputed, opex_imputed, m_cumsum_div_my_prod, brent, doc_prod

## libraries
library(data.table)
library(tidyverse)
library(lubridate)


## ------------------------------------------ load in the files -----------------------------

## field asset matches from entry_input_step1.R
field_asset_matches_v2 <- fread(paste0(proj_dir, output_dir, "entry-model-input/", field_asset_match_file), colClasses = c('doc_field_code' = 'character'))

## load rystad econ information (asset, year, econ group)
rystad_econ <- fread(paste0(proj_dir, rystad_path, "processed/", rystad_econ_file))

## entry varibales (now made in create_entry_econ_variables.R )
rystad_entry_variables <- fread(paste0(proj_dir, output_dir, rystad_entry_file))
rystad_entry_variables[, FieldName := NULL]                  

## inputed cost values
rystad_cost_imputed <- fread(paste0(proj_dir, output_dir, "rystad-imputed-cost/", rystad_imputed_file))

## well production
well_prod <- fread(paste0(proj_dir, data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                               'doc_field_code' = 'character'))

## for initial year production
init_yr_prod <- fread(paste0(proj_dir, output_dir, "well_start_yr/", well_start_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                     'doc_field_code' = 'character',
                                                                                                     'api_field' = 'character')) 
## prices
prices <- fread(paste0(proj_dir, rystad_path, "raw/", brent_file))

## start with the matches, add all variables
## ----------------------------------------------------------------------

## productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

## top 10 fields using 2019 production data
top_fields <- well_prod %>%
  filter(year == 2019) %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(prod19 = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(prod_rank = rank(-prod19),
         top_field = ifelse(prod_rank <= 10, prod_rank, 0)) %>%
  select(doc_field_code, top_field)


## productive fields x year
field_year <- expand.grid(doc_field_code = unique(productive_fields$doc_field_code),
                          year = unique(well_prod$year))

## annual production for all productive fields
field_prod_df_all <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, year)]
field_prod_df_all <- left_join(field_year, field_prod_df_all)
setDT(field_prod_df_all)
field_prod_df_all[, total_prod := fifelse(is.na(total_prod), 0, total_prod)]
field_prod_df_all <- field_prod_df_all[doc_field_code %chin% productive_fields[, c(doc_field_code)]]

## join field asset match with production
prod_df <- full_join(field_prod_df_all, field_asset_matches_v2) 

## View NA
prod_df %>% filter(is.na(original_asset_name)) %>% select(doc_field_code) %>% unique()
## Coal Oil Point offshore (154); 848 Old wilmington
## 154 has not produced since 1978
## 848 has not produced since 1984

prod_df <- prod_df[!is.na(original_asset_name)]

# ## check ABD fields
# abd_fields <- field_asset_matches %>%
#   filter(match_method == "well_match") %>%
#   mutate(ABD = str_detect(FieldName, "(ABD)"))

## are there multipe assets in a field?
n_asset <- unique(prod_df[, c("doc_field_code", "original_asset_name")])
n_asset <- n_asset[, .N, by = .(doc_field_code)]
## yes

## add the economic information
all_combos <- prod_df %>%
  left_join(rystad_cost_imputed) %>%
  select(doc_field_code, doc_fieldname, original_asset_name, year, total_prod, n_wells_asset, match_method, dist_m, capex_impute = capex_forecast, opex_impute = opex_forecast)

## make sure all fields have all years
View(all_combos %>% select(doc_field_code, year, original_asset_name) %>% group_by(doc_field_code, original_asset_name) %>% summarise(n = n()) %>% ungroup())

## set order
setorder(all_combos, doc_field_code, original_asset_name, year)

# ## how many assets per field?
# ## are there multipe assets in a field?
# View(all_combos %>%
#        select(doc_field_code, original_asset_name) %>%
#        unique() %>%
#        group_by(doc_field_code) %>%
#        summarise(n = n()) %>%
#        ungroup())
# 
# ## mostly 1 asset per field, but some with more
# 
# ## how many fields per asset?
# View(all_combos %>%
#        select(doc_field_code, original_asset_name) %>%
#        unique() %>%
#        group_by(original_asset_name) %>%
#        summarise(n = n()) %>%
#        ungroup())
# ## most associated with multiple fields, note that about a third of assets are matched

anti_join(rystad_econ %>% select(original_asset_name) %>% unique(), all_combos %>% select(original_asset_name) %>% unique())


 
# ## SUMMARISE AT THE FIELD LEVEL
# ## ------------------------------------------------

# ### should we remove 000, 154, 848?s

## check for NA cost values
econ_na <- all_combos[year > 1977]
econ_na <- econ_na[is.na(capex_impute) | is.na(opex_impute)]
## none

## 
prod_econ_df3 <- all_combos %>%
  left_join(rystad_entry_variables) %>%
  # filter(!FieldCode %in% c("000", "154", "848")) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(doc_prod = unique(total_prod),
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
  ungroup() 

## weighted mean
wm_prod_econ_df <- all_combos %>%
  left_join(rystad_entry_variables) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(wm_cumsum_div_my_prod = weighted.mean(cumsum_div_my_prod, n_wells_asset),
            wm_cumsum_div_max_res = weighted.mean(cumsum_div_max_resources, n_wells_asset),
            wm_cumsum_eer_prod_bbl = weighted.mean(cumsum_err_production_bbl, n_wells_asset),
            wm_opex_imputed = weighted.mean(opex_impute, n_wells_asset),
            wm_capex_imputed = weighted.mean(capex_impute, n_wells_asset)) %>%
  ungroup() 


## add number of wells

## n wells 2019
# nwells2 <- nwells %>%
#   select(FieldCode, total_wells_field) %>%
#   unique() %>%
#   rename(doc_field_code = FieldCode)

prod_econ_df4 <- left_join(prod_econ_df3, wm_prod_econ_df) %>%
  select(doc_field_code:capex_imputed, wm_capex_imputed, opex_imputed, wm_opex_imputed, wm_cumsum_div_my_prod, wm_cumsum_div_max_res, wm_cumsum_eer_prod_bbl)

## which fields don't have wells?
# no_wells <- prod_econ_df4 %>%
#   select(doc_fieldname, doc_field_code, total_wells_field) %>%
#   unique() %>%
#   filter(is.na(total_wells_field))


## using the same matching method, find the forecasting capex and opex values
## weighted based on n_wells_asset
## ---------------------------------------------------------------------------

rystad_cost_forecast <- rystad_cost_imputed %>%
  filter(year >= 2020)


econ_forecast <- all_combos %>%
  select(doc_field_code, original_asset_name, n_wells_asset) %>%
  unique() %>%
  left_join(rystad_cost_forecast) %>%
  group_by(doc_field_code, year) %>%
  summarise(m_opex_imputed = mean(opex_forecast),
            m_capex_imputed = mean(capex_forecast),
            wm_opex_imputed = weighted.mean(opex_forecast, n_wells_asset),
            wm_capex_imputed = weighted.mean(capex_forecast, n_wells_asset)) %>%
  ungroup() 

## save file
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast.csv")
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_10122020.csv")
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_10132020_v2.csv")
# write_csv(econ_forecast, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/field_capex_opex_forecast_final.csv")
write_csv(econ_forecast, path = paste0(proj_dir, output_dir, "field_capex_opex_forecast_revised.csv"))


## final set of field - asset matches
final_set <- all_combos %>%
  select(doc_field_code, doc_fieldname, original_asset_name, n_wells_asset, match_method, dist_m) %>%
  unique()

# ## save file 
write_csv(final_set, path = paste0(proj_dir, output_dir, "docfield_asset_crosswalk_entrydf_revised.csv"))


## add price
colnames(prices) <- c("year", "wti", "brent")

prices2 <- prices %>%
  filter(year != "Year",
         year >= 1977 & year <= 2019) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(year, brent)

prod_econ_prices_df <- prod_econ_df4 %>%
  left_join(prices2)


# ## save file for now
# write_csv(prod_econ_prices_df, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_inputs.csv")

## well, field,  year
## 0400100001

options(scipen = 999) 

## 10 digit API
## -----------------------------------------

# init_yr_prod2 <- init_yr_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   select(-orig_fc, -FieldCode2) %>%
#   mutate(api_ten_digit = as.character(paste0("0", api_ten_digit)))


## "0403702049" has production in first month in two fields
## how should we treat wells that enter two fields in its first year of production?
## how should we treat wells that produce in two different fields?

new_prod_df <- init_yr_prod %>%
  arrange(api_field, month_year) %>%
  mutate(year = year(month_year),
         start_year = year(start_date),
         start_field = ifelse(start_date == month_year & prod_bbl > 0, doc_field_code, NA)) %>%
  fill(start_field) %>%
  filter(year == start_year & doc_field_code == start_field) %>%
  group_by(api_ten_digit, doc_field_code, year) %>%
  summarise(new_well_prod = sum(prod_bbl, na.rm = T)) %>%
  ungroup() %>%
  filter(new_well_prod > 0) %>%
  group_by(doc_field_code, year) %>%
  summarise(new_prod = sum(new_well_prod),
            n_new_wells = n()) %>%
  ungroup() 



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
         n_new_wells = ifelse(is.na(n_new_wells), 0, n_new_wells)) %>%
  left_join(top_fields) %>%
  mutate(top_field = ifelse(is.na(top_field), 0, top_field))

test <- prod_econ_prices_df2 %>%
  mutate(div = new_prod / doc_prod,
         diff = doc_prod - new_prod)


# ## save file
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10122020.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10132020_v1.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_v2_10132020_v2.csv")
# write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_final.csv")
write_csv(prod_econ_prices_df2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_final_revised.csv")

# agg_new_wells <- prod_econ_prices_df2 %>%
#   group_by(year) %>%
#   summarise(sum_new_wells = sum(n_new_wells, na.rm = T)) %>%
#   ungroup()
# 
# ## n new wells
# ggplot(prod_econ_prices_df2 %>% filter(doc_field_code == "849"), aes(x = year, y = n_new_wells)) +
#   geom_line(size = 1)
# 
# ggplot(agg_new_wells %>% filter(year > 1992), aes(x = year, y = sum_new_wells)) +
#   geom_line(size = 1)
# 
# ggplot(prod_econ_prices_df2 %>% filter(year > 1977,
#                                        doc_field_code == "228"), aes(x = year, y = n_new_wells)) +
#   geom_line(size = 1)
