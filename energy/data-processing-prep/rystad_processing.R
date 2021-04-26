## Tracey Mangin
## July 15, 2020
## Script for cleaning Rystad data

## load libraries
library(tidyverse)
library(rebus)
library(stringi)
library(data.table)

## file paths
rystad_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/"
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"

## laod data
# economics_df <- read_csv(paste0(rystad_path, "raw/archive/ca_asset_opex_apex_govtt.csv"))
economics_df_update <- read_csv(paste0(rystad_path, "raw/asset_opex_capex_govtt.csv"))
production_df <- read_csv(paste0(rystad_path, "raw/ca_production.csv"))
econ_cats <- read_csv(paste0(rystad_path, "raw/asset_econ_categories.csv"))
err_df <- fread(paste0(rystad_path, "raw/resources_prod_myprod.csv"), skip = 1)
rystad_capex_recov_bbl <- read_csv(paste0(rystad_path, "raw/capex_per_recoverable_bbl.csv"))
api_asset <- read.csv(paste0(rystad_path, "raw/asset-wells.csv"))
rystad_capex_bbl_nom <- read_csv(paste0(rystad_path, "raw/capex_per_bbl_nom.csv"))
rystad_opex_bbl_nom <- read_csv(paste0(rystad_path, "raw/opex_per_bbl_nom.csv"))
asset_rename <- read_csv(paste0(rystad_path, "processed/rystad_asset_rename.csv"))
well_cost_eur <- read.csv(paste0(rystad_path, "raw/well_cost_per_eur.csv"))
field_asset <- read_csv(paste0(rystad_path, "raw/field_to_asset.csv"))

## well prod

prod_file               <- "well_prod_m_processed.csv"

well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                               'doc_field_code' = 'character'))


## economics
##-----------------------------------------

## clean data
economics_df2 <- janitor::clean_names(economics_df) %>%
  select(-data_values) %>%
  rename(usd_nom = sum) %>%
  mutate(usd_nom = usd_nom * 1e6,
         string_end = str_detect(asset, pattern = ", US" %R% END),
         ca_string = str_detect(asset, pattern = "_CA_"),
         clean_asset = str_remove(asset, pattern = ", US" %R% END),
         location = sub("_CA.*", "", clean_asset),
         location = ifelse(ca_string == FALSE, clean_asset, location),
         location = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, "United States", location),
         company = sub(".*_CA_", "", clean_asset),
         company = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, sub("_United States*.", "", clean_asset), company),
         company = ifelse(company == location, NA, company)) %>%
  select(original_asset_name = asset, location, company, economics_group, year, usd_nom)

# ## add field names
# fieldnames2 <- fieldnames %>%
#   select(-county) %>%
#   rename(location = field_name) %>%
#   unique()
# 
# anti_join(economics_df2 %>% select(location) %>% unique(), fieldnames2 %>% select(location) %>% unique())




write_csv(economics_df2, paste0(rystad_path, "processed/ca_asset_opex_capex_govtt_clean.csv"))


## updated economics variables (only oil and condensate operations included)
## ---------------------------------------------------------------------------------

## clean data
economics_up_df2 <- janitor::clean_names(economics_df_update) %>%
  pivot_longer(x1900:x2099, names_to = "year", values_to = "usd_nom") %>%
  mutate(year = as.numeric(str_sub(year, 2, 5)),
         usd_nom = usd_nom * 1e6,
         string_end = str_detect(asset, pattern = ", US" %R% END),
         ca_string = str_detect(asset, pattern = "_CA_"),
         clean_asset = str_remove(asset, pattern = ", US" %R% END),
         location = sub("_CA.*", "", clean_asset),
         location = ifelse(ca_string == FALSE, clean_asset, location),
         location = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, "United States", location),
         company = sub(".*_CA_", "", clean_asset),
         company = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, sub("_United States*.", "", clean_asset), company),
         company = ifelse(company == location, NA, company)) %>%
  select(original_asset_name = asset, location, company, economics_group, year, usd_nom)
         

write_csv(economics_up_df2, paste0(rystad_path, "processed/oil_asset_opex_capex_govtt_clean.csv"))  

## production
##-----------------------------------------

prod_df2 <- janitor::clean_names(production_df) %>%
  select(-data_values) %>%
  rename(bbls = sum) %>%
  mutate(bbls = bbls * 1e6,
         string_end = str_detect(asset, pattern = ", US" %R% END),
         ca_string = str_detect(asset, pattern = "_CA_"),
         clean_asset = str_remove(asset, pattern = ", US" %R% END),
         location = sub("_CA.*", "", clean_asset),
         location = ifelse(ca_string == FALSE, clean_asset, location),
         location = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, "United States", location),
         company = sub(".*_CA_", "", clean_asset),
         company = ifelse(str_detect(clean_asset, pattern = "_United States") == TRUE, sub("_United States*.", "", clean_asset), company),
         company = ifelse(company == location, NA, company)) %>%
  select(original_asset_name = asset, location, company, data_source, data_type, oil_and_gas_category, year, bbls)

write_csv(prod_df2, paste0(rystad_path, "processed/ca_oil_production.csv"))

## visualize total annual production

prod_df2 %>%
  group_by(data_type, year) %>%
  summarise(sum_bbls = sum(bbls)) %>%
  ungroup() %>%
  filter(year <= 2019) %>%
  ggplot(aes(x = year, y = sum_bbls / 1e6, fill = data_type)) +
  geom_area()


## econ categories
## ----------------------------
econ_cats2 <- econ_cats %>%
  filter(!is.na(X1)) %>%
  filter(X1 != "Economics Group") %>%
  rename(econ_group = X1,
         econ_cat = X2,
         original_asset_name = Year) %>%
  pivot_longer(`1970`:`2050`, names_to = "year", values_to = "million_usd") %>%
  mutate(year = as.numeric(year),
         usd = as.numeric(million_usd) * 1e6) %>%
  select(-million_usd)

write_csv(econ_cats2, paste0(rystad_path, "processed/asset_economics_cats.csv"))

## economically recoverable resources scenarios
## -----------------


err_df2 = data.table(melt(err_df, id.vars = "[Data Values]",  variable.name = "scenario", value.name = "value"), 
                        year = rep(rep(1900:2100, each = 3), each = nrow(err_df)))
colnames(err_df2)[1] = "asset"
setcolorder(err_df2, c("asset", "year", "scenario", "value"))
setorderv(err_df2, c("year", "asset"))

err_df2[ scenario == "My ProductionMy Production [120] (Million bbl)", scenario := "My Production [120] (Million bbl)" ]

err_df2_wide = dcast(err_df2, asset + year ~ scenario, value.var = 'value')

fwrite(err_df2, paste0(rystad_path, "processed/economically_recoverable_resources_scenarios.csv"))
fwrite(err_df2_wide, paste0(rystad_path, "processed/economically_recoverable_resources_scenarios_wide.csv"))


## capex per bbl
## ------------------------------------

capex_recov_bbl2 <- rystad_capex_recov_bbl %>%
  filter(!is.na(X1)) %>%
  filter(X1 != "Asset") %>%
  rename(original_asset_name = X1,
         economics_group = Year) %>%
  pivot_longer(`1970`:`2050`, names_to = "year", values_to = "usd_per_bbl") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(economics_group = "capex_per_bbl_reserves")

write_csv(capex_recov_bbl2, paste0(rystad_path, "processed/capex_bbl_reserves.csv"))

## ---------------------------------------------------------------------------
## wells in assets
## --------------------------------------------------------------------------


api_asset2 <- api_asset %>%
  rename(APINumber = API.Number,
         no_asset = X) %>%
  filter(APINumber != "Sum") %>%
  mutate(APINumber = as.character(str_remove_all(APINumber, pattern = "-"))) %>%
  pivot_longer(no_asset:Wilmington_CA_Warren.Resources..US, names_to = "asset_name", values_to = "prod") %>%
  filter(!is.na(prod))


## how many have more than one asset?
n_asset <- api_asset2 %>%
  select(APINumber, asset_name) %>%
  group_by(APINumber) %>%
  mutate(n = n()) %>%
  ungroup() 
## 1 to 1

diff_df <- api_asset2 %>%
  mutate(diff = Sum - prod)
## 0 diff

## clean asset names
api_asset3 <- api_asset2 %>%
  left_join(asset_rename) %>%
  select(-asset_name)
  

write_csv(api_asset3, paste0(rystad_path, "processed/rystad_asset_apis.csv"))

## add fields
## --------------------------

## work with API ten digit
api_asset4 <- api_asset3 %>%
  mutate(api_n = nchar(APINumber),
         state = str_sub(APINumber, 1, 2),
         api_adj = str_replace_all(APINumber, pattern = "O", "0"), 
         api_adj = ifelse(api_n < 12, paste0(api_adj, "00000"), APINumber),
         api_adj2 = ifelse(nchar(api_adj) > 12 & state == "04", str_sub(api_adj, 1, 12), api_adj)) %>%
  rename(orig_api = APINumber) %>%
  select(APINumber = api_adj2, original_asset_name) %>%
  mutate(api_ten_digit = str_sub(APINumber, 1, 10))

## unique api_ten_digit - field combos
api_field_df <- well_prod %>%
  group_by(api_ten_digit, doc_field_code, doc_fieldname) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(total_prod > 0) 

## multiple field matches?
View(api_field_df %>% group_by(api_ten_digit) %>% mutate(n = n()) %>% ungroup())
## yes

## try to match
api_asset_match <- api_asset4 %>%
  left_join(api_field_df)

## how many na?
nrow(api_asset_match %>% filter(is.na(doc_field_code))) / nrow(api_asset4)

## asset n wells
asset_fields <- api_asset_match %>%
  group_by(original_asset_name, doc_fieldname) %>%
  summarise(n_field = n()) %>%
  ungroup() %>%
  group_by(original_asset_name) %>%
  mutate(n_asset = sum(n_field)) %>%
  ungroup() %>%
  mutate(rel_field = n_field / n_asset)
  
unique(asset_fields$doc_fieldname)

## reverse, reverse!
api_asset_match_rev <- api_field_df %>%
  left_join(api_asset4) 

## field n asset
field_n_asset <- api_asset_match_rev %>%
  filter(!is.na(original_asset_name),
         original_asset_name != "no_asset") %>%
  group_by(doc_field_code, doc_fieldname, original_asset_name) %>%
  summarise(n_wells_asset = n(),
            bbl_prod = sum(total_prod, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, doc_fieldname) %>%
  mutate(n_wells_field = sum(n_wells_asset),
         field_prod = sum(bbl_prod)) %>%
  ungroup() %>%
  mutate(rel_field = n_wells_asset / n_wells_field,
         rel_prod = bbl_prod / field_prod)

anti_join(api_asset4 %>% select(original_asset_name) %>%  unique(), field_n_asset %>% select(original_asset_name) %>% unique())

write_csv(field_n_asset, paste0(rystad_path, "processed/field_rystad_match_apis_revised.csv"))
  
## capex per bbl nominal
rystad_capex_bbl_nom2 <- rystad_capex_bbl_nom %>%
  rename(original_asset_name = Asset) %>%
  pivot_longer(`1900`:`2099`, names_to = "year", values_to = "capex_per_bbl_nom") %>%
  mutate(year = as.numeric(year),
         capex_per_bbl_nom = as.numeric(capex_per_bbl_nom)) %>%
  select(-`Economics Group`)

write_csv(rystad_capex_bbl_nom2, paste0(rystad_path, "processed/rystad_capex_bbl_nom_clean.csv"))


## opex per bbl nominal
rystad_opex_bbl_nom2 <- rystad_opex_bbl_nom[3:nrow(rystad_opex_bbl_nom),] %>%
  select(-Year) %>%
  rename(original_asset_name = X1) %>%
  pivot_longer(`1970`:`2050`, names_to = "year", values_to = "opex_per_bbl_nom") %>%
  mutate(year = as.numeric(year),
         opex_per_bbl_nom = as.numeric(opex_per_bbl_nom)) 

write_csv(rystad_opex_bbl_nom2, paste0(rystad_path, "processed/rystad_opex_bbl_nom_clean.csv"))


## ---------------------------
## well cost eur
## ---------------------------

well_cost_eur2 <- well_cost_eur %>%
  pivot_longer("Afton.Gas":"Zamora.Gas..ABD.", names_to = "field", values_to = "well_cost_eurusd_per_bbl") %>%
  rename(APInumber = API.Number) %>%
  mutate(APInumber = as.character(str_remove_all(APInumber, pattern = "-"))) %>%
  mutate(both_na = ifelse(is.na(X) & is.na(well_cost_eurusd_per_bbl), 1, 0)) %>%
  filter(both_na != 1) %>%
  select(-X, -both_na) 


write_csv(well_cost_eur2, paste0(rystad_path, "processed/well_cost_per_eur_clean.csv"))

## ---------------------------
## fields
## ---------------------------

field_asset2 <- field_asset %>%
 rename(days_prod = `Days on Production (Days)`) %>%
 filter(!is.na(Asset)) %>%
 select(-days_prod) %>%
 unique()
  
write_csv(field_asset2, paste0(rystad_path, "processed/rystad_field_asset.csv"))




