## Tracey Mangin
## May 11, 2021
## Match fields to assets

## revised: feb 16 2024 by haejin 

# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/capstone/freshcair/meds-freshcair-capstone/data/processed/"
rystad_path        <- "/capstone/freshcair/meds-freshcair-capstone/data/processed/"
sp_dir             <- "/capstone/freshcair/meds-freshcair-capstone/data/inputs/gis/"
save_directory     <- "/capstone/freshcair/meds-freshcair-capstone/data/proprietery-data/"
proj_dir           <- "/capstone/freshcair/meds-freshcair-capstone/"
output_dir         <- "proprietery-data"
match_out_path     <- "proprietery-data"



## files
rystad_file  <- "field_rystad_match_apis_revised.csv"
prod_file    <- "well_prod_m_processed.csv"
field_b_file <- "DOGGR_Admin_Boundaries_Master.shp"
raw_asset_loc_file <- "asset_latlon.csv"
rystad_prod_file <- "ca_oil_production.csv"
rystad_imputed_file <- "Rystad_cost_imputed_all_assets.csv"



## -------------------------- library
library(tidyverse)
library(data.table)
library(mapview)
library(sf)
library(rgeos) #nearest poly to each point
#library(nngeo) #nearest point to each poly


## step 0: fields matched to assets through wells
## ---------------------------------------------------------

## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))


## asset to field match using well APIs
field_asset_match <- fread(paste0(rystad_path, rystad_file), colClasses = c('doc_field_code' = 'character'))
field_asset_match[, c("doc_fieldname", "bbl_prod", "n_wells_field", "field_prod", "rel_field", "rel_prod") := NULL]

## compute productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

## field to asset match by well apis
well_match_df <- productive_fields %>%
  dplyr::select(doc_field_code) %>%
  left_join(field_asset_match) 

fieldcodes <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  raster::unique() %>%
  dplyr::filter(doc_field_code %in% productive_fields$doc_field_code)


## fields that get matched with assets
field_asset_well_match <- well_match_df %>%
  dpylr::select(doc_field_code, original_asset_name) %>%
  raster::unique() %>%
  dplyr::filter(!is.na(original_asset_name))

## save file
# write_csv(field_asset_well_match, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match.csv")
write_csv(field_asset_well_match, file = paste0(save_directory, "stocks-flows/entry-model-input/well_doc_asset_match_revised.csv")) 


## are there multipe assets in a field?
n_assets_x_field <- well_match_df %>%
  select(doc_field_code, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name)) %>%
  group_by(doc_field_code) %>%
  summarise(n = n()) %>%
  ungroup()

ggplot(n_assets_x_field, aes(x = n)) +
  geom_histogram(binwidth = 1)
## yes

## are there any doc fields without assets?
na_asset <- well_match_df %>%
  select(doc_field_code, original_asset_name) %>%
  unique() %>%
  filter(is.na(original_asset_name))
nrow(na_asset)

## filter for the fields without an asset
na_asset2 <- na_asset %>%
  select(doc_field_code) %>%
  unique()

# ## filter field assets for missing assets and doc fields
# sp_matches <- nn_assets %>%
#   select(doc_field_code,
#          original_asset_name = nn_asset,
#          dist) %>%
#   filter(doc_field_code %in% na_asset2$doc_field_code) %>%
#   mutate(n_wells_asset = 1,
#          match_method = "spatial") %>%
#   select(doc_field_code, original_asset_name, n_wells_asset, match_method, dist)
# 
# anti_join(na_asset2 %>% select(doc_field_code) %>% unique(), sp_matches %>% select(doc_field_code) %>% unique())
# # 154 - Coal Oil Point Offshore (ABD) -- this field is not in the field boundary spatial data



## step 1: filter for relevant assets
## ---------------------------------------------------------
field_assets <- read_csv(paste0(rystad_path, "raw/", raw_asset_loc_file))

rystad_prod <- read_csv(paste0(rystad_path, "processed/", rystad_prod_file))

## filter for assets with production in our time horizon
rystad_prod_th <- rystad_prod %>%
  filter(year >= 1977 & year < 2020) %>%
  group_by(original_asset_name) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  filter(bbls > 0)

field_assets_adj <- field_assets %>%
  filter(Asset %in% rystad_prod_th$original_asset_name)

write.csv(field_assets_adj, paste0(rystad_path, "processed/asset_latlon_adj.csv"))


## make df of nearest neighbor field to asset
## ----------------------------------------------------------------

rystad_cost_imputed <- fread(paste0(proj_dir, output_dir, "rystad-imputed-cost/", rystad_imputed_file))

cost_imputed <- rystad_cost_imputed %>%
  mutate(na_val = ifelse(is.na(capex_forecast) | is.na(opex_forecast), 1, 0)) %>%
  filter(na_val == 0) %>%
  group_by(original_asset_name) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(-na_val, -n)
  

## field location                     
fields_loc <- st_read(paste0(sp_dir, field_b_file))

asset_loc_sf <- st_as_sf(field_assets_adj, coords = c("Longitude", "Latitude"), 
                         crs = st_crs(fields_loc))

asset_loc_sf <- asset_loc_sf %>%
  filter(Asset %in% unique(cost_imputed$original_asset_name))

st_crs(fields_loc) == st_crs(asset_loc_sf)

plot(st_geometry(fields_loc))
plot(asset_loc_sf, add = TRUE)

## nearest point to each polygon 
find_n_asset <- function(fcode) {
  
  field_code <- fcode
  
  field_sp <- fields_loc %>%
    filter(FIELD_CODE == field_code)
  
  nf <- st_nn(field_sp, asset_loc_sf, k = 10, returnDist = T)
  
  match_tmp <- asset_loc_sf[nf$nn[[1]], ]
  
  match_tmp_dist <- nf$dist[[1]]
  
  match_tmp$dist <- match_tmp_dist
  
  match_tmp2 <- match_tmp %>%
    filter(dist == min(dist))
  
  final_a <- match_tmp2 %>%
    as_tibble() %>%
    select(nn_asset = Asset,
           dist) 
  
  orig_field_df <- tibble(doc_field_code = rep(field_code, nrow(final_a)))
  
  final <- cbind(orig_field_df, final_a)
  
}

## fields
field_vec <- unique(fields_loc$FIELD_CODE)

## run the function to find nearest asset(s)
nn_assets <- purrr::map(field_vec, find_n_asset) %>%
  bind_rows()

## how many assets per field?
nn_assets_n <- nn_assets %>%
  group_by(doc_field_code) %>%
  mutate(n = n()) %>%
  ungroup()

# ## mapview
# mapview(fields_loc %>% filter(FIELD_CODE == "464"), layer.name = "Fields", label = fields_loc$NAME) +
#   mapview(asset_loc_sf, col.regions = "yellow", layer.name = "Asset",  label = asset_loc_sf$Asset)

write.csv(nn_assets, paste0(rystad_path, "processed/fieldsAssets_adj_revised.csv"))

## nearest field neighbor
## -----------------------------------------

##########################################################
# find nearest field
##########################################################

## OFF FOR NOW
## reassign 848 to 849 because Old Wilmington does not have a spatial boundary, but we know it is in the
## same spot at 849
# matches[, doc_field_code := fifelse(doc_field_code == "848", "849", doc_field_code)]

## fields that get matched with assets
matches <- well_match_df %>%
  filter(!is.na(original_asset_name)) %>%
  unique() 

matches_check <- left_join(matches, asset_loc_sf, by = c("original_asset_name" = "Asset"))

## field options
field_options <- fields_loc %>%
  # st_transform(CRS("+init=epsg:3310")) %>%
  filter(FIELD_CODE %in% matches$doc_field_code) 

## check matches
anti_join(matches %>% select(doc_field_code) %>% unique(), field_options %>% select(FIELD_CODE) %>% rename(doc_field_code = FIELD_CODE) %>% unique())
## no match for 000 Any field or 848 Old Wilmington

## function - find nearest neighbor

find_nfield <- function(fcode) {
  
  field_code <- fcode
  
  field_sp <- fields_loc %>%
    filter(FIELD_CODE == field_code)
  
  other_fields <- field_options %>%
    filter(FIELD_CODE != field_code)
  
  nf <- st_nn(field_sp, other_fields, k = 10, returnDist = T)
  
  match_tmp <- other_fields[nf$nn[[1]], ]
  
  match_tmp_dist <- nf$dist[[1]]
  
  match_tmp$dist <- match_tmp_dist
  
  match_tmp2 <- match_tmp %>%
    filter(dist == min(dist))
  
  # mfn <- match_tmp$NAME
  # mfc <- match_tmp$FIELD_CODE
  # dist <- nf$dist[[1]][1]
  # 
  # final_tib <- tibble(doc_field_code = field_code,
  #                     nn_field_code = mfc,
  #                     nn_field_name = mfn,
  #                     dist = dist)
  
  final_a <- match_tmp2 %>%
    as_tibble() %>%
    select(nn_field_code = FIELD_CODE,
           nn_field_name = NAME,
           dist) 
  
  orig_field_df <- tibble(doc_field_code = rep(field_code, nrow(final_a)))
  
  final <- cbind(orig_field_df, final_a)
  
}

## field vec

# field_vec <- unique(fields_loc$FIELD_CODE)
valid_fields <- fields_loc %>% 
  filter(FIELD_CODE %in% productive_fields$doc_field_code)

# anti_join(productive_fields %>% select(doc_field_code) %>% unique(), valid_fields %>% rename(doc_field_code = FIELD_CODE) %>% select(doc_field_code) %>% unique())


field_vec <- unique(valid_fields$FIELD_CODE)

nn_fields <- purrr::map(field_vec, find_nfield) %>%
  bind_rows()

nn_fields_n <- nn_fields %>%
  group_by(doc_field_code) %>%
  mutate(n = n()) %>%
  ungroup()


write_csv(nn_fields, file = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_x_field_match_revised.csv")


## ---------------------------------------------
## visualizations
fields_sf2 <- fields_loc %>%
  filter(FIELD_CODE %in% productive_fields$doc_field_code) %>%
  mutate(cost_info = ifelse(NAME %in% field_options$NAME, "yes", "no")) %>%
  left_join(nn_fields, by = c("FIELD_CODE" = "doc_field_code")) %>%
  select(NAME, FIELD_CODE, nn_field_code, nn_field_name, cost_info)

## counties
counties <- read_sf(dsn = paste0("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/CA_Counties/", layer = "CA_Counties_TIGER2016.shp")) %>%
  st_transform(st_crs(fields_loc))

## mapview
mapview::mapview(fields_sf2, zcol = "cost_info", layer.name = "Fields", label = fields_sf2$NAME) +
  mapview::mapview(counties, layer.name = "County boundaries", col.regions = "grey", alpha = 0.1, label = counties$NAME)





## create df with all combos
## ----------------------------------------------------------------

## not matched
no_match <- well_match_df %>%
  filter(is.na(original_asset_name))

## for nearest field df, join with nn_field_code asset
match_df <- well_match_df %>%
  filter(!is.na(original_asset_name)) %>%
  rename(nn_field_code = doc_field_code) %>% 
  select(-n_wells_asset) %>%
  mutate(match_method = "nearest_field_w_asset_match")

nn_field2 <- nn_fields %>%
  filter(doc_field_code %in% no_match$doc_field_code) %>%
  left_join(match_df) %>%
  select(doc_field_code, original_asset_name, dist, match_method)

## for nearest asset df, match column names above
nn_asset2 <- nn_assets %>%
  filter(doc_field_code %in% no_match$doc_field_code) %>%
  rename(original_asset_name = nn_asset) %>%
  mutate(match_method = "nearest_asset")

## combine 
no_match_assets <- rbind(nn_field2, nn_asset2) %>%
  group_by(doc_field_code) %>%
  mutate(dist_rank = rank(dist),
         min_rank = min(dist_rank)) %>%
  ungroup() %>%
  filter(dist_rank == min_rank) %>%
  ungroup() %>%
  select(-dist_rank, -min_rank) %>%
  mutate(n_wells_asset = NA) %>%
  left_join(fieldcodes) %>%
  select(doc_field_code, doc_fieldname, original_asset_name, dist, n_wells_asset, match_method)

all_matches <- well_match_df %>%
  filter(!is.na(original_asset_name)) %>%
  left_join(fieldcodes) %>%
  mutate(dist = NA,
         match_method = "well_match") %>%
  select(doc_field_code, doc_fieldname, original_asset_name, dist, n_wells_asset, match_method) %>%
  rbind(no_match_assets) %>%
  filter(doc_field_code != "000") %>%
  rename(dist_m = dist) %>%
  unique()

## save file
write_csv(all_matches, file = paste0(save_directory, match_out_path, "field_asset_matches_revised.csv"))



